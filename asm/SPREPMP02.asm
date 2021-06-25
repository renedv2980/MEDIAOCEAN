*          DATA SET SPREPMP02  AT LEVEL 114 AS OF 05/01/02                      
*PHASE SPMP02T,+0,NOAUTO                                                        
*INCLUDE MEDGETPL                                                               
*INCLUDE MEDACTBK                                                               
         TITLE 'SPREPMP02 - PEPSI LOCKIN REPORT'                                
********************UPDATE LOG*******************************                   
* JUL14/87  LEVEL 95 FIX FOR PIGGYBACKS.                                        
*                                                                               
*************************************************************                   
         EJECT                                                                  
SPMP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMP02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPMPRA                                                     
         LA    R2,2048(RB)         SECOND BASE REG                              
         LA    R2,2048(R2)                                                      
         USING SPMP02+4096,R2                                                   
         ST    R2,SPMPR2                                                        
         ST    R5,RELO                                                          
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         MVI   CURRSTG,0                                                        
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDLCHNK,=F'128'                                                 
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         XC    MEDNUMPE,MEDNUMPE                                                
         MVI   MEDEXTDM,4                                                       
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQPRDEST,C'Y'                                                    
         MVI   RQEQUIV,C'Y'                                                     
         L     RF,=V(FLTTABC)                                                   
         ST    RF,VFLTTAB                                                       
         L     RF,=V(VMDACTBK)                                                  
         ST    RF,MEDACTBK                                                      
         MVI   RCSUBPRG,1                                                       
         L     R4,=V(BUFFALOC)                                                  
         ST    R4,BUFFBUFF                                                      
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',(R4)                                        
         XC    RPTSWS,RPTSWS                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M21                                                              
         MVI   ESTSW,C'N'                                                       
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
         MVI   QOPT1,C'1'                                                       
*        CLI   QOPT1,C'4'          TAKEN OUT 6/11/82                            
*        BH    M2A1                                                             
         CLC   QEST(2),=C'NO'                                                   
         BNE   *+10                                                             
         MVC   QEST,=C'ALL'                                                     
M2A1     DS    0C                                                               
         MVC   QBOOK1,=C'ACT '                                                  
         MVC   MYSTAGE,QOPT1       SET UP STAGES TO BE EXTRACTED                
         NI    MYSTAGE,X'0F'                                                    
         XC    STAGES,STAGES                                                    
         MVC   STAGES(1),MYSTAGE                                                
         MVI   FCRDGOAL,C'Y'       SET TO READ GOALS                            
         CLI   STAGES,1            IS IT A GOAL REQUEST                         
         BE    EXIT                                                             
         MVI   FCRDGOAL,C'P'       SET TO READ PEPSI RECORDS                    
         B     EXIT                                                             
         EJECT                                                                  
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
         GOTO1 DATCON,DMCB,QSTART,(X'02',BQSTARTP)                              
         GOTO1 DATCON,DMCB,QEND,(X'02',BQENDP)                                  
         L     R4,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R4)                                      
*                                                                               
         MVC   SVQEST,QEST         SET ESTIMATE WHICH IS CURRENTLY              
         ZIC   RE,BEST             BEING PROCESSED                              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB+6(2)                                                    
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         MVC   QEST,SVQEST                                                      
*                                                                               
         CLI   QOPT1,C'4'                                                       
         BL    *+12                                                             
         CLI   QBOOK1,C' '                                                      
         BNE   M20A                                                             
         L     R1,ADEST            SET BOOK FROM EST HDR                        
         USING ESTHDR,R1                                                        
         ZIC   RE,EBOOK                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QBOOK1(2),DUB+6(2)                                               
         ZIC   RE,EBOOK+1                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QBOOK1+2(2),DUB+6(2)                                             
*                                                                               
M20A     CLC   QBOOK1,=C'ACT '     SET ADJUSTMENT MONTH                         
         BNE   *+12                                                             
         CLI   QOPT1,C'4'          SET ADJ FOR STAGE 5 AND LESS 8/20/87         
         BH    M20A0                                                            
         L     R1,ADEST            REFER TO EST HDR FOR OTHERS                  
         ZIC   RE,EHUTADJ                                                       
         SRL   RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         UNPK  QBOOK1+4(2),DUB+6(2)                                             
         B     M20A1                                                            
M20A0    MVC   QBOOK1+4(2),=C'NO'  FORCE NO ADJ FOR AFFIDS 8/20/87              
*                                                                               
M20A1    ZIC   RE,BPRD                                                          
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     R5,PRDBUFF                                                       
         LA    R5,28(RE,R5)                                                     
*                                                                               
         GOTO1 DEMOCON,DMCB,(4,0(R5)),(2,WORK),(C'S',ADBLOCK),SPUSRNMS          
         LA    R8,4                                                             
         XC    DNAMES,DNAMES                                                    
         LA    R7,DNAMES                                                        
         LA    R9,WORK                                                          
GETDNAM  OC    0(3,R7),0(R5)                                                    
         BZ    M20B                END                                          
         MVC   3(7,R7),0(R9)                                                    
         LA    R9,7(R9)                                                         
         LA    R5,3(R5)                                                         
         LA    R7,10(R7)                                                        
         BCT   R8,GETDNAM                                                       
M20B     DS    0H                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         B     EXIT                                                             
         EJECT                                                                  
M3       CLI   MODE,PROCBUY                                                     
         BNE   M4                                                               
         L     RE,ADEST                                                         
         USING ESTHDR,RE                                                        
         CLI   EPROF,C'Z'                                                       
         BE    EXIT                                                             
         DROP  RE                                                               
         CLI   STAGES,3            BYPASS PURGE HERE IF RERATE                  
         BH    MBNOPRG                                                          
         CLI   PURGESW,1           FIRST TIME                                   
         BNE   MBNOPRG             NO DONT REPURGE                              
         CLI   STAGES,2            DONT PURGE HERE IF GOAL LOCKIN               
         BL    MBNOPRG                                                          
         GOTO1 =V(PURGE),DMCB,(RA)                                              
MBNOPRG  DS    0H'0'                                                            
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         L     R9,ADBUY                                                         
         LA    R9,24(R9)                                                        
         USING NDELEM,R9                                                        
M3GTBBK  CLI   0(R9),2             GET BOOK FROM BUY RECORD                     
         BE    M3GTBBK1                                                         
         ZIC   R1,1(R9)                                                         
         AR    R9,R1                                                            
         B     M3GTBBK                                                          
M3GTBBK1 MVC   BUYBOOK,NDBOOK                                                   
         DROP  R9                                                               
         LA    RE,STAGES                                                        
         CLI   GBKSW,1                                                          
         BNE   SETSTG1                                                          
         GOTO1 =V(GETBOOK),DMCB,(RA),(R2)                                       
         MVI   GBKSW,0                                                          
         LA    RE,STAGES                                                        
SETSTG1  CLC   CURRSTG,0(RE)       GET NEXT STAGE                               
         BL    SETSTG2                                                          
         LA    RE,1(RE)                                                         
         CLI   0(RE),0                                                          
         BE    EXIT                                                             
         B     SETSTG1                                                          
SETSTG2  MVC   CURRSTG,0(RE)                                                    
         CLI   CURRSTG,2           BUY STAGE                                    
         BL    SETSTG1              NO - TRY NEXT                               
         XC    MYBUFIO,MYBUFIO                                                  
         MVC   BFEST,BUYKEST                                                    
         MVC   BFSEC,BDSEC                                                      
         DROP  R6                                                               
         XC    PSLIST,PSLIST                                                    
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         LA    RE,PSLIST                                                        
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-12                                                             
         MVC   0(2,RE),=X'FFFF'                                                 
M31      LA    RE,PSLIST                                                        
M31A     CLC   0(2,RE),=X'FFFF'    CHECK FOR END                                
         BE    M32                                                              
         CLC   0(1,RE),BPRD        PRODUCT OK                                   
         BNE   *+12                 NO - DELETE                                 
         LA    RE,2(RE)                                                         
         B     M31A                                                             
         XC    0(2,RE),0(RE)                                                    
         LA    RE,2(RE)                                                         
         B     M31A                                                             
*                                                                               
M32      LA    R3,2                                                             
         LA    R6,PSLIST                                                        
M323     CLC   0(2,R6),=X'FFFF'    END                                          
         BE    M34                                                              
         CLI   0(R6),0                                                          
         BNE   *+12                                                             
M323A    LA    R6,2(R6)                                                         
         B     M323                                                             
         L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R6)                                                   
         MVC   MEDSPTLN,1(R6)                                                   
         XC    HLDBOOK,HLDBOOK                                                  
         LA    R7,2                                                             
         CLI   CURRSTG,2           BUYERS DEMOS                                 
         BE    MA322                YES - EXTRACT THEM                          
         MVI   QRERATE,C'P'                                                     
         CLI   CURRSTG,7                                                        
         BNE   *+8                                                              
         MVI   QRERATE,C'I'                                                     
         CLI   QRERATE,C' '                                                     
         BE    MA322                                                            
         CLI   QRERATE,C'A'                                                     
         BNE   MA321                                                            
         LA    R7,5                                                             
         B     MA322                                                            
MA321    LA    R7,3                                                             
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R7,1(R7)                                                         
         CLI   QRERATE,C'I'                                                     
         BNE   *+8                                                              
         LA    R7,3(R7)                                                         
MA322    CLI   CURRSTG,4                                                        
         BH    MA322A                                                           
         L     RE,ADEST                                                         
         USING ESTHDR,RE                                                        
         MVC   HLDBOOK,EBOOK                                                    
         CLI   EBOOK,0                                                          
         BNE   *+10                                                             
         MVC   HLDBOOK,BUYBOOK                                                  
         DROP  RE                                                               
         ZIC   RE,HLDBOOK                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QBOOK1(2),DUB+6(2)                                               
         ZIC   RE,HLDBOOK+1                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QBOOK1+2(2),DUB+6(2)                                             
MA322A   GOTO1 MEDGETBY,DMCB,(RA),(R7)                                          
*                                                                               
* EXTRACT AND POST ROUTINES                                                     
*                                                                               
         L     R5,MEDAFRST                                                      
M324     L     RE,MEDALAST                                                      
         CR    R5,RE                                                            
         BH    M323A                                                            
         CLI   0(R5),0                                                          
         BNE   *+12                                                             
M324A    LA    R5,12(R5)                                                        
         B     M324                                                             
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M324A                                                            
         MVC   BFMED,QMED                                                       
         MVC   BFCLT,QCLT                                                       
         MVC   BFPRD,MEDBRAND                                                   
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         MVC   BFEST,BUYKEST                                                    
         MVC   BFMKT(2),BUYMSTA                                                 
         MVI   BFFLT,1                                                          
         L     RF,VFLTTAB                                                       
M324B    CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DATE NOT IN TABLE - SEE BLDFLT               
         CLC   0(2,RF),0(R5)                                                    
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     M324B                                                            
         MVC   BFFLT,2(RF)                                                      
         MVC   BFSTG,CURRSTG                                                    
         MVC   BFBOOK,HLDBOOK                                                   
         CLC   QBOOK1(3),=C'ACT'                                                
         BNE   *+14                                                             
         BAS   R9,GETACT                                                        
         MVC   BFBOOK,HALF                                                      
         CLI   CURRSTG,4                                                        
         BH    M324C                                                            
         L     R1,ADEST                                                         
         USING ESTHDR,R1                                                        
         MVC   BFBOOK(2),HLDBOOK                                                
         CLI   EBOOK,0                                                          
         BE    *+10                                                             
         MVC   BFBOOK(2),EBOOK                                                  
*---->   OC    BFBOOK+1(1),EHUTADJ OUT 93/08/09 FORCE OUTO                      
         DROP  R1                                                               
         DROP  RE                                                               
M324C    OC    BFBOOK,BFBOOK       BYPASS IF NO RATING BOOK                     
         BZ    M335                                                             
         CLI   PURGESW,1           FIRST TIME                                   
         BNE   MBNOPRG2            NO DONT REPURGE                              
         CLI   STAGES,4            DONT PURGE HERE IF NOT A                     
         BL    MBNOPRG2            RERATE LOCKIN                                
         GOTO1 =V(PURGE),DMCB,(RA)                                              
MBNOPRG2 DS    0H'0'                                                            
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         MVC   BFDPT,BDDAYPT                                                    
         DROP  RE                                                               
         MVC   BFSLN,1(R6)                                                      
         MVC   BFSTART(2),0(R5)                                                 
         MVC   BFSPOTS,MEDBYSPT                                                 
         MVC   BFDOL,MEDBYD                                                     
         MVC   BFDEM1,MEDBY1                                                    
         MVC   BFDEM2,MEDBY2                                                    
         MVC   BFDEM3,MEDBY3                                                    
         MVC   BFDEM4,MEDBY4                                                    
         MVC   BFDOLEQ,MEDBYDEQ                                                 
         MVC   BFDEM1E,MEDBY1EQ                                                 
         MVC   BFDEM2E,MEDBY2EQ                                                 
         MVC   BFDEM3E,MEDBY3EQ                                                 
         MVC   BFDEM4E,MEDBY4EQ                                                 
M334     DS    0H                                                               
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',(R7),MYBUFIO                                
         MVC   BFSTART(2),=X'FFFF'                                              
         GOTO1 (RF)                                                             
         B     M324A                                                            
         EJECT                                                                  
M335     L     RE,ADBUY            NO BOOK TRY TO FIND ONE                      
         LA    RE,24(RE)           FOR SPECIAL STUFF                            
         USING BDELEM,RE                                                        
         CLI   BDDAYPT,C'X'        SPECIAL DAYPART                              
         BE    *+10                                                             
         CLC   STA(4),=C'WGBS'     SPECIAL FOR STATIONS                         
         BE    *+10                                                             
         CLC   STA(4),=C'KSCI'     SPECIAL FOR STATIONS                         
         BE    *+10                                                             
         CLC   STA(4),=C'WSNS'                                                  
         BE    *+10                                                             
         CLC   BMKT,=H'402'        HONOLULU                                     
         BE    *+14                                                             
         OC    RSMKT,RSMKT         STATION ON RTG FILE                          
         BNZ   M324A               YES- BYPASS IT                               
         DROP  RE                                                               
         SPACE 1                                                                
         CLI   CURRSTG,4           OTHERWISE IF THIS IS A RERATE                
         BL    M324A               THEN SET BOOK FROM EST HDR                   
         L     R1,ADEST            OR BUY REC                                   
         USING ESTHDR,R1                                                        
         MVC   BFBOOK(2),HLDBOOK   BUY RECORD BOOK                              
         CLI   EBOOK,0                                                          
         BE    *+10                                                             
         MVC   BFBOOK(2),EBOOK     ESTIMATE HDR BOOK                            
         CLI   BFBOOK,0            BOOK STILL ZERO                              
         BNE   M324C                                                            
         PACK  DUB,EEND(2)         SET YEAR FROM END DATE                       
         CVB   RE,DUB                                                           
         STC   RE,BFBOOK                                                        
         ZIC   RE,EHUTADJ          SET MONTH FROM EST HDR.                      
         SRL   RE,4                                                             
         STC   RE,BFBOOK+1                                                      
*                                                                               
         CLI   BFBOOK+1,0          STILL NOT THERE THEN BYPASS                  
         BZ    M324A                                                            
         CLC   STA(4),=C'WGBS'     STATION BOUGHT IN NOV                        
         BNE   M324C               FIRST SWEPT JAN/86                           
         CLI   BFBOOK,85           BYPASS IF SUBS JAN/86                        
         BH    M324A                                                            
         B     M324C                                                            
         SPACE 2                                                                
M34      B     M3                  PASS DONE - TRY NEXT STAGE                   
         EJECT                                                                  
M4       CLI   MODE,MKTLAST                                                     
         BNE   M5                                                               
         LA    RE,ELSAVE                                                        
         ST    RE,ELADDR                                                        
         XC    MYBUFIO,MYBUFIO                                                  
         MVC   BFMED,QMED                                                       
         MVI   TOTSW,0                                                          
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R7),MYBUFIO,1                             
         TM    DMCB+8,X'80'                                                     
         BO    EXIT                                                             
         MVI   ENDSW,0                                                          
         B     M42                                                              
M41      L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R7),MYBUFIO,1                              
M42      TM    DMCB+8,X'80'        CHECK FOR KEY BREAKS                         
         BO    M48531                                                           
         CLC   PREVSTG,BFSTG       NEW PAGE AT STAGE BREAK                      
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   PREVEST,BFEST                                                    
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   CURRSTG,BFSTG                                                    
         MVC   PREVSTG,BFSTG                                                    
         MVC   PREVSLN,BFSLN                                                    
         MVC   PREVDPT,BFDPT                                                    
         MVC   PREVBRND,BFPRD                                                   
         MVC   PREVSEC,BFSEC                                                    
         MVC   PREVEST,BFEST                                                    
         MVC   PREVDAT,BFSTART                                                  
         L     R6,ELADDR                                                        
         USING PEPDELM,R6                                                       
         MVI   PEPDELM,X'10'                                                    
         MVI   PEPDLEN,12                                                       
         MVC   PEPDFLT,BFFLT                                                    
         MVC   PEPDSTG,BFSTG                                                    
         MVC   PEPDBOOK,BFBOOK                                                  
         MVC   PEPSPOTS,BFSPOTS+2                                               
         L     RE,BFDOL                                                         
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,FULL                                                          
         MVC   PEPDOL,FULL                                                      
         ZIC   RF,BFPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         ZIC   RE,PEPDLEN                                                       
         LA    R7,PEPDDCDE                                                      
         LA    R9,BFDEM1                                                        
         LA    RF,28(RF)                                                        
EXTDEM   OC    0(3,RF),0(RF)                                                    
         BZ    EXTDEM1                                                          
         MVC   0(2,R7),1(RF)       DEMO CODE                                    
         MVC   2(3,R7),1(R9)       VALUE                                        
         LA    RE,5(RE)            BUMP LENGTH                                  
         STC   RE,PEPDLEN                                                       
         LA    R7,5(R7)                                                         
         LA    RF,3(RF)                                                         
         LA    R9,4(R9)                                                         
         B     EXTDEM                                                           
EXTDEM1  A     RE,ELADDR                                                        
         ST    RE,ELADDR                                                        
         XC    0(10,RE),0(RE)                                                   
EXITDEM  DS    0C                                                               
         SPACE 2                                                                
M44      MVC   ENDSW,DMCB+8        SAVE EOF SWITCH                              
         LA    RE,ELSAVE           SET ADDRESS FOR NEXT RECORD                  
         ST    RE,ELADDR                                                        
         XC    KEY,KEY             BUILD GOAL KEY                               
         LA    R8,KEY                                                           
         USING PEPKEY,R8                                                        
         MVI   PEPKCDE,X'0D'                                                    
         MVI   PEPKTYP,X'15'                                                    
         MVC   PEPKAM,BAGYMD                                                    
         MVC   PEPKCLT,BCLT                                                     
         MVC   PEPKPRD,PREVBRND                                                 
         MVC   PEPKMKT,BMKT                                                     
         MVC   PEPKEST,PREVEST                                                  
         MVC   PEPKDPT,PREVDPT                                                  
         MVC   PEPKLEN,PREVSLN                                                  
         MVC   PEPKDTE,PREVDAT                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         L     RE,ADGOAL                                                        
         ST    RE,AREC                                                          
         MVC   WRCMND,=C'PUTREC'                                                
         CLC   KEY(13),KEYSAVE                                                  
         BE    M44GET                                                           
         MVC   WRCMND,=C'ADDREC'                                                
         B     M45                                                              
M44GET   GOTO1 GETGOAL                                                          
         B     M48                                                              
M45      L     R8,ADGOAL                                                        
         LR    RE,R8                                                            
         L     RF,=F'1600'                                                      
         BAS   R9,CLEAR                                                         
         MVC   PEPREC(13),KEYSAVE                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   PEPRLEN,=H'24'                                                   
         EJECT                                                                  
M48      L     R6,ADGOAL                                                        
         LA    R6,24(R6)                                                        
         LA    R8,ELSAVE                                                        
M481     CLI   0(R6),0             FIND SLOT FOR INSERT                         
         BE    M484                                                             
         CLI   0(R6),X'10'                                                      
         BE    M4831                                                            
M482     ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     M481                                                             
         SPACE 2                                                                
M483     LA    R8,ELSAVE                                                        
M4831    CLC   PEPDSTG(1),3(R8)                                                 
         BL    M482                                                             
         BNE   M484                                                             
*                                       (D.F. SEP23/83)                         
         OC    PEPDBOOK(8),PEPDBOOK     ALLOW FOR MULTIPLE BOOKS                
         BNZ   M484                     ON BUYERS DEMOS                         
         L     R7,ADGOAL                                                        
         GOTO1 RECUP,DMCB,(R7),(R6)     DELETE DUPLICATES                       
M484     L     R7,ADGOAL                                                        
         GOTO1 RECUP,DMCB,(R7),(R8),(R6)                                        
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R8),0             END OF ELEMENTS                              
         BNE   M4831                NO - ADD NEXT ELEMENT                       
         EJECT                                                                  
         MVI   P,0                                                              
         LA    R6,ELSAVE                                                        
         MVC   DMCB+4(1),PEPDSTG                                                
         MVI   DMCB+5,0                                                         
         MVC   DMCB+6(2),PEPDBOOK                                               
         CLC   BKSAVE,PEPDBOOK                                                  
         BE    *+14                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   BKSAVE,PEPDBOOK                                                  
         LA    R8,KEY                                                           
         L     R8,ADGOAL                                                        
         MVC   SVDTE,PEPKDTE                                                    
         L     R5,MEDAFRST                                                      
         MVC   PEPKDTE,0(R5)                                                    
         GOTO1 =V(MEDGETPL),DMCB,(RA)                                           
M4841    DS    0C                                                               
         LA    R6,ELSAVE                                                        
         MVC   P1+2(1),PEPKDPT                                                  
         CLC   DPSAVE(2),PEPKDPT                                                
         BE    *+14                                                             
         MVC   DPSAVE(2),PEPKDPT                                                
         MVI   FORCEHED,C'Y'                                                    
         EDIT  (1,PEPKLEN),(3,P1+3),ALIGN=LEFT                                  
         MVC   P+12(4),=C'TOT '                                                 
         MVI   SPACING,2                                                        
         MVI   TOTSW,1                                                          
         CLI   SVDTE,X'FF'                                                      
         BE    M4842                                                            
         GOTO1 DATCON,DMCB,(2,SVDTE),(4,P+12)                                   
         MVI   TOTSW,0             RESET TOTAL SWITCH                           
         MVI   SPACING,1                                                        
M4842    DS    0C                                                               
         EDIT  (1,PEPDFLT),(3,P1+24)                                            
         L     R9,MEDBUFF                                                       
         USING MEDBLOCK,R9                                                      
         L     R5,MEDAFRST                                                      
M485     C     R5,MEDALAST                                                      
         BH    M4852                                                            
         CLI   0(R5),0                                                          
         BNE   *+12                                                             
M4851    LA    R5,12(R5)                                                        
         B     M485                                                             
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         MVC   PEPKDTE,SVDTE                                                    
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M4851                                                            
         EDIT  MEDBYD,(10,P+39),,COMMAS=YES                                     
         EDIT  MEDBYSPT,(3,P+33)                                                
         LA    R1,4                                                             
         LA    RE,MEDBY1                                                        
MEDFIX   MVI   0(RE),0                                                          
         MVI   4(RE),0                                                          
         LA    RE,8(RE)                                                         
         BCT   R1,MEDFIX                                                        
         EDIT  MEDBY1,(7,P+55),1                                                
         EDIT  MEDBY2,(7,P+63),1                                                
         EDIT  MEDBY3,(7,P+71),1                                                
         EDIT  MEDBY4,(7,P+79),1                                                
         GOTO1 REPORT                                                           
         B     M4851                                                            
         DROP  R9                                                               
M4852    CLI   RCWRITE,C'Y'                                                     
         BNE   M4853                                                            
         CLI   TOTSW,1             TOTAL RECORD                                 
         BE    M4853                                                            
         MVC   AREC,ADGOAL                                                      
         CLC   WRCMND,=C'ADDREC'                                                
         BNE   M4852A                                                           
         GOTO1 ADD                                                              
         B     M4853                                                            
M4852A   CLC   WRCMND,=C'PUTREC'                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 PUT                                                              
         SPACE 2                                                                
M4853    DS    0C                                                               
         CLC   QAGY,=C'PE'         PEPSI                                        
         BE    M48530               YES - BYPASS WORKER FILE CREATION           
         GOTO1 =V(PEWORKER),DMCB,(RA)                                           
M48530   CLI   ENDSW,0                                                          
         BE    M41                                                              
M48531   L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R7)                                      
         DROP  R8                                                               
         EJECT                                                                  
M5       CLI   MODE,MKTFRST                                                     
         BNE   M6                                                               
         MVC   FULL,VFLTTAB                                                     
         L     RE,VFLTTAB                                                       
         LA    RF,1000                                                          
         BAS   R9,CLEAR                                                         
         GOTO1 =V(BLDFLT),DMCB,(RA)                                             
         MVI   PURGESW,1                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
M6       CLI   MODE,PRDFRST                                                     
         BNE   M7                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
M7       CLI   MODE,PROCGOAL                                                    
         BNE   M8                                                               
         CLI   PURGESW,1           FIRST TIME                                   
         BNE   MGNOPRG             NO DONT REPURGE                              
         SPACE 2                                                                
         CLC   QAGY,=C'PE'         CHECK IF DOING PEPSI                         
         BNE   M7PRG                                                            
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVI   CPROF+8,C'0'                                                     
         CLI   STAGES,1            IF THIS IS A GOAL PURGE IT                   
         BE    M7PRG                                                            
         L     RE,ADGOAL                                                        
         USING PEPREC,RE                                                        
         L     RF,=A(BDMKTS)                                                    
         A     RF,RELO                                                          
M7BYPRG  CLC   PEPKMKT,0(RF)       BYPASS PURGING FOR OTHER                     
         BE    M7BYPRGX            AGENCY MARKETS                               
         LA    RF,2(RF)                                                         
         CLC   0(2,RF),=X'FFFF'                                                 
         BNE   M7BYPRG                                                          
         DROP  RE                                                               
         SPACE 2                                                                
M7PRG    GOTO1 =V(PURGE),DMCB,(RA)                                              
M7BYPRGX CLI   STAGES,1                                                         
         BNE   EXIT                                                             
MGNOPRG  DS    0H'0'                                                            
         LA    RE,KEY                                                           
         USING GOALREC,RE                                                       
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
*                                                                               
         GOTO1 MEDGETGL,DMCB,(RA)                                               
*                                                                               
* EXTRACT AND POST ROUTINES                                                     
*                                                                               
         L     R5,MEDAFRST                                                      
         L     R6,MEDALAST                                                      
M52      CR    R5,R6                                                            
         BH    EXIT                                                             
         CLI   0(R5),0                                                          
         BNE   *+12                                                             
M522     LA    R5,12(R5)                                                        
         B     M52                                                              
*                                                                               
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDGLD(12),MEDGLD                                                
         BZ    M522                                                             
         XC    MYBUFIO,MYBUFIO                                                  
         MVC   BFMED,QMED                                                       
         MVC   BFCLT,QCLT                                                       
         MVC   BFPRD,MEDBRAND                                                   
         L     RE,ADGOAL                                                        
         MVC   BFEST,GKEYEST                                                    
         MVC   BFMKT,GKEYMKT                                                    
         XC    BFSTAT,BFSTAT                                                    
         MVC   BFDPT,GKEYDPT                                                    
         MVC   BFSLN,MEDSPTLN                                                   
         MVC   BFSEC,MEDSPTLN                                                   
         MVC   BFSTART(2),0(R5)                                                 
         GOTO1 DATCON,DMCB,(2,2(R5)),(3,FULL)                                   
         MVC   HLDBOOK,FULL                                                     
         L     RF,VFLTTAB                                                       
M522B    CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DATE NOT IN TABLE - SEE BLDFLT               
         CLC   0(2,RF),0(R5)                                                    
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     M522B                                                            
         MVC   BFFLT,2(RF)                                                      
         MVI   BFSTG,1                                                          
         MVC   BFBOOK,HLDBOOK                                                   
         CLI   HLDBOOK,0                                                        
         BNE   *+14                                                             
         BAS   R9,GETACT                                                        
         MVC   BFBOOK,HALF                                                      
         MVC   BFDOL,MEDGLD                                                     
         MVC   BFDEM1,MEDGL1                                                    
         MVC   BFDOLEQ,MEDGLDEQ                                                 
         MVC   BFDEM1E,MEDGL1EQ                                                 
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',(R7),MYBUFIO                                
         MVC   BFSTART(2),=X'FFFF'                                              
         GOTO1 (RF)                                                             
         B     M522                                                             
         SPACE 2                                                                
M8       CLI   MODE,STAFRST                                                     
         BNE   M10                                                              
         MVI   GBKSW,1                                                          
         GOTO1 =V(VALSTA),DMCB,(RA)                                             
         B     EXIT                                                             
         SPACE 2                                                                
M10      CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         GOTO1 =V(PEWORKER),DMCB,(RA)                                           
         B     EXIT                                                             
         SPACE 2                                                                
GETACT   LA    R1,ACTAREA                                                       
GETACT2  CLC   2(2,R1),0(R5)                                                    
         BL    *+12                                                             
         MVC   HALF,4(R1)                                                       
         BR    R9                                                               
         LA    R1,6(R1)                                                         
         B     GETACT2                                                          
*                                                                               
CLEAR    DS    0C                                                               
         XCEF                                                                   
         BR    R9                                                               
         EJECT                                                                  
EXIT     XMOD1 1                                                                
         EJECT                                                                  
         DS    0D                                                               
* HEADLINE HOOK                                                                 
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SPMPRB                                                      
         DROP  RF                                                               
         LM    RA,RC,SPMPRA                                                     
         USING SPWORKD,RA,RC                                                    
         L     R2,SPMPR2                                                        
*                                                                               
*                                                                               
         LA    R6,STGTABLE                                                      
STGLOOK  CLI   0(R6),X'FF'                                                      
         BE    STGMATCH                                                         
         CLC   0(1,R6),CURRSTG                                                  
         BE    STGMATCH                                                         
         LA    R6,12(R6)                                                        
         B     STGLOOK                                                          
*                                                                               
STGMATCH MVC   H8+42(5),=C'STAGE'                                               
         MVC   H8+49(1),CURRSTG                                                 
         OI    H8+49,X'F0'                                                      
         MVC   H8+1(8),=C'ESTIMATE'                                             
         ZIC   RE,PREVEST                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H8+10(3),DUB+6(2)                                                
         MVC   H8+67(4),=C'BOOK'                                                
         MVC   HALF,BFBOOK                                                      
         NI    HALF+1,X'0F'                                                     
         GOTO1 DATCON,DMCB,(3,HALF),(6,H8+72)                                   
         ZIC   RE,BFBOOK+1                                                      
         SRL   RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   H8+79(4),=C'(  )'                                                
         UNPK  H8+80(2),DUB+6(2)                                                
         LTR   RE,RE                                                            
         BNZ   *+10                                                             
         MVC   H8+79(4),=C'    '                                                
         MVC   H7(20),SPACES                                                    
*                                                                               
         LA    R5,DNAMES                                                        
         LA    R4,4                                                             
         LA    R6,H9+55                                                         
         LA    R7,H10+55                                                        
DEM      OC    0(3,R5),0(R5)       TEST END                                     
         BZ    DEM2                                                             
         MVC   0(7,R6),3(R5)       DEMO DESCRIPTION                             
         MVC   0(7,R7),=7C'-'      UNDERLINE                                    
         LA    R5,10(R5)                                                        
         LA    R6,8(R6)                                                         
         LA    R7,8(R7)                                                         
         BCT   R4,DEM                                                           
DEM2     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
SPMPRA   DS    F                                                                
SPMPRB   DS    F                                                                
SPMPRC   DS    F                                                                
SPMPR2   DS    F                                                                
*                                                                               
*                                                                               
STGTABLE DC    X'01',C'PLAN       '                                             
         DC    X'02',C'BUYERS EST '                                             
         DC    X'03',C'INIT SEAS A'                                             
         DC    X'04',C'REV SEAS A '                                             
         DC    X'05',C'RERATE     '                                             
         DC    X'06',C'REV RERATE '                                             
         DC    X'07',C'AFFID      '                                             
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
REQSW    DC    X'00'                                                            
SVQEST   DS    CL3                                                              
RDESTSW  DS    CL1                                                              
RPTSWS   DS    0CL7                REPORT SWITCHES                              
WSSW     DS    CL1                 WEEKLY SUMMARY SWITCHES                      
BDSW     DS    CL1                 BUY DETAIL SWITCH                            
         DS    CL5                                                              
RECSEQ   DC    H'0'                                                             
RELO     DS    F'0'                                                             
VWRREC   DC    F'0'                                                             
VCLSE    DC    F'0'                                                             
VOPN     DC    F'0'                                                             
VGETIN   DC    F'0'                                                             
VFLTTAB  DS    F                                                                
ELADDR   DC    F'0'                                                             
PURGESW  DC    X'00'                                                            
ENDSW    DS    C                                                                
PREVBRND DS    C                                                                
PREVEST  DS    C                                                                
PREVDPT  DS    C                                                                
PREVSLN  DS    C                                                                
PREVSEC  DS    C                                                                
PREVDAT  DS    CL2                                                              
PREVSTG  DS    C                                                                
SVDTE    DS    CL2                                                              
WRCMND   DS    CL6                                                              
TOTSW    DS    C                                                                
STAGES   DS    CL7                                                              
MYSTAGE  DS    CL1                                                              
CURRSTG  DS    CL1                                                              
HLDBOOK  DS    CL2                                                              
BUYBOOK  DS    CL2                                                              
RSMKT    DS    CL2                                                              
DUMMYEL  DS    0C                                                               
ELEM     DS    CL100                                                            
         EJECT                                                                  
         DS    0F                                                               
MYBUFIO  DS    0CL62                                                            
BFMED    DS    CL1                                                              
BFCLT    DS    CL3                                                              
BFPRD    DS    CL1                                                              
BFMKT    DS    CL2                                                              
BFSTAT   DS    CL3                                                              
BFSTG    DS    CL1                                                              
BFBOOK   DS    CL2                                                              
BFEST    DS    CL1                                                              
BFDPT    DS    CL1                                                              
BFSLN    DS    CL1                                                              
BFSEC    DS    CL1                                                              
BFSTART  DS    CL2                                                              
BFFLT    DS    CL1                                                              
BFSPOTS  DS    CL4                                                              
BFDOL    DS    CL4                                                              
BFDEM1   DS    CL4                                                              
BFDEM2   DS    CL4                                                              
BFDEM3   DS    CL4                                                              
BFDEM4   DS    CL4                                                              
BFDOLEQ  DS    CL4                                                              
BFDEM1E  DS    CL4                                                              
BFDEM2E  DS    CL4                                                              
BFDEM3E  DS    CL4                                                              
BFDEM4E  DS    CL4                                                              
PSLIST   DS    CL100                                                            
BKSAVE   DS    CL2                                                              
DPSAVE   DS    CL2                                                              
GBSTART  DS    CL2                                                              
GBEND    DS    CL2                                                              
GBKSW    DS    C                                                                
ACTAREA  DS    CL144                                                            
MEDACTBK DS    F                                                                
ELSAVE   DS    1000C                                                            
DNAMES   DS    CL40                                                             
         EJECT                                                                  
BLDFLT   CSECT                                                                  
         NMOD1 0,BLDFLT                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         MVC   SVFKEY,KEY                                                       
         MVC   SVFREC,AREC                                                      
         MVC   AREC,ADGOAL                                                      
         MVC   FLTTAB,FULL                                                      
         XC    KEY,KEY             GET HIGHEST FLITE NUMBER                     
         LA    R5,KEY                                                           
         USING PEPREC,R5                                                        
         MVC   PEPKEY(2),=X'0D15'                                               
         MVC   PEPKAM,BAGYMD                                                    
         MVC   PEPKCLT,BCLT                                                     
         MVC   PEPKPRD,BPRD                                                     
         MVC   PEPKMKT,BMKT                                                     
         MVC   FLTKEY,KEY                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BFGETR                                                           
         L     R5,AREC             BUILD NEW RECORD                             
         XC    0(255,R5),0(R5)                                                  
         MVC   0(13,R5),KEYSAVE                                                 
         MVC   PEPRLEN,=H'24'                                                   
         LA    R6,24(R5)                                                        
         USING PEPA,R6                                                          
         MVI   PEPAELM,X'F0'                                                    
         MVI   PEPALEN,9                                                        
         MVI   PEPAFLT,0                                                        
         MVC   HALF,PEPRLEN                                                     
         LH    RF,HALF                                                          
         LA    RF,9(RF)                                                         
         STH   RF,HALF                                                          
         MVC   PEPRLEN,HALF                                                     
         MVC   WRCMN,=C'ADDREC'    ADD TO FILE                                  
         BAS   R9,WRFLT                                                         
         GOTO1 HIGH                                                             
         B     BFGETR2                                                          
         SPACE 2                                                                
BFGETR   GOTO1 GET                                                              
BFGETR2  L     R5,AREC                                                          
         LA    R6,24(R5)                                                        
         MVC   HIFLITE,PEPAFLT                                                  
         CLI   HIFLITE,0                                                        
         BNE   *+8                                                              
         MVI   HIFLITE,1                                                        
         MVI   SVHIFLT,0                                                        
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         L     R4,FLTTAB                                                        
         USING FLTTABD,R4                                                       
         L     R7,MEDAFRST                                                      
         L     R8,MEDALAST                                                      
         LA    R8,12(R8)                                                        
BFTABLE  CR    R7,R8                                                            
         BNL   BFTABLEX                                                         
         CLI   0(R7),0                                                          
         BE    BFTABLE4                                                         
         MVC   KEY(13),FLTKEY      GET FLIGHT FOR THIS DATE                     
         LA    R5,KEY                                                           
         MVC   PEPKDTE,0(R7)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     HAVE FLIGHT                                  
         BE    BFTABLE1             YES - SAVE IT                               
         L     R5,AREC                                                          
         MVC   PEPKDTE,0(R7)                                                    
         MVC   KEY(13),0(R5)                                                    
         ZIC   RE,HIFLITE           NO - SET FLIGHT                             
         LA    RE,1(RE)                                                         
         STC   RE,PEPAFLT                                                       
         MVC   PEPASTG,STAGES                                                   
         STC   RE,SVHIFLT                                                       
         MVC   WRCMN,=C'ADDREC'                                                 
         BAS   R9,WRFLT                                                         
         B     BFTABLE3                                                         
BFTABLE1 GOTO1 GET                                                              
BFTABLE3 CLI   PEPASTG,2           BYPASS IF ALREADY STAGE 2                    
         BE    BFTAB3A                                                          
         CLI   STAGES,2            BYPASS IF NOT DOING STAGE 2                  
         BNE   BFTAB3A                                                          
         ZIC   RE,HIFLITE          RESET FLIGHT NUMBER                          
         LA    RE,1(RE)                                                         
         STC   RE,PEPAFLT                                                       
         STC   RE,SVHIFLT                                                       
BFTAB3A  MVC   0(2,R4),0(R7)                                                    
         MVC   2(1,R4),PEPAFLT                                                  
         LA    R4,3(R4)                                                         
BFTABLE4 LA    R7,12(R7)                                                        
         B     BFTABLE                                                          
BFTABLEX DS    0H                                                               
         CLI   SVHIFLT,0                                                        
         BE    BFTX1                                                            
         MVC   KEY(13),FLTKEY                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GET                                                              
         L     R5,AREC                                                          
         LA    R6,24(R5)                                                        
         MVC   PEPAFLT,SVHIFLT                                                  
         MVC   PEPASTG,STAGES                                                   
         MVC   WRCMN,=C'PUTREC'                                                 
         BAS   R9,WRFLT                                                         
BFTX1    DS    0H'0'                                                            
         MVC   KEY,SVFKEY                                                       
         GOTO1 HIGH                                                             
         GOTO1 GET                                                              
         XMOD1 1                                                                
         EJECT                                                                  
WRFLT    CLI   RCWRITE,C'Y'                                                     
         BNE   WRFLT3                                                           
         CLC   WRCMN,=C'ADDREC'                                                 
         BNE   WRFLT2                                                           
         GOTO1 ADD                                                              
         BR    R9                                                               
WRFLT2   GOTO1 PUT                                                              
         BR    R9                                                               
WRFLT3   BR    R9                                                               
PRTTEST  MVC   P(6),WRCMN                                                       
         L     R5,AREC                                                          
         GOTO1 HEXOUT,DMCB,(R5),P+7,32,0,0                                      
         GOTO1 REPORT                                                           
         BR    R9                                                               
         LTORG                                                                  
FLTTAB   DS    F                                                                
SVFREC   DS    F                                                                
SVFKEY   DS    CL20                                                             
WRCMN    DS    CL6                                                              
HIFLITE  DC    X'00'                                                            
SVHIFLT  DS    X'00'                                                            
FLTKEY   DS    CL13                                                             
FLTTABD  DSECT                                                                  
FLTDATE  DS    CL2                 WEEK START DATE                              
FLTLIT   DS    CL1                 FLIGHT NUMBER                                
         EJECT                                                                  
         BUFF  LINES=200,ROWS=1,COLUMNS=11,FLAVOR=BINARY,KEYLIST=(20,A)         
         LTORG                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
DEMREC   DS    CL2000                                                           
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         EJECT                                                                  
* PURGE ALL RECORDS FOR THIS STAGE                                              
PURGE    CSECT                                                                  
         NMOD1 0,PURGE                                                          
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         MVI   PURGESW,0                                                        
         MVC   SVPKEY,KEY                                                       
         MVC   SVPREC,AREC                                                      
         L     R8,ADMARKET                                                      
         USING MKTREC,R8                                                        
         CLC   QAGY,=C'PE'         PEPSI EXTRACT                                
         BNE   *+12                                                             
         CLI   MKTLTACC,C'Z'       OTHER AGENCY MARKET                          
         BE    PURGEEX                                                          
         DROP  R8                                                               
         LA    R8,KEY                                                           
         USING PEPKEY,R8                                                        
         XC    KEY(13),KEY                                                      
         MVI   PEPKCDE,X'0D'                                                    
         MVI   PEPKTYP,X'15'                                                    
         MVC   PEPKAM,BAGYMD                                                    
         MVC   PEPKCLT,BCLT                                                     
         MVC   PEPKPRD,BPRD                                                     
         MVC   PEPKMKT,BMKT                                                     
         MVI   PEPKEST,1                                                        
PURGE01  GOTO1 HIGH                                                             
         B     PURGE021                                                         
PURGE02  GOTO1 SEQ                                                              
PURGE021 CLC   KEY(8),KEYSAVE                                                   
         BNE   PURGEEX                                                          
         CLI   PEPKEST,0                                                        
         BE    PURGE02                                                          
         CLC   PEPKDTE,BQSTARTP                                                 
         BL    PURGE02                                                          
         CLC   PEPKDTE,BQENDP                                                   
         BH    PURGE02                                                          
         SPACE 2                                                                
         CLI   STAGES,1            IS IT A GOAL LOCKIN                          
         BE    PURGE04                                                          
         CLI   PEPKDPT,C'G'        NO DONT PURGE DAYPART G                      
         BE    PURGE02                                                          
         SPACE 2                                                                
PURGE04  CLI   BEST,0                                                           
         BE    PURGE06                                                          
         CLC   PEPKEST,BEST                                                     
         BL    PURG04A                                                          
         BE    PURGE06                                                          
         CLI   BESTEND,0                                                        
         BE    PURGE041                                                         
         CLC   PEPKEST,BESTEND                                                  
         BH    PURGE041                                                         
         B     PURGE06                                                          
PURG04A  DS    0C                                                               
         MVC   PEPKEST,BEST                                                     
         XC    PEPKDPT(4),PEPKDPT                                               
         B     PURGE01                                                          
PURGE041 MVC   PEPKEST(5),=X'FFFFFFFFFF'                                        
         B     PURGE01                                                          
         SPACE 2                                                                
PURGE06  GOTO1 GETGOAL                                                          
         L     R7,ADGOAL                                                        
         LA    R7,24(R7)                                                        
         USING PEPELEMS,R7                                                      
         USING PEPELEMS,R7                                                      
         CLI   PEPELEMS,0          ANY ELEMENTS AROUND                          
         BE    PURGE02             GET NEXT RECORD                              
         XC    ELEM,ELEM           RESET ELEMENT SAVE                           
PURGE08  CLI   PEPELEMS,0          GET DATA ELEMENT                             
         BE    PURGE09                                                          
         CLI   PEPELEMS,X'10'                                                   
         BNE   PURGE08A                                                         
         XC    ELEM,ELEM           SAVE DUMMY ELEMENT                           
         ZIC   R1,1(R7)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R7)                                                    
         LA    R1,STAGES                                                        
PURG08A  CLI   1(R1),0             SET UP LATEST STAGE                          
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     PURG08A                                                          
         LR    RE,R7                                                            
         LA    R7,ELEM                                                          
         MVC   PEPDSTG,0(R1)                                                    
         XC    PEPDBOOK(8),PEPDBOOK                                             
         LA    RF,PEPDEMS                                                       
PURG08B  CLI   0(RF),0                                                          
         BE    PURG08C                                                          
         XC    2(3,RF),2(RF)                                                    
         LA    RF,5(RF)                                                         
         B     PURG08B                                                          
PURGE08A ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     PURGE08                                                          
PURG08C  LR    R7,RE                                                            
         SPACE 2                                                                
PURG081  LA    R1,STAGES                                                        
PURG082  CLI   0(R1),0             MATCH TO EXISTING STAGES                     
         BE    PURGE08A             NOT FOUND - GET NEXT ELEMENT                
         CLC   0(1,R1),PEPDSTG     FOUND - ZERO OUT DOLLARS AND DEMOS           
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     PURG082                                                          
         SPACE 2                                                                
         CLI   QOPT2,C'Y'          ALLOW REPLACEMENT                            
         BNE   PURGERR              NO - SET ERROR                              
         XC    PEPDBOOK(8),PEPDBOOK ZERO OUT DATA FOR                           
         ZIC   R9,1(R7)                                                         
         AR    R9,R7               POINT TO NEXT ELEM                           
         LA    R1,PEPDDCDE                                                      
PURG083  XC    2(3,R1),2(R1)       CLEAR OUT DEMO VALUES                        
         LA    R1,5(R1)                                                         
         CR    R1,R9               TEST END OF ELEM                             
         BL    PURG083                                                          
*                                                                               
         ZIC   R0,1(R7)                                                         
         MVC   BYTE,PEPDSTG                                                     
         AR    R7,R0                                                            
PURG084  CLI   0(R7),0             DELETE ALL ELEMENTS FOR THIS STAGE           
         BE    PURGE10                                                          
         OC    PEPDBOOK,PEPDBOOK   AND PREVIOUS DELETES                         
         BZ    PURG085                                                          
         CLC   BYTE,PEPDSTG                                                     
         BE    PURG085                                                          
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     PURG084                                                          
PURG085  L     R9,ADGOAL                                                        
         GOTO1 RECUP,DMCB,(R9),(R7),0                                           
         B     PURG084                                                          
         SPACE 2                                                                
* FORCE LATEST STAGE FOR ALL RECORDS                                            
PURGE09  L     R9,ADGOAL                                                        
         CLI   DUMMYEL,X'10'       NO X'10' ELEMENTS - BYPASS                   
         BNE   PURGE02                                                          
         GOTO1 RECUP,DMCB,(R9),DUMMYEL,(R7)                                     
* WRITE BACK RECORD                                                             
PURGE10  MVC   WRCMND,=C'PUTREC'                                                
         L     R9,ADGOAL                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   PURGE02                                                          
         BNE   PRTPRG              NOOP PREV INSRTUCTION TO PRINT               
         MVC   AREC,ADGOAL                                                      
         GOTO1 PUT                                                              
         CLC   QAGY,=C'PE'                                                      
         BE    PURGE02                                                          
         GOTO1 =V(PEWORKER),DMCB,(RA)                                           
         B     PURGE02                                                          
PURGERR  MVC   P(30),=C'DATA ALREADY EXISTS FOR STAGE '                         
         MVC   P+31(1),PEPDSTG                                                  
         OI    P+30,X'F0'                                                       
         GOTO1 REPORT                                                           
         MVI   MODE,MKTLAST                                                     
         B     PURGEEX                                                          
         SPACE 2                                                                
PURGEEX  DS    0C                                                               
         MVC   KEY,SVPKEY                                                       
         MVC   AREC,SVPREC                                                      
         GOTO1 HIGH                                                             
         GOTO1 GET                                                              
         XMOD1 1                                                                
         SPACE 2                                                                
PRTPRG   MVC   P(6),WRCMND                                                      
         L     R5,AREC                                                          
         GOTO1 HEXOUT,DMCB,(R5),P+7,132,0,0                                     
         GOTO1 REPORT                                                           
         B     PURGE02                                                          
         SPACE 2                                                                
SVPKEY   DS    CL20                                                             
SVPREC   DS    F                                                                
BDMKTS   DC    X'FFFE'                                                          
         DC    X'FFFF'                                                          
*DMKTS   DC    H'264'                                                           
         DC    H'858'                                                           
         DC    H'688'                                                           
         DC    H'620'                                                           
         DC    H'304'                                                           
         DC    H'472'                                                           
         DC    H'340'                                                           
         DC    H'572'                                                           
* TRACY LOCK MARKET NUMBERS                                                     
         DC    H'24'                                                            
         DC    H'60'                                                            
         DC    H'84'                                                            
         DC    H'244'                                                           
         DC    H'404'                                                           
         DC    H'480'                                                           
         DC    H'508'                                                           
         DC    H'554'                                                           
         DC    H'656'                                                           
         DC    H'684'                                                           
         DC    H'888'                                                           
* GEOGRAPHIC MARKETING                                                          
         DC    H'352'                                                           
         DC    X'FFFF'                                                          
         LTORG                                                                  
         EJECT                                                                  
VALSTA   CSECT                                                                  
         NMOD1 0,VALSTA                                                         
         L     RA,0(R1)            R1 POINTS TO A(SPWORKD)                      
*                                               A(CALL LETTERS)                 
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         XC    RSMKT,RSMKT                                                      
         L     R5,ADBLOCK          SET UP THE DBLOCK                            
         USING DBLOCK,R5                                                        
         XC    0(256,R5),0(R5)                                                  
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELSRC,C'A'                                                    
*                                                                               
         CLC   MARKET,=C'0402'     CHECK FOR NSI MARKETS                        
         BE    *+10                                                             
         CLC   MARKET,=C'0954'                                                  
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         MVC   DBSELSTA,STA                                                     
         MVI   DBFUNCT,DBGETMB                                                  
         L     RE,=A(DEMREC)                                                    
         ST    RE,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
* VALIDATE THAT STATION EXISTS                                                  
         GOTO1 DEMAND,DMCB,ADBLOCK,SETMKT                                       
INVMKT   XIT1                                                                   
         SPACE 2                                                                
* DEMAND COMES HERE WITH MARKET SET                                             
SETMKT   MVC   RSMKT,DBACTRMK                                                   
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
GETBOOK  CSECT                                                                  
         NMOD1 0,GETBOOK                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R2,4(R1)                                                         
         CLC   QBOOK1,=C'ACT '                                                  
         BE    GETBACT                                                          
         PACK  DUB,QBOOK1(2)                                                    
         CVB   RF,DUB                                                           
         STC   RF,HLDBOOK                                                       
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   RF,DUB                                                           
         STC   RF,HLDBOOK+1                                                     
         B     GETBOOKX                                                         
GETBACT  L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         L     R5,MEDAFRST                                                      
         MVC   GBSTART,0(R5)                                                    
         L     R5,MEDALAST                                                      
         MVC   GBEND,2(R5)                                                      
         GOTO1 MEDACTBK,DMCB,(RA),GBSTART,ACTAREA                               
         XC    HLDBOOK,HLDBOOK                                                  
         DROP  RE                                                               
         SPACE 2                                                                
GETBOOKX XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'WRITE WORKER FILE FOR OTHER AGENCY MARKETS'                     
PEWORKER CSECT                                                                  
         NMOD1 300,PEWORKER                                                     
         LR    R3,RC                                                            
         USING PEPWRKD,R3                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         CLI   MODE,RUNLAST                                                     
         BE    WRKRCLS                                                          
         L     R8,AREC             SET UP WORKER RECORD                         
         USING PEPREC,R8                                                        
         ZIC   RF,PEPKPRD          SET ALPHA PRODUCT CODE                       
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   WRKREC+4(3),1(RF)                                                
         SR    R1,R1                                                            
         ICM   R1,3,PEPRLEN                                                     
         LA    R4,7(R1)                                                         
         STH   R4,WRKREC                                                        
         XC    WRKREC+2(2),WRKREC+2                                             
         LA    RF,WRKREC+7                                                      
         L     RE,AREC                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         CLI   WRKOPNSW,1                                                       
         BE    WRKOPNOK                                                         
         LA    RE,WRKRINDX                                                      
         USING UKRECD,RE                                                        
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG,=C'SM5'                                                 
         MVC   UKSUBPRG,QAGY                                                    
         PACK  DUB,TODAY                                                        
         NI    DUB+6,X'0F'                                                      
         LH    RF,DUB+6                                                         
         SRL   RF,4                                                             
         STC   RF,UKDAY                                                         
         MVI   UKCLASS,C'R'                                                     
         GOTO1 WORKER,DMCB,=C'INDEX',WRKR4K,WRKRINDX                            
         MVI   WRKOPNSW,1                                                       
WRKOPNOK CLI   PROGPROF,0          ANY CLIENT EQUIVALENCE                       
         BE    WRKADD               NO - PUT RECORD                             
         CLI   PROGPROF,C'*'                                                    
         BE    WRKADD               NO - PUT RECORD                             
         MVC   FULL(3),PROGPROF     YES- SET NEW CLIENT                         
         CLI   FULL+1,C'*'                                                      
         BNE   *+8                                                              
         MVI   FULL+1,C' '                                                      
         CLI   FULL+2,C'*'                                                      
         BNE   *+8                                                              
         MVI   FULL+2,C' '                                                      
         GOTO1 CLPACK,DMCB,FULL,HALF                                            
         MVC   WRKREC+10(2),HALF                                                
WRKADD   GOTO1 WORKER,DMCB,=C'ADD',WRKR4K,WRKRINDX,WRKREC                       
         B     WRKRXIT                                                          
         SPACE 2                                                                
WRKRCLS  CLI   WRKOPNSW,1                                                       
         BNE   WRKRXIT                                                          
         GOTO1 WORKER,DMCB,=C'CLOSE',WRKR4K,WRKRINDX                            
         SPACE 2                                                                
WRKRXIT  XMOD1 1                                                                
         LTORG                                                                  
WRKOPNSW DC    X'00'                                                            
WRKRINDX DS    16C                                                              
WRKR4K   DS    4096C                                                            
PEPWRKD  DSECT                                                                  
WRKREC   DS    1600C                                                            
FLTTABC  CSECT                                                                  
         DS    1000C                                                            
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE SPGENPEPSI                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DMWRKRK                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114SPREPMP02 05/01/02'                                      
         END                                                                    
