*          DATA SET REREPMS02S AT LEVEL 004 AS OF 10/22/98                      
*PHASE REMS02,*                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'MODULE TO CREATE MASTER LEVEL STATION CONTROL'                  
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 31JUL98 (JRD) INITIAL ENTRY                                     *             
*                                                                 *             
* QUESTOR = $$SUMRY WILL PRINT STATIONS WITH MORE THAN ONE REP    *             
*                                                                 *             
* QOPTION1 = P  = PRINT RECORDS                                   *             
*                                                                 *             
* QOPTION2 = T  = TEST:  NO PUTRECS                               *             
*            U  = HARD UPDATE                                     *             
*                                                                 *             
*                                                                 *             
* TEMP VERSION WILL BE DELETED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *             
*******************************************************************             
*                                                                               
REMS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REMS02,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         L     RE,ADCONLST                                                      
         L     RE,VCOMFACS-ADCONSD(RE)                                          
         MVC   VHEXOUT,CHEXOUT-COMFACSD(RE)                                     
*                                                                               
         LA    RE,*                                                             
         AHI   RE,IO1-(*-4)                                                     
         ST    RE,AIO1                                                          
         LA    RE,*                                                             
         AHI   RE,IO2-(*-4)                                                     
         ST    RE,AIO2                                                          
         LA    RE,*                                                             
         AHI   RE,IO3-(*-4)                                                     
         ST    RE,AIO3                                                          
         MVC   AIOAREA,AIO1                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXITH                                                            
*                                                                               
         L     R2,ATSARDWA         A(TSAR CONTROL BOLOCK AREA)                  
         USING TSARD,R2                                                         
         XC    0(TSARDL,R2),0(R2)                                               
         MVC   TSABUF,AAGGREG      USE AGGREG BUFFER                            
         MVC   TSAREC,=A(LENAGG)                                                
         OI    TSRECI,TSRVAR       SET VARIABLE LENGTH                          
         LA    R0,TSKLENQ          SET KEY LENGTH                               
         STC   R0,TSKEYL                                                        
         LA    R0,2000             SET MAX RECORD LENGTH                        
         STH   R0,TSRECL                                                        
         MVI   TSOFFACT,TSAINI     INITIALIZE BUFFER                            
         GOTO1 ATSAROFF,(R2)                                                    
         DROP  R2                                                               
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
*----------------------*                                                        
* READ STATION RECORDS *                                                        
*----------------------*                                                        
         XC    KEY,KEY                                                          
K        USING RSTAKEY,KEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
RDREC002 DS    0H                                                               
         CLI   K.RSTAKTYP,X'02'                                                 
         BNE   RDRECX                                                           
         DROP  K                                                                
*                                                                               
         MVC   AIOAREA,AIO1                                                     
         GOTO1 GREC                                                             
         L     R9,AIOAREA                                                       
         USING RSTAREC,R9                                                       
*                                                                               
         CLC   REPCODE,RSTAKREP    SAME REP?                                    
         BE    RDREC020            YES                                          
*                                                                               
         MVC   REPCODE,RSTAKREP                                                 
         XC    KEY,KEY                                                          
K        USING RREPKEY,KEY                                                      
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,REPCODE                                               
         DROP  K                                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIOAREA,AIO2                                                     
         GOTO1 GREC                                                             
         L     R8,AIOAREA                                                       
R        USING RREPREC,R8                                                       
*                                                                               
         CLC   R.RREPMAST,=C'  '   SUBSIDARY REP?                               
         BNH   RDREC010            NO                                           
*                                                                               
         CLC   R.RREPMAST,=X'FFFF' MASTER REP?                                  
         BNE   RDREC014            NO                                           
*                                                                               
         MVC   P(2),REPCODE                                                     
         MVC   P+5(30),=CL30'- MASTER REP WITH STATION -'                       
         MVC   P+40(L'RSTAKSTA),RSTAKSTA                                        
         MVC   P+50(30),=CL30'- STATION DELETED -'                              
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY(L'RSTAKEY),RSTAKEY                                           
         MVC   KEYSAVE,KEY         RESTORE SEQUENCE                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    RDREC008            YES                                          
         CLI   QOPTION2,C'U'       UPDATIVE?                                    
         BNE   RDREC008            NO                                           
*                                                                               
         OI    KEY+L'RSTAKEY,X'80'                                              
         GOTO1 WRITE               DELETE THE KEY                               
*                                                                               
         MVC   AIOAREA,AIO3                                                     
         GOTO1 GREC                                                             
         L     RE,AIOAREA                                                       
M        USING RSTAREC,RE                                                       
         OI    M.RSTACNTL,X'80'                                                 
         GOTO1 PREC                DELETE THE RECORD                            
         DROP  M                                                                
*                                                                               
RDREC008 DS    0H                                                               
         XC    REPCODE,REPCODE                                                  
         B     RDREC042            PROCESS ALL OF THESE                         
*                                                                               
RDREC010 DS    0H                                                               
         CLC   =C'$$SUMRY',QUESTOR                                              
         BE    RDREC012                                                         
*                                                                               
         MVC   P(2),REPCODE                                                     
         MVC   P+5(30),=CL30'- SKIP READING TO NEXT REP -'                      
         GOTO1 REPORT                                                           
*                                                                               
RDREC012 DS    0H                                                               
         XC    KEY,KEY             SKIP READ TO NEXT REP                        
K        USING RSTAKEY,KEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         ZIC   RE,REPCODE+1                                                     
         LA    RE,1(RE)                                                         
         STC   RE,REPCODE+1                                                     
         MVC   K.RSTAKREP,REPCODE                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         XC    REPCODE,REPCODE                                                  
         B     RDREC002                                                         
         DROP  K                                                                
*                                                                               
RDREC014 DS    0H                                                               
         MVC   MASTCODE,R.RREPMAST                                              
         DROP  R                                                                
*                                                                               
RDREC020 DS    0H                                                               
***      CLI   RSTAKSTA+4,C'C'     COMBO?                                       
***      BE    RDREC040            YES - SKIP THIS STATION                      
*                                                                               
         L     R2,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R2            RELOAD TSAR BLOCK                            
*                                                                               
         XC    TLST(L'TSKEY+4),TLST                                             
         MVC   TSKMAST,MASTCODE                                                 
         MVC   TSKSTA,RSTAKSTA                                                  
*                                                                               
         LA    R0,TLST+2                                                        
         ST    R0,TSAREC                                                        
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         GOTO1 ATSAROFF,(R2)                                                    
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    RDREC022            YES - ADD THE RECORD                         
*                                                                               
         CLC   TSKMAST,MASTCODE    CORRECT REP?                                 
         BNE   RDREC022            NO - ADD THE RECORD                          
         CLC   TSKSTA,RSTAKSTA     CORRECT REP?                                 
         BE    RDREC030            RECORD FOUND - UPDATE IT                     
*                                                                               
RDREC022 DS    0H                                                               
         XC    TLST(L'TSKEY+4),TLST                                             
         MVC   TSKMAST,MASTCODE                                                 
         MVC   TSKSTA,RSTAKSTA                                                  
         XC    TSREC(255),TSREC                                                 
*                                                                               
         MVC   TSRBLEN,=H'8'       ADD FIRST INFO BLOCK                         
         LA    RE,TSKEY+TSROVQ                                                  
         XC    0(8,RE),0(RE)                                                    
         MVC   0(2,RE),REPCODE                                                  
         MVC   2(3,RE),RSTASTRT                                                 
         MVC   5(3,RE),RSTAEND                                                  
*                                                                               
         LA    R0,TLST+2                                                        
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         SR    R0,R0                                                            
         ICM   R0,3,TSRBLEN                                                     
         AHI   R0,TSROVQ+2                                                      
         STCM  R0,3,TSLEN                                                       
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    RDREC040                                                         
         DC    H'0'                NEED TO EXPAND BUFFER                        
*                                                                               
RDREC030 DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,3,TSRBLEN                                                     
         LA    R0,8(RF)                                                         
         STCM  R0,3,TSRBLEN                                                     
         LA    RE,TSKEY+TSROVQ(RF)                                              
*                                                                               
         OC    RSTAEND,RSTAEND     ACTIVE?                                      
         BNZ   RDREC032            NO - ADD AT THE END                          
*                                                                               
         LA    RE,TSKEY+TSROVQ     YES - ADD AT THE START                       
         BCTR  RF,0                                                             
         XC    WORK,WORK                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),WORK                                                     
*                                                                               
RDREC032 DS    0H                                                               
         XC    0(8,RE),0(RE)                                                    
         MVC   0(2,RE),REPCODE                                                  
         MVC   2(3,RE),RSTASTRT                                                 
         MVC   5(3,RE),RSTAEND                                                  
*                                                                               
         LA    R0,TLST+2                                                        
         ST    R0,TSAREC           UPDATE RECORD                                
         SR    R0,R0                                                            
         ICM   R0,3,TSRBLEN                                                     
         AHI   R0,TSROVQ+2                                                      
         STCM  R0,3,TSLEN                                                       
         MVI   TSOFFACT,TSAPUT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    RDREC040                                                         
         DC    H'0'                WHAT?                                        
         DROP  R2                                                               
*                                                                               
RDREC040 DS    0H                                                               
         MVC   KEY(L'RSTAKEY),RSTAKEY                                           
         MVC   KEYSAVE,KEY         RESTORE SEQUENCE                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RDREC042 DS    0H                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
         B     RDREC002                                                         
         DROP  R9                                                               
*                                                                               
RDRECX   DS    0H                                                               
*                                                                               
*--------------------*                                                          
* PRINT TSAR RECORDS *                                                          
*--------------------*                                                          
         L     R2,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R2            RELOAD TSAR BLOCK                            
*                                                                               
         LA    R0,TLST+2           GET TO THE START OF THE FILE                 
         ST    R0,TSAREC                                                        
         XC    TLST(L'TSKEY+4),TLST                                             
         MVI   TSOFFACT,TSARDH                                                  
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
PRREC010 DS    0H                                                               
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    PRRECX              YES - EXIT                                   
*                                                                               
         MVC   P+4(2),TSKMAST                                                   
         MVC   P+10(5),TSKSTA                                                   
         MVI   P+16,C'-'                                                        
         MVC   P+17(5),P+16                                                     
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,TSRBLEN                                                     
         LA    RF,8                                                             
         DR    R0,RF                                                            
         LA    R3,TSKEY+TSROVQ                                                  
         LTR   R0,R1                                                            
         BNP   PRREC024                                                         
*                                                                               
         CLC   =C'$$SUMRY',QUESTOR                                              
         BNE   *+12                                                             
         CHI   R0,1                                                             
         BNH   PRREC024                                                         
*                                                                               
PRREC020 DS    0H                                                               
         MVC   P+23(2),0(R3)                                                    
         GOTO1 DATCON,DMCB,(3,2(R3)),(8,P+34)                                   
         OC    5(3,R3),5(R3)                                                    
         BZ    PRREC022                                                         
         GOTO1 DATCON,DMCB,(3,5(R3)),(8,P+44)                                   
*                                                                               
PRREC022 DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,8(R3)                                                         
         BCT   R0,PRREC020                                                      
*                                                                               
PRREC024 DS    0H                                                               
         MVI   TSOFFACT,TSANXT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         B     PRREC010                                                         
*                                                                               
PRREC030 DS    0H                                                               
         DROP  R2                                                               
*                                                                               
PRRECX   DS    0H                                                               
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    *+12                YES                                          
         CLI   QOPTION2,C'U'       UPDATIVE?                                    
         BNE   UPRECX              NO                                           
*                                                                               
*-------------------------------*                                               
* UPDATE MASTER STATION RECORDS *                                               
*-------------------------------*                                               
         L     R2,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R2            RELOAD TSAR BLOCK                            
*                                                                               
         LA    R0,TLST+2           GET TO THE START OF THE FILE                 
         ST    R0,TSAREC                                                        
         XC    TLST(L'TSKEY+4),TLST                                             
         MVI   TSOFFACT,TSARDH                                                  
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
UPREC010 DS    0H                                                               
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    UPRECX              YES - EXIT                                   
*                                                                               
         L     R0,AIO3                                                          
         LHI   R1,IO2-IO1          LENGTH OF IO AREA                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R9,AIO3                                                          
         USING RSTAREC,R9                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,TSKMAST                                                 
         MVC   RSTAKSTA,TSKSTA                                                  
         MVC   RSTALEN,=Y(RSTAELEM-RSTAKEY)                                     
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,TSRBLEN                                                     
         LA    RF,8                                                             
         DR    R0,RF                                                            
         LTR   R0,R1                                                            
         BNP   UPREC040                                                         
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,TSRBLEN        BUMP THROUGH BACKWARDS                       
         LA    R3,TSKEY+TSROVQ-8(R3)                                            
*                                                                               
UPREC020 DS    0H                                                               
E        USING RSTAMCEL,WORK                                                    
         XC    WORK,WORK                                                        
         MVI   E.RSTAMCEC,RSTAMCHQ                                              
         OC    5(3,R3),5(R3)       LEAVE DATE?                                  
         BNZ   *+8                 YES                                          
         MVI   E.RSTAMCEC,RSTAMCCQ NO - SET AS CURRENT REP                      
         MVI   E.RSTAMCLN,RSTAMCLQ LENGTH                                       
         MVC   E.RSTAMCRC,0(R3)    REPCODE                                      
         MVC   E.RSTAMCJD,2(R3)    JOIN DATE                                    
         MVC   E.RSTAMCLD,5(R3)    LEAVE DATE                                   
         DROP  E                                                                
*                                                                               
         GOTO1 =V(RECUP),DMCB,(C'R',RSTAREC),WORK,RSTAELEM                      
*                                                                               
         AHI   R3,-8               NEXT ELEMENT                                 
         BCT   R0,UPREC020                                                      
*                                                                               
         CLI   RSTAELEM,RSTAMCCQ   CURRENT ELEMENT?                             
         BE    UPREC040            YES                                          
*                                                                               
         XC    WORK,WORK           NO - CREATE A PREVIOS                        
         SR    R4,R4                                                            
         LA    RE,RSTAELEM                                                      
E        USING RSTAMCEL,RE                                                      
UPREC030 DS    0H                                                               
         CLI   0(RE),RSTAMCHQ      HISTORY ELEMENT?                             
         BNE   UPREC034            NO - FINISHED                                
*                                                                               
         CLC   E.RSTAMCLD,WORK     LATER LEAVE DATE?                            
         BNH   *+12                NO                                           
         LR    R4,RE               SAVE ELEMENT ADDRESS                         
         MVC   WORK(3),E.RSTAMCLD  SAVE LEAVE DATE                              
*                                                                               
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     UPREC030                                                         
         DROP  E                                                                
*                                                                               
UPREC034 DS    0H                                                               
         OC    WORK(3),WORK                                                     
         BNZ   *+6                                                              
         DC    H'0'                THERE HAD TO BE ONE ELEMENT                  
*                                                                               
         XC    WORK,WORK                                                        
         ZIC   RE,1(R4)            SAVE AND DELETE MOST RECENT LEAVE            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4)                                                    
*                                                                               
         GOTO1 =V(RECUP),DMCB,(C'R',RSTAREC),(R4),0                             
*                                                                               
         MVI   WORK,RSTAMCPQ       ADD AS PREVIOS REP                           
         GOTO1 =V(RECUP),DMCB,(C'R',RSTAREC),WORK,RSTAELEM                      
*                                                                               
UPREC040 DS    0H                                                               
         CLI   QOPTION1,C'P'       PRINT RECORDS?                               
         BNE   UPREC042            NO                                           
*                                                                               
         GOTO1 PRNTREC,RSTAREC                                                  
*                                                                               
UPREC042 DS    0H                                                               
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    UPREC060            YES                                          
         CLI   QOPTION2,C'U'       UPDATIVE?                                    
         BNE   UPREC060            NO                                           
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSTAKEY,KEY                                                      
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,TSKMAST                                                 
         MVC   RSTAKSTA,RSTAKSTA                                                
         DROP  K                                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   UPREC050            RECORD NOT FOUND - ADDREC                    
*                                                                               
         NI    KEY+L'RSTAKEY,X'FF'-X'80'                                        
         GOTO1 WRITE               UNDELETE KEY                                 
*                                                                               
         MVC   AIOAREA,AIO1                                                     
         OI    DMINBTS,X'08'                                                    
         GOTO1 GREC                                                             
*                                  IF HISTORY NEEDED MUCK AROUND HERE           
         MVC   AIOAREA,AIO3                                                     
         GOTO1 PREC                                                             
*                                                                               
         B     UPREC060                                                         
*                                                                               
UPREC050 DS    0H                                                               
         MVC   AIOAREA,AIO3                                                     
         GOTO1 AREC                                                             
*                                                                               
UPREC060 DS    0H                                                               
         MVI   TSOFFACT,TSANXT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         B     UPREC010                                                         
*                                                                               
*                                                                               
UPRECX   DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
EXITH    LTR   RB,RB                                                            
         B     EXIT                                                             
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXIT     DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
*          DATA SET REREPAI02  AT LEVEL 108 AS OF 03/17/98                      
***********************************************************************         
* PRINT A REP RECORD FOR DEBUGGING                                              
***********************************************************************         
PRNTREC  NTR1                                                                   
         LR    R6,R1                                                            
         MVC   P(12),=C'RECORD DUMP:'                                           
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R6),P+2,34                                         
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,34(R6)           FIRST ELEMENT                                
PRNTR10  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    PRNTR20                                                          
*                                                                               
         ZIC   R2,1(R6)                                                         
         LA    R4,0(R6)                                                         
         LA    R0,P+5                                                           
PRNTR12  DS    0H                                                               
         LR    R3,R2                                                            
         CH    R3,=H'60'                                                        
         BNH   *+8                                                              
         LA    R3,60                                                            
*                                                                               
         SR    R2,R3                                                            
         GOTO1 HEXOUT,DMCB,(R4),(R0),(R3)                                       
         GOTO1 REPORT                                                           
*                                                                               
         AR    R4,R3                                                            
         LA    R0,P+10                                                          
         LTR   R2,R2                                                            
         BNZ   PRNTR12                                                          
*                                                                               
         ZIC   R2,1(R6)                                                         
         LA    R6,0(R2,R6)                                                      
         B     PRNTR10                                                          
*                                                                               
PRNTR20  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'MODULE WORKING STORAGE'                                         
RELO     DC    F'0'                                                             
VHEXOUT  DS    A                                                                
AIOAREA  DS    A                                                                
AIO1     DS    A                                                                
AIO2     DS    A                                                                
AIO3     DS    A                                                                
REPCODE  DC    CL2'  '                                                          
MASTCODE DC    CL2'  '                                                          
COMMAND  DS    CL8                                                              
*                                                                               
**************************                                                      
* TSAR RECORD DEFINITION *                                                      
**************************                                                      
TLST     EQU   *                                                                
TSNUM    DS    XL2                                                              
TSLEN    DS    XL2                                                              
TSKEY    EQU   *                                                                
TSKMAST  DS    CL2                 MASTER REPCODE                               
TSKSTA   DS    CL5                 STATION CALL LETTERS                         
TSKLENQ  EQU   *-TSKEY             KEY LENGTH                                   
*                                                                               
TSREC    EQU   *                                                                
TSRBLEN  DS    XL2                 LENGTH OF ELEMENT BUFFER                     
TSROVQ   EQU   *-TSKEY             RECORD LENGTH                                
         DS    XL(2000-TSROVQ)     ROOM FOR THE REST OF THE RECORD              
*                                                                               
IO1      DS    4096X                                                            
IO2      DS    4096X                                                            
IO3      DS    4096X                                                            
         EJECT                                                                  
*  INCLUDE REGENALL                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*  INCLUDE DDCOMFACS                                                            
*  INCLUDE REREPTSAR                                                            
*  INCLUDE DDTSARD                                                              
         PRINT OFF                                                              
       ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REREPTSAR                                                      
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004REREPMS02S10/22/98'                                      
         END                                                                    
