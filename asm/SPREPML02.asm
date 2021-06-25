*          DATA SET SPREPML02  AT LEVEL 028 AS OF 11/21/19                      
*PHASE SPML02T                                                                  
                                                                                
*==============================================================                 
* QOPT1 =   ' ' TO LOCK PURCHASED ONLY (OR N FOR NO GOALS)                      
*            Y  TO LOCK PURCHASED AND GOALS                                     
*            G  TO LOCK GOALS ONLY                                              
*==============================================================                 
                                                                                
         TITLE 'SPREPML02-LOCKIN REPORT'                                        
         PRINT NOGEN                                                            
SPML02   CSECT                                                                  
         NMOD1 0,SPML02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         ST    R5,RELO                                                          
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMWK,=F'60'                                                  
         MVC   MEDLCHNK,=F'128'                                                 
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         XC    MEDNUMPE,MEDNUMPE                                                
         MVI   MEDEXTDM,4                                                       
         MVI   RQPRDEST,C'Y'                                                    
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         DROP  RE                                                               
         MVI   RCSUBPRG,1                                                       
         L     R2,=V(BUFFALOC)                                                  
         A     R2,RELO                                                          
         ST    R2,BUFFBUFF                                                      
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',(R2)                                        
         XC    RPTSWS,RPTSWS                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M21                                                              
*                                                                               
         OI    RQOPTS,RQOPTS_POST    FLAG TO INDICATE SPOT POSTING              
*                                                                               
         CLC   =C'LOCK=G',QUESTOR                                               
         BNE   *+8                                                              
         MVI   QOPT1,C'G'                                                       
*                                                                               
         CLC   =C'LOCK=B',QUESTOR                                               
         BNE   *+8                                                              
         MVI   QOPT1,C'Y'                                                       
*                                                                               
         CLC   =C'LOCK=P',QUESTOR                                               
         BNE   *+8                                                              
         MVI   QOPT1,C' '                                                       
*                                                                               
         MVI   FCRDBUYS,C'Y'                                                    
         CLI   QOPT1,C'G'          TEST GOALS ONLY                              
         BNE   *+8                                                              
         MVI   FCRDBUYS,C'N'                                                    
*                                                                               
         CLC   QEST(2),=C'NO'      FJWTSA                                       
         BNE   *+10                                                             
         MVC   QEST,=C'ALL'                                                     
*                                                                               
         MVI   ESTSW,C'N'                                                       
         CLI   QRERATE,C'A'        NO LONGER SUPPORTED                          
         BNE   *+8                                                              
         MVI   QRERATE,C' '                                                     
         B     EXIT                                                             
         EJECT                                                                  
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
         GOTO1 DATCON,DMCB,QSTART,(X'02',BQSTARTP)                              
         GOTO1 DATCON,DMCB,QEND,(X'02',BQENDP)                                  
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
*                                                                               
         L     R2,ADEST                                                         
         USING ESTHDR,R2                                                        
*                                                                               
         MVC   SVQDATES,QSTART                                                  
         MVC   QSTART,ESTART                                                    
         MVC   QEND,EEND                                                        
         CLI   EOWSDAY,0           OUT OF WEEK ROT. START DAY                   
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY  SET IT IF THERE                           
         MVI   MEDDAILY,0          RESET DAILY ESTIMATE FLAG                    
         CLI   EDAILY,C'Y'         DAILY ESTIMATE?                              
         BNE   *+8                 NO                                           
         MVI   MEDDAILY,C'Y'       YES                                          
         DROP  R2                                                               
*                                                                               
         MVC   MEDNUMWK,=F'60'                                                  
         L     RE,MEDBUFF                                                       
         MVC   MEDNUMMO,=F'13'                                                  
         L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R2)                                      
         DROP  RE                                                               
*                                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
*                                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
                                                                                
         MVI   MONTHLY,0                                                        
         MVC   QSTART(12),SVQDATES                                              
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVI   RCSUBPRG,1                                                       
         CLI   MONTHLY,1                                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
         B     EXIT                                                             
         EJECT                                                                  
M3       CLI   MODE,PROCBUY                                                     
         BNE   M4                                                               
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
*                                                                               
         XC    MYBUFIO,MYBUFIO                                                  
         MVC   BFEST,BUYKEST                                                    
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         BRAS  RE,SETSLN                                                        
         STC   R0,BFSEC                                                         
         DROP  R6                                                               
         XC    PSLIST,PSLIST                                                    
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         LA    RE,PSLIST                                                        
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-12                                                             
         MVC   0(2,RE),=X'FFFF'                                                 
*                                                                               
M31      LA    RE,PSLIST                                                        
*                                                                               
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
         LA    R2,PSLIST                                                        
M323     CLC   0(2,R2),=X'FFFF'    END                                          
         BE    M34                                                              
         CLI   0(R2),0                                                          
         BNE   *+12                                                             
M323A    LA    R2,2(R2)                                                         
         B     M323                                                             
*                                                                               
         L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
*                                                                               
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         LA    R7,2                                                             
         CLI   QRERATE,C' '                                                     
         BE    MA322                                                            
MA321    LA    R7,3                                                             
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R7,1(R7)                                                         
         CLI   QRERATE,C'I'                                                     
         BNE   *+8                                                              
         LA    R7,3(R7)                                                         
MA322    GOTO1 MEDGETBY,DMCB,(RA),(R7)                                          
         EJECT                                                                  
*============================================================                   
* EXTRACT AND POST ROUTINES                                                     
*============================================================                   
                                                                                
         L     R5,MEDAFRST                                                      
M324     L     R6,MEDALAST                                                      
         CR    R5,R6                                                            
         BH    M323A                                                            
         CLI   0(R5),0                                                          
         BNE   M328                                                             
*                                                                               
M326     LA    R5,12(R5)                                                        
         B     M324                                                             
*                                                                               
M328     L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYSPT,MEDBYSPT                                                
         BZ    M326                                                             
         MVC   BFSPILL,MEDSPILL    ISOLATE THE SPILL                            
         MVC   BFMED,QMED                                                       
         MVC   BFCLT,QCLT                                                       
         MVC   BFPRD,MEDBRAND                                                   
*                                                                               
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         MVC   BFEST,BUYKEST                                                    
         MVC   BFMKT(2),BMKT                                                    
         DROP  RE                                                               
*                                                                               
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         MVC   BFDPT,BDDAYPT                                                    
         DROP  RE                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         BRAS  RE,SETSLN                                                        
         STC   R0,BFSLN                                                         
*                                                                               
         MVC   BFSTART(4),0(R5)                                                 
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
*                                                                               
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',(R7),MYBUFIO                                
         B     M326                                                             
*                                                                               
M34      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
M4       CLI   MODE,MKTLAST                                                     
         BNE   M5                                                               
         L     RE,=A(ELSAVE)                                                    
         ST    RE,ELADDR                                                        
         XC    TOTLKD,TOTLKD       CLEAR LOCAL TOTALS                           
         XC    TOTLKSPT,TOTLKSPT                                                
         XC    TOTLK1,TOTLK1                                                    
         XC    MYBUFIO,MYBUFIO                                                  
         MVC   BFMED,QMED                                                       
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R7),MYBUFIO,1                             
         TM    DMCB+8,X'80'                                                     
         BO    EXIT                                                             
         MVI   ENDSW,0                                                          
         B     M42A                                                             
*                                                                               
M41      L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R7),MYBUFIO,1                              
*                                                                               
M42      TM    DMCB+8,X'80'        CHECK FOR KEY BREAKS                         
         BO    M44                                                              
         CLC   PREVSLN,BFSLN                                                    
         BNE   M44                                                              
         CLC   PREVDPT,BFDPT                                                    
         BNE   M44                                                              
         CLC   PREVBRND,BFPRD                                                   
         BNE   M44                                                              
         CLC   PREVEST,BFEST                                                    
         BNE   M44                                                              
         CLC   PREVSEC,BFSEC                                                    
         BNE   M44                                                              
*                                                                               
M42A     CLI   ENDSW,0                                                          
         BNE   EXIT                                                             
         MVC   PREVSLN,BFSLN                                                    
         MVC   PREVDPT,BFDPT                                                    
         MVC   PREVBRND,BFPRD                                                   
         MVC   PREVSEC,BFSEC                                                    
         MVC   PREVEST,BFEST                                                    
         L     R6,ELADDR                                                        
         USING GLKELEM,R6                                                       
         XC    0(21,R6),0(R6)      BUILD LOCKIN ELEMENT                         
*                                                                               
         MVI   GLKCOD,X'31'                                                     
         CLI   BFSPILL,C'Y'        IF DATA IS NOT SPILL - CONTINUE              
         BNE   *+8                                                              
         MVI   GLKCOD,X'33'        MAKE A SPILL ELEMENT                         
*                                                                               
         MVC   PRECIS,=F'10'       SET UP FOR WEEKLY (DIME) PREC                
         MVC   DPRECIS,=F'1'       SET UP FOR WEEKLY (DEC) PREC                 
         CLI   MONTHLY,1                                                        
         BNE   M42B                                                             
         MVC   PRECIS,=F'100'      SET UP FOR MONTHLY (DOLLAR) PREC             
         MVC   DPRECIS,=F'10'      SET UP FOR MONTHLY (INTGER) PREC             
         NI    GLKCOD,X'FE'        AND ELEMENT CODES                            
*                                                                               
M42B     MVI   GLKLEN,19                                                        
         MVC   GLKDAT,BFSTART                                                   
         MVC   GLKPRD,BFPRD                                                     
         MVC   GLKTSC,BFSLN                                                     
         MVC   GLKSPT,BFSPOTS+2                                                 
*                                                                               
         ICM   RF,15,BFDOL         ADJUST TO PROPER PREC.                       
         M     RE,=F'2'                                                         
         D     RE,PRECIS                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         STCM  RF,15,GLKDLR                                                     
*                                                                               
         SR    R1,R1                                                            
         IC    R1,GLKPRD           FIND PRIMARY DEMO                            
         BCTR  R1,0                                                             
         MH    R1,PRDBUFLN                                                      
         A     R1,PRDBUFF                                                       
         MVC   GLKDEMN(3),28(R1)   MOVE DEMO CODE                               
*                                                                               
         L     R0,DPRECIS          GET PRECISION                                
*                                                                               
         LA    R1,GLKDEMN          POINT R1 TO DEMO CODE                        
         BRAS  RE,ISRATING                                                      
         JNE   M42C                                                             
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC   TEST 2-DEC RTGS                             
         BZ    M42X                 NO                                          
         B     M42D                                                             
*                                                                               
M42C     TM    RQOPTS,RQOPTS_2DECIMP    TEST 2-DEC IMPS                         
         BZ    M42X                                                             
*                                                                               
M42D     MVI   TWODEC,C'Y'         SET FLAG THIS IS A 2-DEC REQUEST             
         MHI   R0,10               SCALE PRECISION FOR 2-DEC                    
*                                                                               
M42X     ICM   RF,15,BFDEM1        ADJUST TO PROPER PREC.                       
         M     RE,=F'2'                                                         
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         STCM  RF,15,BFDEM1                                                     
         STCM  RF,15,GLKDEM+3                                                   
         LA    R6,19(R6)                                                        
         ST    R6,ELADDR                                                        
         B     M41                                                              
         SPACE 2                                                                
M44      MVC   ENDSW,DMCB+8        SAVE EOF SWITCH                              
         L     RE,=A(ELSAVE)       SET ADDRESS FOR NEXT RECORD                  
         ST    RE,ELADDR                                                        
*                                                                               
         XC    KEY,KEY             BUILD GOAL KEY                               
         LA    R8,KEY                                                           
         USING GOALREC,R8                                                       
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,BAGYMD                                                    
         MVC   GKEYCLT,BCLT                                                     
         MVC   GKEYPRD,PREVBRND                                                 
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,PREVEST                                                  
         MVC   GKEYDPT,PREVDPT                                                  
         MVC   GKEYSLN,PREVSLN                                                  
         MVC   GKEYSEC,PREVSEC                                                  
*                                                                               
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'      SUPPRESS REC IS DELETED TEST                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   M44B                                                             
*                                                                               
         TM    KEY+13,X'80'        TEST KEY IS DELETED                          
         BZ    M44A                                                             
         NI    KEY+13,X'7F'        UNSET DELETED FLAG                           
         GOTO1 WRITE                                                            
*                                                                               
M44A     GOTO1 GETGOAL                                                          
         L     RE,ADGOAL                                                        
***DEBUG                                                                        
         CLI   0(RE),2                                                          
         BE    *+8                                                              
         B     *+4                                                              
***DEBUG                                                                        
         NI    15(RE),X'7F'        UNSET DELETED FLAG IF ON                     
         MVC   WRCMND,=C'PUTREC'                                                
         B     M45                                                              
*                                                                               
M44B     MVC   WRCMND,=C'ADDREC'                                                
         L     R0,ADGOAL                                                        
         LHI   R1,2000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         BRAS  RE,BLDREC                                                        
         B     M48                                                              
                                                                                
* DELETE EXISTING LOCKING ELEMENTS                                              
                                                                                
M45      L     R7,ADGOAL                                                        
         LA    R6,24(R7)                                                        
         SR    R0,R0                                                            
*                                                                               
M452     CLI   0(R6),0                                                          
         BE    M48                                                              
         CLI   0(R6),X'30'                                                      
         BE    M456                                                             
         CLI   0(R6),X'31'                                                      
         BE    M456                                                             
         CLI   0(R6),X'32'                                                      
         BE    M456                                                             
         CLI   0(R6),X'33'                                                      
         BE    M456                                                             
*                                                                               
M454     IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     M452                                                             
*                                                                               
M456     CLC   2(2,R6),BQENDP                                                   
         BH    M454                                                             
         CLC   2(2,R6),BQSTARTP                                                 
         BL    M454                                                             
         GOTO1 RECUP,DMCB,(R7),(R6)                                             
         B     M452                                                             
         EJECT                                                                  
M48      L     R6,ADGOAL                                                        
*                                                                               
         CLI   0(R6),2             GOAL RECORD SHOULD BE THERE !                
         BE    *+8                                                              
         BRAS  RE,BLDREC           JUST IN CASE THIS HAPPENS                    
*                                                                               
         L     R6,ADGOAL                                                        
         LA    R6,24(R6)                                                        
         USING GDELEM,R6                                                        
         MVC   GDLKBUY,TODAYP      SET LAST BUY LOCKIN  DATE                    
         DROP  R6                                                               
*                                                                               
M481     CLI   0(R6),0             FIND SLOT FOR INSERT                         
         BE    M4831                                                            
         CLI   0(R6),X'30'                                                      
         BE    M483                                                             
         CLI   0(R6),X'31'                                                      
         BE    M483                                                             
         CLI   0(R6),X'32'                                                      
         BE    M483                                                             
         CLI   0(R6),X'33'                                                      
         BE    M483                                                             
*                                                                               
M482     ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     M481                                                             
*                                                                               
M483     CLC   GLKDAT,BQSTARTP                                                  
         BL    M482                                                             
M4831    L     R8,=A(ELSAVE)       ADD ELEMENTS TO RECORD                       
*                                                                               
M484     GOTO1 RECUP,DMCB,ADGOAL,(R8),(R6)                                      
         LA    R8,19(R8)                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R8),0             END OF ELEMENTS                              
         BNE   M484                 NO - ADD NEXT ELEMENT                       
         EJECT                                                                  
         GOTO1 MEDGETLK,DMCB,(RA)                                               
         MVI   P,0                                                              
         CLC   QUESTOR(5),=C'TEST1'                                             
         BNE   M4841                                                            
         MVC   P(6),WRCMND                                                      
         L     R6,ADGOAL                                                        
         GOTO1 HEXOUT,DMCB,(R6),P+7,60,0,0                                      
*                                                                               
M4841    GOTO1 REPORT                                                           
         LA    R8,KEY                                                           
         USING GOALREC,R8                                                       
         ZIC   RF,GKEYPRD                                                       
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   P(3),1(RF)                                                       
         MVC   P+3(20),4(RF)                                                    
         EDIT  GKEYEST,(3,P+29)                                                 
         MVC   P+34(1),GKEYDPT                                                  
         EDIT  GKEYSEC,(3,P+38)                                                 
         EDIT  GKEYSLN,(3,P+42)                                                 
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
         OC    MEDLKSPT,MEDLKSPT                                                
         BZ    M4851                                                            
         L     RE,MEDLKD                                                        
         A     RE,TOTLKD                                                        
         ST    RE,TOTLKD                                                        
         L     RE,MEDLKSPT                                                      
         A     RE,TOTLKSPT                                                      
         ST    RE,TOTLKSPT                                                      
         L     RE,MEDLK1                                                        
         A     RE,TOTLK1                                                        
         ST    RE,TOTLK1                                                        
         GOTO1 DATCON,DMCB,(2,(R5)),(8,P+46)                                    
         EDIT  MEDLKD,(12,P+55),2,COMMAS=YES                                    
         EDIT  MEDLKSPT,(5,P+68)                                                
         CLI   TWODEC,C'Y'                2 DECIMAL REQUEST?                    
         BNE   M4851A                     NO                                    
         EDIT  MEDLK1,(8,P+75),2          YES - PRINT IN 2 DECIMAL!             
         B     M4851B                                                           
*                                                                               
M4851A   EDIT  MEDLK1,(8,P+75),1                                                
*                                                                               
M4851B   OC    P,SPACES                                                         
         GOTO1 REPORT                                                           
         B     M4851                                                            
M4852    CLI   RCWRITE,C'Y'                                                     
         BNE   M4853                                                            
         CLC   WRCMND,=C'ADDREC'                                                
         BNE   M4852A                                                           
         MVC   AREC,ADGOAL                                                      
         GOTO1 ADD                                                              
         B     M4853                                                            
M4852A   CLC   WRCMND,=C'PUTREC'                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 PUTGOAL                                                          
         SPACE 2                                                                
M4853    CLI   ENDSW,0                                                          
         BE    M42A                                                             
         MVC   P+46(7),=C'*TOTAL*'             PRINT TOTALS                     
         EDIT  TOTLKD,(12,P+55),2,COMMAS=YES                                    
         EDIT  TOTLKSPT,(5,P+68)                                                
         CLI   TWODEC,C'Y'                2 DECIMAL REQUEST?                    
         BNE   M4854                      NO                                    
         EDIT  TOTLK1,(8,P+75),2          YES - PRINT IN 2 DECIMAL!             
         B     M4855                                                            
*                                                                               
M4854    EDIT  TOTLK1,(8,P+75),1                                                
*                                                                               
M4855    GOTO1 REPORT                                                           
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R7)                                      
         SPACE 2                                                                
M5       CLI   MODE,MKTFRST                                                     
         BNE   M6                                                               
         BRAS  RE,CALLAUTH         CALL SPAUTH                                  
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         DROP  R9                                                               
         EJECT                                                                  
*===========================================================                    
* BUILD NEW GOAL RECORD                                                         
*===========================================================                    
                                                                                
BLDREC   NTR1                                                                   
                                                                                
         MVC   KEY,KEYSAVE                                                      
         L     R8,ADGOAL                                                        
         MVC   GOALREC(13),KEYSAVE                                              
         MVC   GAGYALPH,AGENCY                                                  
         MVC   GLENGTH,=H'24'                                                   
         DROP  R8                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R8,ELEM                                                          
         USING GDELEM,R8                                                        
         MVI   GOCODE,X'20'                                                     
         MVI   GOLEN,76                                                         
         MVC   GBUYNAME,=C'*DDS LOCKIN*'                                        
         GOTO1 DATCON,DMCB,(3,TODAYB),(2,GREDATE)                               
         MVC   GACTDATE,GREDATE                                                 
         DROP  R8                                                               
         L     R8,ADGOAL                                                        
         LA    R7,24(R8)                                                        
         GOTO1 RECUP,DMCB,(R8),ELEM,(R7)                                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
M6       CLI   MODE,PROCGOAL       PURGE DATA FOR RELOCKIN                      
         BNE   EXIT                                                             
                                                                                
* SAVE THE GOAL RECORD IN ELSAVE                                                
                                                                                
         L     RE,ADGOAL                                                        
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         GET GOAL RECORD LENGTH                       
         L     R0,=A(ELSAVE)                                                    
         LA    R1,2(RF)                                                         
         MVCL  R0,RE                                                            
                                                                                
* DELETE EXISTING LOCKIN ELEMENTS                                               
                                                                                
         MVC   AREC,ADGOAL                                                      
M65      L     R7,ADGOAL                                                        
         LA    R6,24(R7)                                                        
         SR    R0,R0                                                            
*                                                                               
M651     CLI   0(R6),0             CHECK FOR A LOCKIN ELEMENT                   
         BE    M70                                                              
         CLI   0(R6),X'30'                                                      
         BE    M67                                                              
         CLI   0(R6),X'31'                                                      
         BE    M67                                                              
         CLI   0(R6),X'32'                                                      
         BE    M67                                                              
         CLI   0(R6),X'33'                                                      
         BE    M67                                                              
*                                                                               
         CLI   QOPT1,C'G'          TEST LOCK GOALS                              
         BE    *+12                                                             
         CLI   QOPT1,C'Y'          TEST LOCKING GOALS AND BUYS                  
         BNE   *+12                                                             
         CLI   0(R6),X'A1'         TEST GOAL WEEKLY LOCKIN                      
         BE    M67                                                              
*                                                                               
M66      ICM   R0,1,1(R6)          TRY NEXT ELEMENT                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     M651                                                             
*                                                                               
M67      CLC   2(2,R6),BQENDP       CHECK WITHIN REQ DATES                      
         BH    M66                                                              
         CLC   2(2,R6),BQSTARTP                                                 
         BL    M66                                                              
         GOTO1 RECUP,DMCB,(R7),(R6)                                             
         B     M651                                                             
*                                                                               
M70      LR    R4,R6               SAVE EOR ADDRESS                             
         CLI   QOPT1,C'G'          TEST LOCK GOALS ONLY                         
         BE    *+12                                                             
         CLI   QOPT1,C'Y'          TEST LOCK GOALS AND BUYS                     
         BNE   M80                                                              
                                                                                
* COPY X'21' ELEMS TO X'A1' ELEMS                                               
                                                                                
         L     R7,ADGOAL                                                        
         LA    R6,24(R7)                                                        
         USING GDELEM,R6                                                        
         DROP  R6                                                               
         SR    R0,R0                                                            
*                                                                               
M72      MVI   USRSW1,C'N'         SET KEY NOT PRINTED YET                      
*                                                                               
M74      ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0                                                          
         BE    M80                                                              
         CLI   0(R6),X'21'                                                      
         BNE   M72                                                              
*                                                                               
         USING GLEMENT,R6                                                       
         CLC   GLWEEK,BQENDP       CHECK WITHIN REQ DATES                       
         BH    M74                                                              
         CLC   GLWEEK,BQSTARTP                                                  
         BL    M74                                                              
*                                                                               
         MVC   ELEM(12),0(R6)      MOVE ELEMENT                                 
         OI    ELEM,X'80'          SET NEW ELEMENT CODE                         
         GOTO1 RECUP,DMCB,(R7),ELEM,(R4)                                        
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0               POINT TO NEW EOR                             
         BRAS  RE,PRTGOAL                                                       
         B     M74                 AND CONTINUE                                 
*                                                                               
M80      CLI   RCWRITE,C'Y'                                                     
         BNE   M80X                                                             
                                                                                
* SEE IF RECORD CHANGED BEFORE WRITING                                          
                                                                                
         L     RE,=A(ELSAVE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R4,ADGOAL                                                        
         SR    R5,R5                                                            
         ICM   R5,3,13(R4)                                                      
         CR    RF,R5                                                            
         BNE   M80A                IF LENGTHS DIFFER, CHANGED                   
         CLCL  R4,RE                                                            
         BE    M80X                                                             
*                                                                               
M80A     L     R6,ADGOAL                                                        
         LA    R6,24(R6)                                                        
         USING GDELEM,R6                                                        
         MVC   GDLKGOAL,TODAYP     SET LAST GOAL LOCKIN DATE                    
         DROP  R6                                                               
         GOTO1 PUTGOAL                                                          
*                                                                               
M80X     B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET SPMEDGETBY AT LEVEL 101 AS OF 05/22/19                      
                                                                                
*==============================================================                 
* SUBROUTINE RETURNS WITH CC EQ IF DEMO AT 0(R1) IS A RATING                    
* R1 POINTS TO 3-BYTE DEMO CODE ON ENTRY                                        
*==============================================================                 
                                                                                
ISRATING NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R1),0             TEST NORMALIZED NON-T DEMO                   
         JE    ISRTG2              NO                                           
         CLI   0(R1),C'R'                                                       
         JE    ISRTGYES                                                         
         CLI   0(R1),C'E'                                                       
         JE    ISRTGYES                                                         
         J     ISRTGNO                                                          
*                                                                               
ISRTG2   CLI   2(R1),0             NON-TRAD DEMO CATEGORY?                      
         JE    ISRTG4              YES                                          
*                                                                               
         CLI   1(R1),C'R'          SEE IF NORMAL DEMO IS A RATING               
         JE    ISRTGYES                                                         
         CLI   1(R1),C'E'                                                       
         JE    ISRTGYES                                                         
         J     ISRTGNO                                                          
*                                                                               
ISRTG4   LLC   RF,1(R1)            YES, GET THE NON-TRAD INDEX                  
         BCTR  RF,0                                                             
         SLL   RF,3                8 BYTE/DEMO                                  
         A     RF,VNONTNMS                                                      
         CLI   0(RF),C'R'          SEE IF NON-TRAD CTGY IS RATING               
         JE    ISRTGYES              OR EXTENDED RATING                         
         CLI   0(RF),C'E'                                                       
         JNE   ISRTGNO                                                          
*                                                                               
ISRTGYES MVI   RATING,C'Y'                                                      
         J     ISRTGX                                                           
*                                                                               
ISRTGNO  MVI   RATING,C'N'                                                      
*                                                                               
ISRTGX   CLI   RATING,C'Y'                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
MYTSTDT  DC    C'990308'           TEST DATE FOR SPAUTH                         
REQSW    DC    X'00'                                                            
MONTHLY  DC    X'00'                                                            
RDESTSW  DS    CL1                                                              
SVQDATES DS    CL12                                                             
RPTSWS   DS    0CL7                REPORT SWITCHES                              
WSSW     DS    CL1                 WEEKLY SUMMARY SWITCHES                      
BDSW     DS    CL1                 BUY DETAIL SWITCH                            
         DS    CL5                                                              
RECSEQ   DC    H'0'                                                             
PRECIS   DC    F'0'                                                             
DPRECIS  DC    F'0'                                                             
RELO     DC    F'0'                                                             
VWRREC   DC    F'0'                                                             
VCLSE    DC    F'0'                                                             
VOPN     DC    F'0'                                                             
VGETIN   DC    F'0'                                                             
ELADDR   DC    F'0'                                                             
TOTLKD   DS    F                                                                
TOTLKSPT DS    F                                                                
TOTLK1   DS    F                                                                
ENDSW    DS    C                                                                
PREVBRND DS    C                                                                
PREVEST  DS    C                                                                
PREVDPT  DS    C                                                                
PREVSLN  DS    C                                                                
PREVSEC  DS    C                                                                
ADVAGYC  DS    CL1                                                              
WRCMND   DS    CL6                                                              
SVKMKT   DS    CL2                                                              
RATING   DS    C                                                                
TWODEC   DS    C                                                                
         DS    0F                                                               
ELEM     DS    CL100                                                            
         EJECT                                                                  
         DS    0F                                                               
         DS    CL2                                                              
MYBUFIO  DS    0CL62                                                            
BFMED    DS    CL1                                                              
BFCLT    DS    CL3                                                              
BFPRD    DS    CL1                                                              
BFEST    DS    CL1                                                              
BFMKT    DS    CL2                                                              
BFSTAT   DS    CL3                                                              
BFDPT    DS    CL1                                                              
BFSLN    DS    CL1                                                              
BFSEC    DS    CL1                                                              
BFSTART  DS    CL2                                                              
BFEND    DS    CL2                                                              
BFSPILL  DS    CL1                                                              
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
*                                                                               
PSLIST   DS    CL250                                                            
         EJECT                                                                  
*================================================================               
* PRINT GOAL DATA FOR WEEK BEING LOCKED                                         
*================================================================               
                                                                                
PRTGOAL  NTR1  BASE=*,LABEL=*                                                   
         LA    R8,KEY                                                           
         USING GOALREC,R8                                                       
*                                                                               
         CLI   USRSW1,C'Y'         TEST PRINTED GOAL KEY                        
         BE    PRTGL6                                                           
*                                                                               
         MVI   USRSW1,C'Y'                                                      
         MVC   P(7),=C'*GOALS*'                                                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,GKEYPRD                                                       
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   P+8(3),1(RF)                                                     
         MVC   PRTGLDEM,28(RF)     SAVE DEMO CODE                               
*                                                                               
         CLI   GKEYPRD2,0          TEST FOR PIGGYBACK                           
         BE    PRTGL4                                                           
         CLI   GKEYAGY,0           TEST IT IS REALLY A PRODUCT                  
         BNE   PRTGL4                                                           
*                                                                               
PRTGL2   L     RF,ADCLT                                                         
         AHI   RF,CLIST-CLTHDR                                                  
*                                                                               
PRTGL2A  CLC   GKEYPRD2,3(RF)                                                   
         BE    PRTGL2X                                                          
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   PRTGL2A                                                          
         LA    RF,=C'???'                                                       
*                                                                               
PRTGL2X  MVI   P+11,C'-'                                                        
         MVC   P+12(3),0(RF)                                                    
*                                                                               
PRTGL4   EDIT  GKEYEST,(3,P+29)                                                 
*                                                                               
         MVC   P+34(1),GKEYDPT                                                  
*                                                                               
         EDIT  GKEYSEC,(3,P+38)                                                 
*                                                                               
         EDIT  GKEYSLN,(3,P+42)                                                 
         DROP  R8                                                               
*                                                                               
         USING GLEMENT,R6                                                       
PRTGL6   GOTO1 DATCON,DMCB,(2,GLWEEK),(8,P+46)                                  
         L     R0,GLBUDGET                                                      
         EDIT  (R0),(12,P+55),2,COMMAS=YES                                      
*                                                                               
         L     R0,GLGRP                                                         
         N     R0,=X'3FFFFFFF'   DROP X'40'                                     
*                                                                               
         LA    R1,PRTGLDEM         POINT TO SAVED DEMO CODE                     
         BRAS  RE,ISRATING                                                      
         BNE   PRTGL10                                                          
* DEMO IS A RATING                                                              
         TM    RQOPTS,RQOPTS_2DEC   TEST 2-DEC RTGS                             
         BO    PRTGL12              YES                                         
         B     PRTGL14                                                          
* DEMO IS NOT A RATING                                                          
PRTGL10  TM    RQOPTS,RQOPTS_2DECIMP    TEST 2-DEC IMPS                         
         BZ    PRTGL14                                                          
*                                                                               
PRTGL12  TM    GLGRP,X'40'          TEST INPUT TO 2-DEC                         
         JO    *+8                                                              
         MHI   R0,10                                                            
         EDIT  (R0),(8,P+75),2                                                  
         B     PRTGL20                                                          
*                                                                               
PRTGL14  TM    GLGRP,X'40'         TEST INPUT TO 2-DEC                          
         JZ    PRTGL16             NO                                           
* ROUND TO 1 DEC                                                                
         LA    R1,2                                                             
         MR    R0,R0               DEMO X2 INTO R1                              
         D     R0,=F'10'                                                        
         LA    R0,1(R1)                                                         
         SRL   R0,1                                                             
PRTGL16  EDIT  (R0),(8,P+75),1                                                  
*                                                                               
PRTGL20  GOTO1 REPORT                                                           
         XIT1                                                                   
PRTGLDEM DS    XL3                                                              
         DS    XL1                 ALIGN                                        
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* CALLAUTH -- CALLS SPAUTH TO SET THE DATES                                     
*==================================================================             
                                                                                
CALLAUTH NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R3,ADEST            CHCK SDESK BYTE IN EST REC IS ON             
         USING ESTHDRD,R3                                                       
         TM    EFLAG1,EF1SDE       PIGGYBACK AUTHORIZATION OPEN?                
         BZ    CALLX               NO, GET OUT                                  
         DROP  R3                                                               
*                                                                               
         PUSH  USING                                                            
         USING SPAUTHD,WORK                                                     
         XC    SPAUTHD(SPAUTHLQ),SPAUTHD                                        
         MVC   SPACOM,ACOMFACS                                                  
         MVC   SPAKAM,BAGYMD                                                    
         MVC   SPAKCLT,BCLT                                                     
         MVC   SPAKPRD,BPRD                                                     
         MVC   SPAKEST,BEST                                                     
         MVC   SPAKMKT,BMKT                                                     
         MVC   SPAIO,=A(ELSAVE)    TEST                                         
         MVI   SPAUPDT,SPAUPDML    UPDATE  ML REPORT                            
         GOTO1 DATCON,DMCB,QSTART,(2,SPASDTE)                                   
         GOTO1 DATCON,DMCB,QEND,(2,SPAEDTE)                                     
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,0,X'D9000AB7'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            GET SPAUTH ADDRESS                           
         LA    R1,WORK                                                          
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   CALLX                                                            
*                                                                               
         BASR  RE,RF                                                            
         CLI   SPAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CALLX    XIT1                                                                   
         POP   USING                                                            
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* CONVERT SLN IN R0 TO A REPORTABLE SLN                                         
*====================================================================           
                                                                                
SETSLN   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R1,SVSLNADR         PICK UP PREVIOUS ENTRY ADDRESS               
         CLC   SVEQVAGY,AGY        AND IF AGENCY MATCHES                        
         BNE   *+14                                                             
         CLC   SVEQVMED,MED        AND MEDIA MATCHES                            
         BE    SETSLN18            IT'S THE RIGHT TABLE ENTRY                   
*                                                                               
         MVC   SVEQVMED,MED                                                     
         MVC   SVEQVAGY,AGY                                                     
*                                                                               
         MVI   BYTE,C'R'           FIND EQTAB                                   
         CLI   MED,C'R'                                                         
         BE    SETSLN10                                                         
         CLI   MED,C'X'                                                         
         BE    SETSLN10                                                         
         MVI   BYTE,C'T'                                                        
         CLI   MED,C'T'                                                         
         BE    SETSLN10                                                         
         CLI   MED,C'N'                                                         
         BE    SETSLN10                                                         
         CLI   MED,C'C'                                                         
         BE    SETSLN10                                                         
         DC    H'0'                                                             
*                                                                               
SETSLN10 L     R1,VSLNTAB                                                       
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            DSPL TO EOT                                  
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
SETSLN12 CLC   0(2,R1),=C'00'      TEST DEFAULT ENTRY                           
         BE    SETSLN14                                                         
         CLC   0(2,R1),AGY                                                      
         BNE   *+14                                                             
SETSLN14 CLC   2(1,R1),BYTE                                                     
         BE    SETSLN16                                                         
         BXLE  R1,RE,SETSLN12                                                   
         DC    H'0'                                                             
*                                                                               
SETSLN16 ST    R1,SVSLNADR         SAVE SLNTAB ENTRY ADDRESS                    
*                                                                               
SETSLN18 LR    RE,R0               GET INPUT SLN                                
         AR    RE,RE               X 2                                          
         LA    RE,4(R1,RE)         POINT TO ENTRY FOR THIS SLN                  
         IC    R0,1(RE)            REPORT EQUIVALENCING SLN                     
         XIT1  REGS=(R0)                                                        
*                                                                               
SVSLNADR DS    A                                                                
SVEQVAGY DS    CL2                                                              
SVEQVMED DS    CL1                                                              
         LTORG                                                                  
         EJECT                                                                  
         BUFF  LINES=200,ROWS=1,COLUMNS=11,FLAVOR=BINARY,KEYLIST=(19,A)         
         LTORG                                                                  
         DS    0D                                                               
ELSAVE   DS    4000C                                                            
         PRINT OFF                                                              
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
         PRINT ON                                                               
       ++INCLUDE SPGENGOAL                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPAUTHD                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028SPREPML02 11/21/19'                                      
         END                                                                    
