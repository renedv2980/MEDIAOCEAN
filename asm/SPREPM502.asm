*          DATA SET SPREPM502  AT LEVEL 095 AS OF 11/21/19                      
*PHASE SPM502T                                                                  
         TITLE 'SPREPM502 STANDARD INTERFACE TAPE'                              
         PRINT NOGEN                                                            
SPM502   CSECT                                                                  
         NMOD1 0,SPM502,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         ST    R5,RELO                                                          
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         STM   RA,RC,SPM5RA                                                     
         LA    RF,WRREC                                                         
         ST    RF,VWRREC                                                        
         LA    RF,CLSE                                                          
         ST    RF,VCLSE                                                         
         LA    RF,OPN                                                           
         ST    RF,VOPN                                                          
         LA    RF,GETIN                                                         
         ST    RF,VGETIN                                                        
         XC    RECCNT,RECCNT                                                    
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDLCHNK,=F'128'                                                 
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         XC    MEDNUMPE,MEDNUMPE                                                
         MVI   MEDEXTDM,4                                                       
         MVI   RQPRDEST,C'Y'                                                    
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         DROP  RE                                                               
         L     R2,=A(OUT)          RELOCATE ADDRESS CONSTANTS                   
         A     R2,RELO                                                          
         LA    RE,=A(OUT)                                                       
         ST    R2,0(RE)                                                         
         L     R2,=A(ITMED)                                                     
         A     R2,RELO                                                          
         LA    RE,=A(ITMED)                                                     
         ST    R2,0(RE)                                                         
         L     R2,=A(SAMED)                                                     
         A     R2,RELO                                                          
         LA    RE,=A(SAMED)                                                     
         ST    R2,0(RE)                                                         
         L     R2,=A(IN)                                                        
         A     R2,RELO                                                          
         LA    RE,=A(IN)                                                        
         ST    R2,0(RE)                                                         
         L     R2,=V(BUFFALOC)                                                  
         A     R2,RELO                                                          
         ST    R2,BUFFBUFF                                                      
         L     R2,=V(OLDNUMS)                                                   
         A     R2,RELO                                                          
         ST    R2,VOLDNUMS                                                      
         L     R2,=V(EQUIVOLD)                                                  
         A     R2,RELO                                                          
         ST    R2,VEQUOLD                                                       
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',(R2)                                        
         XC    RPTSWS,RPTSWS                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M21                                                              
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
         CLI   REQSW,0                                                          
         BNE   M2A                                                              
         L     R2,=A(OUT)                                                       
         CLI   PROGPROF+11,C'Y'                                                 
         BNE   *+8                                                              
         L     R2,=A(PCOUT)                                                     
         OPEN  ((R2),(OUTPUT))                                                  
         MVI   RQOPTS,RQOPTS_1DEC  FORCE 1-DECIMAL RATINGS                      
M2A      MVI   REQSW,1                                                          
         MVI   RDESTSW,1                                                        
         MVI   ITMED,C' '                                                       
         MVC   ITMED+1(79),ITMED                                                
         MVC   ITMED,QMED                                                       
         MVC   ITCLT,QCLT                                                       
         MVC   ITPRD,QPRD                                                       
         MVC   ITEST,QEST                                                       
         MVC   ITRCODE,=C'00'                                                   
         MVC   ITMKT,QMKT                                                       
         MVC   ITSTAT,QSTA                                                      
         MVC   ITSTART(19),QSTART                                               
         MVC   ITESTEND,QESTEND                                                 
         MVC   ITRNAME,QUESTOR                                                  
         MVI   ESTSW,C'N'                                                       
         CLI   PROGPROF,C'P'                                                    
         BE    M2APEPSI                                                         
         CLC   QPRD,=C'POL'        POL PRODUCT                                  
         BE    M2AEXIT                 YES - OK                                 
         CLI   PROGPROF,C'C'        NO - CHECK CPP EXTRACT                      
         BNE   M2AEXIT                  NO - OK                                 
         MVI   PROGPROF,C'Y'         YES - SET FOR WEEKLY GOALS                 
M2AEXIT  CLI   PROGPROF,C'C'                                                    
         BNE   EXIT                                                             
         MVI   FCRDGOAL,C'C'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
* EXTRACT PEPSI INFO ONLY                                                       
M2APEPSI MVI   FCRDGOAL,C'P'                                                    
         MVI   FCRDBUYS,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
*                                                                               
         CLI   RDESTSW,1           DONT PUT OUT IF SAME REQUEST                 
         BNE   M21SAMRQ                                                         
         MVC   PAGE,=H'1'                                                       
         MVC   ITSTART(19),QSTART                                               
         MVC   OUTCLT,QCLT                                                      
         CLC   PROGPROF+6(3),=C'***'    CHECK FOR CLIENT CODE OVERRIDE          
         BE    CNVSPACX                                                         
         OC    PROGPROF+6(3),PROGPROF+6                                         
         BZ    CNVSPACX                                                         
         MVC   OUTCLT,PROGPROF+6                                                
         LA    RE,3                                                             
         LA    RF,OUTCLT                                                        
CNVSPAC  CLI   0(RF),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)                                                         
         BCT   RE,CNVSPAC                                                       
CNVSPACX DS    0C                                                               
         MVC   ITCLT,OUTCLT                                                     
         BAS   R9,PUTOUT1                                                       
M21SAMRQ L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R2)                                      
*                                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         CLI   PROGPROF,C'C'       CPP EXTRACT                                  
         BNE   *+20                                                             
         XC    MEDNUMWK,MEDNUMWK   YES - SET FOR MONTHLIES                      
         MVC   MEDNUMMO,=F'13'                                                  
         MVI   FCRDGOAL,C'C'                                                    
         GOTO1 MEDDATE,DMCB,(RA)                                                
         L     RE,MEDBUFF                                                       
         L     RF,MEDAFRST                                                      
         MVC   BQSTARTP,0(RF)                                                   
         L     RF,MEDALAST                                                      
         MVC   BQENDP,2(RF)                                                     
         DROP  RE                                                               
         CLI   RDESTSW,1                                                        
         BNE   M21A                                                             
         GOTO1 =V(ESTREAD),DMCB,REQSW,RR=RELO                                   
M21A     MVI   RDESTSW,0                                                        
         LA    RE,PROGPROF                                                      
         LA    RF,DFLTPROF                                                      
         LA    R9,6                                                             
M21A1    CLI   0(RE),0                                                          
         BNE   *+10                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R9,M21A1                                                         
         LA    RE,PROGPROF                                                      
         LA    RF,QOPT1                                                         
         LA    R9,5                                                             
M21B     CLI   0(RF),C' '                                                       
         BE    *+10                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R9,M21B                                                          
         B     EXIT                                                             
         EJECT                                                                  
M3       CLI   MODE,PROCBUY                                                     
         BNE   M4                                                               
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
*                                                                               
M32      LA    R6,2                SET DEMO TYPE                                
         CLI   QRERATE,C' '                                                     
         BE    M322                                                             
         CLI   QRERATE,C'A'       ADJUST ONLY                                   
         BNE   M321                                                             
         LA    R6,5                                                             
         B     M322                                                             
M321     LA    R6,3                SET FOR PURCHASED RERATED                    
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R6,1(R6)            SET FOR ADJUSTMENT                           
         CLI   QRERATE,C'I'       RERATE BASED ON INVOICE                       
         BNE   *+8                                                              
         LA    R6,3(R6)                                                         
*                                                                               
M322     LA    R2,PSLIST                                                        
M323     CLC   0(2,R2),=X'FFFF'    END                                          
         BE    M34                                                              
         CLI   0(R2),0                                                          
         BNE   *+12                                                             
M323A    LA    R2,2(R2)                                                         
         B     M323                                                             
         L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         ST    R6,SVRRMODE                                                      
         GOTO1 MEDGETBY,DMCB,(RA),(R6)                                          
*                                                                               
* EXTRACT AND POST ROUTINES                                                     
*                                                                               
         L     R5,MEDAFRST                                                      
M324     L     R6,MEDALAST                                                      
         LA    RF,MEDMON01                                                      
         CR    R5,RF                                                            
         BNL   M323A                                                            
         CR    R5,R6                                                            
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
         MVC   BFMKT(5),BUYMSTA                                                 
         DROP  RE                                                               
         GOTO1 MSUNPK,DMCB,BFMKT,MKT,WORK                                       
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         MVC   BFDPT,BDDAYPT                                                    
         DROP  RE                                                               
         MVC   BFSLN,1(R2)                                                      
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
M334     DS    0H                                                               
         L     R7,BUFFBUFF                                                      
         CLI   PROGPROF+1,C'Y'                                                  
         BNE   M324A                                                            
         GOTO1 BUFFALO,DMCB,=C'PUT',(R7),MYBUFIO                                
         B     M324A                                                            
         SPACE 2                                                                
M34      CLI   PROGPROF+2,C'N'     BUY RECORD DETAILS WANTED                    
         BE    EXIT                 NO - EXIT                                   
         GOTO1 =V(BYDET),DMCB,REQSW,RR=RELO                                     
         B     EXIT                                                             
         EJECT                                                                  
M4       CLI   MODE,MKTLAST                                                     
         BNE   M5                                                               
         XC    MYBUFIO,MYBUFIO                                                  
         MVC   BFMED,QMED                                                       
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R7),MYBUFIO,1                             
         B     M42                                                              
M41      GOTO1 BUFFALO,DMCB,=C'SEQ',(R7),MYBUFIO,1                              
M42      TM    DMCB+8,X'80'                                                     
         BO    M44                                                              
         MVI   WSSW,1                                                           
         MVC   ITMED,BFMED                                                      
         MVC   ITCLT,BFCLT                                                      
         MVC   ITCLT,OUTCLT                                                     
         MVC   ITDPT,BFDPT                                                      
         SR    RE,RE                                                            
         IC    RE,BFSLN                                                         
         CVD   RE,DUB                                                           
         MVC   ITSLN,DUB+6                                                      
         SR    RE,RE                                                            
         IC    RE,BPRD                                                          
         CLI   BPRD,X'FF'                                                       
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RF,0(RE,RF)                                                      
         MVC   ITPRD,1(RF)                                                      
         SR    R8,R8                                                            
         IC    R8,BFEST                                                         
         CVD   R8,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ITEST,DUB+6(2)                                                   
         LA    RF,28(RF)                                                        
         LA    R9,ITDEM1CD                                                      
         LA    RE,4                                                             
M43ND    MVC   0(2,R9),1(RF)       SET UP FOR NEW DEMOS                         
         LA    R9,6(R9)                                                         
         LA    RF,3(RF)                                                         
         BCT   RE,M43ND                                                         
         SPACE 2                                                                
M43NDX   GOTO1 MSUNPK,DMCB,BFMKT,ITMKT,ITSTAT                                   
         MVC   ITRCODE,=C'20'                                                   
         CLI   BFSTAT,0            GOAL RECORD                                  
         BNE   M431                                                             
         MVC   ITSTAT,=C'     '                                                 
         MVC   ITRCODE,=C'10'                                                   
         MVC   ITDEM2CD,=C'  '                                                  
         MVC   ITDEM3CD,ITDEM2CD                                                
         MVC   ITDEM4CD,ITDEM2CD                                                
M431     GOTO1 DATCON,DMCB,(X'02',BFSTART),ITSTART                              
         GOTO1 DATCON,DMCB,(X'02',BFEND),ITEND                                  
         MVC   FULL,BFSPOTS                                                     
         L     RE,FULL                                                          
         CVD   RE,DUB                                                           
         MVC   ITSPOTS,DUB+5                                                    
         MVC   FULL,BFDOL                                                       
         L     RE,FULL                                                          
         CVD   RE,DUB                                                           
         MVC   ITDOL,DUB+3                                                      
         MVC   FULL,BFDEM1                                                      
         L     RE,FULL                                                          
         CVD   RE,DUB                                                           
         MVC   ITDEM1,DUB+4                                                     
         MVC   FULL,BFDEM2                                                      
         L     RE,FULL                                                          
         CVD   RE,DUB                                                           
         MVC   ITDEM2,DUB+4                                                     
         MVC   FULL,BFDEM3                                                      
         L     RE,FULL                                                          
         CVD   RE,DUB                                                           
         MVC   ITDEM3,DUB+4                                                     
         MVC   FULL,BFDEM4                                                      
         L     RE,FULL                                                          
         CVD   RE,DUB                                                           
         MVC   ITDEM4,DUB+4                                                     
         CLC   MGR1,SPACES                                                      
         BE    M432                                                             
         MVC   ITRNAME(5),MGR1                                                  
         CLC   MGR2,SPACES                                                      
         BE    M432                                                             
         MVC   ITRNAME(5),MGR2                                                  
         CLC   MGR3,SPACES                                                      
         BE    M432                                                             
         MVC   ITRNAME(5),MGR3                                                  
M432     BAS   R9,PUTOUT1                                                       
M433     B     M41                 DEACTIVATE CPP RECORDS                       
         CLI   ITRCODE+1,C'1'                                                   
         BE    M41                                                              
         MVI   ITRCODE+1,C'1'      CREATE CPP RECORDS                           
         L     RF,BFDEM1E                                                       
         BAS   R9,EQUIV                                                         
         ST    RF,BFDEM1                                                        
         L     RF,BFDEM2E                                                       
         BAS   R9,EQUIV                                                         
         ST    RF,BFDEM2                                                        
         L     RF,BFDEM3E                                                       
         BAS   R9,EQUIV                                                         
         ST    RF,BFDEM3                                                        
         L     RF,BFDEM4E                                                       
         BAS   R9,EQUIV                                                         
         ST    RF,BFDEM4                                                        
         B     M431                                                             
         B     M41                                                              
M44      GOTO1 BUFFALO,DMCB,=C'RESET',(R7)                                      
         B     EXIT                                                             
EQUIV    LTR   RF,RF                                                            
         BZR   R9                                                               
         OC    BFDOLEQ,BFDOLEQ                                                  
         BNZ   EQUIV1                                                           
         SR    RF,RF                                                            
         BR    R9                                                               
EQUIV1   SR    R6,R6                                                            
         ST    R7,FULL                                                          
         L     R7,BFDOL                                                         
         MH    R7,=H'20'                                                        
         DR    R6,RF                                                            
         A     R7,=F'1'                                                         
         SRA   R7,1                                                             
         LR    RF,R7                                                            
         L     R7,FULL                                                          
         BR    R9                                                               
         EJECT                                                                  
M5       CLI   MODE,PROCGOAL                                                    
         BNE   M6                                                               
         CLI   PROGPROF,C'P'                                                    
         BE    EXPEP                                                            
         CLI   PROGPROF,C'N'       WANT GOALS                                   
         BE    EXIT                 NO - EXIT                                   
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
         MVC   BFSLN,GKEYSLN                                                    
         MVC   BFSTART(4),0(R5)                                                 
         MVC   BFDOL,MEDGLD                                                     
         MVC   BFDEM1,MEDGL1                                                    
         MVC   BFDOLEQ,MEDGLDEQ                                                 
         MVC   BFDEM1E,MEDGL1EQ                                                 
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',(R7),MYBUFIO                                
         B     M522                                                             
         EJECT                                                                  
EXPEP    GOTO1 =V(EXPEPC),DMCB,REQSW,RR=RELO                                    
         BNE   EXIT                                                             
         BAS   R9,PUTOUT1                                                       
         B     EXIT                                                             
         EJECT                                                                  
M6       CLI   MODE,STAFRST                                                     
         BNE   M7                                                               
         XC    RECSEQ,RECSEQ                                                    
         CLI   PROGPROF+3,C'Y'     STATION ADDRESS WANTED                       
         BNE   EXIT                 NO - EXIT                                   
         MVI   BDSW,1               YES - READ STATION ADDRESS RECORD           
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'A'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),STA                                                     
         CLI   KEY+6,C' '                                                       
         BNE   *+10                                                             
         MVC   KEY+6(1),QMED                                                    
         MVC   KEY+7(2),QAGY                                                    
         MVC   AREC,ADBUY                                                       
         MVI   ITMED,C' '                                                       
         MVC   ITMED+1(79),ITMED                                                
         MVC   SAMED,QMED                                                       
         MVC   SACLT,OUTCLT                                                     
         MVC   SARCODE,=C'04'                                                   
         MVC   SAMKT,MKT                                                        
         MVC   SASTA,STA                                                        
         MVC   SADCODE,=C'01'                                                   
         L     R7,ADMARKET             GET MARKET NAME                          
         USING MKTREC,R7                                                        
         MVC   SAMKTNAM,MKTNAME                                                 
         L     R7,ADSTAT                                                        
         USING STAREC,R7                                                        
         MVC   SASSIZE,SSIZE                                                    
         MVC   SANETWRK,SNETWRK                                                 
         SPACE 2                                                                
         MVC   AREC,ADSTATAD       READ STATION ADDRESS                         
         GOTO1 READSTAD                                                         
         L     R7,ADSTATAD                                                      
         USING ADDRREC,R7                                                       
         MVC   SADATA(L'ANAME),ANAME                                            
         BAS   R9,PUTOUT1                                                       
         MVC   SADCODE,=C'02'                                                   
         MVC   SADATA(L'A1LINE),A1LINE                                          
         BAS   R9,PUTOUT1                                                       
         MVC   SADCODE,=C'03'                                                   
         MVC   SADATA(L'A2LINE),A2LINE                                          
         BAS   R9,PUTOUT1                                                       
         MVI   SADATA,C' '                                                      
         MVC   SADATA+1(23),SADATA                                              
         MVC   SADCODE,=C'04'                                                   
         MVC   SADATA(2),A3LINE                                                 
         MVC   SADATA+2(5),AZIP                                                 
         BAS   R9,PUTOUT1                                                       
         B     EXIT                                                             
         EJECT                                                                  
M7       CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         CLI   REQSW,1                                                          
         BNE   M7A                                                              
         GOTO1 VCLSE                                                            
         CLI   PROGPROF+11,C'Y'                                                 
         BE    EXIT                                                             
M7A      GOTO1 =V(DETPRNT),DMCB,REQSW,RR=RELO                                   
         EJECT                                                                  
*&&DO                                                                           
ENDJOB   CLOSER IN                                                              
*&&                                                                             
*&&OS                                                                           
ENDJOB   CLOSE (IN)                                                             
*&&                                                                             
         LA    R8,7                                                             
         LA    R9,RPTSWS                                                        
ENDJOB1  CLI   0(R9),0                                                          
         BNE   ENDJOB2                                                          
         LA    R9,1(R9)                                                         
         BCT   R8,ENDJOB1                                                       
         B     EXIT                                                             
ENDJOB2  MVI   0(R9),0                                                          
         OC    RPTSWS,RPTSWS                                                    
         BZ    *+8                                                              
         B     M7A                                                              
         MVC   P(12),=C'RECORD COUNT'                                           
         EDIT  RECCNT,(7,P+13)                                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
PUTOUT1  GOTO1 VOLDNUMS,DMCB,ITMED                                              
         ST    R2,FULL                                                          
         CLI   PROGPROF+11,C'Y'                                                 
         BE    PUTOUTPC                                                         
         L     R2,=A(OUT)                                                       
         PUT   (R2),ITMED                                                       
PCRET    L     RE,RECCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,RECCNT                                                        
         BR    R9                                                               
PUTOUTPC MVI   COMMASW,C'N'                                                     
         CLC   ITRCODE,=C'06'                                                   
         BE    PUTOPC2                                                          
         L     RE,=A(PCRECO)                                                    
         MVI   0(RE),C' '                                                       
         MVC   1(255,RE),0(RE)                                                  
         ST    RE,OUTAREA                                                       
PUTOPC2  GOTO1 =V(PC),DMCB,(RA),ITMED                                           
         B     PCRET                                                            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
         DS    0D                                                               
         USING *,RF                                                             
WRREC    NTR1  BASE=SPM5RB                                                      
         DROP  RF                                                               
         LM    RA,RC,SPM5RA                                                     
         MVC   ITCLT,OUTCLT                                                     
         BAS   R9,PUTOUT1                                                       
         XIT1                                                                   
         SPACE 2                                                                
         DS    0D                                                               
         USING *,RF                                                             
CLSE     NTR1  BASE=SPM5RB                                                      
         DROP  RF                                                               
         LM    RA,RC,SPM5RA                                                     
         CLI   PROGPROF+11,C'Y'                                                 
         BNE   CLSE2                                                            
         CLOSE (PCOUT)                                                          
         B     CLSEX                                                            
CLSE2    CLOSE (OUT)                                                            
CLSEX    XIT1                                                                   
         SPACE 2                                                                
         DS    0D                                                               
         USING *,RF                                                             
OPN      NTR1  BASE=SPM5RB                                                      
         DROP  RF                                                               
         LM    RA,RC,SPM5RA                                                     
*&&DO                                                                           
         OPENR IN                                                               
*&&                                                                             
*&&OS                                                                           
         OPEN  (IN,(INPUT))                                                     
*&&                                                                             
         XIT1                                                                   
         SPACE 2                                                                
         DS    0D                                                               
         USING *,RF                                                             
GETIN    NTR1  BASE=SPM5RB                                                      
         DROP  RF                                                               
         LM    RA,RC,SPM5RA                                                     
         L     R2,=A(IN)                                                        
         GET   (R2),ITMED                                                       
         XIT1                                                                   
         SPACE 2                                                                
EOTAPE   XMOD1 2                                                                
SPM5RA   DC    F'0'                                                             
SPM5RB   DC    F'0'                                                             
SPM5RC   DC    F'0'                                                             
RELO     DS    F                                                                
         LTORG                                                                  
         EJECT                                                                  
REQSW    DC    X'00'                                                            
PRVSTG   DS    C                                                                
RECDEL   DS    C                                                                
RDESTSW  DS    CL1                                                              
RPTSWS   DS    0CL7                REPORT SWITCHES                              
WSSW     DS    CL1                 WEEKLY SUMMARY SWITCHES                      
BDSW     DS    CL1                 BUY DETAIL SWITCH                            
         DS    CL5                                                              
OUTCLT   DS    CL3                                                              
RECSEQ   DC    H'0'                                                             
DFLTPROF DC    C'YYNNNN'                                                        
VWRREC   DC    F'0'                                                             
VCLSE    DC    F'0'                                                             
VOPN     DC    F'0'                                                             
VGETIN   DC    F'0'                                                             
DELADDR  DC    F'0'                                                             
RECCNT   DC    F'0'                                                             
PROGRAM  DS    CL18                                                             
PROGRAMD DS    CL16                                                             
         EJECT                                                                  
ITMED    DS    CL1      A               MEDIA                                   
ITCLT    DS    CL3      AAA             CLIENT                                  
ITPRD    DS    CL3      AAA             PRODUCT                                 
ITEST    DS    CL3      NNN             ESTIMATE NUMBER                         
ITRCODE  DS    CL2      NN              RECORD CODE = 20                        
ITMKT    DS    CL4      NNNN            MARKET NUMBER                           
ITSTAT   DS    CL5      AAAAA           STATION CALL LETTERS                    
ITDPT    DS    CL1      A               DAYPART CODE                            
ITSLN    DS    CL2      NNNS            SPOT LENGTH (SEC)                       
ITSTART  DS    CL6      YYMMDD          START DATE                              
ITEND    DS    CL6      YYMMDD          END DATE                                
ITSPOTS  DS    CL3      NNNNNS          TOTAL SPOTS                             
*                                                                               
         ORG   ITSPOTS                                                          
ITESTEND DS    CL3                 END ESTIMATE FOR 00 RECORDS                  
ITDOL    DS    CL5      NNNNNNN.NNS     TOTAL DOLLARS                           
ITDEM1   DS    CL4      NNNNNN.NS       TOTAL IMPS OR POINTS                    
ITDEM1CD DS    CL2           NNNS       DEMO CODE 1                             
ITDEM2   DS    CL4      NNNNNN.NS       TOTAL IMPS OR POINTS                    
ITDEM2CD DS    CL2           NNNS       DEMO CODE 2                             
ITDEM3   DS    CL4      NNNNNN.NS       TOTAL IMPS OR POINTS                    
ITDEM3CD DS    CL2           NNNS       DEMO CODE 3                             
ITDEM4   DS    CL4      NNNNNN.NS       TOTAL IMPS OR POINTS                    
ITDEM4CD DS    CL2           NNNS       DEMO CODE 4                             
ITRNAME  DS    CL12                     SPARE                                   
         SPACE 2                                                                
         ORG   ITDPT                                                            
ITPROG   DS    CL18                PROGRAM NAME                                 
ITFILM1  DS    CL8                 BRAND 1 FILM                                 
ITFILM2  DS    CL8                 BRAND 2 FILM                                 
ITPROGD  DS    CL16                DEMO FILE PROGRAM NAME                       
         SPACE 2                                                                
         ORG   ITEND                                                            
BTSDOW   DS    CL1            N              START DAY OF WEEK                  
BTADAYS  DS    CL7            NNNNNNN        ROTATION DAYS                      
BTSTIM   DS    CL5            NNNNA          START TIME                         
BTETIM   DS    CL5            NNNNA          END TIME                           
BTSPOTS  DS    CL3            NNNNNS         NUMBER OF SPOTS                    
BTCOST   DS    CL5            NNNNNNN.NNS    COST PER SPOT                      
BTPRD2   DS    CL3                 AAA       PRODUCT 2                          
BTSLN2   DS    CL2                 NNNS      SPOT LENGTH 2                      
BTADAY   DS    CL1                 A         AFFID DAY                          
BTATIM   DS    CL5                 NNNNA     AFFID TIME                         
BTPADJ   DS    CL2                 AA        ADJACENCY/PROGRAM TYPE             
BTPURP   DS    CL1                 A         PURPOSE                            
BTDCD1   DS    CL2                 NNNS          DEMO CODE 1                    
BTDCD2   DS    CL2                 NNNS          DEMO CODE 12                   
BTDCD3   DS    CL2                 NNNS          DEMO CODE 13                   
BTDCD4   DS    CL2                 NNNS          DEMO CODE 14                   
BTSEQ    DS    CL2                 BINARY SEQUENCE NUMBER                       
         SPACE 2                                                                
* BUY DETAIL RECORD 7 LAYOUT                                                    
         ORG   BTDCD1                                                           
BT7SP    DS    CL1                 A             SPECIAL PROGRAM IND.           
BT7DEMN  DS    CL2                 NNNS          DEMO CODE                      
BT7OVR   DS    CL1                 A             OVERRIDE INDICATOR             
BT7DEMV  DS    CL3                 NNNN.NS       DEMO VALUE                     
BT7SPAR  DS    CL1                                                              
         SPACE 2                                                                
         ORG   ITMED                                                            
SAMED    DS    CL1                           MEDIA                              
SACLT    DS    CL3                           CLIENT                             
         DS    CL6                                                              
SARCODE  DS    CL2                           RECORD CODE                        
SAMKT    DS    CL4                           MARKET NUMBER                      
SASTA    DS    CL5                           STATION                            
SADCODE  DS    CL2                           RECORD CODE                        
SADATA   DS    CL24                          DATA                               
SAEXTRA  DS    CL33                          SPARE                              
         ORG   SADATA                                                           
SAMKTNAM DS    CL24                MARKET NAME                                  
SASSIZE  DS    C                   STATION SIZE                                 
SANETWRK DS    CL3                 NETWORK AFFILIATION                          
         ORG   ITMKT                                                            
ERSTART  DS    CL6                                                              
EREND    DS    CL6                                                              
ERDESC   DS    CL20                                                             
ERFILTER DS    CL3                                                              
         DS    CL33                                                             
DEMREC   DS    0CL80                                                            
DMRMED   DS    CL1                                                              
DMRCLT   DS    CL3                                                              
DMRPRD   DS    CL3                                                              
DMREST   DS    CL3                                                              
DMRCODE  DS    CL2                                                              
DMRMNO   DS    CL4                                                              
DMRSTAT  DS    CL5                                                              
DMRSP    DS    CL1                                                              
DMRD1N   DS    CL2                                                              
DMRD1O   DS    CL1                                                              
DMRD1V   DS    CL3                                                              
DMRD2N   DS    CL2                                                              
DMRD2O   DS    CL1                                                              
DMRD2V   DS    CL3                                                              
DMRD3N   DS    CL2                                                              
DMRD3O   DS    CL1                                                              
DMRD3V   DS    CL3                                                              
DMRD4N   DS    CL2                                                              
DMRD4O   DS    CL1                                                              
DMRD4V   DS    CL3                                                              
         DS    0F                                                               
         DS    CL3                                                              
MYBUFIO  DS    0CL61                                                            
BFMED    DS    CL1                                                              
BFCLT    DS    CL3                                                              
BFPRD    DS    CL1                                                              
BFEST    DS    CL1                                                              
BFMKT    DS    CL2                                                              
BFSTAT   DS    CL3                                                              
BFDPT    DS    CL1                                                              
BFSLN    DS    CL1                                                              
BFSTART  DS    CL2                                                              
BFEND    DS    CL2                                                              
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
VOLDNUMS DC    F'0'                                                             
VEQUOLD  DC    F'0'                                                             
COMMASW  DC    C'N'                                                             
OUTAREA  DS    F'0'                                                             
SVRRMODE DS    F                                                                
THISELEM DS    F                                                                
PSLIST   DS    CL250                                                            
PCRECO   DS    CL256                                                            
         DROP  R3                                                               
         EJECT                                                                  
PC       CSECT                                                                  
         NMOD1 0,PCOUT                                                          
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R6,4(R1)                                                         
         USING ITMED,R6                                                         
         CLC   ITRCODE,=C'06'                                                   
         BNE   *+8                                                              
         MVI   COMMASW,C'Y'                                                     
         L     R5,OUTAREA                                                       
         LA    R4,R00ST                                                         
PC1      ST    R4,SAVEIT                                                        
         LA    R4,2(R4)                                                         
PCLOOP   CLI   0(R4),0                                                          
         BE    PCX                                                              
PCSEL    CLI   0(R4),SEL                                                        
         BNE   PCSEL2                                                           
         CLC   ITRCODE(2),3(R4)                                                 
         BE    PCSELN                                                           
PCNEXT   L     R4,SAVEIT                                                        
         SR    RE,RE                                                            
         ICM   RE,3,0(R4)                                                       
         AR    R4,RE                                                            
         B     PC1                                                              
PCSELN   LA    R4,5(R4)                                                         
PCSEL2   CLI   0(R4),0                                                          
         BE    PCNEXT                                                           
         CLI   0(R4),SEL2                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
PCCNVAA  MVC   WORK,SPACES                                                      
         CLI   0(R4),CNVAA                                                      
         BNE   *+8                                                              
         BAS   R9,AACNV                                                         
         CLI   0(R4),CNVAN                                                      
         BNE   *+8                                                              
         BAS   R9,ANCNV                                                         
         CLI   0(R4),CNVP0                                                      
         BNE   *+8                                                              
         BAS   R9,P0CNV                                                         
         CLI   0(R4),CNVP1                                                      
         BNE   *+8                                                              
         BAS   R9,P1CNV                                                         
         CLI   0(R4),CNVP2                                                      
         BNE   *+8                                                              
         BAS   R9,P2CNV                                                         
         CLI   0(R4),CNVP3                                                      
         BNE   *+8                                                              
         BAS   R9,P3CNV                                                         
         CLI   0(R4),CNVP4                                                      
         BNE   *+8                                                              
         BAS   R9,P4CNV                                                         
         CLI   0(R4),CNVP5                                                      
         BNE   *+8                                                              
         BAS   R9,P5CNV                                                         
         CLI   0(R4),CNVDEM                                                     
         BNE   *+8                                                              
         BAS   R9,DEMCNV                                                        
         CLI   0(R4),CNVYMD                                                     
         BNE   *+8                                                              
         BAS   R9,YMDCNV                                                        
         CLI   COMMASW,C'Y'                                                     
         BNE   *+12                                                             
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         MVI   COMMASW,C'Y'                                                     
         BAS   R9,STRTAIL          STRIP TRIALING BLANKS                        
         LTR   RE,RE               BYPASS IF NO DATA                            
         BZ    PCCNVAA1                                                         
         CLI   QUOTESW,C'Y'                                                     
         BNE   *+12                                                             
         MVI   0(R5),C''''                                                      
         LA    R5,1(R5)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK                                                     
         LA    R5,1(RE,R5)                                                      
         CLI   QUOTESW,C'Y'                                                     
         BNE   *+12                                                             
         MVI   0(R5),C''''                                                      
         LA    R5,1(R5)                                                         
PCCNVAA1 LA    R4,3(R4)                                                         
         B     PCLOOP                                                           
PCX      L     R2,=A(PCOUT)                                                     
         LA    R4,PCRECO                                                        
         CLC   ITRCODE,=C'05'                                                   
         BE    PCXIT                                                            
         PUT   (R2),(R4)                                                        
         MVC   P1(256),PCRECO                                                   
         GOTO1 REPORT                                                           
PCXIT    ST    R5,OUTAREA                                                       
         XMOD1 1                                                                
         EJECT                                                                  
STRTAIL  LA    RF,WORK-1(RE)                                                    
         CLI   0(RF),0                                                          
         BE    *+10                                                             
         CLI   0(RF),C' '                                                       
         BNER  R9                                                               
         BCT   RE,STRTAIL                                                       
         BR    R9                                                               
AACNV    DS    0H                                                               
         MVI   QUOTESW,C'Y'                                                     
AACNV2   MVC   WORK,SPACES                                                      
         ZIC   RF,1(R4)                                                         
         ZIC   RE,2(R4)                                                         
         LA    RF,ITMED(RF)                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
         LA    RE,1(RE)                                                         
         BR    R9                                                               
P0CNV    DS    0H                                                               
         MVI   QUOTESW,C'N'                                                     
         MVC   EDOUT,SPACES                                                     
         ZIC   RF,1(R4)                                                         
         ZIC   RE,2(R4)                                                         
         LA    RF,ITMED(RF)                                                     
         TM    1(RF),X'0F'                                                      
         BNZ   *+10                                                             
         MVC   0(2,RF),=PL2'0'                                                  
         ZAP   DUB,0(2,RF)                                                      
         EDIT  (P8,DUB),(3,EDOUT),,ALIGN=LEFT                                   
         MVC   WORK(3),EDOUT                                                    
         LA    RE,2                                                             
         BR    R9                                                               
P1CNV    DS    0H                                                               
         MVI   QUOTESW,C'N'                                                     
         MVC   EDOUT,SPACES                                                     
         ZIC   RF,1(R4)                                                         
         ZIC   RE,2(R4)                                                         
         LA    RF,ITMED(RF)                                                     
         ZAP   DUB,0(3,RF)                                                      
         EDIT  (P8,DUB),(17,EDOUT),1,ALIGN=LEFT                                 
         MVC   WORK(17),EDOUT                                                   
         LA    RE,17                                                            
         BR    R9                                                               
P2CNV    DS    0H                                                               
         MVI   QUOTESW,C'N'                                                     
         MVC   EDOUT,SPACES                                                     
         ZIC   RF,1(R4)                                                         
         ZIC   RE,2(R4)                                                         
         LA    RF,ITMED(RF)                                                     
         ZAP   DUB,0(4,RF)                                                      
         EDIT  (P8,DUB),(17,EDOUT),2,ALIGN=LEFT,MINUS=YES                       
         MVC   WORK(17),EDOUT                                                   
         LA    RE,17                                                            
         BR    R9                                                               
P3CNV    DS    0H                                                               
         MVI   QUOTESW,C'N'                                                     
         MVC   EDOUT,SPACES                                                     
         ZIC   RF,1(R4)                                                         
         ZIC   RE,2(R4)                                                         
         LA    RF,ITMED(RF)                                                     
         ZAP   DUB,0(4,RF)                                                      
         EDIT  (P8,DUB),(17,EDOUT),1,ALIGN=LEFT                                 
         MVC   WORK(17),EDOUT                                                   
         LA    RE,17                                                            
         BR    R9                                                               
P4CNV    DS    0H                                                               
         MVI   QUOTESW,C'N'                                                     
         MVC   EDOUT,SPACES                                                     
         ZIC   RF,1(R4)                                                         
         ZIC   RE,2(R4)                                                         
         LA    RF,ITMED(RF)                                                     
         ZAP   DUB,0(3,RF)                                                      
         EDIT  (P8,DUB),(17,EDOUT),,ALIGN=LEFT                                  
         MVC   WORK(17),EDOUT                                                   
         LA    RE,17                                                            
         BR    R9                                                               
P5CNV    DS    0H                                                               
         MVI   QUOTESW,C'N'                                                     
         MVC   EDOUT,SPACES                                                     
         ZIC   RF,1(R4)                                                         
         ZIC   RE,2(R4)                                                         
         LA    RF,ITMED(RF)                                                     
         ZAP   DUB,0(5,RF)                                                      
         EDIT  (P8,DUB),(17,EDOUT),2,ALIGN=LEFT,MINUS=YES                       
         MVC   WORK(17),EDOUT                                                   
         LA    RE,17                                                            
         BR    R9                                                               
DEMCNV   DS    0H                                                               
         MVI   QUOTESW,C'Y'                                                     
         MVC   EDOUT,SPACES                                                     
         ZIC   RF,1(R4)                                                         
         ZIC   RE,2(R4)                                                         
         LA    RF,ITMED(RF)                                                     
         ZIC   RE,1(RF)                                                         
         EDIT  (RE),(3,EDOUT+1),,ALIGN=LEFT                                     
         MVC   EDOUT(1),0(RF)                                                   
         MVC   WORK(17),EDOUT                                                   
         LA    RE,4                                                             
         BR    R9                                                               
ANCNV    MVI   QUOTESW,C'N'                                                     
         B     AACNV2                                                           
YMDCNV   DS    0H                                                               
         MVI   QUOTESW,C'Y'                                                     
         MVC   EDOUT,SPACES                                                     
         ZIC   RF,1(R4)                                                         
         LA    RF,ITMED(RF)                                                     
         MVC   EDOUT(2),0(RF)                                                   
         MVI   EDOUT+2,C'/'                                                     
         MVC   EDOUT+3(2),2(RF)                                                 
         MVI   EDOUT+5,C'/'                                                     
         MVC   EDOUT+6(2),4(RF)                                                 
         MVC   WORK(12),EDOUT                                                   
         LA    RE,9                                                             
         BR    R9                                                               
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
SAVEIT   DS    F                                                                
QUOTESW  DS    C                                                                
EDOUT    DS    CL17                                                             
PCCNVT   DS    0C                                                               
SEL      EQU   1                   SELECTION ROUTINE                            
SEL2     EQU   2                   SUB SELECTION ROUTINE                        
CNVAA    EQU   3                   ALPHA CONVERSION                             
CNVP0    EQU   4                   2 - 0DEC                                     
CNVP1    EQU   5                   3 - 1DEC                                     
CNVP2    EQU   6                   4 - 2DEC                                     
CNVDEM   EQU   7                   DEMO MODIFIER/CODE                           
CNVYMD   EQU   8                   YY/MM/DD                                     
CNVAN    EQU   9                   UNQUOTED ALPHA                               
CNVP3    EQU   10                  4 - 1DEC                                     
CNVP4    EQU   11                  3 - 0DEC                                     
CNVP5    EQU   12                  5 - 2DEC                                     
ENDTAB   EQU   0                   END OF TABLE                                 
R00ST    DC    AL2(R00EN-R00ST+1)                                               
         DC    AL1(SEL),AL1(ITRCODE-ITMED),AL1(02),C'00'                        
         DC    AL1(CNVAA),AL1(ITMED-ITMED),AL1(L'ITMED)                         
         DC    AL1(CNVAA),AL1(ITCLT-ITMED),AL1(L'ITCLT)                         
         DC    AL1(CNVAA),AL1(ITPRD-ITMED),AL1(L'ITPRD)                         
         DC    AL1(CNVAA),AL1(ITEST-ITMED),AL1(L'ITEST)                         
         DC    AL1(CNVAN),AL1(ITRCODE-ITMED),AL1(L'ITRCODE)                     
         DC    AL1(CNVAA),AL1(ITMKT-ITMED),AL1(L'ITMKT)                         
         DC    AL1(CNVAA),AL1(ITSTAT-ITMED),AL1(L'ITSTAT)                       
         DC    AL1(CNVAA),AL1(ITSTART-ITMED),AL1(L'ITSTART)                     
         DC    AL1(CNVAA),AL1(ITEND-ITMED),AL1(L'ITEND)                         
         DC    AL1(CNVAA),AL1(ITESTEND-ITMED),AL1(L'ITESTEND)                   
         DC    AL1(CNVAA),AL1(ITRNAME-ITMED),AL1(L'ITRNAME)                     
R00EN    DC    AL1(ENDTAB)                                                      
*                                                                               
R03ST    DC    AL2(R03EN-R03ST+1)                                               
         DC    AL1(SEL),AL1(ITRCODE-ITMED),AL1(02),C'03'                        
         DC    AL1(CNVAA),AL1(ITMED-ITMED),AL1(L'ITMED)                         
         DC    AL1(CNVAA),AL1(ITCLT-ITMED),AL1(L'ITCLT)                         
         DC    AL1(CNVAA),AL1(ITPRD-ITMED),AL1(L'ITPRD)                         
         DC    AL1(CNVAN),AL1(ITEST-ITMED),AL1(L'ITEST)                         
         DC    AL1(CNVAN),AL1(ITRCODE-ITMED),AL1(L'ITRCODE)                     
         DC    AL1(CNVYMD),AL1(ERSTART-ITMED),AL1(L'ERSTART)                    
         DC    AL1(CNVYMD),AL1(EREND-ITMED),AL1(L'EREND)                        
         DC    AL1(CNVAA),AL1(ERDESC-ITMED),AL1(L'ERDESC)                       
         DC    AL1(CNVAA),AL1(ERFILTER-ITMED),AL1(L'ERFILTER)                   
R03EN    DC    AL1(ENDTAB)                                                      
*                                                                               
R04ST    DC    AL2(R04EN-R04ST+1)                                               
         DC    AL1(SEL),AL1(ITRCODE-ITMED),AL1(02),C'04'                        
         DC    AL1(CNVAA),AL1(ITMED-ITMED),AL1(L'ITMED)                         
         DC    AL1(CNVAA),AL1(ITCLT-ITMED),AL1(L'ITCLT)                         
         DC    AL1(CNVAA),AL1(ITRCODE-ITMED),AL1(L'ITRCODE)                     
         DC    AL1(CNVAA),AL1(ITMKT-ITMED),AL1(L'ITMKT)                         
         DC    AL1(CNVAA),AL1(ITSTAT-ITMED),AL1(L'ITSTAT)                       
         DC    AL1(SEL2),AL1(SADCODE-ITMED),AL1(02),C'01'                       
         DC    AL1(CNVAA),AL1(SADATA-ITMED),AL1(L'SADATA)                       
         DC    AL1(SEL2),AL1(SADCODE-ITMED),AL1(02),C'02'                       
         DC    AL1(CNVAA),AL1(SADATA-ITMED),AL1(L'SADATA)                       
         DC    AL1(SEL2),AL1(SADCODE-ITMED),AL1(02),C'03'                       
         DC    AL1(CNVAA),AL1(SADATA-ITMED),AL1(L'SADATA)                       
         DC    AL1(SEL2),AL1(SADCODE-ITMED),AL1(02),C'04'                       
         DC    AL1(CNVAA),AL1(SADATA-ITMED),AL1(02) STATE                       
         DC    AL1(CNVAA),AL1(SADATA-ITMED+2),AL1(05) ZIP                       
R04EN    DC    AL1(ENDTAB)                                                      
*                                                                               
R05ST    DC    AL2(R05EN-R05ST+1)                                               
         DC    AL1(SEL),AL1(ITRCODE-ITMED),AL1(02),C'05'                        
         DC    AL1(CNVAA),AL1(ITMED-ITMED),AL1(L'ITMED)                         
         DC    AL1(CNVAA),AL1(ITCLT-ITMED),AL1(L'ITCLT)                         
         DC    AL1(CNVAA),AL1(ITPRD-ITMED),AL1(L'ITPRD)                         
         DC    AL1(CNVAN),AL1(ITEST-ITMED),AL1(L'ITEST)                         
         DC    AL1(CNVAN),AL1(ITRCODE-ITMED),AL1(L'ITRCODE)                     
         DC    AL1(CNVAN),AL1(ITMKT-ITMED),AL1(L'ITMKT)                         
         DC    AL1(CNVAA),AL1(ITSTAT-ITMED),AL1(L'ITSTAT)                       
         DC    AL1(CNVAA),AL1(ITDPT-ITMED),AL1(L'ITDPT)                         
         DC    AL1(CNVP0),AL1(ITSLN-ITMED),AL1(L'ITSLN)                         
         DC    AL1(CNVYMD),AL1(ITSTART-ITMED),AL1(L'ITSTART)                    
         DC    AL1(CNVAN),AL1(BTSDOW-ITMED),AL1(L'BTSDOW)                       
         DC    AL1(CNVAN),AL1(BTADAYS-ITMED),AL1(L'BTADAYS)                     
         DC    AL1(CNVAA),AL1(BTSTIM-ITMED),AL1(L'BTSTIM)                       
         DC    AL1(CNVAA),AL1(BTETIM-ITMED),AL1(L'BTETIM)                       
         DC    AL1(CNVP4),AL1(BTSPOTS-ITMED),AL1(L'BTSPOTS)                     
         DC    AL1(CNVP5),AL1(BTCOST-ITMED),AL1(L'BTCOST)                       
         DC    AL1(CNVAA),AL1(BTPRD2-ITMED),AL1(L'BTPRD2)                       
         DC    AL1(CNVP0),AL1(BTSLN2-ITMED),AL1(L'BTSLN2)                       
         DC    AL1(CNVAN),AL1(BTADAY-ITMED),AL1(L'BTADAY)                       
         DC    AL1(CNVAA),AL1(BTATIM-ITMED),AL1(L'BTATIM)                       
         DC    AL1(CNVAA),AL1(BTPADJ-ITMED),AL1(L'BTPADJ)                       
         DC    AL1(CNVAA),AL1(BTPURP-ITMED),AL1(L'BTPURP)                       
R05EN    DC    AL1(ENDTAB)                                                      
*                                                                               
R06ST    DC    AL2(R06EN-R06ST+1)                                               
         DC    AL1(SEL),AL1(ITRCODE-ITMED),AL1(02),C'06'                        
         DC    AL1(CNVAA),AL1(DMRSP-DEMREC),AL1(L'DMRSP)                        
         DC    AL1(CNVDEM),AL1(DMRD1N-DEMREC),AL1(L'DMRD1N)                     
         DC    AL1(CNVAA),AL1(DMRD1O-DEMREC),AL1(L'DMRD1O)                      
         DC    AL1(CNVP1),AL1(DMRD1V-DEMREC),AL1(L'DMRD1V)                      
         DC    AL1(CNVDEM),AL1(DMRD2N-DEMREC),AL1(L'DMRD2N)                     
         DC    AL1(CNVAA),AL1(DMRD2O-DEMREC),AL1(L'DMRD2O)                      
         DC    AL1(CNVP1),AL1(DMRD2V-DEMREC),AL1(L'DMRD2V)                      
         DC    AL1(CNVDEM),AL1(DMRD3N-DEMREC),AL1(L'DMRD3N)                     
         DC    AL1(CNVAA),AL1(DMRD3O-DEMREC),AL1(L'DMRD3O)                      
         DC    AL1(CNVP1),AL1(DMRD3V-DEMREC),AL1(L'DMRD3V)                      
         DC    AL1(CNVDEM),AL1(DMRD4N-DEMREC),AL1(L'DMRD4N)                     
         DC    AL1(CNVAA),AL1(DMRD4O-DEMREC),AL1(L'DMRD4O)                      
         DC    AL1(CNVP1),AL1(DMRD4V-DEMREC),AL1(L'DMRD4V)                      
R06EN    DC    AL1(ENDTAB)                                                      
*                                                                               
R07ST    DC    AL2(R07EN-R07ST+1)                                               
         DC    AL1(SEL),AL1(ITRCODE-ITMED),AL1(02),C'07'                        
         DC    AL1(CNVAA),AL1(ITMED-ITMED),AL1(L'ITMED)                         
         DC    AL1(CNVAA),AL1(ITCLT-ITMED),AL1(L'ITCLT)                         
         DC    AL1(CNVAA),AL1(ITPRD-ITMED),AL1(L'ITPRD)                         
         DC    AL1(CNVAN),AL1(ITEST-ITMED),AL1(L'ITEST)                         
         DC    AL1(CNVAN),AL1(ITRCODE-ITMED),AL1(L'ITRCODE)                     
         DC    AL1(CNVAN),AL1(ITMKT-ITMED),AL1(L'ITMKT)                         
         DC    AL1(CNVAA),AL1(ITSTAT-ITMED),AL1(L'ITSTAT)                       
         DC    AL1(CNVAA),AL1(ITDPT-ITMED),AL1(L'ITDPT)                         
         DC    AL1(CNVP0),AL1(ITSLN-ITMED),AL1(L'ITSLN)                         
         DC    AL1(CNVYMD),AL1(ITSTART-ITMED),AL1(L'ITSTART)                    
         DC    AL1(CNVAN),AL1(BTSDOW-ITMED),AL1(L'BTSDOW)                       
         DC    AL1(CNVAN),AL1(BTADAYS-ITMED),AL1(L'BTADAYS)                     
         DC    AL1(CNVAA),AL1(BTSTIM-ITMED),AL1(L'BTSTIM)                       
         DC    AL1(CNVAA),AL1(BTETIM-ITMED),AL1(L'BTETIM)                       
         DC    AL1(CNVP4),AL1(BTSPOTS-ITMED),AL1(L'BTSPOTS)                     
         DC    AL1(CNVP5),AL1(BTCOST-ITMED),AL1(L'BTCOST)                       
         DC    AL1(CNVAA),AL1(BTPRD2-ITMED),AL1(L'BTPRD2)                       
         DC    AL1(CNVP0),AL1(BTSLN2-ITMED),AL1(L'BTSLN2)                       
         DC    AL1(CNVAN),AL1(BTADAY-ITMED),AL1(L'BTADAY)                       
         DC    AL1(CNVAA),AL1(BTATIM-ITMED),AL1(L'BTATIM)                       
         DC    AL1(CNVAA),AL1(BTPADJ-ITMED),AL1(L'BTPADJ)                       
         DC    AL1(CNVAA),AL1(BTPURP-ITMED),AL1(L'BTPURP)                       
         DC    AL1(CNVAA),AL1(BT7SP-ITMED),AL1(L'BT7SP)                         
         DC    AL1(CNVYMD),AL1(BT7DEMN-ITMED),AL1(L'BT7DEMN)                    
         DC    AL1(CNVAA),AL1(BT7OVR-ITMED),AL1(L'BT7OVR)                       
         DC    AL1(CNVP1),AL1(BT7DEMV-ITMED),AL1(L'BT7DEMV)                     
R07EN    DC    AL1(ENDTAB)                                                      
*                                                                               
R10ST    DC    AL2(R10EN-R10ST+1)                                               
         DC    AL1(SEL),AL1(ITRCODE-ITMED),AL1(02),C'10'                        
         DC    AL1(CNVAA),AL1(ITMED-ITMED),AL1(L'ITMED)                         
         DC    AL1(CNVAA),AL1(ITCLT-ITMED),AL1(L'ITCLT)                         
         DC    AL1(CNVAA),AL1(ITPRD-ITMED),AL1(L'ITPRD)                         
         DC    AL1(CNVAN),AL1(ITEST-ITMED),AL1(L'ITEST)                         
         DC    AL1(CNVAN),AL1(ITRCODE-ITMED),AL1(L'ITRCODE)                     
         DC    AL1(CNVAN),AL1(ITMKT-ITMED),AL1(L'ITMKT)                         
         DC    AL1(CNVAA),AL1(ITDPT-ITMED),AL1(L'ITDPT)                         
         DC    AL1(CNVP0),AL1(ITSLN-ITMED),AL1(L'ITSLN)                         
         DC    AL1(CNVYMD),AL1(ITSTART-ITMED),AL1(L'ITSTART)                    
         DC    AL1(CNVYMD),AL1(ITEND-ITMED),AL1(L'ITEND)                        
         DC    AL1(CNVP5),AL1(ITDOL-ITMED),AL1(L'ITDOL)                         
         DC    AL1(CNVP3),AL1(ITDEM1-ITMED),AL1(L'ITDEM1)                       
         DC    AL1(CNVDEM),AL1(ITDEM1CD-ITMED),AL1(L'ITDEM1CD)                  
R10EN    DC    AL1(ENDTAB)                                                      
*                                                                               
*                                                                               
R20ST    DC    AL2(R20EN-R20ST+1)                                               
         DC    AL1(SEL),AL1(ITRCODE-ITMED),AL1(02),C'20'                        
         DC    AL1(CNVAA),AL1(ITMED-ITMED),AL1(L'ITMED)                         
         DC    AL1(CNVAA),AL1(ITCLT-ITMED),AL1(L'ITCLT)                         
         DC    AL1(CNVAA),AL1(ITPRD-ITMED),AL1(L'ITPRD)                         
         DC    AL1(CNVAN),AL1(ITEST-ITMED),AL1(L'ITEST)                         
         DC    AL1(CNVAN),AL1(ITRCODE-ITMED),AL1(L'ITRCODE)                     
         DC    AL1(CNVAN),AL1(ITMKT-ITMED),AL1(L'ITMKT)                         
         DC    AL1(CNVAA),AL1(ITSTAT-ITMED),AL1(L'ITSTAT)                       
         DC    AL1(CNVAA),AL1(ITDPT-ITMED),AL1(L'ITDPT)                         
         DC    AL1(CNVP0),AL1(ITSLN-ITMED),AL1(L'ITSLN)                         
         DC    AL1(CNVYMD),AL1(ITSTART-ITMED),AL1(L'ITSTART)                    
         DC    AL1(CNVYMD),AL1(ITEND-ITMED),AL1(L'ITEND)                        
         DC    AL1(CNVP4),AL1(ITSPOTS-ITMED),AL1(L'ITSPOTS)                     
         DC    AL1(CNVP5),AL1(ITDOL-ITMED),AL1(L'ITDOL)                         
         DC    AL1(CNVP3),AL1(ITDEM1-ITMED),AL1(L'ITDEM1)                       
         DC    AL1(CNVDEM),AL1(ITDEM1CD-ITMED),AL1(L'ITDEM1CD)                  
         DC    AL1(CNVP3),AL1(ITDEM2-ITMED),AL1(L'ITDEM2)                       
         DC    AL1(CNVDEM),AL1(ITDEM2CD-ITMED),AL1(L'ITDEM2CD)                  
         DC    AL1(CNVP3),AL1(ITDEM3-ITMED),AL1(L'ITDEM3)                       
         DC    AL1(CNVDEM),AL1(ITDEM3CD-ITMED),AL1(L'ITDEM3CD)                  
         DC    AL1(CNVP3),AL1(ITDEM4-ITMED),AL1(L'ITDEM4)                       
         DC    AL1(CNVDEM),AL1(ITDEM4CD-ITMED),AL1(L'ITDEM4CD)                  
R20EN    DC    AL1(ENDTAB)                                                      
*                                                                               
         DC    AL1(ENDTAB)                                                      
         EJECT                                                                  
OLDNUMS  CSECT                                                                  
         NMOD1 0,OLDNUMS                                                        
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R3,0(R1)                                                         
         USING ITMED,R3                                                         
         CLI   PROGPROF+9,C'Y'                                                  
         BNE   ORNUMX                                                           
         CLC   ITRCODE,=C'20'                                                   
         BE    ONUMR20                                                          
         CLC   ITRCODE,=C'05'                                                   
         BE    ONUMR05                                                          
         CLC   ITRCODE,=C'06'                                                   
         BE    ONUMR06                                                          
         CLC   ITRCODE,=C'07'                                                   
         BE    ONUMR07                                                          
         CLC   ITRCODE,=C'10'                                                   
         BE    ONUMR20                                                          
ORNUMX   XMOD1 1                                                                
ONUMR20  LA    R4,ITDEM1CD                                                      
         BAS   R9,CNVTOOLD                                                      
         LA    R4,ITDEM2CD                                                      
         BAS   R9,CNVTOOLD                                                      
         LA    R4,ITDEM3CD                                                      
         BAS   R9,CNVTOOLD                                                      
         LA    R4,ITDEM4CD                                                      
         BAS   R9,CNVTOOLD                                                      
         B     ORNUMX                                                           
ONUMR05  LA    R4,BTDCD1                                                        
         BAS   R9,CNVTOOLD                                                      
         LA    R4,BTDCD2                                                        
         BAS   R9,CNVTOOLD                                                      
         LA    R4,BTDCD3                                                        
         BAS   R9,CNVTOOLD                                                      
         LA    R4,BTDCD4                                                        
         BAS   R9,CNVTOOLD                                                      
         B     ORNUMX                                                           
ONUMR06  LA    R4,DMRD1N                                                        
         BAS   R9,CNVTOOLD                                                      
         LA    R4,DMRD2N                                                        
         BAS   R9,CNVTOOLD                                                      
         LA    R4,DMRD3N                                                        
         BAS   R9,CNVTOOLD                                                      
         LA    R4,DMRD4N                                                        
         BAS   R9,CNVTOOLD                                                      
         B     ORNUMX                                                           
ONUMR07  LA    R4,BT7DEMN                                                       
         BAS   R9,CNVTOOLD                                                      
         B     ORNUMX                                                           
         SPACE 2                                                                
CNVTOOLD LA    R1,1                CONVERT TO OLD FORMAT                        
         L     RE,VEQUOLD                                                       
         CLC   0(2,R4),=C'  '                                                   
         BNE   *+12                                                             
         MVC   0(2,R4),=PL2'0'                                                  
         BR    R9                                                               
         LA    RF,128                                                           
CTOOLD   CLC   0(2,RE),0(R4)                                                    
         BE    CTOOLD2                                                          
         LA    R1,1(R1)                                                         
         LA    RE,2(RE)                                                         
         BCT   RF,CTOOLD                                                        
         SR    R1,R1                                                            
CTOOLD2  CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   0(2,R4),DUB+6                                                    
         BR    R9                                                               
         LTORG                                                                  
         EJECT                                                                  
EXPEPC   CSECT                                                                  
         NMOD1 0,EXPEPC                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R3,0(R1)                                                         
         USING REQSW,R3                                                         
         L     R2,ADGOAL                                                        
         USING PEPKEY,R2                                                        
         CLC   PEPKDTE,BQENDP                                                   
         BH    NEXITP                                                           
         CLC   PEPKDTE,BQSTARTP                                                 
         BL    NEXITP                                                           
*                                                                               
         DROP  R2                                                               
         USING PEPELEMS,R2                                                      
         LA    R2,24(R2)                                                        
         MVI   RECDEL,1                                                         
         MVI   PRVSTG,0                                                         
EXPEP1   CLI   0(R2),0                                                          
         BE    EXPEP2                                                           
         CLI   1(R2),0                                                          
         BE    EXPEP2                                                           
         CLI   0(R2),X'10'                                                      
         BNE   EXPPEP1A                                                         
         TM    PEPDOL,X'80'                                                     
         BO    EXPPEP1A                                                         
         CLC   PRVSTG,3(R2)        PREV. STAGE GT. CURR                         
         BH    EXPPEP1A             YES - BYPASS                                
         MVC   PRVSTG,3(R2)                                                     
         MVI   RECDEL,1                                                         
         OC    4(2,R2),4(R2)       ELEMENT DELETED                              
         BZ    EXPPEP1A             YES - BYPASS                                
         MVI   RECDEL,0            TURN OFF DELETED SWITCH                      
         ST    R2,FULL                                                          
EXPPEP1A DS    0H'0'                                                            
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     EXPEP1                                                           
EXPEP2   L     R2,FULL                                                          
         CLI   RECDEL,1            RECORD DELETED                               
         BE    NEXITP               YES - BYPASS                                
         OC    PEPSPOTS,PEPSPOTS                                                
         BZ    NEXITP                                                           
         MVI   WSSW,1                                                           
         MVC   ITRCODE,=C'20'                                                   
         MVC   ITRNAME(1),PEPDSTG                                               
         OI    ITRNAME,X'F0'                                                    
         SR    RE,RE                                                            
         ICM   RE,3,PEPSPOTS                                                    
         CVD   RE,DUB                                                           
         MVC   ITSPOTS,DUB+5                                                    
         ICM   RE,15,PEPDOL                                                     
         MH    RE,=H'100'                                                       
         CVD   RE,DUB                                                           
         MVC   ITDOL,DUB+3                                                      
         MVC   ITDEM1,=PL4'0'                                                   
         MVC   ITDEM2,=PL4'0'                                                   
         MVC   ITDEM3,=PL4'0'                                                   
         MVC   ITDEM4,=PL4'0'                                                   
         LA    R9,PEPDEMS                                                       
         ZIC   R1,1(R2)                                                         
         AR    R1,R2                                                            
         SR    R1,R9                                                            
EXPEP2N  SR    R0,R0               GET LENGTH FOR NEW FORMAT                    
         D     R0,=F'5'                                                         
         LR    R0,R1                                                            
         CH    R0,=H'4'                                                         
         BNH   *+8                                                              
         LH    R0,=H'4'                                                         
         LA    R9,ITDEM1                                                        
*                                                                               
EXPEP3   SR    RE,RE                                                            
         ICM   RE,7,PEPDDVAL                                                    
         CLC   PRDBUFLN,=H'56'                                                  
         BE    *+8                                                              
         ICM   RE,7,PEPDMVAL       NEW FORMAT DEMOS ARE IN POS. 2               
         CVD   RE,DUB                                                           
         MVC   0(4,R9),DUB+4                                                    
         ZIC   RE,PEPDDCDE                                                      
         CVD   RE,DUB                                                           
         MVC   4(2,R9),DUB+6                                                    
         CLC   PRDBUFLN,=H'56'                                                  
         BE    *+14                                                             
         MVC   4(2,R9),PEPDMCDE                                                 
         LA    R2,1(R2)            NEW FORMAT ENTRIES ARE 5 BYTES               
         LA    R2,4(R2)                                                         
         LA    R9,6(R9)                                                         
         BCT   R0,EXPEP3                                                        
         DROP  R2                                                               
         SPACE 2                                                                
         L     R2,ADGOAL                                                        
         USING PEPREC,R2                                                        
         MVC   ITMED,QMED                                                       
         MVC   ITCLT,QCLT                                                       
         ZIC   RE,PEPKPRD                                                       
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RF,0(RE,RF)                                                      
         MVC   ITPRD,1(RF)                                                      
         ZIC   RE,PEPKEST                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ITEST,DUB+6(2)                                                   
         SR    RE,RE                                                            
         ICM   RE,3,PEPKMKT                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ITMKT(4),DUB+5(3)                                                
         ZIC   RE,PEPKLEN                                                       
         CVD   RE,DUB                                                           
         MVC   ITSLN,DUB+6                                                      
         MVC   ITDPT,PEPKDPT                                                    
         GOTO1 DATCON,DMCB,(X'02',PEPKDTE),ITSTART                              
         GOTO1 ADDAY,DMCB,ITSTART,ITEND,6                                       
         SR    R1,R1                                                            
         B     EXITP                                                            
NEXITP   LA    R1,1                                                             
EXITP    LTR   R1,R1                                                            
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
DETPRNT  CSECT                                                                  
         NMOD1 0,DETPRNT                                                        
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R3,0(R1)                                                         
         USING REQSW,R3                                                         
         CLI   REQSW,1                                                          
         BE    *+10                                                             
         MVC   RPTSWS(3),=X'010101'                                             
         MVI   REQSW,1                                                          
M51      GOTO1 VOPN                                                             
M51A     GOTO1 VGETIN                                                           
         CLC   ITRCODE,=C'03'                                                   
         BE    M53                                                              
         CLC   ITRCODE,=C'00'      REQUEST RECORD                               
         BNE   M54                                                              
         MVI   RCSUBPRG,1                                                       
         CLI   WSSW,0                                                           
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
         XC    P(132),P                                                         
         MVC   QMED,ITMED          YES - PRINT IT                               
         MVC   QCLT,ITCLT                                                       
         MVC   QPRD,ITPRD                                                       
         MVC   QEST,ITEST                                                       
         MVC   QMKT,ITMKT                                                       
         MVC   QSTA,ITSTAT                                                      
         MVC   QSTART(19),ITSTART                                               
         CLI   QSTART,C' '                                                      
         BNE   *+10                                                             
         MVC   QAREA+31(2),=C'ES'                                               
         MVC   QUESTOR,ITRNAME                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(10),=C'REQUEST = '                                           
         MVC   P+12(76),QAREA+4                                                 
         B     M58                                                              
M53      MVC   P+1(10),=C'ESTIMATE= '                                           
         MVC   P+12(80),ITMED                                                   
         B     M58                                                              
M54      CLI   WSSW,1              WEEKLY SUMMARY ACTIVE                        
         BNE   M6BD                 NO - TRY BUY DETAILS                        
         MVC   P(1),QMED          YES - FORMAT WEEKLY RECORDS                   
         MVI   RCSUBPRG,1          SET HEADLINES                                
         MVC   P+2(3),ITEST                                                     
         B     M55                                                              
M6BD     CLI   BDSW,1                                                           
         BNE   M6DONE                                                           
         CLC   ITRCODE,=C'04'                                                   
         BL    M51A                                                             
         CLC   ITRCODE,=C'08'                                                   
         BH    M51A                                                             
         MVI   RCSUBPRG,2                                                       
         MVC   P+2(1),ITMED                                                     
         MVC   P+6(3),ITCLT                                                     
         MVC   P+10(3),ITPRD                                                    
         MVC   P+15(2),ITRCODE                                                  
         MVC   P+18(4),ITMKT                                                    
         MVC   P+25(5),ITSTAT                                                   
         CLC   ITRCODE,=C'04'                                                   
         BE    M6SA                                                             
         CLC   ITRCODE,=C'08'                                                   
         BE    M6PROG                                                           
         CLC   ITRCODE,=C'06'                                                   
         BE    M6DEM                                                            
         MVC   P+32(1),ITDPT                                                    
         EDIT  (P2,ITSLN),(3,P+36)                                              
         MVC   P+40(6),ITSTART                                                  
         MVC   P+50(1),BTSDOW                                                   
         MVC   P+54(7),BTADAYS                                                  
         MVC   P+62(5),BTSTIM                                                   
         MVC   P+68(5),BTETIM                                                   
         EDIT  (P3,BTSPOTS),(5,P+75)                                            
         EDIT  (P5,BTCOST),(11,P+81),2,MINUS=YES                                
         MVC   P+95(3),BTPRD2                                                   
         TM    BTSLN2,X'0F'                                                     
         BZ    M6BE                                                             
         EDIT  (P2,BTSLN2),(3,P+99)                                             
M6BE     MVC   P+104(1),BTADAY                                                  
         MVC   P+106(5),BTATIM                                                  
         MVC   P+112(2),BTPADJ                                                  
         MVC   P+115(1),BTPURP                                                  
         CLC   ITRCODE,=C'07'                                                   
         BE    M6BREC                                                           
         CLI   PROGPROF+9,C'Y'                                                  
         BNE   M6BF                                                             
         EDIT  (P2,BTDCD1),(3,P+117)                                            
         EDIT  (P2,BTDCD2),(3,P+121)                                            
         EDIT  (P2,BTDCD3),(3,P+125)                                            
         EDIT  (P2,BTDCD4),(3,P+129)                                            
         B     M58                                                              
M6BF     MVC   P+117(1),BTDCD1                                                  
         EDIT  (B1,BTDCD1+1),(3,P+118),,ALIGN=LEFT                              
         MVC   P+121(1),BTDCD2                                                  
         EDIT  (B1,BTDCD2+1),(3,P+122),,ALIGN=LEFT                              
         MVC   P+125(1),BTDCD3                                                  
         EDIT  (B1,BTDCD3+1),(3,P+126),,ALIGN=LEFT                              
         MVC   P+129(1),BTDCD4                                                  
         EDIT  (B1,BTDCD4+1),(3,P+130),,ALIGN=LEFT                              
         B     M58                                                              
M6BREC   MVC   P+117(1),BT7SP                                                   
         CLI   BT7DEMN,C' '                                                     
         BE    M58                                                              
         MVC   P+123(1),BT7OVR                                                  
         EDIT  (P3,BT7DEMV),(6,P+125),1                                         
         MVC   P+120(2),BT7DEMN                                                 
         CLI   PROGPROF+9,C'Y'                                                  
         BNE   M58                                                              
         EDIT  (P2,BT7DEMN),(3,P+119)                                           
         B     M58                                                              
M6SA     MVC   P+31(2),SADCODE                                                  
         MVC   P+40(24),SADATA                                                  
         B     M58                                                              
M6DEM    MVC   DEMREC,ITMED                                                     
         XC    P(132),P                                                         
         MVC   P+1(8),=C'SPECIAL='                                              
         MVC   P+9(1),DMRSP                                                     
         MVC   P+11(6),=C'DEMOS='                                               
         CLI   PROGPROF+9,C'Y'                                                  
         BNE   M6NDEM                                                           
         EDIT  (P2,DMRD1N),(3,P+18)                                             
         MVC   P+22(1),DMRD1O                                                   
         EDIT  (P3,DMRD1V),(6,P+24),1                                           
         CP    DMRD2N,=PL2'0'                                                   
         BE    M6DEM2                                                           
         EDIT  (P2,DMRD2N),(3,P+31)                                             
         MVC   P+35(1),DMRD2O                                                   
         EDIT  (P3,DMRD2V),(6,P+37),1                                           
         CP    DMRD3N,=PL2'0'                                                   
         BE    M6DEM2                                                           
         EDIT  (P2,DMRD3N),(3,P+44)                                             
         MVC   P+48(1),DMRD3O                                                   
         EDIT  (P3,DMRD3V),(6,P+52),1                                           
         CP    DMRD4N,=PL2'0'                                                   
         BE    M6DEM2                                                           
         EDIT  (P2,DMRD4N),(3,P+59)                                             
         MVC   P+63(1),DMRD4O                                                   
         EDIT  (P3,DMRD4V),(6,P+65),1                                           
         B     M6DEM2                                                           
*                                                                               
M6NDEM   MVC   P+17(1),DMRD1N                                                   
         EDIT  (B1,DMRD1N+1),(3,P+18),,ALIGN=LEFT                               
         MVC   P+22(1),DMRD1O                                                   
         EDIT  (P3,DMRD1V),(6,P+24),1                                           
         CLC   DMRD2N,=C'  '                                                    
         BE    M6DEM2                                                           
         MVC   P+31(1),DMRD2N                                                   
         EDIT  (B1,DMRD2N+1),(3,P+32),,ALIGN=LEFT                               
         MVC   P+35(1),DMRD2O                                                   
         EDIT  (P3,DMRD2V),(6,P+37),1                                           
         CLC   DMRD3N,=C'  '                                                    
         BE    M6DEM2                                                           
         MVC   P+44(1),DMRD3N                                                   
         EDIT  (B1,DMRD3N+1),(3,P+45),,ALIGN=LEFT                               
         MVC   P+48(1),DMRD3O                                                   
         EDIT  (P3,DMRD3V),(6,P+52),1                                           
         CLC   DMRD4N,=C'  '                                                    
         BE    M6DEM2                                                           
         MVC   P+58(1),DMRD4N                                                   
         EDIT  (B1,DMRD4N+1),(3,P+59),,ALIGN=LEFT                               
         MVC   P+63(1),DMRD4O                                                   
         EDIT  (P3,DMRD4V),(6,P+65),1                                           
M6DEM2   GOTO1 REPORT                                                           
         B     M51A                                                             
*                                                                               
M6PROG   MVC   P+32(14),ITPROG                                                  
         MVC   P+47(16),ITFILM1                                                 
         MVC   P+64(16),ITPROGD                                                 
         GOTO1 REPORT                                                           
         B     M51A                                                             
*                                                                               
M6DONE   B     M5EXIT                                                           
         EJECT                                                                  
M55      CLC   ITRCODE,=C'10'                                                   
         BL    M51A                                                             
         CLC   ITRCODE,=C'30'                                                   
         BH    M51A                                                             
         MVC   P+6(3),ITCLT                                                     
         MVC   P+10(3),ITPRD                                                    
         MVC   P+15(2),ITRCODE                                                  
         MVC   P+18(4),ITMKT                                                    
         MVC   P+25(5),ITSTAT                                                   
         MVC   P+32(1),ITDPT                                                    
         EDIT  (P2,ITSLN),(3,P+36)                                              
         MVC   P+40(6),ITSTART                                                  
         MVC   P+47(6),ITEND                                                    
         EDIT  (P3,ITSPOTS),(5,P+54)                                            
         EDIT  (P5,ITDOL),(10,P+62),2,MINUS=YES                                 
         EDIT  (P4,ITDEM1),(8,P+72),1                                           
         EDIT  (P4,ITDEM2),(8,P+86),1                                           
         EDIT  (P4,ITDEM3),(8,P+100),1                                          
         EDIT  (P4,ITDEM4),(8,P+114),1                                          
         CLI   PROGPROF+9,C'Y'                                                  
         BNE   M54ND                                                            
         EDIT  (P2,ITDEM1CD),(4,P+81)                                           
         EDIT  (P2,ITDEM2CD),(4,P+95)                                           
         EDIT  (P2,ITDEM3CD),(4,P+109)                                          
         EDIT  (P2,ITDEM4CD),(4,P+123)                                          
         B     M54NDX                                                           
M54ND    MVC   P+81(1),ITDEM1CD                                                 
         ZIC   RF,ITDEM1CD+1                                                    
         EDIT  (RF),(3,P+82),,ALIGN=LEFT                                        
         MVC   P+95(1),ITDEM2CD                                                 
         ZIC   RF,ITDEM2CD+1                                                    
         EDIT  (RF),(3,P+96),,ALIGN=LEFT                                        
         MVC   P+109(1),ITDEM3CD                                                
         ZIC   RF,ITDEM3CD+1                                                    
         EDIT  (RF),(3,P+110),,ALIGN=LEFT                                       
         MVC   P+123(1),ITDEM4CD                                                
         ZIC   RF,ITDEM4CD+1                                                    
         EDIT  (RF),(3,P+124),,ALIGN=LEFT                                       
         MVC   P+128(4),ITRNAME                                                 
M54NDX   CLC   ITRCODE,=C'11'                                                   
         BE    M56                                                              
         CLC   ITRCODE,=C'21'                                                   
         BNE   M58                                                              
M56      EDIT  (P4,ITDEM1),(8,P+72),2                                           
         EDIT  (P4,ITDEM2),(8,P+86),2                                           
         EDIT  (P4,ITDEM3),(8,P+100),2                                          
         EDIT  (P4,ITDEM4),(8,P+114),2                                          
M58      GOTO1 REPORT                                                           
         B     M51A                                                             
M5EXIT   XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
BYDET    CSECT                                                                  
         NMOD1 0,BYDET                                                          
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R3,0(R1)                                                         
         USING REQSW,R3                                                         
         STM   RA,RC,BYDRA                                                      
         ST    R3,BYDR3                                                         
*                                                                               
         MVI   ITMED,C' '                                                       
         MVC   ITMED+1(79),ITMED                                                
         MVC   ITMED,QMED                                                       
         MVC   ITCLT,QCLT                                                       
         MVI   BDSW,1                                                           
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         SR    R8,R8                                                            
         IC    R8,BUYKEST                                                       
         CVD   R8,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ITEST,DUB+6(2)                                                   
         DROP  RE                                                               
         MVC   ITMKT,MKT                                                        
         MVC   ITSTAT,STA                                                       
         SR    RE,RE                                                            
         IC    RE,BFSLN                                                         
         CVD   RE,DUB                                                           
         MVC   ITSLN,DUB+6                                                      
         SR    RE,RE                                                            
         IC    RE,BPRD                                                          
         CLI   BPRD,X'FF'                                                       
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RF,0(RE,RF)                                                      
         MVC   ITPRD,1(RF)                                                      
         MVC   ITRCODE,=C'05'                                                   
         CLI   PROGPROF+2,C'B'                                                  
         BNE   *+10                                                             
         MVC   ITRCODE,=C'07'                                                   
         MVC   DEMREC,ITMED                                                     
         MVC   DMRCODE,=C'06'                                                   
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         MVI   DMRSP,C'N'                                                       
         CLI   BDPROGT-1,0                                                      
         BNE   *+8                                                              
         MVI   DMRSP,C'Y'                                                       
         MVC   PROGRAM,BDPROGRM                                                 
         MVC   ITDPT,BDDAYPT       SET BUY DESCRIPTION DATA                     
         MVC   BTPADJ(1),BDPROGT                                                
         MVC   BTPURP,BDPURP                                                    
         GOTO1 DATCON,DMCB,(X'03',BDSTART),ITSTART                              
         GOTO1 GETDAY,DMCB,ITSTART,WORK                                         
         SR    R7,R7                                                            
         IC    R7,DMCB                                                          
         BCTR  R7,0                                                             
         OI    DMCB,X'F0'                                                       
         MVC   BTSDOW,DMCB                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,BDDAY                                                         
         SLL   RF,25                                                            
         LA    R7,7                                                             
         LA    R8,BTADAYS                                                       
BYD1     SLDL  RE,1                                                             
         STC   RE,0(R8)                                                         
         OI    0(R8),X'F0'                                                      
         LA    R8,1(R8)                                                         
         SR    RE,RE                                                            
         BCT   R7,BYD1                                                          
         LA    R7,2                                                             
         MVC   HALF,BDTIMEND                                                    
         OC    HALF,HALF                                                        
         BNZ   BYD2                                                             
         MVC   HALF,BDTIMST                                                     
BYD2     LH    RF,HALF                                                          
         CH    RF,=H'2400'                                                      
         BNE   *+6                                                              
         SR    RF,RF                                                            
         MVI   BTSTIM,C'A'                                                      
         CH    RF,=H'1200'                                                      
         BL    *+12                                                             
         MVI   BTSTIM,C'P'                                                      
         CH    RF,=H'1300'                                                      
         BL    *+8                                                              
         SH    RF,=H'1200'                                                      
         CH    RF,=H'100'                                                       
         BNL   *+8                                                              
         AH    RF,=H'1200'                                                      
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         BCT   R7,BYD3                                                          
         MVC   BTSTIM+4(1),BTSTIM                                               
         UNPK  BTSTIM(4),DUB+5(3)                                               
         B     BYD31                                                            
BYD3     MVC   BTETIM+4(1),BTSTIM                                               
         UNPK  BTETIM(4),DUB+5(3)                                               
         MVC   HALF,BDTIMST                                                     
         B     BYD2                                                             
         DROP  R6                                                               
         USING NDELEM,R6                                                        
BYD31    LA    R6,24(R6)                                                        
BYD3A    SR    R0,R0               GET DEMO ELEMENT                             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),2                                                          
         BNE   BYD3A                                                            
         ST    R6,DELADDR          SAVE DEMO ELEMENT ADDRESS                    
         DROP  R6                                                               
         L     R4,MEDBUFF                                                       
         USING MEDBLOCK,R4                                                      
         L     RF,MEDADEMO                                                      
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         OC    0(4,RF),0(RF)       USE BUY RECORD IF NOT THERE                  
         BZ    *+10                                                             
         MVC   DELADDR,0(RF)                                                    
         L     RF,DELADDR                                                       
         USING NDELEM,RF                                                        
         MVC   PROGRAMD,NDPROG                                                  
         DROP  RF                                                               
         DROP  R4                                                               
         L     R6,ADBUY            RESET R6                                     
         USING BUYREC,R6                                                        
         SPACE 2                                                                
BYD4     CLI   BUYKPRD,X'FF'                                                    
         BNE   BRANDBUY                                                         
         DROP  R6                                                               
         LA    R6,24(R6)           BYPASS HEADER                                
         USING REGELEM,R6                                                       
POL1     CLI   RCODE,X'0B'                                                      
         BL    POLNXT                                                           
         CLI   RCODE,X'0D'                                                      
         BH    POLNXT                                                           
         TM    RSTATUS,X'C2'       BYPASS MISSED SPOTS                          
         BNZ   POLNXT                                                           
         TM    RSTATUS,X'04'       BYPASS HIATUS                                
         BO    POLNXT                                                           
         MVC   WORK(2),RDATE                                                    
         CLC   WORK(2),BQSTARTP    TEST SPOT WITHIN DATES                       
         BL    POLNXT                                                           
         CLC   WORK(2),BQENDP                                                   
         BH    POLNXT                                                           
         CLI   BPRD,X'FF'                                                       
         BE    *+14                                                             
         CLC   BPRD,RPPRD                                                       
         BNE   POLNXT                                                           
         GOTO1 DATCON,DMCB,(X'02',WORK),ITSTART                                 
         L     R7,ADBUY                                                         
         GOTO1 GETRATE,DMCB,(X'FF',GRSPOTS),(X'00',(R7)),(R6)                   
         L     RE,GRSPOTS          SPOTS                                        
         LTR   RE,RE                                                            
         BZ    POLNXT                                                           
         ST    R6,THISELEM                                                      
         CLI   QRERATE,C' '        NEED AFFID DEMOS IF RERATE                   
         BE    POLNRRX                                                          
         LA    RE,BYDHOOK                                                       
         ST    RE,SPOTHOOK                                                      
         L     R4,MEDBUFF                                                       
         USING MEDBLOCK,R4                                                      
         MVI   MEDEXTAV,C'Y'                                                    
         MVC   MEDBRAND,RPPRD                                                   
         MVC   MEDSPTLN,RPTIME                                                  
         L     R5,SVRRMODE                                                      
         GOTO1 MEDGETBY,DMCB,(RA),(R5)                                          
         L     RF,MEDADEMO                                                      
         MVC   DELADDR,4(RF)                                                    
         XC    SPOTHOOK,SPOTHOOK                                                
         LA    RE,MEDVPACH                                                      
         LA    RF,MEDVAACH                                                      
         L     R6,DELADDR                                                       
         USING NDELEM,R6                                                        
         MVC   PROGRAMD,NDPROG                                                  
         DROP  R6                                                               
         LA    R6,24(R6)                                                        
         LA    R1,4                                                             
POLDEM   CLI   1(R6),0                                                          
         BE    POLNRRX                                                          
         MVC   3(1,R6),3(RF)                                                    
         MVC   6(2,R6),2(RE)                                                    
         LA    R6,8(R6)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,POLDEM                                                        
         DROP  R4                                                               
POLNRRX  L     R6,THISELEM                                                      
         USING REGELEM,R6                                                       
         CVD   RE,DUB                                                           
         MVC   BTSPOTS,DUB+5       COST                                         
         L     RE,GRDOL                                                         
         CVD   RE,DUB                                                           
         MVC   BTCOST,DUB+3                                                     
         SR    RE,RE                                                            
         IC    RE,RPTIME                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   ITSLN,DUB+6                                                      
         SR    RE,RE               PRODUCT CODE A                               
         IC    RE,RPPRD                                                         
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RF,0(RE,RF)                                                      
         LR    RE,RF                                                            
         MVC   ITPRD,1(RF)                                                      
         CLC   PRDBUFLN,=H'56'                                                  
         BNE   POL1ND                                                           
         MVC   FULL,28(RF)                                                      
         LA    RE,FULL                                                          
         LA    R0,4                                                             
         LA    RF,BTDCD1                                                        
         SR    R8,R8                                                            
POL1A    CLI   0(RE),0                                                          
         BE    POL2                                                             
         IC    R8,0(RE)                                                         
         CVD   R8,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   0(2,RF),DUB+6                                                    
         LA    RE,1(RE)                                                         
         LA    RF,2(RF)                                                         
         BCT   R0,POL1A                                                         
POL1NDX  BAS   RE,BLDDEM                                                        
         CLI   RLEN,15                                                          
         BL    POL2                                                             
         SR    RE,RE                                                            
         IC    RE,RPPRD+4                                                       
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RF,0(RE,RF)                                                      
         MVC   BTPRD2,1(RF)                                                     
         SR    RE,RE                                                            
         IC    RE,RPTIME+4                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   BTSLN2,DUB+6                                                     
POL2     CLI   PROGPROF+4,C'Y'                                                  
         BE    GETAFFID                                                         
         BAS   R9,PUTOUT                                                        
POLNXT   SR    RF,RF                                                            
         IC    RF,RLEN                                                          
         AR    R6,RF                                                            
         CLI   0(R6),0                                                          
         BE    BYDETX                                                           
         B     POL1                                                             
         DROP  R6                                                               
         SPACE 2                                                                
POL1ND   LA    RF,BTDCD1                                                        
         LA    R0,4                                                             
         LA    RE,28(RE)                                                        
         ST    RE,FULL                                                          
POL1ND1  CLI   1(RE),0                                                          
         BE    POL1NDX                                                          
         MVC   0(2,RF),1(RE)                                                    
         LA    RE,3(RE)                                                         
         LA    RF,2(RF)                                                         
         BCT   R0,POL1ND1                                                       
         B     POL1NDX                                                          
         EJECT                                                                  
BRANDBUY LA    R6,24(R6)                                                        
         USING PBELEM,R6                                                        
BB1      CLI   PBCODE,0            SET PIGGYBACK BRAND IF ACTIVE                
         BE    BBNOPIG                                                          
         CLI   PBCODE,4                                                         
         BE    BB2                                                              
         SR    RF,RF                                                            
         IC    RF,PBLEN                                                         
         AR    R6,RF                                                            
         B     BB1                                                              
*                                                                               
BB2      CLI   PBLEN,10            ONLY ONE PIGGYBACK                           
         BL    *+6                                                              
         DC    H'0'                 NO-DUMP                                     
         SR    RF,RF                                                            
         IC    RF,PBTIME                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   BTSLN2,DUB+6                                                     
         MVC   BTPRD2,PBPRD                                                     
         DROP  R6                                                               
         SPACE 2                                                                
BBNOPIG  L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         SR    RE,RE                                                            
         IC    RE,BUYKPRD                                                       
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RF,0(RE,RF)                                                      
         MVC   ITPRD,1(RF)                                                      
         LR    RE,RF                                                            
*        MVC   FULL,28(RF)                                                      
         LA    RF,BTDCD1                                                        
         LA    R0,4                                                             
         LA    RE,28(RE)                                                        
         ST    RE,FULL                                                          
BB1ND1   CLI   1(RE),0                                                          
         BE    BB1NDX                                                           
         MVC   0(2,RF),1(RE)                                                    
         LA    RE,3(RE)                                                         
         LA    RF,2(RF)                                                         
         BCT   R0,BB1ND1                                                        
         SPACE 1                                                                
BB1NDX   BAS   RE,BLDDEM                                                        
         DROP  R6                                                               
         LA    R6,24(R6)                                                        
         USING REGELEM,R6                                                       
BB10     CLI   RCODE,6                                                          
         BL    BBNXT                                                            
         CLI   ACODE,8                                                          
         BH    BBNXT                                                            
         TM    RSTATUS,X'40'                                                    
         BO    BBNXT                                                            
         L     R7,ADBUY                                                         
         GOTO1 GETRATE,DMCB,(X'FF',GRSPOTS),(X'00',(R7)),(R6)                   
         L     RE,GRSPOTS                                                       
         LTR   RE,RE                                                            
         BZ    BBNXT                                                            
         CVD   RE,DUB                                                           
         MVC   BTSPOTS,DUB+5       COST                                         
         L     RE,GRDOL                                                         
         CVD   RE,DUB                                                           
         MVC   BTCOST,DUB+3                                                     
         CLI   PROGPROF+4,C'Y'                                                  
         BE    GETAFFID                                                         
         BAS   R9,PUTOUT                                                        
*                                                                               
BBNXT    SR    RF,RF                                                            
         IC    RF,RLEN                                                          
         AR    R6,RF                                                            
         CLI   0(R6),0                                                          
         BE    BYDETX                                                           
         B     BB10                                                             
         EJECT                                                                  
GETAFFID L     R0,GRSPOTS                                                       
         MVC   BTSPOTS,=PL3'1'                                                  
         LR    R7,R6                                                            
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AR    R7,RE                                                            
         USING AFFELEM,R7                                                       
AFF1     CLI   0(R7),X'10'                                                      
         BNE   AFF2                                                             
         GOTO1 DATCON,DMCB,(X'02',ADATE),WORK                                   
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         OI    DMCB,X'F0'                                                       
         MVC   BTADAY,DMCB                                                      
         MVC   HALF,ATIME                                                       
         LH    RF,HALF                                                          
         CH    RF,=H'2400'                                                      
         BNE   *+6                                                              
         SR    RF,RF                                                            
         MVI   BTATIM+4,C'A'                                                    
         CH    RF,=H'1200'                                                      
         BL    *+12                                                             
         MVI   BTATIM+4,C'P'                                                    
         CH    RF,=H'1300'                                                      
         BL    *+8                                                              
         SH    RF,=H'1200'                                                      
         CH    RF,=H'100'                                                       
         BNL   *+8                                                              
         AH    RF,=H'1200'                                                      
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BTATIM(4),DUB+5(3)                                               
*                                                                               
         CLI   PROGPROF+12,C'Y'    FILM NUMBER REQUIRED                         
         BNE   AFF0A                                                            
         MVC   FILM,=CL16' '                                                    
         LR    RE,R7               FIND THE FILM ELEMENT                        
AFF0     ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),X'11'         INTEGRATION - SB NETWORK ONLY                
         BE    AFF0                                                             
         CLI   0(RE),X'12'         FILM ELEMENT                                 
         BNE   *+12                                                             
         ST    RE,FULL                                                          
         BAS   RE,GETFILM          DECODE THE FILM                              
*                                                                               
AFF0A    CLI   PROGPROF+2,C'B'     KEEP DEMOS IF REGULAR REQUEST                
         BNE   AFF1A                                                            
         MVC   BT7DEMN,=PL2'0'                                                  
         MVC   BT7DEMV,=PL3'0'                                                  
         MVC   BT7DEMN,DMRD1N                                                   
         MVC   BT7DEMV,DMRD1V                                                   
AFF1A    BAS   R9,PUTOUT                                                        
         BCTR  R0,0                                                             
         SR    RF,RF                                                            
         IC    RF,1(R7)                                                         
         AR    R7,RF                                                            
         B     AFF1                                                             
AFF2     LTR   R0,R0                                                            
         BZ    NEXTREG                                                          
         BM    NEXTREG                                                          
         MVI   BTADAY,C'0'                                                      
         MVC   BTATIM,BTADAY                                                    
AFF3     BAS   R9,PUTOUT                                                        
         BCT   R0,AFF3                                                          
NEXTREG  CLI   0(R6),9                                                          
         BL    BBNXT                                                            
         B     POLNXT                                                           
*                                                                               
PUTOUT   LH    RF,RECSEQ                                                        
         LA    RF,1(RF)                                                         
         STH   RF,RECSEQ                                                        
         MVC   BTSEQ,RECSEQ                                                     
         CLI   PROGPROF+2,C'B'                                                  
         BNE   PUTOUT2                                                          
         MVC   BT7SP(7),DMRSP                                                   
         MVI   BT7SPAR,C' '                                                     
PUTOUT2  GOTO1 VWRREC                                                           
         MVC   SAVEREC,ITMED                                                    
         CLI   PROGPROF+2,C'D'                                                  
         BNE   PUTOUT3                                                          
         MVC   SAVEREC,ITMED                                                    
         MVC   ITMED(80),DEMREC                                                 
         LH    RF,RECSEQ                                                        
         LA    RF,1(RF)                                                         
         STH   RF,RECSEQ                                                        
         MVC   BTSEQ,RECSEQ                                                     
         GOTO1 VWRREC                                                           
         MVC   ITMED(80),SAVEREC                                                
*                                                                               
PUTOUT3  CLI   PROGPROF+12,C'Y'    PROGRAM RECORD REQUIRED                      
         BNER  R9                                                               
         MVI   ITMED,C' '                                                       
         MVC   ITMED+1(79),ITMED                                                
         MVC   ITMED(ITPROG-ITMED),SAVEREC                                      
         MVC   ITRCODE,=C'08'                                                   
         MVC   ITPROG,PROGRAM                                                   
         MVC   ITFILM1(16),FILM                                                 
         MVC   ITPROGD,PROGRAMD                                                 
         LH    RF,RECSEQ                                                        
         LA    RF,1(RF)                                                         
         STH   RF,RECSEQ                                                        
         MVC   BTSEQ,RECSEQ                                                     
         GOTO1 VWRREC                                                           
         MVC   FILM,=CL16' '                                                    
         MVC   ITMED(80),SAVEREC                                                
         BR    R9                                                               
*                                                                               
BYDETX   XMOD1 1                                                                
         USING *,RF                                                             
BYDHOOK  NTR1  BASE=BYDRB          FILTER FOR AFFID RERATES                     
         DROP  RF                                                               
         LM    RA,RC,BYDRA                                                      
         L     R3,BYDR3                                                         
         CLC   SPOTADDR,THISELEM                                                
         BE    *+8                                                              
         MVI   SPOTYORN,C'N'                                                    
         XIT1                                                                   
         SPACE 2                                                                
BYDRA    DS    F                                                                
BYDRB    DS    F                                                                
         DS    F                                                                
BYDR3    DS    F                                                                
         EJECT                                                                  
GETFILM  NTR1                                                                   
         MVC   FLMAREC,AREC                                                     
         L     R7,FULL             POINTER TO FILM ELEMENT                      
         USING FLMELEM,R7                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AA1'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+6(2),FLMNUM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SOMETHING WRONG - JUST EXIT                  
         BNE   GETFILMX                                                         
         LA    RE,FLMAREA                                                       
         ST    RE,AREC                                                          
         GOTO1 GET                                                              
         MVC   FILM(8),FLMAREA+5                                                
         MVC   KEY,KEYSAVE                                                      
         CLI   1(R7),5             HANDLE PIGGYBACKS                            
         BNH   GETFILMX                                                         
         MVC   KEY+6(2),FLMNUM+2                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETFILMX                                                         
         GOTO1 GET                                                              
         MVC   FILM+8(8),FLMAREA+5                                              
GETFILMX MVC   AREC,FLMAREC        RESTORE RECORD AREA POINTER                  
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
FILM     DS    CL16                                                             
SAVEREC  DS    CL80                                                             
FLMAREA  DS    CL400                                                            
FLMAREC  DS    F                                                                
         EJECT                                                                  
* BUILD BUYERS DEMO RECORD                                                      
         DROP  R6                                                               
BLDDEM   NTR1                                                                   
         CLC   PRDBUFLN,=H'56'                                                  
         BNE   BNDDEM                                                           
         LA    RF,4                INITIALIZE RECORD                            
         LA    R9,DMRD1N                                                        
BLDDEM1  MVC   0(2,R9),=PL2'0'                                                  
         MVI   2(R9),C'N'                                                       
         MVC   3(3,R9),=PL3'0'                                                  
         LA    R9,6(R9)                                                         
         BCT   RF,BLDDEM1                                                       
         SPACE 2                                                                
         LA    RE,FULL             INSERT DEMO VALUES                           
         LA    RF,4                                                             
         LA    R9,DMRD1N                                                        
BLDDEM2  CLI   0(RE),0                                                          
         BE    BLDDEMX                                                          
         L     R6,DELADDR                                                       
         USING NDELEM,R6                                                        
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R1,R6                                                            
         SH    R1,=H'24'                                                        
BLDDEM3  CR    R6,R1                                                            
         BH    BLDDEM4                                                          
         MVC   HALF(1),NDEMNO                                                   
         NI    HALF,X'7F'                                                       
         CLC   HALF(1),0(RE)                                                    
         BE    *+12                                                             
         LA    R6,4(R6)                                                         
         B     BLDDEM3                                                          
         TM    NDEMNO,X'80'                                                     
         BZ    *+8                                                              
         MVI   2(R9),C'Y'                                                       
         CLI   PROGPROF+10,C'Y'    SUPPRESS OVERRIDE FLAG                       
         BNE   *+8                                                              
         MVI   2(R9),C' '                                                       
         MVC   HALF,NDEMRAW                                                     
         LH    R4,HALF                                                          
         SR    R7,R7                                                            
         IC    R7,NDSVI                                                         
         SRDA  R4,32                                                            
         MR    R4,R7                                                            
         SLA   R5,1                                                             
         D     R4,=F'100'                                                       
         A     R5,=F'1'                                                         
         SRA   R5,1                                                             
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   3(3,R9),DUB+5                                                    
BLDDEM4  SR    R5,R5                                                            
         IC    R5,0(RE)                                                         
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   0(2,R9),DUB+6                                                    
         LA    R9,6(R9)                                                         
         LA    R6,4(R6)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,BLDDEM2                                                       
         B     BLDDEMX                                                          
         SPACE 2                                                                
BNDDEM   LA    RF,4                INITIALIZE RECORD                            
         LA    R9,DMRD1N                                                        
BNDDEM1  MVC   0(2,R9),=C'  '                                                   
         MVI   2(R9),C'N'                                                       
         MVC   3(3,R9),=PL3'0'                                                  
         LA    R9,6(R9)                                                         
         BCT   RF,BNDDEM1                                                       
         SPACE 2                                                                
         L     RE,FULL             INSERT DEMO VALUES                           
         LA    RF,4                                                             
         LA    R9,DMRD1N                                                        
BNDDEM2  CLI   1(RE),0                                                          
         BE    BLDDEMX                                                          
         L     R6,DELADDR                                                       
         USING NDELEM,R6                                                        
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R1,R6                                                            
BNDDEM3  CR    R6,R1                                                            
         BH    BNDDEM4                                                          
         MVC   HALF,1(R6)                                                       
         CLC   HALF,1(RE)                                                       
         BE    *+12                                                             
         LA    R6,8(R6)                                                         
         B     BNDDEM3                                                          
         TM    4(R6),X'80'                                                      
         BZ    *+8                                                              
         MVI   2(R9),C'Y'                                                       
         CLI   PROGPROF+10,C'Y'    SUPPRESS OVERRIDE FLAG                       
         BNE   *+8                                                              
         MVI   2(R9),C' '                                                       
         MVC   HALF,6(R6)                                                       
         LH    R4,HALF                                                          
         SR    R7,R7                                                            
         IC    R7,3(R6)                                                         
         SRDA  R4,32                                                            
         MR    R4,R7                                                            
         SLA   R5,1                                                             
         D     R4,=F'100'                                                       
         A     R5,=F'1'                                                         
         SRA   R5,1                                                             
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   3(3,R9),DUB+5                                                    
BNDDEM4  SR    R5,R5                                                            
         MVC   0(2,R9),1(RE)                                                    
         LA    R9,6(R9)                                                         
         LA    R6,8(R6)                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,BNDDEM2                                                       
BLDDEMX  XIT1                                                                   
         LTORG                                                                  
GRSPOTS  DS    F'0'                                                             
GRDOL    DS    F'0'                                                             
GRDOLN   DS    F'0'                                                             
GRDOLF   DS    F'0'                                                             
         EJECT                                                                  
ESTREAD  CSECT                                                                  
         NMOD1 0,ESTREAD                                                        
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R3,0(R1)                                                         
         USING REQSW,R3                                                         
         L     RE,ADEST                                                         
         ST    RE,AREC                                                          
         CLI   PROGPROF+5,C'N'                                                  
         BE    GEEXIT1                                                          
         MVC   EKEYSAVE,KEY                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDR,R4                                                        
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'ALL'                                                  
         BNE   *+10                                                             
         XC    EKEYPRD,EKEYPRD                                                  
GEHIGH   GOTO1 HIGH                                                             
         B     GEREC                                                            
         SPACE 2                                                                
GESEQ    GOTO1 SEQ                                                              
         SPACE 2                                                                
GEREC    LA    R4,KEY                                                           
         CLC   KEY(4),KEYSAVE                                                   
         BNE   GEEXIT                                                           
         CLC   QPRD,=C'ALL'                                                     
         BE    GEREC1                                                           
         CLC   EKEYPRD,QPRD        CHECK PRODCUT                                
         BE    GEFEST                                                           
GEREC1   CLC   EKEYPRD,=C'POL'                                                  
         BE    GESEQ                                                            
*                                                                               
* FILTER KEY ON ESTIMATE NUMBER                                                 
GEFEST   CLI   EKEYEST,0                                                        
         BNE   GEFESTA                                                          
         MVI   EKEYEST,1                                                        
         B     GEHIGH                                                           
GEFESTA  DS    0H                                                               
         CLI   EKEYEST+1,0                                                      
         BE    GEFEST2                                                          
         SPACE 2                                                                
GEFEST1  MVI   EKEYEST+1,X'FF'     SET NEXT PRODUCT                             
         MVC   EKEYEST+2(4),EKEYEST+1                                           
         B     GEHIGH                                                           
         SPACE 2                                                                
GEFEST2  CLI   BEST,0                                                           
         BE    GEFEST5                                                          
         CLC   EKEYEST,BEST        ESTIMATE LOW                                 
         BNL   GEFEST3                                                          
         MVC   EKEYEST,BEST         YES-SET FIRST ESTIMATE                      
         B     GEHIGH                                                           
         SPACE 2                                                                
GEFEST3  CLI   BESTEND,0           END ESTIMATE GIVEN                           
         BNE   GEFEST4              YES - CHECK IT                              
         CLC   EKEYEST,BEST         NO - MUST BE EQUAL                          
         BE    GEFEST5                                                          
         B     GEFEST1                                                          
         SPACE 2                                                                
GEFEST4  CLC   EKEYEST,BESTEND     ESTIMATE WITHIN RANGE                        
         BH    GEFEST1              NO - NEXT PRODUCT                           
         SPACE 2                                                                
GEFEST5  GOTO1 GET                 KEY FILTERS PASSED - GET RECORD              
         L     R4,AREC                                                          
         CLC   EEND,QSTART         DATE FILTERS                                 
         BL    GESEQ                                                            
         CLC   ESTART,QEND                                                      
         BH    GESEQ                                                            
         SPACE 2                                                                
         MVC   ITMED,QMED                                                       
         MVC   ITCLT,QCLT                                                       
         MVC   ITPRD,EKEYPRD                                                    
         SR    RE,RE                                                            
         IC    RE,EKEYEST                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ITEST,DUB+6(2)                                                   
         UNPK  ITEST,DUB                                                        
         MVC   ITRCODE,=C'03'                                                   
         MVC   ERSTART,ESTART                                                   
         MVC   EREND,EEND                                                       
         MVC   ERDESC,EDESC                                                     
         MVC   ERFILTER,EPROF                                                   
         GOTO1 VWRREC                                                           
         B     GESEQ                                                            
         SPACE 2                                                                
GEEXIT   MVC   KEY,EKEYSAVE                                                     
         GOTO1 HIGH                                                             
         GOTO1 GET                                                              
GEEXIT1  XMOD1 1                                                                
         SPACE 2                                                                
EKEYSAVE DS    CL32                                                             
         LTORG                                                                  
         LTORG                                                                  
         EJECT                                                                  
         BUFF  LINES=200,ROWS=1,COLUMNS=11,FLAVOR=BINARY,KEYLIST=(17,A)         
         LTORG                                                                  
         EJECT                                                                  
OUT      DCB   DDNAME=OUT,             DOS SYS005                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=04000,          DOS BLKSIZE=04000               X        
               MACRF=PM                                                         
IN       DCB   DDNAME=IN,              DOS SYS005                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=04000,          DOS BLKSIZE=04000               X        
               MACRF=GM,                                               X        
               EODAD=EOTAPE                                                     
PCOUT    DCB   DDNAME=OUT,             DOS SYS005                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00256,                                            X        
               BLKSIZE=02560,          DOS BLKSIZE=02560               X        
               MACRF=PM                                                         
       ++INCLUDE SPGENPEPSI                                                     
EQUIVOLD CSECT                                                                  
       ++INCLUDE DEEQUIVOLD                                                     
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095SPREPM502 11/21/19'                                      
         END                                                                    
