*          DATA SET SPREPLR02  AT LEVEL 068 AS OF 05/05/03                      
*PHASE SPLR02A                                                                  
         TITLE 'SPREPLR02 MARKET RANK REPORT'                                   
SPLR02   CSECT                                                                  
         NMOD1 0,SPLR02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         XC    RANKCTR(24),RANKCTR                                              
         XC    USTOTAL,USTOTAL                                                  
         MVI   FORCEHED,C'Y'                                                    
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVI   CPROF+3,C'0'                                                     
         CLI   QOPT1,C'A'                                                       
         BNE   *+14                                                             
         MVI   CPROF+3,C'1'                                                     
         MVC   SMATABLE(2),=X'FFFF'                                             
         CLI   QOPT3,C' '                                                       
         BE    *+10                                                             
         MVC   DEMONUM+1(1),QOPT3                                               
         PACK  DUB,QEST                                                         
         CVB   RF,DUB                                                           
         STC   RF,DEMONUM+2                                                     
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         L     R4,ADBLOCK                                                       
         USING DBLOCK,R4                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,QAGY                                                    
         DROP  R4                                                               
         GOTO1 DEMOCON,DMCB,(1,DEMONUM),(2,DNAME1),(C'S',ADBLOCK),     X        
               (SPOTPROF+9,(R4))                                                
         MVC   QSTART(12),=C'820101820101'                                      
         LA    RE,RANKTAB                                                       
         L     RF,=F'10000'                                                     
         XCEF                                                                   
         L     R4,ADBLOCK                                                       
         USING DBLOCK,R4                                                        
         XC    0(256,R4),0(R4)     SET UP DBLOCK TO READ MARKETS                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBGETMKB                                                 
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,QOPT1                                                   
         MVC   DBSELAGY,QAGY                                                    
*                                                                               
         MVC   WORK(4),QBOOK1      MOVE YYMM                                    
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(3,WORK+6)                                      
         MVC   DBSELBK(2),WORK+6                                                
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,ADBUY                                                         
         ST    RE,DBAREC                                                        
         GOTO1 DEMAND,DMCB,ADBLOCK,SVMRKT                                       
         L     R1,RANKCTR                                                       
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         CLI   QOPT2,C'S'          CHECK FOR SPILL REPORT                       
         BE    SPILLRPT                                                         
         MVC   INDXCTR,=F'1'                                                    
         MVC   LOOPCTR,RANKCTR                                                  
GETUNV   OC    LOOPCTR,LOOPCTR     GET MARKET UNIVERSES                         
         BZ    DOUSTOT                                                          
         L     R1,LOOPCTR                                                       
         BCTR  R1,0                                                             
         ST    R1,LOOPCTR                                                       
         L     R1,INDXCTR                                                       
         BCTR  R1,0                                                             
         MH    R1,HRNKLEN                                                       
         LA    R6,RANKTAB(R1)                                                   
         USING RANKTABD,R6                                                      
         L     R4,ADBLOCK                                                       
         MVC   DBSELRMK,RMKT                                                    
         MVI   DBSELDAY,X'40'                                                   
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1715'                                           
         MVI   DBFUNCT,DBGETTOT                                                 
         GOTO1 DEMAND,DMCB,ADBLOCK,SVUNIV                                       
         L     R1,INDXCTR                                                       
         LA    R1,1(R1)                                                         
         ST    R1,INDXCTR                                                       
         B     GETUNV                                                           
         EJECT                                                                  
*COMPUTE US TOTAL                                                               
DOUSTOT  SR    R8,R8                                                            
         L     R9,RANKCTR                                                       
         LA    R6,RANKTAB                                                       
         USING RANKTABD,R6                                                      
DOUSTOT1 ICM   RE,15,RNKUNV                                                     
         LA    R7,SMATABLE                                                      
DOUSTOT2 CLC   RMKT,0(R7)          DONT ADD SMA TO US TOTAL                     
         BE    DOUSTOT3                                                         
         LA    R7,2(R7)                                                         
         CLI   0(R7),X'FF'                                                      
         BNE   DOUSTOT2                                                         
         AR    R8,RE               ADD UNIVERSE TO US TOTAL                     
DOUSTOT3 LA    R6,RANKLEN(R6)                                                   
         BCT   R9,DOUSTOT1                                                      
         ST    R8,USTOTAL                                                       
         SPACE 2                                                                
*COMPUTE PERCENT OF US POP                                                      
         L     R9,RANKCTR                                                       
         LA    R6,RANKTAB                                                       
DOUSPCT  SR    RE,RE                                                            
         ICM   RF,15,RNKUNV                                                     
         M     RE,=F'200000'                                                    
         D     RE,USTOTAL                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,15,RNKPCT                                                     
         LA    R6,RANKLEN(R6)                                                   
         BCT   R9,DOUSPCT                                                       
         SPACE 2                                                                
* PRINT REPORT                                                                  
         L     R9,RANKCTR          SORT MARKETS INTO RANK ORDER                 
         LTR   R9,R9                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 XSORT,DMCB,RANKTAB,(R9),RANKLEN,L'RMKTNAM,              X        
               RMKTNAM-RMKT                                                     
         L     R9,RANKCTR                                                       
         GOTO1 XSORT,DMCB,(C'D',RANKTAB),(R9),RANKLEN,L'RNKUNV,        X        
               RNKUNV-RMKT                                                      
         SPACE 2                                                                
         LA    R1,1                SET FOR SMA RANK                             
         LA    R5,1                SET FOR NON SMA RANK                         
         LA    R6,RANKTAB                                                       
RANKSEED STCM  R1,3,SMARANK        SET UP RANKS IN TABLE                        
         SPACE 2                                                                
         LA    R7,SMATABLE                                                      
RNKSEED1 CLC   RMKT,0(R7)          DON'T RANK SMA MARKETS                       
         BE    RNKSEED3                                                         
         LA    R7,2(R7)                                                         
         CLI   0(R7),X'FF'                                                      
         BNE   RNKSEED1                                                         
RNKSEED2 STCM  R5,3,RANK                                                        
         LA    R5,1(R5)                                                         
RNKSEED3 DS    0C                                                               
         SPACE 2                                                                
         LA    R6,RANKLEN(R6)                                                   
         LA    R1,1(R1)                                                         
         BCT   R9,RANKSEED                                                      
         SPACE 2                                                                
         MVI   RCSUBPRG,1                                                       
         MVI   RPTYPE,C'R'                                                      
REPRINT  L     R9,RANKCTR                                                       
         LA    R6,RANKTAB                                                       
         LA    R7,P1                                                            
         USING PLINE,R7                                                         
PRINTIT  SR    RF,RF                                                            
         ICM   RF,3,RMKT           ADJUST FOR CPP MARKET                        
         CLI   QOPT1,C'N'                                                       
         BNE   *+8                                                              
         AH    RF,=H'400'                                                       
         EDIT  (RF),(4,PLMNUM)                                                  
         CLI   QOPT1,C'N'                                                       
         BNE   PRINT01                                                          
         EDIT  RANK,(3,PLMRANK)                                                 
         OC    RANK,RANK                                                        
         BNZ   *+10                                                             
         MVC   PLMRANK,=C'SMA'                                                  
PRINT01  EDIT  SMARANK,(3,PLSRANK)                                              
         MVC   PLMNAME,RMKTNAM                                                  
         EDIT  RNKPCT,(6,PLUSPCT),3                                             
         ICM   RF,15,RNKUNV                                                     
         CLI   DEMONUM+1,C'U'                                                   
         BNE   *+8                                                              
         MH    RF,=H'1000'                                                      
         EDIT  (RF),(11,PLHMSUNV),,COMMAS=YES                                   
         GOTO1 REPORT                                                           
         LA    R6,RANKLEN(R6)                                                   
         BCT   R9,PRINTIT                                                       
         MVC   PLMNAME(8),=C'US TOTAL'                                          
         ICM   RF,15,USTOTAL                                                    
         CLI   QOPT3,C'U'                                                       
         BNE   *+8                                                              
         MH    RF,=H'1000'                                                      
         EDIT  (RF),(11,PLHMSUNV),,COMMAS=YES                                   
         GOTO1 REPORT                                                           
         MVI   MODE,REQLAST                                                     
         CLI   RPTYPE,C'A'                                                      
         BE    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         L     R9,RANKCTR                                                       
         GOTO1 XSORT,DMCB,RANKTAB,(R9),RANKLEN,L'RMKTNAM,              X        
               RMKTNAM-RMKT                                                     
         MVI   RPTYPE,C'A'                                                      
         MVI   RCSUBPRG,2                                                       
         B     REPRINT                                                          
         EJECT                                                                  
SPILLRPT L     RE,=A(SPILTAB)                                                   
         L     RF,=F'100000'                                                    
         XCEF                                                                   
         L     RE,=A(SPILTAB)                                                   
         ST    RE,LASTRANK                                                      
         MVI   RCSUBPRG,3                                                       
         LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   DRCODE,C'R'         SET TO READ PRIMARY DIRECTORY                
         MVI   DRMEDIA,C'T'                                                     
         MVC   DRSRC,QOPT1         SET RATING SERVICE                           
         MVC   SVSPLKEY,SPLKEY                                                  
         XC    SPILCTR,SPILCTR                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLR6                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
SPLR6    CLC   SPLKEY(3),SVSPLKEY                                               
         BNE   SPLR18                                                           
         OC    DRKMKT,DRKMKT       IS IT SPILL                                  
         BZ    SPLR2               NO - GET NEXT RECORD                         
         L     RE,LASTRANK                                                      
         USING SPILTABD,RE                                                      
         MVC   SMKT,DRKMKT         SET UP SPILL KEY                             
         MVC   SSTA,DRSTAT                                                      
         MVC   SBOOK,DRBOOK                                                     
         LA    RE,SPILLEN(RE)                                                   
         ST    RE,LASTRANK                                                      
         L     R9,SPILCTR          COUNT THE SPILL ENTRIES                      
         LA    R9,1(R9)                                                         
         ST    R9,SPILCTR                                                       
         B     SPLR2                                                            
         DROP  RE                                                               
         DROP  R6                                                               
         SPACE 2                                                                
SPLR18   L     R9,SPILCTR          SORT INTO MARKET ORDER                       
         LTR   R9,R9                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 XSORT,DMCB,A(SPILTAB),(R9),SPILLEN,L'SMS,SMS-SMS                 
         EJECT                                                                  
* PRINT SPILL REPORT                                                            
SPLR20   L     R6,=A(SPILTAB)                                                   
         USING SPILTABD,R6                                                      
         LA    R7,P1                                                            
         USING PSLINE,R7                                                        
         MVC   PVSMS,SMS           FORCE EQUAL ON FIRST TIME                    
SPLR21   LA    R8,PSBOOKS                                                       
         OC    0(SPILLEN,R6),0(R6)                                              
         BZ    EXIT                                                             
         MVC   PSSTA,SSTA                                                       
         L     R9,=A(RANKTAB)                                                   
         USING RANKTABD,R9                                                      
S21MN01  OC    RMKT,RMKT           FIND MARKET NAME                             
         BNZ   *+6                                                              
         DC    H'0'                INVALID MARKET                               
         CLC   SMKT,RMKT                                                        
         BE    *+12                                                             
         LA    R9,RANKLEN(R9)                                                   
         B     S21MN01                                                          
         MVC   PSMKTNAM,RMKTNAM                                                 
         EDIT  (2,SMKT),(4,PSMKTNUM)                                            
SPLR22   MVC   FULL(2),SBOOK                                                    
         XC    FULL(2),=X'FFFF'                                                 
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(6,0(R8))                                   
         LA    R6,SPILLEN(R6)                                                   
         LA    R8,7(R8)                                                         
         CLC   PVSMS,SMS                                                        
         BE    SPLR22                                                           
         GOTO1 REPORT                                                           
         MVC   PVSMS,SMS                                                        
         B     SPLR21                                                           
         DROP  R9                                                               
         DROP  R6                                                               
         DROP  R7                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* SAVE MARKET NUMBER AND NAME FROM MARKET RECORD                                
         DS    0D                                                               
         USING *,RF                                                             
SVMRKT   NTR1  BASE=SPLRRB                                                      
         LM    RA,RC,SPLRRA                                                     
         DROP  RF                                                               
         L     R4,ADBLOCK                                                       
         L     R5,DBAREC                                                        
         USING DMKEY,R5                                                         
         LA    R6,RANKTAB                                                       
         USING RANKTABD,R6                                                      
SVMRKT2  OC    RMKT,RMKT                                                        
         BZ    SVMRKT3                                                          
         CLC   DMRMKT,RMKT                                                      
         BE    EXIT                                                             
         LA    R6,RANKLEN(R6)                                                   
         B     SVMRKT2                                                          
SVMRKT3  CLC   DMRMKT,=H'480'                                                   
         BE    EXIT                                                             
         L     R1,RANKCTR                                                       
         LA    R1,1(R1)                                                         
         ST    R1,RANKCTR                                                       
         MVC   RMKT,DMRMKT                                                      
         LA    R5,DMFRSTEL                                                      
         USING DMELEM,R5                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RMKTNAM(0),DMMNAME                                               
         B     EXIT                                                             
         EJECT                                                                  
* SAVE HOMES UNIVERSE IN RANK TABLE                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SPLRRB                                                      
         LM    RA,RC,SPLRRA                                                     
         DROP  RF                                                               
         MVC   H6+64(7),DNAME1                                                  
         CLI   QOPT1,C'N'                                                       
         BE    MYHEADX                                                          
         MVC   H6+40(4),=C'    '                                                
         MVC   H7+40(4),=C'    '                                                
         MVC   H8+40(4),=C'    '                                                
         MVC   H6+47(4),=C'ADI '                                                
MYHEADX  XIT1                                                                   
         SPACE 2                                                                
         DS    0D                                                               
         USING *,RF                                                             
SVUNIV   NTR1  BASE=SPLRRB                                                      
         LM    RA,RC,SPLRRA                                                     
         DROP  RF                                                               
         L     R4,ADBLOCK                                                       
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         GOTO1 CDEMOUT,DMCB,(C'D',DEMONUM),ADBLOCK,FULL                         
         DROP  R9                                                               
         L     R5,DBAREC                                                        
         USING DRKEY,R5                                                         
         LA    R7,DRFRSTEL                                                      
         USING MARELEM,R7                                                       
         MVC   HALF,MARNO                                                       
         LA    R6,RANKTAB                                                       
         USING RANKTABD,R6                                                      
         L     RE,RANKCTR                                                       
SVUNIV2  CLC   HALF,RMKT                                                        
         BE    SVUNIV3                                                          
         LA    R6,RANKLEN(R6)                                                   
         BCT   RE,SVUNIV2                                                       
         DC    H'0'                                                             
SVUNIV3  MVC   RNKUNV,FULL                                                      
         B     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
SPLRRA DC      F'0'                                                             
SPLRRB DC      F'0'                                                             
SPLRRC DC      F'0'                                                             
         SPACE 2                                                                
DEMONUM  DC    X'00',C'L',AL1(45)                                               
DNAME1   DC    CL7'HOMES'                                                       
SMATABLE DC    AL2(185)            AKRON                                        
         DC    AL2(260)            ANNISTON                                     
         DC    AL2(338)            BOWLING GREEN                                
         DC    AL2(350)            FLAGSTAFF                                    
         DC    AL2(363)            FLAGSTAFF                                    
         DC    AL2(194)            HAGERSTOWN                                   
         DC    AL2(405)            PALM SPRINGS                                 
         DC    AL2(199)            SARASOTA                                     
         DC    AL2(165)            ELMIRA                                       
         DC    AL2(172)            MANCHESTER                                   
         DC    AL2(193)            WORCESTER                                    
         DC    AL2(226)            VICTORIA                                     
         DC    AL2(290)            GREAT BEND                                   
         DC    AL2(333)            FLORENCE,AL                                  
         DC    AL2(341)            HAYS-GOODLAND                                
         DC    AL2(342)            ENSIGN-GARDEN CITY                           
         DC    X'FFFF'                                                          
         EJECT                                                                  
RELO     DC    F'0'                                                             
RANKCTR  DC    F'0'                                                             
SPILCTR  DC    F'0'                                                             
LOOPCTR  DC    F'0'                                                             
USTOTAL  DC    F'0'                                                             
INDXCTR  DC    F'0'                                                             
LASTRANK DC    F'0'                                                             
PVSMS    DS    CL7                                                              
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL24                                                             
HRNKLEN  DC    AL2(RANKLEN)                                                     
RPTYPE   DS    C                                                                
         SPACE 2                                                                
RANKTAB  DS    10000C                                                           
SPILTAB  DS    100000C                                                          
*                                                                               
RANKTABD DSECT                                                                  
RMKT     DS    CL2                                                              
RNKUNV   DS    CL4                                                              
RNKPCT   DS    CL4                                                              
RANK     DS    CL2                                                              
SMARANK  DS    CL2                                                              
RMKTNAM  DS    CL30                                                             
RNKTABEN DS    0C                                                               
RANKLEN  EQU   RNKTABEN-RMKT                                                    
         SPACE 2                                                                
SPILTABD DSECT                                                                  
SMS      DS    0CL7                                                             
SMKT     DS    CL2                                                              
SSTA     DS    CL5                                                              
SBOOK    DS    CL2                                                              
SPILEND  DS    0C                                                               
SPILLEN  EQU   SPILEND-SMKT                                                     
         SPACE 2                                                                
HOMETABD DSECT                                                                  
HSTA     DS    CL5                                                              
HMKT     DS    CL2                                                              
HOMTABEN DS    0C                                                               
HOMELEN  EQU   HOMTABEN-HSTA                                                    
         SPACE 2                                                                
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PLMNUM   DS    CL4                                                              
         DS    CL3                                                              
PLMNAME  DS    CL30                                                             
         DS    CL3                                                              
PLMRANK  DS    CL3                                                              
         DS    CL3                                                              
PLSRANK  DS    CL3                                                              
         DS    CL2                                                              
PLUSPCT  DS    CL6                                                              
         DS    CL1                                                              
PLHMSUNV DS    CL11                                                             
         SPACE 2                                                                
PSLINE   DSECT                                                                  
         DS    CL3                                                              
PSSTA    DS    CL4                                                              
         DS    CL2                                                              
PSMKTNUM DS    CL4                                                              
         DS    CL2                                                              
PSMKTNAM DS    CL30                                                             
         DS    CL4                                                              
PSBOOKS  DS    CL80                                                             
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068SPREPLR02 05/05/03'                                      
         END                                                                    
