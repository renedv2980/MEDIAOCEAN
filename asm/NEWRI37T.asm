*          DATA SET NEWRI37T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEWRI37    AT LEVEL 242 AS OF 10/11/88                      
*PHASE T32037A,+0                                                               
         TITLE 'T32037 - P5  REPORT  PHASE'                                     
T32037   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**P5PR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T32037,RB,RA      RA = 2ND BASE REG                              
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R6,ANETWS2                                                       
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         L     R7,ANETWS4        * ANETWS4 = MYWORK AREA                        
         USING MYD,R7                                                           
         LA    R1,HEADING        * ANETWS2 = NDDEMBLK                           
         ST    R1,SPECS                                                         
         LA    R1,HDRTN          * ANETWS3 = CLIST                              
         ST    R1,HEADHOOK                                                      
         SPACE  1                                                               
* SET UP SORTER *                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         B     INIT2                                                            
         SPACE 1                                                                
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,12,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
         SPACE 1                                                                
*                                                                               
INIT2    CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BAS   RE,REPMOD                                                        
XIT      XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
REPMOD   NTR1                                                                   
         LA    R3,6                ZAP SUBTOTS                                  
         LA    R2,SUBTOTS                                                       
ZAPSUB   ZAP   8(8,R2),=P'0'                                                    
         LA    R2,112(R2)                                                       
         BCT   R3,ZAPSUB                                                        
         LA    R3,6                ZAP SUBTOTS2                                 
         LA    R2,SUBTOTS2                                                      
ZAPSUB2  ZAP   8(8,R2),=P'0'                                                    
         LA    R2,112(R2)                                                       
         BCT   R3,ZAPSUB2                                                       
         XC    FINALTOT,FINALTOT                                                
         XC    FINLEQU,FINLEQU                                                  
         ZAP   FINLCST,=P'0'                                                    
         ZAP   EQUSBCST,=P'0'                                                   
         XC    EQUNT,EQUNT                                                      
         MVI   DEMNUM,0                                                         
         LA    R3,6                ZAP SUBTOTS3                                 
         LA    R2,SUBTOTS3                                                      
ZAPSUB3  ZAP   8(8,R2),=P'0'                                                    
         LA    R2,112(R2)                                                       
         BCT   R3,ZAPSUB3                                                       
         ZAP   PRDEQCST,=P'0'                                                   
         EJECT                                                                  
*                                                                               
         MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBACTOPT,C'Y'       ACTUAL DEMOS                                 
         MVI   NBSPLOPT,X'C0'      SPLIT EVEN IF POOL                           
         MVI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         OI    NBINDS,X'80'        EQUIVALENCE OVERRIDES                        
         NETGO NVDEMOPT,DMCB       ACT SCHED/EST DEMO BUT NOT FOR PFB           
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALCLI                                                  
         BNE   REP1                                                             
         L     R2,NBAIO                                                         
         USING CLTHDR,R2                                                        
         L     RE,CLIST                                                         
         L     RF,ANETWS3                                                       
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         DROP  R2                                                               
REP1     CLI   NBMODE,NBREQLST                                                  
         BE    REP20                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   GETUNIT                                                          
         B     REP2                                                             
         EJECT                                                                  
*                                                                               
REP2     CLI   DEMNUM,0            CENTER REPORT                                
         BNE   REP2A                                                            
         ZIC   R1,NDNDEMOS                                                      
         STC   R1,DEMNUM                                                        
         CLI   DEMNUM,6                                                         
         BNH   *+12                                                             
         MVI   DEMNUM,6                                                         
         LA    R1,6                                                             
         MH    R1,=H'12'                                                        
         A     R1,=F'55'                                                        
         CLI   DEMNUM,0            IF NO DEMOS/DO NOT NEED E/A SPACE            
         BE    *+8                                                              
         LA    R1,3(R1)                                                         
         LA    R2,132                                                           
         SR    R2,R1                                                            
         SRA   R2,1                                                             
         LA    R1,P                                                             
         AR    R1,R2                                                            
         ST    R1,AP1              SET PRINT LINE                               
         LA    R2,P                                                             
         SR    R1,R2                                                            
         LA    R2,H8                                                            
         AR    R2,R1                                                            
         ST    R2,AH8              SET HEADS LINE                               
         EJECT                                                                  
*                                                                               
REP2A    LA    R2,SRTREC           UNIT REC - PREPARE FOR SORTER                
         USING SRTRECD,R2                                                       
         CLI   PRDALL,C'Y'                                                      
         BNE   REP2C                                                            
         CLC   CURPRD1,NBSPLPRN                                                 
         BE    *+8                                                              
         BAS   RE,GETPROD                                                       
         MVC   SRTPRD1,CURPRD                                                   
REP2C    CLI   NETALL,C'Y'                                                      
         BNE   *+10                                                             
         MVC   SRTNET1,NBACTNET                                                 
         MVC   SRTDATE,NBACTDAT                                                 
         MVC   SRTNET,NBACTNET                                                  
         MVC   SRTPROG,NBPROGNM                                                 
         MVC   SRTIME,NBTIME                                                    
         MVC   SRTSLEN,NBLEN                                                    
         MVC   SRTPROD,NBSPLPRN                                                 
         MVC   SRTDOLS,NBACTUAL                                                 
         CLI   ASSFLG,C'Y'                                                      
         BNE   *+10                                                             
         MVC   SRTDOLS,NBASSIGN                                                 
         CLI   DEMNUM,0                                                         
         BE    REP19                                                            
         ZIC   R5,DEMNUM                                                        
         LA    R2,SRTDEMOS                                                      
         LA    R4,NDESTDEM                                                      
         LA    R3,NDACTDEM                                                      
REP5     MVC   0(2,R2),2(R4)       GRP                                          
         MVC   2(4,R2),4(R4)       IMP                                          
         MVC   6(2,R2),2(R3)       GRP                                          
         MVC   8(4,R2),4(R3)       IMP                                          
         LA    R2,12(R2)                                                        
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,REP5                                                          
         EJECT                                                                  
*                                                                               
         LA    R2,SRTREC                                                        
         ZIC   R5,DEMNUM                                                        
         CLI   NBN0B2,0            IF USER IS SET UP TO EQUIVALENCE             
         BE    REP15                                                            
         MVC   SAVIMPE,NBN0B2      SAVE PROFILE BYTES FOR EQUIVALENCY           
         MVC   SAVGRPE,NBN2B1                                                   
         MVI   NBN0B2,0            SET UP TO GET RAW DATA                       
         MVI   NBN2B1,0                                                         
         GOTO1 NBNETVAL,DMCB,NETBLOCK        (REVALUE)                          
         LA    R2,SRTDEMO2                                                      
         LA    R4,NDESTDEM                                                      
         LA    R3,NDACTDEM                                                      
REP10    MVC   0(2,R2),2(R4)       GRP                                          
         MVC   2(4,R2),4(R4)       IMP                                          
         MVC   6(2,R2),2(R3)       GRP                                          
         MVC   8(4,R2),4(R3)       IMP                                          
         LA    R2,12(R2)                                                        
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,REP10                                                         
         MVC   NBN0B2,SAVIMPE      RESTORE EQUIVALENCY                          
         MVC   NBN2B1,SAVGRPE                                                   
         B     REP19                                                            
         SPACE 1                                                                
*                                       USER IS NOT SET UP TO EQUIV             
REP15    DS    0H                                                               
         MVI   NBN0B2,30           SET UP TO GET EQUIVALENCED DATA              
         MVI   NBN2B1,30                                                        
         GOTO1 NBNETVAL,DMCB,NETBLOCK        (REVALUE)                          
         LA    R2,SRTDEMO2                                                      
         LA    R4,NDESTDEM                                                      
         LA    R3,NDACTDEM                                                      
REP18    MVC   0(2,R2),2(R4)       GRP                                          
         MVC   2(4,R2),4(R4)       IMP                                          
         MVC   6(2,R2),2(R3)       GRP                                          
         MVC   8(4,R2),4(R3)       IMP                                          
         LA    R2,12(R2)                                                        
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,REP18                                                         
         MVI   NBN0B2,0                                                         
         MVI   NBN2B1,0                                                         
*                                                                               
REP19    LA    R2,SRTREC                                                        
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         B     GETUNIT                                                          
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
REP20    DS    0H                                                               
         USING SRTRECD,R6                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,4(R1)                                                         
         LTR   R6,R6                                                            
         BZ    REP22                                                            
         CLI   PRDALL,C'Y'         IF PRD=ALL                                   
         BNE   REP20C                                                           
         OC    CURPRD,CURPRD                                                    
         BNZ   *+14                                                             
         MVC   CURPRD,SRTPRD1                                                   
         B     REP20C                                                           
         CLC   CURPRD,SRTPRD1      AND PRD HAS CHANGED                          
         BE    REP20C                                                           
         MVI   ALLOWLIN,7                                                       
         BAS   RE,STOTRTN                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   NETALL,C'Y'                                                      
         BNE   REP20B                                                           
         MVC   CURNET(4),=C'ALL '                                               
         MVI   ALLOWLIN,7                                                       
         BAS   RE,STOTRTN2         NET SPOT LEN SUMMARY                         
         BAS   RE,ST11             PROD TOTALS                                  
         MVC   CURNET,SRTNET                                                    
REP20B   DS    0H                                                               
         MVC   CURPRD,SRTPRD1                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   SUBTFLG,0                                                        
         LA    R1,6                CLEAR SUBTOTS                                
         LA    R2,SUBTOTS                                                       
REP20BB  XC    0(112,R2),0(R2)                                                  
         ZAP   8(8,R2),=P'0'                                                    
         LA    R2,112(R2)                                                       
         BCT   R1,REP20BB                                                       
         B     REP21                                                            
REP20C   CLI   NETALL,C'Y'         IF NET=ALL                                   
         BNE   REP21                                                            
         OC    CURNET,CURNET                                                    
         BNZ   *+14                                                             
         MVC   CURNET,SRTNET                                                    
         B     REP21                                                            
         CLC   CURNET,SRTNET       AND NET HAS CHANGED                          
         BE    REP21                                                            
         MVI   ALLOWLIN,7                                                       
         BAS   RE,STOTRTN          DO SUBTOTS                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   SUBTFLG,0                                                        
         MVC   CURNET,SRTNET       SET NEW NET                                  
         LA    R1,6                CLEAR SUBTOTS                                
         LA    R2,SUBTOTS                                                       
REP20X   XC    0(112,R2),0(R2)                                                  
         ZAP   8(8,R2),=P'0'                                                    
         LA    R2,112(R2)                                                       
         BCT   R1,REP20X                                                        
         XC    SUBFINLT(96),SUBFINLT                                            
         XC    EQUNT,EQUNT                                                      
         EJECT                                                                  
*                                                                               
REP21    BAS   RE,REP30            POST UNIT                                    
         B     REP20                                                            
*                                                                               
REP22    DS    0H                                                               
         LA    R1,SUBTOTS          NO DATA EXIT                                 
         OC    0(8,R1),0(R1)                                                    
         BZ    REP24                                                            
         MVI   ALLOWLIN,7                                                       
         BAS   RE,STOTRTN          EOF-DO SUBTOTS                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   NETALL,C'Y'                                                      
         BNE   REP23D                                                           
         MVC   CURNET,=C'ALL '                                                  
         MVI   ALLOWLIN,7                                                       
         BAS   RE,STOTRTN2         NET TOTS BY SPOT LENTGTH                     
         CLI   PRDALL,C'Y'                                                      
         BNE   REP23F                                                           
         MVC   CURPRD,=C'ALL'                                                   
         BAS   RE,ST11             PROD SUB TOTS                                
REP23D   CLI   PRDALL,C'Y'                                                      
         BNE   REP23F                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   CURPRD,=C'ALL'                                                   
         MVI   ALLOWLIN,7                                                       
         BAS   RE,STOTRTN3         PROD TOTS BY SPOT LENGTH                     
REP23F   BAS   RE,END              DO FINAL AND EQUIV TOTALS                    
REP24    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* POST TO PRINT LINE                                                            
*                                                                               
REP30    NTR1                                                                   
         USING SRTRECD,R6                                                       
         L     R2,AP1                                                           
         USING PLINED,R2                                                        
         CLI   NETALL,C'T'         IF NET=NNN,SHOW NETWORK                      
         BNE   REP31                                                            
         MVC   PNET,SRTNET                                                      
         CLC   CURNET,SRTNET                                                    
         BE    REP31                                                            
         MVC   CURNET,SRTNET                                                    
REP31    GOTO1 DATCON,DMCB,(2,SRTDATE),(4,PDATE)                                
         MVC   PPROG,SRTPROG                                                    
         CLI   POLFLG,C'Y'                                                      
         BNE   REP31C                                                           
         L     R1,ANETWS3          GET PRD NAME                                 
REP31A   CLC   3(1,R1),SRTPROD                                                  
         BE    REP31B                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   REP31A                                                           
         MVC   PSPTLEN(3),=C'UNA'                                               
         B     *+10                                                             
REP31B   MVC   PSPTLEN(3),0(R1)                                                 
REP31C   GOTO1 UNTIME,DMCB,SRTIME,PTIME                                         
         LA    R5,PSPTLEN                                                       
         CLI   POLFLG,C'Y'                                                      
         BNE   *+8                                                              
         LA    R5,PSPTLEN+132                                                   
         EDIT  (B1,SRTSLEN),(3,0(R5)),ALIGN=LEFT                                
         MVC   FULL,SRTDOLS                                                     
         L     R1,FULL                                                          
         LTR   R1,R1                                                            
         BNZ   REP33                                                            
         EDIT  (R1),(10,PDOLLARS),2                                             
         B     REP34                                                            
REP33    M     R0,=F'1'                                                         
         D     R0,=F'50'                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(10,PDOLLARS)                                               
REP34    MVI   PDOLLARS+11,C'E'                                                 
         MVI   PDOLLARS+143,C'A'                                                
         CLI   DEMNUM,0                                                         
         BE    REP40                                                            
         ZIC   R5,DEMNUM                                                        
         LA    R2,PDEMOS                                                        
         LA    R3,SRTDEMOS                                                      
PSLOOP   EDIT  (B2,0(R3)),(4,0(R2)),1                                           
         NETGO NVPRDEM,DMCB,(C'I',0),2(R3),4(R2)                                
         EDIT  (B2,6(R3)),(4,132(R2)),1                                         
         NETGO NVPRDEM,DMCB,(C'I',0),8(R3),136(R2)                              
         LA    R2,12(R2)                                                        
         LA    R3,12(R3)                                                        
         BCT   R5,PSLOOP                                                        
REP40    MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         BAS   RE,ROLLTOT1         ROLL TO SUBTOTS                              
         BAS   RE,ROLLEQU          ROLL TO EQUIVALENCED                         
         B     XIT                                                              
         EJECT                                                                  
*   SUBTOTS KEPT BY SPTLEN                                                      
*   SPTLEN                                                                      
*                                                                               
ROLLTOT1 NTR1                                                                   
         LA    R2,SUBTOTS          ADD TO SUBTOTALS                             
         LA    R5,6                                                             
         LA    R4,SRTDEMOS                                                      
         BAS   RE,ROLLRTN                                                       
         CLI   NETALL,C'Y'                                                      
         BNE   RLT3                                                             
         LA    R2,SUBTOTS2         ADD TO SUBTOTS2 (USED IF NET=ALL)            
         LA    R5,6                                                             
         LA    R4,SRTDEMOS                                                      
         BAS   RE,ROLLRTN                                                       
RLT3     CLI   PRDALL,C'Y'                                                      
         BNE   RLTX                                                             
         LA    R2,SUBTOTS3         ADD TO SUBTOTS3 (USED IF PRD=ALL)            
         LA    R5,6                                                             
         LA    R4,SRTDEMOS                                                      
         BAS   RE,ROLLRTN                                                       
RLTX     B     XIT                                                              
*                                                                               
ROLLRTN  NTR1                                                                   
RL3      OC    0(4,R2),0(R2)                                                    
         BZ    RL5                                                              
         CLC   3(1,R2),SRTSLEN                                                  
         BE    RL5                                                              
         LA    R2,112(R2)                                                       
         BCT   R5,RL3                                                           
         DC    H'0'                                                             
RL5      MVC   3(1,R2),SRTSLEN                                                  
         L     R1,4(R2)            ADD TO UNIT NUMBER                           
         LA    R1,1(R1)                                                         
         ST    R1,4(R2)                                                         
         MVC   FULL,SRTDOLS                                                     
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    8(8,R2),DUB                                                      
         LA    R2,16(R2)                                                        
         LA    R5,6                                                             
RL7      LH    R1,0(R4)            EST GRP                                      
         A     R1,0(R2)                                                         
         ST    R1,0(R2)                                                         
         LH    R1,6(R4)            ACT GRP                                      
         A     R1,4(R2)                                                         
         ST    R1,4(R2)                                                         
         L     R1,2(R4)            EST IMP                                      
         A     R1,8(R2)                                                         
         ST    R1,8(R2)                                                         
         L     R1,8(R4)            ACT IMP                                      
         A     R1,12(R2)                                                        
         ST    R1,12(R2)                                                        
         LA    R4,12(R4)            BUMP SRT DEMS                               
         LA    R2,16(R2)            BUMP SUBTOT AREA                            
         BCT   R5,RL7              R5 = MAX DEMOS(6)                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROLL TO EQUIVALECED                                                           
*                                                                               
ROLLEQU  NTR1                                                                   
         LA    R2,FINLEQU                                                       
         LA    R4,SRTDEMO2                                                      
         LA    R5,6                                                             
         BAS   RE,EQUROLL                                                       
         CLI   NETALL,C'Y'                                                      
         BNE   RLQ5                                                             
         LA    R5,6                                                             
         LA    R2,EQUSUBT                                                       
         LA    R4,SRTDEMO2                                                      
         BAS   RE,EQUROLL                                                       
         MVC   FULL,SRTDOLS                                                     
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    EQUSBCST,DUB                                                     
RLQ5     CLI   PRDALL,C'Y'                                                      
         BNE   XIT                                                              
         LA    R5,6                                                             
         LA    R2,PRDEQSBT                                                      
         LA    R4,SRTDEMO2                                                      
         BAS   RE,EQUROLL                                                       
         MVC   FULL,SRTDOLS                                                     
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    PRDEQCST,DUB                                                     
         B     XIT                                                              
*                                                                               
EQUROLL  NTR1                                                                   
RQ5      LH    R1,0(R4)            EST GRP                                      
         A     R1,0(R2)                                                         
         ST    R1,0(R2)                                                         
         LH    R1,6(R4)            ACT GRP                                      
         A     R1,4(R2)                                                         
         ST    R1,4(R2)                                                         
         L     R1,2(R4)            EST IMP                                      
         A     R1,8(R2)                                                         
         ST    R1,8(R2)                                                         
         L     R1,8(R4)            ACT IMP                                      
         A     R1,12(R2)                                                        
         ST    R1,12(R2)                                                        
         LA    R4,12(R4)            BUMP SRT DEMS                               
         LA    R2,16(R2)            BUMP SUBTOT AREA                            
         BCT   R5,RQ5              R5 = MAX DEMOS(6)                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SUBTOTAL ROUTINE                                                              
*                                                                               
STOTRTN  NTR1                                                                   
*                                                                               
         LA    R2,P                                                             
         ST    R2,ATP                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   SUBTFLG,C'Y'                                                     
         CLI   DEMNUM,0                                                         
         BE    ST2                                                              
         CLI   DEMNUM,6                                                         
         BE    ST2                                                              
         ZIC   R1,DEMNUM                                                        
         SR    R2,R2                                                            
         LA    R2,21(R2)                                                        
         BCT   R1,*-4                                                           
         AH    R2,=H'4'                                                         
         LA    R3,132                                                           
         SR    R3,R2                                                            
         LTR   R3,R3                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         SRA   R3,1                                                             
         LA    R2,P                                                             
         AR    R2,R3                                                            
         ST    R2,ATP                                                           
ST2      DS    0H                                                               
         OC    SUBTOTS(8),SUBTOTS     EXIT IF NO DATA                           
         BZ    XIT                                                              
         LA    R3,SUBTOTS                                                       
ST5      L     R2,ATP                                                           
         MVC   1(6,R2),=C'TOTAL:'                                               
         EDIT  (B4,0(R3)),(2,7(R2)),ALIGN=LEFT                                  
         MVC   133(4,R2),=C'COST'                                               
         MVC   TEMPWRK(8),8(R3)    GET ACTUAL COST(PL8)ED                       
         EDIT  (P8,TEMPWRK),(10,139(R2)),2,ALIGN=LEFT                           
         MVC   265(5,R2),=C'UNITS'                                              
         EDIT  (B4,4(R3)),(6,271(R2)),ALIGN=LEFT                                
         L     R1,4(R3)                                                         
         A     R1,EQUNT                                                         
         ST    R1,EQUNT                                                         
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         ST    R3,ASUBTOTS                                                      
         BAS   RE,POSTUNIT                                                      
         LA    R3,112(R3)                                                       
         OC    0(4,R3),0(R3)                                                    
         BZ    ST7                                                              
         MVI   SPACING,3           SKIP LINES BEFORE NEXT TOTALS                
         BAS   RE,PRINTIT                                                       
         B     ST5                                                              
         EJECT                                                                  
ST7      DS    0H                                                               
         BAS   RE,ROLLTOT2         ROLL SUBS TO FINAL TOTS                      
*                                                                               
         CLI   NETALL,C'Y'         IF NET NOT = ALL,SKIP SUB FINAL              
         BE    ST7A                                                             
         CLI   PRDALL,C'Y'                                                      
         BNE   ST7C                                                             
         BE    ST10                                                             
*                                                                               
ST7A     MVI   SPACING,2           SKIP LINES BEFORE NEXT TOTALS                
         BAS   RE,PRINTIT                                                       
         L     R2,ATP                                                           
         MVC   1(9,R2),=C'ALL TOTAL'                                            
         MVC   133(4,R2),=C'COST'                                               
         EDIT  (P8,EQUSBCST),(10,139(R2)),2,ALIGN=LEFT                          
         MVC   265(5,R2),=C'UNITS'                                              
         EDIT  (B4,EQUNT),(6,271(R2)),ALIGN=LEFT                                
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         MVI   SBFNLFLG,C'Y'                                                    
         LA    R1,SUBFINLT                                                      
         ST    R1,ASUBTOTS                                                      
         MVC   DOLSV,EQUSBCST                                                   
         BAS   RE,POSTUNIT                                                      
         MVI   SBFNLFLG,0                                                       
         MVI   EQUSBFLG,C'Y'                                                    
         CLI   NETALL,C'Y'        IF NETALL=N                                   
         BNE   *+8                THEN SKIP EQU HERE(CATCH IT AT FINAL)         
         BAS   RE,EQURTN                                                        
ST7C     MVI   EQUSBFLG,0                                                       
         LA    R1,24               CLEAR EQUSUBT                                
         LA    R2,EQUSUBT                                                       
ST8      XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
         BCT   R1,ST8                                                           
         ZAP   EQUSBCST,=P'0'                                                   
         XC    EQUNT,EQUNT                                                      
         XC    SUBFINLT(96),SUBFINLT                                            
         B     XIT                                                              
         EJECT                                                                  
* ONLY IF PRD = ALL                                                             
*                                                                               
ST10     DS    0H                                                               
         B     ST12                                                             
ST11     NTR1                                                                   
*                                                                               
ST12     OC    EQUNT,EQUNT                                                      
         BZ    XIT                                                              
         MVI   SPACING,3           SKIP LINES BEFORE NEXT TOTALS                
         BAS   RE,PRINTIT                                                       
         L     R2,ATP                                                           
         MVC   1(9,R2),=C'ALL TOTAL'                                            
         MVC   133(4,R2),=C'COST'                                               
         EDIT  (P8,PRDEQCST),(10,139(R2)),2,ALIGN=LEFT                          
         MVC   265(5,R2),=C'UNITS'                                              
         EDIT  (B4,EQUNT),(6,271(R2)),ALIGN=LEFT                                
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         MVI   SBFNLFLG,C'Y'                                                    
         LA    R1,PRDSUBFT                                                      
         ST    R1,ASUBTOTS                                                      
         MVC   DOLSV,PRDEQCST                                                   
         BAS   RE,POSTUNIT                                                      
         MVI   SBFNLFLG,0                                                       
         MVI   EQUPRFLG,C'Y'                                                    
         BAS   RE,EQURTN                                                        
         MVI   EQUPRFLG,0                                                       
ST16     LA    R1,24                                                            
         LA    R2,PRDEQSBT                                                      
ST17     XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
         BCT   R1,ST17                                                          
         ZAP   PRDEQCST,=P'0'                                                   
         XC    PRDSUBFT(96),PRDSUBFT                                            
         XC    EQUNT,EQUNT                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* FINAL TOTS BY SPOT LENGTH                                                     
* ONLY IF NET=ALL                                                               
*                                                                               
STOTRTN2 NTR1                                                                   
         LA    R3,SUBTOTS2                                                      
         OC    0(8,R3),0(R3)                                                    
         BZ    XIT                                                              
ST52     L     R2,ATP                                                           
         MVC   1(6,R2),=C'TOTAL:'                                               
         EDIT  (B4,0(R3)),(2,7(R2)),ALIGN=LEFT                                  
         MVC   133(4,R2),=C'COST'                                               
         MVC   TEMPWRK(8),8(R3)    GET ACTUAL COST(PL8)ED                       
         EDIT  (P8,TEMPWRK),(10,139(R2)),2,ALIGN=LEFT                           
         MVC   265(5,R2),=C'UNITS'                                              
         EDIT  (B4,4(R3)),(6,271(R2)),ALIGN=LEFT                                
         L     R1,4(R3)                                                         
         A     R1,EQUNT                                                         
         ST    R1,EQUNT                                                         
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         ST    R3,ASUBTOTS                                                      
         BAS   RE,POSTUNIT                                                      
         LA    R3,112(R3)                                                       
         OC    0(4,R3),0(R3)                                                    
         BZ    ST72                                                             
         MVI   SPACING,3           SKIP LINES BEFORE NEXT TOTALS                
         BAS   RE,PRINTIT                                                       
         B     ST52                                                             
ST72     DS    0H                                                               
         LA    R2,SUBTOTS2                                                      
         LA    R1,6                                                             
CLRS2    XC    0(112,R2),0(R2)                                                  
         ZAP   8(8,R2),=P'0'                                                    
         LA    R2,112(R2)                                                       
         BCT   R1,CLRS2                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* FINAL TOTS BY SPOT LENGTH                                                     
* ONLY IF PRD=ALL                                                               
*                                                                               
STOTRTN3 NTR1                                                                   
         LA    R3,SUBTOTS3                                                      
         LA    R5,6                                                             
ST03     L     R2,ATP                                                           
         MVC   1(6,R2),=C'TOTAL:'                                               
         EDIT  (B4,0(R3)),(2,7(R2)),ALIGN=LEFT                                  
         MVC   133(4,R2),=C'COST'                                               
         MVC   TEMPWRK(8),8(R3)    GET ACTUAL COST(PL8)ED                       
         EDIT  (P8,TEMPWRK),(10,139(R2)),2,ALIGN=LEFT                           
         MVC   265(5,R2),=C'UNITS'                                              
         EDIT  (B4,4(R3)),(6,271(R2)),ALIGN=LEFT                                
         L     R1,4(R3)                                                         
         A     R1,EQUNT                                                         
         ST    R1,EQUNT                                                         
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         ST    R3,ASUBTOTS                                                      
         BAS   RE,POSTUNIT                                                      
         LA    R3,112(R3)                                                       
         OC    0(4,R3),0(R3)                                                    
         BZ    ST07                                                             
         MVI   SPACING,3           SKIP LINES BEFORE NEXT TOTALS                
         BAS   RE,PRINTIT                                                       
         BCT   R5,ST03             R5=BCT LIMIT=6 SPOT LENGTHS                  
ST07     B     XIT                                                              
         EJECT                                                                  
*                                                                               
POSTUNIT NTR1                                                                   
         L     R2,ATP                                                           
         USING PTOTLIN,R2                                                       
         MVI   1(R2),C'E'                                                       
         LA    R2,PTDEMO                                                        
         L     R3,ASUBTOTS                                                      
         LA    R4,CPMSV                                                         
         ZIC   R5,DEMNUM                                                        
         CLI   SBFNLFLG,C'Y'                                                    
         BE    ST9                                                              
         LA    R3,8(R3)            POINT R3 TO PL8 DOLLARS                      
         MVC   DOLSV,0(R3)         AND SAVE COST OF THIS SPOT LEN               
         LA    R3,8(R3)            POINT R3 TO EST/ACT DEMOS                    
ST9      EDIT  (B4,0(R3)),(6,0(R2)),1                   ESTIMATED GRP           
         NETGO NVPRDEM,DMCB,(C'I',0),8(R3),(0,7(R2))    ESTIMATED IMP           
         ZAP   TEMPWRK(16),DOLSV                                                
         MP    TEMPWRK(16),=P'100' HUNOPT=Y                                     
         L     R1,8(R3)            IF NO ESTIMATED IMP                          
         LTR   R1,R1               SKIP CPM                                     
         BNZ   ST9A                                                             
         ZAP   0(6,R4),=P'0'                                                    
         B     ST9B                                                             
ST9A     CVD   R1,DUB                                                           
         DP    TEMPWRK(16),DUB                                                  
         AP    TEMPWRK(8),=P'5'                                                 
         DP    TEMPWRK(8),=P'10'                                                
         EDIT  (P6,TEMPWRK),(5,15(R2)),2                                        
         MVC   0(6,R4),TEMPWRK                                                  
ST9B     LA    R3,16(R3)           BUMP TO NEXT DEMO                            
         LA    R2,21(R2)           BUMP P LINE                                  
         LA    R4,12(R4)                                                        
         BCT   R5,ST9                                                           
         BAS   RE,PRINTIT                                                       
*                                  NOW DO ACTUAL GRP/IMP                        
         L     R2,ATP                                                           
         USING PTOTLIN,R2                                                       
         MVI   1(R2),C'A'                                                       
         LA    R2,PTDEMO                                                        
         ZIC   R5,DEMNUM                                                        
         LA    R4,CPMSV                                                         
         L     R3,ASUBTOTS                                                      
         CLI   SBFNLFLG,C'Y'                                                    
         BE    *+8                                                              
         LA    R3,16(R3)                                                        
ST9C     EDIT  (B4,4(R3)),(6,0(R2)),1                   ACTUAL GRP              
         NETGO NVPRDEM,DMCB,(C'I',0),12(R3),(0,7(R2))   ACTUAL IMP              
         ZAP   TEMPWRK(16),DOLSV                                                
         MP    TEMPWRK(16),=P'100'                                              
         L     R1,12(R3)                                                        
         LTR   R1,R1               IF NO ACTUAL IMP                             
         BNZ   ST12A               SKIP CPM                                     
         ZAP   6(6,R4),=P'0'                                                    
         B     ST13                                                             
ST12A    CVD   R1,DUB                                                           
         DP    TEMPWRK(16),DUB                                                  
         AP    TEMPWRK(8),=P'5'                                                 
         DP    TEMPWRK(8),=P'10'                                                
         EDIT  (P6,TEMPWRK),(5,15(R2)),2                                        
         MVC   6(6,R4),TEMPWRK                                                  
ST13     LA    R3,16(R3)           BUMP TO NEXT DEMO                            
         LA    R2,21(R2)           BUMP P LINE                                  
         LA    R4,12(R4)                                                        
         BCT   R5,ST9C                                                          
         BAS   RE,PRINTIT                                                       
*                                  NOW  DIFFERENCE EST-ACT                      
         L     R2,ATP                                                           
         USING PTOTLIN,R2                                                       
         MVC   1(2,R2),=C'+-'                                                   
         LA    R2,PTDEMO                                                        
         L     R3,ASUBTOTS                                                      
         CLI   SBFNLFLG,C'Y'                                                    
         BE    *+8                                                              
         LA    R3,16(R3)                                                        
         LA    R4,CPMSV                                                         
         ZIC   R5,DEMNUM                                                        
ST14     L     R6,0(R3)                                                         
         L     R1,4(R3)                                                         
         SR    R1,R6                                                            
         LR    R6,R1                                                            
         EDIT  (R6),(6,0(R2)),1,FLOAT=-                   GRP DIFF              
         L     R1,8(R3)            ROUND E/A IMP BEFORE SUBTRACITION            
         SR    R0,R0                                                            
         AH    R1,=H'5'                                                         
         D     R0,=F'10'                                                        
         L     RF,12(R3)                                                        
         SR    RE,RE                                                            
         AH    RF,=H'5'                                                         
         D     RE,=F'10'                                                        
         SR    RF,R1                                                            
         LR    R6,RF                                                            
         EDIT  (R6),(7,7(R2)),FLOAT=-                   IMP DIFF                
         SP    6(6,R4),0(6,R4)                                                  
         EDIT  (P6,6(R4)),(5,15(R2)),2,FLOAT=-          CPM DIFF                
         LA    R3,16(R3)           BUMP TO NEXT DEMO                            
         LA    R2,21(R2)           BUMP P LINE                                  
         LA    R4,12(R4)                                                        
         BCT   R5,ST14                                                          
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         EJECT                                                                  
* ROLL OVER SUBTOTS TO FINALTOT                                                 
*                                                                               
ROLLTOT2 NTR1                                                                   
         LA    R2,SUBTOTS                                                       
         LA    R6,SUBTOTS                                                       
         LA    R3,FINALTOT                                                      
         LA    R1,24                                                            
RT3      AP    FINLCST,8(8,R6)    ADD UP COST                                   
         L     R4,4(R6)                                                         
         A     R4,UNITOT                                                        
         ST    R4,UNITOT                                                        
RT4      L     R4,16(R6)                                                        
         L     R5,0(R3)                                                         
         AR    R5,R4                                                            
         ST    R5,0(R3)                                                         
         LA    R6,4(R6)                                                         
         LA    R3,4(R3)                                                         
         BCT   R1,RT4                                                           
         LA    R2,112(R2)          BUMP TO NEXT SPOT LEN                        
         OC    0(4,R2),0(R2)                                                    
         BZ    RT7                                                              
         LR    R6,R2                                                            
         LA    R3,FINALTOT                                                      
         LA    R1,24                                                            
         B     RT3                                                              
RT7      CLI   NETALL,C'Y'        IF NETALL=Y ROLL FOR SUB FINALS               
         BNE   RT10                                                             
         LA    R2,SUBTOTS                                                       
         LA    R6,SUBTOTS                                                       
         LA    R3,SUBFINLT                                                      
         LA    R1,24                                                            
RT9      L     R4,16(R6)                                                        
         L     R5,0(R3)                                                         
         AR    R5,R4                                                            
         ST    R5,0(R3)                                                         
         LA    R6,4(R6)                                                         
         LA    R3,4(R3)                                                         
         BCT   R1,RT9                                                           
         LA    R2,112(R2)          BUMP TO NEXT SPOT LEN                        
         OC    0(4,R2),0(R2)                                                    
         BZ    RT10                                                             
         LR    R6,R2                                                            
         LA    R3,SUBFINLT                                                      
         LA    R1,24                                                            
         B     RT9                                                              
RT10     CLI   PRDALL,C'Y'                                                      
         BNE   RTX                                                              
         LA    R2,SUBTOTS                                                       
         LA    R6,SUBTOTS                                                       
         LA    R3,PRDSUBFT                                                      
         LA    R1,24                                                            
RT12     L     R4,16(R6)                                                        
         L     R5,0(R3)                                                         
         AR    R5,R4                                                            
         ST    R5,0(R3)                                                         
         LA    R6,4(R6)                                                         
         LA    R3,4(R3)                                                         
         BCT   R1,RT12                                                          
         LA    R2,112(R2)          BUMP TO NEXT SPOT LEN                        
         OC    0(4,R2),0(R2)                                                    
         BZ    RTX                                                              
         LR    R6,R2                                                            
         LA    R3,PRDSUBFT                                                      
         LA    R1,24                                                            
         B     RT12                                                             
RTX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* FINAL ROUTINES - DO FINAL TOTS, DO EQUIVALENCED TOTS                          
*                                                                               
END      NTR1                                                                   
         MVI   SUBTFLG,C'Y'                                                     
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         L     R2,ATP                                                           
         USING PTOTLIN,R2                                                       
         MVC   1(12,R2),=C'FINAL TOTALS'                                        
         MVC   133(4,R2),=C'COST'                                               
         EDIT  (P8,FINLCST),(15,140(R2)),2,ALIGN=LEFT                           
         MVC   265(5,R2),=C'UNITS'                                              
         EDIT  (B4,UNITOT),(6,272(R2)),ALIGN=LEFT                               
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         L     R2,ATP                                                           
         MVI   1(R2),C'E'                                                       
         LA    R2,PTDEMO                                                        
         LA    R3,FINALTOT                                                      
         LA    R4,CPMSV                                                         
         ZIC   R5,DEMNUM                                                        
         MVC   DOLSV,FINLCST                                                    
         BAS   RE,FINLPOST                                                      
         BAS   RE,EQURTN              NOW DO EQUIVALENCED                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
EQURTN   NTR1                    HANDELS POSTING OF EQUIVALENCED                
         MVI   EQUFLG,C'Y'                                                      
         MVI   SUBTFLG,C'Y'                                                     
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         LA    R3,FINLEQU                                                       
         MVC   DOLSV,FINLCST                                                    
         CLI   EQUSBFLG,C'Y'       IF SUBTOTS                                   
         BNE   *+14                                                             
         LA    R3,EQUSUBT          THEN POINT TO SUBTOT DATA                    
         MVC   DOLSV,EQUSBCST                                                   
         CLI   EQUPRFLG,C'Y'       IF PRD=ALL SUBTOTS                           
         BNE   *+14                                                             
         LA    R3,PRDEQSBT         THEN POINT TO SUBTOT DATA                    
         MVC   DOLSV,PRDEQCST                                                   
         L     R2,ATP                                                           
         USING PTOTLIN,R2                                                       
         MVC   1(12,R2),=C'EQUIVALENCED'                                        
         MVC   133(5,R2),=C'COST '                                              
         EDIT  (P8,DOLSV),(15,139(R2)),2,ALIGN=LEFT                             
         MVC   265(5,R2),=C'UNITS'                                              
         MVC   FULL,UNITOT                                                      
         CLI   EQUSBFLG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   FULL,EQUNT                                                       
         CLI   EQUPRFLG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   FULL,EQUNT                                                       
         EDIT  (B4,FULL),(6,271(R2)),ALIGN=LEFT                                 
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         L     R2,ATP                                                           
         MVI   1(R2),C'E'                                                       
         LA    R2,PTDEMO                                                        
         LA    R4,CPMSV                                                         
         ZIC   R5,DEMNUM                                                        
         BAS   RE,FINLPOST                                                      
         MVI   EQUFLG,0                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
FINLPOST NTR1                                                                   
*                                                                               
END9     EDIT  (B4,0(R3)),(6,0(R2)),1                   ESTIMATED GRP           
         NETGO NVPRDEM,DMCB,(C'I',0),8(R3),(0,7(R2))    ESTIMATED IMP           
         ZAP   TEMPWRK(16),DOLSV                                                
         MP    TEMPWRK(16),=P'100' HUNOPT=Y                                     
         L     R1,8(R3)            IF NO ESTIMATED IMP                          
         LTR   R1,R1               SKIP CPM                                     
         BNZ   END9A                                                            
         ZAP   0(6,R4),=P'0'                                                    
         B     END10                                                            
END9A    CVD   R1,DUB                                                           
         DP    TEMPWRK(16),DUB                                                  
         AP    TEMPWRK(8),=P'5'                                                 
         DP    TEMPWRK(8),=P'10'                                                
         EDIT  (P6,TEMPWRK),(5,15(R2)),2                                        
         MVC   0(6,R4),TEMPWRK                                                  
END10    LA    R3,16(R3)           BUMP TO NEXT DEMO                            
         LA    R2,21(R2)           BUMP P LINE                                  
         LA    R4,12(R4)           BUMP CPMSV                                   
         BCT   R5,END9                                                          
         BAS   RE,PRINTIT                                                       
*                                  NOW DO ACTUAL GRP/IMP                        
         L     R2,ATP                                                           
         USING PTOTLIN,R2                                                       
         MVI   1(R2),C'A'                                                       
         LA    R2,PTDEMO                                                        
         LA    R3,FINALTOT                                                      
         CLI   EQUFLG,C'Y'         IS IT EQUIVALENCED                           
         BNE   END11                                                            
         LA    R3,FINLEQU          AND FINAL                                    
         CLI   EQUPRFLG,C'Y'                                                    
         BNE   *+12                                                             
         LA    R3,PRDEQSBT             OR PROD                                  
         B     END11                                                            
         CLI   EQUSBFLG,C'Y'           OR SUB                                   
         BNE   END11                                                            
         LA    R3,EQUSUBT                                                       
END11    LA    R4,CPMSV                                                         
         ZIC   R5,DEMNUM                                                        
END12    EDIT  (B4,4(R3)),(6,0(R2)),1                   ACTUAL GRP              
         NETGO NVPRDEM,DMCB,(C'I',0),12(R3),(0,7(R2))   ACTUAL IMP              
         ZAP   TEMPWRK(16),DOLSV                                                
         MP    TEMPWRK(16),=P'100'                                              
         L     R1,12(R3)                                                        
         LTR   R1,R1               IF NO ACTUAL IMP                             
         BNZ   END12A              SKIP CPM                                     
         ZAP   6(6,R4),=P'0'                                                    
         B     END13                                                            
END12A   CVD   R1,DUB                                                           
         DP    TEMPWRK(16),DUB                                                  
         AP    TEMPWRK(8),=P'5'                                                 
         DP    TEMPWRK(8),=P'10'                                                
         EDIT  (P6,TEMPWRK),(5,15(R2)),2                                        
         MVC   6(6,R4),TEMPWRK                                                  
END13    LA    R3,16(R3)           BUMP TO NEXT DEMO                            
         LA    R2,21(R2)           BUMP P LINE                                  
         LA    R4,12(R4)                                                        
         BCT   R5,END12                                                         
         BAS   RE,PRINTIT                                                       
*                                  NOW  DIFFERENCE EST-ACT                      
         L     R2,ATP                                                           
         USING PTOTLIN,R2                                                       
         MVC   1(2,R2),=C'+-'                                                   
         LA    R2,PTDEMO                                                        
         LA    R3,FINALTOT                                                      
         CLI   EQUFLG,C'Y'                                                      
         BNE   END13A                                                           
         LA    R3,FINLEQU                                                       
         CLI   EQUSBFLG,C'Y'                                                    
         BNE   END13A                                                           
         LA    R3,EQUSUBT                                                       
END13A   LA    R4,CPMSV                                                         
         ZIC   R5,DEMNUM                                                        
END14    L     R6,0(R3)                                                         
         L     R1,4(R3)                                                         
         SR    R1,R6                                                            
         LR    R6,R1                                                            
         EDIT  (R6),(6,0(R2)),1,FLOAT=-                   GRP DIFF              
         L     R1,8(R3)            ROUND E/A IMP BEFORE SUBTRACITION            
         SR    R0,R0                                                            
         AH    R1,=H'5'                                                         
         D     R0,=F'10'                                                        
         L     RF,12(R3)                                                        
         SR    RE,RE                                                            
         AH    RF,=H'5'                                                         
         D     RE,=F'10'                                                        
         SR    RF,R1                                                            
         LR    R6,RF                                                            
         EDIT  (R6),(7,7(R2)),FLOAT=-                   IMP DIFF                
         SP    0(6,R4),6(6,R4)                                                  
         EDIT  (P6,0(R4)),(5,15(R2)),2,FLOAT=-          CPM DIFF                
         LA    R3,16(R3)           BUMP TO NEXT DEMO                            
         LA    R2,21(R2)           BUMP P LINE                                  
         LA    R4,12(R4)                                                        
         BCT   R5,END14                                                         
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R6,ANETWS2                                                       
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         LA    R2,H3+44                                                         
         GOTO1 CENTER,DMCB,(R2),40                                              
         CLI   CLTITLE,1           OWN TITLE PROVIDED                           
         BE    HDR2                                                             
         MVC   H1+50(22),=C'DETAIL POST EVALUATION'                             
         MVC   H2+50(22),=C'----------------------'                             
         B     HDR2A                                                            
HDR2     DS    0H                                                               
         NETGO NVTITOUT,DMCB                                                    
HDR2A    MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+17(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),CURPRD                                                  
         CLI   PRDALL,C'Y'                                                      
         BE    HDR2C                                                            
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+17(20),SPLPRON                                                
HDR2C    MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(8),SPLEST                                                  
         CLI   SPLNET,X'40'                                                     
         BNH   HD3                                                              
         MVC   H5+97(7),=C'NETWORK'                                             
         MVC   H5+106(4),SPLNET                                                 
         CLI   NETALL,C'Y'                                                      
         BNE   HD3                                                              
         MVC   H5+106(4),CURNET                                                 
HD3      CLI   SPLDPT,X'40'                                                     
         BNH   *+16                                                             
         MVC   H6+97(7),=C'DAYPART'                                             
         MVC   H6+106(8),SPLDPT                                                 
         CLI   SPLPAK,X'40'                                                     
         BNH   HEADS                                                            
         MVC   H4+50(8),=C'PACKAGE='                                            
         MVC   H4+58(8),SPLPAK                                                  
         DROP  R5                                                               
HEADS    CLI   SUBTFLG,C'Y'        SUBTOT ROUTINE HAS ITS OWN HEADS             
         BE    SUBHEADS                                                         
         L     R2,AH8              PRINT OUT HEADS/DEMO NAME                    
         LA    R2,132(R2)                                                       
         USING PLINED,R2                                                        
         CLI   NETALL,C'T'                                                      
         BNE   *+10                                                             
         MVC   PNET,=C'NTWK'                                                    
         MVC   PDATE+1(4),=C'DATE'                                              
         MVC   PPROG(12),=C'PROGRAM NAME'                                       
         MVC   PTIME(4),=C'TIME'                                                
         MVC   PSPTLEN(3),=C'LEN'                                               
         MVC   PDOLLARS+5(4),=C'COST'                                           
         L     R2,AH8                                                           
         MVC   PDOLLARS+4(6),=C'ACTUAL'                                         
         CLI   ASSFLG,C'Y'                                                      
         BNE   *+16                                                             
         MVC   PDOLLARS+2(8),=C'ASSIGNED'                                       
         MVC   PDOLLARS+136(5),=C'COST '                                        
         CLI   POLFLG,C'Y'                                                      
         BNE   *+10                                                             
         MVC   PSPTLEN(4),=C'PRD/'                                              
         ZIC   R1,DEMNUM                                                        
         LTR   R1,R1                                                            
         BZ    BOXES                                                            
         L     R2,AH8                                                           
         LA    R2,132(R2)                                                       
         LA    R2,PDEMOS                                                        
         MVC   1(3,R2),=C'RTG'                                                  
         MVC   7(4,R2),=C'IMPS'                                                 
         LA    R2,12(R2)                                                        
         BCT   R1,*-16                                                          
         L     R2,AH8                                                           
         LA    R4,DBLOCK                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBCOMFCS,ACOMFACS   *SET FOR DEMOCON                             
         MVC   DBFILE,=C'NTI'      *                                            
         MVI   DBSELMED,C'N'       *                                            
         ZIC   R3,DEMNUM                                                        
         LA    R2,PDEMOS+3                                                      
         SR    R5,R5                                                            
DEM5     NETGO NVDEMCON,DMCB,((R5),NDDEMBLK),DBLOCK,(7,WORK)                    
         MVC   0(7,R2),WORK                                                     
         LA    R2,12(R2)                                                        
         LA    R5,1(R5)                                                         
         BCT   R3,DEM5                                                          
BOXES    DS    0H                   SET PARAMS FOR BOXES                        
         CLI   NOBOX,C'N'                                                       
         BE    HDX                                                              
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         LTR   R5,R5               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         DROP  R2                                                               
         LA    R4,BOXCOLS                                                       
         USING PLINED,R4                                                        
         LA    R1,P                CENTER BOXCOLS ON AP1                        
         L     R2,AP1                                                           
         SR    R2,R1                                                            
         AR    R4,R2                                                            
         MVI   PSTART,C'L'                                                      
         ZIC   R1,DEMNUM                                                        
         LTR   R1,R1                                                            
         BNZ   *+12                                                             
         MVI   58(R4),C'R'                                                      
         B     BOX7                                                             
         LA    R4,55(R4)           BUMP TO DOLLARS                              
         MVI   0(R4),C'C'                                                       
         LA    R4,2(R4)            BUMP OVER E/A                                
         MVI   0(R4),C'C'                                                       
         LA    R4,12(R4)                                                        
         BCT   R1,*-8                                                           
BOX5     MVI   0(R4),C'R'                                                       
*                                                                               
BOX7     LA    R4,BOXROWS                                                       
         LA    R4,6(R4)                                                         
         MVI   0(R4),C'T'                                                       
         MVI   3(R4),C'M'                                                       
         MVI   51(R4),C'B'                                                      
HDX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
SUBHEADS DS    0H                                                               
         ZIC   R1,DEMNUM                                                        
         LTR   R1,R1                                                            
         BZ    SUBX                                                             
         L     R5,ABOX             SET BOXES FOR TOTAL LINE                     
         USING BOXD,R5                                                          
         LTR   R5,R5               IS ABOX ZEROS                                
         BZ    SD2               YES/ ON-LINE SKIP BOXES                        
         CLI   NOBOX,C'N'                                                       
         BE    SD2                                                              
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R4,BOXCOLS                                                       
         USING PTOTLIN,R4                                                       
         LA    R1,P                CENTER BOXCOLS ON AP1                        
         L     R2,ATP                                                           
         SR    R2,R1                                                            
         AR    R4,R2                                                            
         MVI   0(R4),C'L'                                                       
         ZIC   R1,DEMNUM                                                        
         LA    R4,PTDEMO+20                                                     
         B     *+12                                                             
         MVI   0(R4),C'C'                                                       
         LA    R4,21(R4)                                                        
         BCT   R1,*-8                                                           
         MVI   0(R4),C'R'                                                       
*                                                                               
         LA    R4,BOXROWS                                                       
         LA    R4,6(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,51(R4)                                                        
         MVI   0(R4),C'B'                                                       
         DROP  R4                                                               
*                                  SET NEW HEADS FOR TOT LINE                   
SD2      L     R3,ATP                                                           
         LA    R2,P                                                             
         SR    R3,R2                                                            
         LA    R2,H8                                                            
         AR    R2,R3                                                            
         MVC   AH8SV,AH8                                                        
         ST    R2,AH8                                                           
         USING PTOTLIN,R2                                                       
         ZIC   R1,DEMNUM                                                        
         LA    R2,PTDEMO                                                        
         LA    R2,132(R2)                                                       
SD3      MVC   3(3,R2),=C'RTG'                                                  
         MVC   10(4,R2),=C'IMPS'                                                
         MVC   16(3,R2),=C'CPM'                                                 
         MVC   133(19,R2),=19C'-'                                               
         LA    R2,21(R2)                                                        
         BCT   R1,SD3                                                           
         L     R2,AH8                                                           
         L     R6,ANETWS2          ** SET UP FOR DEMOCON                        
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         LA    R4,DBLOCK                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         ZIC   R3,DEMNUM                                                        
         LA    R2,PTDEMO+7                                                      
         SR    R5,R5                                                            
SD7      NETGO NVDEMCON,DMCB,((R5),NDDEMBLK),DBLOCK,(7,WORK)                    
         MVC   0(7,R2),WORK                                                     
         LA    R2,21(R2)                                                        
         LA    R5,1(R5)                                                         
         BCT   R3,SD7                                                           
         MVC   AH8,AH8SV                                                        
         DROP  R6,R2                                                            
SUBX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
GETPROD  NTR1                                                                   
         L     R2,ANETWS3                                                       
GTP3     CLC   NBSPLPRN,3(R2)                                                   
         BE    GTP5                                                             
         LA    R2,4(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   GTP3                                                             
         MVC   CURPRD,=C'UNA'                                                   
         B     XIT                                                              
GTP5     MVC   CURPRD,0(R2)                                                     
         MVC   CURPRD1,3(R2)                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H3,45,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         SPACE                                                                  
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
NOBOX    DS    CL1                 * FROM EDIT                                  
ASSFLG   DS    CL1                 * FROM EDIT                                  
POLFLG   DS    CL1                 * FROM EDIT                                  
CLTITLE  DS    CL40                * FROM EDIT                                  
PRDALL   DS    CL1                 * FROM EDIT                                  
NETALL   DS    CL1                 * FROM EDIT                                  
ESTALL   DS    CL1                 * FROM EDIT                                  
*                                                                               
RELO     DS    A                                                                
AP1      DS    A                                                                
AH8      DS    A                                                                
AH8SV    DS    A                                                                
ASUBTOTS DS    A                                                                
ATP      DS    F                                                                
EQUNT    DS    F                                                                
SAVIMPE  DS    CL1                                                              
SAVGRPE  DS    CL1                                                              
DEMNUM   DS    CL1                                                              
SUBTFLG  DS    CL1                                                              
EQUFLG   DS    CL1                                                              
EQUSBFLG DS    CL1                                                              
EQUPRFLG DS    CL1                                                              
SBFNLFLG DS    CL1                                                              
DOLSV    DS    CL8                 SAVE ACT COST OF SUBTOT LINE                 
CPMSV    DS    CL100               SAVE CPMS                                    
TEMPWRK  DS    CL20                                                             
CURNET   DS    CL4                                                              
CURPRD1  DS    CL1                                                              
CURPRD   DS    CL3                                                              
*                                                                               
SRTREC   DS    CL200                                                            
*                                  SUBTOTALS BY SPOT LENGTH                     
SUBTOTS  DS    F                   SPT LEN                                      
         DS    F                   NO OF UNITS                                  
         DS    PL8                 ACTUAL DOLLARS                               
SUBTDEM  DS    24F                 6 X EST/ACT GRP-EST/ACT IMP                  
*                                                                               
         DS    CL560               FOR 5 MORE SUBTOTS                           
*                                                                               
*                                  SUBTOTS2 (IF NETALL=Y)                       
SUBTOTS2 DS    F                   SPT LEN                                      
         DS    F                   NO OF UNITS                                  
         DS    PL8                 ACTUAL DOLLARS                               
SUBTDEM2 DS    24F                 6 X EST/ACT GRP-EST/ACT IMP                  
*                                                                               
         DS    CL560               FOR 5 MORE SUBTOTS                           
*                                                                               
*                                  SUBTOTS3 (IF PRDALL=Y)                       
SUBTOTS3 DS    F                   SPT LEN                                      
         DS    F                   NO OF UNITS                                  
         DS    PL8                 ACTUAL DOLLARS                               
SUBTDEM3 DS    24F                 6 X EST/ACT GRP-EST/ACT IMP                  
*                                                                               
         DS    CL560               FOR 5 MORE SUBTOTS                           
*                                                                               
FINALTOT DS    24F                 FINAL TOTALS                                 
FINLCST  DS    PL8                 FINAL COST                                   
UNITOT   DS    F                   FINAL UNITS                                  
FINLEQU  DS    24F                 FINAL EQUIVALENCED TOTALS                    
*                                                                               
SUBFINLT DS    24F                 SUBTOTALS      (IF NETALL=Y)                 
EQUSUBT  DS    24F                 EQUIV SUBTOTS  (IF NETALL=Y)                 
EQUSBCST DS    PL8                 EQUIV SUB COST (IF NETALL=Y)                 
*                                                                               
PRDSUBFT DS    24F                 SUBTOTALS      (IF PRDALL=Y)                 
PRDEQSBT DS    24F                 EQUIV SUBTOTS  (IF PRDALL=Y)                 
PRDEQCST DS    PL8                 EQUIV SUB COST (IF PRDALL=Y)                 
         EJECT                                                                  
*                                                                               
SRTRECD  DSECT                    SORT LINE DSECT                               
SRTPRD1  DS    CL3                                                              
SRTEST1  DS    CL1                                                              
SRTNET1  DS    CL4                                                              
SRTDATE  DS    CL4                                                              
SRTNET   DS    CL4                                                              
SRTIME   DS    CL4                                                              
SRTPROG  DS    CL16                                                             
SRTSLEN  DS    CL1                                                              
SRTPROD  DS    CL1                                                              
SRTDOLS  DS    CL4                                                              
SRTDEMOS DS    CL2                 GRP(ESTIMATED)                               
         DS    CL4                 IMP                                          
         DS    CL2                 GRP(ACTUAL)                                  
         DS    CL4                 IMP                                          
         DS    CL60                5 X 12                                       
SRTDEMO2 DS    CL2                 GRP(ESTIMATED)                               
         DS    CL4                 IMP                                          
         DS    CL2                 GRP(ACTUAL)                                  
         DS    CL4                 IMP                                          
         DS    CL60                5 X 12                                       
SRTEND   DS    CL1                                                              
*                                                                               
PLINED   DSECT                    PRINT LINE DSECT                              
PSTART   DS    CL1                                                              
PDATE    DS    CL6                                                              
         DS    CL1                                                              
PNET     DS    CL4                                                              
         DS    CL1                                                              
PPROG    DS    CL16                                                             
         DS    CL1                                                              
PTIME    DS    CL11                                                             
         DS    CL1                                                              
PSPTLEN  DS    CL2                                                              
         DS    CL1                                                              
PDOLLARS DS    CL10                                                             
         DS    CL1                                                              
         DS    CL1                 'E'/'A'                                      
         DS    CL1                                                              
PDEMOS   DS    CL4                                                              
         DS    CL1                                                              
         DS    CL6                                                              
         DS    CL1                                                              
         DS    CL60                                                             
PEND     DS    CL1                                                              
*                                                                               
         SPACE 1                                                                
PTOTLIN  DSECT                    TOTAL LINE DSECT                              
PTSTART  DS    CL1                                                              
         DS    CL1                                                              
         DS    CL1                 E/A                                          
         DS    CL1                                                              
PTDEMO   DS    CL6                 GRP                                          
         DS    CL1                                                              
         DS    CL7                 IMP                                          
         DS    CL1                                                              
         DS    CL5                 CPM                                          
         DS    CL1                                                              
         DS    CL105                                                            
PTEND    DS    CL1                                                              
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE7D                                                       
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRI37T  05/01/02'                                      
         END                                                                    
