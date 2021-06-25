*          DATA SET SPREPPD02R AT LEVEL 045 AS OF 05/01/02                      
*PHASE SPPD02A                                                                  
*INCLUDE DEFINE                                                                 
*INCLUDE TSCAN                                                                  
*INCLUDE MININAM                                                                
         TITLE 'SPREPPD02 - MASTER SPOT DCF EXTRACT- AVG RATING CLONE'          
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
*        QOPT1   RATING SOURCE- A=ARB,N=NSI                                     
*        QOPT2   DPG PHASE (A THRU D = DOWNLOAD)                                
*        QOPT3   Y=DEMO TRACE                                                   
*        QOPT4   Y=INCLUDE STATION DETAIL                                       
*        QOPT5   DRIVER TEST VERSION                                            
*        QOPT6   (QOPT5+1) Y= INCLUDE NON-AFFS                                  
*                                                                               
SPPD02   CSECT                                                                  
         NMOD1 0,SPPD02,RR=R5                                                   
*                                                                               
         L     R9,0(R1)                                                         
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,R9,RA                                                    
         L     R8,=A(GENSUBS)                                                   
         USING GENSUBS,R8                                                       
         ST    R5,RELO                                                          
*                                                                               
         L     RC,=A(GLAREA)                                                    
         USING GLOBALD,RC                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SP050                                                            
         CLI   MODE,REQFRST                                                     
         BE    SP100                                                            
         B     EXIT                                                             
         SPACE 2                                                                
PMKTLIM  DC    H'32000'            MKTS TO PROCESS                              
PMKTTRC  DC    H'5'                MKTS TO TRACE                                
         SPACE 3                                                                
* RUN FIRST                                                                     
*                                                                               
SP050    DS    0H                                                               
         XC    ADRIVER,ADRIVER                                                  
         XC    ASYSDRV,ASYSDRV                                                  
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
* REQUEST FIRST                                                                 
*                                                                               
SP100    DS    0H                                                               
         MVC   DEMTRCE,QOPT3                                                    
         MVC   MKTLIM,PMKTLIM      MOVE LIMITS TO BETTER                        
         MVC   MKTTRC,PMKTTRC      ADDRESSABLE SPACE                            
         MVI   FORCEHED,C'Y'                                                    
         XC    HEADHOOK,HEADHOOK                                                
         MVI   DLHSW,0                                                          
*                                                                               
         LA    RE,GLOBALD          INIT DRIVER STORAGE TO ZEROS                 
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         MVC   GLSIZE,=F'40000'    GLOBAL SIZE                                  
         MVC   GLOBALD(8),=C'*GLOBAL*'                                          
*                                                                               
         MVI   GLTRACE,C'N'                                                     
         NI    GLDOWNLD,X'3F'                                                   
         MVC   DPGFILE+6(1),QOPT2                                               
         CLI   QOPT2,C' '                                                       
         BE    SP102                                                            
         CLI   QOPT2,C'D'          TEST DOWNLOADING                             
         BH    SP102               (A THRU D = DOWNLOADING)                     
         OI    GLDOWNLD,X'C0'                                                   
*                                                                               
SP102    DS    0H                                                               
         GOTO1 LOADER,DMCB,DPGFILE,0    LOAD DPG PROGRAM                        
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,GLAPROG                                                       
*                                                                               
         OC    ADRIVER,ADRIVER                                                  
         BNZ   SP110                                                            
         MVC   DRIVER+6(1),QOPT5   SET TEST VERSION                             
         GOTO1 LOADER,DMCB,DRIVER       LOAD DRIVER                             
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ADRIVER                                                       
*                                                                               
SP110    OC    ASYSDRV,ASYSDRV                                                  
         BNZ   SP120                                                            
         GOTO1 LOADER,DMCB,SPDRIVER     LOAD SYSTEM DRIVER                      
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ASYSDRV                                                       
*                                                                               
SP120    MVC   GLASYSDR,ASYSDRV    SYSTEM DRIVER ADDR                           
*                                                                               
         LA    RE,SPWORKD                                                       
         ST    RE,GLAWORKD         SPOT WORK ADDR                               
         LA    RE,DRHOOK                                                        
         ST    RE,GLAHOOK          OUR DRIVER HOOK                              
         MVI   GLTWORKD,GLTSPOT    SPOT WORK                                    
         MVI   GLDETHED,C'Y'       I GET HEADS AT DETAIL TIME                   
         LA    RE,PROGPROF                                                      
         ST    RE,GLAPPROF                                                      
         L     RE,GLAPLIST                                                      
         MVC   ASPDRWKC,0(RE)      A(SPOT DRIVER WORK AREA)                     
*                                                                               
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         MVC   SRCE,QOPT1          RATING SOURCE                                
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVI   CPROF+3,C'0'                                                     
         CLI   SRCE,C'A'           A=ARB, N=NSI                                 
         BNE   *+8                                                              
         MVI   CPROF+3,C'1'                                                     
*                                                                               
         XC    BMKT,BMKT                                                        
         CLI   QMKT,C'0'           TEST HAVE SINGLE MARKET REQ                  
         BL    SP121                                                            
         PACK  DUB,QMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
*                                                                               
*                                  GET MARKET NAMES                             
*                                  ----------------                             
SP121    DS    0H                                                               
         BAS   RE,BLDMTAB                                                       
*                                  GET UNIVERSES                                
*                                  -------------                                
         L     R4,ADBLOCK                                                       
         USING DBLOCK,R4                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELAGY,QAGY                                                    
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,SRCE       RATING SOURCE                                
         PACK  DUB,QBOOK1(2)       CONVERT BOOK                                 
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK                                                       
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK+1                                                     
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,ADBUY                                                         
         ST    RE,DBAREC                                                        
         MVI   DBSELDAY,X'40'                                                   
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1715'                                           
         MVI   DBFUNCT,DBGETTOT                                                 
*                                                                               
         L     R6,AMKTTAB                                                       
         USING MKTTABD,R6                                                       
         SPACE 2                                                                
SP121G   DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    SP122                                                            
*                                                                               
         MVC   DBSELRMK,MKNUM                                                   
         GOTO1 DEMAND,DMCB,ADBLOCK,SVUNIV                                       
         LA    R6,MKLEN(R6)                                                     
         B     SP121G                                                           
         DROP  R6                                                               
         SPACE 2                                                                
*                                  BUILD STATION TABLE                          
*                                  -------------------                          
SP122    DS    0H                                                               
         BAS   RE,BLDSTAB                                                       
         SPACE 2                                                                
*                                  GET DEMOS AND DO DRIVER INPUT                
*                                  -----------------------------                
         L     R4,ADBLOCK                                                       
         USING DBLOCK,R4                                                        
         XC    0(256,R4),0(R4)                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELAGY,QAGY                                                    
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,SRCE       RATING SOURCE                                
         PACK  DUB,QBOOK1(2)       CONVERT BOOK                                 
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK                                                       
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK+1                                                     
         MVC   DBCOMFCS,ACOMFACS                                                
         L     RE,ADBUY                                                         
         ST    RE,DBAREC                                                        
         MVI   DBSELDAY,X'40'                                                   
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1715'                                           
         L     R2,AMKTTAB          FIRST MARKET                                 
         USING MKTTABD,R2                                                       
         XC    MKTCTR,MKTCTR                                                    
*                                                                               
SP122F   DS    0H                                                               
         CLI   0(R2),X'FF'                                                      
         BE    SP128                                                            
*                                                                               
         ST    R2,SVADMKT          SAVE MKT POINTER                             
         LH    RF,MKTCTR           BUMP MKT COUNTER                             
         LA    RF,1(RF)                                                         
         STH   RF,MKTCTR                                                        
*                                                                               
         MVC   DBSELRMK,MKNUM                                                   
         MVI   DBFUNCT,DBGETMS     GET MARKET/STATIONS                          
         L     R3,=A(MSLST)        START OF MARKET/STATION LIST                 
         ST    R3,ANXTMSL                                                       
         MVI   0(R3),X'FF'                                                      
         GOTO1 DEMAND,DMCB,ADBLOCK,BMSLST                                       
*                                                                               
SP125    DS    0H                                                               
         L     R3,=A(MSLST)                                                     
         MVI   UNVSW,C'Y'          FOR FIRST STATION, ADD POPS                  
         MVI   ACTSW,0                                                          
*                                                                               
SP126    DS    0H                                                               
         CLI   0(R3),X'FF'         EOL                                          
         BE    SP127                                                            
*                                                                               
         USING STNTABD,R6                                                       
         LA    R6,X                                                             
         MVC   0(STNLEN,R6),SPACES                                              
         MVC   STNID,0(R3)          FIND IN STNTAB                              
*                                                                               
         GOTO1 BINSRCH,STNPARS,X                                                
         CLI   0(R1),1                                                          
         BNE   SP126D                                                           
         TM    GLDOWNLD,X'80'      NO MESSAGE IF DOWNLOADING                    
         BNZ   SP126H                                                           
         MVC   P(37),=C'**STATION XXXXX NOT IN STATION LIST**'                  
         MVC   P+10(5),STNID                                                    
         GOTO1 REPORT                                                           
         B     SP126H              SKIP STATION                                 
*                                                                               
SP126D   DS    0H                                                               
         L     R6,0(R1)                                                         
         CLC   STNAFF,=C'P  '      SKIP PUBLICS                                 
         BE    SP126H                                                           
*                                                                               
         CLI   QOPT5+1,C'Y'        TEST TO INCLUDE NON-AFF'S                    
         BE    SP126D4                                                          
         CLI   STNAFF,C'A'                                                      
         BE    SP126D4                                                          
         CLI   STNAFF,C'C'                                                      
         BE    SP126D4                                                          
         CLI   STNAFF,C'N'                                                      
         BNE   SP126H                                                           
*                                                                               
SP126D4  DS    0H                                                               
         GOTO1 =A(DPTDEM)                                                       
*                                                                               
*                                  DRIVER INPUT PHASE                           
*                                  ------------------                           
*                                                                               
         LA    R7,1                FOR DAYPART NUMBER                           
*                                                                               
SP126E   DS    0H                                                               
         LR    RF,R7                                                            
         SLL   RF,2                                                             
         L     RF,DPTDVSRS-4(RF)                                                
         LTR   RF,RF               TEST ANY DATA (QTHS)                         
         BZ    SP126G              NO SKIP                                      
*                                                                               
         MVI   ACTSW,C'Y'          SET STATION ACTIVE                           
         STC   R7,DPTNO                                                         
         LA    R5,2                START WITH PASS 2                            
         CLI   QOPT4,C'Y'          IF NOT DOING STATIONS                        
         BNE   SP126F                                                           
         TM    GLDOWNLD,X'80'      OR IN DOWNLOADING                            
         BNZ   SP126F                                                           
         LA    R5,1                ELSE START WITH PASS 1                       
*                                                                               
SP126F   DS    0H                                                               
         STC   R5,DRPASS                                                        
         MVI   GLMODE,GLINPUT      SET UP FOR DRIVER INPUT                      
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         LA    R5,1(R5)            NEXT PASS                                    
         CH    R5,=H'3'                                                         
         BNH   SP126F                                                           
*                                                                               
SP126G   DS    0H                                                               
         CH    R7,=Y(NDPTS)        TEST DONE WITH DPTS                          
         BNL   SP126H                                                           
         LA    R7,1(R7)            NEXT DAPART                                  
         B     SP126E                                                           
*                                                                               
SP126H   DS    0H                                                               
         LA    R3,5(R3)            NEXT STATION                                 
         CLI   ACTSW,C'Y'          IF A STATION WAS ACTIVE                      
         BNE   *+8                                                              
         MVI   UNVSW,C'N'          DONT ADD POPS FOR LATER STATIONS             
         MVI   ACTSW,0                                                          
         B     SP126                                                            
*                                                                               
SP127    DS    0H                                                               
         LA    R2,MKLEN(R2)        NEXT MARKET                                  
         B     SP122F                                                           
*                                                                               
SP128    DS    0H                                                               
*                                  DRIVER OUTPUT PHASE                          
*                                  -------------------                          
         MVI   FORCEHED,C'Y'                                                    
         MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 ADRIVER,DMCB,(RC)                                                
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        BUILD STATION LIST FOR MARKET                                          
         SPACE 2                                                                
BMSLST   NTR1                                                                   
         L     R4,ADBLOCK                                                       
         L     R5,DBAREC                                                        
         USING MLKEY,R5                                                         
         OC    MLKMKT,MLKMKT       SKIP SPILL                                   
         BNZ   BMSLX                                                            
         CLI   MLSTAT,C'0'         SKIP NUMERICS                                
         BNL   BMSLX                                                            
         CLI   MLBTYP,0            IGNORE SPECIAL SURVEYS (CABLE ETC)           
         BC    0,BMSLX             **NO-OP**                                    
*                                                                               
         L     R3,ANXTMSL                                                       
         MVC   0(5,R3),MLSTAT                                                   
         LA    R3,5(R3)                                                         
         C     R3,=A(MSLSTX)                                                    
         BNH   *+6                                                              
         DC    H'0'                TOO MANY STATIONS IN MKT                     
*                                                                               
         ST    R3,ANXTMSL                                                       
         MVI   0(R3),X'FF'         SET EOL                                      
*                                                                               
BMSLX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
DRHOOK   NTR1                                                                   
*                                                                               
*        DRIVER HOOK ROUTINE                                                    
*                                                                               
         CLI   GLHOOK,GLROUT       TEST TO EXECUTE USER ROUTINE                 
         BNE   DH005                                                            
         CLI   GLMODE,GLINPUT      YES - INPUT PHASE                            
         BE    DH100                                                            
         CLI   GLMODE,GLOUTPUT           OUTPUT PHASE                           
         BE    DH200                                                            
         DC    H'0'                                                             
*                                                                               
DH005    CLI   GLHOOK,GLRESOLV     TEST TO RESOLVE LABELS                       
         BE    DH010                                                            
         CLI   GLHOOK,GLRESLIT     TEST TO RESOLVE LITERAL                      
         BE    DH300                                                            
         CLI   GLHOOK,GLHEAD       HEADLINES                                    
         BE    DH400                                                            
         CLI   GLHOOK,GLPRINT      PRINT                                        
         BE    DH500                                                            
         CLI   GLHOOK,GLPUTSRT     PUT TO SORT OR NOT                           
         BE    DH450                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* RESOLVE ROUTINE LABELS                                                        
*                                                                               
DH010    LA    R1,ROUTLIST                                                      
         LA    R2,GLLABEL                                                       
*                                                                               
DH020    CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         CLC   0(8,R1),0(R2)                                                    
         BE    DH030                                                            
         LA    R1,12(R1)                                                        
         B     DH020                                                            
*                                                                               
DH030    MVC   GLAROUT,8(R1)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
ROUTLIST DS    0F                                                               
         DC    C'MKTNUM  ',A(MKTNUM)                                            
         DC    C'MKTNAM  ',A(MKTNAM)                                            
         DC    C'STATID  ',A(STATID)                                            
         DC    C'DEMIP   ',A(DEMIP)                                             
         DC    C'UNVIP   ',A(UNVIP)                                             
         DC    C'QTRIP   ',A(QTRIP)                                             
         DC    C'DLHTTL  ',A(DLHTTL)                                            
         DC    C'DLHSRC  ',A(DLHSRC)                                            
         DC    C'DLHYR   ',A(DLHYR)                                             
         DC    C'DLHQTR  ',A(DLHQTR)                                            
         DC    C'DLHNMKT ',A(DLHNMKT)                                           
         DC    C'DLHNDPT ',A(DLHNDPT)                                           
         DC    C'DLHNDEM ',A(DLHNDEM)                                           
         DC    C'DEMNAM  ',A(DEMNAM)                                            
         DC    C'DPTNOIN ',A(DPTNOIN)                                           
         DC    C'DPTNAM  ',A(DPTNAM)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* EXECUTE INPUT PHASE ROUTINES                                                  
*                                                                               
DH100    L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
* EXECUTE OUTPUT PHASE ROUTINES                                                 
*                                                                               
DH200    L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
* INPUT ROUTINES                                                                
*                                                                               
MKTNUM   DS    0H                  MARKET                                       
         XC    0(2,R3),0(R3)                                                    
         CLI   DRPASS,3            FOR US PASS                                  
         BE    EXIT                NO MKT NUMBER                                
         MVC   0(2,R3),MKNUM                                                    
         B     EXIT                                                             
*                                                                               
MKTNAM   DS    0H                  MKT NAME                                     
         MVC   0(26,R3),MKNAM                                                   
         CLI   DRPASS,3                                                         
         BNE   EXIT                                                             
         MVC   0(26,R3),=CL26'** U.S. TOTALS**'                                 
         B     EXIT                                                             
*                                                                               
STATID   DS    0H                                                               
         MVC   0(5,R3),STNID                                                    
         CLI   DRPASS,1                                                         
         BE    *+10                                                             
         MVC   0(5,R3),=C'*ALL*'                                                
         B     EXIT                                                             
*                                                                               
UNVIP    DS    0H                  POPULATION                                   
         CLI   DRPASS,1            EXCEPT FOR PASS 1                            
         BE    UNVIP2                                                           
         SR    R1,R1                                                            
         CLI   UNVSW,C'Y'          TEST TO DO POPS                              
         BNE   UNVIP4                                                           
*                                                                               
UNVIP2   DS    0H                                                               
         ZIC   RE,GLARGS           ARG IS DEMO NUM                              
         BCTR  RE,R0                                                            
         SLL   RE,2                                                             
         L     R1,MKUNVS(RE)                                                    
UNVIP4   DS    0H                                                               
         ST    R1,0(R3)                                                         
         B     EXIT                                                             
*                                                                               
DEMIP    DS    0H                  DEMO VALUE                                   
         ZIC   RF,DPTNO                                                         
         BCTR  RF,R0                                                            
         MH    RF,=Y(NDEMS*4)                                                   
         ZIC   RE,GLARGS           ARG IS DEMO NUM                              
         BCTR  RE,R0                                                            
         SLL   RE,2                                                             
         LA    R1,DPTDMS(RF)                                                    
         L     R1,0(R1,RE)                                                      
         TM    GLDOWNLD,X'80'      IF DOWNLOADING DO MKT AUD                    
         BC    0,DEMIP4            **NOP SKIP**                                 
         M     R0,MKUNVS(RE)       RTG X POP                                    
         LA    RF,100              PEOPLE IN TENS                               
         BAS   RE,DIV                                                           
*                                                                               
DEMIP4   DS    0H                                                               
         ST    R1,0(R3)                                                         
         B     EXIT                                                             
*                                                                               
QTRIP    DS    0H                  QTR HOURS (WEIGHT)                           
         ZIC   RF,DPTNO            ARG IS DEMO NUMBER                           
         SLL   RF,2                X 4                                          
         L     R1,DPTDVSRS-4(RF)                                                
         ST    R1,0(R3)                                                         
         B     EXIT                                                             
         SPACE 2                                                                
DLHTTL   DS    0H                  DOWNLOAD HEADER                              
         MVC   0(8,R3),=C'DCFMAST '                                             
         B     EXIT                                                             
*                                                                               
DLHSRC   DS    0H                                                               
         MVC   0(4,R3),=C'ARB '    SOURCE                                       
         CLI   SRCE,C'A'                                                        
         BE    *+10                                                             
         MVC   0(4,R3),=C'ACN '                                                 
         B     EXIT                                                             
*                                                                               
DLHYR    DS    0H                                                               
         PACK  DUB,QBOOK1(2)       YEAR                                         
         CVB   R1,DUB                                                           
         STC   R1,0(R3)                                                         
         B     EXIT                                                             
*                                                                               
DLHQTR   DS    0H                                                               
         MVI   0(R3),C'1'          FIRST QTR                                    
         CLC   QBOOK1+2(2),=C'02'  FEB                                          
         BE    EXIT                                                             
         MVI   0(R3),C'2'          2ND QTR                                      
         CLC   QBOOK1+2(2),=C'05'  MAY                                          
         BE    EXIT                                                             
         MVI   0(R3),C'3'         3RD QTR                                       
         CLC   QBOOK1+2(2),=C'07'  JUL                                          
         BE    EXIT                                                             
         MVI   0(R3),C'4'          4TH QTR                                      
         CLC   QBOOK1+2(2),=C'11'  NOV                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
DLHNMKT  DS    0H                  NUMBER OF MARKETS                            
         MVC   0(2,R3),MKTCNT+2                                                 
         B     EXIT                                                             
*                                                                               
DLHNDPT  DS    0H                  NUMBER OF DPTS                               
         MVI   0(R3),NDPTS                                                      
         B     EXIT                                                             
*                                                                               
DLHNDEM  DS    0H                  NUMBER OF DEMOS                              
         MVI   0(R3),NDEMS                                                      
         B     EXIT                                                             
*                                                                               
DPTNOIN  DS    0H                  DAYPART SEQ NO.                              
         MVC   0(1,R3),DPTNO                                                    
         B     EXIT                                                             
*                                                                               
DPTNAM   DS    0H                  DAYPART NAME                                 
         ZIC   RF,DPTNO                                                         
         SLL   RF,1                                                             
         LA    RF,DPTLST-2(RF)                                                  
         MVC   0(2,R3),0(RF)                                                    
         B     EXIT                                                             
*                                                                               
* OUTPUT ROUTINES                                                               
*                                                                               
DEMNAM   DS    0H                                                               
         ZIC   RF,GLARGS           DEMO                                         
         MH    RF,=H'5'                                                         
         LA    RF,DNAMES-5(RF)                                                  
         MVC   0(5,R3),0(RF)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
         SPACE 2                                                                
* RESOLVE LITERALS                                                              
*                                                                               
DH300    DS    0H                                                               
         DC    H'0'                                                             
*                                                                               
         SPACE 2                                                                
* HEADLINES                                                                     
*                                                                               
DH400    DS    0H                                                               
         MVC   HEAD3(11),=C'*** ACN ***'                                        
         MVC   HEAD4(11),=C'-----------'                                        
         CLI   SRCE,C'N'                                                        
         BE    *+10                                                             
         MVC   HEAD3+4(3),=C'ARB'                                               
*                                                                               
         MVC   HEAD6(5),=C'BOOK='                                               
         MVC   WORK(4),QBOOK1                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(6,DUB)                                         
         MVC   HEAD6+6(6),DUB                                                   
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        TEST TO PUT TO SORT OR NOT                                             
*                                                                               
DH450    DS    0H                                                               
         TM    GLDOWNLD,X'80'      IF DOWNLOADING                               
         BZ    EXIT                                                             
         CLI   GLRECNO,1           FOR RECORD 1 (HEADER)                        
         BNE   DH452                                                            
         CLI   DLHSW,0             TEST HAVE DONE HEADS                         
         BNE   EXIT                                                             
         MVI   DLHSW,1                                                          
         MVI   GLHOOK,GLDONT                                                    
         B     EXIT                                                             
*                                                                               
DH452    DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRINT                                                                  
*                                                                               
DH500    DS    0H                                                               
         B     EXIT                                                             
         DROP R2                                                                
*                                                                               
*        BUILD MARKET DATA TABLE                                                
         SPACE 2                                                                
BLDMTAB  NTR1                                                                   
         MVC   MKTSAV+40(8),=C'ADILIST '                                        
         CLI   SRCE,C'A'                                                        
         BE    *+10                                                             
         MVC   MKTSAV+40(8),=C'DMALIST '                                        
*                                                                               
         OPEN  (MKTSAV,(INPUT))                                                 
*                                  SET BINSRCH PARS                             
         SR    R0,R0                                                            
         L     R1,=A(MKTTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,MKLEN                                                         
         LA    R4,MKKLEN                                                        
         LH    R5,=Y(MKTMAX)                                                    
         STM   R0,R5,MKTPARS                                                    
*                                                                               
         LA    R6,X                                                             
         XC    X,X                                                              
         USING MKTTABD,R6                                                       
         L     R5,ADBUY                                                         
         USING MSRECD,R5           MARKET SAVE RECORD                           
*                                                                               
BMK4     DS    0H                                                               
         GET   MKTSAV,(R5)                                                      
         CLI   0(R5),C'*'          SKIP COMMENTS                                
         BE    BMK4                                                             
         LA    RF,MSRNUM                                                        
         PACK  DUB,0(3,RF)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,MKNUM                                                       
         MVC   MKNAM,MSRNAM                                                     
         MVC   MKTZ,MSRTZ                                                       
         MVC   MKCLS,MSRCLS                                                     
         MVC   MKREG,MSRREG                                                     
         MVC   MKABBR,MSRABBR                                                   
*                                                                               
         GOTO1 BINSRCH,MKTPARS,(1,X)                                            
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   MKTCNT+2(2),MKTLIM                                               
         BNH   BMK4                                                             
*                                                                               
BMEOF    DS    0H                                                               
         L     R6,AMKTTAB                                                       
         LA    RF,MKLEN            SET EOL                                      
         MH    RF,MKTCNT+2                                                      
         AR    RF,R6                                                            
         MVI   0(RF),X'FF'                                                      
*                                                                               
         CLOSE MKTSAV                                                           
         B     EXIT                                                             
         SPACE 2                                                                
MSRECD   DSECT                                                                  
MSRNAM   DS    CL30                                                             
MSRNUM   DS    CL3                                                              
MSRTZ    DS    CL1                                                              
MSRCLS   DS    CL1                                                              
MSRREG   DS    CL2                                                              
MSRABBR  DS    CL8                                                              
         DS    CL32                SPARE                                        
         SPACE 2                                                                
SPPD02   CSECT                                                                  
MKTSAV   DCB   DDNAME=MKTSAV,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=BMEOF                                                      
         SPACE 2                                                                
* SAVE UNIVERSES IN RANK TABLE                                                  
SVUNIV   NTR1                                                                   
         LA    R3,DEMLST           CHANGE DEMLST TO UNIVS                       
         LA    RF,NDEMS                                                         
*                                                                               
         MVI   1(R3),C'L'                                                       
         LA    R3,3(R3)                                                         
         BCT   RF,*-8                                                           
*                                                                               
         L     R4,ADBLOCK                                                       
         L     RF,ACOMFACS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMLST),ADBLOCK,DOUTS                            
         USING MKTTABD,R6                                                       
         MVC   MKUNVS,DOUTS                                                     
*                                                                               
         CLI   SRCE,C'N'           FOR ACN                                      
         BNE   SVU6                MUST DIVIDE BY 100                           
*                                                                               
         LA    R3,MKUNVS                                                        
         LA    R5,NDEMS                                                         
*                                                                               
SVU4     DS    0H                                                               
         L     R1,0(R3)                                                         
         SR    R0,R0                                                            
         L     RF,=F'100'                                                       
         BAS   RE,DIV                                                           
         ST    R1,0(R3)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,SVU4                                                          
*                                                                               
SVU6     DS    0H                                                               
         DROP  R6                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        BUILD STATION TABLE                                                    
         SPACE 2                                                                
BLDSTAB  NTR1                                                                   
*                                  SET BINSRCH PARS                             
         SR    R0,R0                                                            
         L     R1,=A(STNTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,STNLEN                                                        
         LA    R4,STNKLEN                                                       
         LH    R5,=Y(STNMAX)                                                    
         STM   R0,R5,STNPARS                                                    
*                                                                               
         OPEN  (STASAV,(INPUT))                                                 
*                                                                               
         LA    R6,X                                                             
         XC    X,X                                                              
         USING STNTABD,R6                                                       
         L     R5,ADBUY                                                         
         USING STSRECD,R5           STATION SAVE RECORD                         
*                                                                               
BST4     DS    0H                                                               
         GET   STASAV,(R5)                                                      
         CLI   0(R5),C'*'          SKIP COMMENTS                                
         BE    BST4                                                             
         MVC   STNID(STNLEN),STSID                                              
*                                                                               
         GOTO1 BINSRCH,STNPARS,(1,X)                                            
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     BST4                                                             
*                                                                               
BSTEOF   DS    0H                                                               
         L     R6,ASTNTAB                                                       
         LA    RF,STNLEN            SET EOL                                     
         MH    RF,STNCNT+2                                                      
         AR    RF,R6                                                            
         MVI   0(RF),X'FF'                                                      
*                                                                               
         CLOSE STASAV                                                           
*                                                                               
BSTX     B     EXIT                                                             
         SPACE 2                                                                
STSRECD  DSECT                                                                  
STSID    DS    CL5                                                              
STSADI   DS    CL3                                                              
STSDMA   DS    CL3                                                              
STSAFF   DS    CL3                                                              
STSTZ    DS    CL1                                                              
STSMCLS  DS    CL1                                                              
STSLTST  DS    CL4                                                              
STSSTAT  DS    CL1                                                              
*                                                                               
SPPD02   CSECT                                                                  
STASAV   DCB   DDNAME=STASVIP,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=BSTEOF                                                     
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        DPTDEM- GET DEMOS BY DAYPART                                           
         SPACE 2                                                                
         DS    0F                                                               
DPTDEM   NMOD1 0,**DPTDEM                                                       
         USING MKTTABD,R2                                                       
         USING STNTABD,R6                                                       
*                                                                               
         LA    RF,STNADI           TEST STATION HAS PROPER MKT                  
         CLI   SRCE,C'A'                                                        
         BE    *+8                                                              
         LA    RF,STNDMA                                                        
         CLI   0(RF),C' '                                                       
         BNH   SD60                                                             
*                                                                               
         LA    RE,DPTDMS           CLEAR  OUTPUT AREA                           
         LH    RF,=Y(NDEMS*NDPTS*4)                                             
         XCEF                                                                   
         XC    DPTDVSRS,DPTDVSRS                                                
*                                                                               
         L     R4,ADBLOCK                                                       
         USING DBLOCK,R4                                                        
*                                                                               
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBLSTSBF,1          TIME PERIOD DEMOS                            
         XC    DBSELMK,DBSELMK                                                  
         MVC   DBSELSTA,STNID      SET STATION                                  
         MVC   DBSELTIM(2),=H'0600'    6A                                       
         MVC   DBSELTIM+2(2),=H'0545'  TO 545A                                  
         MVI   DBSELDAY,X'7F'          M-SU                                     
*                                                                               
         LA    R3,DEMLST           CHANGE DEMLST TO RTGS                        
         LA    RF,NDEMS                                                         
*                                                                               
         MVI   1(R3),C'R'                                                       
         LA    R3,3(R3)                                                         
         BCT   RF,*-8                                                           
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,ADBLOCK,SVSDEMS                                        
         CLI   DBERROR,0                                                        
         BE    SD50                                                             
         CLI   DBERROR,X'80'                                                    
         BE    SD50                                                             
         B     SD50                NO-OP                                        
         DC    H'0'                                                             
*                                                                               
SD50     DS    0H                                                               
SD60     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
*        GET DEMOS                                                              
         SPACE 2                                                                
SVSDEMS  NTR1                                                                   
         GOTO1 =V(DEFINE),DMCB,=C'PROG+',ADBLOCK,WORK                           
         CLC   WORK(7),=C'OFF AIR'   SKIP                                       
         BE    SVDX                                                             
         GOTO1 =V(DEFINE),DMCB,=C'DAY',ADBLOCK,X                                
         MVC   BINDAY,X              SAVE DAY                                   
         GOTO1 =V(DEFINE),DMCB,=C'TIME',ADBLOCK,X                               
         MVC   STIM,X+2              SAVE START                                 
         CLC   STIM,=H'0600'       0001-0559 TO                                 
         BNL   *+16                TO 2401-2959                                 
         LH    RF,STIM                                                          
         LA    RF,2400(RF)                                                      
         STH   RF,STIM                                                          
*                                                                               
         LA    R7,DPTDEFS                                                       
         USING DFDS,R7                                                          
*                                                                               
SVD40    DS    0H                                                               
         CLI   0(R7),X'FF'         END OF DPT DEF LIST                          
         BE    SVD57                                                            
*                                  ALL IN FIRST DAYPART                         
SVD48    DS    0H                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMLST),ADBLOCK,DOUTS                            
*                                                                               
         ZIC   RF,DFDPT            DPT NUM IS INDEX TO DIVISOR TABLE            
         SLL   RF,2                                                             
         L     RE,DPTDVSRS-4(RF)                                                
         AH    RE,DBFACTOR         BUMP DIVISOR FOR DPT                         
         ST    RE,DPTDVSRS-4(RF)                                                
*                                                                               
         LA    R5,NDEMS                                                         
         LA    R2,DOUTS                                                         
         ZIC   R3,DFDPT            DPT NUM IS INDEX TO OUTPUT                   
         BCTR  R3,R0                                                            
         MH    R3,=Y(NDEMS*4)                                                   
         LA    R3,DPTDMS(R3)                                                    
*                                                                               
SVD50    DS    0H                                                               
         L     R1,0(R2)                                                         
         MH    R1,DBFACTOR                                                      
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,SVD50                                                         
         B     SVD57                                                            
*                                                                               
SVD55    DS    0H                                                               
         LA    R7,DFLEN(R7)      NEXT DPT                                       
         B     SVD40                                                            
*                                                                               
SVD57    DS    0H                                                               
         CLI   DEMTRCE,C'Y'                                                     
         BNE   SVD59                                                            
         CLC   MKTCTR,MKTTRC       TEST AT TRACE LIMIT                          
         BH    SVD59                                                            
*                                                                               
         MVC   P(5),STNID                                                       
         EDIT  (B2,ORIGMKT),(3,P+6),ZERO=NOBLANK,FILL=0                         
         EDIT  (B2,DBSELMK),(3,P+10),ZERO=NOBLANK,FILL=0                        
         GOTO1 =V(DEFINE),DMCB,=C'PROG+',ADBLOCK,X                              
         MVC   P+15(16),X                                                       
         GOTO1 =V(DEFINE),DMCB,=C'TIME',ADBLOCK,X                               
         EDIT  (B2,X+2),(4,P+32),FILL=0                                         
         EDIT  (B2,X+4),(4,P+37),FILL=0                                         
         GOTO1 =V(DEFINE),DMCB,=C'DAY',ADBLOCK,X                                
         MVC   P+43(3),X+2                                                      
         GOTO1 =V(DEFINE),DMCB,=C'WEEK',ADBLOCK,X                               
         MVC   P+47(4),X+1                                                      
         L     R5,DOUTS                                                         
         EDIT  (R5),(7,P+52),ZERO=NOBLANK                                       
         ZIC   RF,DFDPT            DPT NUM IS INDEX TO DIVISOR TABLE            
         BCTR  RF,R0                                                            
         MH    RF,=Y(NDEMS*4)                                                   
         L     R2,DPTDMS(RF)                                                    
         EDIT  (R2),(7,P+60),ZERO=NOBLANK                                       
         EDIT  (B2,DBFACTOR),(3,P+68),ZERO=NOBLANK                              
         ZIC   RF,DFDPT                                                         
         SLL   RF,2                                                             
         L     R2,DPTDVSRS-4(RF)                                                
         EDIT  (R2),(4,P+72),ZERO=NOBLANK                                       
*                                  DAYPART DEFINITION                           
         MVC   P+114(3),=C'***'                                                 
         CLI   DFDPT,X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                QTH UNASSIGNED TO DAYPART                    
*                                                                               
         EDIT  (B1,DFDPT),(2,P+86)                                              
         GOTO1 HEXOUT,DMCB,DFDAYS,P+89,1,=C'N'                                  
         EDIT  (B2,DFSTIM),(4,P+93)                                             
         EDIT  (B2,DFETIM),(4,P+98)                                             
         MVC   P+103(10),DFTZ                                                   
*                                                                               
         ZIC   RF,DFDPT                                                         
         SLL   RF,1                                                             
         LA    RF,DPTLST-2(RF)                                                  
         MVC   P+114(2),0(RF)                                                   
*                                                                               
SVD58    DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
SVD59    DS    0H                                                               
SVDX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 2                                                                
*        TSTPGM - TEST PROGRAM TYPE                                             
         SPACE 2                                                                
TSTPGM   NTR1                                                                   
         MVI   TYPRET,C' '                                                      
*                                                                               
         MVC   BYTE,1(R5)          SAVE PGM TYPE FROM FIRST                     
*                                                                               
TPT4     DS    0H                                                               
         CLC   BYTE,1(R5)          TEST DONE WITH TYPE                          
         BNE   TPTX                                                             
*                                                                               
         ZIC   R2,0(R5)                                                         
         GOTO1 =V(TSCAN),DMCB,WORK,15,((R2),2(R5)),((R2),2(R5))                 
         CLI   DMCB+16,0           TEST FOUND                                   
         BE    TPT8                NO- CONTINUE                                 
*                                                                               
         MVC   TYPRET,DFINEX       IF FOUND- SET RETURN CODE                    
         B     TPTX                                                             
*                                                                               
TPT8     DS    0H                                                               
         LA    R5,2(R2,R5)         NEXT ENTRY                                   
         B     TPT4                                                             
*                                                                               
TPTX     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
       ++INCLUDE SPMPPGMS                                                       
         SPACE 3                                                                
SPPD02   CSECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*        GENSUBS- GENERAL SUBROUTINES AND DATA AREAS                            
         SPACE 2                                                                
GENSUBS  DS    0D                                                               
         SPACE 3                                                                
DIV      DIV   (R0),(RF)       RETURNS VIA RE                                   
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
*        DEMO LIST                                                              
*                                                                               
DEMLST   DS    0X                                                               
         DC    X'00',C'L',AL1(127)   V02+    ONE DEMO                           
NDEMS    EQU   (*-DEMLST)/3                                                     
         DC    X'FFFFFF'                                                        
*                                                                               
         SPACE 2                                                                
DNAMES   DS    0X                                                               
         DC    CL5'V02+ '                                                       
*                                                                               
         SPACE 3                                                                
*        DAYPART NAME LIST                                                      
*                                                                               
DPTLST   DS    0X                                                               
         DC    CL2'XX'             ONE DAYPART                                  
NDPTS    EQU   (*-DPTLST)/2                                                     
*                                                                               
         DC    3X'FF'                                                           
*                                                                               
         SPACE 3                                                                
RELO     DC    F'0'                                                             
DOUTS    DS    XL(NDEMS*4)                                                      
DPTDMS   DS    XL(NDEMS*NDPTS*4)                                                
DPTDVSRS DS    XL(NDPTS*4)                                                      
SVORMKT  DS    H                                                                
ORIGMKT  DS    H                                                                
ORIGNAM  DS    CL26                                                             
SVADMKT  DS    A                                                                
SMKTCNT  DS    F                                                                
X        DS    XL256                                                            
SRCE     DS    C                                                                
TYPRET   DS    C                                                                
         DS    0D                                                               
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
ASPDRWKC DS    A                                                                
MKTPARS  DS    6F                                                               
STNPARS  DS    6F                                                               
AMKTTAB  EQU   MKTPARS+4                                                        
ASTNTAB  EQU   STNPARS+4                                                        
MKTCNT   EQU   MKTPARS+8                                                        
STNCNT   EQU   STNPARS+8                                                        
*                                                                               
DPGFILE  DC    CL8'SPPD05  '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
XFF      DC    20X'FF'                                                          
*                                                                               
SVKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
DEMTRCE  DS    CL1                                                              
ANXTMSL  DS    F                                                                
MKTCTR   DS    H                                                                
MKTLIM   DS    H                                                                
MKTTRC   DS    H                                                                
STIM     DS    H                                                                
BINDAY   DS    X                                                                
DRPASS   DS    X                                                                
DPTNO    DS    X                                                                
DLHSW    DS    C                                                                
UNVSW    DS    X                                                                
ACTSW    DS    X                                                                
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
MKTTABD  DSECT                                                                  
MKNUM    DS    XL2                                                              
MKKLEN   EQU   *-MKTTABD                                                        
MKRNK    DS    XL2                                                              
MKNAM    DS    CL30                                                             
MKTZ     DS    CL1                                                              
MKCLS    DS    CL1                                                              
MKREG    DS    CL2                                                              
MKABBR   DS    CL8                                                              
         DS    0F                                                               
MKUNVS   DS    XL(4*NDEMS)                                                      
MKSPIL   DS    C                                                                
MKLEN    EQU   *-MKTTABD                                                        
*                                                                               
MKTMAX   EQU   250                                                              
         SPACE 2                                                                
STNTABD  DSECT                                                                  
STNID    DS    CL5                                                              
STNKLEN  EQU   *-STNTABD                                                        
STNADI   DS    CL3                                                              
STNDMA   DS    CL3                                                              
STNAFF   DS    CL3                                                              
STNTZ    DS    CL1                                                              
STNMCLS  DS    CL1                                                              
STNLTST  DS    CL4                                                              
STNSTAT  DS    CL1                                                              
STNLEN   EQU   *-STNTABD                                                        
*                                                                               
STNMAX   EQU   1600                                                             
         SPACE 3                                                                
*                                                                               
SPPD02   CSECT                                                                  
         DS    0D                                                               
MKTTAB   DS    CL(MKTMAX*MKLEN)                                                 
         DS    0D                                                               
STNTAB   DS    0X                                                               
         ORG   *+(STNMAX*STNLEN)                                                
         DS    0D                                                               
MSLST    DS    0X                                                               
         ORG   *+(100*5)                                                        
MSLSTX   DS    0X                                                               
         DS    0D                                                               
         DC    CL8'DRGLOBAL'                                                    
GLAREA   DS    40000X              DRIVER GLOBAL AREA                           
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         SPACE 2                                                                
       ++INCLUDE SPDRVWRKD                                                      
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
DBLKD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPREPPD02R05/01/02'                                      
         END                                                                    
