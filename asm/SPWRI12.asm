*          DATA SET SPWRI12    AT LEVEL 006 AS OF 12/15/04                      
*PHASE T20412A,*                                                                
         TITLE 'T20412 - SPOT TV BUYING SCHEDULE'                               
*                                                                               
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI12 (T20412) - SPOT TV BUYING SCHEDULE (SBS)         *           
*                                                                   *           
* *** NOTE NOTE NOTE *** I DON'T KNOW IF THIS IS USED ANYMORE       *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 03NOV03 18 AKT -- FIX MGROUP X'40' BUGS                           *           
* 14OCT02 05 EFJ -- 2 CHAR MGR SCHEME CODES                         *           
* 06AUG01 03 EFJ -- HISTORY LOST                                    *           
*                -- REMOVE *INCLUDE DYNALLOC (NEVER REFERENCED)     *           
*********************************************************************           
T20412   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20412,RA                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         MVI   MYSPACES,C' '                                                    
         MVC   MYSPACES+1(L'MYSPACES-1),MYSPACES                                
*                                                                               
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      SPOTIO HOOK                                  
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
MYSPACES DS    CL132                                                            
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     DS    0H                  INTERESTED IN READING BUYS & GOALS           
         OI    SBQSKIP,SBQSKBIL       SO- SKIP STATION BILL RECORDS             
*                                                                               
         OI    COLIND,COLIDEM      TURN ON WEIGHTING FOR MULTI-MKTS             
         OI    COLIND,COLICPPT     DON'T PRINT CPP ON TOTAL LINE                
         OI    SBQPER,SBQPDY       DAILY INFORMATION IN BLOCKS                  
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         MVI   NDEMOS,3            NEED THREE DEMOS                             
*                                  SET DATA INDICATORS(FOR SPOTIO)              
         OI    DATAIND,DILEN       SPOT LENGTH                                  
         OI    DATAIND,DICPP       CPP                                          
*                                                                               
         OI    DATAIND2,DIPRDNM    PRODUCT NAME                                 
         OI    DATAIND2,DIPRD      PRODUCT                                      
         OI    DATAIND2,DIEST      ESTIMATE                                     
         OI    DATAIND2,DISTA      STATION                                      
*                                                                               
         OI    DATAIND3,DIESTNM    ESTIMATE NAME                                
*                                                                               
         OI    DATAIND4,DIDEMHED   DEMOS IN HEADLINE                            
*                                                                               
         MVI   MYFIRSTH,12         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
         LA    R2,SBSTITH          TITLE                                        
         MVC   TITLE,MYSPACES                                                   
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         CLI   SBQPGRD,C' '       SET PRODUCT GROUPS                            
         BH    *+14                                                             
         XC    RPTLEVS+1(2),RPTLEVS+1                                           
         B     INIT2                                                            
         CLC   SBPGR1LN,SBPGR2LN                                                
         BNE   INIT2                                                            
         MVI   RPTLEVS+2,0                                                      
*                                                                               
INIT2    CLI   SBQMGRD,0          SET MARKET GROUPS                             
         BH    *+14                                                             
         XC    RPTLEVS+5(3),RPTLEVS+5                                           
         B     INIT4                                                            
         CLC   SBMGR1LN,SBMGR2LN                                                
         BNE   *+14                                                             
         XC    RPTLEVS+6(2),RPTLEVS+6                                           
         B     INIT4                                                            
         CLC   SBMGR2LN,SBMGR3LN                                                
         BNE   INIT4                                                            
         MVI   RPTLEVS+7,0                                                      
*                                 SET LEVELS                                    
INIT4    XC    LEVELS,LEVELS                                                    
         LA    R1,LEVELS                                                        
         LA    RE,RPTLEVS                                                       
         LA    RF,1                                                             
INIT6    CLI   0(RE),X'FF'                                                      
         BE    INITX                                                            
         CLI   0(RE),0                                                          
         BE    INIT8                                                            
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
INIT8    LA    RE,1(RE)                                                         
         B     INIT6                                                            
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
RPTLEVS  DC    AL1(QCLT)          HEADLINES                                     
         DC    AL1(QPRDGR1)                                                     
         DC    AL1(QPRDGR2)                                                     
         DC    AL1(QPRD)                                                        
         DC    AL1(QEST)                                                        
         DC    AL1(QMKTGR1)                                                     
         DC    AL1(QMKTGR2)                                                     
         DC    AL1(QMKTGR3)                                                     
         DC    AL1(QMKT)          MIDLINE                                       
         DC    AL1(0)             DETAIL -DUMMY TIME                            
         DC    AL1(QSTA)                                                        
         DC    AL1(0)                     REAL TIME                             
         DC    AL1(0)                     SPOT LENGTH                           
         DC    AL1(0)                     PROGRAM NAME                          
         DC    X'FF'                                                            
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    DS    0H                                                               
         MVI   RPTOPT,C'N'        ROUNDED DEMOS  (DEFAULT)                      
         LA    R2,SBSRPTH                                                       
         CLI   5(R2),0                                                          
         BE    VALID2                                                           
         CLI   8(R2),C'N'                                                       
         BE    VALID2                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   EINV                                                             
         MVI   RPTOPT,C'Y'        UNROUNDED DEMOS                               
*                                                                               
VALID2   MVI   SPOTGRID,C'N'      TARGET POINTS IN GRID (DEFAULT)               
         LA    R2,SBSDATH                                                       
         CLI   5(R2),0                                                          
         BE    VALID5                                                           
         CLI   8(R2),C'N'                                                       
         BE    VALID5                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   EINV                                                             
         MVI   SPOTGRID,C'Y'      SPOTS IN GRID                                 
*                                                                               
VALID5   MVI   MYSPACE,1          SINGLE SPACING (DEFAULT)                      
         LA    R2,SBSPACH                                                       
         CLI   5(R2),0                                                          
         BE    VALIDX                                                           
         CLI   8(R2),C'1'                                                       
         BE    VALIDX                                                           
         CLI   8(R2),C'2'                                                       
         BNE   EINV                                                             
         MVI   MYSPACE,2          DOUBLE SPACING                                
VALIDX   B     XIT                                                              
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         GOTO1 CURSERR                                                          
*                                                                               
         EJECT                                                                  
* SPOTIO HOOK                                                                   
*                                                                               
INPUT    CLI   SBMODE,SBPROCSP    BUY RECORDS?                                  
         BNE   INPUT10                                                          
         MVI   FLGMODE,C'B'                                                     
         B     INPUTX                                                           
*                                                                               
INPUT10  CLI   SBMODE,SBPROCGL    GOAL RECORDS?                                 
         BNE   INPUTX                                                           
         MVI   FLGMODE,C'G'                                                     
         B     INPUTX                                                           
*                                                                               
INPUTX   B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHKNTR  NTR1  ,                                                                
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEADHK                                                           
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
*                                ------  ROWS                                   
         DC    CL8'ITIME   ',A(ITIME)                                           
         DC    CL8'IMYSLN  ',A(IMYSLN)                                          
         DC    CL8'IMYROW  ',A(IMYROW)                                          
         DC    CL8'OMYROW  ',A(OMYROW)                                          
         DC    CL8'OMYSTA  ',A(OMYSTA)                                          
         DC    CL8'ISEQ    ',A(ISEQ)                                            
*                                ------  COLUMNS                                
         DC    CL8'ITRGPT  ',A(ITRGPT)                                          
         DC    CL8'OTRGPT  ',A(OTRGPT)                                          
         DC    CL8'OSPOT   ',A(OSPOT)                                           
         DC    CL8'OMYDEMO ',A(OMYDEMO)                                         
         DC    CL8'IAVGCST ',A(IAVGCST)                                         
         DC    CL8'OAVGCST ',A(OAVGCST)                                         
         DC    CL8'OGLDOLT ',A(OGLDOLT)                                         
         DC    CL8'OGLDEMT ',A(OGLDEMT)                                         
*                                ------  TOTAL ROUTINES                         
         DC    CL8'MORETOT ',A(MORETOT)                                         
         DC    CL8'MORE2TOT',A(MORE2TOT)                                        
         DC    CL8'OMGR1USR',A(OMGR1)                                           
         DC    CL8'OMGR2USR',A(OMGR2)                                           
         DC    CL8'OMGR3USR',A(OMGR3)                                           
*                                ------  HEADLINE ROUTINES                      
         DC    CL8'TRGDAY  ',A(TRGDAY)                                          
         DC    CL8'TRGMON  ',A(TRGMON)                                          
         DC    CL8'TRGDAT  ',A(TRGDAT)                                          
         DC    CL8'DEMNAMS ',A(DEMNAMS)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINT   DS    0H                                                               
DRVINIT  DS    0H                                                               
         MVI   GLNORBOX,X'40'     SUPPRESS BOXES FOR TOTAL LINE                 
*                                                                               
         OI    GLINDS,GLPALDET     PRINT ALL DETAIL LINES                       
         MVC   GLSPACE,MYSPACE    SET SPACING                                   
*                                 SET DEMO TYPE                                 
         MVI   GLOPTS+2,C'Y'      DEFAULT TO PURCHASED                          
         CLI   SBQDATA,0                                                        
         BE    DRV20                                                            
*                                                                               
         TM    SBQDATA,SBQDPUR    PURCHASED?                                    
         BO    *+8                                                              
         MVI   GLOPTS+2,C'N'                                                    
*                                                                               
         MVI   GLOPTS+3,C'Y'                                                    
         TM    SBQDATA,SBQDRERT   RE-RATED?                                     
         BO    *+8                                                              
         MVI   GLOPTS+3,C'N'                                                    
*                                                                               
         MVI   GLOPTS+4,C'Y'                                                    
         TM    SBQDATA,SBQDAFFD   AFFIDAVID?                                    
         BO    *+8                                                              
         MVI   GLOPTS+4,C'N'                                                    
*                                                                               
DRV20    MVI   GLOPTS+5,C'T'      TARGET POINTS IN GRID                         
         CLI   SPOTGRID,C'N'                                                    
         BE    *+8                                                              
         MVI   GLOPTS+5,C'S'      SPOTS IN GRID                                 
*                                                                               
         MVI   FLGMED,C'T'                                                      
         CLI   SBSMED,C'T'        MEDIA T REQUESTED?                            
         BE    *+8                                                              
         MVI   FLGMED,C'R'                                                      
*                                                                               
         MVI   GLOPTS+6,C'R'      ROUNDED REPORT                                
         CLI   RPTOPT,C'N'                                                      
         BE    *+8                                                              
         MVI   GLOPTS+6,C'U'      UNROUNDED REPORT                              
*                                                                               
DRVX     B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
*                                                                               
         L     R5,SBACURCH         R4=A(BUY RECORD CHUNK)                       
         USING SCHUNKD,R5                                                       
*                                                                               
         L     R6,SBAIO1           R6=A(BUY RECORD)                             
         USING BUYRECD,R6                                                       
*                                                                               
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                 ---- ROWS                                     
*                                                                               
ITIME    DS    0H                                                               
         MVC   0(4,R2),=X'FFFFFFFF' NO TIME FOR                                 
         CLI   FLGMODE,C'G'         GOAL RECORDS                                
         BE    ITIMEX                                                           
         MVC   0(2,R2),BDTIMST      GET TIME DIRECTLY FROM BUY RECORD           
         MVC   2(2,R2),BDTIMEND     BOTH START AND END                          
*                                                                               
         CLC   BDTIMST,=X'0000'     IF START TIME BETWEEN 12A                   
         BL    ITIMEX                                                           
         CLC   BDTIMST,=X'022F'     AND 559A                                    
         BH    ITIMEX                                                           
         ICM   R1,3,BDTIMST                                                     
         A     R1,=F'2400'          ADD 2400 TO SINK TO END OF DAY              
         STCM  R1,3,0(R2)                                                       
ITIMEX   B     XIT                                                              
*                                                                               
*                                                                               
IMYSLN   DS    0H                                                               
         MVI   0(R2),X'FF'        NO SPOT LENGTH FOR                            
         CLI   FLGMODE,C'G'       GOAL RECORDS                                  
         BE    IMYSLNX                                                          
         MVC   0(1,R2),SBLEN      SPOT LENGTH                                   
IMYSLNX  B     XIT                                                              
*                                                                               
*                                                                               
IMYROW   CLI   FLGMODE,C'G'       TIME,SPOT LENGTH,PROG NAME                    
         BE    IMYROWX            NOT FOR GOAL RECORDS - EXIT                   
         MVC   0(2,R2),BDTIMST    GET TIME DIRECTLY FROM BUY RECORD             
         MVC   2(2,R2),BDTIMEND   BOTH START AND END                            
         MVC   4(1,R2),SBLEN      SPOT LENGTH                                   
         MVC   5(1,R2),BDDAYPT    GET DAYPART DIRECTLY FROM BUY RECORD          
         MVC   6(17,R2),BDPROGRM  GET PROGRAM NAME DIRECT. FROM BUY REC         
IMYROWX  B     XIT                                                              
*                                                                               
*                                                                               
OMYROW   GOTO1 UNTIME,DMCB,0(R2),0(R3) MOVE TIME TO PRINT LINE                  
         EDIT  (1,4(R2)),(3,13(R3))    MOVE SPOT LENGTH TO PRINT LINE           
         CLI   GLARGS,C'R'             ARE WE ROUNDED REPORT?                   
         BE    OMYROW7                                                          
         MVC   12(1,R3),5(R2)            NO  - UNRNDED HAS DAYPART              
*                                        YES - RNDED HAS NO DAYPART             
OMYROW7  MVC   198(17,R3),6(R2)        MOVE PROGRAM NAME TO PRINT LINE          
OMYROWX  B     XIT                                                              
*                                                                               
*                                                                               
OMYSTA   MVI   PRNTLIN,C'N'       LINE WON'T BE PRINTED                         
         CLC   0(4,R2),=C'AAAA'   DON'T PRINT GOAL STATION                      
         BE    OMYSTAX                                                          
         MVI   PRNTLIN,C'Y'       LINE WILL BE PRINTED                          
         MVC   0(4,R3),0(R2)      MOVE FIRST 4 LETTERS OF STATION ONLY          
         LA    R3,198(R3)                                                       
         OC    5(3,R2),5(R2)      TEST CABLE                                    
         BZ    OMYSTA2                                                          
         MVI   0(R3),C'/'         YES-FORMAT THE CABLE NETWORK                  
         MVC   1(3,R3),5(R2)                                                    
         B     OMYSTAX                                                          
OMYSTA2  CLI   FLGMED,C'T'        ARE WE TELEVISION?                            
         BE    OMYSTAX               YES - EXIT                                 
         MVI   0(R3),C'-'            NEED TO PRINT THE RADIO BAND               
         MVC   1(1,R3),4(R2)                                                    
         MVI   2(R3),C'M'                                                       
OMYSTAX  B     XIT                                                              
*                                                                               
*                                                                               
ISEQ     DS    0H                 SHOW ACTUAL COST (COST/SPOT)                  
         ICM   R1,15,SCSPOTS      FORCING SPOTS OF DIFFERENT COSTS              
         BZ    ISEQX              ON SEPERATE PRINT LINES                       
         ICM   RF,15,SCGROSS                                                    
         BZ    ISEQX                                                            
         SR    RE,RE                                                            
         DR    RE,R1                                                            
         STCM  RF,15,0(R2)        ACTUAL AVERAGE COST                           
ISEQX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                 ---- COLUMNS                                  
*                                                                               
ITRGPT   DS    0H                                                               
         CLI   FLGMODE,C'G'       SKIP IF READING GOAL RECORDS                  
         BE    ITRGPTX                                                          
         L     R1,SBADATE         R1=A(DATELIST)                                
         L     R8,SBNDATES        # OF DATES                                    
         LR    RE,R2                                                            
ITRGPT5  CLC   SCDATE,0(R1)                                                     
         BE    ITRGPT8                                                          
         LA    R1,4(R1)           NEXT SET OF DATES IN TABLE                    
         LA    RE,8(RE)           NEXT ENTRY SPOT                               
         BCT   R8,ITRGPT5                                                       
         DC    H'0'                                                             
*                                                                               
ITRGPT8  MVC   0(4,RE),SCSPOTS    NO - # OF SPOTS (BUYS)                        
         MVC   4(4,RE),SCDEMOS         PRIMARY DEMO VALUE                       
ITRGPTX  B     XIT                EXIT                                          
*                                                                               
*                                                                               
OSPOT    DS    0H                 SPOTS DISPLAYED IN GRID                       
         L     R8,SBNDATES                                                      
OSPOT5   ICM   RF,15,0(R2)                                                      
         BZ    OSPOT6                                                           
         EDIT  (RF),(3,0(R3))      SPOTS                                        
OSPOT6   TM    GLINDS,GLTOTLIN     ONLY SHOW TARGET POINTS                      
         BNO   OSPOT10             ON A TOTAL LINE                              
         XR    RE,RE                                                            
         ICM   RF,15,4(R2)         GET TARGET POINT                             
         BZ    OSPOT10                                                          
         CLI   GLARGS,C'R'         ARE WE ROUNDED REPORT?                       
         BE    OSPOT8                                                           
         EDIT  (RF),(4,198(R3)),1    NO - UNROUNDED SO                          
*                                                                               
OSPOT8   A     RF,=F'5'              YES - SO                                   
         D     RE,=F'10'                   ROUND, GET RID OF 1 DECIMAL          
         EDIT  (RF),(3,198(R3))                                                 
*                                                                               
OSPOT10  LA    R3,4(R3)                                                         
         CLI   GLARGS,C'R'                                                      
         BE    *+8                                                              
         LA    R3,1(R3)                                                         
         LA    R2,8(R2)                                                         
         BCT   R8,OSPOT5                                                        
         B     XIT                                                              
*                                                                               
*                                                                               
OTRGPT   DS    0H                 TARGET POINTS DISPLAYED IN GRID               
         L     R8,SBNDATES                                                      
OTRGPT5  XR    RE,RE                                                            
         ICM   RF,15,4(R2)        GET TARGET POINT                              
         BZ    OTRGPT8                                                          
         CLI   GLARGS,C'R'        ARE WE ROUNDED REPORT?                        
         BE    OTRGPT6                                                          
         EDIT  (RF),(4,0(R3)),1   NO - PRINT DECIMAL                            
         B     OTRGPT8                                                          
*                                                                               
OTRGPT6  A     RF,=F'5'           YES - SO                                      
         D     RE,=F'10'                ROUND, GET RID OF 1 DECIMAL             
         EDIT  (RF),(3,0(R3))                                                   
*                                                                               
OTRGPT8  TM    GLINDS,GLTOTLIN    ONLY SHOW SPOTS                               
         BNO   OTRGPT10           ON A TOTAL LINE                               
         EDIT  (4,0(R2)),(3,198(R3))                                            
*                                                                               
OTRGPT10 LA    R3,4(R3)                                                         
         CLI   GLARGS,C'R'        ARE WE ROUNDED REPORT?                        
         BE    *+8                                                              
         LA    R3,1(R3)           NO- MOVE EXTRA FOR DECIMAL                    
         LA    R2,8(R2)                                                         
         BCT   R8,OTRGPT5                                                       
         B     XIT                                                              
*                                                                               
*                                                                               
IAVGCST  DS    0H                                                               
         MVC   0(4,R2),SCSPOTS    NO - # OF SPOTS (BUYS)                        
         MVC   4(4,R2),SCGROSS         DOLLARS (COST)                           
         MVI   INDATA,1           TELL IT $0 IS DATA                            
IAVGCSTX B     XIT                                                              
*                                                                               
OAVGCST  DS    0H                                                               
         TM    GLINDS,GLTOTLIN    ON A TOTAL LINE                               
         BNO   OAVGCST5                                                         
         MVC   TOTCST,4(R2)          SAVE TOTAL COST                            
         B     OAVGCSTX                                                         
*                                                                               
OAVGCST5 ICM   R1,15,0(R2)        # OF SPOTS                                    
         BZ    OAVGCSTX                                                         
         ICM   RF,15,4(R2)        DOLLARS                                       
         BZ    OAVGCSTX                                                         
         SR    RE,RE                                                            
         DR    RE,R1                                                            
         STCM  RF,15,0(R2)        (RF)=COST/SPOT                                
         MVI   GLHOOK,GLEDIT      TELL DRIVER TO EDIT FOR ME                    
OAVGCSTX B     XIT                                                              
*                                                                               
*                                                                               
OMYDEMO  CLI   SBQMKTWT,C'N'                                                    
         BE    OMYDEMO2                                                         
         TM    OUTIND,OUTICRMK    YES - TEST ACROSS MARKETS                     
         BZ    OMYDEMO2                                                         
         OC    4(4,R2),4(R2)      YES - TEST DEMO IS WEIGHTED                   
         BZ    OMYDEMO2                                                         
         MVC   0(4,R2),4(R2)      YES - UNWEIGHT IT                             
         BAS   RE,UNWEIGHT                                                      
*                                                                               
OMYDEMO2 CLI   GLARGS,2           SECOND DEMO                                   
         BNE   OMYDEMO5                                                         
         MVC   SVDEMO2,0(R2)                                                    
         B     OMYDEMOX                                                         
*                                                                               
OMYDEMO5 CLI   PRNTLIN,C'N'       ARE WE PRINTING THIS LINE?/                   
         BE    OMYDEMO8           NOPE                                          
         EDIT  (4,0(R2)),(7,0(R3)),1                                            
         LA    R3,198(R3)                                                       
         EDIT  SVDEMO2,(7,0(R3)),1                                              
OMYDEMO8 MVI   PRNTLIN,C'Y'       RESET PRINT FLAG                              
OMYDEMOX B     XIT                                                              
*                                                                               
*                                                                               
UNWEIGHT NTR1                                                                   
         SR    RF,RF                                                            
         ICM   R1,15,TOTWGT       TOTAL WEIGHT                                  
         BZ    UNWT2                                                            
         ICM   RF,15,0(R2)                                                      
         BZ    UNWT2                                                            
         SR    RE,RE                                                            
         AR    RF,RF              X2                                            
         DR    RE,R1              DIVIDE BY TOTAL WEIGHT                        
         A     RF,=F'1'                                                         
         SRA   RF,1               ROUND                                         
*                                                                               
UNWT2    ST    RF,0(R2)                                                         
         B     XIT                                                              
*                                                                               
*                                                                               
OGLDOLT  TM    GLINDS,GLTOTLIN    INVISIBLE COLUMN                              
         BNO   *+10                                                             
         MVC   GOLDOLS,0(R2)      JUST SAVE TOTAL GOAL DOLLARS                  
         B     XIT                                                              
*                                                                               
*                                                                               
OGLDEMT  TM    GLINDS,GLTOTLIN    INVISIBLE COLUMN                              
         BNO   *+10                                                             
         MVC   GOLDEMS,0(R2)      JUST SAVE TOTAL GOAL DOLLARS                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                                 ---- TOTAL ROUTINES                           
*                                                                               
MORETOT  DS    0H                                                               
         MVC   0(16,R3),=C'TOT TRPS PER DAY'                                    
         LA    R3,198(R3)                                                       
         MVC   0(22,R3),=C'TOT # OF SPOTS PER DAY'                              
         B     RESTOT                                                           
*                                                                               
*                                                                               
MORE2TOT DS    0H                                                               
         MVC   0(22,R3),=C'TOT # OF SPOTS PER DAY'                              
         LA    R3,198(R3)                                                       
         MVC   0(16,R3),=C'TOT TRPS PER DAY'                                    
         B     RESTOT                                                           
*                                                                               
*                                                                               
RESTOT   DS    0H                                                               
         LA    R3,198(R3)                                                       
         MVC   0(10,R3),=C'TOTAL COST'                                          
         XR    RE,RE                                                            
         ICM   RF,15,TOTCST       ROUND BUY TOTAL TO DOLLARS                    
         BZ    RESTOTX                                                          
         A     RF,=F'50'                                                        
         D     RE,=F'100'                                                       
         EDIT  (RF),(8,25(R3))                                                  
*                                                                               
         LA    R3,198(R3)                                                       
         MVC   0(19,R3),=C'TOTAL BUDGET (GOAL)'                                 
         XR    RE,RE                                                            
         ICM   RF,15,GOLDOLS      ROUND GOAL TOTAL TO DOLLARS                   
         BZ    RESTOTX                                                          
         A     RF,=F'50'                                                        
         D     RE,=F'100'                                                       
         EDIT  (RF),(8,25(R3))                                                  
*                                                                               
         LA    R3,198(R3)                                                       
         MVC   0(19,R3),=C'TOTAL TRPS   (GOAL)'                                 
         XR    RE,RE                                                            
         ICM   RF,15,GOLDEMS      ROUND GOAL DEMO POINTS                        
         BZ    RESTOTX                                                          
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
         EDIT  (RF),(8,25(R3))                                                  
*                                 CLEAR OUT TOTALS                              
         XC    TOTCST,TOTCST                                                    
         XC    GOLDOLS,GOLDOLS                                                  
         XC    GOLDEMS,GOLDEMS                                                  
RESTOTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                 ---- HEADLINE ROUTINES                        
*                                                                               
TRGDAY   DS    0H                                                               
         L     R2,SBADATE         DATE LIST                                     
         L     R8,SBNDATES                                                      
TRGDAY10 GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)                                   
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
         MVC   0(3,R3),FULL       DAY    --1ST PRINT LINE                       
         LA    R3,4(R3)           NEXT SPOT ON PRINT LINE                       
         CLI   RPTOPT,C'N'                                                      
         BE    *+8                                                              
         LA    R3,1(R3)           DECIMAL NEEDS ONE MORE SPACE                  
         LA    R2,4(R2)           NEXT SET OF DATES                             
         BCT   R8,TRGDAY10                                                      
         B     XIT                                                              
*                                                                               
*                                                                               
TRGMON   DS    0H                                                               
         L     R2,SBADATE         DATE LIST                                     
         L     R8,SBNDATES                                                      
TRGMON10 GOTO1 DATCON,DMCB,(2,0(R2)),(4,WORK)                                   
         MVC   0(3,R3),WORK       MONTH  --2ND PRINT LINE                       
         LA    R3,4(R3)           NEXT SPOT ON PRINT LINE                       
         CLI   RPTOPT,C'N'                                                      
         BE    *+8                                                              
         LA    R3,1(R3)           DECIMAL NEEDS ONE MORE SPACE                  
         LA    R2,4(R2)           NEXT SET OF DATES                             
         BCT   R8,TRGMON10                                                      
         B     XIT                                                              
*                                                                               
*                                                                               
TRGDAT   DS    0H                                                               
         L     R2,SBADATE         DATE LIST                                     
         L     R8,SBNDATES                                                      
TRGDAT10 GOTO1 DATCON,DMCB,(2,0(R2)),(4,WORK)                                   
         MVC   1(2,R3),WORK+3     DATE   --3ND PRINT LINE                       
         LA    R3,4(R3)           NEXT SPOT ON PRINT LINE                       
         CLI   RPTOPT,C'N'                                                      
         BE    *+8                                                              
         LA    R3,1(R3)           DECIMAL NEEDS ONE MORE SPACE                  
         LA    R2,4(R2)           NEXT SET OF DATES                             
         BCT   R8,TRGDAT10                                                      
         B     XIT                                                              
*                                                                               
*                                                                               
DEMNAMS  DS    0H                 SECOND DEMO NAME                              
         MVC   0(7,R3),DEMNAMES                                                 
         LA    R3,198(R3)                                                       
         MVC   0(7,R3),DEMNAMES+7                                               
         B     XIT                                                              
*                                                                               
*                                                                               
* MARKET GROUP OUTPUT ROUTINES                                                  
*                                                                               
OMGR1    LA    R4,MGR1HEAD         FORMAT MGR1 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR1BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BAS   RE,MGRCODE                                                       
         LA    R1,SBMGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR1NM                                                
         MVC   19(24,R4),SBMGR1NM                                               
         B     XIT                                                              
*                                                                               
*                                                                               
OMGR2    LA    R4,MGR2HEAD         FORMAT MGR2 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR2BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BAS   RE,MGRCODE                                                       
         LA    R1,SBMGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR2NM                                                
         MVC   19(24,R4),SBMGR2NM                                               
         B     XIT                                                              
*                                                                               
*                                                                               
OMGR3    LA    R4,MGR3HEAD         FORMAT MGR3 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR3BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BAS   RE,MGRCODE                                                       
         LA    R1,SBMGR3LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR3NM                                                
         MVC   19(24,R4),SBMGR3NM                                               
         B     XIT                                                              
*                                                                               
*                                                                               
OGRPCODE UNPK  DUB(5),0(3,R2)                                                   
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         LA    R1,14(R4)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         AHI   R1,1                                                             
         EX    RF,OGRPMVC                                                       
         EX    RF,OGRPCLC                                                       
         BNE   *+10                                                             
         MVC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BR    RE                                                               
*                                                                               
OGRPMVC  MVC   0(0,R1),DUB                                                      
OGRPCLC  CLC   0(0,R1),=C'9999'                                                 
UNKNOWN  DC    C'** UNKNOWN **'                                                 
*                                                                               
MGRCODE  MVI   13(R4),C'?'                                                      
         LA    RF,SPMGRTAB                                                      
         LHI   R0,(SPMGRTBX-SPMGRTAB)/L'SPMGRTAB                                
         CLC   SBQMGRD,2(RF)                                                    
         BE    MGRC10                                                           
         AHI   RF,L'SPMGRTAB                                                    
         BCT   R0,*-14                                                          
         BR    RE                                                               
*                                                                               
MGRC10   MVC   13(2,R4),0(RF)                                                   
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
* HEADHOOK                                                                      
*                                                                               
HEADHK   DS    0H                                                               
         L     R1,AH4                                                           
         LA    R1,1(R1)                                                         
         LR    RE,R1                                                            
         A     RE,PWIDTH                                                        
         CLI   0(RE),C' '          TEST WHETHER HEADS'VE BEEN FORMATTED         
         BH    HD1                 YES                                          
         MVC   0(50,R1),MYSPACES   NO-REMOVE MEDIA FROM FIRST HEADLINE          
         B     HDX                    AND EXIT                                  
*                                                                               
HD1      LA    R5,HEADTAB                                                       
         USING HEADTABD,R5                                                      
*                                                                               
HD2      CLI   0(R5),0                                                          
         BE    HDX                                                              
         CLC   HDREP,GLRECNO       DRIVER REPORT NUMBER                         
         BNE   *+14                                                             
         CLC   HDNMGR,GLOPTS+1     N'MARKET GROUPS                              
         BE    *+12                                                             
         LA    R5,HEADTABL(R5)                                                  
         B     HD2                                                              
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,1,HDMGR1                                                      
         BZ    HDX                                                              
         LA    R3,DREGMGR                                                       
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR1HEAD,RF),MGR1HEAD                                        
         ICM   R2,1,HDMGR2                                                      
         BZ    HDX                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR2HEAD,RF),MGR2HEAD                                        
         ICM   R2,1,HDMGR3                                                      
         BZ    HDX                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR3HEAD,RF),MGR3HEAD                                        
*                                                                               
HDX      B     XIT                                                              
         SPACE 1                                                                
HDPOS    L     R1,AH4                                                           
         SH    R2,=H'4'                                                         
         BNP   *+12                                                             
         A     R1,PWIDTH                                                        
         BCT   R2,*-4                                                           
         LA    RF,0(R3,R1)                                                      
         BR    RE                                                               
         EJECT                                                                  
* ABOUT TO PRINT A LINE                                                         
*                                                                               
PRINT    DS    0H                                                               
         B     XIT                                                              
*                                                                               
*==============*                                                                
* LITERAL POOL *                                                                
*==============*                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
DREGMGR  EQU   48                                                               
*                                                                               
         DS    0F                                                               
MYSPACE  DS    XL1                1=SINGLE, 2=DOUBLE SPACING                    
PRNTLIN  DS    CL1                Y=PRINTING LINE,N=LINE DOESN'T PRINT          
FLGMODE  DS    CL1                B=BUY RECORDS, G=GOAL RECORDS                 
FLGMED   DS    CL1                T=TELEVSION REPORT, R=RADIO RPT               
SPOTGRID DS    CL1                Y=SPOTS, N=TARGET POINTS IN GRID              
RPTOPT   DS    CL1                N=ROUNDED DEMOS, Y=UNROUNDED DEMOS            
TOTCST   DS    XL4                TOTAL COST                                    
GOLDOLS  DS    XL4                TOTAL GOAL DOLLARS                            
GOLDEMS  DS    XL4                TOTAL GOAL DEMO POINTS                        
SVDEMO2  DS    XL4                DEMO VALUE # 2                                
*                                                                               
MGR1HEAD DS    CL44                                                             
MGR2HEAD DS    CL44                                                             
MGR3HEAD DS    CL44                                                             
*                                                                               
* HEADLINE POSITION TABLE                                                       
*                                                                               
HEADTAB  DC    X'01',X'00',X'000000'                                            
         DC    X'01',X'01',X'050000'                                            
         DC    X'01',X'02',X'050600'                                            
         DC    X'01',X'03',X'050607'                                            
         DC    X'00'                                                            
*                                                                               
*                                                                               
HEADTABD DSECT                                                                  
HDREP    DS    X                   REPORT NUMBER                                
HDNMGR   DS    X                   N'MARKET GROUPS                              
HDMGR1   DS    X                   HEADLINE FOR MARKET GROUP 1                  
HDMGR2   DS    X                   HEADLINE FOR MARKET GROUP 2                  
HDMGR3   DS    X                   HEADLINE FOR MARKET GROUP 3                  
HEADTABL EQU   *-HEADTABD                                                       
         EJECT                                                                  
T20412   CSECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*DEDBLOCK                                                                       
*SPGENBUY                                                                       
*SPWRIFFD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIEAD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPWRI12   12/15/04'                                      
         END                                                                    
