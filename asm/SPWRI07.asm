*          DATA SET SPWRI07    AT LEVEL 057 AS OF 12/06/04                      
*PHASE T20407A                                                                  
         TITLE 'T20407 - CCUSA MARKET DETAIL STATUS REPORTS'                    
T20407   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20407,RA                                                  
         LR    R6,RC                                                            
         USING WORKD,R6                                                         
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPSPHOOK     SPOT HOOK                                    
         BE    SPHOOK                                                           
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL      FINAL CALL                                   
         BE    FINAL                                                            
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     OI    SBQSKIP,SBQSKBIL    SKIP READING STATION BILL RECORDS            
         OI    SBQDATA,SBQDGOAL+SBQDPUR  SET DATA                               
         OI    COLIND,COLIDEM      INDICATE DEMOS IS THIS RPT                   
         MVI   DEMOPT,DEMOTGT      TARGET DEMOS ONLY                            
         MVI   WGTOPT,C'Y'         FORCE MARKET WEIGHTING                       
         XC    BUFFL,BUFFL                                                      
         XC    SVBFKEY,SVBFKEY                                                  
         XC    SVBUYKEY,SVBUYKEY                                                
         XC    AGYCLIST(4),AGYCLIST                                             
         MVC   AGCODE,SPACES                                                    
         MVC   AGNAME,SPACES                                                    
         MVI   SVBPRD,0                                                         
         MVI   SVBEST,0                                                         
*                                                                               
         CLI   SBQPGRD,C' '        SET LEVELS                                   
         BH    *+14                                                             
         XC    RPTLEVS+1(2),RPTLEVS+1  SET PRODUCT GROUPS                       
         B     INIT2                                                            
         CLC   SBPGR1LN,SBPGR2LN                                                
         BNE   INIT2                                                            
         MVI   RPTLEVS+2,0                                                      
*                                                                               
INIT2    CLI   SBQMGRD,C' '        SET MARKET GROUPS                            
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
*                                                                               
INIT4    LA    R1,LEVELS           SET THE LEVELS                               
         LA    RE,RPTLEVS                                                       
         LA    RF,1                                                             
INIT6    CLI   0(RE),X'FF'                                                      
         BE    INIT10                                                           
         CLI   0(RE),0                                                          
         BE    INIT8                                                            
         MVC   0(1,R1),0(RE)                                                    
         CLI   0(R1),QMKT          MARKET--                                     
         BNE   *+12                                                             
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         STC   RF,MKTLEV           MARKET LEVEL                                 
         CLI   0(R1),QSTA          STATION--                                    
         BNE   *+12                                                             
         STC   RF,MIDLEV           MIDLINE LEVEL                                
         STC   RF,INDPTLEV         LEVEL ABOVE DAYPART                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
INIT8    LA    RE,1(RE)                                                         
         B     INIT6                                                            
*                                                                               
INIT10   MVI   MYFIRSTH,12         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
         MVI   RPTOPT,RPTSUM+RPTDET   DEFAULT IS SUMMARY+DETAIL RPTS            
         LA    R2,CS2RPTH                                                       
         CLI   5(R2),0                                                          
         BE    INIT12                                                           
         CLI   8(R2),C'1'                                                       
         BL    EINV                                                             
         BH    *+12                                                             
         NI    RPTOPT,FF-RPTDET    1=SUMMARY ONLY                               
         B     INIT12                                                           
         CLI   8(R2),C'3'                                                       
         BH    EINV                                                             
         BE    INIT12                                                           
         NI    RPTOPT,FF-RPTSUM    2=DETAIL ONLY                                
*                                                                               
INIT12   LA    R2,CS2DETH          DEFAULT IS ONLY SHOW BUY DETAILS             
         MVI   ALLMKTS,C'N'        FOR MARKETS OUTSIDE TOLERANCE                
         CLI   5(R2),0                                                          
         BE    INIT14                                                           
         CLI   8(R2),C'1'          1=ONLY OUTSIDE TOLERANCE MKTS                
         BL    EINV                                                             
         BE    INIT14                                                           
         CLI   8(R2),C'2'                                                       
         BH    EINV                                                             
         MVI   ALLMKTS,C'Y'        2=ALL MARKETS                                
*                                                                               
INIT14   MVC   PCTTOLC,=H'100'     DEFAULT TOLERENCES ARE 10PCT                 
         MVC   PCTTOLP,=H'100'                                                  
*                                                                               
         LA    R2,CS2PTLH          YES-VALIDATE OPTIONS                         
         LA    R5,PCTTOLP          POINTS TOLERENCE                             
         BAS   RE,VALPCT                                                        
         LA    R2,CS2CTLH          COST TOLERANCE                               
         LA    R5,PCTTOLC                                                       
         BAS   RE,VALPCT                                                        
*                                                                               
         LA    R2,CS2TITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT18   GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
INIT20   LH    RE,=H'1000'         SET LOWER AND UPPER PCT TOLERENCES           
         LH    RF,PCTTOLP          POINTS                                       
         LR    R1,RE                                                            
         SR    R1,RF                                                            
         ST    R1,LOLIMP                                                        
         LR    R1,RE                                                            
         AR    R1,RF                                                            
         ST    R1,HILIMP                                                        
         LH    RF,PCTTOLC          COST                                         
         LR    R1,RE                                                            
         SR    R1,RF                                                            
         ST    R1,LOLIMC                                                        
         LR    R1,RE                                                            
         AR    R1,RF                                                            
         ST    R1,HILIMC                                                        
*                                                                               
         SR    R1,R1               FIND EXTRA LENGTH OF RECORD                  
         CLI   SBQPGRD,C' '        DUE TO PRODUCT AND MARKET GROUPS             
         BNH   INIT22                                                           
         LA    R1,2(R1)                                                         
         CLC   SBPGR1LN,SBPGR2LN                                                
         BE    INIT22                                                           
         LA    R1,2(R1)                                                         
*                                                                               
INIT22   CLI   SBQMGRD,C' '                                                     
         BNH   INIT24                                                           
         LA    R1,2(R1)                                                         
         CLC   SBMGR1LN,SBMGR2LN                                                
         BE    INIT24                                                           
         LA    R1,2(R1)                                                         
         CLC   SBMGR2LN,SBMGR3LN                                                
         BE    INIT24                                                           
         LA    R1,2(R1)                                                         
*                                                                               
INIT24   ST    R1,LGRPS                                                         
         B     INITX                                                            
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
RPTLEVS  DC    AL1(QCLT)           HEADLINES                                    
         DC    AL1(QPRDGR1)                                                     
         DC    AL1(QPRDGR2)                                                     
         DC    AL1(QPRD)                                                        
         DC    AL1(QEST)                                                        
         DC    AL1(QMKTGR1)                                                     
         DC    AL1(QMKTGR2)                                                     
         DC    AL1(QMKTGR3)                                                     
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QMKT)           LAST HEADLINE                                
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QSTA)           MIDLINE                                      
         DC    AL1(QBUY)           DETAIL                                       
         DC    X'FF'                                                            
         EJECT                                                                  
* VALIDATE PERCENT OPTION                                                       
* R2=A(FIELD HEADER)                                                            
* R5=A(2-BYTE PERCENT FIELD)                                                    
*                                                                               
VALPCT   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    VPCTX                                                            
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),0                                    
         CLI   4(R1),1                                                          
         BNE   EPCT                                                             
         LA    R4,BLOCK                                                         
         CLI   1(R4),0                                                          
         BNE   EPCT                                                             
         TM    2(R4),X'80'         TEST NUMERIC                                 
         BZ    *+16                                                             
         L     R1,4(R4)            YES                                          
         MH    R1,=H'10'                                                        
         B     VPCT8                                                            
         CLI   0(R4),2             NO                                           
         BL    EPCT                                                             
         CLI   0(R4),4                                                          
         BH    EPCT                                                             
         ZIC   R3,0(R4)                                                         
         LA    RE,12(R4)                                                        
         MVI   BYTE,0                                                           
         SR    R1,R1                                                            
         LA    RF,FULL                                                          
*                                                                               
VPCT2    CLI   0(RE),C'.'                                                       
         BNE   VPCT4                                                            
         CLI   BYTE,0                                                           
         BNE   EPCT                                                             
         MVI   BYTE,1                                                           
         CLI   2(RE),C' '                                                       
         BNE   EPCT                                                             
         B     VPCT6                                                            
*                                                                               
VPCT4    CLI   0(RE),C'0'                                                       
         BL    EPCT                                                             
         CLI   0(RE),C'9'                                                       
         BH    EPCT                                                             
         MVC   0(1,RF),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
*                                                                               
VPCT6    LA    RE,1(RE)                                                         
         BCT   R3,VPCT2                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FULL(1)                                                      
         CVB   R1,DUB                                                           
*                                                                               
VPCT8    LTR   R1,R1                                                            
         BNP   EPCT                                                             
         CH    R1,=H'1000'                                                      
         BNL   EPCT                                                             
         STH   R1,0(R5)                                                         
*                                                                               
VPCTX    B     XIT                                                              
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    LA    R2,CS2CLTH                                                       
         CLC   SBQCLT,=C'CC '                                                   
         BNE   ECLT                                                             
         LA    R2,CS2PRDH                                                       
         CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BNE   VALID2              NO                                           
         MVI   SBQBPRD,0           YES-BREAK OUT THE PRODUCTS                   
         OI    SBQPIND,SBQPOLSP                                                 
*                                                                               
VALID2   MVI   SBQSEPES,C'Y'       BREAK OUT ESTIMATES                          
         B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
ECLT     MVC   CONHEAD(L'ECLTM),ECLTM                                           
         B     MYCURSOR                                                         
*                                                                               
EPCT     MVC   CONHEAD(L'EPCTM),EPCTM                                           
         B     MYCURSOR                                                         
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
ECLTM    DC    C'CLIENT MUST BE CC'                                             
EPCTM    DC    C'INVALID PERCENT'                                               
         EJECT                                                                  
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCES     HOOK MODE PASSED FROM SPOTIO                 
         BE    ESTIMATE                                                         
         CLI   SBMODE,SBPROCGL                                                  
         BE    PROCGOAL                                                         
         B     XIT                                                              
         EJECT                                                                  
* ESTIMATE FIRST                                                                
*                                                                               
ESTIMATE TM    DATAIND3,DIESTNM    YES-TEST ESTIMATE DETAILS PUT BY             
         BO    ESTX                    SPWRI01                                  
         GOTO1 PUTESTNM                NO-PUT THEM HERE                         
*                                                                               
ESTX     B     XIT                                                              
         EJECT                                                                  
* PROCESS GOAL RECORD                                                           
*                                                                               
PROCGOAL L     R3,SBAIO1                                                        
         USING GOALRECD,R3                                                      
         XC    BFREC,BFREC                                                      
         MVC   BFPRD,GKEYPRD                                                    
         MVC   BFEST,GKEYEST                                                    
         MVC   BFMKT,GKEYMKT                                                    
         CLC   GKEYPRD,SVBPRD      TEST PRD/EST CHANGED                         
         BNE   *+14                                                             
         CLC   GKEYEST,SVBEST                                                   
         BE    GL1                                                              
         MVC   SVBPRD,GKEYPRD      YES-GET ESTIMATE DATES                       
         MVC   SVBEST,GKEYEST                                                   
         GOTO1 GETESTNM                                                         
*                                                                               
GL1      BAS   RE,GETDATE          GET COMPLETION DATE                          
         MVC   SVDATE,BFDATE       SAVE THE DATE                                
         MVC   SBEPRD,SBQBPRD                                                   
         CLI   SBQBPRD,FF                                                       
         BNE   *+8                                                              
         MVI   SBEPRD,0                                                         
         B     GLX                 ** PASS ALL GOALS **                         
*                                                                               
         MVI   BYTE,0                                                           
         OC    BFDATE,BFDATE       TEST COMPLETED                               
         BNZ   *+8                 YES                                          
         OI    BYTE,X'01'          NO                                           
         CLI   GKEYPRD2,0          TEST PIGGYBACK                               
         BE    GL2                 NO                                           
         MVC   BFPRD,GKEYPRD2      YES-GET COMPLETION DATE FOR 2ND PRD          
         BAS   RE,GETDATE                                                       
         MVC   SVDATE,BFDATE                                                    
         OC    BFDATE,BFDATE       TEST COMPLETED                               
         BNZ   GL2                 YES                                          
         OI    BYTE,X'02'          NO                                           
*                                                                               
GL2      CLI   BYTE,0              TEST COMPLETE                                
         BE    GLX                 YES                                          
         CLI   GKEYPRD2,0          NO-TEST 2ND PRODUCT                          
         BE    GL4                 NO-DROP THE GOAL RECORD                      
         CLI   BYTE,3              YES-TEST BOTH PRDS INCOMPLETE                
         BE    GL4                 YES-DROP                                     
         CLI   SBEPRD,0            TEST SINGLE PRODUCT REQUEST                  
         BE    GL3                 NO                                           
         CLC   SBEPRD,GKEYPRD      YES                                          
         BNE   *+12                                                             
         TM    BYTE,X'01'                                                       
         BO    GL4                                                              
         CLC   SBEPRD,GKEYPRD2                                                  
         BNE   GLX                                                              
         TM    BYTE,X'02'                                                       
         BO    GL4                                                              
         B     GLX                                                              
*                                                                               
GL3      MVC   SBEPRD,GKEYPRD      FILTER ON PRODUCT THAT IS COMPLETE           
         TM    BYTE,X'01'                                                       
         BZ    GLX                                                              
         MVC   SBEPRD,GKEYPRD2                                                  
         B     GLX                                                              
*                                                                               
GL4      MVI   SBBPRD,0            FORCE GOAL RECORD TO BE DROPPED              
*                                                                               
GLX      B     XIT                                                              
         SPACE 1                                                                
GETDATE  LR    R0,RE                                                            
         CLC   BFKEY,SVBFKEY       TEST CHANGE IN PRD/EST/MKT                   
         BNE   *+14                YES                                          
         MVC   BFDATE,SVDATE       NO-PICK UP SAVED DATE                        
         B     GDX                                                              
         OC    BUFFL,BUFFL         TEST BUFFALO INITIALIZED YET                 
         BNZ   *+12                                                             
         BAS   RE,INITBUFF                                                      
         B     GD2                                                              
         BAS   RE,BUFHI                                                         
         BNE   *+14                                                             
         CLC   BFKEY,SVBFKEY       TEST PRD/EST/MKT PUT TO BUFFALO YET          
         BE    GDX                 YES                                          
         MVC   BFKEY,SVBFKEY       NO-READ THE STATUS RECORD                    
*                                                                               
GD2      BAS   RE,GETSTAT          GET STATUS RECORD                            
         MVC   SVBFKEY,BFKEY       SAVE THE KEY                                 
         MVC   BFACCUM,=F'1'       PUT TO BUFFALO                               
         BAS   RE,BUFPUT                                                        
*                                                                               
GDX      LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* SPOT HOOK                                                                     
* HOOK TESTS PRD/EST/MKT IS COMPLETE                                            
* IF NOT, THEN SBYORN IS SET TO C'N'                                            
*                                                                               
SPHOOK   L     R3,SBAIO1                                                        
         USING BUYREC,R3                                                        
         L     R4,SBASPOT                                                       
         USING REGELEM,R4                                                       
         XC    BFREC,BFREC                                                      
         CLI   RCODE,6             DETERMINE THE PRODUCT                        
         BL    SPHKX                                                            
         CLI   RCODE,8                                                          
         BH    *+14                                                             
         MVC   BFPRD,BUYKPRD                                                    
         B     SPHK2                                                            
         CLI   RCODE,11                                                         
         BL    SPHKX                                                            
         CLI   RCODE,13                                                         
         BH    SPHKX                                                            
         CLI   RLEN,10             TEST UNALLOCATED                             
         BH    *+12                                                             
         MVI   SBYORN,C'N'                                                      
         B     SPHKX               YES-THEN INCOMPLETE                          
         MVC   BFPRD,RPPRD                                                      
*                                                                               
SPHK2    MVC   BFEST,BUYKEST                                                    
         MVC   BFMKT,BUYMSTA                                                    
         CLC   BFKEY,SVBFKEY       TEST CHANGE IN PRD/EST/MKT                   
         BNE   SPHK3               YES                                          
         MVC   BFDATE,SVDATE       NO-PICK UP SAVED DATE                        
         OC    BFDATE,BFDATE       TEST COMPLETION DATE                         
         BZ    SPHKNO              NO                                           
         B     SPHK10                                                           
*                                                                               
SPHK3    OC    BUFFL,BUFFL         TEST BUFFALO INITIALIZED YET                 
         BNZ   SPHK4                                                            
         BAS   RE,INITBUFF                                                      
         B     SPHK6                                                            
*                                                                               
SPHK4    BAS   RE,BUFHI                                                         
         BNE   SPHK5                                                            
         CLC   BFKEY,SVBFKEY       TEST PRD/EST/MKT PUT TO BUFFALO YET          
         BNE   SPHK5               NO                                           
         OC    BFDATE,BFDATE       YES-TEST COMPLETION DATE                     
         BNZ   SPHK10              NON-ZERO - GET BUY ACTIVITY DATE             
         B     SPHKNO              ZERO - DROP THIS SPOT                        
*                                                                               
SPHK5    MVC   BFKEY,SVBFKEY       NO-READ THE STATUS RECORD                    
         XC    BFACCUM,BFACCUM                                                  
*                                                                               
SPHK6    MVC   SBESTNDP,RDATE      FAKE ESTIMATE END DATE TO BUY DATE           
         BAS   RE,GETSTAT          GET STATUS RECORD                            
         BNE   SPHK12              NOT FOUND                                    
         OC    BFDATE,BFDATE       TEST NON-ZERO COMPLETION DATE                
         BZ    SPHK12              NO                                           
*                                                                               
SPHK10   OC    BDCHG,BDCHG         LAST CHANGE DATE                             
         BZ    SPHK12                                                           
         GOTO1 DATCON,DMCB,(3,BDCHG),(2,HALF)                                   
         CLC   BFDATE,HALF         TEST BUY ACTIVITY DATE LATER                 
         BNL   SPHK12              NO                                           
         MVC   BFDATE,HALF         YES-SET DATE TO ACTIVITY DATE                
         B     SPHK13                                                           
*                                                                               
SPHK12   OC    BFACCUM,BFACCUM     TEST NEW RECORD                              
         BNZ   SPHK14              NO                                           
*                                                                               
SPHK13   MVC   BFACCUM,=F'1'       PUT TO BUFFALO                               
         BAS   RE,BUFPUT                                                        
*                                                                               
SPHK14   OC    BFDATE,BFDATE       TEST COMPLETION/ACTIVITY DATE                
         BNZ   SPHK20                                                           
*                                                                               
SPHKNO   B     SPHK20              PASS INCOMPLETE MARKETS 7/21/89              
         MVI   SBYORN,C'N'                                                      
         B     SPHKX                                                            
*                                                                               
SPHK20   CLC   SVBUYKEY,BUYREC     TEST NEW BUY RECORD                          
         BE    *+16                                                             
         MVC   SVBUYKEY,BUYREC     YES-CLEAR COMPLETION DATE TABLE              
         XC    CMPDTLST,CMPDTLST                                                
         LA    R0,MAXPRDS          ADD PRODUCT'S COMPLETION DATE TO             
         LA    R1,CMPDTLST         COMPLETION DATE TABLE                        
*                                                                               
SPHK21   CLI   0(R1),0                                                          
         BNE   SPHK22                                                           
         MVC   0(1,R1),BFPRD                                                    
         MVC   1(2,R1),BFDATE                                                   
         B     SPHKX                                                            
*                                                                               
SPHK22   CLC   BFPRD,0(R1)                                                      
         BE    SPHKX                                                            
         LA    R1,3(R1)                                                         
         BCT   R0,SPHK21                                                        
         DC    H'0'                                                             
*                                                                               
SPHKX    MVC   SVBFKEY,BFKEY       SAVE BUFFALO KEY                             
         MVC   SVDATE,BFDATE       SAVE THE DATE                                
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPUTSRT     PUT A SORT RECORD                            
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLLAST       LASTS                                        
         BE    LASTS                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  MVI   GLOPTS+2,C'N'       SET SUMMARY RPT OPTION                       
         TM    RPTOPT,RPTSUM                                                    
         BZ    *+8                                                              
         MVI   GLOPTS+2,C'Y'                                                    
         MVI   GLOPTS+3,C'N'       SET DETAIL RPT OPTION                        
         TM    RPTOPT,RPTDET                                                    
         BZ    *+8                                                              
         MVI   GLOPTS+3,C'Y'                                                    
         OI    GLINDS,GLPALTOT     NO TOTAL SUPRESSION                          
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'OMGR1USR',A(OMGR1)                                           
         DC    CL8'OMGR2USR',A(OMGR2)                                           
         DC    CL8'OMGR3USR',A(OMGR3)                                           
         DC    CL8'IMKTUSR ',A(IMKT)                                            
         DC    CL8'OMKTUSR1',A(OMKT1)                                           
         DC    CL8'OMKTUSR2',A(OMKT2)                                           
         DC    CL8'OSPTUSR ',A(OSPT)                                            
         DC    CL8'OCOSTUSR',A(OCOST)                                           
         DC    CL8'ODEMUSR1',A(ODEMO1)                                          
         DC    CL8'ODEMUSR2',A(ODEMO2)                                          
         DC    CL8'ONDXP   ',A(ONDXP)                                           
         DC    CL8'ONDXC   ',A(ONDXC)                                           
         DC    CL8'OSTATP  ',A(OSTATP)                                          
         DC    CL8'OSTATC  ',A(OSTATC)                                          
         DC    CL8'ODLC    ',A(ODLC)                                            
         DC    CL8'FRSTEST ',A(FRSTEST)                                         
         DC    CL8'TOTEST  ',A(TOTEST)                                          
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* MARKET GROUP OUTPUT ROUTINES                                                  
*                                                                               
OMGR1    LA    R4,MGR1HEAD         FORMAT MGR1 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR1BK                                                
         MVC   13(1,R4),SBQMGRD                                                 
         LA    R1,SBMGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR1NM                                                
         MVC   19(24,R4),SBMGR1NM                                               
         B     XIT                                                              
*                                                                               
OMGR2    LA    R4,MGR2HEAD         FORMAT MGR2 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR2BK                                                
         MVC   13(1,R4),SBQMGRD                                                 
         LA    R1,SBMGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR2NM                                                
         MVC   19(24,R4),SBMGR2NM                                               
         B     XIT                                                              
*                                                                               
OMGR3    LA    R4,MGR3HEAD         FORMAT MGR3 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR3BK                                                
         MVC   13(1,R4),SBQMGRD                                                 
         LA    R1,SBMGR3LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR3NM                                                
         MVC   19(24,R4),SBMGR3NM                                               
         B     XIT                                                              
*                                                                               
OGRPCODE UNPK  DUB(5),0(3,R2)                                                   
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,OGRPMVC                                                       
         EX    RF,OGRPCLC                                                       
         BNE   *+10                                                             
         MVC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BR    RE                                                               
*                                                                               
OGRPMVC  MVC   14(0,R4),DUB                                                     
OGRPCLC  CLC   14(0,R4),=C'9999'                                                
UNKNOWN  DC    C'** UNKNOWN **'                                                 
         EJECT                                                                  
* MARKET ROUTINES FOR MARKETS SUMMARY REPORT                                    
*                                                                               
IMKT     MVC   0(2,R2),SBBMKT                                                   
         MVC   2(1,R2),MKTIND                                                   
         MVI   MAXTOTLV,0                                                       
         CLI   SBMODE,SBPROCGL     TEST PROCESSING GOALS                        
         BNE   IMKT2                                                            
         OC    SVDATE,SVDATE       YES-TEST COMPLETE                            
         BZ    IMKT6                   NO-NO TOTALS                             
         B     IMKTX                                                            
*                                                                               
IMKT2    CLI   SBMODE,SBPROCSP     TEST PROCESSING BUYS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,CMPDTLST         YES-FIND THE PRD'S COMPLETION DATE           
*                                                                               
IMKT4    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SBBPRD,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     IMKT4                                                            
         OC    1(2,R1),1(R1)       TEST COMPLETE                                
         BNZ   IMKTX               YES                                          
*                                                                               
IMKT6    MVC   MAXTOTLV,MKTLEV     NO TOTALS                                    
*                                                                               
IMKTX    B     XIT                                                              
         SPACE 2                                                                
OMKT1    ST    R3,AMKT                                                          
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0                                          
         GOTO1 GETMKTNM                                                         
         CLI   SBQMKTWT,C'N'                                                    
         BE    OMKT12                                                           
         GOTO1 GETMKTWT                                                         
*                                                                               
OMKT12   MVI   0(R3),C' '                                                       
         MVC   1(4,R3),SBMKT                                                    
         MVI   5(R3),C' '                                                       
         MVC   6(23,R3),SBMKTNM                                                 
         B     XIT                                                              
         EJECT                                                                  
* MARKET OUTPUT ROUTINE FOR DETAIL AND DAYPART SUMMARY REPORT                   
*                                                                               
OMKT2    LA    R4,MKTHEAD          FORMAT MKT HEAD TO TEMP SAVE AREA            
         MVC   MKTHEAD,SPACES                                                   
         MVC   0(6,R4),=C'MARKET'                                               
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0                                          
         MVC   7(4,R4),SBMKT                                                    
         GOTO1 GETMKTNM                                                         
         MVC   12(L'SBMKTNM,R4),SBMKTNM                                         
         CLI   2(R2),C'O'                                                       
         BNE   *+10                                                             
         MVC   12+1+L'SBMKTNM(6,R4),=C'*ORIG*'                                  
         CLI   2(R2),C'S'                                                       
         BNE   *+10                                                             
         MVC   12+1+L'SBMKTNM(7,R4),=C'*SPILL*'                                 
         LA    R1,L'MKTHEAD                                                     
         ST    R1,DMCB+4                                                        
         GOTO1 SQUASHER,DMCB,(R4)                                               
         B     XIT                                                              
         EJECT                                                                  
* GOAL DEMO AND COST OUTPUT ROUTINES FOR MARKET SUMMARY REPORT                  
*                                                                               
ODEMO1   CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    ODEMO12                                                          
         TM    OUTIND,OUTICRMK     YES-TEST ACROSS MARKETS NOW                  
         BZ    ODEMO12                                                          
         OC    4(4,R2),4(R2)       YES-TEST DEMO IS WEIGHTED                    
         BZ    ODEMO12                                                          
         BAS   RE,UNWEIGHT         YES-UNWEIGHT                                 
*                                                                               
ODEMO12  MVC   GLDEM,0(R2)                                                      
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
*                                                                               
OCOST    MVC   GLDOL,0(R2)                                                      
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 2                                                                
UNWEIGHT LR    R0,RE                                                            
         SR    RF,RF                                                            
         ICM   R1,15,TOTWGT                                                     
         BZ    UNWGT2                                                           
         ICM   RF,15,4(R2)                                                      
         BZ    UNWGT2                                                           
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
UNWGT2   ST    RF,0(R2)                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* SPOTS AND DEMO OUTPUT ROUTINES FOR BUYLINE DETAIL REPORT                      
*                                                                               
OSPT     MVC   NSPOTS,0(R2)                                                     
         EDIT  NSPOTS,(5,(R3))                                                  
         B     XIT                                                              
*                                                                               
ODEMO2   ICM   R5,15,0(R2)                                                      
         BZ    XIT                                                              
         CLC   NSPOTS,=F'1'        TEST N'SPOTS = 1                             
         BE    ODEMO22                                                          
         OC    NSPOTS,NSPOTS                                                    
         BZ    ODEMO22                                                          
         SR    R4,R4               NO-UNWEIGHT DEMO BY N'SPOTS                  
         SLL   R5,1                                                             
         D     R4,NSPOTS                                                        
         A     R5,=F'1'                                                         
         SRL   R5,1                                                             
ODEMO22  EDIT  (R5),(7,(R3)),1                                                  
         B     XIT                                                              
         EJECT                                                                  
* INDEX OUTPUT ROUTINES                                                         
*                                                                               
         SPACE 1                                                                
ONDXP    LA    RE,2                PURCH VS GOAL POINTS                         
         OC    GLDEM,GLDEM         TEST GOAL DEMO = 0                           
         BZ    ONDXP2              YES-INDEX=HIGH                               
         ZAP   DUB,0(8,R2)                                                      
         CVB   R1,DUB              R1=POINTS INDEX                              
         SR    RE,RE                                                            
         C     R1,LOLIMP           COMPARE TO LOW LIMIT                         
         BNH   ONDXP2                                                           
         LA    RE,1(RE)                                                         
         C     R1,HILIMP           COMPARE TO HIGH LIMIT                        
         BL    ONDXP2                                                           
         LA    RE,1(RE)                                                         
ONDXP2   STC   RE,PLIM                                                          
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
ONDXC    LA    RE,2                PURCH VS GOAL DOLLARS                        
         OC    GLDOL,GLDOL         TEST GOAL COST = 0                           
         BZ    ONDXC2              YES-INDEX=HIGH                               
         ZAP   DUB,0(8,R2)                                                      
         CVB   R1,DUB              R1=COST INDEX                                
         SR    RE,RE                                                            
         C     R1,LOLIMC           COMPARE TO LOW LIMIT                         
         BNH   ONDXC2                                                           
         LA    RE,1(RE)                                                         
         C     R1,HILIMC           COMPARE TO HIGH LIMIT                        
         BL    ONDXC2                                                           
         LA    RE,1(RE)                                                         
ONDXC2   STC   RE,CLIM                                                          
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         EJECT                                                                  
* STATUS OUTPUT ROUTINES                                                        
*                                                                               
OSTATC   MVC   0(6,R3),SPACES      COST STATUS                                  
         TM    OUTIND,OUTICRMK     TEST TOTALING NOW                            
         BO    OSTATC2             YES                                          
         AP    MKTCNT,=P'1'        NO-INCREMENT MARKET COUNT                    
         XC    BFREC,BFREC                                                      
         MVC   BFPRD,SBBPRD                                                     
         MVC   BFEST,SBBEST                                                     
         MVC   BFMKT,SBBMKT                                                     
         BAS   RE,BUFHI            FIND COMPLETION DATE                         
         BNE   *+14                                                             
         CLC   BFKEY,SVBFKEY                                                    
         BE    *+14                                                             
         MVC   BFKEY,SVBFKEY                                                    
         BAS   RE,GETSTAT          ***** FOR NOW                                
         MVC   SVDATE,BFDATE                                                    
         OC    SVDATE,SVDATE       TEST COMPLETE                                
         BNZ   OSTATC1                                                          
         AP    IMKTCNT,=P'1'       NO                                           
         MVC   0(7,R3),=C'*INCOMP'                                              
         B     XIT                                                              
OSTATC1  AP    CMKTCNT,=P'1'       YES                                          
*                                                                               
OSTATC2  MVC   0(2,R3),=C'OK'      WITHIN TOLERANCE                             
         MVI   MKTSTAT,0                                                        
         CLI   CLIM,1                                                           
         BE    XIT                                                              
         MVI   MKTSTAT,FF                                                       
         MVC   0(4,R3),=C'OVER'    OVER TOLERANCE                               
         LA    R1,MKTCOV                                                        
         CLI   CLIM,2                                                           
         BE    OSTATADD                                                         
         MVC   0(5,R3),=C'UNDER'   UNDER TOLERANCE                              
         LA    R1,MKTCUN                                                        
         CLI   CLIM,0                                                           
         BE    OSTATADD                                                         
         DC    H'0'                                                             
*                                                                               
*                                                                               
OSTATP   MVC   0(6,R3),SPACES      POINTS STATUS                                
         TM    OUTIND,OUTICRMK     TEST TOTALING NOW                            
         BO    OSTATP2             YES                                          
         OC    SVDATE,SVDATE       NO-TEST MARKET COMPLETE                      
         BNZ   OSTATP2                                                          
         MVC   0(5,R3),=C'LETE*'                                                
         B     XIT                                                              
*                                                                               
OSTATP2  MVC   0(2,R3),=C'OK'      WITHIN TOLERANCE                             
         CLI   PLIM,1                                                           
         BE    XIT                                                              
         MVI   MKTSTAT,FF                                                       
         MVC   0(4,R3),=C'OVER'    OVER TOLERANCE                               
         LA    R1,MKTPOV                                                        
         CLI   PLIM,2                                                           
         BE    OSTATADD                                                         
         MVC   0(5,R3),=C'UNDER'   UNDER TOLERANCE                              
         LA    R1,MKTPUN                                                        
         CLI   PLIM,0                                                           
         BE    OSTATADD                                                         
         DC    H'0'                                                             
*                                                                               
OSTATADD L     RE,GLATHID          TEST TOTALING NOW                            
         CLC   GLLEVEL,GLDETLEV-GLINTD(RE)                                      
         BL    XIT                                                              
         AP    0(2,R1),=P'1'       NO-THEN ADD TO COUNTER                       
         B     XIT                                                              
*                                                                               
*                                                                               
ODLC     TM    OUTIND,OUTICRMK     TEST TOTALING NOW                            
         BO    XIT                 YES-NO PRINT                                 
         OC    SVDATE,SVDATE       TEST MARKET COMPLETE                         
         BNZ   *+16                                                             
         L     R1,AMKT             NO-MARK MARKET AS INCOMPLETE                 
         MVI   0(R1),C'*'                                                       
         B     XIT                                                              
         CLI   MKTSTAT,0           YES-ADD TO OK/BAD COUNTERS                   
         BNE   *+14                                                             
         AP    MKTOK,=P'1'                                                      
         B     *+10                                                             
         AP    MKTBAD,=P'1'                                                     
         GOTO1 DATCON,DMCB,(2,SVDATE),(5,0(R3))                                 
         B     XIT                                                              
         EJECT                                                                  
* ESTIMATE FIRST                                                                
*                                                                               
FRSTEST  ZAP   MKTCNT,=P'0'        ZAP THE MARKET COUNTS                        
         ZAP   IMKTCNT,=P'0'                                                    
         ZAP   CMKTCNT,=P'0'                                                    
         ZAP   MKTOK,=P'0'                                                      
         ZAP   MKTBAD,=P'0'                                                     
         ZAP   MKTPOV,=P'0'                                                     
         ZAP   MKTPUN,=P'0'                                                     
         ZAP   MKTCOV,=P'0'                                                     
         ZAP   MKTCUN,=P'0'                                                     
         OI    OUTIND,255-OUTICRMK    NOT ACROSS MARKETS                        
         B     XIT                                                              
         SPACE 2                                                                
* ESTIMATE TOTAL                                                                
*                                                                               
TOTEST   OI    OUTIND,OUTICRMK     INDICATE ACROSS MARKETS                      
         MVC   0(17,R3),=C'COMPLETE MARKETS='                                   
         LA    R5,17(R3)                                                        
         GOTO1 EDCNT,CMKTCNT                                                    
         LA    R5,198(R3)                                                       
         MVC   0(19,R5),=C'INCOMPLETE MARKETS='                                 
         LA    R5,19(R5)                                                        
         GOTO1 EDCNT,IMKTCNT                                                    
         LA    R5,198+198(R3)                                                   
         MVC   0(14,R5),=C'TOTAL MARKETS='                                      
         LA    R5,14(R5)                                                        
         GOTO1 EDCNT,MKTCNT                                                     
         LA    R5,2(R5)                                                         
         MVC   0(4,R5),=C'CVG='                                                 
         ICM   R1,15,TOTWGT                                                     
         EDIT  (R1),(6,4(R5)),2,ALIGN=LEFT                                      
         LA    R5,3*198(R3)                                                     
         MVC   0(3,R5),=C'OK='                                                  
         LA    R5,3(R5)                                                         
         GOTO1 EDCNT,MKTOK                                                      
         MVI   0(R5),C','                                                       
         MVC   1(4,R5),=C'BAD='                                                 
         LA    R5,5(R5)                                                         
         GOTO1 EDCNT,MKTBAD                                                     
         LA    R5,4*198(R3)                                                     
         MVC   0(6,R5),=C'POINTS'                                               
         MVC   12(6,R5),=C'UNDER='                                              
         LA    R5,18(R5)                                                        
         GOTO1 EDCNT,MKTPUN                                                     
         MVI   0(R5),C','                                                       
         MVC   1(5,R5),=C'OVER='                                                
         LA    R5,6(R5)                                                         
         GOTO1 EDCNT,MKTPOV                                                     
         LA    R5,5*198(R3)                                                     
         MVC   0(4,R5),=C'COST'                                                 
         MVC   12(6,R5),=C'UNDER='                                              
         LA    R5,18(R5)                                                        
         GOTO1 EDCNT,MKTCUN                                                     
         MVI   0(R5),C','                                                       
         MVC   1(5,R5),=C'OVER='                                                
         LA    R5,6(R5)                                                         
         GOTO1 EDCNT,MKTCOV                                                     
         B     XIT                                                              
*                                                                               
EDCNT    CP    0(2,R1),=P'0'                                                    
         BNE   *+14                                                             
         MVI   0(R5),C'0'                                                       
         LA    R5,1(R5)                                                         
         BR    RE                                                               
         EDIT  (P2,(R1)),(3,(R5)),ALIGN=LEFT                                    
         AR    R5,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* DRIVER'S ABOUT TO PUT A SORT RECORD                                           
*                                                                               
PUTSRT   CLC   SBDPT,XFF           TEST DAYPART GROUP TOTAL                     
         BNE   XIT                                                              
         OC    SBDPTGRP,SBDPTGRP                                                
         BZ    XIT                                                              
         CLI   GLRECNO,3           YES-TEST DAYPART SUMMARY                     
         BE    XIT                                                              
         TM    RPTOPT,RPTSUM                                                    
         BO    *+12                                                             
         CLI   GLRECNO,2                                                        
         BE    XIT                                                              
         MVI   GLHOOK,GLDONT       NO-THEN SUPPRESS THIS RECORD                 
         B     XIT                                                              
         EJECT                                                                  
* PRINT A LINE                                                                  
*                                                                               
PRINT    CLI   MKTSW,C'N'          TEST MARKET SUPPRESSED                       
         BNE   XIT                                                              
         MVI   GLHOOK,GLDONT       YES-SUPRESS THE PRINT LINE                   
         B     XIT                                                              
         EJECT                                                                  
* FIRST TIME CONTROLS                                                           
*                                                                               
FIRSTS   CLI   GLRECNO,1           TEST SUMMARY REPORT                          
         BNE   FS1                                                              
         TM    RPTOPT,RPTSUM       (FIRST REPORT COULD BE BUY DETAIL)           
         BZ    FS2                                                              
         MVI   MKTSW,C'Y'                                                       
         MVI   GLSPACE,1                                                        
         OI    GLNORBOX,X'40'      SUPPRESS BOXES IN ROWS FOR TOTALS            
         B     XIT                                                              
*                                                                               
FS1      CLI   GLRECNO,2           TEST BUYLINE DETAILS                         
         BNE   FS3                                                              
         TM    RPTOPT,RPTSUM       (2ND REPORT COULD BE DPT SUMMARY)            
         BZ    FS4                                                              
FS2      MVI   GLSPACE,2                                                        
         NI    GLNORBOX,FF-X'40'                                                
         CLC   MKTLEV,GLARGS       TEST MARKET FIRST                            
         BE    FMKT                                                             
         B     XIT                                                              
*                                                                               
FS3      CLI   GLRECNO,3           TEST DAYPART SUMMARY                         
         BNE   XIT                                                              
FS4      MVI   GLSPACE,1                                                        
         NI    GLNORBOX,FF-X'40'                                                
         CLI   MKTPRT,C'Y'                                                      
         BNE   XIT                                                              
         MVI   MKTSW,C'Y'                                                       
         B     XIT                                                              
*                                                                               
*                                                                               
FMKT     L     R1,GLATHID          MARKET FIRST FOR DETAIL REPORT               
         USING GLINTD,R1                                                        
         ZIC   RE,MKTLEV                                                        
         SLL   RE,2                                                             
         LA    RE,GLARECL0(RE)                                                  
         L     RE,0(RE)            RE=A(MARKET TOTAL RECORD)                    
         DROP  R1                                                               
         L     R1,LGRPS                                                         
         AR    RE,R1                                                            
         MVC   GLDOL,DGLDOL(RE)    EXTRACT VALUES FROM TOTAL RECORD             
         MVC   GLDEM,DGLDEM(RE)                                                 
         MVC   BYDOL,DBYDOL(RE)                                                 
         MVC   BYPDEM,DBYPDEM(RE)                                               
*                                                                               
         MVC   MKTSUML1,SPACES                                                  
         MVC   MKTSUML1+11(20),=C'GOAL PURCHASED INDEX'                         
         MVC   MKTSUML2,SPACES                                                  
         MVC   MKTSUML2(4),=C'GRPS'                                             
         MVC   MKTSUML3,SPACES                                                  
         MVC   MKTSUML3(7),=C'DOLLARS'                                          
*                                                                               
*                                  EDIT THE VALUES                              
         LA    R2,MKTSUML2+8                                                    
         EDIT  GLDEM,(7,(R2)),1    GOAL DEMO                                    
         LA    R2,MKTSUML2+18                                                   
         EDIT  BYPDEM,(7,(R2)),1   PURCHASED DEMO                               
         L     R0,BYPDEM                                                        
         L     RF,GLDEM                                                         
         BAS   RE,INDEX                                                         
         LA    R2,MKTSUML2+26                                                   
         BAS   RE,EDINDEX          INDEX PURCHASED V GOAL                       
         MVI   MKTPRT,C'N'                                                      
         C     R1,LOLIMP           TEST WITHIN TOLERENCE                        
         BNH   FMKT1               NO                                           
         C     R1,HILIMP                                                        
         BL    FMKT2               YES                                          
*                                                                               
FMKT1    MVI   5(R2),C'*'          MARK OUTSIDE TOLERANCE                       
         MVI   MKTPRT,C'Y'         PRINT THE MARKET                             
         B     FMKT3                                                            
*                                  INSIDE TOLERANCE-                            
FMKT2    CLI   ALLMKTS,C'Y'        TEST INCLUDE ALL MARKETS                     
         BNE   *+8                 NO                                           
         MVI   MKTPRT,C'Y'         YES                                          
*                                                                               
FMKT3    L     R0,GLDOL                                                         
         BAS   RE,ROUND                                                         
         LA    R2,MKTSUML3+8                                                    
         BAS   RE,EDITDOL          GOAL DOLLARS                                 
         L     R0,BYDOL                                                         
         BAS   RE,ROUND                                                         
         LA    R2,MKTSUML3+18                                                   
         BAS   RE,EDITDOL          PURCHASED DOLLARS                            
         L     R0,BYDOL                                                         
         L     RF,GLDOL                                                         
         BAS   RE,INDEX                                                         
         LA    R2,MKTSUML3+26                                                   
         BAS   RE,EDINDEX          INDEX PURCH V GOAL                           
         C     R1,LOLIMC           TEST WITHIN TOLERENCE                        
         BNH   FMKT4               NO                                           
         C     R1,HILIMC                                                        
         BL    FMKT5               YES                                          
*                                                                               
FMKT4    MVI   5(R2),C'*'          MARK OUTSIDE TOLERANCE                       
         MVI   MKTPRT,C'Y'         PRINT THE MARKET                             
*                                                                               
FMKT5    MVC   MKTSW,MKTPRT                                                     
*                                                                               
*                                  GET THE AGENCY NAME                          
         XC    KEY,KEY                                                          
         LA    R5,KEY              GET FIRST BUY RECORD WITH NON-ZERO           
         USING BUYRECD,R5          INTERFILE AGENCY NUMBER                      
         MVC   BUYKAM,SBBAGYMD                                                  
         MVC   BUYKCLT,SBBCLT                                                   
         MVC   BUYKPRD,SBBPRD                                                   
         MVC   BUYMSTA(2),SBBMKT                                                
*                                                                               
FMKT6    GOTO1 HIGH                                                             
         B     FMKT8                                                            
*                                                                               
FMKT7    GOTO1 SEQ                                                              
*                                                                               
FMKT8    LA    R5,KEY                                                           
         CLC   KEY(6),KEYSAVE                                                   
         BNE   FMKT12                                                           
         CLC   BUYKEST,SBBEST                                                   
         BE    FMKT9                                                            
         BL    *+14                                                             
         MVC   BUYKEST(4),XFF                                                   
         B     FMKT6                                                            
         MVC   BUYKEST,SBBEST                                                   
         XC    BUYKBUY,BUYKBUY                                                  
         B     FMKT6                                                            
*                                                                               
FMKT9    L     R5,SBAIO3                                                        
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         CLI   BDXFRAGY,0                                                       
         BE    FMKT7                                                            
         MVC   BYTE,BDXFRAGY       FOUND                                        
         OC    AGYCLIST(4),AGYCLIST    TEST CLT=XXX RECORD READ YET             
         BNZ   FMKT10                                                           
         XC    KEY,KEY             NO-                                          
         LA    R5,KEY                                                           
         USING CLTHDRD,R5                                                       
         MVC   CKEYAM,SBBAGYMD                                                  
         MVC   CKEYCLT,=X'DEF7'    CLT=XXX                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,SBAIO3                                                        
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   AGYCLIST(220),CLIST                                              
         MVC   AGYCLIST+220(220),CLIST+220                                      
         MVC   AGYCLIST+440(220),CLIST+440                                      
         MVC   AGYCLIST+660(220),CLIST+660                                      
*                                                                               
FMKT10   LA    R0,220              FIND THE AGENCY                              
         LA    R1,AGYCLIST                                                      
*                                                                               
FMKT11   CLC   BYTE,3(R1)                                                       
         BE    *+16                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,FMKT11                                                        
         B     FMKT12                                                           
         CLC   AGCODE,0(R1)        FOUND-TEST CHANGE                            
         BE    FMKTX               NO                                           
         MVC   AGCODE,0(R1)        YES-SAVE AGENCY 3-BYTE CODE                  
         XC    KEY,KEY                 READ CLIENT HDR FOR NAME                 
         LA    R5,KEY                                                           
         USING CLTHDRD,R5                                                       
         MVC   CKEYAM,SBBAGYMD                                                  
         GOTO1 CLPACK,DMCB,AGCODE,CKEYCLT                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FMKT13                                                           
         L     R5,SBAIO3                                                        
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   AGNAME,CNAME        EXTRACT THE AGENCY NAME                      
         B     FMKTX                                                            
         DROP  R5                                                               
*                                                                               
FMKT12   MVC   AGCODE,=C'???'                                                   
FMKT13   MVC   AGNAME,SPACES                                                    
         MVC   AGNAME(9),=C'*UNKNOWN*'                                          
*                                                                               
FMKTX    B     XIT                                                              
*                                                                               
*                                                                               
EDITDOL  LTR   R1,R1                                                            
         BNZ   *+10                                                             
         MVI   6(R2),C'0'                                                       
         BR    RE                                                               
         EDIT  (R1),(7,(R2))                                                    
         BR    RE                                                               
*                                                                               
EDINDEX  DS    0H                                                               
         CH    R1,=H'10000'                                                     
         BL    *+12                                                             
         MVC   1(4,R2),=C'HIGH'                                                 
         BR    RE                                                               
         EDIT  (R1),(5,(R2)),1                                                  
         BR    RE                                                               
*                                                                               
INDEX    LH    R1,=H'10000'                                                     
         LTR   RF,RF                                                            
         BZR   RE                                                               
         SRDA  R0,32                                                            
         M     R0,=F'2000'                                                      
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
ROUND    SRDA  R0,32                                                            
         SLDA  R0,1                                                             
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
* DRIVER HOOK LASTS                                                             
*                                                                               
LASTS    TM    RPTOPT,RPTSUM       TEST BUYLINE DETAILS REPORT                  
         BZ    *+16                                                             
         CLI   GLRECNO,2                                                        
         BE    *+16                                                             
         B     XIT                                                              
         CLI   GLRECNO,1                                                        
         BNE   XIT                                                              
         CLC   MKTLEV,GLARGS       AND MARKET LAST                              
         BNE   XIT                                                              
         MVI   MKTSW,C'N'          YES-SUPPRESS MARKET TOTAL LINE               
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HEADHOOK                                                               
*                                                                               
HEAD     LA    R5,HEADTAB                                                       
         USING HEADTABD,R5                                                      
*                                                                               
HD2      CLI   0(R5),0                                                          
         BE    HD10                                                             
         ZIC   RE,GLRECNO                                                       
         TM    RPTOPT,RPTSUM                                                    
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CLC   HDREP,BYTE          DRIVER REPORT NUMBER                         
         BNE   *+14                                                             
         CLC   HDNMGR,GLOPTS+1     N'MARKET GROUPS                              
         BE    *+12                                                             
         LA    R5,HEADTABL(R5)                                                  
         B     HD2                                                              
         SR    R2,R2                                                            
         ICM   R2,1,HDMGR1                                                      
         BZ    HD4                                                              
         LA    R3,DREGMGR                                                       
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR1HEAD,RF),MGR1HEAD                                        
         ICM   R2,1,HDMGR2                                                      
         BZ    HD4                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR2HEAD,RF),MGR2HEAD                                        
         ICM   R2,1,HDMGR3                                                      
         BZ    HD4                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR3HEAD,RF),MGR3HEAD                                        
*                                                                               
HD4      ICM   R2,1,HDMKT                                                       
         BZ    HD6                                                              
         LA    R3,DREGMKT                                                       
         BAS   RE,HDPOS                                                         
         MVC   0(L'MKTHEAD,RF),MKTHEAD                                          
*                                                                               
HD6      ICM   R2,1,HDSUM                                                       
         BZ    HD10                                                             
         LA    R3,DREGSUM                                                       
         BAS   RE,HDPOS                                                         
         MVC   0(L'MKTSUML1,RF),MKTSUML1                                        
         A     RF,PWIDTH                                                        
         MVC   0(L'MKTSUML2,RF),MKTSUML2                                        
         A     RF,PWIDTH                                                        
         MVC   0(L'MKTSUML3,RF),MKTSUML3                                        
*                                                                               
HD10     L     R2,AH4                                                           
         A     R2,PWIDTH                                                        
         A     R2,PWIDTH                                                        
         A     R2,PWIDTH           R2=A(HEAD7)                                  
         LA    R3,96               R3=DISPLACEMENT TO RHS                       
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R3,128                                                           
         LA    R5,0(R3,R2)         CLEAR RHS OF HEAD7                           
         MVC   0(34,R5),SPACES                                                  
         MVC   0(16,R5),=C'PNTS TOLERANCE ='                                    
         EDIT  PCTTOLP,(5,17(R5)),1,ALIGN=LEFT,TRAIL=C'%'                       
         A     R5,PWIDTH           CLEAR RHS OF HEAD8                           
         MVC   0(34,R5),SPACES                                                  
         MVC   0(16,R5),=C'COST TOLERANCE ='                                    
         EDIT  PCTTOLC,(5,17(R5)),1,ALIGN=LEFT,TRAIL=C'%'                       
*                                                                               
         CLI   GLRECNO,1           PRINT AGENCY FOR DETAIL REPORTS              
         BNE   *+12                                                             
         TM    RPTOPT,RPTSUM                                                    
         BO    HDX                                                              
         A     R5,PWIDTH           PRINT THE AGENCY ON HEAD9                    
         MVC   0(6,R5),=C'AGENCY'                                               
         MVC   7(3,R5),AGCODE                                                   
         MVC   11(20,R5),AGNAME                                                 
         B     HDX                                                              
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
* ROUTINE TO GET THE STATUS RECORD                                              
* OUTPUT  : CC EQ - RECORD FOUND AND BFDATE SET TO BUY COMPLETION DATE          
*         : CC NE - BUY COMPLETION DATE NOT FOUND                               
*                                                                               
GETSTAT  NTR1                                                                   
         XC    BFDATE,BFDATE                                                    
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING STATD,R5                                                         
         MVC   STKTYPE,=X'0D71'                                                 
         MVC   STKAGMD,SBBAGYMD                                                 
         MVC   STKCLT,SBBCLT                                                    
         MVC   STKPRD,BFPRD                                                     
         MVC   STKEST,BFEST                                                     
         MVC   STKMKT,BFMKT                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST RECORD FOUND                            
         BNE   GETSTNO             NO                                           
         L     R5,SBAIO3           YES-GET THE RECORD                           
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,24(R5)           LOOK FOR CAMPAIGN STATUS ELEMENT             
         SR    R0,R0                                                            
*                                                                               
GETST2   CLI   0(R5),0                                                          
         BE    GETSTNO             NOT FOUND-INCOMPLETE                         
         CLI   0(R5),CSCODEQ                                                    
         BE    *+14                                                             
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETST2                                                           
         MVC   BFDATE,CSBUY-CSELEM(R5)   COMPLETION DATE = BUY DATE             
*                                                                               
GETSTYES CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
GETSTNO  CLC   SBESTNDP,=X'B2D9'   TEST ESTIMATE END AFTER 6/25/89              
         BH    *+14                                                             
         MVC   BFDATE,=X'B2D9'     NO-FORCE COMPLETION DATE                     
         B     GETSTYES                                                         
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* ROUTINE TO INITIALIZE BUFFALO                                                 
*                                                                               
INITBUFF NTR1                                                                   
         L     R4,=A(BUFFALOC)                                                  
         B     INITBF2                                                          
         GOTO1 COVAIL,DMCB,C'LOOK'                                              
         DC    H'0'                                                             
*                                                                               
INITBF2  GOTO1 COVAIL,DMCB,C'SETB',20000,400000,(R4)                            
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ABUFF,4(R1)                                                      
         MVC   BUFFL,8(R1)                                                      
         MVC   ABUFFC,12(R1)                                                    
         L     R1,ABUFFC                                                        
         USING BUFFALOD,R1                                                      
         MVI   BUFFDOPT,C'Y'       OPTION TO OVERRIDE COMMENT (DATE)            
         DROP  R1                                                               
         BAS   RE,BUFSET                                                        
         B     XIT                                                              
         EJECT                                                                  
* FINALIZATION                                                                  
*                                                                               
FINAL    ICM   R1,15,BUFFL         TEST BUFFALO INITIALIZED                     
         BZ    XIT                 NO                                           
         ST    R1,DMCB+8           YES-RELEASE BUFFALO STORAGE                  
         GOTO1 COVAIL,DMCB,C'FREE',ABUFF                                        
         B     XIT                                                              
         EJECT                                                                  
* BUFFALO ROUTINES                                                              
*                                                                               
BUFSET   LA    R1,=C'SET'                                                       
         B     BUFX                                                             
*                                                                               
BUFRSET  LA    R1,=C'RESET'                                                     
         B     BUFX                                                             
*                                                                               
BUFPUT   LA    R1,=C'PUT'                                                       
         B     BUFX                                                             
*                                                                               
BUFHI    MVC   SVBFKEY,BFKEY                                                    
         LA    R1,=C'HIGH'                                                      
         B     BUFX                                                             
*                                                                               
BUFSEQ   LA    R1,=C'SEQ'                                                       
         B     BUFX                                                             
*                                                                               
BUFX     NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 BUFFALO,DMCB,,ABUFFC,BFREC,1                                     
         TM    8(R1),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
ABUFFC   DS   A                                                                 
ABUFF    DS   A                                                                 
BUFFL    DS   F                                                                 
*                                                                               
AMKT     DS   A                                                                 
*                                                                               
GLDOL    DS   F                                                                 
GLDEM    DS   F                                                                 
BYDOL    DS   F                                                                 
BYPDEM   DS   F                                                                 
*                                                                               
LGRPS    DS   F                                                                 
NSPOTS   DS   F                                                                 
LOLIMP   DS   F                    LOWER POINTS INDEX TOLERENCE                 
HILIMP   DS   F                    UPPER POINTS INDEX TOLERENCE                 
LOLIMC   DS   F                    LOWER COST INDEX TOLERENCE                   
HILIMC   DS   F                    UPPER COST INDEX TOLERENCE                   
PCTTOLC  DS   H                                                                 
PCTTOLP  DS   H                                                                 
*                                                                               
MGR1HEAD DS    CL43                                                             
MGR2HEAD DS    CL43                                                             
MGR3HEAD DS    CL43                                                             
MKTHEAD  DS    CL44                                                             
MKTSUML1 DS    CL32                                                             
MKTSUML2 DS    CL32                                                             
MKTSUML3 DS    CL32                                                             
*                                                                               
AGCODE   DS    CL3                                                              
AGNAME   DS    CL20                                                             
*                                                                               
PLIM     DS    XL1                 LIMIT INDICATORS                             
CLIM     DS    XL1                 0=UNDER,1=WITHIN,2=OVER                      
ALLMKTS  DS    CL1                 Y=DETAILS FOR ALL MARKETS                    
RPTOPT   DS    XL1                 REPORT OPTION                                
RPTSUM   EQU   X'80'                                                            
RPTDET   EQU   X'40'                                                            
*                                                                               
MKTCNT   DS    PL2                 MARKET COUNTS                                
IMKTCNT  DS    PL2                                                              
CMKTCNT  DS    PL2                                                              
MKTOK    DS    PL2                                                              
MKTBAD   DS    PL2                                                              
MKTPUN   DS    PL2                                                              
MKTPOV   DS    PL2                                                              
MKTCUN   DS    PL2                                                              
MKTCOV   DS    PL2                                                              
*                                                                               
MKTSTAT  DS    XL1                                                              
MKTSW    DS    CL1                                                              
MKTPRT   DS    CL1                                                              
*                                                                               
CMPDTLST DS    XL(MAXPRDS*3+1)                                                  
MAXPRDS  EQU   10                                                               
*                                                                               
BFREC    DS    0CL10                                                            
BFKEY    DS    0CL4                                                             
BFPRD    DS    XL1                                                              
BFEST    DS    XL1                                                              
BFMKT    DS    XL2                                                              
BFCOM    DS    0CL2                                                             
BFDATE   DS    XL2                                                              
BFACCUM  DS    XL4                                                              
*                                                                               
SVBUYKEY DS    XL13                                                             
SVBFKEY  DS    CL(L'BFKEY)                                                      
SVDATE   DS    XL2                                                              
SVBPRD   DS    XL1                                                              
SVBEST   DS    XL1                                                              
*                                                                               
PATCH    DC    XL32'00'                                                         
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
* DRIVER MARKET TOTAL RECORD DISPLACEMENTS                                      
*                                                                               
DGLDOL   EQU   66                                                               
DGLDEM   EQU   70                                                               
DBYDOL   EQU   78                                                               
DBYPDEM  EQU   90                                                               
         EJECT                                                                  
* HEADLINE POSITION TABLE                                                       
*                                                                               
HEADTAB  DC    X'01',X'00',X'000000',X'00',X'00'                                
         DC    X'01',X'01',X'050000',X'00',X'00'                                
         DC    X'01',X'02',X'050600',X'00',X'00'                                
         DC    X'01',X'03',X'050607',X'00',X'00'                                
         DC    X'02',X'00',X'000000',X'05',X'07'                                
         DC    X'02',X'01',X'050000',X'06',X'08'                                
         DC    X'02',X'02',X'040500',X'06',X'08'                                
         DC    X'02',X'03',X'040506',X'07',X'08'                                
         DC    X'03',X'00',X'000000',X'05',X'07'                                
         DC    X'03',X'01',X'050000',X'06',X'08'                                
         DC    X'03',X'02',X'040500',X'06',X'08'                                
         DC    X'03',X'03',X'040506',X'07',X'08'                                
         DC    X'00'                                                            
         SPACE 1                                                                
HEADTABD DSECT                                                                  
HDREP    DS    X                   REPORT NUMBER                                
HDNMGR   DS    X                   N'MARKET GROUPS                              
HDMGR1   DS    X                   HEADLINE FOR MARKET GROUP 1                  
HDMGR2   DS    X                   HEADLINE FOR MARKET GROUP 2                  
HDMGR3   DS    X                   HEADLINE FOR MARKET GROUP 3                  
HDMKT    DS    X                   HEADLINE FOR MARKET                          
HDSUM    DS    X                   HEADLINE FOR MARKET SUMMARY                  
HEADTABL EQU   *-HEADTABD                                                       
         SPACE 1                                                                
* HEADLINE DISPLACEMENTS                                                        
*                                                                               
DWIDMGR  EQU   64                  WIDE-    MARKET GROUP                        
DWIDMKT  EQU   67                           MARKET                              
DWIDSUM  EQU   64                           SUMMARY                             
DREGMGR  EQU   48                  REGULAR- MARKET GROUP                        
DREGMKT  EQU   51                           MARKET                              
DREGSUM  EQU   48                           SUMMARY                             
         EJECT                                                                  
T20407   CSECT                                                                  
*                                                                               
AGYCLIST DS    CL(L'CLIST)                                                      
         SPACE 2                                                                
* BUFFALO CSECT                                                                 
*                                                                               
         BUFF  LINES=1,FLAVOR=BINARY,KEYLIST=(4,A),COMMENT=2,COLUMNS=1,+        
               ROWS=1                                                           
         EJECT                                                                  
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    A                                                                
         DS    A                                                                
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPGENAGY                                                                       
*SPGENCLT                                                                       
*SPGENBUY                                                                       
*SPGENGOAL                                                                      
*SPGENSTAT                                                                      
*SPWRIFFD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENSTAT                                                      
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE5D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPWRI07   12/06/04'                                      
         END                                                                    
