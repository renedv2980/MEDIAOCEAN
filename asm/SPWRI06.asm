*          DATA SET SPWRI06    AT LEVEL 065 AS OF 12/06/04                      
*PHASE T20406A                                                                  
         TITLE 'T20406 - CCUSA MARKET SUMMARY STATUS REPORT'                    
T20406   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20406,RA                                                  
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
         OI    DATAIND2,DIEST      ESTIMATE                                     
         CLC   SBQPRD,=C'ALL'      TEST PRD=ALL                                 
         BE    *+8                 YES-DON'T PRINT ESTIMATE NAME                
         OI    DATAIND3,DIESTNM    ESTIMATE NAME                                
         MVI   DEMOPT,DEMOTGT      TARGET DEMOS ONLY                            
         XC    POLDEM,POLDEM                                                    
         XC    BUFFL,BUFFL                                                      
         XC    SVBFKEY,SVBFKEY                                                  
         XC    SVBPRD,SVBPRD                                                    
         XC    SVBEST,SVBEST                                                    
         XC    CONESTAB,CONESTAB                                                
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
         XC    RPTLEVS+4(3),RPTLEVS+4                                           
         B     INIT4                                                            
         CLC   SBMGR1LN,SBMGR2LN                                                
         BNE   *+14                                                             
         XC    RPTLEVS+5(2),RPTLEVS+5                                           
         B     INIT4                                                            
         CLC   SBMGR2LN,SBMGR3LN                                                
         BNE   INIT4                                                            
         MVI   RPTLEVS+6,0                                                      
*                                                                               
INIT4    LA    R1,LEVELS           SET THE LEVELS                               
         LA    RE,RPTLEVS                                                       
         LA    RF,1                                                             
         SR    R2,R2                                                            
INIT6    CLI   0(RE),X'FF'                                                      
         BE    INIT10                                                           
         CLI   0(RE),0                                                          
         BE    INIT8                                                            
         MVC   0(1,R1),0(RE)                                                    
         CLI   0(R1),QMKT                                                       
         BNE   *+16                                                             
         STC   RF,MIDLEV           MIDLINE                                      
         STC   RF,MKTLEV           MARKET LEVEL                                 
         STC   R2,LSTHEDLV         LEVEL ABOVE IS LAST HEADLINE                 
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
INIT8    LA    RE,1(RE)                                                         
         B     INIT6                                                            
*                                                                               
INIT10   MVI   MYFIRSTH,11         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
         MVC   PCTTOLC,=H'100'     DEFAULT TOLERENCES ARE 10PCT                 
         MVC   PCTTOLP,=H'100'                                                  
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INIT20                                                           
*                                                                               
         LA    R2,CS1PTLH          YES-VALIDATE OPTIONS                         
         LA    R5,PCTTOLP          POINTS TOLERENCE                             
         BAS   RE,VALPCT                                                        
         LA    R2,CS1CTLH          COST TOLERANCE                               
         LA    R5,PCTTOLC                                                       
         BAS   RE,VALPCT                                                        
*                                                                               
         MVI   WIDTHOPT,0          MEDIA COMMENTS OPTION                        
         LA    R2,CS1COMH                                                       
         CLI   5(R2),0                                                          
         BE    INIT12                                                           
         CLI   8(R2),C'N'                                                       
         BE    INIT12                                                           
         CLI   8(R2),C'Y'          REPORT WIDE FOR MEDIA COMMENTS               
         BNE   EINV                                                             
         MVI   WIDTHOPT,C'W'                                                    
*                                                                               
INIT12   LA    R2,CS1TITH          TITLE                                        
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
INITX    B     XIT                                                              
         SPACE 2                                                                
RPTLEVS  DC    AL1(QCLT)           HEADLINES                                    
         DC    AL1(QPRDGR1)                                                     
         DC    AL1(QPRDGR2)                                                     
         DC    AL1(QEST)                                                        
         DC    AL1(QMKTGR1)                                                     
         DC    AL1(QMKTGR2)                                                     
         DC    AL1(QMKTGR3)                                                     
         DC    AL1(QMKT)           MIDLINE                                      
         DC    AL1(QPRD)           DETAIL                                       
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
VALID    LA    R2,CS1CLTH                                                       
         CLC   SBQCLT,=C'CC '                                                   
         BNE   ECLT                                                             
         LA    R2,CS1PRDH                                                       
         OC    SBQDEMOS,SBQDEMOS   TEST DEMO MENU                               
         BNZ   *+14                YES                                          
         CLC   SBQPRD,=C'ALL'      NO-TEST PRD=ALL REQUEST                      
         BE    EINV                YES-INVALID                                  
         MVI   CHKDEMSW,C'N'                                                    
         CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BNE   VALID2              NO                                           
         MVI   SBQBPRD,0           YES-BREAK OUT THE PRODUCTS                   
         OI    SBQPIND,SBQPOLSP                                                 
         OC    SBQDEMOS,SBQDEMOS   TEST DEMO MENU                               
         BNZ   VALID2                                                           
         MVI   CHKDEMSW,C'Y'       NO-CHECK DEMO FOR GOALS                      
*                                                                               
VALID2   MVI   QESTIM,C'A'                                                      
         CLI   SBQSEPES,C'Y'                                                    
         BE    XIT                                                              
         CLC   SBQEST,SBQESTND                                                  
         BE    XIT                                                              
         MVI   QESTIM,C'N'                                                      
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
ESTIMATE CLI   CHKDEMSW,C'Y'       TEST GOAL DEMO WILL HAVE TO BE               
         BNE   ESTX                CHECKED                                      
         TM    DATAIND3,DIESTNM    YES-TEST ESTIMATE DETAILS PUT BY             
         BO    EST2                    SPWRI01                                  
         GOTO1 PUTESTNM                NO-PUT THEM HERE                         
*                                                                               
EST2     CLI   SBBPRD,X'FF'        TEST PRD=POL                                 
         BNE   ESTX                                                             
         OC    POLDEM,POLDEM       YES-TEST POL CONTROL ESTIMATE DEMO           
         BNZ   ESTX                    SET YET                                  
         MVC   POLDEM,SBESTDEM                                                  
         B     ESTX                                                             
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
         BE    GL0                                                              
         MVC   SBBPRD,GKEYPRD      YES-GET ESTIMATE'S DEMOS AND DATES           
         MVC   SVBPRD,GKEYPRD                                                   
         MVC   SBBEST,GKEYEST                                                   
         MVC   SVBEST,GKEYEST                                                   
         GOTO1 GETESTNM                                                         
*                                                                               
GL0      BAS   RE,GETDATE          GET COMPLETION DATE                          
         MVC   SVDATE,BFDATE       SAVE THE DATE                                
         CLI   CHKDEMSW,C'Y'       TEST MUST CHECK GOAL DEMO                    
         BNE   GL1                                                              
         CLC   POLDEM,SBESTDEM     TEST ESTIMATE'S DEMO SAME AS POL             
         BNE   GL4                 CONTROL ESTIMATE'S DEMO                      
*                                                                               
GL1      CLI   QESTIM,C'N'         IF EST NE NO, THEN DO NOT DROP GOALS         
         BNE   GLX                 FOR INCOMPLETE MARKETS                       
         MVI   BYTE,0                                                           
         MVC   SBEPRD,SBQBPRD                                                   
         CLI   SBQBPRD,FF                                                       
         BNE   *+8                                                              
         MVI   SBEPRD,0                                                         
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
* IF NOT, AND IT'S AN EST=NO REQUEST, THEN SBYORN IS SET TO C'N'                
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
SPHKNO   CLI   QESTIM,C'N'         INCOMPLETE-TEST ESTIMATE=NO                  
         BNE   SPHK20              NO-                                          
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
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  CLI   WIDTHOPT,C'W'         OPT3=WIDE OPTION                           
         BNE   *+8                                                              
         MVI   GLOPTS+2,C'Y'         OPT3=WIDE OPTION                           
         B     DRHOOKX                                                          
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
         DC    CL8'IESTUSR ',A(IEST)                                            
         DC    CL8'OMGR1USR',A(OMGR1)                                           
         DC    CL8'OMGR2USR',A(OMGR2)                                           
         DC    CL8'OMGR3USR',A(OMGR3)                                           
         DC    CL8'IPRDUSR ',A(IPRD)                                            
         DC    CL8'OPRDUSR ',A(OPRD)                                            
         DC    CL8'OCOSTUSR',A(OCOST)                                           
         DC    CL8'ODEMOUSR',A(ODEMO)                                           
         DC    CL8'ONDXP   ',A(ONDXP)                                           
         DC    CL8'ONDXC   ',A(ONDXC)                                           
         DC    CL8'OSTATP  ',A(OSTATP)                                          
         DC    CL8'OSTATC  ',A(OSTATC)                                          
         DC    CL8'ODLC    ',A(ODLC)                                            
         DC    CL8'OMCOMUSR',A(OMCOM)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* ESTIMATE INPUT ROUTINE                                                        
*                                                                               
IEST     MVI   0(R2),0                                                          
         MVC   1(1,R2),SBBEST                                                   
         CLI   QESTIM,C'N'         TEST EST=NO                                  
         BNE   *+10                                                             
         MVC   1(1,R2),ESTSTART    YES-ONLY THE CONTROL ESTIMATE                
         MVC   2(1,R2),SBBPRD                                                   
         CLI   SBQBPRD,0                                                        
         BNE   XIT                                                              
         MVI   2(R2),FF                                                         
         B     XIT                                                              
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
* PRODUCT ROUTINES                                                              
*                                                                               
IPRD     MVC   0(3,R2),SBPRD                                                    
         MVC   3(3,R2),SBPRD2                                                   
         CLC   SBPRD,=C'POL'                                                    
         BNE   *+10                                                             
         MVC   0(3,R2),=X'FDFDFD'                                               
         MVI   MAXTOTLV,0                                                       
         CLI   SBMODE,SBPROCGL     TEST PROCESSING GOALS                        
         BNE   IPRD2                                                            
         OC    SVDATE,SVDATE       YES-TEST COMPLETE                            
         BZ    IPRD6                   NO-NO TOTALS                             
         B     IPRDX                                                            
*                                                                               
IPRD2    CLI   SBMODE,SBPROCSP     TEST PROCESSING BUYS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,CMPDTLST         YES-FIND THE PRD'S COMPLETION DATE           
*                                                                               
IPRD4    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SBBPRD,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     IPRD4                                                            
         OC    1(2,R1),1(R1)       TEST COMPLETE                                
         BNZ   IPRDX               YES                                          
*                                                                               
IPRD6    MVC   MAXTOTLV,MKTLEV     NO TOTALS                                    
*                                                                               
IPRDX    B     XIT                                                              
         SPACE 2                                                                
OPRD     ST    R3,APRD                                                          
         MVI   0(R3),C' '                                                       
         MVC   1(3,R3),0(R2)                                                    
         MVI   4(R3),C' '                                                       
         L     R1,SBAPRDBF                                                      
         USING PRDBUFFD,R1                                                      
         LH    RE,=Y(PRDBUFFL)                                                  
         LA    R0,256                                                           
         CLC   PBALPH,0(R2)                                                     
         BE    *+16                                                             
         LA    R1,0(R1,RE)                                                      
         BCT   R0,*-14                                                          
         B     XIT                                                              
         MVC   SBPRD,0(R2)                                                      
         MVC   SBBPRD,PBBCODE                                                   
         MVC   5(L'PBNAME,R3),PBNAME                                            
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
* GOAL COST AND POINTS OUTPUT ROUTINES                                          
*                                                                               
OCOST    MVC   GLDOL,0(R2)                                                      
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
ODEMO    CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    ODEMO2                                                           
         TM    OUTIND,OUTICRMK     YES-TEST ACROSS MARKETS NOW                  
         BZ    ODEMO2                                                           
         OC    4(4,R2),4(R2)       YES-TEST DEMO IS WEIGHTED                    
         BZ    ODEMO2                                                           
         BAS   RE,UNWEIGHT         YES-UNWEIGHT                                 
*                                                                               
ODEMO2   MVC   GLDEM,0(R2)                                                      
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
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
* INDEX OUTPUT ROUTINES                                                         
*                                                                               
         SPACE 1                                                                
ONDXP    LA    RE,2                POINTS INDEX                                 
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
ONDXC    LA    RE,2                COST INDEX                                   
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
         BO    OSTATC5             YES                                          
         XC    SVDATE,SVDATE                                                    
         XC    BFREC,BFREC                                                      
         MVC   BFPRD,SBBPRD                                                     
         MVC   BFEST,SBBEST                                                     
         MVC   BFMKT,SBBMKT                                                     
         BAS   RE,BUFHI            GET BUFFALO RECORD                           
         BE    OSTATC1                                                          
         CLI   QESTIM,C'N'                                                      
         BE    OSTATC4                                                          
         DC    H'0'                                                             
*                                                                               
OSTATC1  CLC   BFKEY,SVBFKEY       TEST BUFFALO RECORD FOUND                    
         BE    OSTATC2             YES                                          
         CLI   QESTIM,C'N'         NO-TEST EST=NO                               
         BE    *+6                                                              
         DC    H'0'                NO-ERROR                                     
         CLC   BFKEY(3),SVBFKEY    YES-TEST MKT/PRD MATCH                       
         BNE   OSTATC4                 NO-DONE                                  
*                                                                               
OSTATC2  CLI   QESTIM,C'N'         TEST EST=NO                                  
         BNE   OSTATC3             NO                                           
         CLC   BFDATE,SVDATE       YES-TEST DATE IS LATEST YET                  
         BNH   *+10                NO                                           
         MVC   SVDATE,BFDATE       YES-SAVE THE DATE                            
         BAS   RE,BUFSEQ           GET NEXT BUFFALO RECORD                      
         BE    OSTATC1                                                          
         B     OSTATC4                                                          
*                                                                               
OSTATC3  MVC   SVDATE,BFDATE       SAVE THE DATE                                
*                                                                               
OSTATC4  OC    SVDATE,SVDATE       TEST FOR COMPLETION DATE                     
         BNZ   OSTATC5             YES                                          
         MVC   0(7,R3),=C'*INCOMP' NO                                           
         BZ    XIT                                                              
*                                                                               
OSTATC5  MVC   0(2,R3),=C'OK'      WITHIN TOLERANCE                             
         CLI   CLIM,1                                                           
         BE    XIT                                                              
         MVC   0(4,R3),=C'OVER'    OVER TOLERANCE                               
         CLI   CLIM,2                                                           
         BE    XIT                                                              
         MVC   0(5,R3),=C'UNDER'   UNDER TOLERANCE                              
         CLI   CLIM,0                                                           
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
OSTATP   MVC   0(6,R3),SPACES      POINTS STATUS                                
         TM    OUTIND,OUTICRMK     TEST TOTALING NOW                            
         BO    OSTATP2             YES                                          
         OC    SVDATE,SVDATE       TEST FOR COMPLETION DATE                     
         BNZ   OSTATP2             YES                                          
         MVC   0(5,R3),=C'LETE*'   NO                                           
         BZ    XIT                                                              
*                                                                               
OSTATP2  MVC   0(2,R3),=C'OK'      WITHIN TOLERANCE                             
         CLI   PLIM,1                                                           
         BE    XIT                                                              
         MVC   0(4,R3),=C'OVER'    OVER TOLERANCE                               
         CLI   PLIM,2                                                           
         BE    XIT                                                              
         MVC   0(5,R3),=C'UNDER'   UNDER TOLERANCE                              
         CLI   PLIM,0                                                           
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
ODLC     MVC   0(9,R3),SPACES      DATE LAST CHANGED                            
         TM    OUTIND,OUTICRMK     TEST TOTALING NOW                            
         BO    XIT                 YES-NO PRINT                                 
         OC    SVDATE,SVDATE       TEST COMPLETE                                
         BNZ   *+16                YES                                          
         L     R1,APRD             NO-MARK PRODUCT AS INCOMPLETE                
         MVI   0(R1),C'*'                                                       
         B     XIT                                                              
         GOTO1 DATCON,DMCB,(2,SVDATE),(5,0(R3))                                 
         B     XIT                                                              
         EJECT                                                                  
* MEDIA COMMENTS OUTPUT ROUTINE                                                 
*                                                                               
OMCOM    XC    KEY,KEY             COMMENT HEADER KEY                           
         LA    R2,KEY                                                           
         USING COMHDRD,R2                                                       
         MVC   COMKTYPE,=X'0D0C'                                                
         MVC   COMKAGY,SBBAGYMD                                                 
         MVI   COMCTYPE,C'M'                                                    
         MVC   COMKCLT,SBBCLT                                                   
         MVC   COMKPRD+2(1),SBBPRD                                              
         MVC   COMKEST,SBBEST                                                   
         MVC   COMKMKT,SBBMKT                                                   
         DROP  R2                                                               
         CLI   QESTIM,C'N'         TEST EST=NO                                  
         BNE   OMCOM16                                                          
         CLI   SBQBPRD,0           AND PRD=ALL                                  
         BNE   OMCOM16                                                          
         ZIC   R5,SBBPRD           YES-FIND PRODUCT'S CONTROL ESTIMATE          
         BCTR  R5,0                                                             
         LA    R5,CONESTAB(R5)                                                  
         CLI   0(R5),0             TEST DISCOVERED CONTROL ESTIMATE YET         
         BNE   OMCOM14             YES                                          
         MVC   SVKEY,KEY           NO-READ ESTIMATES FOR CONTROL EST            
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING ESTHDRD,R2                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,SBPRD                                                    
         MVI   EKEYEST,1                                                        
*                                                                               
OMCOM2   GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   OMCOMX                                                           
         OC    KEY+8(5),KEY+8                                                   
         BNZ   OMCOM10                                                          
         CLC   EKEYEST,SBQESTND                                                 
         BH    OMCOMX                                                           
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         OC    SBQSTART,SBQSTART                                                
         BZ    OMCOM4                                                           
         CLC   EEND,SBQSTART                                                    
         BL    OMCOM10                                                          
         CLC   ESTART,SBQEND                                                    
         BH    OMCOM10                                                          
*                                                                               
OMCOM4   CLC   SBQESFLT,SPACES     TEST FOR ESTIMATE FILTERING                  
         BNH   OMCOM12                                                          
         LA    R1,3                                                             
         LA    RE,SBQESFLT                                                      
         LA    RF,EPROF                                                         
*                                                                               
OMCOM6   CLI   0(RE),C'*'                                                       
         BE    OMCOM8                                                           
         CLI   0(RE),C' '                                                       
         BE    OMCOM8                                                           
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    OMCOM7              YES                                          
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   OMCOM10                                                          
         B     OMCOM8                                                           
*                                                                               
OMCOM7   MVC   BYTE,0(RE)                                                       
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)          NEGATIVE FILTER MUST NOT MATCH               
         BE    OMCOM10                                                          
*                                                                               
OMCOM8   LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,OMCOM6                                                        
         B     OMCOM12                                                          
*                                                                               
OMCOM10  LA    R2,KEY              SKIP TO NEXT ESTIMATE                        
         MVC   EKEY+8(5),XFF                                                    
         B     OMCOM2                                                           
*                                                                               
OMCOM12  MVC   0(1,R5),EKEYEST     MOVE CONTROL ESTIMATE INTO TABLE             
         MVC   KEY,SVKEY           RESTORE COMMENT HEADER KEY                   
         DROP  R2                                                               
*                                                                               
         USING COMHDRD,R2                                                       
OMCOM14  MVC   COMKEST,0(R5)       PICK UP CONTROL ESTIMATE FROM TABLE          
*                                                                               
OMCOM16  GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OMCOMX                                                           
         L     R2,AIO1             GET COMMENT HEADER                           
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R2,24(R2)                                                        
         SR    R0,R0                                                            
*                                                                               
OMCOM18  CLI   0(R2),0             FIND COMMENT ELEMENT                         
         BE    OMCOMX                                                           
         CLI   0(R2),5                                                          
         BE    *+14                                                             
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     OMCOM18                                                          
         ZIC   RE,1(R2)                                                         
         SH    RE,=H'3'                                                         
         BM    OMCOMX                                                           
         CH    RE,=H'29'           30 CHARACTERS MAX                            
         BNH   *+8                                                              
         LH    RE,=H'29'                                                        
         EX    RE,*+8                                                           
         B     OMCOMX                                                           
         MVC   0(0,R3),2(R2)       MOVE COMMENT TO PRINT LINE                   
*                                                                               
OMCOMX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* FIRST TIME CONTROLS                                                           
*                                                                               
FIRSTS   CLI   GLARGS,1            TEST CLIENT FIRST                            
         BNE   XIT                                                              
         CLI   SBQBPRD,0           SET PRODUCT FOR DEMO NAME GET RTN            
         BNE   *+14                                                             
         MVC   SBPRD,=C'POL'                                                    
         B     XIT                                                              
         MVC   SBPRD,SBQPRD                                                     
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HEADHOOK                                                               
*                                                                               
HEAD     CLC   MGR1HEAD,SPACES     TEST ANY MARKET GROUP HEADLINES              
         BNH   HD2                                                              
         LA    R2,5                YES-HEAD5=MKTGRP1                            
         LA    R3,DREGMGR                                                       
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R3,DWIDMGR                                                       
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR1HEAD,RF),MGR1HEAD                                        
         CLC   MGR2HEAD,SPACES     TEST MKTGRP 2 HEADLINE                       
         BNH   HD2                                                              
         LA    R2,6                YES-HEAD6=MKTGRP2                            
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR2HEAD,RF),MGR2HEAD                                        
         CLC   MGR3HEAD,SPACES     TEST MKTGRP 3 HEADLINE                       
         BNH   HD2                                                              
         LA    R2,7                YES-HEAD7=MKTGRP3                            
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR3HEAD,RF),MGR3HEAD                                        
*                                                                               
HD2      LA    R2,7                                                             
         LA    R3,96                                                            
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R3,128                                                           
         BAS   RE,HDPOS                                                         
         MVC   0(34,RF),SPACES     CLEAR RHS OF HEAD7                           
         MVC   0(16,RF),=C'PNTS TOLERANCE ='                                    
         EDIT  PCTTOLP,(5,17(RF)),1,ALIGN=LEFT,TRAIL=C'%'                       
         A     RF,PWIDTH           CLEAR RHS OF HEAD8                           
         MVC   0(34,RF),SPACES                                                  
         MVC   0(16,RF),=C'COST TOLERANCE ='                                    
         EDIT  PCTTOLC,(5,17(RF)),1,ALIGN=LEFT,TRAIL=C'%'                       
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
         GOTO1 COVAIL,DMCB,C'SETB',20000,800000,(R4)                            
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
BUFSEQ   MVC   SVBFKEY,BFKEY                                                    
         LA    R1,=C'SEQ'                                                       
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
APRD     DS   A                                                                 
GLDOL    DS   F                                                                 
GLDEM    DS   F                                                                 
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
*                                                                               
PLIM     DS    XL1                 LIMIT INDICATORS                             
CLIM     DS    XL1                 0=UNDER,1=WITHIN,2=OVER                      
*                                                                               
QESTIM   DS    CL1                                                              
POLDEM   DS    XL3                                                              
CHKDEMSW DS    CL1                                                              
*                                                                               
CMPDTLST DS    XL(MAXPRDS*3+1)                                                  
MAXPRDS  EQU   10                                                               
*                                                                               
BFREC    DS    0CL10               BUFFALO RECORD                               
BFKEY    DS    0CL4                                                             
BFMKT    DS    XL2                 KEY - MARKET                                 
BFPRD    DS    XL1                       PRODUCT                                
BFEST    DS    XL1                       ESTIMATE                               
BFCOM    DS    0CL2                                                             
BFDATE   DS    XL2                 CMT - COMPLETION DATE                        
BFACCUM  DS    XL4                                                              
*                                                                               
SVBUYKEY DS    XL13                                                             
SVBFKEY  DS    CL(L'BFKEY)                                                      
SVDATE   DS    XL2                                                              
SVBPRD   DS    XL1                                                              
SVBEST   DS    XL1                                                              
SVKEY    DS    XL(L'KEY)                                                        
*                                                                               
CONESTAB DS    XL255                                                            
*                                                                               
PATCH    DC    XL32'00'                                                         
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
         SPACE 2                                                                
* HEADLINE DISPLACEMENTS                                                        
*                                                                               
DWIDMGR  EQU   64                  WIDE-    MARKET GROUP                        
DREGMGR  EQU   48                  REGULAR- MARKET GROUP                        
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
*SPGENAGY                                                                       
*SPGENEST                                                                       
*SPGENBUY                                                                       
*SPGENGOAL                                                                      
*SPGENSTAT                                                                      
*SPGENCOM                                                                       
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
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENSTAT                                                      
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE4D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065SPWRI06   12/06/04'                                      
         END                                                                    
