*          DATA SET SPWRI19    AT LEVEL 032 AS OF 12/15/04                      
*PHASE T20419A,*                                                                
         TITLE 'T20419 - BUY ACTIVITY REPORT'                                   
*********************************************************************           
*                                                                   *           
*          SPWRI19 (T20419A) - BUY ACTIVITY REPORT                  *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 17JUN97 31 EFJ -- HISTORY LOST.  ADD 'A' TO PHASE                 *           
*                -- PUT TBAKSTA INTO SBBSTA (NOT TBAKMKT)           *           
*                                                                   *           
*********************************************************************           
T20419   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20419,RA                                                  
         LR    R8,RC                                                            
         USING WORKD,R8                                                         
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPFIRST      FIRST CALL                                   
         BE    FIRST                                                            
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPINPUT      SPOTIO HOOK                                  
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
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
* FIRST CALL                                                                    
*                                                                               
FIRST    OI    OPTIND2,OPTINOP     SUPPRESS PERIOD VALIDATION                   
         B     XIT                                                              
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     LA    R2,TBATITH          VALIDATE THE TITLE                           
         MVC   TITLE,BLANKS                                                     
         MVC   TITLE(19),=C'BUY ACTIVITY REPORT'                                
         CLI   5(R2),0                                                          
         BE    INIT1                                                            
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT1    GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE ONLY,                                
         BNE   XIT                                                              
         OI    SBQSKIP,SBQSKBUY    TELL SPOTIO NOT TO READ BUYS,                
         OI    SBQSKIP,SBQSKGL     GOALS,                                       
         OI    SBQSKIP,SBQSKBIL    OR BILLS                                     
         CLI   SBQBPRD,X'FF'       TRANSLATE PRD=POL TO PRD=ALL                 
         BNE   *+8                                                              
         MVI   SBQBPRD,0                                                        
         MVI   SBQSEPES,C'Y'       ENSURE EST=ALL                               
         MVI   SBQPER,SBQPWK       SET PERIOD TO WEEKS                          
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         MVI   MYFIRSTH,11         FIRST HEADING ON HEAD11                      
         XC    LEVELS,LEVELS                                                    
         LA    R1,LEVELS           SET THE LEVELS                               
         LA    R4,RPTLEVS                                                       
         LA    RF,1                                                             
*                                                                               
INIT2    CLI   0(R4),X'FF'                                                      
         BE    INIT4                                                            
         MVC   0(1,R1),0(R4)                                                    
         CLI   0(R1),QMKT                                                       
         BNE   *+8                                                              
         STC   RF,MKTLEV           MARKET LEVEL                                 
         CLI   0(R1),QEST                                                       
         BNE   *+8                                                              
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         LA    R4,1(R4)                                                         
         B     INIT2                                                            
*                                                                               
INIT4    XC    WORK,WORK           READ TA PROFILE                              
         MVC   WORK(4),=C'S0TA'                                                 
         MVC   WORK+4(2),SBQAGY                                                 
         MVC   WORK+6(1),SBQMED                                                 
         OC    SBQBCLT,SBQBCLT                                                  
         BZ    INIT6                                                            
         MVC   WORK+7(3),SBQCLT                                                 
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         B     INIT8                                                            
*                                                                               
INIT6    CLI   SBQCLT,C'*'                                                      
         BNE   INIT8                                                            
         MVC   WORK+10(2),SBQCLT                                                
*                                                                               
INIT8    GOTO1 GETPROF,DMCB,WORK,TAPROF,DATAMGR                                 
         MVC   NUMWKS,TAPROF+3     PICK UP N'WEEKS                              
         CLI   NUMWKS,0            SET TO MAX IF 0                              
         BNE   *+8                                                              
         MVI   NUMWKS,MAXWKS                                                    
         CLI   NUMWKS,MAXWKS       LIMIT NUMBER OF WEEKS                        
         BNH   *+8                                                              
         MVI   NUMWKS,MAXWKS                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),SBQSTART   GET TODAY'S DATE                    
         GOTO1 GETDAY,DMCB,SBQSTART,FULL    GET DAY OF WEEK                     
         CLC   FULL,BLANKS                                                      
         BH    *+6                                                              
         DC    H'0'                                                             
         ZIC   R4,0(R1)                                                         
         SH    R4,=H'1'            TEST MONDAY                                  
         BNP   INIT10                                                           
         LNR   R4,R4               NO-BRING BACK TO MONDAY                      
         GOTO1 ADDAY,DMCB,SBQSTART,WORK,(R4)                                    
         MVC   SBQSTART,WORK                                                    
*                                                                               
INIT10   ZIC   R4,NUMWKS           SET END DATE TO START + 4-15 WEEKS           
         MH    R4,=H'7'                                                         
         BCTR  R4,0                                                             
         GOTO1 ADDAY,DMCB,SBQSTART,SBQEND,(R4)                                  
*                                                                               
         LA    R3,HDATES           SET DATES FOR HEADINGS                       
         ZIC   R4,NUMWKS                                                        
         MVC   WORK(6),SBQSTART                                                 
*                                                                               
INIT12   GOTO1 DATCON,DMCB,WORK,(4,(R3))                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'7'                                      
         MVC   WORK(6),WORK+6                                                   
         LA    R3,5(R3)                                                         
         BCT   R4,INIT12                                                        
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
RPTLEVS  DC    AL1(QMED)           HEADLINES                                    
         DC    AL1(QCLT)                                                        
         DC    AL1(QPRD)                                                        
         DC    AL1(QEST)                                                        
         DC    AL1(QEST)                                                        
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QMKT)           DETAILS                                      
         DC    AL1(QSTA)                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
* SPOTIO INPUT HOOK                                                             
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCCL     CLIENT FIRST                                 
         BE    CLIENT                                                           
         CLI   SBMODE,SBPROCES     ESTIMATE FIRST                               
         BE    ESTIMATE                                                         
         B     XIT                                                              
         EJECT                                                                  
* CLIENT FIRST                                                                  
*                                                                               
CLIENT   MVI   SBSPPROF+2,0        MAKE SURE THE WEEKS ARE BROADCAST            
         MVC   SBSPPROF+6(3),=X'010101'                                         
         B     XIT                                                              
         EJECT                                                                  
* ESTIMATE FIRST                                                                
*                                                                               
ESTIMATE MVI   SBESTOWD,0          IGNORE OUT-OF-WEEK ROTATION                  
         BAS   RE,TRAF             READ TRAFFIC BUY ACTIVITY RECORDS            
         BNE   XIT                                                              
         BAS   RE,BUYS             COUNT BUY MARKETS                            
         BAS   RE,GOALS            COUNT GOAL MARKETS                           
         MVI   GLOPTS+3,C'C'       CALL DRIVER FOR INPUT WITH                   
         MVI   GLMODE,GLINPUT      BUY AND GOAL RECORD COUNTS                   
         BAS   RE,GODRIVER                                                      
         B     XIT                                                              
         EJECT                                                                  
* READ TRAFFIC BUY ACTIVITY RECORDS                                             
* OUTPUT : CC EQ - BUY ACTIVITY FOUND                                           
*          CC NE - NO BUY ACTIVITY                                              
*                                                                               
TRAF     NTR1  ,                                                                
         BAS   RE,SETTRF                                                        
         MVI   ANYACT,C'N'         NO ACTIVITY YET                              
         XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         XC    KEY,KEY                                                          
         LA    R2,KEY              READ THE BUY ACTIVITY RECORDS                
         USING TBARECD,R2                                                       
         MVC   TBAKID,=X'0A2E'                                                  
         MVC   TBAKAM,SBBAGYMD                                                  
         MVC   TBAKCLT,SBBCLT                                                   
*                                                                               
TRAF1    GOTO1 HIGH                                                             
         B     TRAF4                                                            
*                                                                               
TRAF2    GOTO1 SEQ                                                              
*                                                                               
TRAF4    CLC   KEY(TBAKMKT-TBAKEY),KEYSAVE   TEST END OF CLIENT                 
         BNE   TRAFX                                                            
         CLC   TBAKPRD,SBBPRD      COMPARE PRODUCTS                             
         BE    TRAF6                                                            
         BH    TRAF8               HIGH-SKIP TO NEXT STATION                    
         MVC   TBAKPRD,SBBPRD      LOW-SKIP TO PRD/EST                          
         B     TRAF9                                                            
*                                                                               
TRAF6    CLC   TBAKEST,SBBEST      COMPARE ESTIMATES                            
         BE    TRAF10                                                           
         BL    TRAF9               LOW-SKIP TO EST                              
*                                                                               
TRAF8    MVC   TBAKPRD(3),XFF      FORCE NEXT STATION                           
         B     TRAF1                                                            
*                                                                               
TRAF9    MVC   TBAKEST,SBBEST      SKIP TO EST                                  
         MVI   TBAKPRD2,0                                                       
         B     TRAF1                                                            
*                                                                               
TRAF10   CLC   TBAKMKT,SBBMKT      TEST NEW MARKET                              
         BE    TRAF11                                                           
         MVC   SBBMKT,TBAKMKT      YES-                                         
         BAS   RE,ADDMKT           ADD MARKET DETAILS TO NAME POOL              
*                                                                               
TRAF11   CLC   TBAKSTA,SBBSTA      TEST NEW STATION                             
         BE    TRAF12                                                           
         MVC   SBBSTA,TBAKSTA      YES-                                         
         MVC   WORK(2),TBAKMKT                                                  
         MVC   WORK+2(3),TBAKSTA                                                
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,SBSTA                                    
         BAS   RE,GETSTA           GET STATION'S AFFILIATE                      
*                                                                               
TRAF12   XC    SVPRD2,SVPRD2                                                    
         SR    RE,RE                                                            
         ICM   RE,1,TBAKPRD2       TEST PARTNER PRODUCT                         
         BZ    TRAF13                                                           
         BCTR  RE,0                YES-GET IT'S ALPHA CODE                      
         MH    RE,=Y(PRDBUFFL)                                                  
         L     RF,SBAPRDBF                                                      
         AR    RF,RE                                                            
         MVC   SVPRD2,PBALPH-PRDBUFFD(RF)                                       
*                                                                               
TRAF13   L     R2,SBAIO1           GET THE RECORD                               
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R3,TBAKEY+24        SEARCH FOR TBA DATA ELEMENTS                 
*                                  WITHIN REQUEST PERIOD                        
TRAF14   CLI   0(R3),0                                                          
         BE    TRAF20                                                           
         CLI   0(R3),5                                                          
         BNE   TRAF19                                                           
         USING TBADTAEL,R3                                                      
         L     R1,AWEEKS                                                        
         LA    RE,1                                                             
         ZIC   R0,NUMWKS                                                        
*                                                                               
TRAF16   CLC   TBADTAWK,0(R1)                                                   
         BL    *+14                                                             
         CLC   TBADTAWK,2(R1)                                                   
         BNH   TRAF18                                                           
         LA    RE,1(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,TRAF16                                                        
         B     TRAF19                                                           
*                                                                               
TRAF18   MVC   ACTIND,TBADTAAC     ACTIVITY INDICATOR                           
         NI    ACTIND,X'C0'                                                     
         CLI   ACTIND,0            TEST ANY SIGNIFICANT DATA                    
         BE    TRAF19                                                           
         MVI   ANYACT,C'Y'         YES-                                         
         STC   RE,WKNUM            STORE WEEK NUMBER                            
         ZIC   RE,TBADTASL         ADD UP SPOT LENGTHS                          
         ZIC   RF,TBADTAS2                                                      
         AR    RE,RF                                                            
         STC   RE,SLN                                                           
         MVI   GLOPTS+3,C'D'       DETAIL REPORT                                
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         BAS   RE,GODRIVER                                                      
*                                                                               
TRAF19   ZIC   RE,1(R3)            NEXT ELEMENT                                 
         AR    R3,RE                                                            
         B     TRAF14                                                           
*                                                                               
TRAF20   LA    R2,KEY              READ SEQUENTIAL                              
         B     TRAF2                                                            
*                                                                               
TRAFX    BAS   RE,SETSPT                                                        
         CLI   ANYACT,C'Y'         TEST ANY ACTIVITY                            
         B     XIT                                                              
         EJECT                                                                  
SETTRF   MVC   SYSDIR,=C'TRFDIR  '                                              
         MVC   SYSFIL,=C'TRFFILE '                                              
         BR    RE                                                               
*                                                                               
SETSPT   MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFILE '                                              
         BR    RE                                                               
         EJECT                                                                  
* READ BUY KEYS TO COUNT NUMBER OF MARKETS BOUGHT                               
*                                                                               
BUYS     NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING BUYRECD,R2                                                       
         MVC   BUYKAM,SBBAGYMD                                                  
         MVC   BUYKCLT,SBBCLT                                                   
         MVC   BUYKPRD,SBBPRD                                                   
         XC    SBBMKT,SBBMKT                                                    
         SR    R3,R3                                                            
*                                                                               
BUY2     GOTO1 HIGH                                                             
         CLC   KEY(BUYMSTA-BUYKEY),KEYSAVE    READ TO END OF PRODUCT            
         BNE   BUY6                                                             
         CLI   BUYKBUY,X'80'       TEST SPILL                                   
         BNE   *+14                                                             
         MVC   BUYKEST(4),XFF      YES-SKIP TO NEXT STATION                     
         B     BUY2                                                             
         CLC   BUYKEST,SBBEST      COMPARE ESTIMATES                            
         BE    BUY4                                                             
         BL    *+14                                                             
         MVC   BUYKEST(4),XFF      HIGH-SKIP TO NEXT STATION                    
         B     BUY2                                                             
         MVC   BUYKEST,SBBEST      LOW-SKIP TO EST                              
         XC    BUYKBUY,BUYKBUY                                                  
         B     BUY2                                                             
*                                                                               
BUY4     CLC   SBBMKT,BUYMSTA      TEST MARKET CHANGE                           
         BE    *+14                                                             
         LA    R3,1(R3)            YES-AUGMENT MARKET COUNT                     
         MVC   SBBMKT,BUYMSTA                                                   
         MVC   BUYMSTA+2(7),XFF    SKIP TO NEXT MARKET                          
         B     BUY2                                                             
*                                                                               
BUY6     ST    R3,BUYCNT           STORE BUY MARKET COUNT                       
*                                                                               
BUYX     B     XIT                                                              
         EJECT                                                                  
* READ GOAL KEYS TO COUNT NUMBER OF MARKETS GOALED                              
*                                                                               
GOALS    NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING GOALRECD,R2                                                      
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,SBBAGYMD                                                  
         MVC   GKEYCLT,SBBCLT                                                   
         MVC   GKEYPRD,SBBPRD                                                   
         SR    R3,R3                                                            
*                                                                               
GOAL2    GOTO1 HIGH                                                             
         CLC   KEY(GKEYMKT-GKEY),KEYSAVE   READ TO END OF PRODUCT               
         BNE   GOAL6                                                            
         CLC   GKEYEST,SBBEST      COMPARE ESTIMATES                            
         BE    GOAL4                                                            
         BL    *+14                                                             
         MVC   GKEYEST(6),XFF      HIGH-SKIP TO NEXT MARKET                     
         B     GOAL2                                                            
         MVC   GKEYEST,SBBEST      LOW-SKIP TO EST                              
         XC    GKEYDPT(5),GKEYDPT                                               
         B     GOAL2                                                            
*                                                                               
GOAL4    LA    R3,1(R3)            AUGMENT MARKET COUNT                         
         MVC   GKEYEST(6),XFF      SKIP TO NEXT MARKET                          
         B     GOAL2                                                            
*                                                                               
GOAL6    ST    R3,GOALCNT          STORE GOAL MARKET COUNT                      
*                                                                               
GOALX    B     XIT                                                              
         EJECT                                                                  
* ADD MARKET DETAILS TO NAME POOL                                               
*                                                                               
ADDMKT   NTR1  ,                                                                
         SR    R0,R0               TEST MARKET IN NAME POOL YET                 
         SR    R1,R1                                                            
         ICM   R1,3,SBBMKT                                                      
         D     R0,=F'8'                                                         
         LA    RF,MKTTAB(R1)                                                    
         LA    RE,X'80'                                                         
         LTR   R0,R0                                                            
         BZ    *+12                                                             
         SRL   RE,1                                                             
         BCT   R0,*-4                                                           
         EX    RE,TM                                                            
         BO    ADDMKTX             YES                                          
         EX    RE,OI               NO -                                         
         XC    SBMKTREC,SBMKTREC                                                
         MVC   SBMKTNM,BLANKS                                                   
         MVC   SBMKTNM(9),=C'*UNKNOWN*'                                         
         XC    SBMKTWGT,SBMKTWGT                                                
         MVC   ELEM,KEY            SAVE KEY AND KEYSAVE                         
         XC    KEY,KEY             READ MARKET RECORD                           
         LA    R2,KEY                                                           
         USING MKTKEY,R2                                                        
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,SBQMED                                                   
         SR    R1,R1                                                            
         ICM   R1,3,SBBMKT                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,SBAGY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,SBAIO2                   
         CLI   8(R1),0                                                          
         BNE   ADDMKT2                                                          
         L     R2,SBAIO2                                                        
         MVC   SBMKTREC(L'MKTREC),MKTREC  MOVE MARKET TO BLOCK                  
         LA    R2,SBMKTREC                                                      
         MVC   SBMKTNM,MKTNAME     EXTRACT MARKET NAME                          
*                                                                               
ADDMKT2  DS    0H                                                               
         GOTO1 PUTMKTNM            PUT MARKET DETAILS IN NAME POOL              
         MVC   KEY,ELEM            RESTORE KEYS                                 
         MVC   KEYSAVE,ELEM+L'KEY                                               
         GOTO1 HIGH                AND RESTORE READ SEQUENCE                    
         DROP  R2                                                               
*                                                                               
ADDMKTX  B     XIT                                                              
         SPACE 2                                                                
TM       TM    0(RF),0             EXECUTED INSTRUCTIONS                        
OI       OI    0(RF),0                                                          
         EJECT                                                                  
* READ STATION RECORD                                                           
*                                                                               
GETSTA   NTR1  ,                                                                
         MVC   ELEM,KEY            SAVE KEY AND KEYSAVE                         
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING STARECD,R2                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(16),STAKEY                                              
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,SBQMED                                                   
         MVC   STAKCALL,SBSTA                                                   
         CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,SBCLT                                                    
         L     R2,SBAIO2                                                        
         ST    R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SBAFFIL,SNETWRK     EXTRACT THE AFFILIATION                      
         MVC   KEY,ELEM            RESTORE KEYS                                 
         MVC   KEYSAVE,ELEM+L'KEY                                               
         GOTO1 HIGH                AND RESTORE READ SEQUENCE                    
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEDHK                                                            
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
         DC    CL8'IBCNT   ',A(IBCNT)                                           
         DC    CL8'OBCNT   ',A(OBCNT)                                           
         DC    CL8'IGCNT   ',A(IGCNT)                                           
         DC    CL8'OGCNT   ',A(OGCNT)                                           
         DC    CL8'ISLN    ',A(ISLN)                                            
         DC    CL8'IPART   ',A(IPART)                                           
         DC    CL8'IWEEK   ',A(IWEEK)                                           
         DC    CL8'OWEEK   ',A(OWEEK)                                           
         DC    CL8'HWEEK   ',A(HWEEK)                                           
         DC    CL8'FEST    ',A(FEST)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  MVC   GLOPTS+2(1),NUMWKS      SET N'REQUEST WEEKS                      
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   *+8                                                              
         MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
IBCNT    MVC   0(4,R2),BUYCNT      BUY AND GOAL MARKET COUNTS                   
         B     XIT                                                              
*                                                                               
IGCNT    MVC   0(4,R2),GOALCNT                                                  
         B     XIT                                                              
*                                                                               
OBCNT    MVC   BUYCNT,0(R2)                                                     
         B     XIT                                                              
*                                                                               
OGCNT    MVC   GOALCNT,0(R2)                                                    
         B     XIT                                                              
*                                                                               
ISLN     MVC   0(1,R2),SLN         SPOT LENGTH                                  
         B     XIT                                                              
*                                                                               
IPART    MVC   0(3,R2),SVPRD2      PARTNER PRODUCT                              
         B     XIT                                                              
*                                                                               
IWEEK    CLC   WKNUM,GLARGS        WEEK INPUT                                   
         BNE   XIT                                                              
         MVC   0(1,R2),ACTIND                                                   
         B     XIT                                                              
*                                                                               
OWEEK    CLI   0(R2),0             WEEK OUTPUT                                  
         BE    XIT                                                              
         CLI   TAPROF+4,C'Y'       PROFILE OPTION TO SUPPRESS *T                
         BNE   *+12                                                             
         CLI   0(R2),X'C0'                                                      
         BE    XIT                                                              
         MVC   0(2,R3),=C'*M'                                                   
         CLI   0(R2),X'80'         *M = MEDIA BUY, NO TRAFICKING                
         BE    XIT                                                              
         MVI   1(R3),C'T'                                                       
         CLI   0(R2),X'C0'         *T = STATION INTRUCTED FOR SOME              
         BE    XIT                      WEEKS BUT NOT THIS                      
         MVI   2(R3),C'M'                                                       
         CLI   0(R2),X'D0'         *TM = *T WEEK THAT NOW HAS A MEDIA           
         BE    XIT                       BUY                                    
         MVC   0(3,R2),BLANKS                                                   
         B     XIT                                                              
*                                                                               
HWEEK    ZIC   R1,GLARGS           WEEK HEADING                                 
         BCTR  R1,0                                                             
         MH    R1,=H'5'                                                         
         LA    R1,HDATES(R1)                                                    
         MVC   0(3,R3),0(R1)                                                    
         MVC   199(2,R3),3(R1)                                                  
         B     XIT                                                              
*                                                                               
FEST     XC    BUYCNT,BUYCNT       ESTIMATE FIRST - CLEAR RECORD COUNTS         
         XC    GOALCNT,GOALCNT                                                  
         B     XIT                                                              
         EJECT                                                                  
* DRIVER'S ABOUT TO PRINT A LINE                                                
*                                                                               
PRINT    CLI   GLRECNO,1           TEST FIRST REPORT                            
         BNE   *+8                                                              
         MVI   GLHOOK,GLDONT       YES-DON'T PRINT                              
         B     XIT                                                              
         EJECT                                                                  
* HEADHOOK                                                                      
*                                                                               
HEDHK    L     R2,AH4              FORMAT MARKET COUNTS TO HEADLINES            
         A     R2,PWIDTH                                                        
         LA    R3,54(R2)                                                        
         MVC   0(17,R3),=C'MARKETS GOALED = '                                   
         LA    R3,17(R3)                                                        
         OC    GOALCNT,GOALCNT                                                  
         BNZ   *+12                                                             
         MVI   0(R3),C'0'                                                       
         B     HEDHK2                                                           
         EDIT  GOALCNT,(4,(R3)),ALIGN=LEFT                                      
*                                                                               
HEDHK2   A     R2,PWIDTH                                                        
         LA    R3,54(R2)                                                        
         MVC   0(17,R3),=C'MARKETS BOUGHT = '                                   
         LA    R3,17(R3)                                                        
         OC    BUYCNT,BUYCNT                                                    
         BNZ   *+12                                                             
         MVI   0(R3),C'0'                                                       
         B     HEDHK4                                                           
         EDIT  BUYCNT,(4,(R3)),ALIGN=LEFT                                       
*                                                                               
HEDHK4   A     R2,PWIDTH           ESTIMATE DATES                               
         A     R2,PWIDTH           ON HEAD8                                     
         LA    R3,16(R2)                                                        
         GOTO1 DATCON,DMCB,(2,SBESTSTP),(5,(R3))                                
         MVI   8(R3),C'-'                                                       
         GOTO1 (RF),(R1),(2,SBESTNDP),(5,9(R3))                                 
*                                                                               
HEDHKX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                   *         
***********************************************************************         
         SPACE 1                                                                
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
BUYCNT   DS    F                                                                
GOALCNT  DS    F                                                                
TAPROF   DS    CL16                                                             
HDATES   DS    (MAXWKS)CL5                                                      
MAXWKS   EQU   15                                                               
NUMWKS   DS    XL1                                                              
WKNUM    DS    XL1                                                              
ACTIND   DS    XL1                                                              
SLN      DS    XL1                                                              
ANYACT   DS    CL1                                                              
SVPRD2   DS    CL3                                                              
*                                                                               
XFF      DC    XL12'FFFFFFFFFFFFFFFFFFFFFFFF'                                   
BLANKS   DC    CL80' '                                                          
*                                                                               
MKTTAB   DC    2250X'00'                                                        
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    F                                                                
         DS    F                                                                
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
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPGENMKT                                                                       
*SPGENSTA                                                                       
*SPGENBUY                                                                       
*SPGENGOAL                                                                      
*SPTRTBAE                                                                       
*SPWRIFFD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPTRTBAE                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIEED                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPWRI19   12/15/04'                                      
         END                                                                    
