*          DATA SET SPWRI15    AT LEVEL 046 AS OF 12/06/04                      
*PHASE T20415A                                                                  
         TITLE 'T20415 - DRAFT ORDER REPORT'                                    
T20415   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20415,RA                                                      
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
         CLI   RPMODE,RPINPUT      INPUT MODE                                   
         BE    INPUT                                                            
         CLI   RPMODE,RPSPHOOK     SPOT HOOK                                    
         BE    SPOT                                                             
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL      FINAL HOOK                                   
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
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     MVI   SBQSKIP,SBQSKGL+SBQSKBIL   READ BUY RECORDS ONLY                 
         MVI   SBQPER,SBQPWK       PERIODS ARE WEEKS                            
         MVI   SBQPERLO,1          ALL WEEKS WITHIN PERIOD                      
         MVI   SBQPERHI,X'FF'                                                   
         OI    DATAIND2,DIAFFIL    NEED STATION AFFILIATE                       
         OI    DATAIND3,DICHAN+DIREP        AND CHANNEL AND REP                 
         XC    TAXTOT,TAXTOT                                                    
         XC    DOLTOT,DOLTOT                                                    
         MVI   ACTSW,C'N'          NO PRINT ACTIVITY YET                        
*                                                                               
         XC    QBYR,QBYR           VALIDATE BUYER CODE                          
         LA    R2,DORBYCH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    INIT2                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   QBYR(0),8(R2)                                                    
         OC    QBYR,BLANKS                                                      
*                                                                               
INIT2    MVC   QBYRNM,BLANKS       VALIDATE BUYER NAME                          
         LA    R2,DORBYNH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    INIT4                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   QBYRNM(0),8(R2)                                                  
*                                                                               
INIT4    OC    QBYR,QBYR           VALIDATE CAMPAIGN NUMBER                     
         BZ    INIT5                                                            
         LA    R2,DORCAMH                                                       
         GOTO1 ANY                                                              
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   RE,DUB                                                           
         STH   RE,QCAM                                                          
*                                                                               
INIT5    LA    R2,DORCVEH          VALIDATE CURRENT VERSION                     
         GOTO1 ANY                                                              
         MVC   CURVER,WORK                                                      
         LA    R2,DORPVEH          VALIDATE PREVIOUS VERSION                    
         GOTO1 ANY                                                              
         MVC   PREVER,WORK                                                      
         XC    PREDATE,PREDATE                                                  
         LA    R2,DORPDTH          VALIDATE PREVIOUS DATE                       
         CLI   5(R2),0                                                          
         BE    INIT6                                                            
         GOTO1 ANY                                                              
         MVC   PREDATE,WORK                                                     
*                                                                               
INIT6    MVI   MAXLPP,0            MAX LINES PER PAGE                           
         LA    R2,DORLPPH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    INIT7                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         STC   RE,MAXLPP                                                        
*                                                                               
INIT7    MVI   DAILY,C'N'                                                       
         LA    R2,DORDLYH          DAILY SCHEDULE OPTION                        
         CLI   5(R2),0                                                          
         BE    INIT8                                                            
         MVC   DAILY,8(R2)                                                      
         CLI   DAILY,C'N'                                                       
         BE    INIT8                                                            
         CLI   DAILY,C'Y'                                                       
         BNE   EINV                                                             
*                                                                               
INIT8    LA    R2,DORTITH          VALIDATE TITLE                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(11),=C'DRAFT ORDER'                                        
         CLI   5(R2),0                                                          
         BE    INIT9                                                            
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT9    GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         LA    R1,LEVELS           SET THE LEVELS                               
         LA    RE,RPTLEVS                                                       
         LA    RF,1                                                             
INIT10   CLI   0(RE),X'FF'                                                      
         BE    INIT14                                                           
         CLI   0(RE),0                                                          
         BE    INIT12                                                           
         MVC   0(1,R1),0(RE)                                                    
         CLI   0(R1),QMKT                                                       
         BNE   *+8                                                              
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
INIT12   LA    RE,1(RE)                                                         
         B     INIT10                                                           
*                                                                               
INIT14   MVI   MYFIRSTH,11         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
         XC    WORK,WORK           READ CP PROFILE                              
         MVC   WORK(4),=C'S0CP'                                                 
         MVC   WORK+4(2),SBQAGY                                                 
         MVC   WORK+6(1),SBQMED                                                 
         MVC   WORK+7(3),SBQCLT                                                 
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,CPPROF,DATAMGR                                 
*                                                                               
         CLI   CPPROF+2,C'Y'       TEST CP PROFILE ASKS FOR DAILY GRID          
         BNE   *+8                                                              
         MVI   DAILY,C'Y'          YES                                          
         CLI   DAILY,C'Y'                                                       
         BNE   INITX                                                            
         MVI   SBQPER,SBQPDY       ASK FOR DAYS                                 
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
RPTLEVS  DC    AL1(QMED)           HEADLINES                                    
         DC    AL1(QCLT)                                                        
         DC    AL1(QPRD)                                                        
         DC    AL1(QEST)                                                        
         DC    AL1(QMKT)                                                        
         DC    AL1(QSTA)           MIDLINE                                      
         DC    AL1(QBUY)           DETAIL                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* FURTHER REQUEST VALIDATION                                          *         
***********************************************************************         
         SPACE 1                                                                
VALID    LA    R2,DORCLTH                                                       
         OC    SBQBCLT,SBQBCLT     SINGLE CLIENT ONLY                           
         BZ    EINV                                                             
         LA    R2,DORPRDH                                                       
         CLI   SBQBPRD,0           PRODUCT ALL IS INVALID                       
         BE    EINV                                                             
         LA    R2,DORESTH                                                       
         CLC   SBQEST,SBQESTND     SINGLE ESTIMATE ONLY                         
         BNE   EINV                                                             
         LA    R2,DORMKTH                                                       
         CLC   SBQMKT,ZEROS        SINGLE MARKET ONLY                           
         BNH   EINV                                                             
         LA    R2,DORSTAH                                                       
         CLC   SBQSTA,BLANKS       SINGLE STATION ONLY                          
         BNH   EINV                                                             
*                                                                               
VALIDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS AND MESSAGES                                            *         
***********************************************************************         
         SPACE 1                                                                
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
         EJECT                                                                  
***********************************************************************         
* INPUT HOOK FROM SPOTIO                                              *         
***********************************************************************         
         SPACE 1                                                                
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCSP     BUY RECORD                                   
         BE    PROCBUY                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUY RECORD HOOK                                                     *         
***********************************************************************         
         SPACE 1                                                                
PROCBUY  OC    QBYR,QBYR           TEST BWS BUYER FILTER                        
         BZ    BUYX                                                             
         L     R6,SBAIO1                                                        
         USING BUYRECD,R6                                                       
         LA    R1,BDELEM           YES-ONLY ACCEPT THIS BUY IF BUYER            
         SR    R0,R0                   AND CAMPAIGN MATCH THOSE IN THE          
*                                      BWS TRANSFER ELEMENT                     
BUY2     CLI   0(R1),0                                                          
         BE    BUY9                                                             
         CLI   0(R1),BWSCODEQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BUY2                                                             
         USING BWSELEM,R1                                                       
         CLC   BWSBYR,QBYR                                                      
         BNE   BUY9                                                             
         CLC   BWSCAM,QCAM                                                      
         BE    BUYX                                                             
*                                                                               
BUY9     MVI   RPMODE,RPSKIOHK     SKIP THIS BUY                                
*                                                                               
BUYX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SPOT HOOK                                                           *         
* SETS SBYORN TO C'N' TO REJECT A SPOT                                *         
***********************************************************************         
         SPACE 1                                                                
SPOT     L     R4,SBASPOT                                                       
         USING REGELEM,R4                                                       
         CLI   CPPROF+1,C'Y'       TEST TO EXCLUDE PIGGYBACKS FROM              
         BNE   SPOTX               SINGLE PRODUCT REQUEST                       
         CLI   SBQBPRD,0                                                        
         BE    SPOTX                                                            
         CLI   SBQBPRD,X'FF'                                                    
         BE    SPOTX                                                            
         CLI   SBQBPRD2,0                                                       
         BNE   SPOTX                                                            
         CLI   RLEN,14                                                          
         BNH   SPOTX                                                            
*                                                                               
SPOTNO   MVI   SBYORN,C'N'                                                      
*                                                                               
SPOTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOKS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLOUTPUT     DRIVER OUTPUT                                
         BE    OUT                                                              
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                            *         
***********************************************************************         
         SPACE 1                                                                
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
         DC    CL8'OSTA    ',A(OSTA)                                            
         DC    CL8'TSTA    ',A(TSTA)                                            
         DC    CL8'OMKT    ',A(OMKT)                                            
         DC    CL8'IBUY    ',A(IBUY)                                            
         DC    CL8'OBUY    ',A(OBUY)                                            
         DC    CL8'HBUY    ',A(HBUY)                                            
         DC    CL8'ISPOTS  ',A(ISPOTS)                                          
         DC    CL8'OSPOTS  ',A(OSPOTS)                                          
         DC    CL8'HSPOTS  ',A(HSPOTS)                                          
         DC    CL8'ITOTSPTS',A(ITOTSPTS)                                        
         DC    CL8'IDOL    ',A(IDOL)                                            
         DC    CL8'ODOL    ',A(ODOL)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DRIVER INITIALIZATION                                               *         
***********************************************************************         
         SPACE 1                                                                
DRVINIT  MVI   GLAUTOCH,C'N'       NO AUTO-CHUNKING                             
         OI    GLINDS,GLPALTOT     PRINT ALL TOTALS                             
         MVI   GLUNDCHR,C'-'       UNDERLINE CHARACTER FOR MIDLINES             
         LA    R1,RPTSPECS         REPORT SPECS                                 
         ST    R1,SPECS                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK TO EXECUTE ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   EXECX                                                            
         L     R1,GLADTENT         YES-                                         
         USING DRIND,R1                                                         
         CLI   DRINLEV,1           TEST LEVEL 1                                 
         BH    *+8                                                              
         MVI   INDATA,0            YES-RESET DATA INDICATOR                     
         DROP  R1                                                               
         L     R5,SBACURCH         R5=A(CHUNK ENTRY)                            
         USING SCHUNKD,R5                                                       
         L     R6,SBAIO1           R6=A(BUY RECORD)                             
         USING BUYRECD,R6                                                       
         B     EXECX                                                            
*                                                                               
EXECX    L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* INPUT ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
IBUY     CLC   BUYKEY,BUYKEYSV     TEST NEW BUY KEY                             
         BE    IBUY6                                                            
         MVC   BUYKEYSV,BUYKEY                                                  
         MVC   BYLINE1,BUYKBUY                                                  
         MVC   BYDAYS,BDDAY                                                     
         MVC   BYTIMES,BDTIMST                                                  
         MVC   BYPROG,BDPROGRM                                                  
         MVC   BYLEN,BDSEC                                                      
         MVC   BYRECDA,SBRECDA                                                  
         MVC   BYSEDAY,BDSEDAY                                                  
         SR    R1,R1                                                            
         ICM   R1,7,BDCOST                                                      
         STCM  R1,15,BYCOST                                                     
         MVI   BYIND,0                                                          
         MVI   BYPKGIND,0                                                       
         SR    R0,R0               LOOK FOR COMMENT, ORBIT AND PACKAGE          
         LA    R1,BDELEM           ELEMENTS                                     
*                                                                               
IBUY2    CLI   0(R1),0                                                          
         BE    IBUY6                                                            
         CLI   0(R1),X'66'                                                      
         BNE   *+12                                                             
         OI    BYIND,BYICOM                                                     
         B     IBUY4                                                            
         CLI   0(R1),X'67'                                                      
         BNE   *+12                                                             
         OI    BYIND,BYIORB                                                     
         B     IBUY4                                                            
         CLI   0(R1),5                                                          
         BNE   IBUY4                                                            
         USING PKGELEM,R1                                                       
         MVC   BYPKGIND,PKGIND     FOUND-SET PACKAGE INDICATOR                  
         CLI   PKGIND,2            TEST PACKAGE SLAVE                           
         BE    *+12                                                             
         CLI   PKGIND,6            OR REVISION SLAVE                            
         BNE   IBUY4                                                            
         MVC   BYLINE1,PKGLINES    YES-SET MASTER LINE NUMBER                   
         MVC   BYLINE2,BUYKBUY         AND SLAVE                                
         DROP  R1                                                               
*                                                                               
IBUY4    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     IBUY2                                                            
*                                                                               
IBUY6    MVC   0(L'BYREC,R2),BYREC                                              
*                                                                               
IBUYX    B     XIT                                                              
         SPACE 2                                                                
ISPOTS   L     R5,SBACURCH         WEEKLY SPOTS                                 
         USING SCHUNKD,R5                                                       
         XC    0(2,R2),0(R2)                                                    
         OC    SCSPOTS,SCSPOTS                                                  
         BZ    XIT                                                              
         ZIC   R3,GLARGS                                                        
         LA    R6,2                                                             
ISPOTS2  C     R3,SBNDATES                                                      
         BH    XIT                                                              
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         L     RE,SBADATE                                                       
         LA    RE,0(R1,RE)                                                      
         CLC   SCDATE,0(RE)                                                     
         BE    ISPOTS4                                                          
         LA    R3,14(R3)           IN CASE PERIOD GREATER THAN 14 WEEKS         
         LA    R2,1(R2)                                                         
         BCT   R6,ISPOTS2                                                       
         B     XIT                                                              
ISPOTS4  MVC   0(1,R2),SCSPOTS+3                                                
         MVI   INDATA,1                                                         
         B     XIT                                                              
*                                                                               
ITOTSPTS L     R5,SBACURCH         TOTAL SPOTS                                  
         OC    SCSPOTS,SCSPOTS                                                  
         BZ    XIT                                                              
         MVC   0(2,R2),SCSPOTS+2                                                
         MVI   INDATA,1                                                         
         B     XIT                                                              
*                                                                               
IDOL     L     R5,SBACURCH         COST                                         
         L     R1,SCGROSS                                                       
         CLI   SBETAX,C'Y'         TEST OPTION TO SUBTRACT TAX                  
         BNE   IDOL2                                                            
         L     RE,SCTAX                                                         
         SR    R1,RE                                                            
         A     RE,TAXTOT           YES-KEEP TALLY OF TAX                        
         ST    RE,TAXTOT                                                        
IDOL2    ST    R1,0(R2)                                                         
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT ROUTINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
OMKT     MVC   SBBMKT,0(R2)        MARKET                                       
         B     XIT                                                              
*                                                                               
OSTA     LA    R3,1(R3)            STATION                                      
         MVC   0(8,R3),BLANKS                                                   
         MVC   0(4,R3),0(R2)                                                    
         OC    5(3,R2),5(R2)       TEST CABLE NETWORK                           
         BZ    OSTA2                                                            
         MVI   4(R3),C'/'          YES-FORMAT THE NETWORK                       
         MVC   5(3,R3),5(R2)                                                    
         B     OSTA4                                                            
*                                                                               
OSTA2    MVI   4(R3),C'-'                                                       
         MVC   5(1,R3),4(R2)                                                    
         MVI   6(R3),C'M'                                                       
         CLI   4(R2),C'T'                                                       
         BE    *+12                                                             
         CLI   4(R2),C' '                                                       
         BH    OSTA4                                                            
         MVC   5(2,R3),=C'TV'                                                   
*                                                                               
OSTA4    MVC   SVSTA,0(R3)                                                      
         LA    R3,9(R3)                                                         
         CLC   SBCHAN,BLANKS                                                    
         BNH   OSTA6                                                            
         MVC   0(2,R3),=C'CH'                                                   
         MVC   3(4,R3),SBCHAN                                                   
         LA    R3,8(R3)                                                         
*                                                                               
OSTA6    CLC   SBAFFIL,BLANKS                                                   
         BNH   OSTA8                                                            
         MVC   0(9,R3),=C'AFFILIATE'                                            
         MVC   10(3,R3),SBAFFIL                                                 
         LA    R3,14(R3)                                                        
*                                                                               
OSTA8    CLI   CPPROF,C'N'         ONLY PRINT REP IF CP PROFILE SAYS SO         
         BE    OSTAX                                                            
         CLC   SBREP,ZEROS                                                      
         BNH   OSTAX                                                            
         CLC   SBREPNM,BLANKS                                                   
         BH    *+8                                                              
         BAS   RE,GETREP                                                        
         MVC   0(4,R3),=C'REP-'                                                 
         MVC   5(22,R3),SBREPNM                                                 
*                                                                               
OSTAX    B     XIT                                                              
*                                                                               
*                                                                               
ODOL     TM    GLINDS,GLTOTLIN     IF NOT TOTAL LINE,                           
         BO    XIT                                                              
         L     R1,0(R2)            ACCUMULATE TOTAL COST                        
         A     R1,DOLTOT                                                        
         ST    R1,DOLTOT                                                        
         B     XIT                                                              
*                                                                               
OSPOTS   LA    R5,2                SPOTS                                        
         SR    R1,R1                                                            
OSPOTS2  MVI   2(R3),0                                                          
         CLC   SBNDATES,=F'14'                                                  
         BNH   OSPOTS6                                                          
         C     R5,=F'1'                                                         
         BH    OSPOTS4                                                          
         ZIC   RE,GLARGS                                                        
         LA    RE,14(RE)                                                        
         C     RE,SBNDATES                                                      
         BH    OSPOTS6                                                          
OSPOTS4  MVI   2(R3),C'.'                                                       
OSPOTS6  ICM   R1,1,0(R2)                                                       
         BZ    OSPOTS8                                                          
         EDIT  (R1),(3,(R3))                                                    
OSPOTS8  CLC   SBNDATES,=F'14'                                                  
         BNH   XIT                                                              
         LA    R2,1(R2)                                                         
         LA    R3,198(R3)                                                       
         BCT   R5,OSPOTS2                                                       
         B     XIT                                                              
         EJECT                                                                  
OBUY     MVC   BYREC,0(R2)         BUYLINE                                      
         GOTO1 DAYUNPK,DMCB,(BYSEDAY,BYDAYS),(R3)                               
         GOTO1 UNTIME,DMCB,BYTIMES,9(R3)                                        
         MVC   21(16,R3),BYPROG                                                 
         EDIT  BYLEN,(3,38(R3))                                                 
         LA    R3,198(R3)                                                       
         MVI   3(R3),C'$'                                                       
         EDIT  BYCOST,(9,4(R3)),2,ALIGN=LEFT                                    
         CLI   BYPKGIND,0                                                       
         BE    OBUY3                                                            
         CLI   BYPKGIND,1          TEST PACKAGE MASTER                          
         BNE   *+14                                                             
         MVC   12(16,R3),=C'*PACKAGE MASTER*'                                   
         B     OBUY2                                                            
         CLI   BYPKGIND,5          TEST REVISION MASTER                         
         BNE   *+14                                                             
         MVC   12(17,R3),=C'*REVISION MASTER*'                                  
         B     OBUY2                                                            
         CLI   BYPKGIND,2          TEST PACKAGE SLAVE                           
         BNE   *+14                                                             
         MVC   12(15,R3),=C'*PACKAGE SLAVE*'                                    
         B     OBUY2                                                            
         CLI   BYPKGIND,6          OR REVISION SLAVE                            
         BNE   OBUY3                                                            
         MVC   12(16,R3),=C'*REVISION SLAVE*'                                   
*                                                                               
OBUY2    LA    R3,198(R3)                                                       
         B     OBUY4                                                            
*                                                                               
OBUY3    TM    BYIND,BYICOM+BYIORB    TEST COMMENTS OR ORBITS                   
         BZ    OBUY4                                                            
         CLC   SBNDATES,=F'14'     AND MORE THAN 14 WEEKS                       
         BNH   OBUY4                                                            
         LA    R3,198(R3)          YES-LEAVE LINE FOR 2ND LINE OF SPOTS         
*                                                                               
OBUY4    TM    BYIND,BYICOM+BYIORB TEST NEED COMMENTS OR ORBITS                 
         BZ    OBUY14                                                           
         XC    KEY,KEY             YES-READ THE BUY RECORD                      
         MVC   KEY+14(4),BYRECDA                                                
         L     R5,AIO2                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,BDELEM-BUYRECD(R5)                                            
         SR    R0,R0                                                            
*                                                                               
OBUY6    CLI   0(R5),0             FIND COMMENT ELEMENTS                        
         BE    OBUY14                                                           
         CLI   0(R5),X'66'                                                      
         BNE   OBUY8                                                            
         USING COMELEM,R5                                                       
         ZIC   RE,CMLEN                                                         
         SH    RE,=Y(CMDATA-COMELEM)                                            
         BNP   OBUY12                                                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   22(0,R3),CMDATA                                                  
         LA    R3,198(R3)                                                       
*                                                                               
OBUY8    CLI   0(R5),X'67'                                                      
         BNE   OBUY12                                                           
         LR    R6,R5                                                            
         USING ORBELEM,R6                                                       
         ZIC   R2,1(R6)                                                         
         SH    R2,=Y(ORBDAY-ORBELEM)                                            
         SRL   R2,4                                                             
         LTR   R2,R2                                                            
         BZ    OBUY12                                                           
         MVC   22(7,R3),=C'*ORBIT*'                                             
         LA    R4,31(R3)                                                        
*                                                                               
OBUY10   DS    0H                                                               
         GOTO1 DAYUNPK,DMCB,ORBDAY,(R4)                                         
         GOTO1 UNTIME,DMCB,ORBTIME,9(R4)                                        
         MVC   21(7,R4),ORBDESC                                                 
         LA    R4,29(R4)                                                        
         LA    R6,16(R6)                                                        
         BCT   R2,OBUY10                                                        
*                                                                               
OBUY12   IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     OBUY6                                                            
         DROP  R5                                                               
*                                                                               
OBUY14   CLC   0(132,R3),BLANKS                                                 
         BNH   *+8                                                              
         LA    R3,198(R3)                                                       
         MVI   0(R3),0                                                          
*                                                                               
OBUYX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* HEADING ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
HBUY     MVC   0(40,R3),=C'DAY      TIME        PROGRAMMING      LN'            
         LA    R3,198(R3)                                                       
         MVC   0(41,R3),=C'---      ----        -----------      ---'           
         B     XIT                                                              
         SPACE 1                                                                
HSPOTS   ZIC   R5,GLARGS                                                        
         LA    R6,2                                                             
HSPOTS2  C     R5,SBNDATES                                                      
         BH    XIT                                                              
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         L     R2,SBADATE                                                       
         LA    R2,0(R1,R2)                                                      
         GOTO1 DATCON,DMCB,(2,(R2)),(4,DUB)                                     
         MVC   0(3,R3),DUB                                                      
         MVC   198+1(2,R3),DUB+3                                                
         LA    R5,14(R5)           IN CASE PERIOD GT 14 WEEKS                   
         LA    R3,198+198(R3)                                                   
         BCT   R6,HSPOTS2                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* TOTAL ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
TSTA     MVC   0(2,R3),=C'**'      STATION TOTAL                                
         MVC   3(8,R3),SVSTA                                                    
         MVC   12(8,R3),=C'TOTAL **'                                            
         MVI   21(R3),C'$'                                                      
         OC    DOLTOT,DOLTOT                                                    
         BNZ   *+14                                                             
         MVC   22(4,R3),=C'0.00'                                                
         B     TSTA2                                                            
         EDIT  DOLTOT,(11,22(R3)),2,ALIGN=LEFT                                  
*                                                                               
TSTA2    CLI   SBETAX,C'Y'         TEST TAX EXCLUDED                            
         BNE   XIT                                                              
         OC    TAXTOT,TAXTOT                                                    
         BZ    XIT                                                              
         MVI   198(R3),0           YES                                          
         LA    R3,198+198(R3)                                                   
         MVC   0(10,R3),=C'*** TAX OF'                                          
         EDIT  TAXTOT,(9,11(R3)),2                                              
         MVC   21(29,R3),=C'EXCLUDED FROM THIS REPORT ***'                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A REP RECORD                                         *         
* INPUT  : SBREP   = 3-CHAR REP CODE                                  *         
* OUTPUT : SBREPNM = REP NAME                                         *         
***********************************************************************         
         SPACE 1                                                                
GETREP   NTR1  ,                                                                
         LA    R6,KEY              READ REP RECORD                              
         USING REPRECD,R6                                                       
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(16),REPKEY                                              
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,SBQMED                                                   
         MVC   REPKREP,SBREP                                                    
         MVC   REPKAGY,AGENCY                                                   
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   SBREPNM,=CL24'* UNKNOWN *'                                       
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   *+16                                                             
         MVC   SBREPNM,BLANKS                                                   
         MVC   SBREPNM(L'RNAME),RNAME   EXTRACT REP NAME                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DRIVER ABOUT TO BE CALLED FOR OUTPUT                                *         
***********************************************************************         
         SPACE 1                                                                
OUT      L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVI   MAXLINES,60                                                      
         CLI   MAXLPP,0            ALTER MAX LPP IF REQUESTED                   
         BE    *+10                                                             
         MVC   MAXLINES,MAXLPP                                                  
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DRIVER HEADHOOK                                                     *         
***********************************************************************         
         SPACE 1                                                                
HEAD     L     R2,AH1              ADD 8 HOURS TO GET MILITARY TIME             
         CLI   20(R2),C' '                                                      
         BNE   *+8                                                              
         MVI   20(R2),C'0'                                                      
         CLI   21(R2),C' '                                                      
         BNE   *+8                                                              
         MVI   21(R2),C'0'                                                      
         PACK  DUB,20(2,R2)                                                     
         AP    DUB,=P'8'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  20(2,R2),DUB                                                     
         CLI   20(R2),C'0'                                                      
         BNE   *+8                                                              
         MVI   20(R2),C' '                                                      
*                                                                               
         LR    R3,R2               MOVE CENTER TITLES TO THE LEFT               
         MVC   32(63,R3),BLANKS                                                 
         MVC   25(63,R3),TITLE                                                  
         A     R3,PWIDTH                                                        
         MVC   32(63,R3),BLANKS                                                 
         GOTO1 UNDERLIN,DMCB,(63,TITLE),25(R3)                                  
         A     R3,PWIDTH                                                        
         MVC   48(32,R3),BLANKS                                                 
         MVC   41(32,R3),SUBTITLE                                               
*                                                                               
         MVC   75(33,R2),USERNAME  AGENCY NAME                                  
         A     R2,PWIDTH                                                        
         MVC   75(33,R2),USERADDR  AND ADDRESS                                  
*                                                                               
         MVC   11(L'QBYRNM,R2),QBYRNM    BUYER NAME                             
         L     R2,AH4              MARKET                                       
         A     R2,PWIDTH                                                        
         MVC   48(6,R2),=C'MARKET'                                              
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  55(4,R2),DUB                                                     
         MVC   60(L'SBMKTNM,R2),SBMKTNM                                         
*                                                                               
         A     R2,PWIDTH                                                        
         OC    PREDATE,PREDATE     TEST VERSION 2 OR MORE                       
         BZ    HD2                                                              
         LR    R3,R2                                                            
         A     R3,PWIDTH                                                        
         MVC   41(11,R3),STARS                                                  
         MVC   53(7,R3),=C'REVISED'                                             
         MVC   61(11,R3),STARS                                                  
*                                                                               
HD2      MVC   80(12,R2),=C'**** VERSION'                                       
         MVC   93(3,R2),CURVER                                                  
         MVC   97(4,R2),=C'****'                                                
         OC    PREDATE,PREDATE                                                  
         BZ    HEADX                                                            
         A     R2,PWIDTH                                                        
         MVC   80(12,R2),=C'REPLACES VER'                                       
         MVC   93(3,R2),PREVER                                                  
         MVC   97(2,R2),=C'OF'                                                  
         MVC   100(8,R2),PREDATE                                                
*                                                                               
HEADX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER ABOUT TO PRINT A LINE                                        *         
***********************************************************************         
         SPACE 1                                                                
PRINT    L     R1,GLAINTP1         LOOK FOR PRINTABLE LINES                     
         LA    R0,20                                                            
         CLC   0(198,R1),BLANKS                                                 
         BH    *+16                                                             
         LA    R1,198(R1)                                                       
         BCT   R0,*-14                                                          
         B     XIT                                                              
         MVI   ACTSW,C'Y'          INDICATE PRINT ACTIVITY                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FINAL HOOK                                                          *         
***********************************************************************         
         SPACE 1                                                                
FINAL    CLI   ACTSW,C'Y'          BLOW UP IF NO PRINT ACTIVITY                 
         BE    XIT                                                              
         DC    H'0',C'NO DATA TO REPORT'                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
TAXTOT   DS    F                                                                
DOLTOT   DS    F                                                                
*                                                                               
BYREC    DS    0CL35                                                            
BYLINE1  DS    XL1                                                              
BYLINE2  DS    XL1                                                              
BYDAYS   DS    XL1                                                              
BYTIMES  DS    XL4                                                              
BYPROG   DS    CL16                                                             
BYLEN    DS    XL1                                                              
BYCOST   DS    XL4                                                              
BYRECDA  DS    XL4                                                              
BYIND    DS    XL1                                                              
BYICOM   EQU   X'80'                                                            
BYIORB   EQU   X'40'                                                            
BYPKGIND DS    XL1                                                              
BYSEDAY  DS    XL1                                                              
*                                                                               
QBYR     DS    CL3                                                              
QBYRNM   DS    CL12                                                             
QCAM     DS    XL2                                                              
CURVER   DS    CL3                                                              
PREVER   DS    CL3                                                              
PREDATE  DS    CL8                                                              
SVSTA    DC    CL8' '                                                           
MAXLPP   DS    XL1                                                              
DAILY    DS    CL1                                                              
ACTSW    DS    CL1                                                              
BUYKEYSV DS    XL13                                                             
CPPROF   DS    XL16                                                             
*                                                                               
ZEROS    DC    CL8'00000000'                                                    
BLANKS   DC    CL198' '                                                         
STARS    DC    CL16'****************'                                           
*                                                                               
RPTSPECS DS    0C                  REPORT SPECS                                 
         SSPEC H1,2,RUN                                                         
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,81,REPORT                                                     
         SSPEC H4,94,PAGE                                                       
         DC    X'00'                                                            
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*SPGENEST                                                                       
*SPGENBUY                                                                       
*SPGENREP                                                                       
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRID1D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046SPWRI15   12/06/04'                                      
         END                                                                    
