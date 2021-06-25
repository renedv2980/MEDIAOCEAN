*          DATA SET SPWRI08    AT LEVEL 043 AS OF 10/16/06                      
*PHASE T20408A                                                                  
         TITLE 'T20408 - ALLOCATION ANALYSIS REPORT'                            
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI08 (T20408) - ALLOCATION ANALYSIS REPORT            *           
*                                                                   *           
* *** NOTE NOTE NOTE *** I DON'T KNOW IF THIS IS USED ANYMORE       *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 03NOV03 18 AKT -- FIX MGROUP X'40' BUGS                           *           
* 14OCT02 41 EFJ -- 2 CHAR MGR SCHEME CODES                         *           
* 14OCT02 HISTORY LOST                                              *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
T20408   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20408,RA                                                  
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
         CLI   RPMODE,RPINPUT      INPUT MODE                                   
         BE    INPUT                                                            
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
         MVI   SBQSEPES,C'N'       ENSURE NOT EST=ALL                           
         OI    SBQDATA,SBQDGOAL+SBQDPUR  SET DATA                               
         OI    SBQPER,SBQPQT+SBQPWK      QUARTERS AND WEEKS                     
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         OI    DATAIND2,DIEST      ESTIMATE                                     
         MVI   WIDTHOPT,C'W'       WIDE REPORT                                  
         XC    AQTS,AQTS                                                        
*                                                                               
         XC    BUFFL,BUFFL                                                      
*                                                                               
         LA    R2,ALABYTH          VALIDATE CASH/TRADE OPTION                   
         MVI   BYTYPOPT,C'N'                                                    
         CLI   5(R2),0                                                          
         BE    INIT1                                                            
         MVC   BYTYPOPT,8(R2)                                                   
         CLI   BYTYPOPT,C'Y'                                                    
         BE    INIT1                                                            
         CLI   BYTYPOPT,C'N'                                                    
         BNE   EINV                                                             
*                                                                               
INIT1    CLI   SBQPGRD,C' '        SET LEVELS                                   
         BH    *+14                                                             
         XC    RPTLEVS+1(2),RPTLEVS+1  SET PRODUCT GROUPS                       
         B     INIT2                                                            
         CLC   SBPGR1LN,SBPGR2LN                                                
         BNE   INIT2                                                            
         MVI   RPTLEVS+2,0                                                      
*                                                                               
INIT2    CLI   SBQMGRD,0           SET MARKET GROUPS                            
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
INIT4    CLI   BYTYPOPT,C'Y'                                                    
         BE    *+8                                                              
         MVI   RPTLEVS+4,0                                                      
*                                                                               
         LA    R1,LEVELS           SET THE LEVELS                               
         LA    RE,RPTLEVS                                                       
         LA    RF,1                                                             
INIT6    CLI   0(RE),X'FF'                                                      
         BE    INIT10                                                           
         CLI   0(RE),0                                                          
         BE    INIT8                                                            
         MVC   0(1,R1),0(RE)                                                    
         CLI   0(R1),QMKT                                                       
         BNE   *+12                                                             
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         STC   RF,MKTLEV           MARKET LEVEL                                 
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
INIT8    LA    RE,1(RE)                                                         
         B     INIT6                                                            
*                                                                               
INIT10   MVI   MYFIRSTH,11         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
*                                                                               
         LA    R2,ALATITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT18   GOTO1 CENTER,DMCB,TITLE,64                                             
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
RPTLEVS  DC    AL1(QCLT)           HEADLINES                                    
         DC    AL1(QPRDGR1)                                                     
         DC    AL1(QPRDGR2)                                                     
         DC    AL1(QEST)                                                        
         DC    AL1(QBYTYPE)                                                     
         DC    AL1(QMKTGR1)                                                     
         DC    AL1(QMKTGR2)                                                     
         DC    AL1(QMKTGR3)                                                     
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QMKT)           LAST HEADLINE                                
         DC    AL1(QPER)                                                        
         DC    AL1(QPRD)           DETAIL                                       
         DC    X'FF'                                                            
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    LA    R2,ALAPRDH                                                       
         CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BNE   *+12                NO                                           
         MVI   SBQBPRD,0           YES-BREAK OUT THE PRODUCTS                   
         OI    SBQPIND,SBQPOLSP                                                 
         B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
         EJECT                                                                  
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCPR                                                  
         BE    PRODUCT                                                          
         B     XIT                                                              
         EJECT                                                                  
* PRODUCT FIRST                                                                 
*                                                                               
PRODUCT  OC    AQT1,AQT1           TEST WEEK TABLE QUARTER POINTERS             
         BNZ   PRDX                SET YET                                      
         LA    R0,5                                                             
         LA    R1,AQT1                                                          
         L     R5,AWEEKS                                                        
         L     R6,AQTRS                                                         
*                                                                               
PRD1     OC    0(4,R6),0(R6)                                                    
         BZ    PRD4                                                             
         CLC   0(2,R5),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R5,0(R1)                                                         
         LA    RE,1                                                             
*                                                                               
PRD2     OC    0(4,R5),0(R5)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   2(2,R5),2(R6)                                                    
         BNH   *+6                                                              
         DC    H'0'                                                             
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         LA    R5,4(R5)                                                         
         B     PRD2                                                             
         STC   RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         BCT   R0,PRD1                                                          
*                                                                               
PRD4     LA    R0,5                BUILD QUARTER DATES LISTS                    
         LA    R2,DATLST8                                                       
         LA    R3,DATLST5                                                       
         L     R5,AQTRS                                                         
*                                                                               
PRD5     OC    0(4,R5),0(R5)                                                    
         BZ    PRDX                                                             
         GOTO1 DATCON,DMCB,(2,0(R5)),(4,0(R3))                                  
         GOTO1 (RF),(R1),(2,2(R5)),(4,5(R3))                                    
         CLI   QPERTYPE,2                                                       
         BE    PRD6                                                             
         GOTO1 (RF),(R1),(2,0(R5)),(8,0(R2))                                    
         GOTO1 (RF),(R1),(2,2(R5)),(8,8(R2))                                    
         B     PRD7                                                             
*                                                                               
PRD6     GOTO1 (RF),(R1),(2,0(R5)),(6,0(R2))                                    
         GOTO1 (RF),(R1),(2,2(R5)),(6,8(R2))                                    
*                                                                               
PRD7     LA    R2,16(R2)                                                        
         LA    R3,10(R3)                                                        
         LA    R5,4(R5)                                                         
         BCT   R0,PRD5                                                          
*                                                                               
PRDX     B     XIT                                                              
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
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
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
         DC    CL8'OMKTUSR ',A(OMKT)                                            
         DC    CL8'IESTUSR ',A(IEST)                                            
         DC    CL8'IQTRUSR ',A(IQTR)                                            
         DC    CL8'OQTRUSR ',A(OQTR)                                            
         DC    CL8'ICOLUSR1',A(ICOL1)                                           
         DC    CL8'ICOLUSR2',A(ICOL2)                                           
         DC    CL8'OCOLUSR1',A(OCOL1)                                           
         DC    CL8'OCOLUSR2',A(OCOL2)                                           
         DC    CL8'HCOLUSR ',A(HCOL)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  MVI   GLPUTMAX,20         DRIVPUT MAX=20                               
         MVC   GLOPTS+2(1),BYTYPOPT  CASH/TRADE OPTION                          
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
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
         L     R1,SBACURCH                                                      
         CLI   SBMODE,SBPROCSP                                                  
         BNE   EXEC2                                                            
         USING SCHUNKD,R1                                                       
         L     R5,SCGROSS          R5=DOLLAR VALUE                              
         LA    R6,SCDATE           R6=A(CHUNK DATES)                            
         B     EXECX                                                            
         DROP  R1                                                               
*                                                                               
EXEC2    CLI   SBMODE,SBPROCGL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SGLCHNKD,R1                                                      
         L     R5,SGDOL                                                         
         LA    R6,SGDATE                                                        
         DROP  R1                                                               
*                                                                               
EXECX    L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
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
* MARKET OUTPUT ROUTINE                                                         
*                                                                               
OMKT     LA    R4,MKTHEAD          FORMAT MKT HEAD TO TEMP SAVE AREA            
         MVC   MKTHEAD,SPACES                                                   
         MVC   0(6,R4),=C'MARKET'                                               
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0                                          
         MVC   7(4,R4),SBMKT                                                    
         GOTO1 GETMKTNM                                                         
         MVC   12(L'SBMKTNM,R4),SBMKTNM                                         
         LA    R1,L'MKTHEAD                                                     
         ST    R1,DMCB+4                                                        
         GOTO1 SQUASHER,DMCB,(R4)                                               
         B     XIT                                                              
         EJECT                                                                  
* ESTIMATE INPUT ROUTINE                                                        
*                                                                               
IEST     MVC   1(1,R2),ESTSTART                                                 
         MVI   2(R2),1                                                          
         B     XIT                                                              
         SPACE 2                                                                
* QUARTER I/O ROUTINES                                                          
*                                                                               
IQTR     LA    R0,NQTRS                                                         
         L     R1,AQTRS                                                         
         LA    RF,AQT1                                                          
         LA    RE,1                                                             
*                                                                               
IQTR2    OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R6),0(R1)                                                    
         BL    *+14                                                             
         CLC   0(2,R6),2(R1)                                                    
         BNH   IQTR4                                                            
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,IQTR2                                                         
         DC    H'0'                                                             
*                                                                               
IQTR4    STC   RE,0(R2)                                                         
         L     RF,0(RF)                                                         
         ST    RF,THISQTR                                                       
         B     XIT                                                              
         SPACE 2                                                                
OQTR     ZIC   RE,0(R2)            GET QUARTER NUMBER                           
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    R1,AQT1(RE)                                                      
         MVC   THISQTR,0(R1)       SET N'WEEKS IN THE QUARTER                   
*                                  AND A(QUARTER START IN WEEK TABLE)           
         MVC   QTRNUM,0(R2)        SAVE THIS QTR NUMBER                         
         B     XIT                                                              
         EJECT                                                                  
* COST AND COST INDEX INPUT/OUTPUT ROUTINES                                     
*                                                                               
ICOL1    XC    0(8,R2),0(R2)                                                    
         CLI   GLARGS,255          TEST ALL WEEKS                               
         BE    ICOL11                                                           
         CLC   GLARGS(1),THISQTR   TEST THERE ARE THIS MANY WKS IN QTR          
         BH    ICOL1X              NO                                           
         ZIC   RE,GLARGS           YES-TEST DATA IS FOR THIS WEEK               
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         L     R1,THISQTR                                                       
         AR    R1,RE                                                            
         CLC   0(2,R6),0(R1)                                                    
         BNE   ICOL1X                                                           
*                                                                               
ICOL11   LTR   R5,R5               TEST ANY DATA                                
         BZ    ICOL13                                                           
         XC    BFKEY,BFKEY         YES-SET BUFFALO KEY                          
         MVC   BFPRD,SBBPRD                                                     
         MVC   BFMKT,SBBMKT                                                     
         MVC   BFWEEK,0(R6)                                                     
         CLI   SBMODE,SBPROCSP     TEST BUY RECORD                              
         BNE   ICOL12                                                           
         CLI   GLRECNO,1           YES-ON FIRST REPORT,                         
         BNE   ICOL13                                                           
         CLI   GLARGS,255          TEST 'ALL WEEKS'                             
         BE    ICOL13              YES                                          
         OC    BUFFL,BUFFL         NO-TEST BUFFALO INITIALIZED                  
         BNZ   *+8                                                              
         BAS   RE,INITBUFF                                                      
         BAS   RE,PUTBUF           PUT PRD/MKT/WK TO BUFFALO                    
         B     ICOL13                                                           
*                                                                               
ICOL12   BAS   RE,HIBUF            GOAL RECORD-TEST ANY BUY DATA                
         BNE   ICOL1X              NO                                           
         CLC   BFKEY,SVBFKEY                                                    
         BNE   ICOL1X              NO-THEN IGNORE GOALS                         
*                                                                               
ICOL13   CLI   SBMODE,SBPROCSP                                                  
         BE    *+8                                                              
         LA    R2,4(R2)                                                         
         ST    R5,0(R2)            MOVE THE DOLLAR VALUE                        
         B     ICOL1X                                                           
*                                                                               
ICOL1X   OC    0(8,R2),0(R2)       TEST ANY DATA                                
         BZ    XIT                 NO                                           
         MVI   INDATA,1            YES-SET DATA INDICATOR                       
         B     XIT                                                              
*                                                                               
*                                                                               
OCOL1    CLI   GLRECNO,2           TEST 'ALL QUARTERS' REPORT                   
         BE    OCOL130                                                          
         CLI   GLARGS,1            NO-TEST WEEK 1                               
         BNE   *+8                                                              
         MVI   ANYDATA,C'N'        YES-RESET DATA SWITCH                        
         CLI   GLARGS,255                                                       
         BE    *+14                                                             
         CLC   GLARGS(1),THISQTR   TEST THERE ARE THIS MANY WKS IN QTR          
         BH    OCOL1X                                                           
         CLI   GLRECNO,3           TEST TOTALING NOW                            
         BNE   OCOL16                                                           
         ICM   RE,15,0(R2)         YES-PRINT INDEX ONLY                         
         BZ    *+8                                                              
         MVI   ANYDATA,C'Y'                                                     
         SRDA  RE,32                                                            
         ICM   R1,15,4(R2)         R1=GOAL COST                                 
         BZ    OCOL12                                                           
         M     RE,=F'20000'        INDEX ACTUAL TO GOAL                         
         DR    RE,R1                                                            
         LA    R1,1(RF)                                                         
         SRA   R1,1                                                             
*                                                                               
OCOL12   CLI   GLARGS,255                                                       
         BE    OCOL14                                                           
         EDIT  (R1),(9,0(R3)),2    EDIT THE INDEX                               
         B     OCOL1X                                                           
*                                                                               
OCOL14   EDIT  (R1),(11,0(R3)),2                                                
         B     OCOL1X                                                           
*                                                                               
OCOL16   ICM   R0,15,0(R2)         R0=ACTUAL COST (PENNIES)                     
         BNZ   OCOL18                                                           
         CLI   GLARGS,255                                                       
         BE    *+12                                                             
         MVI   4(R3),C'0'                                                       
         B     OCOL120                                                          
         MVI   5(R3),C'0'                                                       
         B     OCOL120                                                          
*                                                                               
OCOL18   MVI   ANYDATA,C'Y'        THERE IS SIGNIFICANT DATA                    
         SRDA  R0,32                                                            
         SLDA  R0,1                                                             
         LR    RE,R0                                                            
         LR    RF,R1                                                            
         D     R0,=F'100'          ROUND TO DOLLARS                             
         LTR   R1,R1                                                            
         BM    *+8                                                              
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         CLI   GLARGS,255                                                       
         BE    OCOL112                                                          
         LTR   R1,R1                                                            
         BM    OCOL110                                                          
         EDIT  (R1),(5,(R3))       EDIT ACTUAL COST                             
         B     OCOL116                                                          
OCOL110  EDIT  (R1),(5,(R3)),MINUS=YES                                          
         B     OCOL116                                                          
*                                                                               
OCOL112  LTR   R1,R1                                                            
         BM    OCOL114                                                          
         EDIT  (R1),(6,(R3))                                                    
         B     OCOL116                                                          
OCOL114  EDIT  (R1),(6,(R3)),MINUS=YES                                          
*                                                                               
OCOL116  ICM   R1,15,4(R2)         R1=GOAL COST                                 
         BZ    OCOL122                                                          
         M     RE,=F'100'          INDEX ACTUAL TO GOAL                         
         DR    RE,R1                                                            
         LTR   RF,RF                                                            
         BZ    OCOL120                                                          
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         CH    RF,=H'1000'                                                      
         BNL   OCOL122                                                          
         CLI   GLARGS,255                                                       
         BE    OCOL118                                                          
         EDIT  (RF),(3,6(R3))      EDIT THE INDEX                               
         B     OCOL1X                                                           
OCOL118  EDIT  (RF),(3,7(R3))                                                   
         B     OCOL1X                                                           
*                                                                               
OCOL120  CLI   GLARGS,255          INDEX=0                                      
         BNE   *+8                                                              
         LA    R3,1(R3)                                                         
         MVI   8(R3),C'0'                                                       
         B     OCOL1X                                                           
OCOL122  CLI   GLARGS,255          INDEX=HIGH                                   
         BNE   *+8                                                              
         LA    R3,1(R3)                                                         
         MVC   7(2,R3),=C'HI'                                                   
         B     OCOL1X                                                           
*                                                                               
OCOL130  L     RE,0(R2)            'ALL QUARTERS' REPORTS                       
         EDIT  (RE),(12,(R3)),2,MINUS=YES                                       
         SRDA  RE,32                                                            
         ICM   R1,15,4(R2)                                                      
         BZ    OCOL132                                                          
         M     RE,=F'20000'                                                     
         DR    RE,R1                                                            
         LA    R1,1(RF)                                                         
         SRA   R1,1                                                             
OCOL132  EDIT  (R1),(8,13(R3)),2                                                
         B     OCOL1X                                                           
*                                                                               
OCOL1X   CLI   GLARGS,255          TEST LAST COLUMN                             
         BNE   XIT                                                              
         CLI   ANYDATA,C'Y'        YES-TEST ANY SIGNIFICANT DATA                
         BE    XIT                                                              
         MVI   PRTSW,C'N'              NO-SUPPRESS THE PRINT LINE               
         B     XIT                                                              
         SPACE 2                                                                
ICOL2    ZAP   0(8,R2),=P'0'       ALL MARKETS/ALL QUARTERS                     
         ZAP   8(8,R2),=P'0'                                                    
         LTR   R5,R5               TEST ANY DATA                                
         BZ    XIT                                                              
         CLI   SBMODE,SBPROCGL     YES-TEST GOAL RECORD                         
         BNE   ICOL22                                                           
         XC    BFKEY,BFKEY         YES-SET BUFFALO KEY                          
         MVC   BFPRD,SBBPRD                                                     
         MVC   BFMKT,SBBMKT                                                     
         MVC   BFWEEK,0(R6)                                                     
         BAS   RE,HIBUF            TEST ANY BUY DATA                            
         BNE   XIT                 NO                                           
         CLC   BFKEY,SVBFKEY                                                    
         BNE   XIT                 NO-THEN IGNORE GOALS                         
*                                                                               
ICOL22   MVI   INDATA,1                                                         
         CVD   R5,DUB                                                           
         CLI   SBMODE,SBPROCSP                                                  
         BE    *+8                                                              
         LA    R2,8(R2)                                                         
         MVC   0(8,R2),DUB                                                      
         B     XIT                                                              
*                                                                               
OCOL2    ZAP   DUB,0(8,R2)                                                      
         EDIT  (P8,DUB),(12,(R3)),2,MINUS=YES                                   
         ZAP   BIG,DUB                                                          
         ZAP   DUB,8(8,R2)                                                      
         CP    DUB,=P'0'                                                        
         BZ    OCOL22                                                           
         MP    BIG,=PL3'20000'                                                  
         DP    BIG,DUB                                                          
         ZAP   DUB,BIG(8)                                                       
         AP    DUB,=P'1'                                                        
         ZAP   BIG,DUB                                                          
         DP    BIG,=PL8'2'                                                      
         ZAP   DUB,BIG(8)                                                       
OCOL22   EDIT  (P8,DUB),(8,13(R3)),2                                            
         B     OCOL1X                                                           
         EJECT                                                                  
* COLUMN HEADLINE ROUTINE                                                       
*                                                                               
HCOL     MVC   0(9,R3),SPACES                                                   
         CLI   GLARGS,255          TEST QUARTER TOTAL                           
         BNE   HCOL2                                                            
         ZIC   R5,QTRNUM                                                        
         BCTR  R5,0                                                             
         MH    R5,=H'10'                                                        
         LA    R5,DATLST5(R5)                                                   
         MVC   0(5,R3),0(R5)                                                    
         MVI   5(R3),C'-'                                                       
         MVC   6(5,R3),5(R5)                                                    
         B     HCOL4                                                            
*                                                                               
HCOL2    CLC   GLARGS(1),THISQTR   TEST THIS NUMBER OF WKS IN THIS QTR          
         BH    XIT                 NO                                           
         L     R5,THISQTR                                                       
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         AR    R5,RE                                                            
         GOTO1 DATCON,DMCB,(2,(R5)),(7,2(R3))                                   
*                                                                               
HCOL4    LA    R3,198(R3)                                                       
         MVC   0(9,R3),SPACES                                                   
         L     R1,GLATHID                                                       
         CLI   GLRECNO,3           TEST TOTALING NOW                            
         BNE   HCOL6                                                            
         MVC   2(5,R3),=C'INDEX'   YES                                          
         B     HCOLX                                                            
*                                                                               
HCOL6    CLI   GLARGS,255                                                       
         BNE   *+8                                                              
         LA    R3,1(R3)                                                         
         MVC   1(4,R3),=C'COST'                                                 
         MVC   6(3,R3),=C'NDX'                                                  
*                                                                               
HCOLX    B     XIT                                                              
         EJECT                                                                  
* DRIVER HEADHOOK                                                               
*                                                                               
HEAD     LA    R5,HEADTAB                                                       
         USING HEADTABD,R5                                                      
*                                                                               
HD2      CLI   0(R5),X'FF'                                                      
         BE    HD6                                                              
         CLC   HDNMGR,GLOPTS+1     N'MARKET GROUPS                              
         BE    *+12                                                             
         LA    R5,HEADTABL(R5)                                                  
         B     HD2                                                              
         SR    R2,R2                                                            
         ICM   R2,1,HDMGR1                                                      
         BZ    HD4                                                              
         LA    R3,DWIDMGR                                                       
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
         LA    R3,DWIDMKT                                                       
         BAS   RE,HDPOS                                                         
         CLI   GLRECNO,3           TEST TOTALING NOW                            
         BL    *+14                                                             
         MVC   3(19,RF),=C'*** ALL MARKETS ***'                                 
         B     HD6                                                              
         MVC   0(L'MKTHEAD,RF),MKTHEAD                                          
*                                                                               
HD6      CLI   GLRECNO,2           TEST ALL QUARTERS                            
         BE    HDX                                                              
         CLI   GLRECNO,4                                                        
         BE    HDX                                                              
         CLI   QPERTYPE,2                                                       
         BNE   *+14                                                             
         CLC   SBQSTART(4),SBQEND                                               
         BE    HDX                                                              
         ZIC   R5,QTRNUM           NO-FUDGE PERIOD HEADLINE TO                  
         BCTR  R5,0                   ONLY THIS QUARTER                         
         SLL   R5,4                                                             
         LA    R5,DATLST8(R5)                                                   
         L     R1,AH1                                                           
         A     R1,PWIDTH                                                        
         A     R1,PWIDTH                                                        
         LA    R1,64(R1)                                                        
         CLI   QPERTYPE,2                                                       
         BE    HD7                                                              
         MVC   12(8,R1),0(R5)                                                   
         MVC   24(8,R1),8(R5)                                                   
         B     HDX                                                              
*                                                                               
HD7      MVC   14(6,R1),0(R5)                                                   
         MVC   24(6,R1),8(R5)                                                   
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
* ROUTINE TO INITIALIZE BUFFALO                                                 
*                                                                               
INITBUFF NTR1                                                                   
         L     R4,=A(BUFFALOC)                                                  
         B     INITBF2                                                          
         GOTO1 COVAIL,DMCB,C'LOOK'                                              
         DC    H'0'                                                             
*                                                                               
INITBF2  GOTO1 COVAIL,DMCB,C'SETB',20000,380000,(R4)                            
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ABUFF,4(R1)                                                      
         MVC   BUFFL,8(R1)                                                      
         MVC   ABUFFC,12(R1)                                                    
         BAS   RE,SETBUF                                                        
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
SETBUF   LA    R1,=C'SET'                                                       
         B     BUFX                                                             
*                                                                               
PUTBUF   LA    R1,=C'PUT'                                                       
         B     BUFX                                                             
*                                                                               
HIBUF    MVC   SVBFKEY,BFKEY                                                    
         LA    R1,=C'HIGH'                                                      
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
         DS    0D                                                               
BIG      DS    PL16                                                             
*                                                                               
AQTS     DS    0XL20                                                            
AQT1     DS    A                                                                
AQT2     DS    A                                                                
AQT3     DS    A                                                                
AQT4     DS    A                                                                
AQT5     DS    A                                                                
THISQTR  DS    A                                                                
*                                                                               
ABUFF    DS    A                                                                
ABUFFC   DS    A                                                                
BUFFL    DS    F                                                                
*                                                                               
BFREC    DS    0CL5                                                             
BFKEY    DS    0CL5                                                             
BFPRD    DS    XL1                                                              
BFMKT    DS    XL2                                                              
BFWEEK   DS    XL2                                                              
*                                                                               
QTRNUM   DS    XL1                                                              
DATLST8  DS    5CL16                                                            
DATLST5  DS    5CL10                                                            
*                                                                               
ANYDATA  DS    CL1                                                              
BYTYPOPT DS    CL1                                                              
MGR1HEAD DS    CL44                                                             
MGR2HEAD DS    CL44                                                             
MGR3HEAD DS    CL44                                                             
MKTHEAD  DS    CL36                                                             
*                                                                               
SVBFKEY  DS    CL(L'BFKEY)                                                      
*                                                                               
PATCH    DC    XL32'00'                                                         
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
         EJECT                                                                  
* HEADLINE POSITION TABLE                                                       
*                                                                               
HEADTAB  DC    X'00',X'000000',X'05'                                            
         DC    X'01',X'050000',X'07'                                            
         DC    X'02',X'050600',X'08'                                            
         DC    X'03',X'050607',X'08'                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
HEADTABD DSECT                                                                  
HDNMGR   DS    X                   N'MARKET GROUPS                              
HDMGR1   DS    X                   HEADLINE FOR MARKET GROUP 1                  
HDMGR2   DS    X                   HEADLINE FOR MARKET GROUP 2                  
HDMGR3   DS    X                   HEADLINE FOR MARKET GROUP 3                  
HDMKT    DS    X                   HEADLINE FOR MARKET                          
HEADTABL EQU   *-HEADTABD                                                       
         SPACE 1                                                                
* HEADLINE DISPLACEMENTS                                                        
*                                                                               
DWIDMGR  EQU   64                  WIDE-    MARKET GROUP                        
DWIDMKT  EQU   67                           MARKET                              
DREGMGR  EQU   48                  REGULAR- MARKET GROUP                        
DREGMKT  EQU   51                           MARKET                              
         EJECT                                                                  
T20408   CSECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
* BUFFALO CSECT                                                                 
*                                                                               
         BUFF  LINES=1,FLAVOR=DATA,KEYLIST=(5,A)                                
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
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE6D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPWRI08   10/16/06'                                      
         END                                                                    
