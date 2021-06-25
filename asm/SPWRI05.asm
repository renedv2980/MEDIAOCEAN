*          DATA SET SPWRI05    AT LEVEL 063 AS OF 12/15/04                      
*PHASE T20405A,*                                                                
         TITLE 'T20405 - SPOT WRITER RADIO ROTATION SCHEDULE'                   
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI05 (T20405) - RADIO ROTATION SCHEDULE REPORT        *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 11MAR04 63 AKT -- 2 CHAR MGR X'40' BUG FIX                        *           
* 14OCT02 62 EFJ -- 2 CHAR MGR SCHEME CODES                         *           
* 02JUL96 60 EFJ -- SUPPORT NEW INVOICES                            *           
*                -- CHANGE EPERM LABEL TO AVOID CONFLICT            *           
* 02JUL96 HISTORY LOST                                              *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
T20405   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20405,RA,R8                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
         MVC   VBUFFALO,BUFFALO                                                 
         DROP  R6                                                               
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     XC    SVINVKEY,SVINVKEY                                                
         OI    SBQSKIP,SBQSKBIL+SBQSKGL+SBQSKBUY                                
         OI    SBQREAD,SBQRDINV                                                 
         MVI   SBQMKTWT,C'N'                                                    
         CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BNE   *+12                                                             
         MVI   SBQBPRD,0           YES-REPORT POL PRODUCTS SEPARATELY           
         OI    SBQPIND,SBQPOLSP                                                 
         XC    BUFFL,BUFFL                                                      
         MVI   ESTFRST,C'Y'                                                     
*                                                                               
         CLI   SBQPGRD,C' '        SET LEVELS                                   
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
INIT4    LA    R1,LEVELS           SET THE LEVELS                               
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
         CLI   0(R1),QSTA                                                       
         BNE   *+8                                                              
         STC   RF,MIDLEV           MIDLINE LEVEL                                
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
INIT8    LA    RE,1(RE)                                                         
         B     INIT6                                                            
*                                                                               
INIT10   MVI   MYFIRSTH,12         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
*                                  YES-VALIDATE OPTIONS                         
*                                                                               
INIT14   LA    R2,WIRTITH          VALIDATE TITLE                               
         MVC   TITLE,BLANKS                                                     
         MVC   TITLE(28),=C'SPOT RADIO ROTATION SCHEDULE'                       
         CLI   5(R2),0                                                          
         BE    INIT18                                                           
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT18   GOTO1 CENTER,DMCB,TITLE,64                                             
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
         DC    AL1(QMKT)                                                        
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QSTA)           MIDLINE                                      
         DC    AL1(QDPT)           DETAIL                                       
         DC    X'FF'                                                            
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    LA    R2,WIRPERH          VALIDATE REQUEST PERIOD                      
         OI    SBQPER,SBQPWK                                                    
*                                                                               
*                                                                               
*  SETDATE CANNOT BE CALLED ONLINE WITH NEW WRI CHANGES (EJOR 7/94)             
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VALID2                                                           
         GOTO1 SETDATE             SET WEEK TABLE NOW                           
         B     VALID3                                                           
*                                                                               
VALID2   XC    WORK,WORK           BUILD DATE TABLES                            
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
*                                                                               
         L     R4,AWEEKS           BUILD WEEKS                                  
         GOTO1 MOBILE,DMCB,(55,SBQSTART),(4,(R4)),WORK,SBSPPROF                 
         CLI   0(R4),X'FF'                                                      
         BE    *+12                                                             
         LA    R4,4(R4)                                                         
         B     *-12                                                             
         MVI   0(R4),0                                                          
*                                                                               
VALID3   SR    R5,R5               COUNT THE WEEKS                              
         L     RE,AWEEKS                                                        
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         BCT   R5,*-12                                                          
         LPR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R5,NUMWKS                                                        
         CLI   NUMWKS,6            TEST MORE THAN 6 WEEKS                       
         BH    EPER                YES-ERROR                                    
         CLC   SBQEST,SBQESTND     TEST SINGLE ESTIMATE REQUEST                 
         BNE   *+12                                                             
         CLI   NUMWKS,5            YES-CHECK NOT MORE THAN 5 WEEKS              
         BH    EPER                                                             
*                                                                               
         XC    BUYNAME,BUYNAME                                                  
         LA    R2,WIRNM1H          TEST BUYER NAME ENTERED                      
         CLI   5(R2),0                                                          
         BE    VALID4              NO                                           
         GOTO1 ANY                 YES-VALIDATE                                 
         CLI   WORK,C'='           MUST START WITH C'='                         
         BNE   VALID4                                                           
         MVC   BUYNAME,WORK+1                                                   
*                                                                               
VALID4   XC    BILNAME,BILNAME                                                  
         LA    R2,WIRNM2H          TEST BILLER NAME ENTERED                     
         CLI   5(R2),0                                                          
         BE    VALIDX              NO                                           
         GOTO1 ANY                 YES-VALIDATE                                 
         CLI   WORK,C'='           MUST START WITH C'='                         
         BNE   VALIDX                                                           
         MVC   BILNAME,WORK+1                                                   
*                                                                               
VALIDX   B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
EPER     MVC   CONHEAD(L'EPERMESS),EPERMESS                                     
         B     MYCURSOR                                                         
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
EPERMESS DC    C'MAXIMUN REQUEST PERIOD IS 5 WEEKS'                             
         EJECT                                                                  
* SPOTIO HOOKS                                                                  
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCCL                                                  
         BE    CLIENT                                                           
         CLI   SBMODE,SBPROCES                                                  
         BE    ESTIMATE                                                         
         CLI   SBMODE,SBPROCNV                                                  
         BE    PROCINV                                                          
         B     XIT                                                              
         EJECT                                                                  
* CLIENT FIRST                                                                  
*                                                                               
CLIENT   LA    R1,SBBQSTP          SET SPOT BLOCK DATES NOW                     
         ST    R1,SBADATE                                                       
         MVC   SBNDATES,=F'1'                                                   
         B     XIT                                                              
         EJECT                                                                  
* ESTIMATE FIRST                                                                
*                                                                               
ESTIMATE CLI   ESTFRST,C'Y'        TEST FIRST ESTIMATE FIRST                    
         BNE   ESTX                                                             
         MVI   ESTFRST,C'N'        YES-                                         
         OI    ININD,INIOWSDY      SET OUT-OF-WEEK SWITCH                       
         CLC   SBQEST,SBQESTND     TEST SINGLE ESTIMATE REQUEST                 
         BE    EST10               YES                                          
         CLI   SBESTOWD,0          NO-TEST THIS ESTIMATE HAS OUT OF             
         BE    EST10                  WEEK ROTATOR                              
         GOTO1 SETDATE             YES-RESET WEEK TABLE                         
         SR    R5,R5               COUNT THE WEEKS                              
         L     RE,AWEEKS                                                        
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         BCT   R5,*-12                                                          
         LPR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R5,NUMWKS                                                        
*                                                                               
EST10    L     R3,AWEEKS           SET DATE TABLE MMMDD/YY                      
         LA    R4,DATETAB                                                       
         ZIC   R5,NUMWKS                                                        
         CLI   NUMWKS,5            CHECK NO MORE THAN 5 WEEKS                   
         BNH   EST12                                                            
         LA    R5,5                                                             
         STC   R5,NUMWKS                                                        
*                                                                               
EST12    GOTO1 DATCON,DMCB,(2,0(R3)),(5,0(R4))                                  
         LA    R3,4(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R5,EST12                                                         
         SH    R3,=H'2'            AND END DATE                                 
         GOTO1 DATCON,DMCB,(2,0(R3)),(5,ENDDATE)                                
*                                                                               
ESTX     B     XIT                                                              
         EJECT                                                                  
* PROCESS INVOICE RECORD                                                        
*                                                                               
PROCINV  L     R3,SBAIO1                                                        
         USING INVRECD,R3                                                       
         MVI   ESTSTART,1                                                       
         CLC   SBQEST,SBQESTND                                                  
         BNE   *+10                                                             
         MVC   ESTSTART,SBQEST                                                  
         OC    SVINVKEY,SVINVKEY   TEST FIRST INV RECORD                        
         BNZ   INV2                                                             
         BAS   RE,INITBUFF         YES-INITIALIZE BUFFALO                       
         B     INV4                                                             
*                                                                               
INV2     DS    0H                                                               
         CLI   0(R3),X'0B'        OLD STYLE INVOICE RECORD?                     
         BE    *+14                 YES                                         
         CLC   SVINVKEY+5(3),5(R3) NEW INV REC STATION                          
         B     *+10                                                             
         CLC   SVINVKEY(5),0(R3)   TEST STATION BREAK                           
         BE    INV10               NO                                           
         MVC   SVSTA2,SBSTA        YES-SAVE CURRENT STATION DETAILS             
         MVC   SVBMKT2,SBBMKT                                                   
         MVC   SVBMGR2,SBBMGR                                                   
         MVC   SBSTA,SVSTA         RESTORE PREVIOUS STATION DETAILS             
         MVC   SBBMKT,SVBMKT                                                    
         MVC   SBBMGR,SVBMGR                                                    
         BAS   RE,PUTAFDS          PUT AFFIDS FOR RPT 1                         
         BAS   RE,INITBUFF         INITIALIZE BUFFALO                           
         MVC   SBSTA,SVSTA2        RE-SET CURRENT STATION DETAILS               
         MVC   SBBMKT,SVBMKT2                                                   
         MVC   SBBMGR,SVBMGR2                                                   
*                                                                               
INV4     MVC   SVSTA,SBSTA         SAVE CURRENT STATION DETAILS                 
         MVC   SVBMKT,SBBMKT                                                    
         MVC   SVBMGR,SBBMGR                                                    
*                                                                               
INV10    MVC   SVINVKEY,0(R3)      SAVE THE INV KEY                             
         MVI   GLOPTS+3,2          PROCESS REPORTS 2 AND 3                      
*                                                                               
INVX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* DRIVER INPUT ROUTINE                                                          
*                                                                               
         USING GLOBALD,R4                                                       
DRIVIN   LR    R0,RE                                                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER                                                      
         CLI   MKTIND,C'O'         TEST ORIGINATING MARKET                      
         BE    *+12                                                             
         CLI   MKTIND,C'S'         OR SPILL MARKET                              
         BNE   DRIVINX                                                          
         MVC   SVMKTIND,MKTIND                                                  
         MVC   SVMAXTLV,MAXTOTLV                                                
         MVI   MKTIND,FF           SET COMBINED MARKET                          
         CLC   MKTLEV,MAXTOTLV     YES-                                         
         BNH   *+10                                                             
         MVC   MAXTOTLV,MKTLEV     DO NOT GENERATE ANY TOTALS                   
         BAS   RE,GODRIVER         CALL DRIVER FOR COMBINED MARKET              
         MVC   MKTIND,SVMKTIND                                                  
         MVC   MAXTOTLV,SVMAXTLV                                                
*                                                                               
DRIVINX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRINITAL                                                         
         CLI   GLHOOK,GLOUTPUT     OUTPUT                                       
         BE    DROUTPUT                                                         
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
* DRIVER INITIALIZATION HOOK                                                    
*                                                                               
DRINITAL MVC   GLOPTS+2(1),NUMWKS    SET NUMBER OF WEEKS                        
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  DS    0H                                                               
         LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
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
         DC    CL8'OSTAUSR ',A(OSTA)                                            
         DC    CL8'IAFFID  ',A(IAFFID)                                          
         DC    CL8'OAFFID  ',A(OAFFID)                                          
         DC    CL8'IHOUR1  ',A(IHOUR1)                                          
         DC    CL8'IHOUR2  ',A(IHOUR2)                                          
         DC    CL8'OHOUR   ',A(OHOUR)                                           
         DC    CL8'INAFFID ',A(INAFFID)                                         
         DC    CL8'ONAFFID ',A(ONAFFID)                                         
         DC    CL8'INAFTOT ',A(INAFTOT)                                         
         DC    CL8'ODPTUSR ',A(ODPT)                                            
         DC    CL8'FMKTUSR ',A(FMKT)                                            
         DC    CL8'HWEEK   ',A(HWEEK)                                           
         DC    CL8'HMONTH  ',A(HMONTH)                                          
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT MODE                              
         BNE   EXEC2                                                            
         B     EXEC4                                                            
*                                                                               
EXEC2    CLI   GLMODE,GLOUTPUT     TEST OUTPUT MODE                             
         BNE   EXEC4                                                            
*                                                                               
EXEC4    L     RF,GLAROUT          BRANCH TO ROUTINE                            
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
OGRPCLC  CLC   4(0,R1),=C'9999'                                                 
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
* MARKET AND STATION OUTPUT ROUTINES                                            
*                                                                               
OMKT     LA    R4,MKTHEAD          FORMAT MKT HEAD TO TEMP SAVE AREA            
         MVC   MKTHEAD,BLANKS                                                   
         MVC   0(6,R4),=C'MARKET'                                               
         MVC   7(4,R4),SBMKT                                                    
         MVC   12(L'SBMKTNM,R4),SBMKTNM                                         
         LA    R1,L'MKTHEAD                                                     
         ST    R1,DMCB+4                                                        
         GOTO1 SQUASHER,DMCB,(R4)                                               
         B     XIT                                                              
*                                                                               
OSTA     LA    R4,STAHEAD                                                       
         MVC   STAHEAD,BLANKS                                                   
         MVC   0(7,R4),=C'STATION'                                              
         MVC   8(4,R4),0(R2)                                                    
         MVI   12(R4),C'-'                                                      
         MVC   13(1,R4),4(R2)                                                   
         MVI   14(R4),C'M'                                                      
         B     XIT                                                              
         EJECT                                                                  
* I/O ROUTINES FOR AFFID LIST REPORT                                            
*                                                                               
IAFFID   MVC   0(4,R2),XFF                                                      
         CLC   NUMWKS,GLARGS                                                    
         BL    XIT                                                              
         L     R1,AAFDENT                                                       
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         AR    R1,RE                                                            
         MVC   0(4,R2),0(R1)                                                    
         OC    0(4,R2),0(R2)                                                    
         BNZ   XIT                                                              
         MVC   0(4,R2),XFF                                                      
         B     XIT                                                              
*                                                                               
OAFFID   MVC   0(16,R3),BLANKS                                                  
         CLC   0(4,R2),XFF                                                      
         BE    XIT                                                              
         GOTO1 DATCON,DMCB,(2,0(R2)),(4,0(R3))                                  
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,WORK)                                   
         GOTO1 GETDAY,DMCB,WORK,6(R3)                                           
         CLC   6(3,R3),BLANKS                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(2),2(R2)                                                    
         XC    WORK+2(2),WORK+2                                                 
         GOTO1 UNTIME,DMCB,WORK,10(R3)                                          
         B     XIT                                                              
         EJECT                                                                  
* I/O ROUTINES FOR TIME PERIODS                                                 
*                                                                               
IHOUR1   L     R5,SBACURCH                                                      
         BAS   RE,GETHR                                                         
         MVC   0(1,R2),4(R1)                                                    
         B     XIT                                                              
*                                                                               
IHOUR2   L     R5,SBACURCH                                                      
         BAS   RE,GETHR                                                         
         STC   RF,0(R2)                                                         
         B     XIT                                                              
*                                                                               
OHOUR    ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'5'                                                         
         LA    R1,HOURTAB(R1)                                                   
         MVC   FULL,0(R1)                                                       
         OC    FULL(2),FULL                                                     
         BNZ   *+10                                                             
         MVC   FULL(2),=H'2400'                                                 
         GOTO1 UNTIME,DMCB,FULL,(R3)                                            
         B     XIT                                                              
*                                                                               
ODPT     ZIC   RE,0(R2)                                                         
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,DPTTAB(RE)                                                    
         MVC   FULL,0(RE)                                                       
         GOTO1 UNTIME,DMCB,FULL,(R3)                                            
         B     XIT                                                              
*                                                                               
         USING INVELEM,R5                                                       
GETHR    LR    R0,RE                                                            
         LA    R1,HOURTAB                                                       
         LA    RF,1                                                             
         MVC   HALF,INVTIM                                                      
*                                                                               
         CLI   0(R5),X'40'        NEW STYLE INVOICE RECORD?                     
         BNE   GETHR1               NO                                          
         SR    RE,RE                                                            
         ZICM  RF,SNVIDTIM-SNVIDELD(R5),2                                       
         D     RE,=F'60'           CONVERT TO MILITARY TIME                     
         MH    RF,=H'100'                                                       
         AR    RF,RE                                                            
         AH    RF,=H'600'          (0 = 6:00A)                                  
         STH   RF,HALF                                                          
         LA    RF,1                                                             
*                                                                               
GETHR1   CLC   HALF,=H'2400'                                                    
         BL    GETHR2                                                           
         LH    RE,HALF                                                          
         SH    RE,=H'2400'                                                      
         STH   RE,HALF                                                          
         B     GETHR1                                                           
*                                                                               
GETHR2   CLC   HALF,0(R1)                                                       
         BL    *+14                                                             
         CLC   HALF,2(R1)                                                       
         BNH   GETHRX                                                           
         LA    R1,5(R1)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(R1),FF                                                         
         BNE   GETHR2                                                           
         DC    H'0'                                                             
GETHRX   LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R5                                                               
         SPACE 2                                                                
HOURTAB  DC    AL2(0500,0559),AL1(1)                                            
         DC    AL2(0600,0659),AL1(1)                                            
         DC    AL2(0700,0759),AL1(1)                                            
         DC    AL2(0800,0859),AL1(1)                                            
         DC    AL2(0900,0959),AL1(1)                                            
         DC    AL2(1000,1059),AL1(2)                                            
         DC    AL2(1100,1159),AL1(2)                                            
         DC    AL2(1200,1259),AL1(2)                                            
         DC    AL2(1300,1359),AL1(2)                                            
         DC    AL2(1400,1459),AL1(2)                                            
         DC    AL2(1500,1559),AL1(3)                                            
         DC    AL2(1600,1659),AL1(3)                                            
         DC    AL2(1700,1759),AL1(3)                                            
         DC    AL2(1800,1859),AL1(3)                                            
         DC    AL2(1900,1959),AL1(3)                                            
         DC    AL2(2000,2059),AL1(4)                                            
         DC    AL2(2100,2159),AL1(4)                                            
         DC    AL2(2200,2259),AL1(4)                                            
         DC    AL2(2300,2359),AL1(4)                                            
         DC    AL2(0000,0059),AL1(4)                                            
         DC    AL2(0100,0159),AL1(5)                                            
         DC    AL2(0200,0259),AL1(5)                                            
         DC    AL2(0300,0359),AL1(5)                                            
         DC    AL2(0400,0459),AL1(5)                                            
         DC    X'FF'                                                            
*                                                                               
DPTTAB   DC    AL2(0500,0959)                                                   
         DC    AL2(1000,1459)                                                   
         DC    AL2(1500,1959)                                                   
         DC    AL2(2000,0059)                                                   
         DC    AL2(0100,0459)                                                   
         EJECT                                                                  
* I/O ROUTINES FOR AFFID COUNTS                                                 
*                                                                               
INAFFID  L     R5,SBACURCH                                                      
         USING INVELEM,R5                                                       
         MVC   MYIDATE,INVDAT                                                   
         MVC   MYITIME,INVTIM                                                   
         CLI   0(R5),X'40'         NEW INVOICE?                                 
         BNE   INAFF10              NO                                          
*                                                                               
* GET INV DATE IN SAME FORMAT AS OLD                                            
         ZIC   R6,SNVIDDAY-SNVIDELD(R5)                                         
         GOTO1 DATCON,DMCB,(2,ELEM+25),(0,WORK)                                 
         GOTO1 ADDAY,(R1),WORK,WORK,(R6)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,MYIDATE)                                 
*                                                                               
* GET INV TIME IN SAME FORMAT AS OLD                                            
         SR    RE,RE                                                            
         ZICM  RF,SNVIDTIM-SNVIDELD(R5),2                                       
         D     RE,=F'60'           CONVERT TO MILITARY TIME                     
         MH    RF,=H'100'                                                       
         AR    RF,RE                                                            
         AH    RF,=H'600'          (0 = 6:00A)                                  
         STCM  RF,3,MYITIME                                                     
*                                                                               
INAFF10  XC    0(4,R2),0(R2)                                                    
         CLC   NUMWKS,GLARGS                                                    
         BL    XIT                                                              
         ZIC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         L     RE,AWEEKS                                                        
         AR    RE,R1                                                            
         CLC   MYIDATE,0(RE)       TEST AFFID DATE WITHIN THIS WEEK             
         BL    XIT                                                              
         CLC   MYIDATE,2(RE)                                                    
         BH    XIT                                                              
         MVC   BFPRD,SBBPRD        YES-PUT TO BUFFALO                           
         MVC   BFEST,SBBEST                                                     
         CLI   SBQSEPES,C'Y'                                                    
         BE    *+8                                                              
         MVI   BFEST,0                                                          
         MVC   BFWK,GLARGS                                                      
         MVC   BFDAY,MYIDATE                                                    
         MVC   BFTIME,MYITIME                                                   
         BAS   RE,BUFPUT                                                        
         GOTO1 DATCON,DMCB,(2,MYIDATE),WORK  GET DAY OF WEEK                    
         GOTO1 GETDAY,(R1),WORK,FULL                                            
         CLC   FULL(3),BLANKS                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),6             TEST MO-FR                                   
         BNL   *+12                                                             
         MVI   1(R2),1             YES                                          
         B     XIT                                                              
         MVI   3(R2),1             NO-THEN IT'S SA-SU                           
         B     XIT                                                              
*                                                                               
INAFTOT  L     R5,SBACURCH         TOTAL                                        
         USING INVELEM,R5                                                       
         MVC   MYIDATE,INVDAT                                                   
         MVC   MYITIME,INVTIM                                                   
         CLI   0(R5),X'40'         NEW INVOICE?                                 
         BNE   INAFTOT1             NO                                          
*                                                                               
* GET INV DATE IN SAME FORMAT AS OLD                                            
         ZIC   R6,SNVIDDAY-SNVIDELD(R5)                                         
         GOTO1 DATCON,DMCB,(2,ELEM+25),(0,WORK)                                 
         GOTO1 ADDAY,(R1),WORK,WORK,(R6)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,MYIDATE)                                 
*                                                                               
INAFTOT1 XC    0(4,R2),0(R2)                                                    
         ZIC   RF,NUMWKS           TEST DATE WITHIN ONE OF THE WEEKS            
         L     R1,AWEEKS                                                        
INAFTOT2 CLC   MYIDATE,0(R1)                                                    
         BL    *+14                                                             
         CLC   MYIDATE,2(R1)                                                    
         BNH   *+16                                                             
         LA    R1,4(R1)                                                         
         BCT   RF,INAFTOT2                                                      
         B     XIT                                                              
         GOTO1 DATCON,DMCB,(2,MYIDATE),WORK  GET DAY OF WEEK                    
         GOTO1 GETDAY,(R1),WORK,FULL                                            
         CLC   FULL(3),BLANKS                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),6             TEST MO-FR                                   
         BNL   *+12                                                             
         MVI   1(R2),1             YES                                          
         B     XIT                                                              
         MVI   3(R2),1             NO-THEN IT'S SA-SU                           
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
ONAFFID  LH    R5,0(R2)            MO-FR                                        
         EDIT  (R5),(5,(R3))                                                    
         LH    R6,2(R2)            SA-SU                                        
         EDIT  (R6),(5,6(R3))                                                   
         L     R1,GLATHID          TEST TOTALING NOW                            
         CLC   GLLEVEL,GLDETLEV-GLINTD(R1)                                      
         BNL   XIT                                                              
         AR    R5,R6               YES-THEN PRINT MO-SU                         
         BNP   XIT                                                              
         EDIT  (R5),(6,12(R3)),FLOAT==                                          
         B     XIT                                                              
         EJECT                                                                  
* MARKET FIRST                                                                  
*                                                                               
FMKT     MVC   SVBUYNAM,ASTERS                                                  
         MVC   SVBILNAM,ASTERS                                                  
         CLC   BUYNAME,BLANKS      TEST OVERRIDE BUYER NAME                     
         BNH   *+10                                                             
         MVC   SVBUYNAM,BUYNAME    YES-SET BUYER NAME FOR HEADLINE              
         CLC   BILNAME,BLANKS      TEST OVERRIDE BILLER NAME                    
         BNH   *+10                                                             
         MVC   SVBILNAM,BILNAME    YES-SET BILLER NAME FOR HEADLINE             
         CLC   BUYNAME,BLANKS      TEST AT LEAST ONE NAME NOT                   
         BNH   *+14                OVERRIDDEN                                   
         CLC   BILNAME,BLANKS                                                   
         BH    XIT                                                              
         XC    KEY,KEY             YES-GET STATUS RECORD                        
         LA    R5,KEY                                                           
         USING STATD,R5                                                         
         MVC   STKTYPE,=X'0D71'                                                 
         MVC   STKAGMD,SBBAGYMD                                                 
         MVC   STKCLT,SBBCLT                                                    
         MVC   STKMKT,SBBMKT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XIT                                                              
         L     R5,SBAIO3                                                        
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         CLC   BUYNAME,BLANKS                                                   
         BH    FMKT2                                                            
         CLI   BPBUYER,C' '        TEST BUYER NAME SET                          
         BNH   FMKT2                                                            
         MVC   SVBUYNAM,BPBUYER    YES-SET BUYER NAME                           
*                                                                               
FMKT2    CLC   BILNAME,BLANKS                                                   
         BH    XIT                                                              
         CLI   BPPAYER,C' '        TEST BILLER NAME SET                         
         BNH   XIT                                                              
         MVC   SVBILNAM,BPPAYER    YES-SET BILLER NAME                          
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* HEADLINE ROUTINES                                                             
*                                                                               
HWEEK    CLC   NUMWKS,GLARGS                                                    
         BL    XIT                                                              
         MVC   0(8,R3),=C'WEEK OF '                                             
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,3                                                             
         LA    RE,DATETAB(RE)                                                   
         MVC   8(8,R3),0(RE)                                                    
         B     XIT                                                              
*                                                                               
HMONTH   MVC   0(8,R3),DATETAB                                                  
         MVI   8(R3),C'-'                                                       
         MVC   9(8,R3),ENDDATE                                                  
         B     XIT                                                              
         EJECT                                                                  
* DRIVER IS ABOUT TO PUT A SORT RECORD                                          
*                                                                               
PUTSRT   MVI   INDATA,1            TELL SYSDRIVER THERE IS SIGNIFICANT          
         B     XIT                 DATA                                         
         EJECT                                                                  
* DRIVER IS ABOUT TO BE CALLED FOR OUTPUT                                       
*                                                                               
DROUTPUT OC    BUFFL,BUFFL         TEST BUFFALO INITIALIZED                     
         BZ    XIT                                                              
*                                                                               
         MVC   SVSTA2,SBSTA        SAVE CURRENT STATION DETAILS                 
         MVC   SVBMKT2,SBBMKT                                                   
         MVC   SVBMGR2,SBBMGR                                                   
         MVC   SBSTA,SVSTA         RESTORE PREVIOUS STATION DETAILS             
         MVC   SBBMKT,SVBMKT                                                    
         MVC   SBBMGR,SVBMGR                                                    
         BAS   RE,PUTAFDS          PUT AFFIDS FOR LAST STATION                  
         MVC   SBSTA,SVSTA2        RE-SET CURRENT STATION DETAILS               
         MVC   SBBMKT,SVBMKT2                                                   
         MVC   SBBMGR,SVBMGR2                                                   
*                                                                               
         MVC   DMCB+8(4),BUFFL     RELEASE BUFFALO STORAGE                      
         GOTO1 COVAIL,DMCB,C'FREE',ABUFF                                        
         B     XIT                                                              
         EJECT                                                                  
* PRINT A LINE                                                                  
*                                                                               
PRINT    B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK FIRSTS                                                            
*                                                                               
FIRSTS   B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK LASTS                                                             
*                                                                               
LASTS    B     XIT                                                              
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
         LA    R3,DMGR                                                          
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
         LA    R3,DMKT                                                          
         BAS   RE,HDPOS                                                         
         MVC   0(L'MKTHEAD,RF),MKTHEAD                                          
*                                                                               
HD6      CLI   GLRECNO,2           ONLY PRINT STA FOR DETAIL REPORTS            
         BH    HD8                                                              
         ICM   R2,1,HDSTA                                                       
         BZ    HD8                                                              
         LA    R3,DSTA                                                          
         BAS   RE,HDPOS                                                         
         MVC   0(L'STAHEAD,RF),STAHEAD                                          
         DROP  R5                                                               
*                                                                               
HD8      LA    R2,6                FORMAT THE BUYER NAME                        
         LA    R3,96                                                            
         BAS   RE,HDPOS                                                         
         MVC   0(5,RF),=C'BUYER'                                                
         MVC   7(L'SVBUYNAM,RF),SVBUYNAM                                        
         LA    R2,7                FORMAT THE BILLER NAME                       
         LA    R3,96                                                            
         BAS   RE,HDPOS                                                         
         MVC   0(6,RF),=C'BILLER'                                               
         MVC   7(L'SVBILNAM,RF),SVBILNAM                                        
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
* ROUTINE TO PUT THE AFFIDS FOR REPORT 1                                        
*                                                                               
PUTAFDS  NTR1                                                                   
         MVI   GLOPTS+3,1          TELL DRIVER ONLY PROCESS 1ST REPORT          
         MVI   BUFEOF,C'N'                                                      
         BAS   RE,BUFHI            READ BUFFALO HIGH                            
         BNE   PUTAFDX                                                          
         B     PUTAFD6                                                          
*                                                                               
PUTAFD2  BAS   RE,BUFSEQ                                                        
         BE    *+12                                                             
         MVI   BUFEOF,C'Y'                                                      
         B     PUTAFD3                                                          
         CLC   BFPRD,SBBPRD        TEST PRODUCT BREAK                           
         BNE   PUTAFD3                                                          
         CLI   SBQSEPES,C'Y'                                                    
         BNE   PUTAFD8                                                          
         CLC   BFEST,SBBEST        TEST ESTIMATE BREAK                          
         BE    PUTAFD8                                                          
*                                                                               
PUTAFD3  LA    R1,AFDLIST                                                       
*                                                                               
PUTAFD4  OC    0(20,R1),0(R1)                                                   
         BZ    PUTAFD6                                                          
         ST    R1,AAFDENT                                                       
         BAS   RE,DRIVIN                                                        
         LA    R1,20(R1)                                                        
         C     R1,=A(AFDLISTX)                                                  
         BL    PUTAFD4                                                          
         DC    H'0'                AFFID LIST FULL                              
*                                                                               
PUTAFD6  CLI   BUFEOF,C'Y'                                                      
         BE    PUTAFDX                                                          
         LA    RE,AFDLIST          CLEAR THE AFFID LIST                         
         L     RF,=A(AFDLISTX)                                                  
         SR    RF,RE                                                            
         XCEF                                                                   
         CLC   BFPRD,SBBPRD        TEST PRODUCT BREAK                           
         BE    PUTAFD7                                                          
         MVC   SBBPRD,BFPRD        YES-SET THE PRODUCT                          
         ZIC   R1,SBBPRD                                                        
         BCTR  R1,0                                                             
         MH    R1,=Y(PRDBUFFL)                                                  
         L     RE,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH        SET PRODUCT DETAILS                          
         MVC   SBPRDNM,PBNAME                                                   
         MVC   SBBPGR,PBGROUP      SET PRODUCT GROUP                            
         OC    SBBPGR,SBBPGR                                                    
         BNZ   PUTAFD7                                                          
         MVC   SBBPGR,=X'9999'                                                  
         DROP  R1                                                               
*                                                                               
PUTAFD7  CLI   SBQSEPES,C'Y'       TEST SEPARATE ESTIMATES                      
         BNE   PUTAFD10                                                         
         MVC   SBBEST,BFEST        YES-SET THE ESTIMATE                         
         B     PUTAFD10                                                         
*                                                                               
PUTAFD8  CLC   BFWK,WEEK           TEST WEEK BREAK                              
         BE    PUTAFD12                                                         
*                                                                               
PUTAFD10 MVC   WEEK,BFWK           SET THE WEEK                                 
         ZIC   R5,WEEK                                                          
         BCTR  R5,0                                                             
         SLL   R5,2                                                             
         LA    R5,AFDLIST(R5)                                                   
*                                                                               
PUTAFD12 MVC   0(2,R5),BFDAY                                                    
         MVC   2(2,R5),BFTIME                                                   
         LA    R5,20(R5)                                                        
         C     R5,=A(AFDLISTX)                                                  
         BL    PUTAFD2                                                          
         DC    H'0'                AFFID LIST FULL                              
*                                                                               
PUTAFDX  B     XIT                                                              
         EJECT                                                                  
* BUFFALO INITIALIZATION                                                        
*                                                                               
INITBUFF NTR1                                                                   
         OC    SVINVKEY,SVINVKEY   TEST FIRST TIME                              
         BNZ   INITB2                                                           
         L     R4,=A(BUFFALOC)                                                  
         GOTO1 COVAIL,DMCB,C'SETB',50000,1000000,(R4)                           
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ABUFF,4(R1)                                                      
         MVC   BUFFL,8(R1)                                                      
         MVC   ABUFFC,12(R1)                                                    
         BAS   RE,BUFSET                                                        
         B     INITBX                                                           
*                                                                               
INITB2   BAS   RE,BUFRSET                                                       
*                                                                               
INITBX   B     XIT                                                              
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
BUFHI    LA    R1,=C'HIGH'                                                      
         XC    BFKEY,BFKEY                                                      
         B     BUFX                                                             
*                                                                               
BUFSEQ   LA    R1,=C'SEQ'                                                       
         B     BUFX                                                             
*                                                                               
BUFX     NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 VBUFFALO,DMCB,,ABUFFC,BFREC,1                                    
         TM    8(R1),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD01                                                      
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* HEADLINE DISPLACEMENTS                                                        
*                                                                               
DMGR     EQU   49                  MARKET GROUP                                 
DMKT     EQU   52                  MARKET                                       
DSTA     EQU   56                  STATION                                      
         SPACE 2                                                                
* WORKING STORAGE                                                               
*                                                                               
VBUFFALO DS    V                                                                
AAFDENT  DS    A                                                                
ABUFFC   DS    A                                                                
ABUFF    DS    A                                                                
BUFFL    DS    F                                                                
*                                                                               
NUMWKS   DS    XL1                                                              
DATETAB  DS    5CL8                                                             
ENDDATE  DS    CL8                                                              
WEEK     DS    XL1                                                              
BUFEOF   DS    CL1                                                              
ESTFRST  DS    CL1                                                              
BILNAME  DS    CL12                                                             
BUYNAME  DS    CL12                                                             
SVBILNAM DS    CL12                                                             
SVBUYNAM DS    CL12                                                             
*                                                                               
MGR1HEAD DS    CL44                                                             
MGR2HEAD DS    CL44                                                             
MGR3HEAD DS    CL44                                                             
MKTHEAD  DS    CL44                                                             
STAHEAD  DS    CL44                                                             
*                                                                               
SVINVKEY DS    XL(L'INVKEY)                                                     
SVSTA    DS    CL5                                                              
SVSTA2   DS    CL5                                                              
SVBMKT   DS    XL2                                                              
SVBMKT2  DS    XL2                                                              
SVBMGR   DS    XL2                                                              
SVBMGR2  DS    XL2                                                              
SVMKTIND DS    XL1                                                              
SVMAXTLV DS    XL1                                                              
*                                                                               
MYIDATE  DS    XL2                                                              
MYITIME  DS    XL2                                                              
*                                                                               
BFREC    DS    0CL7                                                             
BFKEY    DS    0CL7                                                             
BFPRD    DS    XL1                                                              
BFEST    DS    XL1                                                              
BFWK     DS    XL1                                                              
BFDAY    DS    XL2                                                              
BFTIME   DS    XL2                                                              
*                                                                               
PATCH    DC    XL32'00'                                                         
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
BLANKS   DC    CL80' '                                                          
ASTERS   DC    CL12'************'                                               
FF       EQU   X'FF'                                                            
         EJECT                                                                  
* HEADLINE POSITION TABLE                                                       
*                                                                               
HEADTAB  DC    X'00',X'000000',X'05',X'07'                                      
         DC    X'01',X'050000',X'07',X'09'                                      
         DC    X'02',X'050600',X'07',X'09'                                      
         DC    X'03',X'050607',X'08',X'09'                                      
         DC    X'FF'                                                            
         SPACE 1                                                                
HEADTABD DSECT                                                                  
HDNMGR   DS    X                   N'MARKET GROUPS                              
HDMGR1   DS    X                   HEADLINE FOR MARKET GROUP 1                  
HDMGR2   DS    X                   HEADLINE FOR MARKET GROUP 2                  
HDMGR3   DS    X                   HEADLINE FOR MARKET GROUP 3                  
HDMKT    DS    X                   HEADLINE FOR MARKET                          
HDSTA    DS    X                   HEADLINE FOR STATION                         
HEADTABL EQU   *-HEADTABD                                                       
         EJECT                                                                  
T20405   CSECT                                                                  
*                                                                               
AFDLIST  DS    150XL(5*4)                                                       
AFDLISTX EQU   *                                                                
*                                                                               
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
         BUFF  LINES=1,FLAVOR=DATA,KEYLIST=(7,A)                                
*                                                                               
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPGENINV                                                                       
*SPGENSTAT                                                                      
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
INVRECD  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
SNVRECD  DSECT                                                                  
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENSTAT                                                      
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE3D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063SPWRI05   12/15/04'                                      
         END                                                                    
