*          DATA SET SPREPA202  AT LEVEL 015 AS OF 09/28/07                      
*PHASE SPA202A                                                                  
*INCLUDE SPA2TP                                                                 
*INCLUDE CASHVAL                                                                
*                                                                               
* 07AUG02  USE PL6 VALUES FOR GROSS, NET, ACTUAL FROM BILL HEADERS              
*                                                                               
* 15MAY02  SUPPORT ALPHA-NUMERIC REPS                                           
*          ALLOW REPORT AT NET DOLS AND A2 PROF SUPPORT FOR IT                  
*                                                                               
* 10JUL97  SUPPORT FOR YEAR 2000                                                
*                                                                               
* 23AUG94  NEW STAPACK                                                          
*                                                                               
* 22JUL94  PRINT WIM/CLT VERSION IN HEADLINES                                   
*                                                                               
* 23FEB93  SHOW UDEF FIELDS                                                     
*                                                                               
* 30JUN92  USE SPBVAL FOR 'EFFECTIVE GROSS' AND FORMULA CALCS                   
*                                                                               
* 05JUN92  DON'T CLEAR TAX BEFORE CALLING GROSS-UP ROUTINE, IDIOT               
*                                                                               
* 18FEB92  DON'T PRINT GST ROWS FOR EXEMPT/ZERO RATED CLIENTS                   
*          AND OBSERVE GST STATUS OF PRODUCT ON OUTPUT GST REQUEST              
*                                                                               
* 03MAY91  SUPPRESS FEATURE THAT FORCED ORDERED DOLLARS TO BILLED               
*          DOLLARS WHEN OUTPUT GST WAS REQUESTED                                
*                                                                               
* DEC19/90 SUPPORT INCLUDE GST                                                  
         TITLE 'SPA202 - SPOTPAK PRODUCT SUMMARIES'                             
         PRINT NOGEN                                                            
SPA202   CSECT                                                                  
         NMOD1 0,SPA202,RR=R5                                                   
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         LA    R8,2048(RC)                                                      
         LA    R8,2048(R8)                                                      
         USING SPA202+4096,RC,R8                                                
*                                                                               
         ST    R5,RELO             SAVE RELOCATION VALUE                        
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
*                                                                               
         MVI   PRSUMSW,0           RESET SUMMARY SWITCH FOR HL RTN              
         CLI   MODE,PROCBUY                                                     
         BE    PR40                                                             
         CLI   MODE,PROCGOAL                                                    
         BE    PR140                                                            
         CLI   MODE,STAFRST                                                     
         BE    PR35                                                             
         CLI   MODE,MKTFRST                                                     
         BE    PR38                                                             
*                                                                               
         MVI   SPACING,1                                                        
         CP    PRMONCNT,=P'6'                                                   
         BNH   *+8                                                              
         MVI   SPACING,2                                                        
*                                                                               
         CLI   MODE,STALAST                                                     
         BE    PR60                                                             
         CLI   MODE,MKTLAST                                                     
         BE    PR65                                                             
         CLI   MODE,MGR3LAST                                                    
         BE    PR80                                                             
         CLI   MODE,MGR2LAST                                                    
         BE    PR90                                                             
         CLI   MODE,MGR1LAST                                                    
         BE    PR100                                                            
         CLI   MODE,MGR3FRST                                                    
         BE    PR39                                                             
         CLI   MODE,MGR2FRST                                                    
         BE    PR39                                                             
         CLI   MODE,MGR1FRST                                                    
         BE    PR39                                                             
         CLI   MODE,PRDLAST                                                     
         BE    PR110                                                            
         CLI   MODE,PGR2LAST                                                    
         BE    PR160                                                            
         CLI   MODE,PGR1LAST                                                    
         BE    PR170                                                            
         CLI   MODE,ESTFRST                                                     
         BE    PR30                                                             
         CLI   MODE,CLTLAST                                                     
         BE    PR180                                                            
         CLI   MODE,CLTFRST                                                     
         BE    PR20                                                             
         CLI   MODE,REQFRST                                                     
         BE    PR15                                                             
         CLI   MODE,RUNFRST                                                     
         BE    PR10                                                             
         CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         GOTO1 =V(SPA2TP),DMCB,(C'X',(RA))        CLOSE INTFC IF REQD           
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
PR10     L     R0,=A(PRHDHK)                                                    
         ST    R0,HEADHOOK                                                      
         GOTO1 =A(HDHKINIT),DMCB,(R7),(RA),HDHKDATA                             
*                                                                               
         STM   R7,RC,SPHKR7                                                     
*                                                                               
         LA    R0,PRREC                                                         
         ST    R0,BUFFIO                                                        
*                                                                               
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
*                                                                               
         L     R2,BUFFBUFF                                                      
         USING BUFFALOD,R2                                                      
         L     R1,BUFFCRMX         GET MAX LINES IN CORE                        
         M     R0,BUFFLALL         X REC LENGTH                                 
         ST    R1,SVBFSIZE         GIVES BUFFER SIZE (SAVE IT)                  
         DROP  R2                                                               
*                                                                               
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   MEDEXTAC,C'Y'       ACCTG DATA                                   
*                                                                               
         MVI   SPSUPMKT,C'Y'                                                    
         LA    R1,MEDXCHX-MEDXCHG+200                                           
         ST    R1,MEDLCHNK                                                      
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVI   RQMGOPT,C'Y'        MG AS MSSD OPTION ALLOWED                    
* RELOCATE ACCUMULATOR ADDRESSES                                                
         LA    R0,13                                                            
         LA    R1,APRTOTST                                                      
         L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         L     RF,APRBLPER         AND A(PRBLPER)                               
         A     RF,RELO                                                          
         ST    RF,APRBLPER                                                      
*                                                                               
         L     R1,ADAGY                                                         
         MVC   AGYCAN,AGYPROF+7-AGYHDR(R1)                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
PR15     DS    0H                                                               
         MVC   PRQBYID,QBYID       SAVE REQUEST VALUE                           
         MVI   RQGETBF,C' '        RESET DEFAULT DOLLARS                        
*                                                                               
         L     R2,=A(DICSECT)                                                   
         USING DICSECT,R2                                                       
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNL      TRANSLATE LIST                               
         MVI   DDRETN,DDCASEU                                                   
         MVI   DDSYS,2                                                          
         MVC   DDLANG,RCLANG                                                    
         LA    RF,DCLIST                                                        
         STCM  RF,7,DDIADR                                                      
         LA    RF,DSLIST                                                        
         STCM  RF,7,DDOADR                                                      
         GOTO1 DICTATE                                                          
         DROP  R1,R2                                                            
*                                                                               
         MVC   CURTAB(8),=X'00000002115B4040'                                   
*                                                                               
         XC    PRQDATES,PRQDATES   CLEAR PREVIOUS REQUEST DATES                 
         MVI   FCGETMKT,C'Y'       RESET                                        
         MVI   FCGETSTA,C'Y'                                                    
         CLC   =C'0000',QMKT       TEST MKT 0 REQUEST                           
         BNE   *+8                                                              
         MVI   FCGETMKT,C'N'       YES - SUPPRESS MARKET FETCH                  
*                                                                               
         XC    RQBILLDT,RQBILLDT                                                
         CLI   QOPT7,C'Y'          TEST BILLED TODAY REQUEST                    
         BNE   *+10                                                             
         MVC   RQBILLDT,TODAYB                                                  
         B     EXIT                                                             
         EJECT                                                                  
*======================================================*                        
* CLTFRST                                              *                        
*                                                      *                        
* CANADIAN REPORTING OPTIONS IN PROGPROF+14 IF CURR=C  *                        
*                                                      *                        
* 0 = INCLUDE C58 + MSF BUT DO NOT BREAK OUT           *                        
* 1 = BREAK OUT EXCHANGE               DATASW=DSEXCH   *                        
* 2 = BREAK OUT EXCHANGE + C58               =DSC58    *                        
* 3 = BREAK OUT EXCHANGE + C58 + MSF         =DSMSF    *                        
* 4 = BREAK OUT EXCHANGE/EXCLUDE C58 + MSF             *                        
* 9 = DO NOT BREAK OUT EXCHANGE/EXCLUDE C58 + MSF      *                        
*======================================================*                        
         SPACE 1                                                                
PR20     DS    0H                                                               
         CLI   FCRDBUYS,C'I'                                                    
         BNE   *+8                                                              
         BRAS  RE,NOTEOUT                                                       
*                                                                               
         L     R1,ADCLT                                                         
         MVC   CLTCNT,CEXTRA+9-CLTHDR(R1)                                       
         MVI   DATASW,DSTAX                                                     
         LA    R0,27               SET NUMBER OF COLUMNS                        
         CLI   QGST,C'O'           TEST OUTPUT GST REQUEST                      
         BE    PR21                                                             
         CLI   PROGPROF+4,C'Y'     TEST PRINT GROSS+TAX/TAX                     
         BE    PR21                                                             
         CLI   PROGPROF+4,C'X'     TEST PRINT GROSS/TAX/GROSS+TAX               
         BE    PR21                                                             
         LA    R0,14               ELSE SET TO IGNORE TAX COLUMNS               
         MVI   DATASW,0                                                         
*                                                                               
PR21     CLI   AGYCAN,C'C'         TEST CANADIAN AGENCY                         
         BNE   PR22                                                             
*                                                                               
         CLI   RQCRRNCY,C'C'       YES-TEST CANADIAN DOLLARS                    
         BNE   PR22                                                             
         CLI   Q2COL21,C' '        TEST REQUEST OVERRIDE                        
         BNH   *+14                                                             
         MVC   PROGPROF+14(1),Q2COL21                                           
         NI    PROGPROF+14,X'0F'                                                
*                                                                               
         CLI   PROGPROF+14,0       TEST BREAKOUT REQUESTED                      
         BE    PR22                NO                                           
         CLI   PROGPROF+14,9       TEST SUPPRESS C58+MSF                        
         BE    PR22                                                             
         EJECT                                                                  
* SOME BREAKOUT REQUESTED                                                       
         CLI   PROGPROF+4,C'Y'     TEST Y OPTION (GROSS+TAX/TAX)                
         BNE   *+8                 NO                                           
         MVI   PROGPROF+4,C'X'     DON'T ALLOW IT                               
*                                                                               
         OI    DATASW,DSEXCH       YES-REPORT EXCHANGE DOLLARS                  
         LA    R0,40                                                            
         CLI   QOPT1,C'P'          PAID,UNPAID,BILLED AND BILLABLE              
         BE    PR22                DO NOT GET C58 TAX                           
         CLI   QOPT1,C'U'          OR MEDIA SERVICE FEE                         
         BE    PR22                                                             
         CLI   QOPT1,C'R'                                                       
         BE    PR22                                                             
         CLI   QOPT1,C'B'                                                       
         BE    PR22                                                             
         CLI   PROGPROF+14,4       BREAK OUT XCH/EXLCUDE C58+MSF                
         BE    PR22                                                             
         CLI   PROGPROF+14,1       TEST C58 REQUESTED                           
         BNH   PR22                                                             
         CLI   CLTCNT,C'U'         YES-TEST USA CLIENT                          
         BE    PR22                YES                                          
         OI    DATASW,DSC58        NO-REPORT C58 RESERVE                        
         LA    R0,53                                                            
         CLI   PROGPROF+14,3       TEST MEDIA SERVICE FEE REQUESTED             
         BNE   PR22                                                             
         OI    DATASW,DSMSF        YES                                          
         LA    R0,66                                                            
         EJECT                                                                  
PR22     STH   R0,PRCOLS                                                        
*======================================================*                        
* RESET BUFFALO PARAMS                                 *                        
*======================================================*                        
         SPACE 1                                                                
         L     R2,BUFFBUFF                                                      
         USING BUFFALOD,R2                                                      
         ST    R0,BUFFCOLS         SET NUMBER OF COLUMNS                        
         SLL   R0,3                X 8                                          
         ST    R0,BUFFLDTA         GIVES DATA LENGTH                            
         ST    R0,BUFFWROW          AND ROW WIDTH                               
         A     R0,BUFFLKEY         + KEY LENGTH                                 
         A     R0,BUFFLCOM         + COMMENT LENGTH                             
         ST    R0,BUFFLALL         GIVES REC LENGTH                             
         L     RE,SVBFSIZE         BUFF SIZE                                    
         SRDA  RE,32                                                            
         DR    RE,R0               DIVIDED BY REC LEN                           
         BCTR  RF,0                ALLOW ONE LINE FOR BUFFER (CHICKEN)          
         ST    RF,BUFFCRMX         GIVES MAX LINES IN CORE                      
         DROP  R2                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',BUFFBUFF                                  
* ADD DUMMY E-O-F REC                                                           
         XC    PRKEY,PRKEY                                                      
         MVI   PRKEY,X'FE'                                                      
         LA    R2,PRDATA                                                        
         LA    R0,NACCUMS          ACCUMS/ROW                                   
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R0,*-10                                                          
         BAS   RE,PUTBUFF                                                       
*                                                                               
         MVC   P,SPACES                                                         
         MVI   SW,C'N'                                                          
         MVC   SW+1(L'SW-1),SW                                                  
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         OC    PROGPROF+5(7),SPACES                                             
         CLI   QOPT1,C' '                                                       
         BH    *+10                                                             
         MVC   QOPT1,PROGPROF+5    SET DEFAULT ANALYSIS TYPE                    
         CLI   QOPT2,C' '                                                       
         BH    *+10                                                             
         MVC   QOPT2,PROGPROF+7    DEFAULT GOAL/AUTH TOTALS                     
         CLI   QOPT3,C' '                                                       
         BH    *+10                                                             
         MVC   QOPT3,PROGPROF+6    DEFAULT PAID TOTALS                          
         CLI   QOPT4,C' '                                                       
         BH    *+10                                                             
         MVC   QOPT4,PROGPROF+8    DEFAULT SUMMARIES ONLY                       
         CLI   QOPT5,C' '                                                       
         BH    *+10                                                             
         MVC   QOPT5,PROGPROF+9    DEFAULT SPECIAL REP BREAKOUT                 
         CLI   QOPT6,C' '                                                       
         BH    *+10                                                             
         MVC   QOPT6,PROGPROF+11   DEFAULT PRINT COMMENTS                       
* MAKE SURE DEFAULT VALUES SET                                                  
         LA    R1,QOPT1                                                         
         LA    R0,6                                                             
         LA    RE,DFLTOPTS                                                      
*                                                                               
PR23     CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,PR23                                                          
*                                                                               
         CLI   QBYID,C' '                                                       
         BH    *+10                                                             
         MVC   QBYID,PROGPROF+10   DEFAULT ID SEQUENCE                          
*                                                                               
         CLI   QOPT4,C'Y'          TEST SUMMARIES ONLY                          
         BNE   *+8                                                              
         MVI   QBYID,C'N'          THEN NO ID SEQUENCE                          
*                                                                               
         CLI   PROGPROF+12,C'Y'    TEST DEFAULT PROG=AB                         
         BNE   PR23B                                                            
         CLC   QPROG(2),=C'AB'     IS IT AB ALREADY                             
         BE    PR23B               YES                                          
         MVI   QPROG+1,C'B'        NOW IT IS                                    
* FETCH FIRST TWO CHARS OF AB PROFILE FOR DATE OPTIONS                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0AB'                                                 
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*--->    MVI   WORK+10,C'*'                                                     
         MVI   WORK+10,C'*'                                                     
         L     RE,ADCLT                                                         
         MVC   WORK+11(1),COFFICE-CLTHDRD(RE)                                   
         GOTO1 GETPROF,DMCB,WORK,WORK+24,DATAMGR                                
         OC    WORK+24(2),WORK+24  MAKE SURE THERE IS SOMETHING THERE           
         BZ    *+10                                                             
         MVC   PROGPROF(4),WORK+24 ELSE MOVE TO PROGPROF                        
*                                                                               
PR23B    CLI   Q2GUP,C' '          ANY DOLLAR ADJUSTMENT                        
         BNE   PR23C               YES - IGNORE DEFAULT                         
* READ A2A/ABA PROFILE FOR GROSS/NET DEFAULT                                    
         XC    WORK,WORK                                                        
         MVI   WORK,C'S'                                                        
         NI    WORK,X'FF'-X'40'    MAKE 'S' LOWERCASE                           
         MVC   WORK+1(2),QPROG                                                  
         MVI   WORK+3,C'A'         SET FOR A2A PROFILE                          
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*--->    MVI   WORK+10,C'*'                                                     
         MVI   WORK+10,C'*'                                                     
         L     RE,ADCLT                                                         
         MVC   WORK+11(1),COFFICE-CLTHDRD(RE)                                   
         GOTO1 GETPROF,DMCB,WORK,WORK+24,DATAMGR                                
*                                                                               
         MVI   RQGETBF,C' '        REPORT GROSS                                 
         CLI   WORK+24,C'N'        TEST DEFAULT TO NET                          
         BNE   PR23C                                                            
         MVI   RQGETBF,C'X'                                                     
*                                                                               
PR23C    MVI   RQGSTOPT,0                                                       
         CLI   AGYCAN,C'C'         TEST CANADIAN AGENCY                         
         BNE   PR23X                                                            
         CLI   QGST,C'Y'           C'Y' AND C'I' ARE INPUT GST REQ'S            
         BE    *+12                                                             
         CLI   QGST,C'I'                                                        
         BNE   PR23D                                                            
         MVI   RQGSTOPT,C'Y'       SET INPUT GST OPTION                         
         B     PR23X                                                            
*                                                                               
PR23D    CLI   QGST,C'O'           TEST OUTPUT GST OPTION                       
         BNE   PR23X                                                            
         CLI   QOPT1,C'O'          TEST ORDERED SUMMARY                         
         BE    PR23E                                                            
         CLI   QOPT1,C'B'          TEST BILLABLE SUMMARY                        
         BE    PR23E                                                            
         CLI   QOPT1,C'R'          TEST BILLED SUMMARY                          
         BE    PR23E                                                            
*---> INVALID REQUEST                                                           
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         MVC   P(L'SP@INVL3),SP@INVL3                                           
         B     PR24                                                             
PR23E    MVI   PROGPROF+4,0        SUPPRESS ALL BREAK-OUTS                      
         MVI   DATASW,DSTAX        SUPPRESS EXCHANGE BUT FORCE TAX              
*                                                                               
PR23X    CLI   QOPT2,C'A'                                                       
         BNE   *+16                                                             
         CLI   SPOTPROF+2,3        TEST BRDCST OR CAL MNTHS                     
         BNH   *+8                                                              
         MVI   QOPT2,C'N'          ELSE SUPPRESS AUTH REQUEST                   
*                                                                               
         MVI   FCRDGOAL,C'N'       SUPPRESS GOALS                               
         CLI   QOPT2,C'G'           UNLESS SPECIFICALLY REQUESTED               
         BNE   *+8                                                              
         MVI   FCRDGOAL,C'Y'                                                    
*                                                                               
         MVI   RCSUBPRG,1          ORDERED                                      
         CLI   QOPT1,C'O'                                                       
         BE    PR25                                                             
         MVI   RCSUBPRG,2          PAID                                         
         CLI   QOPT1,C'P'                                                       
         BE    PR25                                                             
         MVI   RCSUBPRG,3          UNPAID                                       
         CLI   QOPT1,C'U'                                                       
         BE    PR25                                                             
         MVI   RCSUBPRG,6          COMMISSION                                   
         CLI   QOPT1,C'C'                                                       
         BE    PR25                                                             
         MVI   RCSUBPRG,7          POL UNALL                                    
         CLI   QOPT1,C'N'                                                       
         BE    PR25                                                             
         MVI   FCRDGOAL,C'B'       SET TO READ NEW BILLS                        
         MVI   RCSUBPRG,4          BILLED                                       
         CLI   QOPT1,C'R'                                                       
         BE    PR25                                                             
         MVI   RCSUBPRG,5          BILLABLE                                     
         CLI   QOPT1,C'B'                                                       
         BE    PR25                                                             
*                                                                               
         MVI   RCSUBPRG,100                                                     
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P(43),=C'** INVALID REPORT TYPE - REQUEST IGNORED **'            
         MVC   P(L'SP@INVL1),SP@INVL1                                           
         DROP  RE                                                               
PR24     MVC   P2(80),QAREA                                                     
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
DFLTOPTS DC    C'ONYNNN'                                                        
         EJECT                                                                  
PR25     MVC   PSTART(4),=X'0000FFFF'                                           
         CLI   QOPT1,C'P'                                                       
         BNE   PR26                                                             
         CLC   QAREA+49(12),SPACES                                              
         BE    PR26                                                             
         GOTO1 DATCON,DMCB,QAREA+49,(2,PSTART)                                  
         GOTO1 (RF),(R1),QAREA+55,(2,PEND)                                      
*                                                                               
PR26     XC    PRGROSS,PRGROSS                                                  
*                                                                               
         CLI   Q2GUP,C' '          TEST REQUESTED                               
         BE    PR29                NO                                           
*                                                                               
         CLC   =C'GROSS',Q2GUP     USED FOR OVRD IF PROFILE IS NET              
         BE    PR29                                                             
*                                                                               
         CLC   =C'NET',Q2GUP                                                    
         BNE   PR27                                                             
         MVI   RQGETBF,C'X'        X MEANS RETURN NET DOLLARS                   
         B     PR29                                                             
*                                                                               
PR27     CLC   =C'BF',Q2GUP         TEST FOR BILL FORMULA ADJUSTMENTS           
         BNE   PR28                 NO-THEN IT'S A PERCENTAGE                   
         CLC   =C'NO',QEST          ONLY ALLOW FOR EST=ALL OR                   
         BE    PR29                 SINGLE ESTIMATE                             
         CLC   QESTEND,SPACES                                                   
         BH    PR29                                                             
         MVI   RQGETBF,C'Y'                                                     
         B     PR29                                                             
*                                                                               
PR28     LA    RE,Q2GUP+7          POINT TO END OF FIELD                        
         LA    R0,8                AND SET COUNTER TO MAX LEN                   
*                                                                               
         CLI   0(RE),C' '          SCAN BACKWARDS FOR DATA                      
         BNE   *+10                                                             
         BCTR  R0,0                                                             
         BCT   RE,*-10                                                          
*                                                                               
         GOTO1 =V(CASHVAL),DMCB,(4,Q2GUP),(R0)                                  
         L     R0,4(R1)                                                         
         ST    R0,PRGROSS          SAVE PERCENT ADJUST                          
         MVI   PRGROSS,4           SET DECIMAL PLACES                           
         CLI   0(R1),X'FF'         CHECK FOR ERROR                              
         BE    *+10                                                             
         LTR   R0,R0                                                            
         BP    PR29                                                             
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P(45),=C'** INVALID GROSS-UP PCTG - REQUEST IGNORED **'          
         MVC   P(L'SP@INVL2),SP@INVL2                                           
         DROP  RE                                                               
         MVI   RCSUBPRG,100                                                     
         B     PR24                                                             
         EJECT                                                                  
PR29     CLI   QMED,C'N'                                                        
         BNE   PR29X                                                            
         CLI   AGYCAN,C'C'         TEST CANADA                                  
         BNE   PR29X                                                            
         CLI   FCRDGOAL,C'B'       TEST READ NEW BILLS                          
         BNE   PR29X                                                            
         SPACE 1                                                                
*=========================================================*                     
* BUILD A TABLE OF CANADIAN NETWORK SEQUENCE NUMBERS      *                     
*=========================================================*                     
         SPACE 1                                                                
         L     R4,=A(CNNETTAB)          GET TABLE ADDRESS                       
         L     R5,=A(CNNETTBX-CNNETTAB) AND LENGTH                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGENCY                                                  
*                                                                               
PR29A    MVC   KEY+8(5),=5X'FF'                                                 
*                                                                               
PR29B    GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PR29X                                                            
*                                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
*                                                                               
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
*                                                                               
PR29C    CLI   0(R6),X'02'         FIND NETWORK DEF ELEMENT                     
         BE    PR29D                                                            
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   PR29C                                                            
         B     PR29A               IGNORE IF ELEMENT MISSING                    
*                                                                               
PR29D    ZIC   RE,2(R6)            GET NETWORK BITS                             
         BCTR  RE,0                                                             
         SLL   RE,2                X 4                                          
         LA    RE,0(R4,RE)         POINT TO SLOT                                
         MVC   0(4,RE),KEY+4       MOVE NETWORK                                 
         B     PR29A                                                            
*                                                                               
PR29X    B     PR192               GO CLEAR ACCUMS AND EXIT                     
         EJECT                                                                  
* ESTFRST                                                                       
*                                                                               
PR30     DS    0H                                                               
         XC    APLG1(APLEND-APLG1),APLG1   CLEAR PRINT LINE ADDRESSES           
*                                                                               
         CLI   RQGETBF,C'Y'        TEST BILL FORMULA ADJUST                     
         BNE   PR30A                                                            
         GOTO1 GETBF,DMCB,,WORK    YES-GET BILL FORMULA                         
*                                                                               
PR30A    MVC   CLTSW(5),=5C'Y'     SET CLT/PGR1/PGR2/PRD ACTIVE                 
*                                                                               
* OVERRIDE SYSTEM PROFILES FOR 'AB' REPORT                                      
         CLC   =C'AB',QPROG                                                     
         BNE   PR31                                                             
         MVC   SPOTPROF+2(1),PROGPROF                                           
         MVC   SPOTPROF+6(3),PROGPROF+1                                         
         SPACE 1                                                                
*==========================================================*                    
* THIS CODE DEALS WITH THE PROBLEM OF THE SAME ESTIMATES   *                    
* HAVING DIFFERENT DATES FOR DIFFERENT PRODUCTS BY FORCING *                    
* THE DATES BACK TO THE SAME DATES AT ESTFRST. IT SHOULD   *                    
* NOT DO THIS IF QEST=ALL                                  *                    
*==========================================================*                    
         SPACE 1                                                                
PR31     OC    PRQDATES,PRQDATES   TEST REQ DATES SAVED YET                     
         BNE   *+10                YES                                          
         MVC   PRQDATES,QSTART     ELSE SAVE NOW                                
*                                                                               
         CLC   =C'ALL',QEST         USE ACTUAL ESTIMATE DATES                   
         BE    *+10                 IF QEST=ALL                                 
         MVC   QSTART(12),PRQDATES  RESTORE REQUEST DATES                       
*                                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
*                                                                               
* COUNT MONTHS IN REQUEST PERIOD                                                
*                                                                               
         L     R0,MEDNUMMO                                                      
         LA    R1,MEDMON01                                                      
         OC    0(2,R1),0(R1)                                                    
         BZ    *+12                                                             
         LA    R1,12(R1)                                                        
         BCT   R0,*-14                                                          
         L     R1,MEDNUMMO                                                      
         SR    R1,R0                                                            
         CVD   R1,DUB                                                           
         ZAP   PRMONCNT,DUB                                                     
* SET COMMENT SEARCH ARGUMENTS                                                  
         MVI   FORCECMT,C'N'                                                    
         L     R6,ADCOMREC                                                      
         XC    0(13,R6),0(R6)                                                   
         XC    BCMTYPE(14),BCMTYPE                                              
         CLI   QOPT5+1,C'Y'                                                     
         BNE   PR32                                                             
         MVI   BCMTYPE,C'A'                                                     
         MVC   BCMCLT,BCLT                                                      
         MVC   BCMPGR,BPGR                                                      
         MVC   BCMPRD,BPRD                                                      
         CLI   BESTEND,0           TEST EST SERIES                              
         BNE   *+10                YES - NO EST COMMENTS                        
         MVC   BCMEST,BEST                                                      
         GOTO1 GETCOM                                                           
         MVI   FORCECMT,C'Y'                                                    
         EJECT                                                                  
PR32     MVI   MEDEXTPW,C'N'                                                    
         L     RF,ADCLT                                                         
         LA    RF,CPWPCT-CLTHDRD(RF)                                            
         OC    0(3,RF),0(RF)       TEST PW CLIENT                               
         BZ    PR32A                                                            
         L     RF,ADEST                                                         
         LA    RF,EPWPCT-ESTHDRD(RF)                                            
         OC    0(4,RF),0(RF)       TEST PW CLIENT                               
         BZ    PR32A                                                            
         CLI   QPWCV,C'Y'          TEST CLIENT VERSION REQUEST                  
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
*                                                                               
PR32A    CLI   RCSUBPRG,4          TEST BILLED/BILLABLE SUMMARY                 
         BL    EXIT                                                             
         CLI   RCSUBPRG,5                                                       
         BH    EXIT                                                             
* NEED TO DETERMINE BILLING PERIOD NUMBERS                                      
         GOTO1 DATCON,DMCB,QSTART,WORK                                          
         MVC   QSTART(6),WORK                                                   
         GOTO1 (RF),(R1),QEND                                                   
         MVC   QEND(6),WORK                                                     
* GO BACK 30 MONTHS AND FORWARD 2                                               
         GOTO1 ADDAY,(R1),(C'M',QSTART),WORK,F'-30'                             
         GOTO1 (RF),(R1),(C'M',QEND),WORK+6,F'2'                                
*                                                                               
         IC    R0,SPOTPROF+2       DATE CONTROL                                 
*                                                                               
         GOTO1 MOBILE,DMCB,(50,WORK),((R0),ADBUY),0                             
*                                                                               
* NOW WORK OUT PERIOD NUMBERS - FIND PLACES WHERE YEAR CHANGES                  
*                                                                               
         L     R5,APRBLPER                                                      
         XC    0(256,R5),0(R5)                                                  
         XC    256(L'PRBLPER-256,R5),256(R5)                                    
         L     R4,ADBUY                                                         
*                                                                               
PR34A    DS    0H                                                               
         BAS   RE,CKNEWYR          TEST START OF NEW YEAR                       
         BE    PR34A2              YES                                          
         LA    R4,4(R4)                                                         
         B     PR34A                                                            
*                                                                               
PR34A2   DS    0H                  SET INITIAL YEAR                             
         ZIC   R3,2(R4)                                                         
         SRL   R3,1                                                             
*                                                                               
PR34B    DS    0H                                                               
         LA    R1,1                FOR PERIOD NUMBERS WITHIN YEAR               
PR34B2   DS    0H                                                               
         STC   R3,0(R5)            YEAR                                         
         STC   R1,1(R5)            MONTH                                        
         MVC   2(4,R5),0(R4)       START-END OF PERIOD                          
         LA    R5,6(R5)                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'         TEST E-O-L                                   
         BE    PR34C                                                            
         LA    R1,1(R1)                                                         
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BNE   PR34B2              NO - CONTINUE                                
         LA    R3,1(R3)            GET NEXT YEAR                                
         B     PR34B                                                            
         EJECT                                                                  
* REQUEST START MUST AGREE WITH SOME BILLING PERIOD START                       
*                                                                               
PR34C    L     R4,APRBLPER                                                      
         LA    R5,MEDMON01                                                      
PR34D    CLC   2(2,R4),0(R5)                                                    
         BE    PR145               GO READ BILL RECORDS                         
         BH    PR34ERR                                                          
         LA    R4,6(R4)                                                         
         B     PR34D                                                            
*                                                                               
PR34ERR  MVI   RCSUBPRG,100                                                     
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P(42),=C'** REQUEST/BILLING PERIODS DO NOT MATCH **'             
         MVC   P(L'SP@REQ01),SP@REQ01                                           
         DROP  RE                                                               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*                                  FIND START OF NEW YEAR                       
*                                  1) A PERIOD THAT SPANS YEAR CHANGE           
*                                     AND BEGINS NO FURTHER AWAY                
*                                     FROM 12/31 THAN IT ENDS                   
*                             OR   2) A PERIOD THAT STARTS BEFORE 1/14          
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R4)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
         SPACE 2                                                                
* STAFRST                                                                       
*                                                                               
PR35     DS    0H                                                               
         MVI   XCHIND,0            RESET                                        
         CLI   FCRDBUYS,C'I'       IF READING BY ID POINTERS                    
         BNE   EXIT                                                             
         L     RF,NXTMKTSV         SAVE MARKET NUMBER/NAME                      
         USING MKTSVD,RF                                                        
         MVC   MKTSVMKT,BMKT        MARKET NO.                                  
         MVC   MKTSVMKN,MKTNM                                                   
         DROP  RF                                                               
*                                                                               
         LA    RF,MKTSVL(RF)                                                    
         MVI   0(RF),X'FF'         SET EOL                                      
         L     R0,=A(MKTSVTBX)                                                  
         A     R0,RELO                                                          
         CR    RF,R0                                                            
         BL    *+6                                                              
         DC    H'0'                TOO MANY MKTS FOR MGR/ID                     
         ST    RF,NXTMKTSV                                                      
         B     EXIT                                                             
         SPACE 2                                                                
* MKTFRST                                                                       
*                                                                               
PR38     MVI   USRSW1,C'N'         RESET MKT NAME PRINTED                       
         MVI   PRID,0              RESET UNIQUE ID NUM                          
         MVI   XCHIND,0            RESET                                        
         B     EXIT                                                             
         SPACE 2                                                                
* MGRFRST                                                                       
*                                                                               
PR39     CLI   FCRDBUYS,C'I'       ONLY IF DOING ID POINTERS                    
         BNE   EXIT                                                             
         L     RF,=A(MKTSVTAB)     RESET MARKET DATA SAVE                       
         A     RF,RELO                                                          
         ST    RF,NXTMKTSV                                                      
         PACK  DUB(3),MGR3+1(5)    HOLD PACKED MGR                              
         MVC   HLDMGR,DUB          NOTE- WHOLE MGR ALWAYS IN MGR3               
         MVI   USRSW1,C'N'         RESET MGR NAME PRINTED                       
         MVI   MGRPSW,C'N'       RESET HAVE PRINTED STATION                     
*                                  DETAILS FOR MGR                              
         B     EXIT                                                             
         EJECT                                                                  
* PROCBUY                                                                       
*                                                                               
PR40     DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         CLC   =C'PRGTYP=',QUESTOR                                              
         BNE   *+14                                                             
         CLC   BDPROGT,QUESTOR+7                                                
         BNE   EXIT                                                             
*                                                                               
         CLI   Q2COL32,C' '        TEST RATE TYPE FILTER                        
         BNH   PR40A                                                            
         GOTO1 =A(RATEFLT)                                                      
         BNE   EXIT                                                             
*                                                                               
PR40A    OC    XCHIND,BDCIND2      SET EXCHANGE FLAG                            
         MVC   MEDBRAND,BPRD                                                    
         CLI   MEDBRAND,X'FF'                                                   
         BNE   PR41                                                             
         CLI   QOPT1,C'N'          TEST POL UNALL REQ                           
         BNE   *+8                                                              
         MVI   MEDBRAND,219                                                     
*                                                                               
         CLI   QPGR,C' '                                                        
         BE    PR41                                                             
         CLC   =C'POL',QPRD                                                     
         BNE   PR41                                                             
         L     RE,SVPRDADR                                                      
         MVC   MEDBRAND,5(RE)      POL PGR REQ ==> 1 PRD AT A TIME              
*                                                                               
PR41     DS    0H                                                               
         MVI   PRDATYP,C'S'        SET SUMMARY DATA                             
         LA    R0,PRSPHK                                                        
         ST    R0,SPOTHOOK                                                      
*                                                                               
*                                                                               
PR42     GOTO1 MEDGETBY,DMCB,(RA),0                                             
*                                                                               
         CLI   MEDSPILL,C'Y'       IGNORE SPILL BUYS                            
         BE    EXIT                                                             
         EJECT                                                                  
*======================================================*                        
* POST TO SUMMARY ACCUMS                               *                        
*======================================================*                        
         SPACE 1                                                                
PR43     DS    0H                                                               
**NOP**  CLI   QGST,C'O'           FOR OUTPUT GST OPTION SUMMARY DATA           
**NOP**  BE    PR44X               COMES FROM PRODUCT TOTALS                    
         L     R2,APRSUMPR                                                      
         USING SUMACCD,R2                                                       
         LA    R4,MEDMON01                                                      
         L     R5,MEDNUMMO                                                      
*                                                                               
PR44     L     R1,4(R4)            GET DATA ADDRESS                             
         USING MEDDATA,R1                                                       
*                                                                               
         OC    0(4,R4),0(R4)                                                    
         BZ    PR44B                                                            
         L     R0,MEDBYGRS         GET GROSS DOLLARS                            
*                                                                               
         CLI   PROGPROF+14,4       TEST SUPPRESS C58+MSF                        
         BE    PR44A                                                            
         CLI   PROGPROF+14,9       TEST SUPPRESS C58+MSF                        
         BE    PR44A                                                            
*                                                                               
         TM    DATASW,DSC58        TEST C58 BREAK OUT                           
         BO    PR44A               YES                                          
         A     R0,MEDX58G          IF NO, ADD TO GROSS                          
*                                                                               
         TM    DATASW,DSMSF        TEST MSF BREAK OUT                           
         BO    PR44A               YES                                          
         A     R0,MEDXMSG          IF NO, ADD TO GROSS                          
*                                                                               
PR44A    ST    R0,GUPGROSS                                                      
         MVC   GUPTAX,MEDBYTAX                                                  
         OC    PRGROSS+1(3),PRGROSS+1   TEST GROSS-UP REQUEST                   
         BZ    *+8                                                              
         BAS   RE,GUP                                                           
*                                                                               
         L     R0,GUPGROSS                                                      
         CVD   R0,DUB                                                           
         AP    SUMORD,DUB          ORDERED                                      
*                                                                               
         L     R0,MEDBYPAY                                                      
         S     R0,MEDBYPTG                                                      
         CVD   R0,DUB                                                           
         AP    SUMPAY,DUB          PAID-TO-DATE                                 
*                                                                               
         L     R0,MEDBYPTG                                                      
         CVD   R0,DUB                                                           
         AP    SUMPAYTD,DUB        PAID TODAY                                   
*                                                                               
PR44B    LA    R2,48(R2)                                                        
         LA    R4,12(R4)                                                        
         BCT   R5,PR44                                                          
         B     PR44X                                                            
         EJECT                                                                  
*=======================================================*                       
* SUBROUTINE GROSSES UP DOLLARS BY SPECIFIED PERCENTAGE *                       
* AFTER SUBTRACTING TAX DOLLARS                         *                       
* INPUT IS GUPGROSS/GUPTAX. OUTPUT IS GUPGROSS          *                       
*=======================================================*                       
         SPACE 1                                                                
GUP      NTR1                                                                   
         L     R0,GUPGROSS                                                      
         S     R0,GUPTAX                                                        
         SR    R1,R1                                                            
         ICM   R1,7,PRGROSS+1      GET ADJUSTMENT PERCENTAGE                    
         MR    R0,R0                                                            
         SLDA  R0,1                                                             
*                                                                               
         LH    RE,=H'100'          NEED TO WORK OUT VALUE OF DIVISOR            
         SR    RF,RF                                                            
         ICM   RF,1,PRGROSS        GET NUMBER OF DECIMALS                       
         BZ    *+12                                                             
         MH    RE,=H'10'                                                        
         BCT   RF,*-4                                                           
*                                                                               
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         A     R1,GUPTAX           ADD BACK TAX                                 
         ST    R1,GUPGROSS         STORE RESULT OVER INPUT                      
         B     EXIT                                                             
         EJECT                                                                  
*======================================================*                        
* PROCESS BUY AGAIN FOR DETAIL DATA                    *                        
*======================================================*                        
         SPACE 1                                                                
PR44X    XC    SPOTHOOK,SPOTHOOK                                                
         CLC   PSTART(4),=X'0000FFFF'                                           
         BE    PR45                                                             
         MVI   PRDATYP,C'P'                                                     
         LA    R0,PRSPHK           NO HOOK UNLESS PAYMENT FILTER                
         ST    R0,SPOTHOOK                                                      
*                                                                               
PR45     DS    0H                                                               
*                                                                               
PR46     GOTO1 MEDGETBY,DMCB,(RA),0                                             
*                                                                               
         LA    R0,C'D'             SET BUY DETAIL LEVEL INTRFC                  
         GOTO1 =A(PRINTRFC)                                                     
*                                                                               
         CLI   AGYCAN,C'C'         TEST CANADIAN AGENCY                         
         BNE   PR49                                                             
         CLI   RQCRRNCY,C'C'       TEST REPORTING IN CANADIAN DOLLARS           
         BNE   PR49                NO - USE EXCHANGED DOLLARS                   
*                                                                               
         TM    DATASW,DSEXCH       TEST REPORT EXCHANGE BREAKOUT                
         BZ    PR49                                                             
         BAS   RE,SETDOL           YES - GET DIFFERENCES                        
         B     PR49                                                             
         EJECT                                                                  
*========================================================*                      
* WHEN REPORTING IN CANADIAN $, CALCULATE THE EXCHANGE   *                      
* AMOUNTS AS DIFFERENCES                                 *                      
* IF BUY IS FOR $140C (=$100U), WHEN REPORTING IN $C,    *                      
* WANT TO SHOW IT AS $100 + $40 XCH                      *                      
*========================================================*                      
         SPACE 1                                                                
SETDOL   NTR1  ,                                                                
         LA    R4,MEDMON01                                                      
         L     R5,MEDNUMMO                                                      
*                                                                               
SETDOL2  OC    0(2,R4),0(R4)                                                    
         BZ    SETDOL6                                                          
         L     R1,4(R4)                                                         
         USING MEDDATA,R1                                                       
*                                                                               
         LA    RE,MEDBYPAY         GROSS PAID                                   
         LA    RF,MEDXCHGP         GROSS PAID EXCHANGED                         
         MVC   MEDBYTXP,MEDXCHTP   SET TAX = EXCHANGED PAID                     
         CLI   QOPT1,C'P'                                                       
         BE    SETDOL4                                                          
         LA    RE,MEDBYUNP         GROSS UNPAID                                 
         LA    RF,MEDXCHGU         GROSS UNPAID EXCHANGED                       
         MVC   MEDBYTXU,MEDXCHTU   SET TAX = EXCHANGED UNPAID                   
         CLI   QOPT1,C'U'                                                       
         BE    SETDOL4                                                          
         LA    RE,MEDBYGRS         GROSS ORDERED                                
         LA    RF,MEDXCHG          GROSS EXHCANGED                              
         MVC   MEDBYTAX,MEDXCHT    SET TAX = EXCHANGED TAX                      
*                                                                               
SETDOL4  L     R0,0(RE)            GET $C IN REG                                
         MVC   0(4,RE),0(RF)       THEN SET ACCUM IN $U                         
         S     R0,0(RF)            GET DIFFERENCE                               
         ST    R0,MEDXCHG          AND SET AS EXCHANGE AMOUNT                   
*                                                                               
SETDOL6  LA    R4,12(R4)                                                        
         BCT   R5,SETDOL2                                                       
*                                                                               
SETDOLX  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* POST TO DETAIL ACCUMS                                                         
*                                                                               
PR49     L     R2,APRTOTST                                                      
         LA    R3,8(R2)                                                         
         LA    R4,MEDMON01                                                      
         L     R5,MEDNUMMO                                                      
*                                                                               
         LH    R0,PRCOLS                                                        
         LA    R1,PRDATA                                                        
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LA    R6,PRDATA+8         POST HERE FOR REP TOTALS                     
*                                                                               
PR50     OC    0(2,R4),0(R4)                                                    
         BZ    PR56                                                             
         L     R1,4(R4)                                                         
         USING MEDDATA,R1                                                       
*                                                                               
         CLI   QOPT1,C'O'          ORDERED                                      
         BE    *+12                                                             
         CLI   QOPT1,C'N'          POL UNALLOCATED                              
         BNE   PR50A                                                            
*                                                                               
         L     R0,MEDBYGRS         THIS IS GROSS (+ EXCHANGE)                   
         CLI   PROGPROF+14,4       TEST SUPPRESS C58+MSF                        
         BE    PR52                                                             
         CLI   PROGPROF+14,9       TEST SUPPRESS C58+MSF                        
         BE    PR52                                                             
         TM    DATASW,DSC58        TEST BREAK OUT C58                           
         BO    *+8                                                              
         A     R0,MEDX58G          NO - INCLUDE IN GROSS                        
         TM    DATASW,DSMSF        TEST BREAK OUT MSF                           
         BO    *+8                 YES                                          
         A     R0,MEDXMSG          NO - INCLUDE IN GROSS                        
         B     PR52                                                             
*                                                                               
PR50A    CLI   QOPT1,C'P'          PAID                                         
         BNE   PR50B                                                            
         MVC   MEDBYSPT,MEDBYPSP                                                
         L     R0,MEDBYPAY                                                      
         MVC   MEDBYTAX,MEDBYTXP                                                
         B     PR52                                                             
*                                                                               
PR50B    CLI   QOPT1,C'U'          UNPAID                                       
         BNE   PR50C                                                            
         L     R0,MEDBYSPT                                                      
         S     R0,MEDBYPSP                                                      
         ST    R0,MEDBYSPT                                                      
         L     R0,MEDBYUNP                                                      
         MVC   MEDBYTAX,MEDBYTXU                                                
         B     PR52                                                             
*                                                                               
PR50C    CLI   QOPT1,C'R'          BILLED                                       
         BE    EXIT                                                             
*                                                                               
PR50D    CLI   QOPT1,C'B'          BILLABLE                                     
         BNE   PR50E                                                            
         L     R0,MEDBYGRS         BUYREC GROSS = BILLABLE                      
         B     PR52                                                             
*                                                                               
PR50E    CLI   QOPT1,C'C'          COMMISSION                                   
         BNE   PR50X                                                            
         L     R0,MEDBYGRS                                                      
         S     R0,MEDBYNET           COMMISSION=GROSS-NET                       
         XC    MEDBYTAX(12),MEDBYTAX CLEAR TAX                                  
         B     PR52                                                             
*                                                                               
PR50X    DC    H'0'                                                             
*                                                                               
PR52     ST    R0,GUPGROSS                                                      
         MVC   GUPTAX,MEDBYTAX     SAVE TAX AMOUNT                              
         OC    PRGROSS+1(3),PRGROSS+1   TEST GROSS-UP REQUEST                   
         BZ    PR53                     NO                                      
         BAS   RE,GUP                                                           
*                                                                               
PR53     L     R0,GUPGROSS                                                      
         CVD   R0,DUB                                                           
         AP    0(8,R3),DUB         MONTHLY DOLLARS                              
         AP    0(8,R6),DUB         MONTHLY REP DOLLARS                          
*                                                                               
         TM    DATASW,DSTAX        TEST REPORTING TAX                           
         BZ    PR54                NO - LEAVE TAX ACCUMS ALONE                  
*                                                                               
         ZAP   DUB,=P'0'                                                        
         ICM   R0,15,MEDBYTAX                                                   
         BZ    PR54                                                             
         CVD   R0,DUB                                                           
         AP    104(8,R3),DUB       TAX DOLLARS                                  
         AP    104(8,R6),DUB        AND REP TAX DOLLARS                         
*                                                                               
         CLI   PROGPROF+4,C'X'     TEST EXCLUDE TAX FROM GROSS                  
         BNE   PR54                                                             
         SP    0(8,R3),DUB                                                      
         SP    0(8,R6),DUB                                                      
*                                                                               
PR54     TM    DATASW,DSEXCH       EXCHANGE $                                   
         BZ    PR54A                                                            
         ICM   R0,15,MEDXCHG                                                    
         BZ    PR54A                                                            
         CVD   R0,DUB                                                           
         AP    208(8,R3),DUB                                                    
         AP    208(8,R6),DUB                                                    
*                                                                               
PR54A    TM    DATASW,DSC58        CANADIAN C58 RESERVE                         
         BZ    PR54B                                                            
         ICM   R0,15,MEDX58G                                                    
         BZ    PR54B                                                            
         CVD   R0,DUB                                                           
         AP    312(8,R3),DUB                                                    
         AP    312(8,R6),DUB                                                    
*                                                                               
PR54B    TM    DATASW,DSMSF        CANADIAN MEDIA SERVICE FEE                   
         BZ    PR55                                                             
         ICM   R0,15,MEDXMSG                                                    
         BZ    PR55                                                             
         CVD   R0,DUB                                                           
         AP    416(8,R3),DUB                                                    
         AP    416(8,R6),DUB                                                    
*                                                                               
PR55     L     R0,MEDBYSPT                                                      
         CVD   R0,DUB                                                           
         AP    0(8,R2),DUB         TOTAL SPOTS                                  
         AP    PRDATA(8),DUB       REP SPOTS                                    
*                                                                               
PR56     LA    R3,8(R3)            NEXT MONTH BUCKET                            
         LA    R6,8(R6)                                                         
         LA    R4,12(R4)                                                        
         BCT   R5,PR50                                                          
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         OC    BDREP,BDREP         TEST SPECIAL REP                             
         BZ    PR57                NO                                           
         CLI   QOPT5,C'Y'          TEST SPECIAL REP REQ                         
         BNE   PR57                                                             
* CREATE BUFFALO RECS FOR REP                                                   
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,C'R'         TYPE                                         
         MVC   PRKREP,BDREP        REP NUM                                      
         MVC   PRKMKT(5),BUYKEY+4  MKT-STA                                      
         MVC   PRNAME,MKTNM        SAVE MKT NAME                                
         BAS   RE,PUTBUFF                                                       
         MVC   PRKSTA,=3X'FF'      MKT TOTS                                     
         BAS   RE,PUTBUFF                                                       
         MVC   PRKMKT,=3X'FF'      REP TOTS                                     
         BAS   RE,PUTBUFF                                                       
         DROP  R6                                                               
*                                                                               
PR57     DS    0H                                                               
         CLI   QMED,C'N'           TEST NTWK REQUEST                            
         BNE   PR59                                                             
         L     R6,ADAGY                                                         
         USING AGYHDRD,R6                                                       
         CLI   AGYPROF+7,C'C'      TEST CANADIAN AGY                            
         BNE   PR59                                                             
         DROP  R6                                                               
* ACCUMULATE TOTALS BY NTWK                                                     
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
PR58     IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'68'                                                      
         BNE   PR58                                                             
* CREATE BUFFALO REC FOR NTWK                                                   
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,C'N'                                                      
         MVC   PRKNTWK,2(R6)                                                    
         BAS   RE,PUTBUFF                                                       
         XC    PRKNTWK,PRKNTWK                                                  
         MVC   PRKNTWK(3),=3X'FF'                                               
         BAS   RE,PUTBUFF                                                       
*                                                                               
PR59     DS    0H                                                               
         CLI   QPGR,C' '                                                        
         BE    PR59X                                                            
         CLC   =C'POL',QPRD                                                     
         BNE   PR59X                                                            
* FIND LAST BRAND PROCESSED FOR POL PGRP REQUEST                                
         L     RE,SVPRDADR                                                      
PR59A    CLC   MEDBRAND,5(RE)                                                   
         BE    PR59B                                                            
         LA    RE,6(RE)                                                         
         CLI   5(RE),0                                                          
         BNE   PR59A                                                            
         DC    H'0'                                                             
*                                                                               
PR59B    CLC   0(2,RE),6(RE)       TEST NEXT PRD SAME PRDGRP                    
         BNE   PR59X                                                            
         MVC   MEDBRAND,11(RE)     YES - PROCESS NEXT PRD                       
         B     PR41                                                             
*                                                                               
PR59X    B     EXIT                                                             
         EJECT                                                                  
* SPOTHOOK                                                                      
*                                                                               
         DROP  RB,RC,R8                                                         
         USING *,RF                                                             
PRSPHK   NTR1                                                                   
         LM    R7,RC,SPHKR7                                                     
         B     SPHK2                                                            
SPHKR7   DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
         USING SPA202,RB,RC,R8                                                  
SPHK2    CLI   PRDATYP,C'P'                                                     
         BE    SPHK10                                                           
         MVI   CLTSW,C'Y'                                                       
         MVC   CLTSW+1(10),CLTSW                                                
*&&DO                                                                           
* CODE BELOW DISABLED 13MAR06 WHEN WE CONVERTED THE FILE TO ALLOW               
* CANADIAN NETWORK COST OVERRIDES AND TURNED OFF THE X'20' FLAGS                
         L     R6,SPOTADDR                                                      
         CLI   0(R6),X'0B'                                                      
         BL    SPHK4                                                            
         CLI   0(R6),X'0D'                                                      
         BH    SPHK4                                                            
         L     RE,ADBUY                                                         
         CLI   3(RE),X'FF'         TEST POL BUY                                 
         BNE   SPHK4                                                            
         OC    4(2,RE),4(RE)       TEST MARKET 0                                
         BNZ   SPHK4               NO                                           
         TM    6(R6),X'20'         TEST COST OVRD PRESENT                       
         BO    SPHK4               YES - OK                                     
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P(29),=C'**** MISSING $0 OVERRIDE ****'                          
         MVC   P(L'SP@MIS01),SP@MIS01                                           
         DROP  RE                                                               
         GOTO1 HEXOUT,DMCB,ADBUY,P+30,13,=C'MIX'                                
         GOTO1 REPORT                                                           
*&&                                                                             
SPHK4    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
SPHK10   L     R6,SPOTADDR                                                      
         CLC   PSTART,4(R6)        TEST PAID WITHIN LIMITS                      
         BH    SPHK12                                                           
         CLC   PEND,4(R6)                                                       
         BL    SPHK12                                                           
         B     EXIT                                                             
SPHK12   XC    4(2,R6),4(R6)       NOT PAID IN LIMITS, SET UNPAID               
         B     EXIT                                                             
*                                                                               
PRDATYP  DS    C                                                                
         LTORG                                                                  
         EJECT                                                                  
* STALAST                                                                       
*                                                                               
PR60     DS    0H                                                               
         L     R2,APRTOTST                                                      
         BAS   RE,TSTZERO                                                       
         BZ    EXIT                                                             
* PUT STATION TOTALS OUT TO BUFFALO                                             
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,C'S'                                                      
         MVC   PRKMKT(5),BMKT      SET MKT/STA                                  
         CLI   FCRDBUYS,C'I'       TEST READING ID POINTERS                     
         BNE   *+10                                                             
         MVC   PRKREP(2),HLDMGR    SET MGR IN SPECIAL REP FIELD                 
         IC    RE,PRID                                                          
         LA    RE,1(RE)                                                         
         STC   RE,PRID                                                          
         CLI   QBYID,C'Y'                                                       
         BNE   *+8                                                              
         STC   RE,PRKMKT+5         SET UNIQUE ID FOR TOTALS BY ID               
         L     R6,ADSTAT                                                        
         USING STARECD,R6                                                       
         MVC   PRNAME(9),BIGSTA    SAVE CALL LETTERS                            
         MVC   PRNAME+9(1),SSIZE   AND SIZE                                     
         MVC   PRNAME+10(12),BUYID                                              
         DROP  R6                                                               
         LA    R1,PRDATA                                                        
         L     R2,APRTOTST                                                      
         BAS   RE,MVDATA                                                        
         BAS   RE,PUTBUFF                                                       
*                                                                               
         CLC   =C'FDT',QUESTOR                                                  
         BNE   PR62                                                             
         LA    R0,C'S'             SET STATION TOT LEVEL INTERFACE              
         GOTO1 =A(PRINTRFC)                                                     
PR62     LH    R0,PRCOLS                                                        
         L     R1,APRTOTST                                                      
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* MKTLAST                                                                       
*                                                                               
PR65     DS    0H                                                               
         CLI   FCRDBUYS,C'I'     IF READING ID POINTERS SKIP MKTLAST            
         BE    EXIT              (STATION DETAILS AT MGRLAST)                   
         XC    PRKEY,PRKEY                                                      
         MVI   PRKEY,C'S'                                                       
         MVC   PRKMKT,BMKT                                                      
         LA    R1,=C'HIGH'                                                      
         BAS   RE,BUFFX                                                         
         BNE   EXIT                                                             
         CLC   PRKMKT,BMKT                                                      
         BNE   EXIT                                                             
*                                                                               
         MVI   CLTSW,C'Y'          SET ACTIVITY SWITCHES                        
         MVC   CLTSW+1(9),CLTSW      THRU MKT                                   
*                                                                               
         MVI   PRSUMSW,0           RESET FOR HEADLINES                          
         MVI   MODE,STALAST        SET MODE WHILE PRINTING STATION TOTS         
*                                                                               
PR65A    LA    R2,PRDATA                                                        
         BAS   RE,TSTZERO                                                       
         BZ    PR68                                                             
*                                                                               
         CLC   =C'FDT',QUESTOR                                                  
         BE    PR65A1                                                           
         LA    R0,C'S'             SET STATION TOT LEVEL INTERFACE              
         GOTO1 =A(PRINTRFC)                                                     
*                                                                               
* ROLL STATION TOTS TO MKT/MGR3/MGR2/MGR1/PRD/PGR2/PGR1/CLT                     
*                                                                               
PR65A1   L     R3,APRTOTMK                                                      
PR65B    LA    R2,PRDATA                                                        
         LA    R0,NACCUMS          ACCUMS/ROW                                   
*                                                                               
PR65C    AP    0(8,R3),0(8,R2)                                                  
         LA    R3,8(R3)                                                         
         LA    R2,8(R2)                                                         
         BCT   R0,PR65C                                                         
         L     R0,APRTOTCL                                                      
         CR    R3,R0                                                            
         BNH   PR65B                                                            
* IF BY ID, PRINT STATION (AND MKT IF NEEDED) ON LINE ABOVE AMOUNTS             
         CLI   QBYID,C'Y'                                                       
         BE    PR65D                                                            
*                                                                               
         LA    R2,PRDATA                                                        
         GOTO1 =A(FMTTOT)                                                       
*                                                                               
PR65D    MVC   P+25(9),PRNAME                                                   
*                                                                               
         CLI   USRSW1,C'N'         TEST MKT NAME PRINTED YET                    
         BNE   PR66                                                             
         MVC   P(24),MKTNM                                                      
         MVI   ALLOWLIN,8                                                       
PR66     DS    0H                                                               
         CLI   PROGPROF+3,C'Y'     TEST TO PRINT STATION SIZE                   
         BNE   PR66A                                                            
         MVI   P+22,C' '           SUPPRESS LAST LETTER OF MKT                  
         MVC   P+23(1),PRNAME+9                                                 
*                                                                               
PR66A    CLI   QBYID,C'Y'          TEST ID SEQ                                  
         BNE   PR66B               NO                                           
         LA    R2,PRDATA           FORMAT DOLLARS                               
         GOTO1 =A(FMTTOT)                                                       
* COPY ALL PRINT LINES DOWN 1 LINE                                              
         LA    R0,13                                                            
         LA    RE,P13                                                           
         LA    RF,P14                                                           
         MVC   0(132,RF),0(RE)                                                  
         SH    RE,=H'132'                                                       
         SH    RF,=H'132'                                                       
         BCT   R0,*-14                                                          
*                                                                               
         MVC   P+34(98),SPACES     BLANK ALL BUT MARKET/STATION                 
         MVC   P2(34),SPACES       AND BLANK MKT STA ON LINE 2                  
         MVC   P2+9(12),BUYIDNAM   CONTRACT ID TO LINE 2                        
         MVC   P2+22(12),PRNAME+10                                              
         GOTO1 SQUASHER,DMCB,P2+9,25                                            
         B     PR66D                                                            
*                                                                               
PR66B    CLI   QOPT4,C'Y'          TEST SUMMARIES ONLY                          
         BNE   PR66C                                                            
         BAS   RE,CLRPRT                                                        
         B     PR67                                                             
*                                                                               
PR66C    CLI   QPWCV,C'Y'          TEST PW CLIENT VERSION                       
         BNE   PR66D                                                            
         MVC   P14,P               SAVE FIRST PRINT LINE                        
         BAS   RE,CLRPRT           CLEAR 10 PRINT LINES                         
         MVC   P(40),P14           RESTORE MARKET/STATION/SPOTS                 
         MVC   P14,SPACES                                                       
         CLI   USRSW1,C'Y'         TEST MKTNAME PRINTED YET                     
         BE    *+8                                                              
         MVI   ALLOWLIN,2                                                       
         B     PR66E                                                            
*                                                                               
PR66D    DS    0H                                                               
         CP    PRMONCNT,=P'6'                                                   
         BNH   *+8                                                              
         MVI   SPACING,2                                                        
PR66E    DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
PR67     MVI   ALLOWLIN,0          RESET                                        
         MVI   USRSW1,C'Y'         SET MKT NAME PRINTED                         
*                                                                               
PR68     DS    0H                                                               
         BAS   RE,SEQBUFF                                                       
         BNE   PR68X                                                            
         CLC   PRKMKT,BMKT                                                      
         BNE   PR68X                                                            
         B     PR65A                                                            
PR68X    DS    0H                                                               
         EJECT                                                                  
         MVI   MODE,MKTLAST        SET MODE FOR MARKET TOTALS                   
PR70     CLI   MKTSW,C'N'                                                       
         BE    EXIT                                                             
*                                                                               
         L     R2,APRTOTMK                                                      
         BAS   RE,TSTZERO                                                       
         BZ    PR72                                                             
         GOTO1 =A(FMTTOT)                                                       
*                                                                               
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+2(24),=C'** MARKET 9999 TOTALS **'                             
         MVC   P+2(2),=C'**'                                                    
         MVC   P+5(L'SPSMNTL),SPSMNTL                                           
         MVC   P+24(2),=C'**'                                                   
         MVC   P+12(4),MKT                                                      
         DROP  RE                                                               
*                                                                               
         MVI   SPACING,2                                                        
         CLI   QOPT4,C'Y'                                                       
         BNE   *+12                                                             
         BAS   RE,CLRPRT                                                        
         B     PR72                                                             
         GOTO1 REPORT                                                           
*                                                                               
PR72     MVI   MKTSW,C'N'                                                       
         MVI   USRSW1,C'N'         RESET MKT NAME PRTD                          
         OC    BMKT,BMKT           TEST PROCESSING MKT 0                        
         BNZ   EXIT                NO                                           
         CLI   QMGR,C' '           TEST BY MKTGRP                               
         BE    EXIT                NO                                           
         MVI   MODE,MGR3LAST       FORCE MGR3 BREAK                             
         CLC   MGR3LEN,MGR2LEN     TEST 3 LEVELS OF MKTGRP                      
         BNE   PR80                YES - PROCESS                                
         MVI   MODE,MGR2LAST                                                    
         CLC   MGR2LEN,MGR1LEN     TEST 2 LEVELS OF MKTGRP                      
         BNE   PR90                                                             
         MVI   MODE,MGR1LAST                                                    
         B     PR100                                                            
         SPACE 2                                                                
CLRPRT   LA    R0,10                                                            
         LA    R1,P                                                             
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
         MVI   SPACING,1           RESET                                        
         BR    RE                                                               
         EJECT                                                                  
* MGR3LAST                                                                      
*                                                                               
PR80     CLI   FCRDBUYS,C'I'       IF READING ID POINTERS                       
         BNE   PR81                PRINT STATION  DETAILS NOW                   
         CLI   MGRPSW,C'Y'         UNLESS ALREADY DONE                          
         BE    PR81                                                             
         MVI   MGRPSW,C'Y'                                                      
         BAS   RE,MGRSTA                                                        
*                                                                               
PR81     DS    0H                                                               
         CLI   MGR3SW,C'N'                                                      
         BE    EXIT                                                             
         MVI   MGR3SW,C'N'                                                      
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,X'13'                                                     
         MVC   PRKMGR,MGR3                                                      
         MVC   PRCODE,SPACES                                                    
         MVC   PRCODE(6),MGR3N                                                  
         MVC   PRNAME,MGR3NM                                                    
         LA    R1,PRDATA                                                        
         L     R2,APRTOTM3                                                      
         BAS   RE,MVDATA                                                        
         BAS   RE,PUTBUFF                                                       
*                                                                               
         L     R2,APRTOTM3                                                      
         BAS   RE,TSTZERO                                                       
         BZ    PR82                                                             
         GOTO1 =A(FMTTOT)                                                       
         MVI   P+7,C'*'                                                         
         MVC   P+9(12),MGR3BK                                                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+23(8),=C'TOTALS *'                                             
         MVC   P+23(L'SP8TOTAL),SP8TOTAL                                        
         DROP  RE                                                               
         GOTO1 SQUASHER,DMCB,P+7,24                                             
         MVI   SPACING,2                                                        
         CLI   QOPT4,C'Y'          TEST RECAPS ONLY                             
         BNE   *+12                                                             
         BAS   RE,CLRPRT                                                        
         B     PR82                                                             
         GOTO1 REPORT                                                           
PR82     MVI   FORCEHED,C'Y'                                                    
         OC    BMKT,BMKT                                                        
         BNZ   EXIT                                                             
         MVI   MODE,MGR2LAST                                                    
         EJECT                                                                  
* MGR2LAST                                                                      
*                                                                               
PR90     CLI   FCRDBUYS,C'I'       IF READING ID POINTERS                       
         BNE   PR91                PRINT STATION  DETAILS NOW                   
         CLI   MGRPSW,C'Y'         UNLESS ALREADY DONE                          
         BE    PR91                                                             
         MVI   MGRPSW,C'Y'                                                      
         BAS   RE,MGRSTA                                                        
*                                                                               
PR91     DS    0H                                                               
         CLI   MGR2SW,C'N'                                                      
         BE    EXIT                                                             
         MVI   MGR2SW,C'N'                                                      
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,X'12'                                                     
         MVC   PRKMGR,MGR2                                                      
         MVC   PRCODE,SPACES                                                    
         MVC   PRCODE(6),MGR2N                                                  
         MVC   PRNAME,MGR2NM                                                    
         LA    R1,PRDATA                                                        
         L     R2,APRTOTM2                                                      
         BAS   RE,MVDATA                                                        
         BAS   RE,PUTBUFF                                                       
*                                                                               
         MVI   PRSUMSW,X'13'                                                    
         BAS   RE,PRRECAP                                                       
         BAS   RE,CLRBUFF                                                       
         CP    PRCNTR,=P'1'                                                     
         BH    PR96                                                             
         MVI   FORCEHED,C'N'                                                    
         MVI   PRSUMSW,0                                                        
*                                                                               
PR96     L     R2,APRTOTM2                                                      
         BAS   RE,TSTZERO                                                       
         BZ    PR98                                                             
         GOTO1 =A(FMTTOT)                                                       
         MVI   P+7,C'*'                                                         
         MVC   P+9(12),MGR2BK                                                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+23(8),=C'TOTALS *'                                             
         MVC   P+23(L'SP8TOTAL),SP8TOTAL                                        
         DROP  RE                                                               
         GOTO1 SQUASHER,DMCB,P+7,24                                             
         MVI   SPACING,2                                                        
         CLI   QOPT4,C'Y'          TEST RECAPS ONLY                             
         BNE   PR97                NO                                           
         CLC   MGR2LEN,MGR3LEN     TEST 3 LEVELS                                
         BNE   PR97                YES - NEED ALL BUT LOWEST                    
         BAS   RE,CLRPRT                                                        
         B     PR98                                                             
*                                                                               
PR97     GOTO1 REPORT                                                           
PR98     MVI   FORCEHED,C'Y'                                                    
         OC    BMKT,BMKT                                                        
         BNZ   EXIT                                                             
         MVI   MODE,MGR1LAST                                                    
         EJECT                                                                  
* MGR1LAST                                                                      
*                                                                               
PR100    CLI   FCRDBUYS,C'I'       IF READING ID POINTERS                       
         BNE   PR101               PRINT STATION  DETAILS NOW                   
         CLI   MGRPSW,C'Y'         UNLESS ALREADY DONE                          
         BE    PR101                                                            
         MVI   MGRPSW,C'Y'                                                      
         BAS   RE,MGRSTA                                                        
*                                                                               
PR101    DS    0H                                                               
         CLI   MGR1SW,C'N'                                                      
         BE    EXIT                                                             
         MVI   MGR1SW,C'N'                                                      
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,X'11'                                                     
         MVC   PRKMGR,MGR1                                                      
         MVC   PRCODE,SPACES                                                    
         MVC   PRCODE(6),MGR1N                                                  
         MVC   PRNAME,MGR1NM                                                    
         LA    R1,PRDATA                                                        
         L     R2,APRTOTM1                                                      
         BAS   RE,MVDATA                                                        
         BAS   RE,PUTBUFF                                                       
*                                                                               
         MVI   PRSUMSW,X'12'                                                    
         BAS   RE,PRRECAP                                                       
         BAS   RE,CLRBUFF                                                       
         CP    PRCNTR,=P'1'                                                     
         BH    PR106                                                            
         CLI   QOPT4,C'Y'          TEST SUMMARIES ONLY                          
         BE    *+8                                                              
         MVI   FORCEHED,C'N'                                                    
         MVI   PRSUMSW,0                                                        
*                                                                               
PR106    L     R2,APRTOTM1                                                      
         BAS   RE,TSTZERO                                                       
         BZ    PR108                                                            
         GOTO1 =A(FMTTOT)                                                       
         MVI   P+7,C'*'                                                         
         MVC   P+9(12),MGR1BK                                                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+23(8),=C'TOTALS *'                                             
         MVC   P+23(L'SP8TOTAL),SP8TOTAL                                        
         DROP  RE                                                               
         GOTO1 SQUASHER,DMCB,P+7,24                                             
         CLI   QOPT4,C'Y'          TEST RECAPS ONLY                             
         BNE   PR107               NO                                           
         CLC   MGR1LEN,MGR2LEN     TEST 2 OR MORE LEVELS                        
         BNE   PR107               YES - NEED ALL BUT LOWEST                    
         BAS   RE,CLRPRT                                                        
         B     PR108                                                            
*                                                                               
PR107    GOTO1 REPORT                                                           
PR108    MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
* PRDLAST                                                                       
*                                                                               
PR110    DS    0H                                                               
         SPACE 1                                                                
* CLEAR ALL BUFFALO 'S' RECS *                                                  
         SPACE 1                                                                
         MVI   PRSUMSW,C'S'                                                     
         BAS   RE,CLRBUFF                                                       
*                                                                               
         LA    R0,C'P'             SET PRD TOT LEVEL INTRFC                     
         GOTO1 =A(PRINTRFC)                                                     
*                                                                               
         CLI   PRDSW,C'N'                                                       
         BE    PR120                                                            
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,X'20'                                                     
         MVC   PRKPRD,PRD                                                       
         MVC   PRCODE,SPACES                                                    
         MVC   PRCODE(3),PRD                                                    
         MVC   PRNAME,PRDNM                                                     
         LA    R1,PRDATA                                                        
         L     R2,APRTOTPR                                                      
         BAS   RE,MVDATA                                                        
         BAS   RE,PUTBUFF                                                       
*                                                                               
         CLI   QPGR,C' '           TEST PRDGRPS                                 
         BE    PR114                                                            
         CLC   PGR1LEN,PGR2LEN     TEST 2 LEVELS                                
         BE    *+12                NO                                           
*                                                                               
         MVI   PRKTYP,X'22'        GEN PGR2TOT REC                              
         BAS   RE,PUTBUFF                                                       
*                                                                               
         MVI   PRKTYP,X'21'        GEN PGR1TOT REC                              
         BAS   RE,PUTBUFF                                                       
*                                                                               
PR114    LA    R0,3*27             CLEAR ALL MGRTOTS (27 * 3 ROWS)              
         L     R1,APRTOTM3                                                      
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         MVI   PRSUMSW,X'11'                                                    
         BAS   RE,PRRECAP                                                       
         BAS   RE,CLRBUFF                                                       
         CP    PRCNTR,=P'1'                                                     
         BH    *+8                                                              
         MVI   PRSUMSW,0                                                        
*                                                                               
         CLI   QMGR,C' '           TEST MKTGRPS                                 
         BE    PR116               NO                                           
* BY MKTGRP - PRINT TOTS ON NEW PAGE IF NO RECAP - ELSE ON RECAP PG             
         MVI   FORCEHED,C'Y'                                                    
         ZAP   DUB,=P'2'           MIN DATA REQ'D UNLESS RECAPS ONLY            
         CLI   QOPT4,C'Y'          TEST RECAPS ONLY                             
         BNE   *+10                NO                                           
         ZAP   DUB,=P'1'           THIS OPTION PRINTS RECAP IF ANY DATA         
         CP    PRCNTR,DUB          TEST RECAP PRINTED                           
         BL    *+8                                                              
         MVI   FORCEHED,C'N'       TOTALS ON SAME PAGE IF RECAPPED              
*                                                                               
PR116    L     R2,APRTOTPR                                                      
         L     R3,APRSUMPR                                                      
         BAS   RE,CALCGST          CALCULATE OUTPUT GST IF NEEDED               
*                                                                               
         CLI   QOPT4,C'Y'          TEST SUMMARIES ONLY                          
         BNE   PR117                                                            
         CLI   QMGR,C' '           TEST BY MKTGRP                               
         BNE   PR117                                                            
         SPACE 2                                                                
* SUPPRESS PRD TOTALS IF SUMMARIES ONLY AND NO MKTGRPS                          
         LA    R0,27                                                            
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R0,*-10                                                          
         B     PR120                                                            
*                                                                               
PR117    BAS   RE,TSTZERO                                                       
         BZ    PR120                                                            
*                                                                               
PR118    DS    0H                                                               
         GOTO1 =A(FMTTOT)                                                       
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+7(20),=C'** PRODUCT TOTALS **'                                 
         MVC   P+7(L'SP@PRDTL),SP@PRDTL                                         
         DROP  RE                                                               
         GOTO1 REPORT                                                           
*                                                                               
PR120    DS    0H                                                               
         GOTO1 =A(PROCEB),DMCB,(RC)  PROCESS ESTIMATES AND BILLS                
         B     PR150                                                            
         EJECT                                                                  
*========================================================*                      
* ROUTINE COMPUTES OUTPUT GST AMOUNT AT STANDARD RATE    *                      
* CALCULATION OCCURS AT PRDLAST                          *                      
* ROUTINE ALSO ROLLS PRODUCT TOTALS TO ORDERED SUMMARY   *                      
* ACCUMS, BECAUSE GST ADJUSTMENT MUST OCCUR AFTER TAX    *                      
* HAS BEEN SUBTRACTED                                    *                      
* ON ENTRY R2 POINTS TO TOTALS ROW                       *                      
*          R3 POINTS TO SUMMARY ROW                      *                      
*========================================================*                      
         SPACE 1                                                                
CALCGST  NTR1                                                                   
*                                                                               
         CLI   AGYCAN,C'C'         TEST CANADIAN AGENCY                         
         BNE   CALCGSTX                                                         
         CLI   QGST,C'O'           TEST OUTPUT GST REQUEST                      
         BNE   CALCGSTX                                                         
*                                                                               
         L     RE,ADCLT                                                         
         CLI   CEXTRA+11-CLTHDRD(RE),C'X'                                       
         BE    CALCGSTX                                                         
         CLI   CEXTRA+11-CLTHDRD(RE),C'Z'                                       
         BE    CALCGSTX                                                         
         CLI   MODE,PRDLAST                                                     
         BH    CALCGST0                                                         
         L     RE,ADPRD                                                         
         CLI   PGSTCODE-PRDHDRD(RE),C'X'                                        
         BE    CALCGSTX                                                         
         CLI   PGSTCODE-PRDHDRD(RE),C'Z'                                        
         BE    CALCGSTX                                                         
*                                                                               
CALCGST0 LA    R2,8(R2)            POINT TO FIRST DOLLAR AMOUNT                 
         LA    R0,13               SET FOR 13 MONTHS                            
*                                                                               
CALCGST2 ZAP   DUB,0(8,R2)         MONTH DOLLARS                                
         SP    DUB,104(8,R2)       SUBTRACT TAX                                 
         MP    DUB,=P'7'            X GST RATE                                  
         SRP   DUB,64-2,5          ADD 5/ DIVIDE BY 100                         
         ZAP   104(8,R2),DUB       POST TO TAX ROW                              
         B     CALCGST4            **** NOP FORCED SUMMARY ROW *****            
*                                                                               
         LTR   R3,R3               TEST INHIBIT SUMMARY CALCULATIONS            
         BZ    CALCGST4                                                         
         ZAP   0(8,R3),0(8,R2)     FORCE SUMMARY ORDERED DOLLARS                
**NOP**  AP    0(8,R3),DUB         AND ADD GST                                  
         LA    R3,48(R3)                                                        
*                                                                               
CALCGST4 LA    R2,8(R2)                                                         
         BCT   R0,CALCGST2                                                      
*                                                                               
CALCGSTX XIT1                                                                   
         EJECT                                                                  
* PROCGOAL (BUDGETS)                                                            
*                                                                               
PR140    DS    0H                                                               
         CLI   FCRDGOAL,C'B'       TEST NEW BILLING PROCESSED                   
         BE    EXIT                                                             
*                                                                               
         L     RE,ADGOAL                                                        
         MVC   MEDSPTLN,9(RE)      SET SPTLEN                                   
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         MVI   MEDSPTLN,0          UNSET SPTLEN                                 
*                                                                               
* POST TO SUMMARY ACCUMS                                                        
*                                                                               
         L     R2,APRSUMPR                                                      
         USING SUMACCD,R2                                                       
         LA    R4,MEDMON01                                                      
         L     R5,MEDNUMMO                                                      
*                                                                               
PR142    L     R1,4(R4)                                                         
         USING MEDDATA,R1                                                       
         OC    0(4,R4),0(R4)                                                    
         BZ    PR144                                                            
         L     R0,MEDGLD                                                        
         CVD   R0,DUB                                                           
         AP    SUMAUTH,DUB                                                      
*                                                                               
PR144    LA    R2,48(R2)                                                        
         LA    R4,12(R4)                                                        
         BCT   R5,PR142                                                         
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
*======================================================*                        
* NEW BILL RECORD PROCESSING                           *                        
* THIS PROCESSING OCCURS AT ESTFRST                    *                        
*                                                      *                        
*  REQUEST    CAN    US       BILLED     EXCHANGE      *                        
*  CURRENCY   BILL?  BILL?    DOLLARS    DOLLARS       *                        
*  --------   -----  -----    -------    --------      *                        
*     C         Y      N         C          C          *                        
*                                                      *                        
*     C         Y      Y         U          C-U        *                        
*                                                      *                        
*     U         Y      Y         C          U-C        *                        
*======================================================*                        
         SPACE 1                                                                
PR145    DS    0H                                                               
*                                                                               
         MVI   STASW,C'N'          RESET SWITCH                                 
*                                                                               
         CLI   FCRDGOAL,C'B'       TEST NEW BILLRECS PROCESSED                  
         BNE   EXIT                                                             
*                                                                               
         MVI   PRBLLCUR,0          SET CURRENCY BYTE VALUE (KEY+12)             
         CLI   AGYCAN,C'C'                                                      
         BNE   PR145A                                                           
         CLI   RQCRRNCY,C'U'       TEST US CURR REQ                             
         BNE   PR145A                                                           
         MVI   PRBLLCUR,C'U'                                                    
*                                                                               
PR145A   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD                                                    
         CLI   BPRD,X'FF'                                                       
         BNE   *+8                                                              
         MVI   KEY+5,0                                                          
*                                                                               
PR145B   GOTO1 HIGH                                                             
         B     PR145D                                                           
*                                                                               
PR145C   GOTO1 SEQ                                                              
*                                                                               
PR145D   CLC   KEY(5),KEYSAVE      0E01/A-M/CLT                                 
         BNE   PR146Z                                                           
         CLC   KEY(6),KEYSAVE      0E01/A-M/CLT/PRD                             
         BE    PR145F                                                           
         SPACE 1                                                                
* CHANGE OF PRODUCT *                                                           
         SPACE 1                                                                
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BNE   PR146Z                                                           
*                                                                               
PR145F   DS    0H                                                               
         TM    DATASW,DSEXCH       TEST EXCHANGE ACTIVE                         
         BO    *+14                YES - PROCESS ALL CURRENCIES                 
         CLC   PRBLLCUR,KEY+12     ELSE TEST RIGHT CURRENCY                     
         BNE   PR145C                                                           
*                                                                               
         CLI   BEST,0                                                           
         BE    PR145G                                                           
         CLC   KEY+6(1),BEST                                                    
         BE    PR146                                                            
         BL    PR145C              IF LO, READ SEQ                              
         CLC   KEY+6(1),BESTEND                                                 
         BH    PR145C              IF HI, READ SEQ                              
         B     PR146               ELSE GO PROCESS                              
*                                                                               
PR145G   ZIC   RE,KEY+6                                                         
         LA    RE,ESTLST(RE)                                                    
         CLI   0(RE),0                                                          
         BE    PR145C                                                           
*                                                                               
PR146    GOTO1 =A(PRBILL)          READ AND PROCESS BILL RECORD                 
         CLI   STASW,C'Y'          TEST ACTIVE                                  
         BNE   PR145C              NO - CONTINUE                                
*                                                                               
         TM    DATASW,DSEXCH       TEST EXCHANGE ACTIVE                         
         BO    PR147               YES                                          
*                                                                               
         CLI   STASW,C'Y'                                                       
         BNE   PR146X                                                           
         BAS   RE,PUTBUFF                                                       
*                                                                               
         CLI   AGYCAN,C'C'         TEST CANADIAN                                
         BNE   PR146X                                                           
         CLI   QMED,C'N'                                                        
         BNE   PR146X                                                           
* CREATE BUFFALO REC FOR NTWK                                                   
         GOTO1 MSUNPK,DMCB,PRKMKT,WORK,STA                                      
         L     RE,=A(CNNETTAB)     DETERMINE IF NTWK OR STATION                 
         LA    R0,CNNETTBN                                                      
PR146A   CLC   0(4,RE),STA                                                      
         BE    PR146B              NOTE EXITS WITH RE AT NETWORK                
         LA    RE,4(RE)                                                         
         BCT   R0,PR146A                                                        
*                                                                               
         ZIC   RE,PRKMKT+4         GET NETWORK NUMBER                           
         BCTR  RE,0                                                             
         SLL   RE,2                X 4                                          
         L     R0,=A(CNNETTAB)                                                  
         AR    RE,R0               POINT TO NTWK ENTRY IN TABLE                 
*                                                                               
PR146B   XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,C'N'                                                      
         MVC   PRKNTWK,0(RE)                                                    
         ZAP   PRDATA(8),=P'0'    FORCE SPOTS TO 0 SO GET                       
         BAS   RE,PUTBUFF          NON-ZERO TOTAL SO WILL PRINT                 
         XC    PRKNTWK,PRKNTWK                                                  
         MVC   PRKNTWK(3),=3X'FF'                                               
         BAS   RE,PUTBUFF                                                       
PR146X   MVI   STASW,C'N'                                                       
         B     PR145C              GO READ SEQUENTIAL                           
         SPACE 1                                                                
* ATTEMPT TO PRINT BILLING DETAILS FOR CANAD NTWK                               
         SPACE 1                                                                
PR146Z   CLI   AGYCAN,C'C'                                                      
         BNE   EXIT                                                             
         CLI   QMED,C'N'                                                        
         BNE   EXIT                                                             
         MVI   MODE,MKTLAST        FORCE MODE                                   
         XC    BMKT,BMKT                                                        
         MVC   MKT,=C'0000'                                                     
         MVC   MGR3(1),QMGR                                                     
         MVC   MGR3+1(4),SPACES                                                 
         MVC   MGR2,MGR3                                                        
         MVC   MGR1,MGR2                                                        
         MVC   MKTNM,=CL24'NETWORK BILLING'                                     
         MVC   MGR3NM,MKTNM                                                     
         MVC   MGR1NM,MKTNM                                                     
         MVC   MGR1NM,MKTNM                                                     
         B     PR65                                                             
         EJECT                                                                  
*======================================================*                        
* EXCHANGE IS ACTIVE - LOOK FOR BILL IN OTHER CURRENCY *                        
*======================================================*                        
         SPACE 1                                                                
PR147    MVC   KEYSAVE,KEY         READ FOR BILL IN OTHER CURRENCY              
         GOTO1 SEQ                                                              
         CLC   KEY(12),KEYSAVE     TEST SAME KEY/OTHER CURRENCY                 
         BE    PR147A              YES                                          
*                                                                               
         BAS   RE,PUTBUFF                                                       
         MVI   STASW,C'N'                                                       
         B     PR145D              REMEMBER DID SEQ READ, SO GO TEST            
         SPACE 1                                                                
*=====================================================*                         
* FOUND BILL IN US CURRENCY                           *                         
*=====================================================*                         
         SPACE 1                                                                
PR147A   L     RE,=A(SVPRDATA)     POINT TO SAVE AREA                           
         MVC   0(216,RE),PRDATA    SAVE 27 ACCUMS (SPOTS/$/TAX)                 
*                                                                               
         MVI   STASW,C'N'          FORCE TO CLEAR IF ACTIVE                     
         GOTO1 =A(PRBILL)          PROCESS SECOND BILL                          
         CLI   STASW,C'Y'          TEST ACTIVE                                  
         BNE   PR147X              NO - PUT OLD PRDATA (STILL THERE)            
*                                                                               
         CLI   RQCRRNCY,C'C'       TEST CAN REQ                                 
         BE    PR147B              NO                                           
* EXCHANGE SAVED PRDATA AND CURRENT PRDATA                                      
         L     RE,=A(SVPRDATA)                                                  
         XC    PRDATA(216),0(RE)                                                
         XC    0(216,RE),PRDATA                                                 
         XC    PRDATA(216),0(RE)                                                
         SPACE 1                                                                
* BILLED $ = CURRENT PRDATA, EXCHANGE = SAVED - CURRENT                         
         SPACE 1                                                                
PR147B   LA    R0,13                                                            
         LA    R1,PRDATA+8                                                      
         L     R3,=A(SVPRDATA)                                                  
         LA    R3,8(R3)                                                         
*                                                                               
PR147C   ZAP   208(8,R1),0(8,R1)   SET EXCH TO CURRENT                          
         AP    208(8,R1),104(8,R1)  + TAX                                       
         MP    208(8,R1),=P'-1'    MAKE IT NEGATIVE                             
         AP    208(8,R1),0(8,R3)   NOW ADD IN CURRENT                           
         AP    208(8,R1),104(8,R3)  + TAX                                       
         LA    R1,8(R1)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,PR147C                                                        
*                                                                               
PR147X   BAS   RE,PUTBUFF                                                       
         MVI   STASW,C'N'                                                       
         B     PR145C              GO READ SEQUENTIAL                           
         EJECT                                                                  
*=======================================================*                       
* ROLL PRD SUM DATA TO PGR2/PGR1/CLT                    *                       
*=======================================================*                       
         SPACE 1                                                                
PR150    L     R3,APRSUMP2                                                      
*                                                                               
PR152    L     R4,APRSUMPR                                                      
         LA    R5,78               6 ACCUMS/MO X 13 MONTHS                      
*                                                                               
PR154    AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R5,PR154                                                         
*                                                                               
         L     R0,APRSUMCL                                                      
         CR    R3,R0                                                            
         BNH   PR152                                                            
* PRINT SUMMARY                                                                 
         L     R2,APRSUMPR                                                      
         CLI   QOPT4,C'Y'          TEST SUMMARIES ONLY                          
         BNE   PR155               NO                                           
         CLI   QMGR,C' '           TEST MKTGRPS                                 
         BNE   PR155               YES - PRINT SUMMARY                          
         SPACE 1                                                                
* TEST FOR CANADIAN NETWORK REQUEST    *                                        
* WHICH NEEDS TO PRINT PRODUCT SUMMARY *                                        
         SPACE 1                                                                
         MVI   PRTSUMSW,C'N'       NO PRODUCT SUMMARY IS PRINTED                
         CLI   QMED,C'N'           TEST NTWK REQUEST                            
         BNE   PR156                                                            
         L     R6,ADAGY                                                         
         USING AGYHDRD,R6                                                       
         CLI   AGYPROF+7,C'C'      TEST CANADIAN AGY                            
         BE    PRREP               IF YES DO NOT CLEAR SUMMARY TOTS             
         DROP  R6                                                               
PR155    BAS   RE,PRSUM                                                         
*                                                                               
PR156    BAS   RE,CLRSUM                                                        
*                                                                               
         GOTO1 =A(PRTBOT)          PRINT COMMENTS AT BOTTOM OF PAGE             
         EJECT                                                                  
* PRINT SPECIAL REP RECAP IF NEEDED                                             
*                                                                               
PRREP    MVI   PRSUMSW,C'R'                                                     
         BAS   RE,HIGHBUFF                                                      
         BNE   PRNTWK                                                           
PRREP2   MVI   KEY,C'0'            READ REP RECORD                              
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),QMED                                                    
         GOTO1 VRCPACK,DMCB,(C'U',PRKREP),KEY+2                                 
         MVC   KEY+5(2),AGY                                                     
         GOTO1 HIGHREP                                                          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PRREP4   MVC   P+1(24),PRNAME                                                   
         MVC   MKTNM,PRNAME        SET IT HERE TOO FOR HL RTN                   
         MVI   ALLOWLIN,6                                                       
*                                                                               
PRREP6   LA    R2,P+25                                                          
         GOTO1 MSUNPK,DMCB,PRKMKT,MKT,(R2)                                      
         CLI   4(R2),C' '                                                       
         BE    *+14                                                             
         MVC   5(1,R2),4(R2)                                                    
         MVI   4(R2),C'-'                                                       
*                                                                               
         LA    R2,PRDATA                                                        
         GOTO1 =A(FMTTOT)                                                       
         MVI   ALLOWLIN,2                                                       
         GOTO1 REPORT                                                           
         MVI   ALLOWLIN,0                                                       
         MVI   USRSW1,C'Y'         SET MKT NAME PRINTED                         
*                                                                               
         BAS   RE,SEQBUFF                                                       
         BNE   PRREPX                                                           
         CLC   PRKSTA,=3X'FF'                                                   
         BNE   PRREP6                                                           
* MARKET TOTALS                                                                 
         LA    R2,PRDATA                                                        
         GOTO1 =A(FMTTOT)                                                       
*                                                                               
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+7(18),=C'MARKET 9999 TOTALS'                                   
         MVC   P+7(L'SPSMNTL),SPSMNTL                                           
         MVC   P+14(4),MKT                                                      
         DROP  RE                                                               
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVI   USRSW1,C'N'         RESET MKT NAME PRTD                          
*                                                                               
         BAS   RE,SEQBUFF                                                       
         CLC   PRKMKT,=3X'FF'                                                   
         BNE   PRREP4                                                           
* REP TOTALS                                                                    
         LA    R2,PRDATA                                                        
         GOTO1 =A(FMTTOT)                                                       
         L     R6,ADREP                                                         
         USING REPRECD,R6                                                       
         MVC   P+1(20),RNAME                                                    
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+26(6),=C'TOTALS'                                               
         MVC   P+26(L'SP6TOTAL),SP6TOTAL                                        
         DROP  RE                                                               
         GOTO1 SQUASHER,DMCB,P+1,31                                             
         GOTO1 REPORT                                                           
*                                                                               
PRREPX   MVI   FORCEHED,C'Y'                                                    
         BAS   RE,SEQBUFF                                                       
         BE    PRREP2                                                           
         BAS   RE,CLRBUFF                                                       
         EJECT                                                                  
* PRINT CANAD NTWK RECAP IF NEEDED                                              
*                                                                               
PRNTWK   CLI   QMED,C'N'                                                        
         BNE   PRNTWKX                                                          
         MVI   PRSUMSW,C'N'                                                     
         BAS   RE,HIGHBUFF                                                      
         BNE   PRNTWKX                                                          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PRNTWK2  MVC   P+27(4),PRKNTWK                                                  
*                                                                               
         CLC   =C'NTRACE',QUESTOR  TEST TO TRACE BUFFALO                        
         BNE   PRNTWK4                                                          
         L     RF,BUFFBUFF                                                      
         L     R2,BUFFLALL-BUFFALOD(RF)                                         
         GOTO1 HEXOUT,DMCB,PRREC,P,(R2),=C'N'                                   
         GOTO1 REPORT                                                           
*                                                                               
PRNTWK4  MVI   MODE,STALAST        FORCE MODE SO NO OUTPUT GST                  
         CLI   QGST,C'O'           TEST OUTPUT GST REQUEST                      
         BNE   PRNTWK6                                                          
* SUPPRESS ANY GST/TAX DATA BY SETTING ACCUMS TO 0 !                            
         LA    R1,PRDATA+104                                                    
         LA    R0,13                                                            
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
PRNTWK6  LA    R2,PRDATA                                                        
         GOTO1 =A(FMTTOT)                                                       
         MVI   ALLOWLIN,2                                                       
         GOTO1 REPORT                                                           
         MVI   ALLOWLIN,0                                                       
*                                                                               
         BAS   RE,SEQBUFF                                                       
         BNE   PRNTWKX                                                          
         CLC   PRKNTWK(3),=3X'FF'                                               
         BNE   PRNTWK2                                                          
* PRINT PRD TOTALS                                                              
         MVI   MODE,PRDLAST        RESTORE MODE                                 
         LA    R2,PRDATA                                                        
         SR    R3,R3               NO SUMMARIES                                 
         BAS   RE,CALCGST          GET GST IF NEEDED                            
*                                                                               
         GOTO1 =A(FMTTOT)                                                       
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+7(20),=C'** PRODUCT TOTALS **'                                 
         MVC   P+7(L'SP@PRDTL),SP@PRDTL                                         
         DROP  RE                                                               
         GOTO1 REPORT                                                           
*                                                                               
         L     R2,APRSUMPR         PRINT PAID/BILLED/AUTH SUMMARY               
         BAS   RE,PRSUM                                                         
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PRNTWKX  L     R2,APRSUMPR                                                      
         BAS   RE,CLRSUM                                                        
*                                                                               
         BAS   RE,CLRBUFF                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* PGR2LAST                                                                      
*                                                                               
PR160    CLI   PGR2SW,C'N'                                                      
         BE    EXIT                                                             
         L     R2,APRTOTP2                                                      
         L     R3,APRSUMP2                                                      
         BAS   RE,CALCGST          MAYBE GET OUTPUT GST                         
*                                                                               
         MVC   SVQEST(6),QEST      SAVE QEST/QESTEND                            
         CLC   =C'ALL',QEST        TEST ALL ESTS                                
         BNE   PR160A                                                           
         CLC   =C'ES',QSTAUTO      AND ES DATES                                 
         BE    PR162X              IF SO - SUPPRESS SUMMARY                     
         MVC   QEST(6),SPACES                                                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         MVC   QEST(2),=C'NO'                                                   
         DROP  RE                                                               
PR160A   MVI   PGR2SW,C'N'                                                      
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,X'32'                                                     
         MVC   PRKPGR,PGR2                                                      
         MVC   PRCODE,SPACES                                                    
         MVC   PRCODE(5),PGR2                                                   
         MVC   PRNAME,PGR2NM                                                    
         LA    R1,PRDATA                                                        
         L     R2,APRTOTP2                                                      
         BAS   RE,MVDATA                                                        
         BAS   RE,PUTBUFF                                                       
*                                                                               
         MVI   PRSUMSW,X'22'                                                    
         BAS   RE,PRRECAP                                                       
         BAS   RE,CLRBUFF                                                       
*                                                                               
         CP    PRCNTR,=P'1'                                                     
         BH    PR162                                                            
         MVI   PRSUMSW,0                                                        
*                                                                               
PR162    L     R2,APRTOTP2                                                      
         GOTO1 =A(FMTTOT)                                                       
         MVI   P+7,C'*'                                                         
         MVC   P+9(12),PGR2BK                                                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+23(8),=C'TOTALS *'                                             
         MVC   P+23(L'SP8TOTAL),SP8TOTAL                                        
         DROP  RE                                                               
         GOTO1 SQUASHER,DMCB,P+7,24                                             
         GOTO1 REPORT                                                           
* PRINT SUMMARY                                                                 
         L     R2,APRSUMP2                                                      
         BAS   RE,PRSUM                                                         
*                                                                               
PR162X   MVI   FORCEHED,C'Y'                                                    
         BAS   RE,CLRSUM                                                        
         MVC   QEST(6),SVQEST                                                   
         B     EXIT                                                             
         EJECT                                                                  
* PGR1LAST                                                                      
*                                                                               
PR170    CLI   PGR1SW,C'N'                                                      
         BE    EXIT                                                             
         L     R2,APRTOTP1                                                      
         L     R3,APRSUMP1                                                      
         BAS   RE,CALCGST          MAYBE GET OUTPUT GST                         
*                                                                               
         MVC   SVQEST(6),QEST      SAVE QEST/QESTEND                            
         CLC   =C'ALL',QEST        IF ALL ESTIMATES                             
         BNE   PR170A                                                           
         CLC   =C'ES',QSTAUTO      AND ES DATES                                 
         BE    PR178               SUPPRESS SUMMARY                             
         MVC   QEST(6),SPACES                                                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         MVC   QEST(2),=C'NO'                                                   
         DROP  RE                                                               
PR170A   MVI   PGR1SW,0            CLEAR SWITCH                                 
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,X'31'                                                     
         MVC   PRKPGR,PGR1                                                      
         MVC   PRCODE,SPACES                                                    
         MVC   PRCODE(5),PGR1                                                   
         MVC   PRNAME,PGR1NM                                                    
         LA    R1,PRDATA                                                        
         L     R2,APRTOTP1                                                      
         BAS   RE,MVDATA                                                        
         BAS   RE,PUTBUFF                                                       
* RESET COMMENT SEARCH ARGUMENTS                                                
         CLI   QOPT5+1,C'Y'        TEST PRINTING COMMENTS                       
         BNE   PR171                                                            
         MVI   BCMPRD,0                                                         
         GOTO1 GETCOM                                                           
         MVI   FORCECMT,C'Y'                                                    
* PRINT RECAP BY PRD                                                            
PR171    MVI   PRSUMSW,X'21'                                                    
         BAS   RE,PRRECAP                                                       
         BAS   RE,CLRBUFF                                                       
         MVI   PRSUMSW,0                                                        
         CP    PRCNTR,=P'1'        RECAP PRTD                                   
         BL    PR172               NO                                           
*                                                                               
         OI    PGR1SW,X'80'        SET RECAP 1 PRTD                             
         L     R1,APRTOTPR                                                      
         L     R2,APRTOTP1                                                      
         BAS   RE,MVDATA           SAVE PGR1 TOTS                               
         GOTO1 =A(FMTTOT)                                                       
         L     R1,APRTOTP1                                                      
         L     R2,APRTOTPR                                                      
         BAS   RE,MVDATA           RESTORE                                      
*                                                                               
         LA    R0,27                                                            
         L     R1,APRTOTPR                                                      
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         MVI   P+7,C'*'                                                         
         MVC   P+9(12),PGR1BK                                                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+23(8),=C'TOTALS *'                                             
         MVC   P+23(L'SP8TOTAL),SP8TOTAL                                        
         DROP  RE                                                               
         GOTO1 SQUASHER,DMCB,P+7,24                                             
         GOTO1 REPORT                                                           
* PRINT SUMMARY                                                                 
         L     R2,APRSUMP1                                                      
         BAS   RE,SVSUM                                                         
         BAS   RE,PRSUM                                                         
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,UNSVSUM                                                       
* PRINT RECAP BY PGR2                                                           
PR172    MVI   PRSUMSW,X'32'                                                    
         BAS   RE,PRRECAP                                                       
         BAS   RE,CLRBUFF                                                       
         CP    PRCNTR,=P'1'        TEST RECAP PRTD                              
         BNH   PR174               NO                                           
         OI    PGR1SW,X'40'        SET RECAP 2 PRTD                             
*                                                                               
PR174    TM    PGR1SW,X'40'        TEST RECAP 2 PRTD                            
         BO    PR176               YES - PRINT TOTS                             
         TM    PGR1SW,X'80'        NO RECAP 2 - DID WE DO RECAP 1               
         BO    PR178               YES - DONE                                   
         MVI   FORCEHED,C'Y'       NO - PRT PGR1 TOTS ON NEW PAGE               
PR176    L     R2,APRTOTP1                                                      
         GOTO1 =A(FMTTOT)                                                       
         MVI   P+7,C'*'                                                         
         MVC   P+9(12),PGR1BK                                                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+23(8),=C'TOTALS *'                                             
         MVC   P+23(L'SP8TOTAL),SP8TOTAL                                        
         DROP  RE                                                               
         GOTO1 SQUASHER,DMCB,P+7,24                                             
         GOTO1 REPORT                                                           
         L     R2,APRSUMP1                                                      
         BAS   RE,PRSUM                                                         
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PR178    L     R2,APRSUMP1                                                      
         BAS   RE,CLRSUM                                                        
* CLEAR TOTALS                                                                  
         LA    R0,27                                                            
         L     R1,APRTOTP1                                                      
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         MVI   PGR1SW,C'N'                                                      
         MVC   QEST(6),SVQEST                                                   
         B     EXIT                                                             
         EJECT                                                                  
* CLTLAST                                                                       
*                                                                               
PR180    MVC   QBYID,PRQBYID       RESTORE REQ VALUE                            
         CLI   CLTSW,C'N'                                                       
         BE    EXIT                                                             
         L     R2,APRTOTCL                                                      
         L     R3,APRSUMCL                                                      
         BAS   RE,CALCGST          MAYBE GET OUTPUT GST                         
*                                                                               
         MVI   CLTSW,C'N'                                                       
         MVI   FORCEHED,C'Y'                                                    
* RESET COMMENT SEARCH ARGUMENTS                                                
         CLI   QOPT5+1,C'Y'        TEST PRINTING COMMENTS                       
         BNE   PR181                                                            
         XC    BCMPGR(4),BCMPGR    CLEAR PGR AND PRD                            
         GOTO1 GETCOM                                                           
         MVI   FORCECMT,C'Y'                                                    
* PRINT RECAP BY PRD IF ALL PRD REQUEST                                         
PR181    CLC   =C'ALL',QPRD                                                     
         BNE   PR192                                                            
         CLC   =C'ALL',QEST        BUT NOT IF ALL ESTIMATES                     
         BNE   PR181A                                                           
         CLC   =C'ES',QSTAUTO      UNLESS DATES ARE SPECIFIED                   
         BE    PR192                                                            
*                                                                               
PR181A   MVI   PRSUMSW,X'20'                                                    
         BAS   RE,PRRECAP                                                       
*                                                                               
         L     R2,APRTOTCL                                                      
         CLI   QCLT,C'A'           TEST ONE CLT REQUEST                         
         BNL   *+12                                                             
         BAS   RE,TSTZERO          MUST BE OFFICE OR LIST                       
         BZ    PR192               SO IF NO DATA, NO TOTALS                     
         L     R1,APRTOTPR                                                      
         BAS   RE,MVDATA           SAVE CLIENT TOTALS                           
         GOTO1 =A(FMTTOT)                                                       
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+7(17),=C'* CLIENT TOTALS *'                                    
         MVC   P+7(L'SP@CLITL),SP@CLITL                                         
         DROP  RE                                                               
         GOTO1 REPORT                                                           
         L     R1,APRTOTCL                                                      
         L     R2,APRTOTPR                                                      
         BAS   RE,MVDATA           RESTORE TOTALS                               
* PRINT SUMMARY                                                                 
         MVI   PRTSUMSW,C'N'                                                    
         L     R2,APRSUMCL                                                      
         BAS   RE,SVSUM                                                         
         BAS   RE,PRSUM                                                         
         GOTO1 =A(PRTBOT)                                                       
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,UNSVSUM                                                       
* PRINT RECAP BY PGR1                                                           
         CLI   QPGR,C' '                                                        
         BE    PR192                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   PRSUMSW,X'31'                                                    
         BAS   RE,PRRECAP                                                       
         L     R2,APRTOTCL                                                      
         GOTO1 =A(FMTTOT)                                                       
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+7(17),=C'* CLIENT TOTALS *'                                    
         MVC   P+7(L'SP@CLITL),SP@CLITL                                         
         DROP  RE                                                               
         GOTO1 REPORT                                                           
* PRINT SUMMARY                                                                 
         MVI   PRTSUMSW,C'N'       RESET SWITCH                                 
         L     R2,APRSUMCL                                                      
         BAS   RE,PRSUM                                                         
         GOTO1 =A(PRTBOT)                                                       
         MVI   FORCEHED,C'Y'                                                    
* CLEAR SUMMARY ACCUMS                                                          
*                                                                               
PR192    DS    0H                                                               
         LA    R0,13*6*4           13X6 ACCUMS X 4 ROWS                         
         L     R1,APRSUMPR                                                      
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LA    R0,NACCUMS*NTOTLEVS                                              
         L     R1,APRTOTST                                                      
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO MOVE DATA FROM (R2) TO (R1) FOR LENGTH (LDATA)                     
* NOTE: ROUTINE DESTROYS R0,R4,R5,RF                                            
*                                                                               
MVDATA   LR    R0,RE                                                            
         LR    RE,R1                                                            
         LA    RF,LDATA                                                         
         LR    R4,R2                                                            
         LR    R5,RF                                                            
         MVCL  RE,R4                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* PRINT A RECAP IF 2 OR MORE DATA RECORDS                                       
* UNLESS SUMMARIES ONLY                                                         
*                                                                               
PRRECAP  NTR1                                                                   
*                                                                               
         ZAP   PRCNTR,=P'0'                                                     
         BAS   RE,HIGHBUFF                                                      
         BNE   EXIT                                                             
         CLI   QOPT4,C'Y'                                                       
         BE    RECAP1                                                           
         CLI   PRSUMSW,X'20'                                                    
         BE    RECAP1                                                           
         CLI   PRSUMSW,X'21'       ALWAYS PRINT PGR1 SUMMARY IF DATA            
         BE    RECAP1                                                           
         CLI   PRSUMSW,X'31'       ALSO CLT SUMMARY                             
         BE    RECAP1                                                           
         AP    PRCNTR,=P'1'                                                     
         BAS   RE,SEQBUFF                                                       
         BNE   EXIT                                                             
* PRINT RECAP                                                                   
RECAP1   ZAP   PRCNTR,=P'0'                                                     
         MVI   FORCEHED,C'Y'                                                    
         CLI   PRSUMSW,X'20'       TEST PRD/PGR SUMMARY2                        
         BL    RECAP1X             NO                                           
         CLI   QOPT5+1,C'Y'        TEST PRINTING COMMENTS                       
         BNE   RECAP1X             NO                                           
         MVI   FORCECMT,C'Y'                                                    
RECAP1X  BAS   RE,HIGHBUFF                                                      
         B     *+8                                                              
RECAP2   BAS   RE,SEQBUFF                                                       
         BNE   EXIT                                                             
         AP    PRCNTR,=P'1'                                                     
         LA    R2,PRDATA                                                        
         SR    R3,R3                                                            
         BAS   RE,CALCGST                                                       
*                                                                               
         GOTO1 =A(FMTTOT)                                                       
         MVC   P(6),PRCODE                                                      
         MVC   P+7(24),PRNAME                                                   
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     RECAP2                                                           
         EJECT                                                                  
* STATION DETAIL PRINT AT MGRLAST - (ID READ ONLY)                              
*                                                                               
MGRSTA   NTR1                                                                   
         XC    LMKT,LMKT                                                        
         XC    PRKEY,PRKEY                                                      
         MVI   PRKEY,C'S'                                                       
         MVC   PRKREP,HLDMGR                                                    
         LA    R1,=C'HIGH'                                                      
         BAS   RE,BUFFX                                                         
         BNE   EXIT                                                             
         CLC   PRKREP,HLDMGR                                                    
         BNE   EXIT                                                             
*                                                                               
         MVI   CLTSW,C'Y'          SET ACTIVITY SWITCHES                        
         MVC   CLTSW+1(9),CLTSW      THRU MKT                                   
*                                                                               
         MVI   PRSUMSW,0           RESET FOR HEADLINES                          
         MVI   MODE,STALAST        SET MODE WHILE PRINTING STATION TOTS         
*                                                                               
MGST65A  LA    R2,PRDATA                                                        
         BAS   RE,TSTZERO                                                       
         BZ    MGST68                                                           
*                                                                               
         LA    R0,C'S'             SET STATION TOT LEVEL INTERFACE              
         GOTO1 =A(PRINTRFC)                                                     
*                                                                               
         CLC   PRKMKT,LMKT         TEST VS LAST MKT                             
         BE    MGST65J                                                          
         OC    LMKT,LMKT           FIRST TIME TEST                              
         BZ    MGST65E                                                          
*                                  PRINT MARKET TOTALS                          
MGST65B  DS    0H                                                               
         L     R2,APRTOTMK                                                      
         BAS   RE,TSTZERO                                                       
         BZ    MGST65D                                                          
         GOTO1 =A(FMTTOT)                                                       
*                                                                               
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*--->    MVC   P+2(24),=C'** MARKET 9999 TOTALS **'                             
         MVC   P+2(2),=C'**'                                                    
         MVC   P+5(L'SPSMNTL),SPSMNTL                                           
         MVC   P+24(2),=C'**'                                                   
         DROP  RE                                                               
         SR    RF,RF                                                            
         ICM   RF,3,LMKT                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+12(4),DUB                                                      
*                                                                               
         MVI   SPACING,2                                                        
         CLI   QOPT4,C'Y'                                                       
         BNE   *+12                                                             
         BAS   RE,CLRPRT                                                        
         B     MGST65D                                                          
         GOTO1 REPORT                                                           
*                                                                               
MGST65D  DS    0H                                                               
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
MGST65E  DS    0H                                                               
         CLI   PRKREP,X'FF'        TEST 'EOF'                                   
         BE    MGST72                                                           
*                                  GET MARKET SAVED DATA                        
         L     RF,=A(MKTSVTAB)                                                  
         A     RF,RELO                                                          
         USING MKTSVD,RF                                                        
MGST65G  DS    0H                                                               
         CLI   0(RF),X'FF'         EOL                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MKTSVMKT,PRKMKT                                                  
         BE    *+12                                                             
         LA    RF,MKTSVL(RF)                                                    
         B     MGST65G                                                          
         MVC   P(24),MKTSVMKN      SET MARKET NAME                              
         MVI   ALLOWLIN,8                                                       
*                                                                               
MGST65J  DS    0H                                                               
* ROLL STATION TOTS TO MKT/MGR3/MGR2/MGR1/PRD/PGR2/PGR1/CLT                     
         L     R3,APRTOTMK                                                      
MGST65K  LA    R2,PRDATA                                                        
         LA    R0,NACCUMS          ACCUMS/ROW                                   
*                                                                               
MGST65L  AP    0(8,R3),0(8,R2)                                                  
         LA    R3,8(R3)                                                         
         LA    R2,8(R2)                                                         
         BCT   R0,MGST65L                                                       
         L     R0,APRTOTCL                                                      
         CR    R3,R0                                                            
         BNH   MGST65K                                                          
*                                                                               
         MVC   LMKT,PRKMKT         SAVE CURRENT MKT                             
         LA    R2,PRDATA                                                        
         GOTO1 =A(FMTTOT)                                                       
*                                                                               
         MVC   P+26(7),PRNAME                                                   
         CLI   PROGPROF+3,C'Y'     TEST TO PRINT STATION SIZE                   
         BNE   MGST66A                                                          
         MVI   P+23,C' '           SUPPRESS LAST LETTER OF MKT                  
         MVC   P+24(1),PRNAME+9                                                 
*                                                                               
MGST66A  DS    0H                                                               
         CLI   QOPT4,C'Y'          TEST SUMMARIES ONLY                          
         BNE   *+12                                                             
         BAS   RE,CLRPRT                                                        
         B     MGST67                                                           
*                                                                               
MGST66C  DS    0H                                                               
         CP    PRMONCNT,=P'6'                                                   
         BNH   *+8                                                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
MGST67   MVI   ALLOWLIN,0          RESET                                        
*                                                                               
MGST68   DS    0H                                                               
         BAS   RE,SEQBUFF                                                       
         BNE   MGST68D                                                          
         CLC   PRKREP,HLDMGR                                                    
         BE    MGST65A                                                          
*                                                                               
MGST68D  DS    0H                                                               
         MVC   PRKREP(4),=4X'FF'   SET 'EOF'                                    
         B     MGST65B                                                          
         SPACE 2                                                                
MGST72   MVI   MKTSW,C'N'                                                       
         SPACE 1                                                                
         B     EXIT                                                             
         EJECT                                                                  
PRSUM    NTR1                                                                   
         MVI   PRTSUMSW,C'N'       NO SUMMARY PRINTED YET                       
         USING SUMACCD,R2                                                       
         CLI   QPWCV,C'Y'          TEST PW CLIENT VERSION                       
         BE    PRSUMX              YES - NO SUMMARY                             
* MUST BE ALL STATIONS AND MARKETS                                              
         CLC   =C'ALL',QMKT                                                     
         BNE   PRSUMX                                                           
         CLC   QSTA,SPACES                                                      
         BE    *+14                                                             
         CLC   =C'ALL',QSTA                                                     
         BNE   PRSUMX                                                           
* CHECK FOR ACTIVITY                                                            
         LA    R0,13*6             13X6 ACCUMS                                  
         LR    R1,R2                                                            
         CP    0(8,R1),=P'0'                                                    
         BNE   PRSUM2                                                           
         LA    R1,8(R1)                                                         
         BCT   R0,*-14                                                          
         B     PRSUMX                                                           
*                                                                               
* CALC LINES NEEDED FOR SUMMARY                                                 
*                                                                               
PRSUM2   MVI   PRTSUMSW,C'Y'       SUMMARY WILL BE PRINTED                      
         LA    RE,8                SP/ORD/SP/BLD/SP/BLD TDY/SP/ORD LS           
         CP    PRMONCNT,=P'6'                                                   
         BNH   *+8                                                              
         LA    RE,4(RE)                                                         
         CLI   QOPT2,C'N'          SUPPRESS AUTH                                
         BE    PRSUM4                                                           
         LA    RE,4(RE)            SP/AUTH/SP/AUTH LS ORD                       
         CP    PRMONCNT,=P'6'                                                   
         BNH   *+8                                                              
         LA    RE,2(RE)                                                         
PRSUM4   CLI   QOPT3,C'N'                                                       
         BE    PRSUM6                                                           
         LA    RE,4(RE)            SP/PAID/SP/PAID TDY                          
         CP    PRMONCNT,=P'6'                                                   
         BNH   *+8                                                              
         LA    RE,2(RE)                                                         
*                                                                               
PRSUM6   DS    0H                                                               
         STC   RE,ALLOWLIN                                                      
         MVI   SPACING,2                                                        
         GOTO1 REPORT              SKIP A LINE                                  
         MVI   ALLOWLIN,0                                                       
         EJECT                                                                  
PRSUM8   LA    R4,P                                                             
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*--->    MVC   7(15,R4),=C'ORDERED TO DATE'                                     
         MVC   7(L'SP@ORDDT,R4),SP@ORDDT                                        
         LA    R3,SUMORD                                                        
         BAS   RE,FMTSUM                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QOPT2,C'N'                                                       
         BE    PRSUM10                                                          
*                                                                               
         LA    R4,P                                                             
         CLI   QOPT2,C'G'                                                       
         BNE   *+14                                                             
*--->    MVC   7(12,R4),=C'GOAL DOLLARS'                                        
         MVC   7(L'SP@GLDL,R4),SP@GLDL                                          
         B     *+10                                                             
*--->    MVC   7(18,R4),=C'AUTHORIZED DOLLARS'                                  
         MVC   7(L'SP@ATDL,R4),SP@ATDL                                          
         LA    R3,SUMAUTH                                                       
         BAS   RE,FMTSUM                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,SUMAUTH                                                       
         LA    R4,SUMORD                                                        
         MVI   BYTE,C'-'                                                        
         BAS   RE,ROLLSUM                                                       
*                                                                               
         LA    R4,P                                                             
         CLI   QOPT2,C'G'                                                       
         BNE   *+14                                                             
*--->    MVC   7(17,R4),=C'GOAL LESS ORDERED'                                   
         MVC   7(L'SP@GLORD,R4),SP@GLORD                                        
         B     *+10                                                             
*--->    MVC   7(4,R4),=C'AUTHORIZED LESS ORDERED'                              
         MVC   7(L'SP@AUTH,R4),SP@AUTH                                          
         BAS   RE,FMTSUM                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
PRSUM10  CLI   QOPT3,C'N'                                                       
         BE    PRSUM20                                                          
*                                                                               
         LA    R4,P                                                             
*--->    MVC   7(12,R4),=C'PAID TO DATE'                                        
         MVC   7(L'SP@PDDT,R4),SP@PDDT                                          
         LA    R3,SUMPAY                                                        
         BAS   RE,FMTSUM                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         LA    R4,P                                                             
*--->    MVC   7(10,R4),=C'PAID TODAY'                                          
         MVC   7(L'SP@PDTDY,R4),SP@PDTDY                                        
         LA    R3,SUMPAYTD                                                      
         BAS   RE,FMTSUM                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
PRSUM20  LA    R4,P                                                             
*--->    MVC   7(14,R4),=C'BILLED TO DATE'                                      
         MVC   7(L'SP@BLDT,R4),SP@BLDT                                          
         LA    R3,SUMBLL                                                        
         BAS   RE,FMTSUM                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         LA    R4,P                                                             
*--->    MVC   7(12,R4),=C'BILLED TODAY'                                        
         MVC   7(L'SP@BLDTY,R4),SP@BLDTY                                        
         LA    R3,SUMBLLTD                                                      
         BAS   RE,FMTSUM                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,SUMBLL                                                        
         LA    R4,SUMBLLTD                                                      
         MVI   BYTE,C'+'                                                        
         BAS   RE,ROLLSUM          GET TOTAL BILLED                             
*                                                                               
         LA    R3,SUMORD                                                        
         LA    R4,SUMBLL                                                        
         MVI   BYTE,C'-'                                                        
         BAS   RE,ROLLSUM                                                       
*                                                                               
         LA    R4,P                                                             
*--->    MVC   7(19,R4),=C'ORDERED LESS BILLED'                                 
         MVC   7(L'SP@OLB,R4),SP@OLB                                            
         DROP  R5                                                               
         BAS   RE,FMTSUM                                                        
         GOTO1 REPORT                                                           
*                                                                               
PRSUMX   B     EXIT                                                             
         EJECT                                                                  
FMTSUM   NTR1                                                                   
*                                                                               
         STM   R3,R4,SUMSV3                                                     
         ZAP   HALF,=P'7'                                                       
         CP    PRMONCNT,=P'13'                                                  
         BE    FSUM2                                                            
         ZAP   HALF,=P'6'                                                       
         CP    HALF,PRMONCNT                                                    
         BNH   *+10                                                             
         ZAP   HALF,PRMONCNT                                                    
*                                                                               
FSUM2    LA    R5,118(R4)          SET ADDRESS FOR TOTAL                        
         LA    R4,40(R4)                                                        
FSUM4    DS    0H                                                               
         BAS   RE,SUMEDT                                                        
         LA    R3,48(R3)                                                        
         LA    R4,13(R4)                                                        
         SP    HALF,=P'1'                                                       
         BP    FSUM4                                                            
*                                                                               
         ZAP   HALF,PRMONCNT                                                    
         SP    HALF,=P'6'                                                       
         BNP   FSUM20                                                           
         CP    PRMONCNT,=P'13'                                                  
         BNE   *+10                                                             
         ZAP   HALF,=P'6'                                                       
*                                                                               
         L     R4,SUMSV4                                                        
         LA    R5,132(R5)          SET TOTAL ADDRESS                            
         LA    R4,132+40(R4)                                                    
*                                                                               
FSUM10   DS    0H                                                               
         BAS   RE,SUMEDT                                                        
         LA    R3,48(R3)                                                        
         LA    R4,13(R4)                                                        
         SP    HALF,=P'1'                                                       
         BP    FSUM10                                                           
* NOW GET TOTAL                                                                 
FSUM20   L     R3,SUMSV3                                                        
         LA    R0,13                                                            
         ZAP   DUB,=P'0'                                                        
*                                                                               
         AP    DUB,0(8,R3)                                                      
         LA    R3,48(R3)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         LA    R3,DUB                                                           
         LR    R4,R5                                                            
         CLI   Q2COL31,C'Y'        TEST SUPPRESS TOTAL COLUMN                   
         BE    *+8                                                              
         BAS   RE,SUMEDT                                                        
         B     EXIT                                                             
SUMSV3   DS    A                                                                
SUMSV4   DS    A                                                                
         EJECT                                                                  
SUMEDT   DS    0H                                                               
         STM   RE,R1,CURER1                                                     
*--->    EDIT  (P8,0(R3)),(12,(R4)),2,MINUS=YES                                 
         CURED (P8,0(R3)),(12,(R4)),CURTAB,FLOAT=-                              
         LM    RE,R1,CURER1                                                     
         CP    0(8,R3),=P'0'                                                    
         BM    *+8                                                              
*--->    MVI   11(R4),C'*'                                                      
         MVI   12(R4),C'*'                                                      
         BR    RE                                                               
*                                                                               
ROLLSUM  NTR1                                                                   
*                                                                               
         LA    R0,13                                                            
ROLLSM2  CLI   BYTE,C'+'                                                        
         BNE   *+14                                                             
         AP    0(8,R3),0(8,R4)                                                  
         B     *+10                                                             
         SP    0(8,R3),0(8,R4)                                                  
*                                                                               
         LA    R3,48(R3)                                                        
         LA    R4,48(R4)                                                        
         BCT   R0,ROLLSM2                                                       
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
CLRSUM   LA    R0,13*6                                                          
         LR    R1,R2                                                            
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         SPACE 2                                                                
SVSUM    LA    R0,3                                                             
         L     R1,APRSUMPR                                                      
         MVC   0(208,R1),0(R2)                                                  
         LA    R1,208(R1)                                                       
         LA    R2,208(R2)                                                       
         BCT   0,*-14                                                           
         SH    R2,=H'624'          RESTORE REG POINTER                          
         BR    RE                                                               
*                                                                               
UNSVSUM  LA    R0,3                                                             
         L     R1,APRSUMPR                                                      
         MVC   0(208,R2),0(R1)                                                  
         LA    R1,208(R1)                                                       
         LA    R2,208(R2)                                                       
         BCT   R0,*-14                                                          
         SH    R2,=H'624'          RESTORE REG POINTER                          
         LA    R0,13*6                                                          
         L     R1,APRSUMPR                                                      
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
PUTBUFF  MVI   PRSUMSW,0                                                        
         LA    R1,=C'PUT'                                                       
         B     BUFFX                                                            
*                                                                               
HIGHBUFF DS    0H                                                               
         XC    PRKEY,PRKEY                                                      
         LA    R1,=C'HIGH'                                                      
         B     BUFFX                                                            
*                                                                               
SEQBUFF  LA    R1,=C'SEQ'                                                       
         B     BUFFX                                                            
*                                                                               
BUFFX    NTR1                                                                   
         ST    R1,DMCB             SET COMMAND ADDR                             
         GOTO1 BUFFALO,DMCB,,(PRSUMSW,BUFFBUFF),PRREC,1                         
         MVC   SVBFCB8,DMCB+8                                                   
*                                                                               
         CLC   =C'TRACE ',QUESTOR  TEST TO TRACE BUFFALO                        
         BNE   BUFFX2                                                           
         L     RF,BUFFBUFF                                                      
         L     R2,BUFFLALL-BUFFALOD(RF)                                         
         GOTO1 HEXOUT,DMCB,PRREC,P,(R2),=C'N'                                   
         GOTO1 REPORT                                                           
*                                                                               
BUFFX2   DS    0H                                                               
         TM    SVBFCB8,X'80'         SET CC ON EXIT                             
         XIT1                                                                   
*                                                                               
CLRBUFF  NTR1                                                                   
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(PRSUMSW,BUFFBUFF),(X'80',1)              
         B     EXIT                                                             
*                                                                               
TSTZERO  LH    R0,PRCOLS                                                        
         LR    R1,R2                                                            
TSTZER2  CP    0(8,R1),=P'0'                                                    
         BNER  RE                  EXIT WITH CC NOT EQ                          
         LA    R1,8(R1)                                                         
         BCT   R0,TSTZER2                                                       
         BR    RE                  EXIT WITH CC EQUAL                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'A2COMMON'                                                    
A2COMMON DS    0D                                                               
*                                                                               
RELO     DC    A(0)                                                             
PRGROSS  DC    F'0'                                                             
GUPGROSS DC    F'0'                                                             
GUPNET   DC    F'0'                                                             
GUPTAX   DC    F'0'                                                             
PSTART   DS    H                                                                
PEND     DS    H                                                                
PRMONCNT DC    PL2'0'                                                           
PRCNTR   DC    PL2'0'                                                           
PRSUMSW  DC    X'00'                                                            
PRBLLCUR DC    X'00'                                                            
PRID     DC    X'00'                                                            
PRQDATES DS    CL12                                                             
PRQBYID  DS    C                   SAVE AREA FOR REQ VALUE                      
PRTSUMSW DS    C                   'Y' IF PRODUCT SUMMARY WAS PRINTED           
PRCOLS   DS    H                   NUMBER OF ACTIVE BUFFALO COLUMNS             
SVBFSIZE DS    F                   SIZE OF BUFFALO CORE BUFFER                  
SVBFCB8  DS    X                                                                
HLDMGR   DS    XL2                                                              
MGRPSW   DC    C'N'                                                             
NXTMKTSV DS    A                                                                
SVDOLS   DS    PL8                                                              
SVTAX    DS    PL8                                                              
LMKT     DS    XL2                                                              
SVQEST   DS    CL6                 SAVE AREA FOR QEST/QESTEND                   
*                                                                               
AGYCAN   DS    CL1                 C=CANADIAN AGENCY                            
CLTCNT   DS    CL1                 CLIENT COUNTRY C/U                           
XCHIND   DS    XL1                                                              
DATASW   DS    XL1                 DATA SWITCH                                  
DSTAX    EQU   X'80'               TAX                                          
DSEXCH   EQU   X'40'               EXCHANGE DOLLARS                             
DSC58    EQU   X'20'               C58 RESERVE                                  
DSMSF    EQU   X'10'               MEDIA SERVICE FEE                            
CNDATASW DS    XL1                                                              
CURER1   DS    4A                                                               
*                                                                               
HDHKDATA DS    0F                  ADDRESS BLOCK FOR HEADHOOK INIT              
         DC    A(PRGROSS)                                                       
         DC    A(PRSUMSW)                                                       
         DC    A(PRMONCNT)                                                      
*                                                                               
APLG     DC    A(0)                ACTIVE MONTH LINE                            
APLT     DC    A(0)                ACTIVE  TAX  LINE                            
APLX     DC    A(0)                ACTIVE  EXCHANGE LINE                        
APLC     DC    A(0)                ACTIVE  C58 TAX LINE                         
APLM     DC    A(0)                ACTIVE  MEDIA SERVICE FEE LINE               
APLGT    DC    A(0)                ACTIVE  GROSS+TAX LINE                       
*                                                                               
APLG1    DC    A(0)                MONTH LINE 1 ADDRESS                         
APLG2    DC    A(0)                           2 ADDRESS                         
APLT1    DC    A(0)                  TAX LINE 1 ADDRESS                         
APLT2    DC    A(0)                           2 ADDRESS                         
APLX1    DC    A(0)                  EXCHANGE LINE 1 ADDRESS                    
APLX2    DC    A(0)                                2 ADDRESS                    
APLC1    DC    A(0)                  C58 TAX LINE 1 ADDRESS                     
APLC2    DC    A(0)                               2 ADDRESS                     
APLM1    DC    A(0)                  MEDIA SERVICE FEE LINE 1 ADDRESS           
APLM2    DC    A(0)                                         2 ADDRESS           
APLGT1   DC    A(0)                  GROSS+TAX LINE 1 ADDRESS                   
APLGT2   DC    A(0)                                 2 ADDRESS                   
APLEND   EQU   *                                                                
*                                                                               
APRBLPER DC    A(PRBLPER)                                                       
*                                                                               
         DS    0D                                                               
APRTOTST DC    A(PRTOTSTA)                                                      
APRTOTMK DC    A(PRTOTMKT)                                                      
APRTOTM3 DC    A(PRTOTMG3)                                                      
APRTOTM2 DC    A(PRTOTMG2)                                                      
APRTOTM1 DC    A(PRTOTMG1)                                                      
APRTOTPR DC    A(PRTOTPRD)                                                      
APRTOTP2 DC    A(PRTOTPG2)                                                      
APRTOTP1 DC    A(PRTOTPG1)                                                      
APRTOTCL DC    A(PRTOTCLT)                                                      
NTOTLEVS EQU   (*-APRTOTST)/4                                                   
*                                                                               
APRSUMPR DC    A(PRSUMPRD)                                                      
APRSUMP2 DC    A(PRSUMPG2)                                                      
APRSUMP1 DC    A(PRSUMPG1)                                                      
APRSUMCL DC    A(PRSUMCLT)                                                      
*                                                                               
TACT     DS    CL1                 ACTION                                       
TMKST    DS    XL5                 BINARY MKT/STA                               
TMKT     DS    CL4                 MARKET NUMBER                                
TSTA     DS    CL5                 STATION                                      
TNET     DS    CL3                 NETWORK                                      
*                                                                               
*                                                                               
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
* BUFFALO RECORD                                                                
*                                                                               
         DS    0D                                                               
PRREC    DS    0C                                                               
*                                                                               
PRKEY    DS    0XL12                                                            
PRKTYP   DS    CL1                 TYPE                                         
*                                  X'11' = MGR1TOT WITHIN PRD                   
*                                  X'12' = MGR2TOT WITHIN MGR1                  
*                                  X'13' = MGR3TOT WITHIN MGR2                  
*                                  X'20' = PRD TOT WITHIN CLT                   
*                                  X'21' = PRD TOT WITHIN PGR1                  
*                                  X'22' = PRD TOT WITHIN PGR2                  
*                                  X'31' = PGR1TOT WITHIN CLT                   
*                                  X'32' = PGR2TOT WITHIN PGR1                  
PRKPGR   DS    CL5                                                              
PRKPRD   DS    CL3                                                              
*                                                                               
         ORG   PRKPGR                                                           
PRKMGR   DS    CL5                                                              
*                                                                               
         ORG   PRKPGR                                                           
PRKREP   DS    CL2                                                              
PRKMKT   DS    CL2                                                              
PRKSTA   DS    CL3                                                              
*                                                                               
         ORG   PRKPGR                                                           
PRKNTWK  DS    CL4                                                              
*                                                                               
         ORG   PRKEY+12                                                         
PRCODE   DS    CL8                                                              
PRNAME   DS    CL24                                                             
PRDATA   DS    (LDATA)X                                                         
         SPACE 2                                                                
NVALS    EQU   5                   GROSS/TAX/EXCHANGE/C58/MSF                   
NACCUMS  EQU   1+(NVALS*13)        SPOTS+(N'VALUES*N'MONTHS)                    
LDATA    EQU   8*NACCUMS                                                        
         EJECT                                                                  
CNNETTAB DS    127XL4                                                           
CNNETTBX EQU   *                                                                
CNNETTBN EQU   (CNNETTBX-CNNETTAB)/4                                            
*                                                                               
* DETAIL ACCUMULATORS -                                                         
*                                                                               
PRTOTSTA DS    (LDATA)C             SPOTS/MON1/MON2/.../MON12/MON13             
PRTOTMKT DS    (LDATA)C                                                         
PRTOTMG3 DS    (LDATA)C                                                         
PRTOTMG2 DS    (LDATA)C                                                         
PRTOTMG1 DS    (LDATA)C                                                         
PRTOTPRD DS    (LDATA)C                                                         
PRTOTPG2 DS    (LDATA)C                                                         
PRTOTPG1 DS    (LDATA)C                                                         
PRTOTCLT DS    (LDATA)C                                                         
*                                                                               
         SPACE 2                                                                
* SUMMARY ACCUMULATORS                                                          
*                                                                               
PRSUMPRD DS    13XL48              ORD/AU/PAY/PAYTD/BLL/BLLTD BY MON            
PRSUMPG2 DS    13XL48                                                           
PRSUMPG1 DS    13XL48                                                           
PRSUMCLT DS    13XL48                                                           
*                                                                               
PRBLPER  DS    CL304                                                            
*                                                                               
SVPRDATA DS    XL216               27 * 8 BYTES                                 
         SPACE 2                                                                
MKTSVD   DSECT                     MARKET SAVE AREA                             
MKTSVMKT DS    XL2                 MARKET                                       
MKTSVMKN DS    CL24                MKT NMA NAME                                 
MKTSVL   EQU   *-MKTSVD                                                         
         SPACE 2                                                                
SPA202   CSECT                                                                  
MKTSVTAB DS    0X                  MKT DATA SAVE TABLE FOR ID RUNS              
         ORG   *+MKTSVL*100        100 MARKETS PER ID                           
         DS    X                   ONE EXTRA                                    
MKTSVTBX EQU   *                                                                
         EJECT                                                                  
*                                                                               
* INITIALIZE STORAGE FOR HEADHOOK ROUTINE                                       
*                                                                               
HDHKINIT NMOD1 0,**HDHK**                                                       
         L     RE,0(R1)                                                         
         ST    RE,AMEDBUFF                                                      
         L     RE,4(R1)                                                         
         ST    RE,ASPWORK                                                       
         L     RE,8(R1)                                                         
         MVC   HDHKADDR(HDHKADRL),0(RE)                                         
         ST    RB,HDHKRB                                                        
*                                                                               
HDHKXIT  XIT1  ,                                                                
         EJECT                                                                  
* HEADHOOK                                                                      
*                                                                               
         DROP  RB                                                               
         CNOP  0,4                                                              
         USING *,RF                                                             
PRHDHK   NTR1                                                                   
         L     RB,HDHKRB                                                        
         B     HDHK10                                                           
HDHKRB   DC    F'0'                                                             
         DROP  RF                                                               
         USING HDHKINIT,RB                                                      
*                                                                               
HDHK10   L     R7,AMEDBUFF                                                      
         USING MEDBLOCK,R7                                                      
*                                                                               
         L     RA,ASPWORK                                                       
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         L     R3,=A(DICSECT)                                                   
         USING DICSECT,R3                                                       
*                                                                               
         CLI   HEADHOOK,C'R'       TEST ADDITIONAL HEADLINES                    
         BNE   HDHK20                                                           
         MVI   HEADHOOK,0                                                       
         B     HDHKEXT                                                          
*                                                                               
HDHK20   MVC   H5+113(1),QPROG+1   CORRECT PROGRAM CODE IN REPORT               
*                                                                               
         L     R1,APRGROSS                                                      
         MVC   HHGROSS,0(R1)                                                    
         L     R1,APRSUMSW                                                      
         MVC   HHSUMSW,0(R1)                                                    
         L     R1,APRMNCNT                                                      
         MVC   HHMONCNT,0(R1)                                                   
*                                                                               
         CLI   RCSUBPRG,100        TEST ERROR MSG TO PRINT                      
         BE    HDHKXIT                                                          
*                                                                               
         CLC   =C'PRGTYP=',QUESTOR                                              
         BNE   HDHK30                                                           
         MVC   H2+23(L'SP@FLACT),SP@FLACT                                       
*                                                                               
HDHK30   CLI   Q2COL32,C' '        TEST RATE TYPE FILTER                        
         BNH   *+16                                                             
         MVC   H2+23(16),=C'RATE TYPE X ONLY'                                   
         MVC   H2+33(1),Q2COL32                                                 
*                                                                               
         CLC   =C'SIZE=',QUESTOR                                                
         BNE   *+10                                                             
         MVC   H2+23(L'SP@FLACT),SP@FLACT                                       
*                                                                               
         CLI   QSTA+4,C'-'                                                      
         BNE   HDHK40                                                           
         MVC   H2+23(18),=C'* CABLE EXCLUDED *'                                 
         B     HDHK50                                                           
*                                                                               
HDHK40   CLI   QSTA+4,C'/'                                                      
         BNE   HDHK50                                                           
         MVC   H2+23(14),=C'* CABLE ONLY *'                                     
         CLI   QCBLNET,C'A'                                                     
         BL    HDHK50                                                           
         MVC   H2+23(22),=C'* CABLE NET XXX ONLY *'                             
         MVC   H2+35(3),QCBLNET                                                 
*                                                                               
HDHK50   CLI   QCOST2,C'Y'         TEST SHOWING COST2                           
         BNE   HDHK52                                                           
         MVC   MID1(3),=C'***'                                                  
         MVC   MID1+4(L'SP@DLAMT),SP@DLAMT                                      
         MVC   MID1+23(18),=C'COST TO CLIENT ***'                               
*                                                                               
HDHK52   SR    R0,R0                                                            
         ICM   R0,7,HHGROSS+1                                                   
         BZ    HDHK80                                                           
         CLI   HHGROSS,2                                                        
         BNE   HDHK60                                                           
         CURED (R0),(6,MID1+23),CURTAB,ALIGN=LEFT                               
         B     HDHK70                                                           
*                                                                               
HDHK60   CLI   HHGROSS,4                                                        
         BNE   HDHK100                                                          
         MVI   CURTAB+3,4                                                       
         CURED (R0),(8,MID1+24),CURTAB,ALIGN=LEFT                               
         MVI   CURTAB+3,2                                                       
*                                                                               
HDHK70   LA    RE,MID1+24                                                       
         AR    RE,R0                                                            
*--->    MVC   1(27,RE),=C'PERCENT OF ACTUAL GROSS ****'                        
         MVC   1(L'SP@POAG,RE),SP@POAG                                          
         MVC   26(3,RE),=C'***'                                                 
*--->    MVC   MID1(22),=C'*** DOLLAR AMOUNTS ARE'                              
         MVC   MID1+4(L'SP@DLAMT),SP@DLAMT                                      
         MVC   MID1(3),=C'***'                                                  
         B     HDHK100                                                          
*                                                                               
HDHK80   CLI   RQGETBF,C'Y'                                                     
         BNE   HDHK82                                                           
         OC    SVBFORM,SVBFORM     UNLESS NO FORMULAS APPLY                     
         BZ    HDHK100                                                          
*                                                                               
*--->    MVC   MID1(37),=C'*** DOLLAR AMOUNTS HAVE BEEN ADJUSTED'               
*--->    MVC   MID1+37(20),=C' BY BILL FORMULA ***'                             
         MVC   MID1(3),DASHES                                                   
         MVC   MID1+4(L'SP@DOL01),SP@DOL01                                      
         LA    R5,MID1+4+L'SP@DOL01-1                                           
         CLI   0(R5),C' '                                                       
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,2(R5)                                                         
*                                                                               
         OC    SVBFORM,SVBFORM                                                  
         BZ    HDHK100                                                          
         CLI   MODE,CLTLAST                                                     
         BNL   HDHK100                                                          
*                                                                               
         MVI   0(R5),C'G'             *** MID1+54                               
         TM    SVBFORM,X'10'                                                    
         BZ    *+8                                                              
         MVI   0(R5),C'N'             *** MID1+54                               
         MVI   1(R5),C'+'              *** MID1+55                              
         ICM   RE,15,SVBFORM+1                                                  
         LTR   RE,RE                                                            
         BNM   *+10                                                             
         MVI   1(R5),C'-'              *** MID1+55                              
         LPR   RE,RE                                                            
*                                                                               
         LA    R6,2(R5)                                                         
         LR    R0,RE                                                            
         MVI   CURTAB+3,4                                                       
         CURED (R0),(8,(R6)),CURTAB,ALIGN=LEFT                                  
         MVI   CURTAB+3,2                                                       
         AR    R6,R0                                                            
         MVC   0(6,R6),=C'%G ---'                                               
         TM    SVBFORM,X'01'                                                    
         BZ    *+8                                                              
         MVI   1(R6),C'N'                                                       
         MVC   2(3,R6),DASHES                                                   
         B     HDHK100                                                          
*                                                                               
HDHK82   CLI   RQGETBF,C'X'        TEST REPORTING NET DOLLARS                   
         BNE   HDHK100                                                          
         MVC   MID1(30),=C'*** DOLLAR AMOUNTS ARE NET ***'                      
*                                                                               
HDHK100  DS    0H                                                               
         LA    RF,SP@GSTI          INPUT GST INCLUDED ...                       
         CLI   RQGSTOPT,C'Y'       TEST DOING INPUT GST                         
         BE    HDHK110                                                          
         LA    RF,SP@GSTO           OUTPUT GST INCLUDED ...                     
         CLI   QGST,C'O'                                                        
         BNE   HDHK120                                                          
         L     RE,ADCLT                                                         
         CLI   CEXTRA+11-CLTHDRD(RE),C'X'                                       
         BE    HDHK120                                                          
         CLI   CEXTRA+11-CLTHDRD(RE),C'Z'                                       
         BE    HDHK120                                                          
         CLI   MODE,PRDLAST                                                     
         BH    HDHK110                                                          
         L     RE,ADPRD                                                         
         CLI   PGSTCODE-PRDHDRD(RE),C'X'                                        
         BE    HDHK120                                                          
         CLI   PGSTCODE-PRDHDRD(RE),C'Z'                                        
         BE    HDHK120                                                          
*                                                                               
HDHK110  LA    R1,MID1                                                          
         CLC   MID1,SPACES                                                      
         BNH   *+8                                                              
         LA    R1,MID2                                                          
         MVC   0(50,R1),0(RF)       MOVE STUPID GST MESSAGE                     
*                                                                               
HDHK120  CLI   RCSUBPRG,2          TEST PAID SUMMARY                            
         BNE   HDHK130                                                          
         CLC   QAREA+49(12),SPACES TEST PAY DATES ENTERED                       
         BE    HDHK130             NO                                           
*--->    MVC   H4+45(40),=C'** PAYMENTS FROM JAN01/76 TO DEC31/76 **'           
         MVC   H4+45(L'SP@PAY01),SP@PAY01                                       
         GOTO1 DATCON,DMCB,QAREA+49,(5,H4+62)                                   
         GOTO1 (RF),(R1),QAREA+55,(5,H4+74)                                     
*                                                                               
HDHK130  CLI   AGYCAN,C'C'         TEST CANADIAN AGENCY                         
         BNE   HDHK131                                                          
         CLI   QCRRNCY,C' '        TEST CURRENCY SPECIFIED                      
         BE    HDHK131             NO - DON'T PRINT ANYTHING                    
*--->    MVC   H6+99(20),=CL20'* CANADIAN DOLLARS *'                            
         MVC   H6+99(L'SP@CANDL),SP@CANDL                                       
         CLI   QCRRNCY,C'C'                                                     
         BE    HDHK131                                                          
*--->    MVC   H6+99(20),=CL20'*** U.S. DOLLARS ***'                            
         MVC   H6+99(L'SP@USDOL),SP@USDOL                                       
*                                                                               
HDHK131  MVI   BYTE,C'N'                                                        
         CLI   PRSUMSW,0           TEST PRINTING RECAP                          
         BNE   HDHK190             NO                                           
         L     RE,ADCLT                                                         
         USING CLTHDRD,RE                                                       
         L     RF,ADPRD            PRD RECORD                                   
         USING PRDHDRD,RF                                                       
         LA    R2,HEAD1                                                         
         LA    R1,14               FIND FIRST EMPTY LINE                        
*                                                                               
HDHK135  CLC   0(L'HEAD1,R2),SPACES                                             
         BNH   HDHK137                                                          
         LA    R2,L'HEAD1(R2)      BUMP TO NEXT LINE                            
         BCT   R1,HDHK135                                                       
*                                                                               
HDHK137  LA    R2,L'HEAD1(R2)      SKIP A LINE                                  
         LR    R1,R2                                                            
         CLC   CPU1,SPACES         IS THERE A USER DEF DESC                     
         BNH   HDHK140                                                          
         TM    CPU1FLG1,CFLGA2Q    SHOW ON A2                                   
         BNO   HDHK140                                                          
         CLI   PROGPROF+15,C'Y'    IF PROF SET TO SHOW UDEF REGARDLESS          
         BE    HDHK137A                                                         
         CLC   PUSER1,SPACES       IS THERE A USER DEF                          
         BNH   HDHK140                                                          
*                                                                               
HDHK137A MVC   0(L'CPU1,R1),CPU1                                                
         LA    R1,L'CPU1-1(R1)                                                  
*                                                                               
HDHK138  CLI   0(R1),C' '          BACK UP TO LAST CHARACTER                    
         BH    HDHK139                                                          
         BCT   R1,HDHK138                                                       
*                                                                               
HDHK139  MVC   1(3,R1),=C':  '                                                  
         MVC   4(L'PUSER1,R1),PUSER1                                            
         LA    R1,70(R2)                                                        
         MVI   BYTE,C'Y'                                                        
*                                                                               
HDHK140  CLC   CPU2,SPACES         IS THERE A USER DEF DESC                     
         BNH   HDHK150                                                          
         TM    CPU2FLG1,CFLGA2Q    SHOW ON A2                                   
         BNO   HDHK150                                                          
         CLI   PROGPROF+15,C'Y'    IF PROF SET TO SHOW UDEF REGARDLESS          
         BE    HDHK140A                                                         
         CLC   PUSER2,SPACES       IS THERE A USER DEF                          
         BNH   HDHK150                                                          
*                                                                               
HDHK140A MVC   0(L'CPU2,R1),CPU2                                                
         LA    R1,L'CPU2-1(R1)                                                  
*                                                                               
HDHK141  CLI   0(R1),C' '          BACK UP TO LAST CHARACTER                    
         BH    HDHK142                                                          
         BCT   R1,HDHK141                                                       
*                                                                               
HDHK142  MVC   1(3,R1),=C':  '                                                  
         MVC   4(L'PUSER2,R1),PUSER2                                            
         MVI   BYTE,C'Y'                                                        
*                                                                               
HDHK150  L     RF,ADEST            EST RECORD                                   
         USING ESTHDRD,RF                                                       
         CLI   BYTE,C'Y'                                                        
         BNE   *+8                                                              
         LA    R2,L'HEAD1(R2)                                                   
         LR    R1,R2                                                            
         CLC   =C'NO',QEST         IF QEST = NO                                 
         BE    HDHK190                                                          
         CLI   QEST+3,C' '         OR IF A RANGE OF ESTIMATES                   
         BH    HDHK190                DON'T PRINT USER DEF                      
         CLC   CEU1,SPACES         IS THERE A USER DEF DESC                     
         BNH   HDHK160                                                          
         TM    CEU1FLG1,CFLGA2Q    SHOW ON A2                                   
         BNO   HDHK160                                                          
         CLI   PROGPROF+15,C'Y'    IF PROF SET TO SHOW UDEF REGARDLESS          
         BE    HDHK152                                                          
         CLC   EUSER1,SPACES       IS THERE A USER DEF                          
         BNH   HDHK160                                                          
*                                                                               
HDHK152  MVC   0(L'CEU1,R1),CEU1                                                
         LA    R1,L'CEU1-1(R1)                                                  
*                                                                               
HDHK155  CLI   0(R1),C' '          BACK UP TO LAST CHARACTER                    
         BH    HDHK157                                                          
         BCT   R1,HDHK155                                                       
*                                                                               
HDHK157  MVC   1(3,R1),=C':  '                                                  
         MVC   4(L'EUSER1,R1),EUSER1                                            
         MVI   BYTE,C'Y'                                                        
         LA    R1,70(R2)                                                        
         MVI   BYTE,C'Y'                                                        
*                                                                               
HDHK160  CLC   CEU2,SPACES         IS THERE A USER DEF DESC                     
         BNH   HDHK190                                                          
         TM    CEU2FLG1,CFLGA2Q    SHOW ON A2                                   
         BNO   HDHK190                                                          
         CLI   PROGPROF+15,C'Y'    IF PROF SET TO SHOW UDEF REGARDLESS          
         BE    HDHK165                                                          
         CLC   EUSER1,SPACES       IS THERE A USER DEF                          
         BNH   HDHK190                                                          
*                                                                               
HDHK165  MVC   0(L'CEU2,R1),CEU2                                                
         LA    R1,L'CEU2-1(R1)                                                  
*                                                                               
HDHK170  CLI   0(R1),C' '          BACK UP TO LAST CHARACTER                    
         BH    HDHK180                                                          
         BCT   R1,HDHK170                                                       
*                                                                               
HDHK180  MVC   1(3,R1),=C':  '                                                  
         MVC   4(L'EUSER2,R1),EUSER2                                            
*                                                                               
HDHK190  DS    0H                                                               
         OC    CPWPCT,CPWPCT       TEST PW CLIENT                               
         BZ    HDHKX                                                            
         L     RF,ADEST                                                         
         USING ESTHDRD,RF                                                       
         OC    EPWPCT,EPWPCT       TEST PW CLIENT                               
         BZ    HDHKX               NO                                           
*                                                                               
         LA    RE,H1+56                                                         
         CLI   0(RE),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,*-8              BACK UP TO A SPACE                           
         SH    RE,=H'3'            BACK UP 3 MORE                               
         MVC   0(3,RE),=C' IM'                                                  
         CLI   QPWCV,C'Y'          TEST PW CLIENT VERSION                       
         BNE   *+10                                                             
         MVC   0(3,RE),=C'CLT'                                                  
         MVC   132(4,RE),=C'----'   MAKE --'S CONTINUOUS                        
         GOTO1 CENTER,DMCB,H1+48,36                                             
         GOTO1 (RF),(R1),H2+48,36                                               
*                                                                               
HDHKX    MVI   HEADHOOK,C'R'       SET ADDITIONAL HEADLINES                     
         B     HDHKXIT                                                          
         DROP  RF                                                               
*                                                                               
*        ADDITIONAL HEADLINES                                                   
*                                                                               
HDHKEXT  DS    0H                                                               
         LA    R1,14               CLEAR HEADLINES                              
         LA    R2,HEAD1                                                         
*                                                                               
HDEXT10  MVC   0(L'HEAD1,R2),SPACES                                             
         LA    R2,L'HEAD1(R2)      BUMP TO NEXT LINE                            
         BCT   R1,HDEXT10                                                       
*                                                                               
         CLI   FCRDBUYS,C'I'                                                    
         BE    *+10                                                             
*--->    MVC   H10(6),=C'MARKET'                                                
         MVC   H2(L'SP@MRKT),SP@MRKT                                            
         CLI   HHSUMSW,C'R'                                                     
         BE    HDEXT20                                                          
         CLI   HHSUMSW,0           TEST PRINTING RECAP                          
         BE    HDEXT20             NO                                           
*--->    MVC   H7+46(40),=C'** XXXXXXXXXXXX RECAP BY ZZZZZZZZZZZZ **'           
         MVC   H1+46(L'SP@RECAP),SP@RECAP                                       
*                                                                               
         CLI   HHSUMSW,X'11'                                                    
         BNE   *+16                                                             
*--->    MVC   H7+49(12),=CL12'PRODUCT'                                         
         MVC   H1+49(L'SP@PRO),SP@PRO                                           
         MVC   H1+71(12),MGR1BK                                                 
         CLI   HHSUMSW,X'12'                                                    
         BNE   *+16                                                             
         MVC   H1+49(12),MGR1BK                                                 
         MVC   H1+71(12),MGR2BK                                                 
         CLI   HHSUMSW,X'13'                                                    
         BNE   *+16                                                             
         MVC   H1+49(12),MGR2BK                                                 
         MVC   H1+71(12),MGR3BK                                                 
*                                                                               
         CLI   HHSUMSW,X'20'                                                    
         BNE   *+16                                                             
*--->    MVC   H7+49(12),=CL12'CLIENT'                                          
         MVC   H1+49(L'SP@CLI),SP@CLI                                           
*--->    MVC   H7+71(12),=CL12'PRODUCT'                                         
         MVC   H1+71(L'SP@PRO),SP@PRO                                           
         CLI   HHSUMSW,X'21'                                                    
         BNE   *+16                                                             
         MVC   H1+49(12),PGR1BK                                                 
*--->    MVC   H7+71(12),=CL12'PRODUCT'                                         
         MVC   H1+71(L'SP@PRO),SP@PRO                                           
         CLI   HHSUMSW,X'22'                                                    
         BNE   *+16                                                             
         MVC   H1+49(12),PGR2BK                                                 
*--->    MVC   H7+71(12),=CL12'PRODUCT'                                         
         MVC   H1+71(L'SP@PRO),SP@PRO                                           
*                                                                               
         CLI   HHSUMSW,X'31'                                                    
         BNE   *+16                                                             
*--->    MVC   H7+49(12),=CL12'CLIENT'                                          
         MVC   H1+49(L'SP@CLI),SP@CLI                                           
         MVC   H1+71(12),PGR1BK                                                 
         CLI   HHSUMSW,X'32'                                                    
         BNE   *+16                                                             
         MVC   H1+49(12),PGR1BK                                                 
         MVC   H1+71(12),PGR2BK                                                 
*                                                                               
         CLI   HHSUMSW,C'N'                                                     
         BNE   *+16                                                             
*--->    MVC   H7+49(12),=CL12'PRODUCT'                                         
         MVC   H1+49(L'SP@PRO),SP@PRO                                           
*--->    MVC   H7+71(12),=CL12'NETWORK'                                         
         MVC   H1+71(L'SP@NTWRK),SP@NTWRK                                       
* REPEAT NAME AS COLHDG                                                         
         CLI   HHSUMSW,C'N'                                                     
         BE    *+10                                                             
         MVC   H2(12),H1+71                                                     
*                                                                               
         GOTO1 SQUASHER,DMCB,H1+46,40                                           
         GOTO1 CENTER,DMCB,H1+46,40                                             
*                                                                               
HDEXT20  GOTO1 UNDERLIN,DMCB,(12,H2),H3                                         
*                                                                               
         CLI   HHSUMSW,C'N'                                                     
         BNE   *+16                                                             
*--->    MVC   H10+26(7),=C'NETWORK'                                            
         MVC   H2+26(L'SP@NTWRK),SP@NTWRK                                       
         MVC   H3+26(7),DASHES                                                  
         CLI   HHSUMSW,C'R'        TEST REP SUMMARY                             
         BE    *+12                                                             
         CLI   HHSUMSW,0                                                        
         BNE   HDEXT30                                                          
*--->    MVC   H10+25(07),=C'STATION'                                           
         MVC   H2+25(L'SP@STATN),SP@STATN                                       
         MVC   H3+25(7),DASHES                                                  
         CLI   USRSW1,C'Y'         TEST MKT NM PRINTED YET                      
         BNE   HDEXT30             NO                                           
         CLI   MODE,STALAST                                                     
         BNE   HDEXT30                                                          
         MVC   P(24),SPACES                                                     
         MVC   P(14),MKTNM                                                      
*--->    MVC   P+16(8),=C'(CONT''D)'                                            
         MVC   P+15(L'SP@CONTI),SP@CONTI                                        
*                                                                               
HDEXT30  DS    0H                                                               
         CLI   Q2COL22,C'N'        TEST PRINT SPOTS                             
         BE    HDEXT40             NO                                           
         CLI   PRSUMSW,C'N'        TEST CAN NTWK SUMMARY                        
         BE    HDEXT40                                                          
*--->    MVC   H10+35(5),=C'SPOTS'                                              
         MVC   H2+35(L'SP5SPOTS),SP5SPOTS                                       
         MVC   H3+35(5),DASHES                                                  
HDEXT40  CLI   HHSUMSW,C'R'                                                     
         BNE   HDEXT50                                                          
*--->    MVC   H7+99(5),=C'* REP'                                               
         MVC   H1+99(L'SP@REP),SP@REP                                           
         L     R6,ADREP                                                         
         USING REPRECD,R6                                                       
         MVC   H1+105(3),2(R6)                                                  
         MVI   H1+108,C'*'                                                      
         MVC   H1+110(20),RNAME                                                 
*                                                                               
HDEXT50  DS    0H                                                               
* FORMAT DATES                                                                  
*                                                                               
HDEXT60  ZAP   HALF,=P'7'                                                       
         CP    HHMONCNT,=P'13'                                                  
         BE    HDEXT70                                                          
         ZAP   HALF,=P'6'                                                       
         CP    HALF,HHMONCNT                                                    
         BNH   *+10                                                             
         ZAP   HALF,HHMONCNT                                                    
*                                                                               
HDEXT70  LA    R4,H2+41                                                         
         LA    R5,MEDMON01                                                      
*                                                                               
HDEXT80  CLI   SPOTPROF+2,0        TEST NORMAL MONTHS                           
         BNE   HDEXT90                                                          
* FORMAT MMM/YY                                                                 
         GOTO1 DATCON,DMCB,(2,2(R5)),(6,3(R4))                                  
         MVC   135(6,R4),DASHES                                                 
         B     HDEXT100                                                         
* FORMAT MMMDD-MMMDD                                                            
HDEXT90  GOTO1 DATCON,DMCB,(2,0(R5)),(7,0(R4))                                  
         MVI   5(R4),C'-'                                                       
         GOTO1 (RF),(R1),(2,2(R5)),(7,6(R4))                                    
         MVC   132(11,R4),DASHES                                                
*                                                                               
HDEXT100 LA    R4,13(R4)                                                        
         LA    R5,12(R5)                                                        
         SP    HALF,=P'1'                                                       
         BP    HDEXT80                                                          
*                                                                               
         ZAP   HALF,HHMONCNT                                                    
         SP    HALF,=P'6'                                                       
         BP    HDEXT110                                                         
         CLI   Q2COL31,C'Y'        TEST SUPPRESS TOTAL COLUMN                   
         BE    HDEXT150                                                         
*--->    MVC   H10+122(5),=C'TOTAL'                                             
         MVC   H2+122(L'SP5TOTAL),SP5TOTAL                                      
         MVC   H3+122(5),DASHES                                                 
         B     HDEXT150                                                         
*                                                                               
HDEXT110 CP    HHMONCNT,=P'13'                                                  
         BNE   *+10                                                             
         ZAP   HALF,=P'6'                                                       
*                                                                               
         LA    R4,H3+41                                                         
*                                                                               
HDEXT120 CLI   SPOTPROF+2,0                                                     
         BNE   HDEXT130                                                         
         GOTO1 DATCON,DMCB,(2,2(R5)),(6,3(R4))                                  
         B     HDEXT140                                                         
HDEXT130 GOTO1 DATCON,DMCB,(2,0(R5)),(7,0(R4))                                  
         MVI   5(R4),C'-'                                                       
         GOTO1 (RF),(R1),(2,2(R5)),(7,6(R4))                                    
*                                                                               
HDEXT140 LA    R4,13(R4)                                                        
         LA    R5,12(R5)                                                        
         SP    HALF,=P'1'                                                       
         BP    HDEXT120                                                         
         CLI   Q2COL31,C'Y'        TEST SUPPRESS TOTAL COLUMN                   
         BE    HDEXT150                                                         
*--->    MVC   H11+122(5),=C'TOTAL'                                             
         MVC   H3+122(L'SP5TOTAL),SP5TOTAL                                      
         DROP  R3                                                               
*                                                                               
HDEXT150 DS    0H                                                               
*                                                                               
HDHKEXX  MVI   HEADHOOK,0                                                       
         B     HDHKXIT                                                          
         EJECT                                                                  
* HEADHOOK ROUTINE STORAGE                                                      
*                                                                               
DASHES   DC    12C'-'                                                           
*                                                                               
AMEDBUFF DS    A                                                                
ASPWORK  DS    A                                                                
*                                                                               
HDHKADDR DS    0F                                                               
APRGROSS DS    A                                                                
APRSUMSW DS    A                                                                
APRMNCNT DS    A                                                                
HDHKADRL EQU   *-HDHKADDR                                                       
*                                                                               
HHGROSS  DS    F                                                                
HHSUMSW  DS    X                                                                
HHMONCNT DS    PL2                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*=======================================================*                       
* PROCESS NEW BILLING RECORD                            *                       
*=======================================================*                       
         SPACE 1                                                                
         DROP  RB,RC,R8                                                         
*                                                                               
PRBILL   NMOD1 0,**PRBL**                                                       
         L     R8,=A(A2COMMON)                                                  
         USING A2COMMON,R8                                                      
*                                                                               
         CLI   QCOST2,C'Y'         TEST COS2 REQUEST                            
         BNE   PRB1                                                             
         CLI   KEY+12,1            TEST 2ND CURRENCY BILL                       
         BNE   PRBX                                                             
*                                                                               
PRB1     L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         USING STABUCKD,R6                                                      
         GOTO1 GET                                                              
         SPACE 1                                                                
*======================================================*                        
* SEARCH FOR ELEMENTS IN REQUEST PERIOD                *                        
*======================================================*                        
         SPACE 1                                                                
         LA    R6,24(R6)                                                        
PRB2     CLI   0(R6),0                                                          
         BE    PRBX                                                             
         CLI   0(R6),X'0E'                                                      
         BE    PRB6                                                             
*                                                                               
PRB4     ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     PRB2                                                             
*                                                                               
PRB6     L     R4,APRBLPER                                                      
*                                                                               
PRB8     CLC   0(2,R4),2(R6)       MATCH PERIOD NUMBERS                         
         BE    PRB10                                                            
         LA    R4,6(R4)                                                         
         CLI   0(R4),0                                                          
         BE    PRB4                                                             
         B     PRB8                                                             
         SPACE 1                                                                
*====================================================*                          
* FIND ACCUMULATOR POSITION                          *                          
*====================================================*                          
         SPACE 1                                                                
PRB10    MVC   HALF,2(R4)          SAVE PERIOD START DATE                       
         LA    R4,MEDMON01                                                      
         L     R5,MEDNUMMO                                                      
         LA    R3,PRDATA+8                                                      
*                                                                               
PRB12    OC    0(2,R4),0(R4)                                                    
         BZ    *+14                                                             
         CLC   0(2,R4),HALF        MATCH DATES                                  
         BE    PRB14                                                            
         LA    R4,12(R4)                                                        
         LA    R3,8(R3)                                                         
         BCT   R5,PRB12                                                         
         B     PRB4                                                             
*                                                                               
PRB14    DS    0H                                                               
         CLI   STASW,C'Y'                                                       
         BE    PRB16                                                            
*                                                                               
         MVI   STASW,C'Y'                                                       
         LH    R0,PRCOLS           CLEAR BUFFALO DATA                           
         LA    R1,PRDATA                                                        
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         XC    PRKEY,PRKEY                                                      
         MVI   PRKTYP,C'S'                                                      
         L     RE,ADBUY                                                         
         MVC   PRKMKT(5),7(RE)     MKT/STA (NOTE-MKT IS MGR IF                  
*                                           READING ID POINTERS)                
         GOTO1 MSUNPK,DMCB,PRKMKT,WORK,STA                                      
         MVC   PRNAME(4),STA                                                    
         MVC   PRNAME+4(3),=C'- M'                                              
         MVC   PRNAME+5(1),STA+4                                                
         CLI   STA+4,C'A'                                                       
         BE    PRB16                                                            
         CLI   STA+4,C'F'                                                       
         BE    PRB16                                                            
         MVC   PRNAME+5(2),=C'TV'                                               
         CLI   QMED,C'T'                                                        
         BE    PRB16                                                            
         MVC   PRNAME+4(5),SPACES                                               
         EJECT                                                                  
         USING STABELEM,R6                                                      
PRB16    DS    0H                                                               
         XC    GUPGROSS(12),GUPGROSS                                            
         XC    SVBFORM,SVBFORM                                                  
         CLI   RQGETBF,C'Y'        TEST BILL FORMULA ADJUST                     
         BNE   PRB17               YES, USE GETBF, BUT ONLY TO GET              
         MVI   MODE,PROCBUY        FORMULA, NOT AMOUNT                          
         GOTO1 GETBF,DMCB,(BPRD,GUPGROSS),SVBFORM                               
         MVI   MODE,ESTFRST                                                     
*                                                                               
PRB17    DS    0H                                                               
         GOTO1 SPBVAL,DMCB,(C'E',STABELEM),SPBVALD,SVBFORM                      
*                                                                               
         MVC   GUPGROSS,SPBVACT    ACTUAL                                       
         CLI   RQGETBF,C'Y'                                                     
         BE    *+10                                                             
         MVC   GUPGROSS,SPBVEGRS   OR EFFECTIVE GROSS                           
         MVC   GUPNET,SPBVENET     AND NET                                      
         MVC   GUPTAX,SPBVETAX     AND TAX                                      
*                                                                               
         OC    PRGROSS+1(3),PRGROSS+1   TEST GROSS-UP REQUEST                   
         BZ    *+8                                                              
         BAS   RE,PRBGUP                                                        
*                                                                               
PRB18    L     R1,GUPGROSS                                                      
         LH    R0,STABSPTS                                                      
*                                                                               
         CLI   RCSUBPRG,4          TEST BILLED SUMMARY                          
         BE    *+8                                                              
         LCR   R0,R0                                                            
         LCR   R1,R1                                                            
*                                                                               
         CVD   R0,DUB                                                           
         AP    PRDATA(8),DUB       ADD TO TOTAL SPOTS                           
*                                                                               
PRB20    CVD   R1,DUB                                                           
         ZAP   SVDOLS,DUB          SAVE DOLLARS                                 
         L     R0,GUPTAX           GET TAX DOLLARS                              
         CLI   RCSUBPRG,4          TEST BILLED SUMMARY                          
         BE    *+6                                                              
         LCR   R0,R0                                                            
         CVD   R0,DUB                                                           
         ZAP   SVTAX,DUB                                                        
         AP    0(8,R3),SVDOLS      ADD TO MONTHLY DOLLARS                       
         TM    DATASW,DSTAX        TEST REPORTING TAX SEPARATELY                
         BZ    PRB22               NO                                           
         AP    104(8,R3),SVTAX     ADD TO TAX DOLLARS                           
         CLI   PROGPROF+4,C'X'     TEST EXCLUDE TAX FROM GROSS                  
         BNE   *+10                NO                                           
         SP    0(8,R3),SVTAX       YES - GET IT OUT OF THERE                    
PRB22    B     PRB4                                                             
*                                                                               
PRBX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*=======================================================*                       
* SUBROUTINE GROSSES UP DOLLARS BY SPECIFIED PERCENTAGE *                       
* AFTER SUBTRACTING TAX DOLLARS                         *                       
* INPUT IS GUPGROSS/GUPTAX. OUTPUT IS GUPGROSS          *                       
*=======================================================*                       
         SPACE 1                                                                
PRBGUP   NTR1                                                                   
         L     R0,GUPGROSS                                                      
         S     R0,GUPTAX                                                        
         SR    R1,R1                                                            
         ICM   R1,7,PRGROSS+1      GET ADJUSTMENT PERCENTAGE                    
         MR    R0,R0                                                            
         SLDA  R0,1                                                             
*                                                                               
         LH    RE,=H'100'          NEED TO WORK OUT VALUE OF DIVISOR            
         SR    RF,RF                                                            
         ICM   RF,1,PRGROSS        GET NUMBER OF DECIMALS                       
         BZ    *+12                                                             
         MH    RE,=H'10'                                                        
         BCT   RF,*-4                                                           
*                                                                               
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         A     R1,GUPTAX           ADD BACK TAX                                 
         ST    R1,GUPGROSS         STORE RESULT OVER INPUT                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=======================================================*                       
* WORK OUT LINE SPACING AND PRINT                       *                       
*=======================================================*                       
         SPACE 1                                                                
         DROP  RB,R8                                                            
FMTTOT   NMOD1 0,*FMTTOT*                                                       
         L     R8,=A(A2COMMON)                                                  
         USING A2COMMON,R8                                                      
*                                                                               
         MVI   FTOTGST,C'N'                                                     
         CLI   QGST,C'O'           TEST OUTPUT GST REQUEST                      
         BNE   FTOT0               NO                                           
         L     RE,ADCLT                                                         
         CLI   CEXTRA+11-CLTHDRD(RE),C'X'                                       
         BE    FTOT0                                                            
         CLI   CEXTRA+11-CLTHDRD(RE),C'Z'                                       
         BE    FTOT0                                                            
         L     RE,ADPRD                                                         
         CLI   PGSTCODE-PRDHDRD(RE),C'X'                                        
         BE    FTOT0                                                            
         CLI   PGSTCODE-PRDHDRD(RE),C'Z'                                        
         BE    FTOT0                                                            
*                                                                               
         CLI   MODE,PRDLAST        TEST REACHED PRD TOTALS YET                  
         BL    FTOT0                                                            
         CLI   PRSUMSW,C'R'        TEST REP RECAP                               
         BE    FTOT0               YES - SUPPRESS                               
         MVI   FTOTGST,C'Y'        ELSE SET TO PRINT GST THIS TIME              
*                                                                               
FTOT0    TM    DATASW,DSEXCH       TEST CANADIAN EXCHANGE                       
         BO    FMTCN                                                            
*                                                                               
         CLI   FTOTGST,C'Y'        TEST OUTPUT GST                              
         BE    FTOT0X              YES - ALWAYS RESET ADDRESSES                 
         OC    APLG1,APLG1         TEST ADDRESSES SET                           
         BNZ   FTOT10                                                           
*                                                                               
* FIND SPACING TABLE ENTRY                                                      
*                                                                               
FTOT0X   LA    R4,PRPTAB           NO-                                          
         CP    PRMONCNT,=P'6'      TEST MORE THAN 6 MONTHS                      
         BNH   *+8                 NO                                           
         LA    R4,18(R4)                                                        
         CLI   PROGPROF+0,1        TEST SPACING OPTION                          
         BNH   FTOT2                                                            
         LA    R4,6(R4)                                                         
         CLI   PROGPROF+4,C'X'     TEST PRINT GROSS+TAX                         
         BE    FTOT2               YES - NO TRIPLE SPACE OPTION                 
         CLI   MODE,PRDLAST        IGNORE OUTPUT GST BELOW PRODUCT              
         BL    *+12                                                             
         CLI   FTOTGST,C'Y'        TEST OUTPUT GST OPTION                       
         BE    FTOT2                                                            
         CLI   PROGPROF+0,2                                                     
         BNH   FTOT2                                                            
         LA    R4,6(R4)                                                         
         B     FTOT2                                                            
*                                                                               
         SPACE 2                                                                
*==============================================================*                
* ENTRIES IN TABLE ARE PRINT LINE NUMBERS FOR                  *                
*              MONTH LINE 1 / MONTH LINE 2 (AND TOTALS)        *                
*                TAX LINE 1 /   TAX LINE 2 (AND TOTALS)        *                
*              GR+TAX LINE 1/GR+TAX LINE 2 (AND TOTALS)        *                
*                                                              *                
* NOTE - TRIPLE SPACING FOR GROSS+TAX NOT SUPPORTED            *                
*==============================================================*                
         SPACE 1                                                                
PRPTAB   DS    0H                                                               
         DC    AL1(1,1,2,2,3,3)     LE 6 MONTHS/SINGLE SPACED                   
         DC    AL1(1,1,3,3,5,5)                 DOUBLE                          
         DC    AL1(1,1,4,4,7,7)                 TRIPLE                          
         DC    AL1(1,2,4,5,7,8)     GT 6 MONTHS/SINGLE                          
         DC    AL1(1,3,5,7,9,11)                 DOUBLE                         
         DC    AL1(1,4,7,10,13,16)               TRIPLE                         
         EJECT                                                                  
*======================================================*                        
* NOW SET PRINT LINE ADDRESSES                         *                        
*======================================================*                        
         SPACE 1                                                                
FTOT2    DS    0H                                                               
         LA    R0,4                                                             
         LA    R1,APLG1                                                         
FTOT3    BAS   RE,FTSETPA                                                       
         LA    R1,4(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,FTOT3                                                         
*                                                                               
         LA    R0,2                                                             
         LA    R1,APLGT1                                                        
FTOT4    BAS   RE,FTSETPA                                                       
         LA    R1,4(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,FTOT4                                                         
*                                                                               
         CLI   PROGPROF+4,C'Y'     TEST PRINT GROSS+TAX/TAX                     
         BNE   FTOT6               NO                                           
FTOT5    XC    APLGT1(8),APLGT1    CLEAR GROSS+TAX ADDRESSES                    
         B     FTOT10                                                           
*                                                                               
FTOT6    CLI   PROGPROF+4,C'X'     TEST PRINT GROSS/TAX/GROSS+TAX               
         BE    FTOT10              YES                                          
         CLI   FTOTGST,C'Y'        TEST OUTPUT GST OPTION                       
         BE    FTOT10                                                           
         XC    APLT1(8),APLT1      CLEAR TAX ADDRESSES                          
         XC    APLGT1(8),APLGT1    CLEAR GROSS+TAX ADDRESSES                    
         B     FTOT10                                                           
         SPACE 2                                                                
FTSETPA  SR    RF,RF                                                            
         IC    RF,0(R4)                                                         
         BCTR  RF,0                                                             
         MH    RF,=H'132'                                                       
         LA    RF,P(RF)                                                         
         ST    RF,0(R1)                                                         
         BR    RE                                                               
         EJECT                                                                  
*========================================================*                      
* FOR CANADIAN EXCHANGE FEATURES, NEED TO WORK OUT PRINT *                      
* LINE SPACING EVERY TIME WE PRINT                       *                      
*========================================================*                      
         SPACE 1                                                                
FMTCN    XC    APLG1(APLEND-APLG1),APLG1    CLEAR PRINT LINE ADDRS              
         MVI   CNDATASW,0                                                       
*                                                                               
         LA    R1,104(R2)                                                       
         LA    R0,13                                                            
         BAS   RE,CNZERTST         TEST NEED TO PRINT                           
         BZ    FMTCN2                                                           
         OI    CNDATASW,DSTAX                                                   
*                                                                               
FMTCN2   CLI   MODE,STALAST        ONLY PRINT EXCH AT STATION LEVEL             
         BH    FMTCN10                                                          
         TM    DATASW,DSEXCH       TEST REPORT EXCHANGE                         
         BZ    FMTCN4                                                           
         LA    R1,208(R2)                                                       
         LA    R0,13                                                            
         BAS   RE,CNZERTST                                                      
         BE    FMTCN4                                                           
         OI    CNDATASW,DSEXCH                                                  
*                                                                               
FMTCN4   TM    DATASW,DSC58        TEST REPORT C58                              
         BZ    FMTCN6                                                           
         LA    R1,312(R2)                                                       
         LA    R0,13                                                            
         BAS   RE,CNZERTST                                                      
         BE    FMTCN6                                                           
         OI    CNDATASW,DSC58                                                   
*                                                                               
FMTCN6   TM    DATASW,DSMSF        TEST REPORT MEDIA SVC FEE                    
         BZ    FMTCN10                                                          
         LA    R1,416(R2)                                                       
         LA    R0,13                                                            
         BAS   RE,CNZERTST                                                      
         BE    FMTCN10                                                          
         OI    CNDATASW,DSMSF                                                   
*                                                                               
FMTCN10  LA    R4,CNPTAB           CANADIAN EXCHANGE TABLES                     
         CP    PRMONCNT,=P'6'                                                   
         BNH   *+8                                                              
         LA    R4,30(R4)                                                        
         CLI   PROGPROF+0,1                                                     
         BNH   FMTCN12                                                          
         CP    PRMONCNT,=P'6'                                                   
         BH    FMTCN12                                                          
         LA    R4,10(R4)                                                        
         CLI   PROGPROF+0,2                                                     
         BNH   FMTCN12                                                          
         LA    R4,10(R4)                                                        
         EJECT                                                                  
FMTCN12  DS    0H                                                               
         LA    R0,2                                                             
         LA    R1,APLG1                                                         
FMTCN14  BAS   RE,FTSETPA                                                       
         LA    R1,4(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,FMTCN14                                                       
*                                                                               
         TM    CNDATASW,DSTAX      TEST TAX TO PRINT THIS TIME                  
         BZ    FMTCN18                                                          
         LA    R0,2                                                             
         LA    R1,APLT1                                                         
FMTCN16  BAS   RE,FTSETPA                                                       
         LA    R1,4(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,FMTCN16                                                       
*                                                                               
FMTCN18  TM    CNDATASW,DSEXCH     TEST EXCH TO PRINT THIS TIME                 
         BZ    FMTCN22                                                          
         LA    R0,2                                                             
         LA    R1,APLX1                                                         
FMTCN20  BAS   RE,FTSETPA                                                       
         LA    R1,4(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,FMTCN20                                                       
*                                                                               
FMTCN22  TM    CNDATASW,DSC58      TEST C58 TO PRINT THIS TIME                  
         BZ    FMTCN26                                                          
         LA    R0,2                                                             
         LA    R1,APLC1                                                         
FMTCN24  BAS   RE,FTSETPA                                                       
         LA    R1,4(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,FMTCN24                                                       
*                                                                               
FMTCN26  TM    CNDATASW,DSMSF      TEST MSF TO PRINT THIS TIME                  
         BZ    FMTCN30                                                          
         LA    R0,2                                                             
         LA    R1,APLM1                                                         
FMTCN28  BAS   RE,FTSETPA                                                       
         LA    R1,4(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,FMTCN28                                                       
*                                                                               
FMTCN30  CLI   CNDATASW,0          TEST ANY SPECIAL LINES TO PRINT              
         BE    FMTCNX              NO                                           
         LA    R0,2                IF YES MUST PRINT TOTAL LINE                 
         LA    R1,APLGT1                                                        
FMTCN32  BAS   RE,FTSETPA                                                       
         LA    R1,4(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,FMTCN32                                                       
*                                                                               
FMTCNX   DS    0H                                                               
         B     FTOT10                                                           
*                                                                               
CNZERTST DS    0H                                                               
         CP    0(8,R1),=P'0'                                                    
         BNER  RE                  EXIT WITH CC NOT EQ                          
         LA    R1,8(R1)                                                         
         BCT   R0,CNZERTST                                                      
         BR    RE                  EXIT WITH CC EQUAL                           
*                                                                               
CNPTAB   DS    0H                                                               
         DC    AL1(1,1,2,2,3,3,4,4,5,5)     LE 6 MNTHS/SINGLE SPACED            
         DC    AL1(1,1,3,3,5,5,7,7,9,9)                DOUBLE                   
         DC    AL1(1,1,4,4,7,7,10,10,13,13)            TRIPLE                   
         DC    AL1(1,2,4,5,7,8,10,11,13,14) GT 6 MNTHS/SINGLE                   
         EJECT                                                                  
*======================================================*                        
* PRINT THE DATA NOW (AT LAST)                         *                        
*======================================================*                        
         SPACE 1                                                                
FTOT10   MVC   APLG,APLG1          SET ACTIVE ADDRS                             
         MVC   APLT,APLT1                                                       
         MVC   APLX,APLX1                                                       
         MVC   APLC,APLC1                                                       
         MVC   APLM,APLM1                                                       
         MVC   APLGT,APLGT1                                                     
*                                                                               
         L     R4,APLG1                                                         
         CLI   PRSUMSW,C'N'        TEST CANAD NTWK SUMMARY                      
         BE    FTOT11              YES - SKIP SPOTS                             
         CLI   Q2COL22,C'N'        TEST SUPPRESS SPOTS                          
         BE    FTOT11                                                           
         EDIT  (P8,(R2)),(6,33(R4))     SPOTS                                   
         CLI   MODE,STALAST                                                     
         BNH   *+8                                                              
         MVI   39(R4),C'*'                                                      
*                                                                               
FTOT11   L     R3,=A(DICSECT)                                                   
         USING DICSECT,R3                                                       
*                                                                               
         ICM   R4,15,APLT                                                       
         BZ    FTOT1101                                                         
*--->    MVC   27(3,R4),=C'TAX'                                                 
         MVC   27(L'SP@TX,R4),SP@TX                                             
         CLI   FTOTGST,C'Y'                                                     
         BNE   *+10                                                             
         MVC   27(3,R4),=C'GST'                                                 
*                                                                               
FTOT1101 ICM   R4,15,APLGT                                                      
         BZ    FTOT11A                                                          
*--->    MVC   27(5,R4),=C'TOTAL'                                               
         MVC   27(L'SP5TOTAL,R4),SP5TOTAL                                       
         TM    DATASW,DSEXCH       TEST CANADIAN EXCH                           
         BO    FTOT11A                                                          
*--->    MVC   27(11,R4),=C'GROSS + TAX'                                        
         MVC   27(L'SP@GRSTX,R4),SP@GRSTX                                       
         CLI   FTOTGST,C'Y'                                                     
         BNE   *+10                                                             
*--->    MVC   27(11,R4),=C'GROSS + GST'                                        
         MVC   27(11,R4),SP@GRGST                                               
*                                                                               
FTOT11A  ICM   R4,15,APLX                                                       
         BZ    *+10                                                             
*--->    MVC   27(8,R4),=C'EXCHANGE'                                            
         MVC   27(L'SP@EXCHG,R4),SP@EXCHG                                       
         ICM   R4,15,APLC                                                       
         BZ    *+10                                                             
*--->    MVC   27(11,R4),=C'C58 RESERVE'                                        
         MVC   27(L'SP@C58,R4),SP@C58                                           
         ICM   R4,15,APLM                                                       
         BZ    *+10                                                             
*--->    MVC   27(11,R4),=C'MED SVC FEE'                                        
         MVC   27(L'SP@MSF,R4),SP@MSF                                           
         DROP  R3                                                               
*                                                                               
         LA    R3,8(R2)            DOLLARS                                      
         LA    R4,40               PRINT LINE DSPL                              
         ZAP   HALF,=P'7'                                                       
         CP    PRMONCNT,=P'13'                                                  
         BE    FTOT12                                                           
         ZAP   HALF,=P'6'                                                       
         CP    HALF,PRMONCNT                                                    
         BNH   *+10                                                             
         ZAP   HALF,PRMONCNT                                                    
         EJECT                                                                  
FTOT12   CLI   MODE,STALAST                                                     
         BNH   FTOT12X                                                          
* IF MODE > STALAST, ROLL EXCH/C58/MSF TO GROSS                                 
         TM    DATASW,DSEXCH                                                    
         BZ    *+16                                                             
         AP    0(8,R3),208(8,R3)    ADD EXCH                                    
         ZAP   208(8,R3),=P'0'                                                  
*                                                                               
         TM    DATASW,DSC58                                                     
         BZ    *+16                                                             
         AP    0(8,R3),312(8,R3)   ADD C58                                      
         ZAP   312(8,R3),=P'0'                                                  
*                                                                               
         TM    DATASW,DSMSF                                                     
         BZ    *+16                                                             
         AP    0(8,R3),416(8,R3)   ADD MSF                                      
         ZAP   416(8,R3),=P'0'                                                  
*                                                                               
FTOT12X  BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R3,8(R3)                                                         
         LA    R4,13(R4)                                                        
         SP    HALF,=P'1'                                                       
         BP    FTOT12                                                           
*                                                                               
         MVC   APLG,APLG2           UPDATE PRINT LINE ADDRESSES                 
         MVC   APLT,APLT2                                                       
         MVC   APLX,APLX2                                                       
         MVC   APLC,APLC2                                                       
         MVC   APLM,APLM2                                                       
         MVC   APLGT,APLGT2                                                     
*                                                                               
         ZAP   HALF,PRMONCNT                                                    
         SP    HALF,=P'6'                                                       
         BNP   FTOT20                                                           
         CP    PRMONCNT,=P'13'                                                  
         BNE   *+10                                                             
         ZAP   HALF,=P'6'                                                       
         LA    R4,40               SET DSPL                                     
*                                                                               
FTOT14   BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R3,8(R3)                                                         
         LA    R4,13(R4)                                                        
         SP    HALF,=P'1'                                                       
         BP    FTOT14                                                           
         EJECT                                                                  
*=======================================================*                       
* SUM AND PRINT PERIOD AND TAX TOTALS                   *                       
*=======================================================*                       
         SPACE 1                                                                
FTOT20   LA    RF,5                                                             
         LR    R3,R2                                                            
*                                                                               
FTOT21   ZAP   DUB,=P'0'                                                        
         LA    RE,8(R3)                                                         
         L     R0,MEDNUMMO                                                      
         AP    DUB,0(8,RE)         MONTHLY DOLLARS                              
         LA    RE,8(RE)                                                         
         BCT   R0,*-10                                                          
         ZAP   0(8,R3),DUB         STORE TOTS IN ACCUMS                         
         LA    R3,104(R3)                                                       
         BCT   RF,FTOT21                                                        
*                                                                               
         CLI   Q2COL31,C'Y'        TEST SUPPRESS TOTAL COL                      
         BE    FTOT22                                                           
*                                                                               
         LR    R3,R2                                                            
         LA    R4,118              SET DSPL                                     
         BAS   RE,FMTEDT                                                        
*                                                                               
FTOT22   CP    104(8,R2),=P'0'     TEST TAX=0                                   
         BNE   FTOT23                                                           
         ICM   R1,15,APLT1         YES-SUPPRESS ALL TAX PRINTING                
         BZ    FTOT24                                                           
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         C     R1,APLT2                                                         
         BNH   *-14                                                             
         ICM   R1,15,APLGT1        SUPPRESS GROSS+TAX                           
         BZ    FTOT24                                                           
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         C     R1,APLGT2                                                        
         BNH   *-14                                                             
         B     FTOT24                                                           
         EJECT                                                                  
* NEED TO INSURE LINE SKIP AFTER TAX                                            
*                                                                               
FTOT23   CLI   SPACING,2                                                        
         BNL   *+8                                                              
         MVI   SPACING,2                                                        
*                                                                               
* NEED TO FORCE BLANK LINES BETWEEN DATA TO PRINT                               
*                                                                               
FTOT24   LA    R1,P14                                                           
         LA    R0,13                                                            
FTOT25   CLC   0(132,R1),SPACES                                                 
         BH    FTOT28                                                           
         SH    R1,=H'132'                                                       
         BCT   R0,FTOT25                                                        
         B     FTOT30                                                           
*                                                                               
FTOT26   SH    R1,=H'132'          BACK UP TO PREVIOUS LINE                     
         CLI   0(R1),C' '          IF SPACE,                                    
         BNE   *+8                                                              
         MVI   0(R1),0              FORCE IT TO PRINT                           
FTOT28   BCT   R0,FTOT26                                                        
*                                                                               
FTOT30   LH    R0,PRCOLS           CLEAR ACCUMS                                 
         ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R0,*-10                                                          
* ATTEMPT CORRECT SPACING                                                       
         CLI   PROGPROF+0,2                                                     
         BL    *+8                                                              
         MVI   SPACING,2                                                        
         CLI   PROGPROF+0,3                                                     
         BL    *+8                                                              
         MVI   SPACING,3                                                        
*                                                                               
FMTTOTX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
FMTEDT   LR    R1,RE                                                            
         L     RF,APLG             GET GROSS DOLLAR PRINT LINE ADDR             
         ZAP   DUB,0(8,R3)                                                      
         BAS   RE,FMTDOL                                                        
*                                                                               
         ICM   RF,15,APLT          TEST TAX REPORTING                           
         BZ    *+14                                                             
         ZAP   DUB,104(8,R3)                                                    
         BAS   RE,FMTDOL                                                        
*                                                                               
         ICM   RF,15,APLX          TEST EXCHANGE REPORTING                      
         BZ    *+14                                                             
         ZAP   DUB,208(8,R3)                                                    
         BAS   RE,FMTDOL                                                        
*                                                                               
         ICM   RF,15,APLC          TEST C58 TAX REPORTING                       
         BZ    *+14                                                             
         ZAP   DUB,312(8,R3)                                                    
         BAS   RE,FMTDOL                                                        
*                                                                               
         ICM   RF,15,APLM          TEST MEDIA SERVICE FEE REPORTING             
         BZ    *+14                                                             
         ZAP   DUB,416(8,R3)                                                    
         BAS   RE,FMTDOL                                                        
*                                                                               
         ICM   RF,15,APLGT         TEST TOTAL REPORTING                         
         BZ    FMTEDTX                                                          
         ZAP   DUB,0(8,R3)                                                      
         AP    DUB,104(8,R3)                                                    
         AP    DUB,208(8,R3)                                                    
         AP    DUB,312(8,R3)                                                    
         AP    DUB,416(8,R3)                                                    
         BAS   RE,FMTDOL                                                        
*                                                                               
FMTEDTX  LR    RE,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
FMTDOL   AR    RF,R4                                                            
         STM   RE,R1,CURER1                                                     
         LR    R5,RF               SAVE RF                                      
*--->    EDIT  (P8,DUB),(12,(RF)),2,MINUS=YES                                   
         CURED (P8,DUB),(12,(R5)),CURTAB,FLOAT=-                                
         LM    RE,R1,CURER1                                                     
         CP    DUB,=P'0'                                                        
         BM    *+16                                                             
         CLI   MODE,STALAST                                                     
         BNH   *+8                                                              
*--->    MVI   11(RF),C'*'                                                      
         MVI   12(RF),C'*'                                                      
         BR    RE                                                               
FTOTGST  DC    C'N'                                                             
         LTORG                                                                  
         EJECT                                                                  
*****************************                                                   
* PROCESS ESTHDRS AND BILLS *                                                   
*****************************                                                   
         SPACE 1                                                                
PROCEB   NMOD1 0,**PREB**                                                       
         L     R8,=A(A2COMMON)                                                  
         USING A2COMMON,R8                                                      
*                                                                               
         CLC   =C'ALL',QMKT                                                     
         BNE   PROCEBX                                                          
         CLI   PRDSW,C'Y'          TEST ANY ESTS PROCESSED                      
         BNE   PROCEBX                                                          
         MVI   PRDSW,C'N'          RESET SWITCH                                 
* MUST BE ALL STATIONS AS WELL AS ALL MARKETS                                   
         CLC   QSTA,SPACES                                                      
         BE    *+14                                                             
         CLC   =C'ALL',QSTA                                                     
         BNE   PROCEBX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,ADPRD                                                         
         MVC   KEY(13),0(R6)       RESEQ TO PRDHDR                              
         CLI   BPRD,X'FF'                                                       
         BNE   *+10                                                             
         XC    KEY+4(3),KEY+4      OR CLTHDR IF POL REQ                         
         GOTO1 HIGH                                                             
*                                                                               
PROCEB2  GOTO1 SEQ                                                              
         CLI   BPRD,X'FF'          TEST POL REQ                                 
         BNE   PROCEB4             NO                                           
         CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         BNE   PROCEBX                                                          
         B     PROCEB6                                                          
*                                                                               
PROCEB4  CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   PROCEBX                                                          
*                                                                               
PROCEB6  OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   *+12                YES                                          
         CLI   KEY+7,0             TEST ESTHDR                                  
         BE    PROCEB2             NO                                           
* FILTER ON EST                                                                 
         CLI   BEST,0              TEST 'NO' REQ                                
         BE    PROCEB10                                                         
         CLI   BESTEND,0                                                        
         BNE   PROCEB8                                                          
         CLC   BEST,KEY+7                                                       
         BNE   PROCEB2                                                          
         B     PROCEB12                                                         
PROCEB8  CLC   BEST,KEY+7                                                       
         BH    PROCEB2                                                          
         CLC   BESTEND,KEY+7                                                    
         BL    PROCEB2                                                          
         B     PROCEB12                                                         
*                                                                               
PROCEB10 ZIC   RE,KEY+7                                                         
         LTR   RE,RE               TEST EST=0 IN KEY                            
         BZ    PROCEB12                                                         
         LA    RE,ESTLST(RE)                                                    
         CLI   0(RE),0                                                          
         BE    PROCEB2                                                          
*                                                                               
PROCEB12 OC    KEY+8(5),KEY+8      TEST BILL                                    
         BZ    PROCEB24                                                         
         L     R6,ADBILL                                                        
         USING BILLRECD,R6                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         CLI   BRETAIL,X'41'       TEST RETAIL CONTROL                          
         BE    PROCEB2             YES - IGNORE                                 
*                                                                               
         CLI   BTYPE+1,4           TEST DETAIL BILL                             
         BE    PROCEB2             YES - IGNORE                                 
*                                                                               
         LA    R0,C'I'             SET INVOICE LEVEL INTRFC                     
         GOTO1 =A(PRINTRFC)                                                     
*                                                                               
* POST TO SUMMARY ACCUMULATORS                                                  
*                                                                               
         L     R2,APRSUMPR                                                      
         USING SUMACCD,R2                                                       
         LA    R4,MEDMON01                                                      
         L     R5,MEDNUMMO                                                      
         OC    BILSTADT,BILSTADT   TEST BILL PERIOD START DATE PRESENT          
         BZ    PROCEB16            NO                                           
*                                                                               
         CLI   BEST,0              TEST EST=NO                                  
         BE    PROCEB14                                                         
         CLI   BESTEND,0           OR SERIES                                    
         BNE   PROCEB14                                                         
         L     RE,ADEST                                                         
         USING ESTHDRD,RE                                                       
         CLC   QSTART,ESTART       TEST REQ START = EST START                   
         BH    PROCEB14                                                         
         CLC   0(2,R4),BILSTADT    IF BILL PRIOR TO EST START                   
         BH    PROCEB18            POST TO FIRST MONTH                          
         DROP  RE                                                               
*                                                                               
PROCEB14 CLC   0(2,R4),BILSTADT                                                 
         BH    *+14                                                             
         CLC   2(2,R4),BILSTADT                                                 
         BNL   PROCEB18                                                         
         LA    R2,48(R2)                                                        
         LA    R4,12(R4)                                                        
         BCT   R5,PROCEB14                                                      
         B     PROCEB2                                                          
*                                                                               
PROCEB16 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,2(R4)),DUB     GET MONTH END IN YYMMDD            
         CLC   BMONSERV(4),DUB                                                  
         BE    PROCEB18                                                         
         LA    R2,48(R2)                                                        
         LA    R4,12(R4)                                                        
         BCT   R5,PROCEB16                                                      
         B     PROCEB2                                                          
*                                                                               
PROCEB18 DS    0H                                                               
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
         ZAP   EBDOLS,SPBVGRSP     EFFECTIVE GROSS                              
         CLI   RQGETBF,C'X'        TEST REPORT NET DOLLARS                      
         BNE   *+10                                                             
         ZAP   EBDOLS,SPBVNETP                                                  
*                                                                               
         CLI   QCOST2,C'Y'         TEST COS2 REQUEST                            
         BE    PROCEB20                                                         
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CAN AGY                           
         BNE   PROCEB21                                                         
         CLI   RQCRRNCY,C'U'       TEST US $ REQ                                
         BNE   PROCEB21                                                         
PROCEB20 ZAP   EBDOLS,BGRS2P                                                    
*                                                                               
PROCEB21 DS    0H                                                               
         MVC   EBTAX,SPBVETAX                                                   
         OC    PRGROSS+1(3),PRGROSS+1  TEST GROSS UP REQUEST                    
         BZ    *+8                                                              
         BAS   RE,PEBGUP                                                        
*                                                                               
         CLI   RQGETBF,C'Y'        TEST BILL FORMULA REQUEST                    
         BNE   *+10                                                             
         ZAP   EBDOLS,SPBVACTP     IF YES, SET ACTUAL BILL AMT                  
*                                                                               
         CLC   BDATE(6),TODAY                                                   
         BE    PROCEB22                                                         
         AP    SUMBLL,EBDOLS                                                    
         B     PROCEB2                                                          
*                                                                               
PROCEB22 AP    SUMBLLTD,EBDOLS                                                  
         B     PROCEB2                                                          
         EJECT                                                                  
* PROCESS ESTHDR *                                                              
         SPACE 1                                                                
PROCEB24 CLI   QOPT2,C'A'          TEST AUTH AMTS REQUESTED                     
         BNE   PROCEB2             YES - ALWAYS PROCESS                         
         L     R6,ADBUY                                                         
         USING ESTHDRD,R6                                                       
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
* USE END OF BROADCAST MONTH TO GET EST START YEAR/MONTH                        
         GOTO1 GETBROAD,DMCB,ESTART,WORK                                        
         GOTO1 DATCON,DMCB,WORK+6,(3,DUB)                                       
         MVC   HALF(2),DUB         SAVE END YEAR/MONTH                          
* NOW ADD 11 MONTHS TO GET LAST Y/M IN EST PERIOD                               
         MVC   WORK(6),WORK+6      MOVE END DATE                                
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                              
         GOTO1 DATCON,DMCB,WORK+6,(3,DUB)                                       
         MVC   HALF2(2),DUB        SET END OF 12 MONTH PERIOD                   
*                                                                               
         MVC   FULL(2),HALF                                                     
         MVI   FULL+2,15                                                        
*                                                                               
PROCEB26 DS    0H                                                               
* POST TO SUMMARY ACCUM                                                         
         L     R2,APRSUMPR                                                      
         USING SUMACCD,R2                                                       
         LA    R4,MEDMON01                                                      
         L     R5,MEDNUMMO                                                      
*                                                                               
PROCEB28 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,2(R4)),(3,DUB)      GET 3 BYTE YMD                
         CLC   DUB(2),FULL         MATCH TO MONTH IN PROCESS                    
         BE    PROCEB30                                                         
         LA    R2,48(R2)                                                        
         LA    R4,12(R4)                                                        
         BCT   R5,PROCEB28                                                      
         B     PROCEB32                                                         
*                                                                               
PROCEB30 ZIC   R5,FULL+1           GET MONTH NUM                                
         BCTR  R5,0                                                             
         MHI   R5,6                                                             
         LA    R5,EAUTH(R5)                                                     
         AP    SUMAUTH,0(6,R5)                                                  
*                                                                               
PROCEB32 ZIC   RE,FULL+1                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FULL+1                                                        
         CLI   FULL+1,12                                                        
         BNH   PROCEB34                                                         
         MVI   FULL+1,1                                                         
         IC    RE,FULL                                                          
         LA    RE,1(RE)                                                         
         STC   RE,FULL                                                          
PROCEB34 CLC   FULL(2),HALF2       TEST PAST EST END                            
         BNH   PROCEB26                                                         
         B     PROCEB2                                                          
*                                                                               
PROCEBX  XIT1                                                                   
         EJECT                                                                  
*=======================================================*                       
* SUBROUTINE GROSSES UP DOLLARS BY SPECIFIED PERCENTAGE *                       
* AFTER SUBTRACTING TAX DOLLARS                         *                       
* INPUT IS GUPGROSS/GUPTAX. OUTPUT IS GUPGROSS          *                       
*=======================================================*                       
         SPACE 1                                                                
PEBGUP   NTR1                                                                   
         ZAP   EBWORK,EBDOLS                                                    
         L     R0,EBTAX                                                         
         CVD   R0,DOUBLE                                                        
         SP    EBWORK,DOUBLE       EBWORK = EBDOLS - EBTAX                      
         SR    R0,R0                                                            
         ICM   R0,7,PRGROSS+1      GET ADJUSTMENT PERCENTAGE                    
         CVD   R0,DUB                                                           
         MP    EBWORK,DUB          EBWORK = %AGE ADJUSTED EBDOLS                
         SRP   EBWORK,64-6,5       DIVIDE BY 1,000,000 AND ROUND                
         AP    EBWORK,DOUBLE       ADD BACK TAX NOW                             
         ZAP   EBDOLS,EBWORK       AND SET RESULT                               
         XIT1                                                                   
*                                                                               
EBWORK   DS    PL16                                                             
EBDOLS   DS    PL6                                                              
EBTAX    DS    F                                                                
         LTORG                                                                  
         EJECT                                                                  
************************************                                            
* PRINT COMMENTS AT BOTTOM OF PAGE *                                            
************************************                                            
         SPACE 1                                                                
PRTBOT   NMOD1 0,**PRTB**                                                       
         L     R8,=A(A2COMMON)                                                  
         USING A2COMMON,R8                                                      
*                                                                               
         CLI   PRTSUMSW,C'Y'       TEST PRODUCT SUMMARY WAS PRINTED             
         BNE   PRTBOTX             NO                                           
*                                                                               
         L     R1,ADCOMREC         A(COMMENT RECORD)                            
         OC    0(13,R1),0(R1)      TEST ANY COMMENT FOUND                       
         BZ    PRTBOTX             NO                                           
*                                                                               
         LA    R1,24(R1)           A(FIRST ELEMENT)                             
         MVI   P1,0                SKIP THE FIRST LINE                          
         LA    R2,P2               PUT COMMENTS IN P2-P14                       
         MVI   BYTE,C'N'           NO COMMENTS FOUND YET                        
*                                                                               
PRTBOT10 CLI   0(R1),X'15'         FIND A BOTTOM COMMENT                        
         BNE   PRTBOT20                                                         
         ZIC   RE,1(R1)            GET ELEMENT LENGTH                           
         SH    RE,=H'3'            SET FOR EX                                   
         BM    PRTBOT20            MAKE SURE THERE'S SOME DATA                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),2(R1)       PUT COMMENT IN PRINT LINE                    
         LA    R2,132(R2)          BUMP TO NEXT PRINT LINE                      
         MVI   BYTE,C'Y'           A COMMENT WAS FOUND                          
*                                                                               
PRTBOT20 ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             TEST END OF RECORD                           
         BNE   PRTBOT10            NO                                           
*                                                                               
         CLI   BYTE,C'Y'           TEST ANY COMMENTS FOUND                      
         BNE   PRTBOTX             NO                                           
         GOTO1 REPORT              PRINT BOTTOM COMMENTS                        
*                                                                               
PRTBOTX  XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*******************************************************                         
* PROVIDE COMMON LINKAGE TO INTERFACE TAPE SUBROUTINE *                         
* ON ENTRY, INTERFACE LEVEL CODE IS IN R0             *                         
*******************************************************                         
         SPACE 1                                                                
PRINTRFC NMOD1 0,*PRNTRFC                                                       
         L     R8,=A(A2COMMON)                                                  
         USING A2COMMON,R8                                                      
*                                                                               
         CLC   =C'BJ',AGY                                                       
         BNE   PRNTRFC2                                                         
         CLC   =C'TAPE',QUESTOR                                                 
         BNE   PRNTRFCX                                                         
         B     PRNTRFC4                                                         
PRNTRFC2 DS    0H                                                               
         CLC   =C'JW',AGY                                                       
         BNE   PRNTRFCX                                                         
         CLC   =C'EK-',QUESTOR                                                  
         BE    PRNTRFC4                                                         
         CLC   =C'FDT-',QUESTOR                                                 
         BE    PRNTRFC4                                                         
         B     PRNTRFCX                                                         
*                                                                               
PRNTRFC4 DS    0H                                                               
         GOTO1 =V(SPA2TP),DMCB,((R0),(RA)),PRREC                                
*                                                                               
PRNTRFCX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------*               
* FILTER ON RATE TYPE                                           *               
* NOTE THAT THERE IS AN EXPLICIT TEST FOR TRADE BUYS BECAUSE    *               
* 'T' CAN BE COMBINED WITH OTHER RATE TYPES AND CONFUSES THINGS *               
*---------------------------------------------------------------*               
         SPACE 1                                                                
RATEFLT  NTR1  BASE=*                                                           
*                                                                               
         CLI   Q2COL32,C'P'        TEST NTP REQUEST                             
         BNE   RATEFL2                                                          
         CLC   QPRD,=C'POL'                                                     
         BNE   RATEFL2                                                          
         MVI   NOPRDNTP,C'Y'       SET SUPPRESS NTP BY PRD                      
*                                                                               
RATEFL2  LA    RE,COSTAB                                                        
         LA    RF,(COSTABX-COSTAB)/3                                            
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         CLI   Q2COL32,C'T'        TEST 'TRADE' REQUEST                         
         BNE   RATEFL4                                                          
         TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BO    RATEFLQ                                                          
         B     RATEFLNQ                                                         
*                                                                               
*  DECODE VALUES IN BUY RECORD                                                  
*                                                                               
RATEFL4  MVC   HALF(1),BDCIND      GET VALUE FROM BUY                           
         NI    HALF,X'FE'          TURN OFF MINUS SPOT IND                      
         MVC   HALF+1(1),BDCIND2                                                
         NI    HALF+1,X'80'        DROP ALL BUT C RATE                          
*                                                                               
RATEFL6  CLC   HALF,1(RE)          MATCH BDCIND VALUE                           
         BNE   RATEFL8                                                          
         CLC   HALF+1(1),2(RE)     MATCH BDCIND2 VALUE                          
         BE    RATEFL10                                                         
*                                                                               
RATEFL8  LA    RE,3(RE)                                                         
         BCT   RF,RATEFL6                                                       
         B     RATEFLNQ                                                         
*                                                                               
RATEFL10 CLC   Q2COL32(1),0(RE)    DOES BUY MATCH REQUEST                       
         BNE   RATEFLNQ                                                         
*                                                                               
RATEFLQ  CR    RE,RE               SET CC EQ                                    
         B     *+6                                                              
RATEFLNQ LTR  RE,RE                                                             
         XIT1                                                                   
*                                                                               
COSTAB   DC    C'S',X'0400',C'F',X'8000',C'N',X'1000',C'Q',X'4000'              
         DC    C'V',X'0800',C'X',X'0200',C'P',X'0000',C'C',X'2080'              
         DC    C'G',X'2000'                                                     
COSTABX  EQU   *                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO SEND AN EMAIL NOTIFICATION IF FCRDBUYS=I                           
*=====================================================================          
                                                                                
NOTEOUT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADCONLST                                                      
         USING SPADCONS,R6                                                      
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
         GOTOR VSMTP,DMCB,('SMTPAPRS',TOWHO),(L'SUBJECT,SUBJECT)                
***                                                                             
* GET JOB/STEP NAMES                                                            
***                                                                             
         LA    R4,FULL                                                          
         EXTRACT (R4),FIELDS=TIOT                                               
*                                                                               
         L     R4,FULL                                                          
         USING TIOTD,R4                                                         
         MVC   P(8),TIOCNJOB                                                    
         MVC   P+9(8),TIOCPSTN     PROC STEP NAME                               
         DROP  R4                                                               
*                                                                               
         MVC   P+18(80),QAREA                                                   
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
         DROP  R6                                                               
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
TOWHO    DC    C'MHER,EJOR:'                                                    
SUBJECT  DC    C'SA2 RUNNING IN ID SEQUENCE'                                    
         EJECT                                                                  
DICSECT  DS    0D                                                               
DCLIST   DS    0C                                                               
         DCDDL SP#ATDL,18                                                       
         DCDDL SP#AUTH,23                                                       
         DCDDL SP#BADAL,14                                                      
         DCDDL SP#BLDT,17                                                       
         DCDDL SP#BLDTY,19                                                      
         DCDDL SP#C58,11                                                        
         DCDDL SP#CANDL,20,F                                                    
         DCDDL SP#CLI,6                                                         
         DCDDL SP#CLITL,17,F                                                    
         DCDDL SP#CONTI,9                                                       
         DCDDL SP#DLAMT,19                                                      
         DCDDL SP#DOL01,57                                                      
         DCDDL SP#EXCHG,8                                                       
         DCDDL SP#FLACT,17,F                                                    
         DCDDL SP#GLDL,12                                                       
         DCDDL SP#GLORD,17                                                      
         DCDDL SP#GRGST,17                                                      
         DCDDL SP#GRSTX,11                                                      
         DCDDL SP#GST,3                                                         
         DCDDL SP#GSTI,50                                                       
         DCDDL SP#GSTO,50                                                       
         DCDDL SP#INVL1,43,F                                                    
         DCDDL SP#INVL2,45,F                                                    
         DCDDL SP#INVL3,45,F                                                    
         DCDDL SP#MIS01,29,F                                                    
         DCDDL SP#MNTL,18,L,LABEL=SPSMNTL                                       
         DCDDL SP#MRKT,6                                                        
         DCDDL SP#MSF,11                                                        
         DCDDL SP#NTWRK,7                                                       
         DCDDL SP#ORDDT,18                                                      
         DCDDL SP#OLB,22                                                        
         DCDDL SP#PAY01,40,F                                                    
         DCDDL SP#PDDT,14                                                       
         DCDDL SP#PDTDY,16                                                      
         DCDDL SP#POAG,23                                                       
         DCDDL SP#PRO,7                                                         
         DCDDL SP#PRDTL,20,F                                                    
         DCDDL SP#RECAP,40,F                                                    
         DCDDL SP#REP,5                                                         
         DCDDL SP#REQ01,42,F                                                    
         DCDDL SP#SPOTS,5,LABEL=SP5SPOTS                                        
         DCDDL SP#STATN,7                                                       
         DCDDL SP#TX,4                                                          
         DCDDL SP#TOTAL,8,L,LABEL=SP8TOTAL                                      
         DCDDL SP#TOTAL,6,L,LABEL=SP6TOTAL                                      
         DCDDL SP#TOTAL,5,L,LABEL=SP5TOTAL                                      
         DCDDL SP#USDOL,20,F                                                    
DCLISTX  DC    X'00'                                                            
*                                                                               
DSLIST   DS    0C                                                               
         DSDDL PRINT=YES                                                        
DSLISTX  EQU   *                                                                
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************                                                         
* BUFFALO  DEFINITION *                                                         
***********************                                                         
SPA202   CSECT                                                                  
         BUFF  LINES=800,ROWS=1,COLUMNS=14,FLAVOR=PACKED,              X        
               KEYLIST=(12,A),COMMENT=32                                        
         SPACE 2                                                                
TOTACCD  DSECT                                                                  
TOTSPOTS DS    PL8                                                              
TOTMON1  DS    PL8                                                              
TOTMON2  DS    PL8                                                              
TOTMON3  DS    PL8                                                              
TOTMON4  DS    PL8                                                              
TOTMON5  DS    PL8                                                              
TOTMON6  DS    PL8                                                              
TOTMON7  DS    PL8                                                              
TOTMON8  DS    PL8                                                              
TOTMON9  DS    PL8                                                              
TOTMON10 DS    PL8                                                              
TOTMON11 DS    PL8                                                              
TOTMON12 DS    PL8                                                              
TOTMON13 DS    PL8                                                              
         SPACE 2                                                                
SUMACCD  DSECT                     24 BYTES PER MONTH                           
*                                                                               
SUMORD   DS    PL8                 ORDERED TO DATE                              
SUMAUTH  DS    PL8                 AUTHORIZED (GOAL)                            
SUMPAY   DS    PL8                 PAID TO DATE                                 
SUMPAYTD DS    PL8                      TODAY                                   
SUMBLL   DS    PL8                 BILLED TO DATE                               
SUMBLLTD DS    PL8                        TODAY                                 
         EJECT                                                                  
       ++INCLUDE DDBUFFALOD                                                     
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
* SPGENSTAB                                                                     
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENAGY                                                       
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
         PRINT  ON                                                              
         SPACE 2                                                                
QOPT6    EQU   QOPT5+1             COL 67                                       
QOPT7    EQU   QOPT6+1             COL 68                                       
*                                                                               
Q2COL21  EQU   QAREA2+20           US/CAN DOLLAR OVERRIDE                       
Q2COL22  EQU   QAREA2+21           PRINT SPOTS                                  
Q2GUP    EQU   QAREA2+22   (8)     GROSS UP PCTG (OR 'BF' OR 'NET')             
Q2COL31  EQU   QAREA2+30                                                        
Q2COL32  EQU   QAREA2+31           RATE TYPE FILTER                             
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
TIOTD    DSECT                                                                  
         IEFTIOT1                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPA202 09/28/07'                                      
         END                                                                    
