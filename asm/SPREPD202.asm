*          DATA SET SPREPD202  AT LEVEL 096 AS OF 11/19/19                      
*PHASE SPD202T                                                                  
*INCLUDE MEDBDESC                                                               
*INCLUDE REVBUY                                                                 
*INCLUDE SPRPFOOT                                                               
*INCLUDE REPSPILL                                                               
*INCLUDE REPUDESC                                                               
*INCLUDE REPCALOV                                                               
*INCLUDE SPOMCOM                                                                
*INCLUDE COVAIL                                                                 
                                                                                
*===================== SPECIAL REPORTS===============================*          
*                                                                    *          
*  QOPT1 = A  1. FORCES AN 'X' TO BE PRINTED IN THE GRID             *          
*             2. SUPPRESSES DEMOS, COST, CPP/M.                      *          
*             3. FORCES NO SPILL REPORTING.                          *          
*             4. SUPPRESSES STATION TOTALS.                          *          
*             5. FORCES MARKET TOTAL REPORT (BDS) WITH NO DAYPARTS   *          
*             6. SUPPRESSES BUYLINE TOTAL SPOTS COLUMN.              *          
*                                                                    *          
*  QOPT1 = B  1. SUPPRESSES DEMOS, COST, CPP/M.                      *          
*             2. FORCES NO SPILL REPORTING.                          *          
*             3. SUPPRESSES STATION TOTALS.                          *          
*             4. FORCES MARKET TOTAL REPORT (BDS) WITH NO DAYPARTS   *          
*             5. SHIFTS HEADLINES TO 110 POSITIONS                   *          
*                                                                    *          
*====================================================================*          
* NOTE THAT FOR ALL REPORTS BUT SDX, PROFILES ARE IDENTICAL          *          
* BE CAREFUL !                                                       *          
*====================================================================*          
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* MHER             05/24/19 SUPPORT 2-DECIMAL IMPS                              
* AKAT DSSUP-7331  04/19/16 FIX DEMO OVERRIDE 2-DECIMAL BUG           *         
* AKAT MOSTK-86    02/09/16 SUPPORT NEW CM BAND                       *         
***********************************************************************         
         SPACE 1                                                                
         PRINT NOGEN                                                            
         TITLE 'SPREPD202-BRS,BTS,SAL REPORTS'                                  
SPD202   CSECT                                                                  
         NMOD1 0,SPD202,R3                                                      
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
*                                                                               
         L     R2,=A(SPD2WK)                                                    
         USING SPD2WK,R2                                                        
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
*                                                                               
         USING MEDDATA,R4                                                       
*                                                                               
         L     RE,=A(SPD2RA)                                                    
         STM   RA,RC,0(RE)                                                      
         L     RE,=A(SPD2R2)                                                    
         STM   R2,R3,0(RE)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   M00                                                              
         MVI   DXFNSW,C'N'                                                      
         MVI   DARESVSW,C'N'                                                    
         MVI   RQALPHA,C'Y'         STATIONS IN ALPHA SEQ                       
         OI    RQOPTS,RQOPTS_CENTS  GET CENTS TO $10000 FOR RADIO               
         OI    RQOPTS,RQOPTS_CBHLAST  AND ASK FOR HDND LAST MODE                
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
         CLC   QPROG,=C'U3'        T/A TRASH IN THIS FIELD SOMETIME             
         BNE   *+10                PROTECT AGAINST THAT                         
         MVC   QBOOK1(7),SPACES                                                 
*                                                                               
         MVI   PWADJCR,C'N'                                                     
         MVI   D2ADJCR,C'N'                                                     
         MVI   MEDDAILY,C'N'       INIT                                         
         MVI   PRDEMCNT,4                                                       
         SPACE 2                                                                
         CLC   QPROG,=C'D7'                                                     
         BE    SET02                                                            
         CLC   QPROG,=C'DX'                                                     
         BNE   SET04                                                            
SET02    OI    RQOPTS,RQOPTS_NOPIG  SUPPRESS P/B DOLLAR SPLIT FOR DX            
*                                                                               
SET04    MVI   SPDWSW,0                                                         
         CLC   QPROG,=C'D2'                                                     
         BE    SET04A                                                           
         CLC   QPROG,=C'D3'                                                     
         BE    SET04A                                                           
         CLC   QPROG,=C'D4'                                                     
         BE    SET04A                                                           
         CLC   QPROG,=C'D6'                                                     
         BNE   SET04B                                                           
SET04A   OI    RCOPTS,RCOPT_PWADJTST                                            
*                                                                               
         XC    UNWSTRP,UNWSTRP     CLEAR UNWIND START                           
         MVC   UNWENDP,=X'FFFF'    AND FORCE HIGH END                           
         MVI   UNWOPTS,0           CLEAR UNWIND OPTIONS                         
         CLI   Q2UNWIND,C'U'       TEST UNWIND REQUEST                          
         BNE   SET04B              NO                                           
         CLI   Q2UNWMG,C'Y'                                                     
         BNE   *+8                                                              
         OI    UNWOPTS,X'80'                                                    
         CLI   Q2UNWPLS,C'Y'                                                    
         BNE   *+8                                                              
         OI    UNWOPTS,X'40'                                                    
         CLI   Q2UNWMIN,C'Y'                                                    
         BNE   *+8                                                              
         OI    UNWOPTS,X'20'                                                    
         CLI   Q2UNWSTR,C' '       TEST DATES SPECIFIED                         
         BNH   SET04B                                                           
         GOTO1 DATCON,DMCB,Q2UNWSTR,(2,UNWSTRP)                                 
         GOTO1 DATCON,DMCB,Q2UNWEND,(2,UNWENDP)                                 
         MVC   Q2UNWIND(16),SPACES  BLANK UNWIND OPTIONS ON Q2                  
*                                                                               
SET04B   CLI   QPWCV,C'Y'          SET UP FOR SPECIAL WESTERN REPORT            
         BNE   *+8                                                              
         MVI   SPDWSW,C'Y'                                                      
         MVC   RQC58OPT,Q2USER                                                  
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         XC    DMCB,DMCB                                                        
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
         DROP  R5,R1                                                            
*                                                                               
M00      CLI   MODE,REQLAST        ALWAYS PROCESS REQ LAST                      
         BE    M34                                                              
         CLI   MODE,CBHLAST                                                     
         BNE   M00A                                                             
         BRAS  RE,PRTMGA                                                        
         B     EXIT                                                             
*                                                                               
M00A     CLI   MODE,MKTLAST                                                     
         BL    BYPW                                                             
         MVI   H11,C' '            ADD UP MARKET WEIGHTS                        
         MVC   H11+1(131),H11                                                   
         MVC   H12,H11                                                          
         MVC   BRSPCAP,H11                                                      
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         MVC   WEIGHT,SPWEIGHT                                                  
BYPW     DS    0H                                                               
         CLI   MODE,ESTFRST                                                     
         BNH   *+12                                                             
         CLI   QSTART,C' '                                                      
         BE    EXIT                                                             
         TM    RQFLAG,RQFLAG_PWADJ                                              
         BZ    *+8                                                              
         MVI   PWADJCR,C'Y'                                                     
         L     RE,ADEST                                                         
         USING ESTHDR,RE                                                        
         CLC   QPROG,=C'D7'                                                     
         BE    *+14                                                             
         CLC   QPROG,=C'DX'                                                     
         BNE   BYPDAY                                                           
         CLI   EDAILY,C'Y'         SUPPORT FAXABLE DAILIES                      
         BNE   BYPDAY                                                           
         CLI   QESTEND,C' '        BUT ONLY FOR SINGLE EST REQUEST              
         BNE   BYPDAY                                                           
         TM    QEST,X'F0'                                                       
         BNO   BYPDAY                                                           
         MVI   MEDDAILY,C'Y'       SET FOR DAILY CALENDAR                       
         DROP  RE                                                               
BYPDAY   GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
         CLI   MODE,RUNFRST                                                     
         BNE   M1                                                               
         BRAS  RE,INIT                                                          
         B     EXIT                                                             
         SPACE 2                                                                
M1       CLI   MODE,MKTFRST                                                     
         BL    M2                                                               
         CLI   ESTACT,0            ANY ESTIMATES FOR PRODUCT                    
         BE    EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M3                                                               
         L     RE,=A(PWDLTAB)                                                   
         XC    0(50,RE),0(RE)                                                   
         L     RE,=A(PWADJTAB)                                                  
         XC    0(50,RE),0(RE)                                                   
         BRAS  RE,RQFIRST                                                       
*                                                                               
         BRAS  RE,RQFRSTA                                                       
         L     RE,VSVMDBLK                                                      
         LA    RF,1204                                                          
         XCEF                                                                   
         MVI   PASS,0                                                           
         MVI   MODE,RUNFRST                                                     
         BAS   R9,GOTOSUB                                                       
         MVI   MODE,REQFRST                                                     
         GOTO1 =A(BFLOAT)             SET BUFFALO LEN - BEYOND BRAS             
*                                                                               
M2A      BAS   R9,GOTOSUB                                                       
         MVI   FIRST,0                                                          
         B     EXIT                                                             
         EJECT                                                                  
M3       CLI   MODE,CLTFRST        CLIENT FIRST                                 
         BNE   M4                                                               
         MVI   CALLMGBY,C'Y'       SET TO CALL MGBY BEFORE MGABLD               
* READ FX PROFILE                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0FX'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),CLT                                                    
         MVI   WORK+10,C'*'                                                     
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVC   WORK+11(1),COFFICE                                               
         MVC   WORK+16(16),WORK         SAVE FX PROFILE KEY                     
         GOTO1 GETPROF,DMCB,WORK,FXPROF,DATAMGR                                 
* READ DAR PROFILE                                                              
         MVC   WORK(4),=C'SDAR'                                                 
         NI    WORK,X'BF'          MAKE 'S' LOWERCASE                           
         GOTO1 GETPROF,DMCB,WORK,DARPROF,DATAMGR                                
         DROP  R6                                                               
*                                                                               
M3A      MVI   BPRD2,0                                                          
         CLC   QPROG,=C'CP'        TEST CP OR DX WITH SECOND                    
         BE    *+14                PRODUCT                                      
         CLC   QPROG,=C'DX'                                                     
         BNE   M3D                                                              
         CLC   QPTNR,SPACES                                                     
         BNH   M3D                                                              
         L     R5,ADCLT            YES-GET ITS BINARY CODE                      
         LA    R1,CLIST-CLTHDR(R5)                                              
         LA    R0,220                                                           
*                                                                               
M3C      CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   QPTNR,0(R1)                                                      
         BE    *+14                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,M3C                                                           
         DC    H'0'                                                             
         MVC   BPRD2,3(R1)                                                      
         MVC   PRD2,QPTNR                                                       
         MVC   PRDNM2(9),=C'*UNKNOWN*'                                          
         XC    KEY,KEY             READ PRODUCT RECORD                          
         MVC   KEY+1(3),1(R5)                                                   
         MVC   KEY+4(3),PRD2                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEY                                                      
         BNE   M3D                                                              
*                                                                               
         L     R5,ADPRD                                                         
         ST    R5,AREC                                                          
         GOTO1 GET                                                              
         USING PRDHDRD,R5                                                       
         MVC   PRDNM2(L'PNAME),PNAME   EXTRACT PARTNER NAME                     
         DROP  R5                                                               
         SPACE 1                                                                
*==========================================================                     
* FIX DEMO OPTIONS FOR D7/DX                                                    
*==========================================================                     
         SPACE 1                                                                
M3D      CLC   QPROG,=C'D7'                                                     
         BNE   M3E                                                              
         CLI   Q2FAXDEM,C'Y'                                                    
         BE    M3G1                                                             
         B     M4                                                               
*                                                                               
M3E      CLC   QPROG,=C'DX'                                                     
         BNE   M4                                                               
         MVI   PRDEMCNT,0                                                       
         MVI   DETOPTS+1,0                                                      
         CLI   Q2FAXDEM,C'N'       SUPPRESS DEMOS                               
         BE    M4                                                               
         CLI   Q2FAXDEM,C' '       TEST USE DEFAULT                             
         BNE   M3F                 NO                                           
         CLI   PROGPROF,0          TEST DEFAULT TO NO DEMOS                     
         BE    M4                  YES - SKIP                                   
*                                                                               
M3F      MVC   PRDEMCNT,PROGPROF   SET DEFAULT VALUE                            
         CLI   Q2FAXDEM,C'0'                                                    
         BNH   *+10                                                             
         MVC   PRDEMCNT,Q2FAXDEM   OR OVERRIDE VALUE                            
         NI    PRDEMCNT,X'0F'                                                   
         MVI   Q2FAXDEM,C'Y'       SET TO PRINT DEMOS                           
*                                                                               
M3G      CLI   PRDEMCNT,0          MAKE SURE DEMO COUNT VALID                   
         BH    *+8                                                              
         MVI   PRDEMCNT,1          DEFAULT TO 1                                 
*                                                                               
         CLI   PRDEMCNT,4                                                       
         BNH   *+8                                                              
M3G1     MVI   PRDEMCNT,1                                                       
*                                                                               
M3H      MVI   Q2FAXDEM,C'Y'       SET TO INDICATE DEMOS PRINTING               
         MVI   DETOPTS+1,1         SET THIS SWITCH TOO !                        
         EJECT                                                                  
M4       CLI   MODE,ESTFRST                                                     
         BNE   M5                                                               
         L     RE,=A(DAREIO)       CLEAR ORDER I/O AREA                         
         XC    0(256,RE),0(RE)                                                  
         OI    DXSW,DXNEWSTA       PRESET IN CASE NO STAFRST                    
         CLI   PWADJCR,C'Y'        IF ADJ/CR PRESENT                            
         BNE   *+10                                                             
         CLC   QPROG,=C'D2'        HEADER PAGE FOR D2 REPORTS                   
         BNE   M4ADJCRX                                                         
         LA    R0,32                                                            
M4ADJCR  MVI   P,C'*'                                                           
         MVC   P+1(131),P                                                       
         MVC   P+48(34),=C'ESTIMATE INCLUDES ADJUSTED CR/DR $'                  
         MVC   BYTE,RCSUBPRG                                                    
         MVI   RCSUBPRG,255        SUPPRESS ALL HEADLINES                       
         GOTO1 REPORT                                                           
         MVC   RCSUBPRG,BYTE                                                    
         BCT   R0,M4ADJCR                                                       
         SPACE 2                                                                
M4ADJCRX CLI   Q2NET,C'N'          THIS VALUE SHOULDN'T BE THERE !              
         BNE   *+8                                                              
         MVI   Q2NET,C' '                                                       
         CLI   Q2NET,C'B'          NEED A BILL FORMULA                          
         BNE   *+8                                                              
         MVI   RQGETBF,C'Y'                                                     
         CLI   Q2NET,C'Y'          REPORT AT NET                                
         BNE   *+8                                                              
         MVI   RQGETBF,C'X'        TO AVOID CONFUSION WITH NO                   
*                                                                               
         L     RE,ADEST            OUT OF WEEK ROTATOR START DAY                
         USING ESTHDR,RE                                                        
*                                                                               
         CLC   QPROG,=C'D2'                                                     
         BE    *+10                                                             
         CLC   QPROG,=C'D6'                                                     
         BNE   M4AB                                                             
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
         TM    Q2USER+16,X'01'      UDEF=E1?                                    
         BZ    M4AA                 NO                                          
         MVC   H7(L'CEU1),CEU1      YES - MOVE IN UDEF DESCRIPTION              
         MVC   H7+L'CEU1+2(L'EUSER1),EUSER1                                     
*                                                                               
M4AA     TM    Q2USER+16,X'02'      UDEF=E2?                                    
         BZ    M4AB                 NO                                          
         MVC   H8(L'CEU2),CEU2      YES - MOVE IN UDEF DESCRIPTION              
         MVC   H8+L'CEU2+2(L'EUSER2),EUSER2                                     
         DROP  RF                                                               
*                                                                               
M4AB     MVC   PWPREFIX,SPACES                                                  
         MVI   PWPRESW,C'N'                                                     
         CLC   QAGY,=C'WI'         ALWAYS FOR DO FOR REG WIM                    
         BE    *+14                                                             
         CLC   QAGY,=C'WJ'                                                      
         BNE   *+8                                                              
         MVI   PWPRESW,C'Y'                                                     
*                                                                               
         OC    ECOST2,ECOST2                                                    
         BNZ   *+14                                                             
         OC    EPWPCT,EPWPCT       TEST PW CLIENT                               
         BZ    M4A                                                              
*                                                                               
         MVI   PWPRESW,C'Y'                                                     
         MVC   PWPREFIX,=C' IM  '  THERE IS A LEADING SPACE HERE !              
         CLC   QAGY,=C'WI'                                                      
         BE    *+10                                                             
         MVC   PWPREFIX,=C' AGY'                                                
         CLI   QPWCV,C'Y'                                                       
         BNE   M4A                                                              
         MVC   PWPREFIX,=C'CLT  '                                               
M4A      CLI   RQGETBF,C'Y'                                                     
         BNE   M4B                                                              
         GOTO1 GETBF,DMCB,(BPRD,WORK),SVBFORM                                   
*                                                                               
M4B      L     RE,ADEST                                                         
         CLI   EOWSDAY,0           ONLY IF THERE IS ONE INPUT                   
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         DROP  RE                                                               
*                                                                               
         XC    SVSTAT,SVSTAT       CLEAR SAVED STATION                          
         XC    LASTBIG,LASTBIG                                                  
         MVI   ESTACT,1                                                         
         MVI   PASS,0                                                           
         BAS   R9,GOTOSUB                                                       
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVC   BRDPOLSW,CPROF                                                   
         DROP  R6                                                               
         SPACE 2                                                                
         BRAS  RE,EFRSTC                                                        
*                                                                               
         CLI   QOPT5,C' '          SPILL REPORTING OPTION                       
         BE    *+14                                                             
         MVC   PROGPROF+15(1),QOPT5                                             
         NI    PROGPROF+15,X'0F'                                                
*                                                                               
         CLI   PROGPROF+15,C'0'                                                 
         BE    *+10                                                             
         MVC   SPOTPROF+5(1),PROGPROF+15                                        
*                                                                               
         CLI   SPOTPROF+1,0                                                     
         BNE   *+8                                                              
         MVI   SPOTPROF+1,C'N'                                                  
*                                                                               
         CLI   SPOTPROF+5,20                                                    
         BL    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
*                                                                               
         CLI   SVQOPT1,C'A'        SPECIAL REPORTS                              
         BE    *+12                                                             
         CLI   SVQOPT1,C'B'                                                     
         BNE   M5A                                                              
         MVI   SPOTPROF+5,0                                                     
         MVI   PROGPROF+9,C'N'                                                  
         B     M5A                                                              
*                                                                               
M5       CLI   MODE,PRDFRST                                                     
         BNE   PROCB                                                            
         MVI   PASS,0                                                           
         MVI   ESTACT,0            RESET ESTIMATE ACTIVE SWITCH                 
         BAS   R9,GOTOSUB                                                       
*                                                                               
M5A      XC    DNAME1(28),DNAME1                                                
         MVI   DEMINDX,0                                                        
         GOTO1 =A(SETPTDEM)        GET PTBUFF DEMO ADDRESS IN RF                
         LR    R6,RF               SAVE A(DEMLIST)                              
*                                                                               
         SR    R9,R9                                                            
         IC    R9,PRDEMCNT         VARIABLE DEMO MAXIMUM                        
*                                                                               
         LA    R8,DBLKAREA                                                      
         USING DBLOCK,R8                                                        
         XC    DBLKAREA,DBLKAREA                                                
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'           SEND TO RADIO TABLE                          
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         L     RF,ADAGY            TEST CANADIAN AGENCY                         
         CLI   AGYPROF+7-AGYHDR(RF),C'C'                                        
         BNE   M5B                                                              
         L     RF,ADCLT                                                         
         CLI   CEXTRA+0-CLTHDR(RF),C'U' TEST U.S. DEMOS                         
         BE    M5B                                                              
         MVI   DBSELMED,C'C'       ELSE SET TO CANAD                            
         DROP  R8                                                               
*                                                                               
M5B      CLI   MODE,ESTFRST        NEED ESTHDR TO DO THIS!                      
         JL    EXIT                                                             
*                                                                               
         L     R8,ADEST            SET FOR USER NAMES                           
         AHI   R8,EUSRNMS-ESTHDR                                                
         ST    R8,DMCB+12                                                       
*                                                                               
         L     R8,VNONTNMS                                                      
         ST    R8,DMCB+16                                                       
*                                                                               
         GOTO1 DEMOCON,DMCB,((R9),(R6)),(2,DNAMES),(C'S',DBLKAREA),,,0          
         B     EXIT                                                             
         EJECT                                                                  
PROCB    CLI   MODE,PROCBUY                                                     
         BNE   M8                                                               
*                                                                               
         CLI   CALLMGBY,C'Y'                                                    
         BNE   PB1                                                              
         L     R8,ADBUY                                                         
         LLC   R0,0(R8)                                                         
         MVI   0(R8),0             INDICATE NO BUYREC PRESENT                   
         GOTO1 MEDGETBY,DMCB,(RA),0                                             
         STC   R0,0(R8)            RESTORE AGY/MD                               
         MVI   CALLMGBY,C'N'                                                    
*                                                                               
PB1      CLI   QBYID,C'Y'                                                       
         BNE   PB2                                                              
         CLC   SAVEID,BUYID                                                     
         BE    PB2                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   SAVEID,BUYID                                                     
*                                                                               
PB2      L     RF,=A(FIXCOVRD)                                                  
         BASR  RE,RF                                                            
                                                                                
* THIS CODE NEEDED TO SET UP HEADENDS WHICH DON'T ALWAYS                        
* GET A STATION BREAK                                                           
         CLC   BIGSTA,SVBIGSTA                                                  
         BE    PB10                                                             
*                                                                               
         MVC   BYTE,FORCEHED       MAINTAIN CURRRENT SETTING                    
         BRAS  RE,GETSADDR                                                      
         MVC   FORCEHED,BYTE                                                    
         CLI   SVBUYKEY+6,X'E8'    TEST CABLE                                   
         BNL   PB10                NO                                           
         L     RE,=A(MGTABST)      FOR CABLE, CLEAR AT CBHLAST                  
         L     RF,=A(MGTABSTX-MGTABST)                                          
         XCEF                                                                   
*                                                                               
PB10     CLI   SPDWSW,C'Y'         FOR PW CLIENT VERSION                        
         BNE   PB20                                                             
         CLI   D2APROF,C'N'        TEST TO SUPPRESS MG CAPTIONS                 
         BE    PB30                SUPPRESS=NO MEANS PRINT                      
         B     PB50                                                             
*                                                                               
PB20     CLC   QAGY,=C'WI'         ALWAYS FOR DO FOR REG WIM                    
         BE    PB30                                                             
         CLC   QAGY,=C'WJ'                                                      
         BE    PB30                                                             
         CLC   QPROG,=C'D4'        FOR SALESPERSON TIMESHEET                    
         BE    PB50                                                             
*                                                                               
PB30     L     R5,=A(MGABLK)                                                    
         USING MGABLKD,R5                                                       
         XC    0(MGALNQ,R5),0(R5)                                               
         MVI   MGAACT,MGAQBLN                                                   
         MVC   MGAACOM,ACOMFACS                                                 
         MVC   MGABUY,ADBUY                                                     
         MVC   MGGETBUY,VGETBUY    MOVES MG1OR2 AND VGETBUY                     
*                                                                               
         L     R0,=A(MGTABLE)                                                   
         ST    R0,MGATAB                                                        
         L     R1,=A(MGTABLEX-MGTABLE)                                          
         ST    R1,MGATABLN                                                      
         OI    MGAOPT,MGOFULN      SET FLAG FOR FULLWORD LEN                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   MGAAGMD,SVAGYMD                                                  
         MVC   MGACLT,SVCLT                                                     
         MVC   MGAPRD,SVPRDCD                                                   
         MVC   MGASTA(2),SVMKT                                                  
         MVC   MGASTA+2(3),SVSTA                                                
         MVC   MGAEST,SVEST                                                     
         L     RF,PRDBUFF                                                       
         LLC   RE,SVPRDCD                                                       
         CLI   SVPRDCD,X'FF'                                                    
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         LA    RF,DEMDISP(RE,RF)                                                
         ST    RF,MGADEM                                                        
         ST    RF,MGABRDEM                                                      
         OI    MGAOPT,MGONEDIT                                                  
         OI    MGAOPT,MGONONC                                                   
         TM    RQOPTS,RQOPTS_2DEC                                               
         JZ    *+8                                                              
         OI    MGAOPT2,MGAOPT2_2DEC                                             
         TM    RQOPTS,RQOPTS_2DECIMP                                            
         JZ    *+8                                                              
         OI    MGAOPT2,MGAOPT2_2DECIMP                                          
*                                                                               
         CLI   UNWOPTS,0              TEST UNWIND ACTIVE                        
         BE    PB32                   NO                                        
         GOTO1 MEDGETBY,DMCB,(RA),0   UNWIND BUY DATA BEFORE BLDMGA             
*                                                                               
PB32     L     RE,MGABUY                                                        
         SR    R0,R0                                                            
         ICM   R0,3,BUYKBUY-BUYREC(RE)       GET LINE NUMBER                    
         TM    BUYRCNTL-BUYREC(RE),BUYRLN2   TEST 2-BYTE LINE NUMBER            
         BO    PB34                                                             
         LR    RF,R0                                                            
         SRL   RF,8                                                             
         STCM  RF,3,BUYKBUY-BUYREC(RE)    ALWAYS SET 2-BYTE LINE NUMBER         
PB34     MVI   MG1OR2,2                   AND TELL MGABLD                       
*                                                                               
         GOTO1 VBLDMGN,MGABLKD                                                  
*                                                                               
         L     RE,MGABUY                                                        
         STCM  R0,3,BUYKBUY-BUYREC(RE)    AND RESTORE ORIGINAL LINE NUM         
         CLI   MGAERR,0            IGNORE FOR NOW                               
         BE    *+4                                                              
         CLI   QMGA,C'Y'                                                        
         BNE   PB50                                                             
         L     RE,MGATAB           SAVE IN STATION TABLE                        
         USING MGENTRYD,RE                                                      
         OC    0(L'MGECODE,RE),0(RE)                                            
         BZ    PB50                                                             
         L     RF,=A(MGTABST)                                                   
PB40     OC    0(4,RF),0(RF)                                                    
         BZ    *+12                                                             
         LA    RF,MGERECL(RF)                                                   
         B     PB40                                                             
         CLI   MGETYPE,X'FE'                                                    
         BE    PB42                                                             
         CLI   PASS,0              ONLY SAVE ON PASS ZERO                       
         BH    PB42                                                             
*                                                                               
         L     R1,ADBUY                                                         
         USING BUYREC,R1                                                        
         MVC   MGEUSER(1),BUYKEST                                               
         MVC   0(MGERECL,RF),0(RE)                                              
PB42     LA    RE,MGERECL(RE)                                                   
         OC    0(L'MGECODE,RE),0(RE)                                            
         BNZ   PB40                                                             
         DROP  RE                                                               
         DROP  R1                                                               
*                                                                               
* TEST CONFIRMED ONLY                                                           
PB50     L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         CLC   QPROG,=C'CP'        TEST DRAFT ORDER                             
         BNE   PB60                                                             
         CLC   Q2USER+5(3),SPACES  AND BWS BUYER/CAMPAIGN FILTER                
         BNH   PB60                                                             
         LA    R1,BDELEM           YES-ONLY ACCEPT THIS BUY IF BUYER            
         SR    R0,R0                   AND CAMPAIGN MATCH THOSE IN THE          
*                                      BWS TRANSFER ELEMENT                     
PB52     CLI   0(R1),0                                                          
         BE    EXIT                                                             
         CLI   0(R1),BWSCODEQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     PB52                                                             
         USING BWSELEM,R1                                                       
         CLC   BWSBYR,Q2USER+5                                                  
         BNE   EXIT                                                             
         PACK  DUB,Q2USER+8(5)                                                  
         CVB   RE,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,BWSCAM                                                      
         CR    RE,RF                                                            
         BNE   EXIT                                                             
         DROP  R1                                                               
*                                                                               
PB60     XC    CFDS(2),CFDS                                                     
         TM    BDCFD,1                                                          
         BZ    *+8                                                              
         MVI   CFDE,C')'                                                        
         TM    BDCFD,2                                                          
         BZ    *+8                                                              
         MVI   CFDS,C'('                                                        
*                                                                               
         CLI   QPRGTYPE,C' '       PROGRAM TYPE FILTER                          
         BE    *+14                                                             
         CLC   BDPROGT,QPRGTYPE                                                 
         BNE   EXIT                                                             
*                                                                               
         CLC   QPROG,=C'DX'                                                     
         BNE   PB68                                                             
*                                                                               
         CLI   DRCSHTRD,C'R'       TEST TRADE SIDE OF CASH/TRD                  
         BNE   PB62                                                             
         BRAS  RE,CHKTRD           COMPARE TO REP                               
         BNE   EXIT                IF NO MATCH IT'S CASH SO EXIT                
         B     PB68                                                             
*                                                                               
CHKTRD   ST    RE,FULL                                                          
         CLI   DARPROF+14,C'Y'     MULTIPLE TRADE CODES?                        
         BE    CHKTRD10                                                         
         CLC   BDREP,DRTRDDTA      NO, REP CODE MUST MATCH                      
         L     RE,FULL                                                          
         BR    RE                                                               
CHKTRD10 GOTO1 VRCPACK,DMCB,(C'U',BDREP),DRTRDDTA+5                             
         CLC   DRTRDDTA+2(2),DRTRDDTA+5   DO THEY MATCH FOR 2 DIGITS?           
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
PB62     CLI   DRCSHTRD,C'R'-X'40' TEST CASH SIDE OF CASH TRD                   
         BNE   PB68                                                             
         BRAS  RE,CHKTRD           COMPARE TO SPECIAL REP                       
         BE    EXIT                IF MATCH IT'S TRADE, SO EXIT                 
*                                                                               
PB68     LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         MVC   SVSPREP,BDREP       SAVE SPECIAL REP                             
*                                                                               
         CLI   PROFSPR,0                                                        
         BE    *+12                                                             
         CLI   PROFSPR,C'N'                                                     
         BNE   *+10                                                             
         XC    SVSPREP,SVSPREP                                                  
         OC    SVSPREP,SVSPREP     HAVE SAVED SPECIAL REP?                      
         BZ    PB70                NO                                           
         XC    SVSPREPN,SVSPREPN   CLEAR REP NAME                               
         CLI   PROFSPR,C'B'        PRINT REP NAME?                              
         BNE   PB70                NO - DON'T LOOK UP REP NAME                  
         DROP  RE                                                               
*                                                                               
         GOTO1 VRCPACK,DMCB,(C'U',SVSPREP),WORK                                 
         OC    WORK(3),WORK                                                     
         BZ    PB70                                                             
         MVI   REPFLAG,C'Y'                                                     
         GOTO1 VGETREP                                                          
*                                                                               
         SPACE 2                                                                
PB70     CLI   QOPT3,C'Y'          SUPPRESS CONFIRMED                           
         BNE   PB72                                                             
         TM    BDCFD,X'03'                                                      
         BZ    *+8                                                              
         B     EXIT                                                             
PB72     DS    0H                                                               
         BAS   R9,GOTOSUB                                                       
         LA    RE,COMAREA                                                       
         LA    RF,400                                                           
         XCEF                                                                   
         XC    PSLIST,PSLIST                                                    
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         CLI   BPRD,X'FF'                                                       
         BNE   *+10                                                             
         MVC   PSLIST,=X'FF000000' SET FOR POL                                  
         LA    RE,PSLIST                                                        
PB74     ST    RE,SVPSLIST                                                      
         CLC   0(1,RE),BPRD                                                     
         BNE   PB150                                                            
         XC    HIATAB,HIATAB                                                    
         XC    PREMTAB,PREMTAB                                                  
         BRAS  RE,EXTRCT                                                        
*                                                                               
         CLI   QOPT1,C'N'          SUPRESS DETAILS                              
         BNE   *+12                                                             
         MVI   BUYACT,1            TURN ON ACTIIVTY                             
         B     PB150                                                            
*                                                                               
         CLI   MEDSPILL,C'Y'                                                    
         BNE   PB90                                                             
         L     RE,=A(MGTABST)      NO MGA FOR SPILL                             
         L     RF,=A(MGTABSTX-MGTABST)                                          
         XCEF                                                                   
         LA    R5,MEDPERD          TEST IF VALID SPILL                          
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD                                                
         BZ    PB150                                                            
         CLI   SPOTPROF+5,0                                                     
         BE    EXIT                                                             
         MVI   BUYACT,1            DOING SPILL SUMMARY SO ACTIVATE              
         CLI   SPLPRINT,1        SAVE SPILL IF SPILL PRINT ACTIVE               
         BNE   PB80                                                             
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'01',SPBUFSTA),0                        
         L     RE,=A(SPBUFMKT)                                                  
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'01',(RE)),0                            
         MVI   SPLPRINT,2                                                       
PB80     CLI   SPOTPROF+5,3        PRINT SPILL IF REQUESTED                     
         BE    *+8                                                              
         CLI   SPOTPROF+5,2                                                     
         BNE   EXIT                                                             
         CLI   SPLPRINT,2          PRINT SPILL CAPTION FIRST TIME               
         BNE   PB90                                                             
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFSTA),SPSTPL                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   SVSTLINE+43(17),=C'***SPILLS FROM   '                            
         MVC   SVSTLINE+43(L'SP@SPLFR),SP@SPLFR                                 
         DROP  RE                                                               
         MVC   SVSTLINE+60(24),SPSTPL+17                                        
         GOTO1 SQUASHER,DMCB,SVSTLINE,132                                       
PB90     XC    OVRFLAG(4),OVRFLAG                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    PREMTAB,PREMTAB                                                  
         BNZ   *+14                                                             
         OC    MEDBYD(12),MEDBYD                                                
         BZ    PB140                                                            
*                                                                               
         CLI   NEWMKT,C'Y'         TEST MARKET FIRST                            
         BNE   PB100                                                            
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVI   NEWMKT,C'N'         TURN OFF MARKET FIRST SWITCH                 
         CLC   QPROG,=C'CP'        DRAFT ORDER IS PASSED GOOD REQUESTOR         
         BE    *+18                NAMES                                        
         CLI   RQGETNAM,C'Y'       TEST BUYERS' NAMES NEEDED                    
         BNE   *+10                                                             
         MVC   QUESTOR,SVBUYNAM    YES-OVERRIDE THE REQUESTOR NAME              
*                                                                               
PB100    TM    DXSW,DXNEWSTA       TEST STATION FIRST FOR DX REPORT             
         BZ    PB102                                                            
         GOTO1 =A(DXFAX)           YES - PRINT FAX (BEYOND BRAS)                
*                                                                               
PB102    CLI   QBOOK1,C' '                                                      
         BNE   PB106                                                            
         CLI   DETOPTS,1                                                        
         BNE   PB106                                                            
         L     RE,MEDADEMO                                                      
         L     R6,0(RE)                                                         
         USING NDELEM,R6                                                        
         LA    RF,NDEMNO           SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               SET END ADDRESS                              
         LA    RE,OVRFLAG                                                       
PB104    CR    RF,R6                                                            
         BNL   PB106                                                            
         TM    4(RF),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(RE),C'*'                                                       
         LA    RF,8(RF)                                                         
         LA    RE,1(RE)                                                         
         B     PB104                                                            
PB106    DS    0H                                                               
         DROP  R6                                                               
         MVI   BUYACT,1                                                         
         CLI   PASS,0                                                           
         BE    PB110                                                            
         CLI   FORCEHED,C'Y'                                                    
         BE    PB110                                                            
         CLI   FORCEMID,C'Y'                                                    
         BNE   PB110                                                            
*                                                                               
         MVC   P1,P2                                                            
         MVC   P2,P3                                                            
         XC    P3,P3                                                            
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,2                                                       
PB110    DS    0H                                                               
         DROP  R5                                                               
         CLI   QPROG,C'U'                                                       
         BNE   PB112                                                            
         GOTO1 VREVBUY,DMCB,(RA)                                                
*                                                                               
PB112    CLI   FORCEHED,C'Y'                                                    
         BE    PB120                                                            
         CLC   CURPDAT1(112),CURHDAT1                                           
         BE    PB120                                                            
*                                                                               
         LLC   RE,MAXLINES                                                      
         LLC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'12'                                                        
         BH    *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     PB120                                                            
*                                                                               
         L     RE,PSTAGRID                                                      
         MVC   0(56,RE),CURPDAT1                                                
         MVC   133(56,RE),CURPDAT2                                              
         GOTO1 REPORT                                                           
         MVC   CURHDAT1(112),CURPDAT1                                           
*                                                                               
PB120    MVI   SPLPRINT,0                                                       
         MVC   P2,SPACES                                                        
         GOTO1 =V(VMDBDESC),DMCB,(RA),SAVLINE                                   
         GOTO1 =A(GETCAP)                                                       
         L     R6,DSTAGRID         SET GRID ADDRESS                             
         BAS   R9,BGRID            BUILD DETAIL GRID                            
         BAS   RE,CSDEMCP          GET DEMOS                                    
         GOTO1 VEDTDEMS            EDIT DEMO/CPP                                
         BAS   R9,BTSPD            MOVE DEMOS/CPP TO PRINT LINE                 
         LA    R5,SAVLINE                                                       
         L     RF,DDESC            MOVE IN DESCRIPTION                          
         BASR  R9,RF                                                            
*                                                                               
         CLC   QPROG,=C'D7'        DOUBLE SPACE D7 REPORT                       
         BE    *+14                                                             
         CLC   QPROG,=C'DX'                                                     
         BNE   *+8                                                              
         MVI   P2,0                                                             
*                                                                               
         CLC   QPROG,=C'CP'        DOUBLE SPACE DRAFT ORDER REPORT              
         BNE   PB130                                                            
         MVI   P3,0                                                             
         MVI   MAXLINES,60         DEFAULT IS 60 LPP                            
         CLC   Q2USER+13(2),=C'45' TEST REQUEST CARD HAS OVERRIDE               
         BL    PB130                                                            
         PACK  DUB,Q2USER+13(2)    YES-USE IT                                   
         CVB   RE,DUB                                                           
         STC   RE,MAXLINES                                                      
*                                                                               
PB130    CLI   SPACESW,0                                                        
         BE    *+8                                                              
         MVI   P3,0                                                             
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,2                                                       
         B     PB150                                                            
*                                                                               
PB140    OC    HIATAB,HIATAB                                                    
         BNZ   PB106                                                            
         OC    PREMTAB,PREMTAB                                                  
         BNZ   PB106                                                            
         CLI   MGSW,0                                                           
         BNE   PB106                                                            
         B     PB150                                                            
*                                                                               
PB150    L     RE,SVPSLIST         ANOTHER SPOT LENGTH FOR PRODUCT              
         LA    RE,2(RE)                                                         
*                                                                               
PB152    CLI   0(RE),0                                                          
         BE    EXIT                                                             
         CLC   0(1,RE),BPRD                                                     
         BE    PB74                                                             
         LA    RE,2(RE)                                                         
         B     PB152                                                            
*                                                                               
         EJECT                                                                  
M8       CLI   MODE,STAFRST        STATION FIRST                                
         BNE   M10                                                              
*                                                                               
M9       CLI   PWADJCR,C'Y'        CLIENT VERSION AND ADJUSTMENTS               
         BNE   M8NOPWCR                                                         
         CLC   QPROG,=C'D3'        KILL THE D3 AND D4 REPORTS                   
         BE    *+10                                                             
         CLC   QPROG,=C'D4'                                                     
         BNE   M8NOPWCR                                                         
         MVI   P,C'*'                                                           
         MVC   P+1(131),P                                                       
         MVC   P+48(34),=C'ESTIMATE INCLUDES ADJUSTED CR/DR $'                  
         MVC   P2+56(19),=C'NO REPORT GENERATED'                                
         MVC   BYTE,RCSUBPRG                                                    
         MVI   RCSUBPRG,255        SUPPRESS ALL HEADLINES                       
         GOTO1 REPORT                                                           
         MVC   RCSUBPRG,BYTE                                                    
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
*                                                                               
M8NOPWCR CLC   QPROG,=C'DX'        TEST DX REPORT                               
         BNE   M9C                                                              
         CLI   BIGSTA+4,C'/'       TEST CABLE                                   
         BNE   M9A                                                              
         CLC   SVBIGSTA(4),BIGSTA  TEST SAME HEADEND                            
         BE    M9B                                                              
*                                                                               
M9A      OI    DXSW,DXNEWSTA       YES-INDICATE NEW STATION                     
*                                                                               
M9B      L     RE,VMASTC                                                        
         USING MASTD,RE                                                         
         OC    MCREMPQK,MCREMPQK   SOON?                                        
         BNZ   M9C                 NOT ZERO = SOON => SKIP CHKAUTH CALL         
         CLI   RCWRITE,C'Y'        IF CAN'T WRITE TO FILE, SPAUTH DIES          
         BNE   M9C                                                              
         DROP  RE                                                               
         L     RF,=A(CHKAUTH)                                                   
         BASR  RE,RF                                                            
         B     M9E                                                              
*                                                                               
M9C      CLI   BIGSTA+4,C'/'       TEST CABLE                                   
         BNE   M9D                                                              
         CLC   SVBIGSTA(4),BIGSTA  TEST SAME HEADEND                            
         BE    M9E                                                              
*                                                                               
M9D      MVI   PAGEONE,C'Y'        SET FOR ZENITH/OPTIMEDIA HEADLINES           
*                                                                               
M9E      CLC   QPROG,=C'CP'                                                     
         BNE   *+8                                                              
         MVI   RCSUBPRG,9                                                       
*                                                                               
         BAS   R9,GOTOSUB                                                       
         BRAS  RE,GETSADDR                                                      
* READ DARE ORDER IF DARE DX REQUEST                                            
         CLC   QPROG,=C'DX'        CHECK IT'S THE DX REPORT                     
         BNE   EXIT                                                             
         CLI   QOPT1,C'D'          TEST FROM DARE                               
         BNE   EXIT                                                             
         L     RF,=A(GETDRORD)                                                  
         BASR  RE,RF               BEYOND BRAS                                  
         B     EXIT                                                             
         EJECT                                                                  
M10      CLI   MODE,STALAST        STATION LAST                                 
         BNE   M12                                                              
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         LA    RE,MEDWEEKS         RESTORE D2 DATES                             
         LHI   RF,672              FOR HL PRINTING - DESTRYED BY GOSUB          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PASS                                                          
         MHI   RF,268                                                           
         LA    RF,PASSTAB(RF)                                                   
         USING PASSTABD,RF                                                      
         MVC   MEDNUMWK(L'PASSP1),PASSP1                                        
         MVC   MEDWEEKS(L'PASSP2),PASSP2                                        
         XC    MEDMON01(144),MEDMON01                                           
         MVC   MEDMON01(L'PASSP3),PASSP3                                        
         MVC   MEDPERD(L'PASSP4),PASSP4                                         
         DROP  RF                                                               
*                                                                               
         TM    RQOPTS,RQOPTS_HDEND TOTAL SYS CODE ONLY                          
         BZ    M10BIGOK                                                         
         TM    BIGSTA,X'F0'         TEST NUMERIC STATION (CABLE)                
         BNO   M10BIGOK                                                         
         MVC   BIGSTA+4(5),=C'    ' ZAP THE CALL LETTERS                        
*                                                                               
M10BIGOK GOTO1 VSTATOT                                                          
*                                                                               
         CLI   PASS,0                                                           
         BNE   *+8                                                              
         BRAS  RE,PRTSTCOM         PRINT STATION COMMENT                        
*                                                                               
         CLC   QPROG,=C'D4'        DON'T PRINT FOR WIM D4                       
         BE    M10MGA                                                           
         L     RF,=A(MGTABST)                                                   
         OC    0(40,RF),0(RF)                                                   
         BZ    M10MGA                                                           
         CLI   PASS,0                                                           
         BNE   M10MGA                                                           
         BRAS  RE,PRTMGA                                                        
*                                                                               
M10MGA   CLI   PASS,0                                                           
         BNE   *+10                                                             
         MVC   SVSTLINE,SPACES                                                  
         BAS   R9,GOTOSUB                                                       
*                                                                               
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
*                                                                               
         CLC   QPROG,=C'CP'        ALWAY PAGE BREAK FOR CP AND D7               
         BE    M10CP                                                            
         CLC   QPROG,=C'D7'                                                     
         BE    M10CP                                                            
*                                                                               
         CLC   QPROG,=C'DX'        TEST PROFILE IF DX AND                       
         BNE   M10D7X              NO, DON'T PAGE BREAK                         
         CLI   BIGSTA+4,C'/'       YES, CABLE STATION                           
         BNE   M10CP               NO, THEN PAGE BREAK                          
         CLI   PROFPAG,C'Y'        TEST PAGE BREAK                              
         BNE   M10D7X              NO                                           
*                                                                               
M10CP    MVC   MID1,SPACES                                                      
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         MVC   MID2,MID1                                                        
         MVI   FORCEMID,C'N'                                                    
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
M10D7X   CLI   PASS,0                                                           
         BNE   EXIT                                                             
         CLI   PROFMSR,C'S'                                                     
         BE    M14A                                                             
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
M12      CLI   MODE,MKTFRST                                                     
         BNE   M14                                                              
         XC    SVBIGSTA,SVBIGSTA                                                
         MVI   SPILORIG,0                                                       
         MVI   BUYACT,0                                                         
         MVI   NEWMKT,C'Y'                                                      
         CLI   QPWCV,C'Y'          SET UP FOR SPECIAL WESTERN REPORT            
         BNE   M12A                                                             
         GOTO1 =A(GETPW)                                                        
         MVC   KEY,SVBUYKEY                                                     
         GOTO1 HIGH                                                             
*                                                                               
M12A     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         L     RE,=A(SPBUFMKT)                                                  
         LA    RF,500                                                           
         XCEF                                                                   
         BAS   R9,GOTOSUB                                                       
         B     EXIT                                                             
         SPACE 2                                                                
M14      CLI   MODE,MKTLAST                                                     
         BNE   M16                                                              
         XC    SAVEID,SAVEID                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   FORCEMID,C'N'                                                    
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         CLI   PROFMSR,C'M'                                                     
         BNE   M14A1                                                            
         CLI   PROFPAG,C'Y'                                                     
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
M14A     MVI   BUFCDE,X'90'                                                     
         MVI   LEVEL,1                                                          
         MVC   MCOUNT,=F'1'                                                     
         MVI   NOSUM,0                                                          
         CLI   BUYACT,1                                                         
         BNE   M15                                                              
         TM    SPILORIG,X'02'      TEST FOR ANY SPILL                           
         BZ    M14ANOSP                                                         
         MVI   BUFCDE,X'88'        SPILL                                        
         BRAS  RE,MLASTC           PRINT OUT MARKET REPORT                      
M14ANOSP DS    0C                                                               
         TM    SPILORIG,X'01'      TEST FOR ANY ORIGINATING                     
         BNZ   M14AA                                                            
         MVI   BUFCDE,X'89'        ORIGINATING                                  
         MVC   SVNOSUM,NOSUM       SAVE OFF SUPPRESS SUMMARY SWITCH             
         MVI   NOSUM,1             DON'T PRINT SUMMARY - ALREADY DID IT         
         BRAS  RE,MLASTC           JUST CLEAR THE X'89' BUFFALO REC!            
         MVC   NOSUM,SVNOSUM       RESTORE THE SUMMARY SWITCH                   
         B     M14ANOOR                                                         
*                                                                               
M14AA    MVI   BUFCDE,X'89'        ORIGINATING                                  
         BRAS  RE,MLASTC           PRINT OUT MARKET REPORT                      
*                                                                               
M14ANOOR DS    0C                                                               
         LA    RE,PROGPROF                                                      
         CLI   PROFDPT,C'Y'        DON'T HAVE DAYPARTS IN SPILL TOTS            
         BE    M14DPTY             SO FORCE THE SUMMARY TO PRINT                
         CLI   MODE,MKTLAST                                                     
         BL    *+16                                                             
         TM    SPILORIG,3          SUPRESS DUP TOTAL IF MISSING                 
         BNM   *+8                  EITHER SPILL OR ORIGNIATING                 
         MVI   NOSUM,1                                                          
M14DPTY  MVI   BUFCDE,X'90'        TOTAL SUMMARY                                
         BRAS  RE,MLASTC           PRINT OUT MARKET REPORT                      
         MVI   NOSUM,0                                                          
         MVI   BUFCDE,X'96'        SPOT LENGTH SUMMARY                          
         BRAS  RE,MLASTC           PRINT OUT MARKET REPORT                      
         MVI   BUFCDE,X'97'        DAYPART SUMMARY                              
         BRAS  RE,MLASTC           PRINT OUT MARKET REPORT                      
         DROP  RE                                                               
*                                                                               
         CLI   MODE,MKTLAST        CAN ALSO BE STATION                          
         BNE   M14PWST                                                          
         L     RE,=A(PWDLTAB)      CLEAR PW CLIENT OVERRIDES                    
         XC    0(16,RE),0(RE)                                                   
         L     RE,=A(PWADJTAB)     AND PW BILL ADJ                              
         XC    0(16,RE),0(RE)                                                   
M14PWST  DS    0H                                                               
*                                                                               
         L     RE,=A(SPBUFMKT)                                                  
         OC    0(10,RE),0(RE)      PRINT ORIGINATING MARKETS                    
         BZ    M14A1               IF THERE WAS ANY SPILL                       
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P1(12),=C'****SPILL****'                                         
         MVC   P1(L'SP@SPILL),SP@SPILL                                          
         DROP  RE                                                               
         L     RE,=A(SPBUFMKT)                                                  
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',(RE)),P1+13                        
         GOTO1 REPORT                                                           
*                                                                               
M14A1    CLI   MODE,MKTLAST                                                     
         BNE   M15                                                              
         GOTO1 =A(PRTMCOM)         PRINT MEDIA COMMENTS                         
*                                                                               
M14B     BAS   R9,GOTOSUB                                                       
*                                                                               
M15      MVI   PASS,0                                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   BUYACT,1            ANY ACTIVITY                                 
         BNE   EXIT                                                             
         CLC   QUESTOR(10),=C'COORS-TAPE'  UGH! ANOTHER SPECIAL OPTION          
         BNE   EXIT                                                             
         MVC   BYTE,RCSUBPRG       SAVE HEADLINE CONTROL                        
         MVI   RCSUBPRG,255        SUPPRESS HEADLINES                           
         MVI   P,0                 FORCE NEW PAGE                               
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'       RESET TO WHERE WE WERE                       
         MVC   RCSUBPRG,BYTE                                                    
         B     EXIT                                                             
         EJECT                                                                  
M16      CLI   MODE,ESTLAST                                                     
         BNE   M17                                                              
         CLC   QPROG,=C'DX'                                                     
         BNE   M17                                                              
         CLC   =C'ALL',QSTA        CAN'T DO THIS IF NO STATION                  
         BE    M17                                                              
         TM    DXSW,DXNEWSTA       IF FLAG IS OFF, PRINTED SOMETHING            
         BZ    M17                                                              
* DX WITH NO DATA PRINTS NEEDS TO PRINT SOMETHING                               
         MVI   MODE,STALAST        FAKE MODE TO MAKE STATION PRINT              
         MVC   DUB(5),QSTA                                                      
         MVC   DUB+5(3),QCBLNET                                                 
         GOTO1 MSPACK,DMCB,QMKT,DUB,BMKT  NEED TO READ DARE ORDER               
         MVC   MKT,QMKT                                                         
         MVC   MKTNM,SPACES                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         CLI   QMKT,C'0'                                                        
         BL    M16A                                                             
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),QAGY                                                    
         MVC   KEY+8(7),=C'0000000'                                             
         GOTO1 READMKT                                                          
         L     RE,ADMARKET                                                      
         MVC   MKTNM,MKTNAME-MKTREC(RE)                                         
*                                                                               
M16A     MVC   STA,QSTA            SET THIS MUCH UP AT LEAST                    
         MVC   STAPRINT,SPACES                                                  
         MVC   STAPRINT(4),QSTA                                                 
         MVI   STAPRINT+4,C'-'                                                  
         MVC   STAPRINT+5(2),=C'TV'                                             
         CLI   QSTA+4,C'T'                                                      
         BE    M16B                                                             
         MVC   STAPRINT+5(2),=C'DV'                                             
         CLI   QSTA+4,C'D'                                                      
         BE    M16B                                                             
         MVC   STAPRINT+5(2),=C'AM'                                             
         CLI   QSTA+4,C'A'                                                      
         BE    M16B                                                             
         MVC   STAPRINT+5(2),=C'FM'                                             
         CLI   QSTA+4,C'F'                                                      
         BE    M16B                                                             
         MVC   STAPRINT+5(2),=C'SM'                                             
         CLI   QSTA+4,C'S'                                                      
         BE    M16B                                                             
         MVC   STAPRINT+5(2),=C'CM'                                             
         CLI   QSTA+4,C'C'                                                      
         BE    M16B                                                             
         MVC   STAPRINT+4(3),SPACES                                             
*                                                                               
M16B     MVC   BIGSTA(7),STAPRINT  UPDATE BIGSTA WITH CORRECT STATION           
         MVC   BIGSTA+7(2),SPACES  AND TRAILING 2 SPACES                        
*                                                                               
* DON'T KNOW IF YOU NEED THIS SINCE YOU CAN'T GET HERE IF NOT DX                
         CLC   QPROG,=C'DX'        CHECK IT'S THE DX REPORT                     
         BNE   EXIT                                                             
* DON'T KNOW IF YOU NEED THIS SINCE YOU CAN'T GET HERE IF NOT DX                
*                                                                               
         CLI   QOPT1,C'D'          TEST FROM DARE                               
         BNE   EXIT                                                             
         L     RF,=A(GETDRORD)                                                  
         BASR  RE,RF               BEYOND BRAS                                  
*                                                                               
         OI    DXSW,DXNEWSTA                                                    
         BRAS  RE,DXFAX                                                         
         MVC   P(7),=C'STATION'                                                 
         MVC   P+8(7),STAPRINT                                                  
         CLI   STAPRINT,C'0'       TEST CABLE                                   
         BL    M16C                                                             
         MVC   P+8(5),QSTA                                                      
         MVC   P+13(3),QCBLNET                                                  
M16C     MVC   P2(29),=C'** NO BUYS ON THIS STATION **'                         
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
M17      CLI   MODE,PROCGOAL                                                    
         BNE   M18                                                              
         CLI   BUYACT,1                                                         
         BNE   EXIT                                                             
         BAS   R9,GOTOSUB                                                       
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         LA    RE,MEDWEEKS         RESTORE D2 DATES                             
         LA    RF,672              FOR HL PRINTING - DESTRYED BY GOSUB          
         XCEF                                                                   
         SR    RF,RF                                                            
         IC    RF,PASS                                                          
         MH    RF,=H'268'                                                       
         LA    RF,PASSTAB(RF)                                                   
         USING PASSTABD,RF                                                      
         MVC   MEDNUMWK(L'PASSP1),PASSP1                                        
         MVC   MEDWEEKS(L'PASSP2),PASSP2                                        
         XC    MEDMON01(144),MEDMON01                                           
         MVC   MEDMON01(L'PASSP3),PASSP3                                        
         MVC   MEDPERD(L'PASSP4),PASSP4                                         
         DROP  RF                                                               
         BRAS  RE,GETGL                                                         
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
M18      CLI   MODE,PRDLAST                                                     
         BNE   M20                                                              
         MVI   LEVEL,2                                                          
         B     MXXSUB                                                           
*                                                                               
M20      CLI   MODE,MGR1LAST                                                    
         BNE   M22                                                              
         MVI   LEVEL,3                                                          
         B     MXXSUB                                                           
*                                                                               
M22      CLI   MODE,MGR2LAST                                                    
         BNE   M24                                                              
         MVI   LEVEL,4                                                          
         B     MXXSUB                                                           
*                                                                               
M24      CLI   MODE,MGR3LAST                                                    
         BNE   M26                                                              
         MVI   LEVEL,5                                                          
         B     MXXSUB                                                           
         SPACE 2                                                                
M26      CLI   MODE,CLTLAST                                                     
         BNE   M28                                                              
         CLC   QPRD,=C'ALL'                                                     
         BNE   EXIT                                                             
         MVI   BUFCDE,X'93'                                                     
         MVI   LEVEL,2                                                          
         BAS   RE,DOSUM                                                         
         B     M34                                                              
*                                                                               
M28      CLI   MODE,PGR1LAST                                                    
         BNE   M30                                                              
         MVI   BUFCDE,X'93'                                                     
         MVI   LEVEL,3                                                          
         BAS   RE,DOSUM                                                         
         B     M34                                                              
*                                                                               
M30      CLI   MODE,PGR2LAST                                                    
         BNE   M32                                                              
         MVI   BUFCDE,X'93'                                                     
         MVI   LEVEL,4                                                          
         BAS   RE,DOSUM                                                         
         B     M34                                                              
M32      CLI   MODE,PGR3LAST                                                    
         BNE   M34                                                              
         MVI   BUFCDE,X'93'                                                     
         MVI   LEVEL,5                                                          
         BAS   RE,DOSUM                                                         
         B     M34                                                              
M34      CLI   MODE,REQLAST                                                     
         BNE   M36                                                              
         CLI   FOOT1,C' '                                                       
         BE    RESPRGM                                                          
         MVI   FORCEHED,C'N'                                                    
         MVI   P,0                                                              
         MVC   P2(132),FOOT1                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
RESPRGM  CLI   ESTACT,0            DO SUB-PROGRAM END IF ACTIVITY               
         BE    *+8                                                              
         BAS   R9,GOTOSUB                                                       
         GOTO1 MEDADDWT,DMCB,(RA)  CLEAR OUT WEIGHT ACCUMS ALWAYS               
         LA    R6,CURPH01                                                       
         LA    R9,3                                                             
RESPRGM1 L     RF,8(R6)            RESTORE PROGRAMS IN CALLOV AREA              
         L     RE,0(R6)                                                         
         L     R1,4(R6)                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    R6,12(R6)                                                        
         BCT   R9,RESPRGM1                                                      
         B     EXIT                                                             
         SPACE 2                                                                
M36      BAS   R9,GOTOSUB                                                       
         MVI   FORCEHED,C'Y'                                                    
EXIT     XIT1                                                                   
*                                                                               
SVNOSUM  DS    C                                                                
*                                                                               
MXXSUB   MVI   BUFCDE,X'90'                                                     
         BAS   RE,DOSUM                                                         
*        MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
GOTOSUB  CLI   PASS,0              PASS = 0                                     
         BNER  R9                   NO - BYPASS SUBPROGRAM                      
         CLI   SUBPSW,0                                                         
         BNE   *+10                                                             
         CLI   MODE,ESTFRST                                                     
         BHR   R9                                                               
         MVI   MEDEXTAC,C' '                                                    
         BRAS  RE,GOSUB                                                         
         BR    R9                                                               
         EJECT                                                                  
* BUILD WEEKLY GRID AND SUM INTO STATION BUCKETS  R6=PRINT POSITION             
BGRID    L     R5,MEDAFRST                                                      
         LA    R8,STAGRID                                                       
         ST    R6,FULL             SAVE PRINT LINE ADDRESS                      
BGRID2   L     R4,4(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BZ    BGRID5                                                           
         CLI   MODE,STALAST                                                     
         BL    BGRID3                                                           
         EDIT  MEDBYSPT,(4,(R6))                                                
         B     BGRID4                                                           
BGRID3   EDIT  MEDBYSPT,(3,1(R6))                                               
         CLI   SVQOPT1,C'A'        REPORT TYPE A SPECIALS                       
         BNE   BGRID4                                                           
         OC    MEDBYSPT,MEDBYSPT                                                
         BZ    *+10                                                             
         MVC   1(3,R6),=C'  X'                                                  
BGRID4   L     RE,0(R8)            SUM WEEKLY SPOTS                             
         A     RE,MEDBYSPT                                                      
         ST    RE,0(R8)                                                         
         LA    R6,4(R6)                                                         
         LA    R8,4(R8)                                                         
         LA    R5,12(R5)                                                        
         B     BGRID2                                                           
BGRID5   CLI   MODE,PROCBUY        PRINT PRE-EMPTIONS                           
         BNE   BGRIDX                                                           
         OC    PREMTAB,PREMTAB                                                  
         BZ    BGRID6                                                           
         L     R6,FULL             SET PRINT LINE ADDRESS                       
         LA    R6,132(R6)                                                       
         ST    R6,FULL                                                          
         L     R5,MEDAFRST                                                      
         LA    R8,PREMTAB                                                       
BGRID5A  L     R4,4(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BZ    BGRID6                                                           
         MVC   BYTE,0(R8)          SAVE MADEGOOD FLAG                           
         MVI   0(R8),0                                                          
         L     RF,0(R8)                                                         
         LTR   RF,RF                                                            
         BZ    BGRID5B                                                          
         MVI   3(R6),C'P'                                                       
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         MVI   3(R6),C'M'                                                       
         EDIT  (RF),(3,(R6))                                                    
BGRID5B  LA    R8,4(R8)            NEXT SLOT                                    
         LA    R5,12(R5)                                                        
         LA    R6,4(R6)                                                         
         B     BGRID5A                                                          
         SPACE 2                                                                
BGRID6   OC    HIATAB,HIATAB       PRINT HIATUS                                 
         BZ    BGRIDX                                                           
         L     R6,FULL                                                          
         LA    R6,132(R6)                                                       
         L     R5,MEDAFRST                                                      
         LA    R8,HIATAB                                                        
BGRID6A  L     R4,4(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BZ    BGRIDX                                                           
         L     RF,0(R8)                                                         
         LTR   RF,RF                                                            
         BZ    BGRID6B                                                          
         MVI   3(R6),C'H'                                                       
         EDIT  (RF),(3,(R6))                                                    
BGRID6B  LA    R8,4(R8)                                                         
         LA    R5,12(R5)                                                        
         LA    R6,4(R6)                                                         
         B     BGRID6A                                                          
BGRIDX   CLI   MODE,PROCBUY                                                     
         BNE   BGRIDX2                                                          
         CLI   SVQOPT1,C'A'                                                     
         BE    BGRIDX2                                                          
         L     R6,DTOTSPT          EDIT DETAIL TOTAL SPOTS                      
         LTR   R6,R6                                                            
         BZ    BGRIDX2                                                          
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         EDIT  MEDBYSPT,(3,(R6))                                                
BGRIDX2  BR    R9                                                               
         EJECT                                                                  
         USING BDEXTD,R5                                                        
* BTS DESCRIPTION                                                               
BTSDESC  MVC   P1(L'BDPDAY),BDPDAY                                              
         MVC   P1+9(L'BDPTIME),BDPTIME                                          
         MVC   P1+21(L'BDPPROG),BDPPROG                                         
         MVC   P1+38(L'BDPSLN),BDPSLN                                           
         CLI   D2APROF+9,C'N'      ANOTHER WESTERN POS                          
         BE    *+16                                                             
         MVC   P1+42(L'BDPDPT),BDPDPT    KILL DAYPART                           
         MVC   P1+44(L'BDPPURP),BDPPURP  ALSO THE PURP CODE PER LISA            
         MVC   P2(16),PKGAREA                                                   
         OC    SVSPREP,SVSPREP                                                  
         BZ    BTSDESC1                                                         
         GOTO1 VRCPACK,DMCB,(C'U',SVSPREP),FULL                                 
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         LA    R8,P2                                                            
         CLI   0(R8),0                                                          
         BE    *+8                                                              
         LA    R8,17(R8)                                                        
*---->   MVC   0(21,R8),=C'***SPECIAL REP=   ***'                               
         MVC   0(L'SP@SPREP,R8),SP@SPREP                                        
         DROP  RE                                                               
         MVC   15(3,R8),FULL                                                    
         OC    SVSPREPN,SVSPREPN                                                
         BZ    BTSDESC1                                                         
         MVC   15(L'SVSPREPN,R8),SVSPREPN                                       
         LA    RE,14+L'SVSPREPN(R8)                                             
         CLI   0(RE),X'40'                                                      
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   2(RE),C'('                                                       
         MVC   3(3,RE),FULL                                                     
         MVC   6(4,RE),=C')***'                                                 
BTSDESC0 CLI   P2,C' '                                                          
         BNE   *+8                                                              
         MVI   P2,0                                                             
BTSDESC1 DS    0H                                                               
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         LA    R8,P2+1                                                          
         CLI   P2+1,C' '                                                        
         BE    *+8                                                              
         LA    R8,P3+1                                                          
         CLI   PIGPRNT,C' '                                                     
         BE    BTSDESC2                                                         
*---->   MVC   0(9,R8),=C'PARTNER='                                             
         MVC   0(L'SP@PRTNR,R8),SP@PRTNR                                        
         DROP  RE                                                               
         LR    RF,R8                                                            
         LA    RF,L'SP@PRTNR-1(RF)                                              
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   9(L'PIGPRNT,R8),PIGPRNT                                          
         LA    R8,132(R8)                                                       
BTSDESC2 DS    0H                                                               
         ST    R8,FULL                                                          
         GOTO1 VCOMPRNT                                                         
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
*---->   MVC   P1+101(7),=C'*SPILL*'                                            
         MVC   P1+101(L'SP7SPILL),SP7SPILL                                      
         DROP  RE                                                               
         BR    R9                                                               
         CLI   DETOPTS+2,0         SUPPRESS COST                                
         BER   R9                   YES                                         
         MVC   P1+101(6),BDPCOST+6                                              
         CLI   BDPCOST+9,C','                                                   
         BE    *+8                                                              
         CLI   BDPCOST+9,C'.'                                                   
         BNE   *+10                                                             
         MVC   P1+101(7),BDPCOST+4                                              
         CLI   BDPCOST+4,C' '                                                   
         BNE   *+10                                                             
         MVC   P1+101(7),BDPCOST+5                                              
BTSDESC4 GOTO1 VGETCOVR                                                         
         CLI   BDPCOST+4,C' '                                                   
         BE    *+8                                                              
         CLI   QMED,C'R'                                                        
         BNER  R9                                                               
         MVC   P1+101(7),BDPCOST+5                                              
         BR    R9                                                               
* SAL DESCRIPTION                                                               
SALDESC  DS    0H                                                               
         MVC   P1(L'BDPDAY),BDPDAY                                              
         MVC   P1+9(L'BDPTIME),BDPTIME                                          
         MVC   P1+21(L'BDPPROG),BDPPROG                                         
         MVC   P1+39(L'BDPSLN),BDPSLN                                           
         CLI   D2APROF+9,C'Y'                                                   
         BNE   *+14                                                             
         MVI   P1+37,C' '                                                       
         MVC   P1+38(1),BDPDPT                                                  
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   PKGAREA,0           PRINT MG/REV/PKG CAPTION                     
         BE    *+10                                                             
         MVC   P2+2(16),PKGAREA                                                 
         CLI   PIGPRNT,C' '        PRINT PIGYBACKS                              
         BE    SALDSC01                                                         
*---->   MVC   P2+22(6),=C'PRTNR='                                              
         MVC   P2+22(L'SP5PRTNR),SP5PRTNR                                       
         LA    RF,P2+22+L'SP5PRTNR-1                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   P2+28(L'PIGPRNT),PIGPRNT                                         
         DROP  RE                                                               
SALDSC01 OC    SVSPREP,SVSPREP     PRINT SPECIAL REPS                           
         BZ    SALDESC1                                                         
         LA    R8,P2                                                            
         CLI   PKGAREA,0                                                        
         BE    *+8                                                              
         LA    R8,P3                                                            
         GOTO1 VRCPACK,DMCB,(C'U',SVSPREP),FULL                                 
         MVC   0(21,R8),PCAPSRP                                                 
         MVC   15(3,R8),FULL                                                    
         OC    SVSPREPN,SVSPREPN                                                
         BZ    SALDESC1                                                         
         MVC   15(L'SVSPREPN,R8),SVSPREPN                                       
         LA    RE,14+L'SVSPREPN(R8)                                             
         CLI   0(RE),X'40'                                                      
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   2(RE),C'('                                                       
         MVC   3(3,RE),FULL                                                     
         MVC   6(4,RE),=C')***'                                                 
SALDESC1 DS    0H                                                               
         LA    R8,P2+22                                                         
         OC    P2(132),SPACES                                                   
         CLC   P2(131),P2+1                                                     
         BE    *+8                                                              
         LA    R8,132(R8)                                                       
         ST    R8,FULL                                                          
         GOTO1 VCOMPRNT                                                         
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
*---->   MVC   P1+101(7),=C'*SPILL*'                                            
         MVC   P1+101(L'SP7SPILL),SP7SPILL                                      
         DROP  RE                                                               
         BR    R9                                                               
         CLI   DETOPTS+2,0                                                      
         BER   R9                                                               
         CLC   QPROG,=C'D7'        PRINT PENNIES AND REPOSITION FOR D7          
         BE    SALDESC6                                                         
         CLC   QPROG,=C'DX'                                                     
         BE    SALDESC6                                                         
         CLC   QPROG,=C'CP'        PRINT PENNIES AND REPOSITION FOR CP          
         BE    SALDESC8                                                         
         MVC   P1+101(6),BDPCOST+6                                              
*                                                                               
         CLC   QPROG,=C'D6'                                                     
         BNE   SALDESC4                                                         
         CLI   PROGPROF+6,C'Y'     TEST PRINT DEMOS ON SEP LINES                
         BNE   SALDESC4                                                         
         MVC   P2+98(9),BDPCOST+3                                               
         MVC   P1+101(6),SPACES                                                 
         GOTO1 VGETCOVR                                                         
         BR    R9                                                               
*                                                                               
SALDESC4 GOTO1 VGETCOVR                                                         
         CLI   BDPCOST+9,C','                                                   
         BE    *+8                                                              
         CLI   BDPCOST+9,C'.'                                                   
         BNE   *+10                                                             
         MVC   P1+101(7),BDPCOST+4                                              
         CLI   BDPCOST+4,C' '                                                   
         BNE   *+10                                                             
         MVC   P1+101(7),BDPCOST+5                                              
         TM    BDPCOST+5,X'F0'     LEAVE SUPPRESSED COST IF TOO BIG             
         BO    *+8                                                              
         CLI   QMED,C'R'                                                        
         BNER  R9                                                               
         MVC   P1+101(7),BDPCOST+5                                              
         BR    R9                                                               
*                                                                               
SALDESC6 CLC   QPROG,=C'DX'                                                     
         BNE   SALDSC07                                                         
         CLI   DRCSHTRD,C'R'       TEST TRADE SIDE OF CASH/TRD                  
         BNE   SALDSC07                                                         
         CLI   DARPROF+12,C'Y'    YES, ZERO COST TRADE?                         
         BNE   SALDSC07            NO                                           
         XC    BDPCOST,BDPCOST                                                  
         MVC   BDPCOST+7(5),=C'$0.00'                                           
*                                                                               
SALDSC07 MVC   P1+102(9),BDPCOST+3      D7 COST PRINTING                        
         CLC   QPROG,=C'D7'        PRINT COST OVERRIDES FOR                     
         BE    *+10                                                             
         CLC   QPROG,=C'DX'        PRINT COST OVERRIDES FOR                     
         BNER  R9                                                               
SALDSC08 GOTO1 VGETCOVR                                                         
         BR    R9                                                               
SALDESC8 MVC   P2+1(9),BDPCOST+3                                                
         BR    R9                                                               
         EJECT                                                                  
* BRS DESCRIPTION                                                               
BRSDESC  DS    0H                                                               
* PRINT DEMOS AND CPP FOR BRS,BTS,BDS,SAL                                       
         GOTO1 VBRSDSCC                                                         
         BR    R9                                                               
         EJECT                                                                  
BTSPD    LA    R8,P1               SET PRINT LINE                               
         CLI   MODE,STALAST                                                     
         BNE   *+8                                                              
         LA    R8,P2                                                            
         CLI   DETOPTS+1,1         DEMOS REQUESTED                              
         BNE   BTSPDX                                                           
         CLI   Q2FAXDEM,C'Y'       REPOSITION IF FAX REPORT                     
         BE    BTSPDDX                                                          
* FOR D6 ONLY                                                                   
         CLC   QPROG,=C'D6'                                                     
         BNE   *+12                                                             
         CLI   PROGPROF+6,C'Y'     TEST PRINT DEMOS ON SEP LINES                
         BE    BTSPDD6                                                          
*                                                                               
         MVC   108(5,R8),PLD1      MOVE DEMOS TO PRINT LINE                     
         MVC   113(1,R8),OVRFLAG                                                
         MVC   114(5,R8),PLD2                                                   
         MVC   119(1,R8),OVRFLAG+1                                              
         MVC   120(5,R8),PLD3                                                   
         MVC   125(1,R8),OVRFLAG+2                                              
         MVC   126(5,R8),PLD4                                                   
         MVC   131(1,R8),OVRFLAG+3                                              
         LA    R8,132(R8)                                                       
         B     BTSPD1                                                           
                                                                                
BTSPD1   CLI   DETOPTS+3,1         CPP/M REQUESTED                              
         BNE   BTSPDX                                                           
*                                                                               
         MVC   107(6,R8),PLD1CP    MOVE CPP/M TO PRINT LINE                     
         MVC   119(6,R8),PLD3CP                                                 
         CLI   BIGCPP,C'Y'                                                      
         BE    BTSPD2                                                           
* PRINT ON FIRST PRINT LINE                                                     
         MVC   113(6,R8),PLD2CP                                                 
         MVC   125(6,R8),PLD4CP                                                 
         B     BTSPDX                                                           
*                                                                               
BTSPD2   MVC   132+113(6,R8),PLD2CP                                             
         MVC   132+125(6,R8),PLD4CP  STAGGER PRINTING FOR BIG NUMBERS           
*                                                                               
BTSPDX   BR    R9                                                               
         EJECT                                                                  
BTSPDD6  DS    0H                                                               
         MVC   109(7,R8),DNAME1                                                 
         MVC   117(5,R8),PLD1                                                   
         MVC   122(1,R8),OVRFLAG                                                
         CLI   DETOPTS+3,1         CPP/M REQUESTED                              
         BNE   *+10                                                             
         MVC   125(6,R8),PLD1CP    MOVE CPP/M TO PRINT LINE                     
*                                                                               
         LA    R8,132(R8)                                                       
         MVC   109(7,R8),DNAME2                                                 
         MVC   117(5,R8),PLD2                                                   
         MVC   122(1,R8),OVRFLAG+1                                              
         CLI   DETOPTS+3,1                                                      
         BNE   *+10                                                             
         MVC   125(6,R8),PLD2CP                                                 
*                                                                               
         LA    R8,132(R8)                                                       
         MVC   109(7,R8),DNAME3                                                 
         MVC   117(5,R8),PLD3                                                   
         MVC   122(1,R8),OVRFLAG+2                                              
         CLI   DETOPTS+3,1                                                      
         BNE   *+10                                                             
         MVC   125(6,R8),PLD3CP                                                 
*                                                                               
         LA    R8,132(R8)                                                       
         MVC   109(7,R8),DNAME4                                                 
         MVC   117(5,R8),PLD4                                                   
         MVC   122(1,R8),OVRFLAG+3                                              
         CLI   DETOPTS+3,1                                                      
         BNE   *+10                                                             
         MVC   125(6,R8),PLD4CP                                                 
         BR    R9                                                               
         EJECT                                                                  
*============================================================                   
* MOVE DEMOS TO PRINT LINE FOR DX REPORT                                        
*============================================================                   
         SPACE 1                                                                
BTSPDDX  LA    R1,DXDEMDSP         POINT TO LIST OF PRINT DSPLS                 
         CLI   PRDEMCNT,4                                                       
         BL    *+8                                                              
         LA    R1,8(R1)                                                         
*                                                                               
         LR    RE,R8                                                            
         AH    RE,0(R1)            POINT TO OUTPUT POSN                         
         MVC   0(5,RE),PLD1                                                     
         MVC   5(1,RE),OVRFLAG                                                  
*                                                                               
         LR    RE,R8                                                            
         AH    RE,2(R1)            POINT TO OUTPUT POSN                         
         MVC   0(5,RE),PLD2                                                     
         MVC   5(1,RE),OVRFLAG+1                                                
*                                                                               
         LR    RE,R8                                                            
         AH    RE,4(R1)            POINT TO OUTPUT POSN                         
         MVC   0(5,RE),PLD3                                                     
         MVC   5(1,RE),OVRFLAG+2                                                
*                                                                               
         CLI   PRDEMCNT,4                                                       
         BLR   R9                                                               
*                                                                               
         LR    RE,R8                                                            
         AH    RE,6(R1)            POINT TO OUTPUT POSN                         
         MVC   0(5,RE),PLD4                                                     
         MVC   5(1,RE),OVRFLAG+3                                                
         BR    R9                                                               
*                                                                               
DXDEMDSP DC    AL2(111,119,127,0)    PRINT POSNS FOR 1-3 DEMOS                  
         DC    AL2(111,247,121,257)  PRINT POSNS FOR 4 DEMOS                    
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* CALCULATE AND SAVE DEMOS AND CPP/CPM                                          
*===============================================================                
         SPACE 1                                                                
CSDEMCP  NTR1                                                                   
         LA    R5,MEDPERD                                                       
         L     R4,4(R4)                                                         
         L     R4,4(R5)                                                         
         XC    SVDEMS,SVDEMS                                                    
         OC    MEDBYD(12),MEDBYD                                                
         BZ    CSDEMCPX                                                         
         L     RE,STASPOT                                                       
         A     RE,MEDBYSPT                                                      
         ST    RE,STASPOT                                                       
         L     RE,STACOST                                                       
         CHI   RE,-1               PRESERVE $ OVERFLOW                          
         BE    CSDEM10                                                          
         CLC   MEDBYD,=F'-1'                                                    
         BE    *+12                                                             
         A     RE,MEDBYD                                                        
         BNO   *+8                 DETECT $ OVERFLOW                            
         LHI   RE,-1               FFS TO INDICATE $ OVERFLOW                   
         ST    RE,STACOST                                                       
*                                                                               
CSDEM10  L     RE,STACOST+4                                                     
         CHI   RE,-1               PRESERVE $ OVERFLOW                          
         BE    CSDEM20                                                          
         CLC   MEDBYDEQ,=F'-1'                                                  
         BE    *+12                                                             
         A     RE,MEDBYDEQ                                                      
         BNO   *+8                 DETECT $ OVERFLOW                            
         LHI   RE,-1               FFS TO INDICATE $ OVERFLOW                   
         ST    RE,STACOST+4                                                     
*                                                                               
CSDEM20  LA    R0,8                                                             
         LA    R1,STADEMS                                                       
         LA    R6,MEDBY1                                                        
*                                                                               
CSDEM30  L     RE,0(R1)                                                         
         A     RE,0(R6)                                                         
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R6,4(R6)                                                         
         BCT   R0,CSDEM30                                                       
*                                                                               
         XC    PRTLINE,PRTLINE                                                  
         LA    R6,PRTLINE                                                       
         USING SUMDSECT,R6                                                      
         MVC   SUMDL(8),MEDBYD                                                  
         MVC   SUMD1(32),MEDBY1                                                 
         GOTO1 VCALCPP,DMCB,MEDBYSPT,(R6)                                       
         DROP  R6                                                               
CSDEMCPX XIT1                                                                   
         EJECT                                                                  
*===========================================================                    
* DO SUMMARIES FOR VARIOUS BREAKS                                               
*===========================================================                    
                                                                                
DOSUM    NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM2                                                           
         MVC   WEIGHT,SPWEIGHT                                                  
         GOTO1 VSUMMRY                                                          
DOSUM2   SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         L     R8,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',(R9))                
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* HEADLINE ROUTINES                                                             
*===============================================================                
         DS    0D                                                               
MYHEAD   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,SPD2R2                                                     
         L     RA,SPD2RA                                                        
         L     RC,SPD2RC                                                        
         CLI   RCSUBPRG,255        SUPPRESS HEADLINES                           
         BE    MYHEADX                                                          
*                                                                               
         CLI   HEADHOOK,C'R'       TEST RETURN CALL                             
         BNE   MYHEAD2                                                          
         CLI   SALZEN,C'Y'         ZENITH HEADLINES?                            
         BNE   MYHDS1                                                           
         BRAS  RE,MYHDZEN          YES - CURRENTLY ONLY FOR ZENITH              
*                                                                               
MYHDS1   CLI   SALOPT,C'Y'         OPTIMEDIA HEADLINES?                         
         BNE   MYHDSX1                                                          
         BRAS  RE,MYHDOPT          YES - CURRENTLY ONLY FOR ZENITH              
*                                                                               
MYHDSX1  CLI   DXFNSW,C'Y'         TEST A NEW DX                                
         BE    MYHEADX             HE WILL PRINT THIS DXFN                      
*                                                                               
         CLI   DARESVSW,C'Y'       HAVE SAVE HEADLINES?                         
         BNE   MYHEADX                                                          
*                                                                               
         L     RE,=A(DAREHDSV)     GET SAVED DATES/TITLES                       
         LA    R1,H8               (ACCOUNT FOR 8 SPECIAL HEADLINES)            
         LHI   R0,DAREHDLQ                                                      
*                                                                               
MYHDSX2  OC    0(132,R1),0(RE)     DID I JUST MOVE SPACES?                      
         BE    MYHDSX3             YES                                          
         MVC   0(132,RE),SPACES                                                 
         AHI   R1,132                                                           
MYHDSX3  AHI   RE,132                                                           
         BCT   R0,MYHDSX2                                                       
*                                                                               
         MVI   DARESVSW,C'N'                                                    
         B     MYHEADX                                                          
*                                                                               
MYHEAD2  CLI   UNWOPTS,0                                                        
         BE    MYHEAD4                                                          
*                                                                               
         MVC   H1+12(3),=C'---'                                                 
         LA    R1,H1+12                                                         
         TM    UNWOPTS,X'80'                                                    
         BO    *+8                                                              
         MVI   0(R1),C'+'                                                       
         AHI   R1,1                                                             
         TM    UNWOPTS,X'40'                                                    
         BO    *+8                                                              
         MVI   0(R1),C'+'                                                       
         AHI   R1,1                                                             
         TM    UNWOPTS,X'20'                                                    
         BO    *+8                                                              
         MVI   0(R1),C'+'                                                       
         AHI   R1,1                                                             
         OC    UNWSTRP,UNWSTRP                                                  
         BZ    *+10                                                             
         MVC   0(2,R1),=C'..'      INDICATE LIMITED DATES                       
*                                                                               
MYHEAD4  MVI   MID1,0                                                           
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   MODE,STALAST                                                     
         BH    *+10                                                             
         MVC   MID2,SVSTLINE                                                    
*                                                                               
         CLI   MODE,CBHLAST                                                     
         BNE   *+10                                                             
         MVC   MID2(L'SVCBLLIN),SVCBLLIN                                        
*                                                                               
         CLI   MODE,MGR1LAST                                                    
         BNH   *+10                                                             
*---->   MVC   H7+51(20),HCAPSUM                                                
         MVC   H7+51(L'SP@SUM),SP@SUM                                           
         DROP  RE                                                               
         CLC   QPROG,=C'CP'        TEST DRAFT ORDER REPORT                      
         BNE   MYHEAD10                                                         
         PACK  DUB,H8+101(2)       YES-ADD 8 HRS TO GET MILITARY TIME           
         AP    DUB,=P'8'           (ALWAYS RUNS SOON, SO NEVER AFTER            
         OI    DUB+7,X'0F'          MIDNIGHT)                                   
         UNPK  H8+101(2),DUB                                                    
         CLI   H8+101,C'0'                                                      
         BNE   MYHEAD10                                                         
         MVI   H8+101,C' '                                                      
*                                                                               
MYHEAD10 CLC   RTYPE,=C'BRS'                                                    
         BNE   MYHEAD12                                                         
         BRAS  RE,BRSHEAD                                                       
         B     MYHEAD22                                                         
MYHEAD12 CLC   RTYPE,=C'BDS'                                                    
         BE    *+10                                                             
         CLC   RTYPE,=C'SAL'                                                    
         BNE   MYHEAD20                                                         
         BRAS  RE,SALHEAD                                                       
         B     MYHEAD22                                                         
MYHEAD20 CLC   RTYPE,=C'BTS'                                                    
         BNE   MYHEAD30                                                         
         BRAS  RE,BTSHEAD                                                       
*                                                                               
MYHEAD22 MVC   CURHDAT1(112),CURPDAT1 SAVE PRINTED HL DATES                     
         LA    R4,H1+45                                                         
         LA    R5,42               CENTER ON H1+66                              
         CLI   RCSUBPRG,9          TEST 110 CHAR CP                             
         BNE   *+8                                                              
         LHI   R5,28                                                            
         CLI   SVQOPT1,C'B'        TEST FOR 110 CHAR REPORT OPTIONS             
         BNE   MYHEAD24                                                         
         CLC   RTYPE,=C'BDS'                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'SAL'                                                    
         BNE   MYHEAD24                                                         
         LA    R4,H1+18                                                         
         LA    R5,50               CENTER ON H1+43                              
*                                                                               
MYHEAD24 MVC   0(4,R4),PWPREFIX                                                 
         CLI   D2ADJCR,C'Y'                                                     
         BNE   *+10                                                             
         MVC   5(3,R4),=C'ADJ'                                                  
*                                                                               
         GOTO1 SQUASHER,DMCB,(R4),(R5)                                          
         L     R0,4(R1)            GET SQUASHED LENGTH                          
         GOTO1 UNDERLIN,(R1),((R0),(R4)),132(R4)                                
         GOTO1 CENTER,DMCB,(R4),(R5)                                            
         LA    R4,132(R4)                                                       
         GOTO1 (RF),(R1),(R4),(R5)                                              
         B     MYHEADX                                                          
*                                                                               
MYHEAD30 CLC   RTYPE,=C'MGA'                                                    
         BNE   MYHEADX                                                          
         MVC   H1+54(24),=C'MISSED/MAKEGOOD ANALYSIS'                           
         MVC   H2+54(24),=C'------------------------'                           
         MVC   H10(34),=C' CD TYPE EST-LIN DATE     LEN TIME'                   
         MVC   H11(34),=C' -- ---- ------- ----     --- ----'                   
         MVC   H10+41(34),=CL34'MSD COST MSD RTG MGD COST MGD RTG'              
*                                                                               
         CLI   DNAMES,C'R'         TEST RATING TARGET                           
         JE    MYHEAD32                                                         
         CLI   DNAMES,C'E'                                                      
         JE    MYHEAD32                                                         
         MVC   H10+54(3),=C'IMP'                                                
         MVC   H10+71(3),=C'IMP'                                                
*                                                                               
MYHEAD32 MVC   H11+41(34),=CL34'-------- ------- -------- -------'              
         MVC   H10+75(47),=CL47'PROGRAM NAME'                                   
         MVC   H11+75(47),=CL47'------------'                                   
         MVC   H12,SPACES                                                       
         MVC   H13,SPACES                                                       
*                                                                               
MYHEADX  CLI   PAGEONE,C'Y'        TEST FIRST PAGE                              
         BNE   MYHEADX2                                                         
         CLI   SALZEN,C'Y'         TEST ZENITH SAL REPORT                       
         BE    MYHEADX1                                                         
         CLI   SALOPT,C'Y'         TEST OPTIMEDIA SAL REPORT                    
         BNE   MYHEADX2                                                         
*                                                                               
MYHEADX1 MVI   HEADHOOK,C'R'       REQUEST HEADHOOK RETURN                      
*                                                                               
* PRINT ADDS STUFF NOW                                                          
         CLC   Q2ADDS,SPACES       TEST REVISION DATA                           
         BE    MYHEADX2                                                         
         LA    R4,H4+90            POINT TO PRINT POSITION                      
         BRAS  RE,PRTVRSN                                                       
*                                                                               
MYHEADX2 CLI   DARESVSW,C'Y'       TEST HEADS ALREADY SAVED                     
         JE    EXIT                                                             
*                                                                               
         CLI   DXFNSW,C'Y'         TEST A NEW DX                                
         BNE   MYHEADX4                                                         
         LA    R1,H10              SAVE H10-H14 FOR DARE                        
         L     RE,=A(DAREMDSV)                                                  
         MVC   0(L'DAREMDSV,RE),MID2  AND MID2                                  
         MVC   MID2,SPACES                                                      
         MVC   MID1,SPACES                                                      
         MVI   FORCEMID,C'N'                                                    
         B     MYHEADX6                                                         
*                                                                               
MYHEADX4 CLI   HEADHOOK,C'R'       REQUEST HEADHOOK RETURN?                     
         JNE   EXIT                                                             
         LA    R1,H9               SAVE H19-H13 FOR DARE                        
*                                                                               
MYHEADX6 MVI   DARESVSW,C'Y'                                                    
         L     RE,=A(DAREHDSV)                                                  
         LHI   R0,DAREHDLQ                                                      
*                                                                               
MYHEADX8 OC    0(132,R1),SPACES                                                 
         BE    MYHEADX9                                                         
         MVC   0(132,RE),0(R1)                                                  
         MVC   0(132,R1),SPACES                                                 
         AHI   RE,132                                                           
MYHEADX9 AHI   R1,132                                                           
         BCT   R0,MYHEADX8                                                      
         J     EXIT                                                             
*                                                                               
SPD2R2   DC    F'0'                                                             
SPD2R3   DC    F'0'                                                             
SPD2RA   DC    F'0'                                                             
SPD2RB   DC    F'0'                                                             
SPD2RC   DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* PRINT SPECIAL CAPTION FOR ZENITH SALESPERSON SCHEDULE                         
*==========================================================                     
                                                                                
MYHDZEN  NTR1  BASE=*,LABEL=*                                                   
         MVI   HEADHOOK,0          CLEAR HEADHOOK RETURN FLAG                   
         MVI   PAGEONE,C'N'        RESET FLAGS                                  
         MVI   SALZEN,C'N'                                                      
*                                                                               
         LA    R0,14               CLEAR OUT PREVIOUS HEADLINES                 
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVI   H1,C'*'                                                          
         MVC   H1+1(91),H1                                                      
*                                                                               
         LA    R0,5                                                             
         LA    R1,H2                                                            
*                                                                               
         MVI   0(R1),C'*'                                                       
         MVI   91(R1),C'*'                                                      
         AHI   R1,132                                                           
         BCT   R0,*-12                                                          
         MVC   0(92,R1),H1         COPY STARS FROM H1                           
*                                                                               
         MVC   H2+2(19),=C'STATION INVOICE TO:'                                 
*                                                                               
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
*                                                                               
         MVC   H3+1(90),SPACES                                                  
         MVC   H4+1(90),SPACES                                                  
         MVC   H5+1(90),SPACES                                                  
*                                                                               
         CLC   CACCAGY,=C'DF'                                                   
         BE    MYHDZ10                                                          
         CLC   CACCAGY,=C'DW'                                                   
         BE    MYHDZ10                                                          
*                                                                               
         CLC   CACCOFC,=C'3A'                                                   
         BE    MYHDZ10                                                          
         CLC   CACCOFC,=C'3C'                                                   
         BE    MYHDZ10                                                          
         CLC   CACCOFC,=C'3D'                                                   
         BE    MYHDZ10                                                          
         CLC   CACCOFC,=C'3K'                                                   
         BE    MYHDZ10                                                          
         CLC   CACCOFC,=C'3P'                                                   
         BNE   MYHDZ20                                                          
MYHDZ10  MVC   H3+2(18),=C'SAATCHI && SAATCHI,'                                 
         MVC   H3+21(42),=C'79 MADISON AVENUE, NEW YORK, NY 10016-7802'         
         MVC   H4+2(47),SSAGENT                                                 
         B     MYHDZ70                                                          
*                                                                               
MYHDZ20  CLC   CACCAGY,=C'TH'                                                   
         BNE   MYHDZ30                                                          
         MVC   H3+2(28),=C'ZENITH MEDIA SERVICES, INC.,'                        
         MVC   H3+31(40),=C'79 MADISON AVE., NEW YORK, NY 10016-7802'           
         MVC   H3+73(12),=C'646-935-4700'                                       
         MVC   H4+2(19),=C'AS AGENT FOR CLIENT'                                 
         MVC   H6+2(24),=C'THIS ORDER IS MADE UNDER'                            
         MVC   H6+27(28),=C'AND IS SUBJECT TO AAAA TERMS'                       
         MVC   H6+56(34),=C'AND CONDITIONS FOR LOCAL BROADCAST'                 
         B     MYHDZ70                                                          
*                                                                               
MYHDZ30  CLC   CACCAGY,=C'BS'                                                   
         BNE   MYHDZ40                                                          
         MVC   H3+2(11),=C'BATES USA, '                                         
         MVC   H3+14(30),=C'498 SEVENTH AVENUE,NY NY 10018'                     
         B     MYHDZ70                                                          
*                                                                               
MYHDZ40  MVC   H3+2(31),AGYNM                                                   
         MVI   H3+33,C','                                                       
         MVC   H3+34(31),AGYADR                                                 
*                                                                               
MYHDZ70  MVC   H5+2(37),=C'--- MUST INCLUDE REFERENCE NUMBER ---'               
*                                                                               
MYHDZ90  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  RF                                                               
*                                                                               
SSAGENT  DC    C'AGENT FOR SAATCHI AND SAATCHI, AGENT FOR CLIENT'               
*                                                                               
         LTORG                                                                  
*                                                                               
*==========================================================                     
* PRINT SPECIAL CAPTION FOR OPTIMEDIA SALESPERSON SCHEDULE                      
*==========================================================                     
                                                                                
MYHDOPT  NTR1  BASE=*,LABEL=*                                                   
         MVI   HEADHOOK,0          CLEAR HEADHOOK RETURN FLAG                   
         MVI   PAGEONE,C'N'        RESET FLAGS                                  
         MVI   SALOPT,C'N'                                                      
*                                                                               
         LA    R0,14               CLEAR OUT PREVIOUS HEADLINES                 
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         MVI   H1,C'*'                                                          
         MVC   H1+1(91),H1                                                      
*                                                                               
         LA    R0,4                                                             
         LA    R1,H2                                                            
*                                                                               
         MVI   0(R1),C'*'                                                       
         MVI   91(R1),C'*'                                                      
         AHI   R1,132                                                           
         BCT   R0,*-12                                                          
         MVC   0(92,R1),H1         COPY STARS FROM H1                           
*                                                                               
         MVC   H2+2(33),=C'ORIGINAL INVOICES AND AFFIDAVITS'                    
         MVC   H2+36(29),=C'SHOULD BE SENT BY THE 10TH OF'                      
         MVC   H2+66(23),=C'THE FOLLOWING MONTH TO:'                            
*                                                                               
         MVC   H3+2(20),=C'SIMON PROPERTY GROUP'                                
         MVC   H3+23(14),=C'C/O OPTIMEDIA,'                                     
*                                                                               
         MVC   H4+2(42),=C'79 MADISON AVENUE, NEW YORK, NY 10016-7802'          
*                                                                               
         MVC   H5+2(20),=C'SIMON PROPERTY GROUP'                                
         MVC   H5+23(24),=C', LP, BY AND THROUGH ITS'                           
         MVC   H5+48(39),=C'AGENT, OPTIMEDIA INTERNATIONAL US, INC.'            
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
         EJECT                                                                  
INIT     NTR1  BASE=*,LABEL=*                                                   
         MVC   SVMAXLIN,MAXLINES                                                
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         L     RF,=V(SPRPFOOT)                                                  
         ST    RF,VFOOT                                                         
         L     RF,=A(SVMDBLK)                                                   
         ST    RF,VSVMDBLK                                                      
         L     RF,=A(HDATES)                                                    
         ST    RF,VHDATES                                                       
         L     RF,=A(GETBUF)                                                    
         ST    RF,VGETBUF                                                       
         L     RF,=A(CALCPP)                                                    
         ST    RF,VCALCPP                                                       
         L     RF,=A(STATOT)                                                    
         ST    RF,VSTATOT                                                       
         L     RF,=A(EDTDEMSC)                                                  
         ST    RF,VEDTDEMS                                                      
         L     RF,=A(SUBPAREA)                                                  
         ST    RF,VSUBPARA                                                      
         L     RF,=V(REVBUY)                                                    
         ST    RF,VREVBUY                                                       
         L     RF,=A(BRSDSCC)                                                   
         ST    RF,VBRSDSCC                                                      
         L     RF,=A(COMPRNT)                                                   
         ST    RF,VCOMPRNT                                                      
         L     RF,=A(GETCOVR)                                                   
         ST    RF,VGETCOVR                                                      
         L     RF,=V(REPCALOV)                                                  
         ST    RF,REPCALOV                                                      
         L     RF,=A(BTSDESC)                                                   
         ST    RF,ABTSDESC                                                      
         L     RF,=A(SALDESC)                                                   
         ST    RF,ASALDESC                                                      
         L     RF,=A(BRSDESC)                                                   
         ST    RF,ABRSDESC                                                      
         L     RE,=A(MYHEAD)                                                    
         ST    RE,HEADHOOK                                                      
         L     RE,=A(GETREP)                                                    
         ST    RE,VGETREP                                                       
         MVI   FIRST,1                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
GOSUB    NTR1  BASE=*,LABEL=*                                                   
         MVC   SVOPTS,QOPT1                                                     
         MVC   QOPT1(7),MSOPT                                                   
         MVC   SVPROF,PROGPROF                                                  
         MVC   PROGPROF,MSPROF                                                  
         MVC   SVSPECS,SPECS       GO TO SUBPROGRAM                             
         MVC   SVSUPMKT,SPSUPMKT                                                
         MVC   SVMDTAB,MEDTABLE                                                 
         MVC   SPECS,SVPH01        SET SPECS                                    
         MVC   MEDTABLE,SVPH04                                                  
         OC    MSBFHOOK,MSBFHOOK                                                
         BZ    GOSUB01                                                          
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   BUFFHOOK,MSBFHOOK                                                
         DROP  RF                                                               
*                                                                               
GOSUB01  CLI   MODE,STAFRST        BYPASS CLEARING BUFFER                       
         BL    GOTOSUB1                                                         
         CLI   MODE,MKTLAST BYPASS HEADLINE FORCE                               
         BL    GOSUB01A                                                         
         MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
GOSUB01A LA    RE,MEDWEEKS         CLEAR SLOTS FOR MEDIA SUMMARIES              
         LHI   RF,672                                                           
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PASS                                                          
         MHI   RF,268                                                           
         LA    RF,PASSTAB(RF)                                                   
         USING PASSTABD,RF                                                      
         MVC   MEDNUMWK(L'PASSP1),PASSP1                                        
         MVC   MEDWEEKS(L'PASSP2),PASSP2                                        
         XC    MEDMON01(144),MEDMON01                                           
         MVC   MEDMON01(L'PASSP3),PASSP3                                        
         MVC   MEDPERD(L'PASSP4),PASSP4                                         
         DROP  RF                                                               
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
*                                                                               
         L     R5,MEDAFRST         CLEAR MEDBUFF SLOTS FOR SUMMARY              
         LA    R6,MEDPERD                                                       
*                                                                               
GOSUB02  CR    R5,R6                                                            
         BH    GOTOSUB1                                                         
         OC    4(4,R5),4(R5)                                                    
         BZ    GOSUB03                                                          
         L     RE,4(R5)                                                         
         L     RF,MEDLCHNK                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
GOSUB03  LA    R5,12(R5)                                                        
         B     GOSUB02                                                          
*                                                                               
GOTOSUB1 MVC   SVRCSUB,RCSUBPRG                                                 
         L     R0,MEDBUFF          RESTORE MEDIA SUMMARY DATES                  
         LHI   R1,1208                                                          
         L     RE,VSVMDBLK                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SVSPHK,SPOTHOOK                                                  
         MVC   SPOTHOOK,MSSPHK                                                  
         MVC   SVHDHOOK,HEADHOOK                                                
         MVC   HEADHOOK,MSHDHOOK                                                
         MVC   RCSUBPRG,MSRCSUB                                                 
         MVC   SPSUPMKT,MSSUPMKT                                                
* CALL SUB                                                                      
         L     RF,SVPH02                                                        
         GOTO1 (RF),DMCB,(RA)                                                   
*                                                                               
         MVC   SVPH01,SPECS        CAPTURE ANY CHANGES                          
         MVC   SVPH04,MEDTABLE                                                  
         CLI   MODE,MKTLAST        BREAK POINT POINT FOR TESTING                
         B     *+6                                                              
         DC    H'0'                                                             
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   MSBFHOOK,BUFFHOOK                                                
         XC    BUFFHOOK,BUFFHOOK                                                
         DROP  RF                                                               
         MVC   MSOPT,QOPT1                                                      
         MVC   QOPT1(7),SVOPTS                                                  
         MVC   MSSUPMKT,SPSUPMKT                                                
         MVC   SPSUPMKT,SVSUPMKT                                                
         MVC   PROGPROF,SVPROF                                                  
*                                                                               
         L     R0,MEDBUFF          RESTORE MEDIA SUMMARY DATES                  
         LHI   R1,1208                                                          
         L     RE,VSVMDBLK                                                      
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   MSSPHK,SPOTHOOK                                                  
         MVC   SPOTHOOK,SVSPHK                                                  
         MVC   MSHDHOOK,HEADHOOK                                                
         MVC   MSRCSUB,RCSUBPRG                                                 
         MVC   RCSUBPRG,SVRCSUB                                                 
         MVC   HEADHOOK,SVHDHOOK                                                
         MVC   SPECS,SVSPECS                                                    
         MVC   MEDTABLE,SVMDTAB                                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
BRSDSCC  NTR1  BASE=*,LABEL=*                                                   
         BAS   R9,BRSDGO                                                        
         XIT1                                                                   
         SPACE 2                                                                
*PRINT BRS DESCRIPTION                                                          
BRSDGO   MVI   P1+3,C'-'                                                        
         MVC   P1(L'BDPEST),BDPEST                                              
         MVC   P1+4(L'BDPLIN),BDPLIN                                            
         MVC   P1+9(L'BDPBDATE),BDPBDATE                                        
         MVC   P1+8(1),CFDS                                                     
         MVC   P1+20(1),CFDE                                                    
         MVC   P1+21(L'BDPWKS),BDPWKS                                           
         CLI   BDPWIND,C'O'                                                     
         BE    *+10                                                             
         MVC   P1+23(1),BDPWIND                                                 
         MVC   P1+25(L'BDPDAY),BDPDAY                                           
         MVC   P1+33(L'BDPNPWK),BDPNPWK                                         
         MVC   P1+36(L'BDPTIME),BDPTIME                                         
         MVC   P1+48(L'BDPDPT),BDPDPT                                           
         MVC   P2+9(L'BDPSLN),BDPSLN                                            
         MVC   P2+13(L'BDPPROG),BDPPROG                                         
         MVC   P3+31(16),PKGAREA                                                
*                                                                               
         CLI   BDPPTYP,C' '        TEST FOR ALPHA ADJ CODE                      
         BNH   *+10                                                             
         MVC   P2+44(1),BDPPTYP                                                 
*                                                                               
         CLI   BDPADJ,C' '         TEST FOR NUMERIC ADJ CODE                    
         BNH   *+10                                                             
         MVC   P2+43(2),BDPADJ                                                  
*                                                                               
         OC    SVSPREP,SVSPREP                                                  
         BZ    BRSDESC1                                                         
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P3(21),PCAPSRP                                                   
         MVC   P3(L'SP@SPREP),SP@SPREP                                          
         DROP  RE                                                               
         GOTO1 VRCPACK,DMCB,(C'U',SVSPREP),FULL                                 
         LA    R8,P3                                                            
         MVC   15(3,R8),FULL                                                    
         OC    SVSPREPN,SVSPREPN                                                
         BZ    BRSDESC1                                                         
         MVC   15(L'SVSPREPN,R8),SVSPREPN                                       
         LA    RE,14+L'SVSPREPN(R8)                                             
         CLI   0(RE),X'40'                                                      
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   2(RE),C'('                                                       
         MVC   3(3,RE),FULL                                                     
         MVC   6(4,RE),=C')***'                                                 
BRSDESC1 DS    0H                                                               
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING NDELEM,RE                                                        
BRSD1    CLI   NDCODE,2                                                         
         BE    BRSD2                                                            
         SR    R0,R0                                                            
         IC    R0,NDLEN                                                         
         AR    RE,R0                                                            
         B     BRSD1                                                            
BRSD2    CLI   NDBOOK,0            NO BOOK - DON'T PRINT IT                     
         BE    BRSD3                                                            
         GOTO1 DATCON,DMCB,(X'03',NDBOOK),(X'09',P2+2)                          
*                                                                               
         CLI   MEDSPILL,C'Y'                                                    
         BE    BRSD2E                                                           
*                                                                               
BRSD2A   L     RE,ADBUY            CHECK FOR SPECIAL SURVEY                     
         LA    RE,24(RE)                                                        
         USING NDELEM,RE                                                        
BRSD2B   CLI   NDCODE,X'24'        SS CODE RESIDES IN THIS ELEMENT              
         BE    BRSD2C                                                           
         CLI   NDCODE,0                                                         
         BE    BRSD3                                                            
         SR    R0,R0                                                            
         IC    R0,NDLEN                                                         
         AR    RE,R0                                                            
         B     BRSD2B                                                           
BRSD2C   MVC   P2+8(1),2(RE)       MOVE SS BEHIND BOOK                          
         B     BRSD3                                                            
*                                                                               
BRSD2E   DS    0H                  THIS PART FOR SPILL                          
         L     RE,MEDADEMO                                                      
         L     RE,4(RE)                                                         
         MVC   P2+8(1),NDBOOK+6                                                 
         DROP  RE                                                               
*                                                                               
BRSD3    CLI   PIGPRNT,C' '                                                     
         BE    BRSD4                                                            
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P3+1(9),=C'PARTNER='                                             
         MVC   P3+1(L'SP@PRTNR),SP@PRTNR                                        
         LA    RF,P3+1+L'SP@PRTNR-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'='                                                       
         MVC   P3+10(L'PIGPRNT),PIGPRNT                                         
         DROP  RE                                                               
BRSD4    DS    0C                                                               
         LA    R8,P3+13                                                         
         CLI   PIGPRNT,C' '                                                     
         BE    *+8                                                              
         LA    R8,P4+13                                                         
         CLI   P3+13,C' '                                                       
         BE    *+8                                                              
         LA    R8,P4+13                                                         
         CLI   PKGAREA,0                                                        
         BE    *+8                                                              
         LA    R8,P4+13                                                         
         ST    R8,FULL                                                          
         GOTO1 VCOMPRNT                                                         
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         L     RF,=A(DICSECT)                                                   
         USING DICSECT,RF                                                       
         CLI   PROFFTA,C'Y'        PRINT TODAYS ACTIVITY                        
         BNE   *+10                                                             
         MVC   P1+7(1),BDPAST                                                   
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
*---->   MVC   P2+28(7),=C'*SPILL*'                                             
         MVC   P2+28(L'SP7SPILL),SP7SPILL                                       
         DROP  RF                                                               
         BR    R9                                                               
         CLI   DETOPTS+2,0         SUPPRESS COST                                
         BER   R9                   YES                                         
         MVC   P2+30(10),BDPCOST+2                                              
         GOTO1 VGETCOVR                                                         
         DROP  RE                                                               
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         OC    BDNTAX,BDNTAX                                                    
         BZR   R9                                                               
         MVI   P2+41,C'T'                                                       
         SR    RF,RF                                                            
         ICM   RF,3,BDNTAX                                                      
         MVI   CURTAB+3,3                                                       
         CURED (RF),(5,P2+42),CURTAB,DMCB=CURDMCB,ALIGN=LEFT                    
         CLI   BDPPTYP,C' '                                                     
         BER   R9                                                               
         MVC   P2+48(1),BDPPTYP    REPOSITION PROG TYPE                         
         BR    R9                                                               
         DROP  R5                                                               
         DROP  RE                                                               
         LTORG                                                                  
         EJECT                                                                  
RQFIRST  NTR1  BASE=*,LABEL=*                                                   
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         MVC   SVQUEST,QUESTOR     SAVE REQUESTOR                               
         L     R1,ADAGY                                                         
         CLI   AGYPROF+14-AGYHDR(R1),C'Y' TEST AGY WANTS BUYERS' NAMES          
         BNE   *+8                                                              
         MVI   RQGETNAM,C'Y'       YES-GET BUYER/BILLER NAMES                   
*---->   MVC   STACAP(7),=C'STATION'                                            
         MVC   STACAP(L'SP@STATN),SP@STATN                                      
         MVI   QCOMPARE,C'A'                                                    
         CLI   QRERATE,C'I'                                                     
         BNE   *+8                                                              
         MVI   QCOMPARE,C'B'                                                    
RQ1      DS    0H                                                               
         MVC   MAXLINES,SVMAXLIN                                                
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   FOOT1,C' '                                                       
         BE    RQ1NOFT                                                          
         LLC   R0,MAXLINES                                                      
         SH    R0,=H'3'                                                         
         STC   R0,MAXLINES                                                      
         MVC   FOOT1,SPACES                                                     
RQ1NOFT  DS    0H                                                               
         SPACE 2                                                                
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         XC    SPOTPROF,SPOTPROF                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   PASS,0                                                           
         MVC   RTYPE,=C'BTS'       SET REPORT TYPE                              
         LA    R1,H11+44                                                        
         ST    R1,AHDATES                                                       
         MVI   SPACESW,1                                                        
         XC    DTOTSPT,DTOTSPT                                                  
         L     RF,ABTSDESC                                                      
         ST    RF,DDESC                                                         
         LA    RF,P1+44                                                         
         ST    RF,DSTAGRID                                                      
         LA    RF,P2+31                                                         
         ST    RF,PSTASPT                                                       
         LA    RF,P2+101                                                        
         ST    RF,PSTACOST                                                      
         LA    RF,P2+44                                                         
         ST    RF,PSTAGRID                                                      
         MVI   PENNYSW,1                                                        
         MVI   HALF,C'1'           DEFAULT MARKET TOTAL (SAL)                   
         SPACE 2                                                                
         CLC   QPROG,=C'D7'                                                     
         BE    M2SAL                                                            
         CLC   QPROG,=C'DX'                                                     
         BE    M2SAL                                                            
         CLC   QPROG,=C'CP'                                                     
         BE    M2SAL                                                            
         CLC   QPROG,=C'D4'                                                     
         BE    M2SAL                                                            
         MVI   HALF,C'3'           DEFAULT MARKET TOTAL (BTS)                   
         CLC   QPROG,=C'D6'                                                     
         BNE   M2BRS                                                            
         MVI   HALF,C'4'           DEFAULT MARKET TOAL (BDS)                    
         MVC   RTYPE,=C'BDS'                                                    
         B     *+10                                                             
*                                                                               
M2SAL    MVC   RTYPE,=C'SAL'                                                    
         LA    R1,H11+42                                                        
         ST    R1,AHDATES                                                       
         MVI   SPACESW,0                                                        
         L     RF,ASALDESC                                                      
         ST    RF,DDESC                                                         
         LA    RF,P1+42                                                         
         ST    RF,DSTAGRID                                                      
         LA    RF,P1+98                                                         
         ST    RF,DTOTSPT                                                       
         LA    RF,P2+97                                                         
         ST    RF,PSTASPT                                                       
         LA    RF,P3+100                                                        
         ST    RF,PSTACOST                                                      
         LA    RF,P2+42                                                         
         ST    RF,PSTAGRID                                                      
         MVI   PENNYSW,1                                                        
         MVI   HADOFLOW,C'N'                                                    
         SPACE 2                                                                
M2BRS    CLC   QPROG,=C'U3'                                                     
         BE    M2BRS1                                                           
         CLC   QPROG,=C'D2'                                                     
         BNE   M2RPTX                                                           
M2BRS1   MVI   HALF,C'2'           DEFAULT MARKET TOTAL (BRS)                   
         MVC   RTYPE,=C'BRS'                                                    
         LA    R1,H11+49                                                        
         ST    R1,AHDATES                                                       
         XC    DTOTSPT,DTOTSPT                                                  
         L     RF,ABRSDESC                                                      
         ST    RF,DDESC                                                         
         LA    RF,P1+49                                                         
         ST    RF,DSTAGRID                                                      
         LA    RF,P3+33                                                         
         ST    RF,PSTACOST                                                      
         LA    RF,P2+49                                                         
         ST    RF,PSTAGRID                                                      
         LA    RF,P2+31                                                         
         ST    RF,PSTASPT                                                       
         MVI   PENNYSW,0                                                        
         SPACE 2                                                                
M2RPTX   DS    0H                                                               
         DROP  R5                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
RQFRSTA  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         MVC   SVQOPT1,QOPT1                                                    
         SPACE 2                                                                
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
*                                                                               
         CLI   SVQOPT1,C'B'        SPECIAL REPORT                               
         BNE   *+8                                                              
         MVI   PROFMTR,C'1'                                                     
         CLI   SVQOPT1,C'A'                                                     
         BNE   *+8                                                              
         MVI   PROFMTR,C'4'                                                     
*                                                                               
         CLI   Q2MTR,C'A'          SET MARKET TOTAL REPORT FROM Q2MTR           
         BL    *+10                                                             
         MVC   PROFMTR(1),Q2MTR                                                 
*                                                                               
         TM    PROFMTR,X'0F'                                                    
         BNZ   *+10                                                             
         MVC   PROFMTR,HALF        SET MARKET TOTAL FROM PROFILE                
         L     RF,=A(SALSUM)       SET MARKET TOTAL REPORT                      
         CLI   PROFMTR,C'3'        WEEKLY                                       
         BE    *+8                                                              
         CLI   PROFMTR,C'5'        MONTHLY                                      
         BE    *+8                                                              
         CLI   PROFMTR,C'6'        WEEKLY/MONTHLY                               
         BNE   *+8                                                              
         L     RF,=A(BRSSUM)                                                    
         CLI   PROFMTR,C'2'                                                     
         BNE   *+8                                                              
         L     RF,=A(BTSSUM)                                                    
         CLI   PROFMTR,C'4'                                                     
         BNE   *+8                                                              
         L     RF,=A(BDSSUM)                                                    
         ST    RF,VSUMMRY                                                       
*                                                                               
         L     RF,=V(REPUDESC)                                                  
         ST    RF,VUDESC                                                        
*                                                                               
         MVC   MRPTTYP,PROFMTR                                                  
         MVC   CPPSW,SPOTPROF                                                   
         MVC   DETOPTS,=X'01010101'  SET DETAIL OPTIONS                         
*                                                                               
         CLC   QPROG,=C'CP'        EXCEPT FOR CP REPORT,                        
         BE    *+18                                                             
         CLI   QOPT1,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT1,PROFDPC       DETAIL PRINT CONTROL                         
*                                                                               
         CLI   SVQOPT1,C'A'        SPECIAL REPORT OPTION                        
         BE    *+8                                                              
         CLI   SVQOPT1,C'B'                                                     
         BNE   *+12                                                             
         LA    R6,5                                                             
         B     RQFRSTA1                                                         
*                                                                               
         TM    QOPT1,X'F0'                                                      
         BNO   M21                                                              
         PACK  DUB,QOPT1                                                        
         CVB   R6,DUB                                                           
RQFRSTA1 SLL   R6,2                                                             
         LA    R6,DETOPT(R6)                                                    
         MVC   DETOPTS,0(R6)                                                    
*                                                                               
M21      CLI   SPDWSW,C'Y'         TEST SUPPRESS COST AND CPP                   
         BE    *+8                                                              
         CLI   QOPT4,C'Y'          TEST SUPPRESS COST AND CPP                   
         BNE   *+10                                                             
         XC    DETOPTS+2(2),DETOPTS+2  YES                                      
*                                                                               
         MVC   SUMOPTS,=X'010101'  SET SUMMARY OPTIONS                          
         CLI   QOPT2,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT2,PROFMPC       MARKET PRINT CONTROL                         
*                                                                               
         CLI   SVQOPT1,C'A'        SPECIAL                                      
         BE    *+8                                                              
         CLI   SVQOPT1,C'B'        SPECIAL                                      
         BNE   *+12                                                             
         LA    R6,2                                                             
         B     M21AA                                                            
*                                                                               
         TM    QOPT2,X'F0'                                                      
         BNO   M2AA                                                             
         PACK  DUB,QOPT2                                                        
         CVB   R6,DUB                                                           
M21AA    MH    R6,=H'3'                                                         
         LA    R6,SUMOPT(R6)                                                    
         MVC   SUMOPTS,0(R6)                                                    
*                                                                               
M2AA     CLI   QOPT4,C'Y'          TEST SUPPRESS COST AND CPP                   
         BNE   *+8                                                              
         MVI   SUMOPTS+1,0         YES-SUPPRESS DOLLARS                         
*                                                                               
         CLC   QPROG,=C'CP'                                                     
         BE    M2D7A                                                            
         CLC   QPROG,=C'D7'                                                     
         BE    *+14                                                             
         CLC   QPROG,=C'DX'                                                     
         BNE   M2D7X                                                            
         XC    DETOPTS,DETOPTS                                                  
         CLI   QOPT4,C'Y'           OPTION TO SUPPRESS COST                     
         BE    M2D7B                                                            
*                                                                               
M2D7A    CLI   QPWCV,C'Y'                                                       
         BE    *+10                                                             
         MVC   DETOPTS,=X'00000100' PRINT COST                                  
*                                                                               
M2D7B    XC    SUMOPTS,SUMOPTS                                                  
         MVI   PROFMSR,C'S'                                                     
*                                                                               
M2D7X    MVI   SUBPSW,0                                                         
         MVC   MSOPT,SPACES        INITIALIZE MS OPTIONS                        
         MVC   SVOPTS,QOPT1        SAVE REPORT OPTIONS                          
         MVC   SUBPROG1,=C'M2'                                                  
         MVI   PROFMSP,C'M'                                                     
         CLI   PROFMSP,C'0'                                                     
         BNE   *+12                                                             
         MVI   PROFBMS,C'0'                                                     
         MVI   PROFPMS,C'0'                                                     
         CLC   QPRD,=C'POL'        POL REQUEST                                  
         BNE   M2AA1                NO                                          
         TM    PROFPMS,X'0F'        YES - ANY MEDIA SUMMARY                     
         BZ    M2AA3                                                            
         MVC   SUBPROG1(1),PROFMSP                                              
         MVC   SUBPROG1+1(1),PROFPMS                                            
         B     M2AA2                                                            
M2AA1    TM    PROFBMS,X'0F'                                                    
         BZ    M2AA3                                                            
         MVC   SUBPROG1(2),PROFMSP                                              
M2AA2    MVI   SUBPSW,1                                                         
*                                                                               
M2AA3    CLI   QOPT4,C'Y'          TEST OPTION TO SUPPRESS DOLLARS              
         BNE   M2AA4                                                            
         CLC   SUBPROG1,=C'M2'     YES-PASS THROUGH TO M2,M3 AND M4             
         BL    M2AA4                                                            
         CLC   SUBPROG1,=C'M4'                                                  
         BH    M2AA4                                                            
         MVI   MSOPT+3,C'Y'                                                     
*                                                                               
M2AA4    MVC   WORK(12),=CL12'S000'   READ MS PROFILE                           
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVC   WORK+2(2),SUBPROG1                                               
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  R6                                                               
         GOTO1 GETPROF,DMCB,WORK,MSPROF,DATAMGR                                 
         MVC   WORK(4),=C'SD2A'    READ SECONDARY PROFILE                       
         MVC   WORK+1(2),QPROG                                                  
         NI    WORK,X'BF'                                                       
         XC    D2APROF,D2APROF                                                  
         GOTO1 GETPROF,DMCB,WORK,D2APROF,DATAMGR                                
*                                                                               
         MVI   IDXRPT,C'N'                                                      
         MVI   HORIDX,C'N'                                                      
         MVI   SLSUMSW,C'N'                                                     
         MVI   DPSUMSW,C'N'                                                     
         MVI   RAWGRP,C'N'                                                      
         NI    RQOPTS,X'FF'-RQOPTS_HDEND                                        
         CLI   QCBLNET,C'A'       TEST FILTERING ON NETWORK                     
         BNL   M2AA5              YES - NO SYSCODE ONLY TOTALS  !               
         CLI   D2APROF+10,C'Y'                                                  
         BNE   *+8                                                              
         OI    RQOPTS,RQOPTS_HDEND                                              
*                                                                               
M2AA5    CLC   QAGY,=C'WI'         ONLY FOR WESTERN                             
         BE    *+10                                                             
         CLC   QAGY,=C'WJ'                                                      
         BNE   WIPROFX                                                          
         CLI   Q2NET,C' '          ALREADY SET - LEAVE ALONE                    
         BH    WIPROF4                                                          
         TM    D2APROF+8,X'01'      NET DOLLARS                                 
         BZ    *+8                                                              
         MVI   Q2NET,C'Y'                                                       
*                                                                               
         TM    D2APROF+8,X'04'     NET DOLS ADJUSTED BY BILL FORM               
         BZ    *+12                                                             
         MVI   Q2NET,C'B'                                                       
*                                                                               
WIPROF4  CLI   QPWCV,C' '          PW OPTION ALREADY SET - LEAVE ALONE          
         BH    WIPROFX                                                          
         TM    D2APROF+8,X'02'                                                  
         BZ    WIPROFX                                                          
         MVI   QPWCV,C'Y'          SET TO DO PW COSTS                           
         MVI   SPDWSW,C'Y'                                                      
WIPROFX  DS    0H                                                               
*                                                                               
         CLI   D2APROF+4,0                                                      
         BE    *+8                                                              
         MVI   SLSUMSW,C'Y'                                                     
         CLI   D2APROF+5,0                                                      
         BE    *+8                                                              
         MVI   DPSUMSW,C'Y'                                                     
         TM    D2APROF+5,X'01'                                                  
         BZ    *+8                                                              
         MVI   IDXRPT,C'Y'                                                      
         TM    D2APROF+5,X'02'                                                  
         BZ    *+8                                                              
         MVI   RAWGRP,C'Y'                                                      
         TM    D2APROF+5,X'04'                                                  
         BZ    *+8                                                              
         MVI   HORIDX,C'Y'                                                      
         TM    D2APROF+4,X'01'                                                  
         BZ    *+8                                                              
         MVI   IDXRPT,C'Y'                                                      
         TM    D2APROF+4,X'02'                                                  
         BZ    *+8                                                              
         MVI   RAWGRP,C'Y'                                                      
         TM    D2APROF+4,X'04'                                                  
         BZ    *+8                                                              
         MVI   HORIDX,C'Y'                                                      
*                                                                               
         CLI   Q2MCOM,C' '         TEST PRINT MCOM OPTION SET                   
         BNH   *+10                                                             
         MVC   D2APROF+2(1),Q2MCOM    YES-SET OPTION IN D2A PROFILE             
         SPACE 2                                                                
         CLI   QRERATE,C'I'        FIX DEFAULT DATA COMPARE                     
         BNE   AFFCOMP                                                          
         CLI   MSPROF,C'B'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'A'                                                      
         CLI   MSPROF,C'D'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'C'                                                      
         CLI   MSPROF,C'F'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'E'                                                      
         B     COMPOK                                                           
         SPACE 2                                                                
AFFCOMP  CLI   MSPROF,C'A'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'B'                                                      
         CLI   MSPROF,C'C'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'D'                                                      
         CLI   MSPROF,C'E'                                                      
         BNE   *+8                                                              
         MVI   MSPROF,C'F'                                                      
COMPOK   DS    0H                                                               
         XC    MSBFHOOK,MSBFHOOK                                                
*                                                                               
         MVC   SUBPROG2,=C'01'                                                  
         L     R6,VSUBPARA                                                      
         ST    R6,CURPGPTR         SET TO START OF SAVE AREA                    
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RE,DMCB+4                                                        
         ST    RE,SVPH01                                                        
         MVC   CURPH01(4),CURPGPTR                                              
         MVC   CURPH01+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         MVC   SUBPROG2,=C'02'                                                  
*                                                                               
         CLC   =C'SUB02=T',QUESTOR   SUBPROGRAM TESTING                         
         BNE   *+8                                                              
         MVI   SUBPROGT,C'T'                                                    
*                                                                               
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
*                                                                               
         CLC   =C'SUB02=T',QUESTOR   SUBPROGRAM TESTING                         
         BNE   *+8                                                              
         MVI   SUBPROGT,C' '                                                    
*                                                                               
         L     RE,DMCB+4                                                        
         ST    RE,SVPH02                                                        
         MVC   CURPH02(4),CURPGPTR                                              
         MVC   CURPH02+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         MVC   SUBPROG2,=C'04'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         MVC   SVPH04,DMCB+4                                                    
         MVC   CURPH04(4),CURPGPTR                                              
         MVC   CURPH04+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         LA    R9,3                                                             
         LA    R6,CURPH01                                                       
SAVPRGM  L     RF,0(R6)                                                         
         L     RE,8(R6)                                                         
         L     R1,4(R6)                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    R6,12(R6)                                                        
         BCT   R9,SAVPRGM                                                       
         L     RF,VSVMDBLK                                                      
         L     RE,MEDBUFF                                                       
         LA    R1,980                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         TM    QSTA,X'F0'          ALLOW MARKET SUMMARY FOR MKT GROUPS          
         BO    RQFRSTAX                                                         
         CLC   QSTA(3),=C'   '     DEACTIVATE MARKET SUMMARY IF                 
         BE    *+10                WE ARE NOT DOING ENTIRE MARKET               
         CLC   QSTA(3),=C'ALL'                                                  
         BE    *+8                                                              
         MVI   SUBPSW,0                                                         
RQFRSTAX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
EFRSTC   NTR1  BASE=*,LABEL=*                                                   
         USING SPD2WK,R2                                                        
* SET NUMBER OF LEVELS                                                          
         LA    RF,LVCNTRL                                                       
         LA    RE,5                                                             
         NI    0(RF),X'7F'                                                      
         LA    RF,1(RF)                                                         
         BCT   RE,*-8                                                           
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         L     RE,BUFFROWS                                                      
         BCTR  RE,0                                                             
         MH    RE,=H'4'                                                         
         LA    RE,LVCNTRL(RE)                                                   
         OI    0(RE),X'80'                                                      
         DROP  RF                                                               
         SPACE 2                                                                
* CREATE WEEKLY TABLES FOR ALL REPORTS                                          
         XC    TAXAMT,TAXAMT                                                    
         MVC   MEDNUMWK,=F'60'     CREATE BTS TABLES                            
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'5'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
EFRSTA   DS    0H                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   SVRDTE,MEDPERD      SAVE REQUEST DATES                           
         LA    R5,MEDMON01                                                      
         LA    R6,12                                                            
         LA    R8,0                                                             
EFRST1   CLI   0(R5),0                                                          
         BE    EFRST2                                                           
         LA    R8,1(R8)                                                         
         LA    R5,12(R5)                                                        
         BCT   R6,EFRST1                                                        
EFRST2   ST    R8,ACTMO                                                         
         SPACE 2                                                                
* DETERMINE DATES FOR EACH PASS                                                 
*                   R6 = PASS TABLE ENTRY                                       
*                   R7 = PASS TABLE ENTRY LENGTH                                
         LA    R6,PASSTAB                                                       
         USING PASSTABD,R6                                                      
         LA    R5,PASSEND          GET ENTRY LENGTH                             
         SR    R5,R6                                                            
         LR    RF,R5                                                            
         SLL   RF,2                X 4                                          
         LR    RE,R6                                                            
         XCEF                                                                   
         MVI   MAXPASS,0                                                        
         L     RE,MEDAFRST         CHECK FOR MORE THAN 14 WEEKS                 
         SR    RF,RF                                                            
         CLI   MEDDAILY,C'Y'                                                    
         BE    PTAB1                                                            
         CLC   RTYPE,=C'BDS'                                                    
         BE    PTAB1                                                            
PTABA    C     RE,MEDALAST                                                      
         BH    PTABA1                                                           
         OC    0(2,RE),0(RE)                                                    
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,12(RE)                                                        
         B     PTABA                                                            
*                                                                               
PTABA1   CH    RF,=H'14'           WILL IT REQUIRE ONLY ONE PASS                
         BH    PTAB1                NO                                          
         MVC   MEDNUMWK,=F'14'      YES - GENERATE NEW DATES                    
         MVC   MEDNUMMO,=F'4'                                                   
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   PASSSD,QSTART        SAVE PASS 0 TABLE                           
         MVC   PASSED,QEND                                                      
         MVC   WORK(12),QSTART                                                  
         MVC   PASSP1,MEDNUMWK                                                  
         MVC   PASSP2,MEDWEEKS                                                  
         MVC   PASSP3,MEDMON01                                                  
         MVC   PASSP4,MEDPERD                                                   
         MVI   MAXPASS,1                                                        
         B     PTABX                                                            
         SPACE 2                                                                
* SET UP AND SAVE QUARTER TABLES                                                
PTAB1    CLC   RTYPE,=C'BDS'                                                    
         BNE   PTAB1A                                                           
         BAS   RE,VBDSDATE                                                      
PTAB1A   MVC   P(4),MEDQRT01       SET QUARTER DATES                            
         GOTO1 DATCON,DMCB,(X'02',P),(X'03',PASSSD3)                            
         GOTO1 DATCON,DMCB,(X'02',P+2),(X'03',PASSED3)                          
         MVC   P+4(4),MEDQRT02                                                  
         MVC   P+8(4),MEDQRT03                                                  
         MVC   P+12(4),MEDQRT04                                                 
         LA    R0,4                                                             
         MVC   WORK(12),QSTART     SAVE REQUEST DATES                           
         MVC   MEDNUMWK,=F'14'                                                  
         MVC   MEDNUMMO,=F'3'                                                   
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         LA    R8,P                SET TO FIRST QUARTER                         
PTAB2    OC    0(4,R8),0(R8)                                                    
         BZ    PTABX                                                            
         GOTO1 DATCON,DMCB,(X'02',(R8)),PASSSD                                  
         GOTO1 DATCON,DMCB,(X'02',2(R8)),PASSED                                 
         MVC   QSTART,PASSSD                                                    
         MVC   QEND,PASSED                                                      
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   PASSP1,MEDNUMWK                                                  
         MVC   PASSP2,MEDWEEKS                                                  
         MVC   PASSP3,MEDMON01                                                  
         MVC   PASSP4,MEDPERD                                                   
         AR    R6,R5               SET TO NEXT PASS TABLE ENTRY                 
         LA    R8,4(R8)                                                         
         SR    RF,RF                                                            
         IC    RF,MAXPASS                                                       
         LA    RF,1(RF)                                                         
         STC   RF,MAXPASS                                                       
         BCT   R0,PTAB2                                                         
PTABX    MVC   QSTART(12),WORK                                                  
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
VBDSDATE NTR1                                                                   
         L     R5,MEDAFRST         FIX QUARTER DATES FOR BDS                    
         LA    R4,4                 (BIWEEKLY DATES)                            
         LA    R8,MEDQRT01                                                      
         MVC   SVQEND,QEND                                                      
         GOTO1 DATCON,DMCB,QEND,(X'02',HALF)                                    
BDSDFIX  CLC   HALF,0(R5)          FIX MEDDATE DATES                            
         BNL   *+10                                                             
         XC    0(4,R5),0(R5)                                                    
         CLC   HALF,2(R5)                                                       
         BNL   *+10                                                             
         MVC   2(2,R5),HALF                                                     
         LA    R5,12(R5)                                                        
         C     R5,MEDALAST                                                      
         BNH   BDSDFIX                                                          
         L     R5,MEDAFRST                                                      
BDSDATE1 C     R5,MEDALAST                                                      
         BH    BDSDATEX                                                         
         OC    0(4,R5),0(R5)                                                    
         BNZ   *+12                                                             
         LA    R5,12(R5)                                                        
         B     BDSDATE1                                                         
*                                                                               
         LA    R6,14               SET UP FOR FOUTEEN DAY PERIOD                
         MVC   0(4,R8),0(R5)                                                    
         B     *+8                                                              
*                                                                               
BDSDATE2 LA    R5,12(R5)           AND LIMIT THEM TO 4                          
         C     R5,MEDALAST                                                      
         BH    BDSDATEX                                                         
         OC    0(4,R5),0(R5)                                                    
         BZ    BDSDATE2                                                         
         GOTO1 DATCON,DMCB,(X'02',2(R5)),QEND                                   
         MVC   2(2,R8),2(R5)                                                    
         MVC   MEDPERD+2(2),2(R5)                                               
         BCT   R6,BDSDATE2                                                      
         LA    R8,12(R8)                                                        
         LA    R5,12(R5)                                                        
         BCT   R4,BDSDATE1                                                      
BDSDATEX CLC   SVQEND,QEND                                                      
         BNH   *+10                                                             
         MVC   QSTAUTO,=C'  '                                                   
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
GETCOVR  NTR1  BASE=*,LABEL=*                                                   
         USING SPD2WK,R2                                                        
         CLI   DETOPTS+2,1         PRINT COSTS                                  
         BNE   GETCOVRX             NO - EXIT                                   
         LA    RE,COVRHLD          INITIALIZE                                   
         LA    RF,600                                                           
         XCEF                                                                   
         MVI   OVRCNT,0                                                         
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         L     R5,MEDAFRST                                                      
         MVC   STRDTE,0(R5)                                                     
         L     R5,MEDALAST                                                      
         MVC   ENDDTE,2(R5)                                                     
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING REGELEM,R5                                                       
*                                                                               
         LA    R8,COVRHLD                                                       
GCONXT   LLC   R0,RLEN                                                          
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    GETCOVRP                                                         
         CLI   RCODE,10                                                         
         BL    GCONXT                                                           
         CLI   RCODE,13                                                         
         BH    GCONXT                                                           
         TM    RSTATUS,X'20'                                                    
         BZ    GCONXT                                                           
         TM    RSTATUS,X'C2'       MINUS OR MISSED                              
         BNZ   GCONXT                                                           
         CLC   RDATE,STRDTE                                                     
         BL    GCONXT                                                           
         CLC   RDATE,ENDDTE                                                     
         BH    GCONXT                                                           
         CLI   MEDBRAND,X'FF'      POL PRODUCT                                  
         BE    GETCO2               OK                                          
         CLI   RLEN,14             UNALLOCATED                                  
         BL    GCONXT               BYPASS                                      
         SR    RE,RE                                                            
         IC    RE,MEDBRAND                                                      
         SR    RF,RF                                                            
         IC    RF,RPPRD                                                         
         CLI   Q2TRADE,C'Y'        TEST COMBINE CASH/TRADE                      
         BNE   GETCO1                                                           
         N     RE,=X'0000007F'                                                  
         N     RF,=X'0000007F'                                                  
GETCO1   CR    RE,RF               COMPARE BRANDS                               
         BNE   GCONXT               BYPASS                                      
         SPACE 2                                                                
GETCO2   MVC   WORK(2),RDATE       SAVE COST OVERRIDES                          
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         TM    BDSTAT,X'80'                                                     
         BZ    GETCO2A                                                          
         MVC   WORK+2(3),RPCOST    GET COST OVRRDE AND NO OF SPOTS              
         MVC   WORK+5(1),RPCOST                                                 
         NI    WORK+2,B'00000011'                                               
         LLC   RE,WORK+5                                                        
         SRL   RE,2                                                             
         STC   RE,WORK+5                                                        
         DROP  RE                                                               
         B     GETCO2X                                                          
         SPACE 2                                                                
GETCO2A  DS    0C                                                               
         MVC   WORK+2(3),RPCOST                                                 
         MVI   WORK+5,1                                                         
GETCO2X  DS    0C                                                               
         LA    RE,COVRHLD                                                       
GETCO3   CLI   0(RE),0             CHECK FOR PREVIOUS                           
         BE    GETCO4                                                           
         CLC   WORK(5),0(RE)                                                    
         BE    GETCO5                                                           
         LA    RE,6(RE)                                                         
         B     GETCO3                                                           
         SPACE 2                                                                
GETCO4   MVC   0(6,RE),WORK        INSERT INTO LIST                             
         B     GCONXT                                                           
         SPACE 2                                                                
GETCO5   LLC   RF,5(RE)            ADD SPOT COUNT                               
         LLC   R1,WORK+5                                                        
         AR    RF,R1                                                            
         STC   RF,5(RE)                                                         
         B     GCONXT                                                           
         EJECT                                                                  
* PRINT OUT COST OVERRIDES                                                      
         DROP  R5                                                               
GETCOVRP LA    R4,P                                                             
         MVI   COVRFRST,1                                                       
         LA    R5,COVRHLD                                                       
         MVI   OVRCNT,0                                                         
GCOP1    CLC   1(131,R4),0(R4)     FIRST UNUSED LINE                            
         BE    GCOP2                                                            
         LA    R4,132(R4)                                                       
         LA    R0,P14                                                           
         CR    R4,R0                                                            
         BH    GETCOVRX            IF NO MORE PRINT LINES, STOP !               
         CLI   0(R4),0                                                          
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         ST    R4,APL                                                           
         B     GCOP1                                                            
         SPACE 2                                                                
GCOP2    DS    0H                                                               
         LA    R0,P14+131                                                       
         CR    R4,R0               MAKE SURE STILL IN PRINT LINES               
         BNL   GCOP6                                                            
         L     R6,=A(DICSECT)                                                   
         USING DICSECT,R6                                                       
         CLI   0(R5),0                                                          
         BE    GETCOVRX                                                         
         CLI   COVRFRST,1                                                       
         BNE   *+14                                                             
*---->   MVC   0(20,R4),=C'***COST OVERRIDES***'                                
         MVC   0(L'SP@COST2,R4),SP@COST2                                        
         MVI   COVRFRST,0                                                       
         CLI   OVRCNT,0                                                         
         BNE   *+8                                                              
         LA    R4,21(R4)                                                        
         MVI   132(R4),0                                                        
         GOTO1 DATCON,DMCB,(X'02',0(R5)),(X'04',0(R4))                          
         EDIT  (1,5(R5)),(2,6(R4))                                              
*                                                                               
*---->   MVC   9(4,R4),=C'SPOT'                                                 
         MVC   9(L'SP4SPOTS,R4),SP4SPOTS                                        
         CLI   5(R5),1                                                          
         BE    *+10                                                             
*---->   MVC   9(5,R4),=C'SPOTS'                                                
         MVC   9(L'SP5SPOTS,R4),SP5SPOTS                                        
         DROP  R6                                                               
*                                                                               
         L     RE,ADBUY                                                         
         AHI   RE,BDCIND2-BUYREC                                                
         MVI   CURTAB+3,2                                                       
         TM    0(RE),X'10'         TEST COST IN DOLLARS                         
         BZ    *+8                                                              
         MVI   CURTAB+3,0                                                       
*                                                                               
         TM    0(RE),X'20'         TEST CANADIAN BUY                            
         BZ    GCOP4                                                            
         TM    0(RE),X'01'         IS THE RATE IN PENNIES (CAN NET)             
         BZ    GCOP4               NO                                           
         MVI   CURTAB+3,2                                                       
*                                                                               
GCOP4    MVI   FULL,0                                                           
         MVC   FULL+1(3),2(R5)                                                  
         CURED (4,FULL),(9,15(R4)),CURTAB,DMCB=CURDMCB,ALIGN=LEFT               
         LA    R4,25(R4)                                                        
         LLC   RE,OVRCNT                                                        
         LA    RE,1(RE)                                                         
         STC   RE,OVRCNT                                                        
         LA    R5,6(R5)                                                         
         CLI   OVRCNT,2                                                         
         BNH   GCOP2                                                            
         MVI   OVRCNT,0                                                         
         L     R4,APL                                                           
         LA    R4,132(R4)                                                       
         ST    R4,APL                                                           
         LA    RE,P14                                                           
         CR    R4,RE                                                            
         BNE   GCOP2                                                            
GCOP6    GOTO1 REPORT                                                           
         LA    R4,P                                                             
         ST    R4,APL                                                           
         B     GCOP2                                                            
         SPACE 2                                                                
GETCOVRX DS    0C                                                               
         EJECT                                                                  
         L     R5,ADBUY                                                         
         CLI   3(R5),X'FF'         POL BUY                                      
         BNE   GPGPRNX              NO - EXIT                                   
*                                                                               
         MVI   OVRCNT,0                                                         
         LA    RE,COVRHLD          YES - LOOK FOR PIGGYBACKS                    
         LA    RF,600                                                           
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   PROGPROF+2,C'Y'     TEST SUPPRESS PIGGYBACKS                     
         BE    GPGPRNX             YES - EXIT                                   
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         L     R5,MEDAFRST                                                      
         MVC   STRDTE,0(R5)                                                     
         L     R5,MEDALAST                                                      
         MVC   ENDDTE,2(R5)                                                     
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING REGELEM,R5                                                       
GPGNXT   LLC   RE,RLEN                                                          
         AR    R5,RE                                                            
         CLI   0(R5),0             END                                          
         BE    GPGPRN               YES - PRINT PIGGYBACKS                      
         CLI   RCODE,10                                                         
         BL    GPGNXT                                                           
         CLI   RCODE,13                                                         
         BH    GPGNXT                                                           
         CLI   RLEN,18                                                          
         BNE   GPGNXT                                                           
         CLC   RDATE,STRDTE                                                     
         BL    GPGNXT                                                           
         CLC   RDATE,ENDDTE                                                     
         BH    GPGNXT                                                           
         L     RF,SVPSLIST                                                      
         CLI   1(RF),0             ACCEPT ANY SPOT LENGTHS                      
         BE    *+14                                                             
         CLC   RPTIME,1(RF)                                                     
         BNE   GPGNXT                                                           
         LLC   RF,RLEN                                                          
         SHI   RF,10                                                            
         SRL   RF,2                                                             
         LR    R0,RF                                                            
         LA    RE,RPPRD                                                         
         CLI   MEDBRAND,X'FF'                                                   
         BE    GPG1A                                                            
GPG1     CLC   MEDBRAND,0(RE)      REQUESTED BRAND                              
         BE    GPG1A                YES - SAVE IT                               
         LA    RE,4(RE)             NO - CHECK NEXT                             
         BCT   RF,GPG1                                                          
         B     GPGNXT                                                           
GPG1A    LA    RE,RPPRD                                                         
         LA    RF,WORK                                                          
         XC    WORK,WORK                                                        
GPG1B    MVC   0(1,RF),0(RE)                                                    
         MVC   1(1,RF),1(RE)                                                    
         LA    RF,2(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,GPG1B                                                         
         MVI   WORK+8,1                                                         
         LA    RE,COVRHLD                                                       
GPG2     CLI   0(RE),0                                                          
         BE    GPG4                                                             
         CLC   WORK(8),0(RE)                                                    
         BE    GPG5                                                             
         LA    RE,9(RE)                                                         
         B     GPG2                                                             
         SPACE 2                                                                
GPG4     MVC   0(9,RE),WORK        INSERT INTO LIST                             
         B     GPGNXT                                                           
         SPACE 2                                                                
GPG5     LLC   RF,8(RE)            BUMP SPOT COUNT                              
         LA    RF,1(RF)                                                         
         STC   RF,8(RE)                                                         
         B     GPGNXT                                                           
         EJECT                                                                  
* PRINT OUT POOL PIGGYBACKS                                                     
         DROP  R5                                                               
GPGPRN   LA    R4,P                                                             
         MVI   COVRFRST,1                                                       
         MVI   OVRCNT,0                                                         
         LA    R5,COVRHLD                                                       
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
GPGPRN1  CLC   1(131,R4),0(R4)     FIRST UNUSED LINE                            
         BE    GPGPRN2                                                          
         LA    R4,132(R4)                                                       
         CLI   0(R4),0                                                          
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         ST    R4,APL                                                           
         B     GPGPRN1                                                          
         SPACE 2                                                                
GPGPRN2  CLI   0(R5),0                                                          
         BE    GPGPRNX                                                          
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   COVRFRST,1                                                       
         BNE   *+14                                                             
*---->   MVC   0(20,R4),=C'*****PIGGYBACKS*****'                                
         MVC   0(L'SP@PIGGB,R4),SP@PIGGB                                        
         DROP  RE                                                               
         MVI   COVRFRST,0                                                       
         CLI   OVRCNT,0                                                         
         BNE   *+8                                                              
         LA    R4,25(R4)                                                        
         MVI   132(R4),0                                                        
         LR    R1,R4               PRINT PARTNERS                               
         LR    R3,R5                                                            
GPGPRN3  CLI   0(R3),0                                                          
         BE    GPGPRN4                                                          
         LA    RF,CLIST                                                         
GPGPRN3A CLC   0(1,R3),3(RF)       GET BRAND                                    
         BE    GPGPRN3B                                                         
         LA    RF,4(RF)                                                         
         B     GPGPRN3A                                                         
GPGPRN3B MVC   0(3,R1),0(RF)                                                    
         MVI   3(R1),C'-'                                                       
         LLC   RE,1(R3)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(2,R1),DUB+6(2)                                                 
         MVI   6(R1),C'/'                                                       
         LA    R1,7(R1)                                                         
         LA    R3,2(R3)                                                         
         B     GPGPRN3                                                          
         SPACE 2                                                                
GPGPRN4  BCTR  R1,0                                                             
         MVI   0(R1),C' '                                                       
         LA    R4,15(R4)                                                        
         LA    R5,9(R5)                                                         
         LLC   RE,OVRCNT                                                        
         AHI   RE,1                                                             
         STC   RE,OVRCNT                                                        
         CLI   OVRCNT,6                                                         
         BNH   GPGPRN2                                                          
         MVI   OVRCNT,0                                                         
         L     R4,APL                                                           
         LA    R4,132(R4)                                                       
         ST    R4,APL                                                           
         LA    R0,P14                                                           
         CR    R4,R0                                                            
         BNH   GPGPRN2             PRINT ONLY IF THERE IS ROOM                  
*                                                                               
GPGPRNX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
GETCAP   NTR1  BASE=*,LABEL=*                                                   
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING PKGELEM,R5                                                       
         XC    PKGAREA,PKGAREA                                                  
GETCAP1  CLI   PKGCODE,5                                                        
         BE    GETCAP2                                                          
         CLI   PKGCODE,0                                                        
         BE    GETCAP4                                                          
         SR    RE,RE                                                            
         IC    RE,PKGLEN                                                        
         AR    R5,RE                                                            
         B     GETCAP1                                                          
         SPACE 2                                                                
GETCAP2  DS    0H                                                               
         L     R6,=A(DICSECT)                                                   
         USING DICSECT,R6                                                       
         MVC   BYTE,PKGIND                                                      
         NI    BYTE,X'0F'          DROP2-BYTE LINE FLAG                         
         CLI   BYTE,1                                                           
         BNE   *+14                                                             
*---->   MVC   PKGAREA(7),=C'PKG MST'                                           
         MVC   PKGAREA(L'SP@PKMST),SP@PKMST                                     
         B     GETCAPX                                                          
         CLI   BYTE,3                                                           
         BNE   *+14                                                             
*---->   MVC   PKGAREA(7),=C'ORB MST'                                           
         MVC   PKGAREA(L'SP@ORMST),SP@ORMST                                     
         B     GETCAPX                                                          
         CLI   BYTE,5                                                           
         BNE   *+14                                                             
*---->   MVC   PKGAREA(7),=C'REV MST'                                           
         MVC   PKGAREA(L'SP@RVMST),SP@RVMST                                     
         B     GETCAPX                                                          
         CLI   BYTE,7                                                           
         BE    GETCAPX                                                          
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         CLI   BDMGDATE,X'C0'                                                   
         BH    GETCAP4                                                          
         DROP  RE                                                               
         CLI   BYTE,8                                                           
         BNE   GETCAP3                                                          
         CLI   PNOMKGD,C'Y'                                                     
         BE    GETCAPX                                                          
*---->   MVC   PKGAREA(4),=C'*MG*'                                              
         MVC   PKGAREA(L'SP@MG),SP@MG                                           
         B     *+10                                                             
GETCAP3  DS    0H                                                               
*---->   MVC   PKGAREA(4),=C'MST='                                              
         MVC   PKGAREA(L'SP@MST),SP@MST                                         
         LLC   R0,PKGLINES                                                      
         TM    PKGIND,X'10'        TEST 2-BYTE LINE NUMBERS                     
         BZ    *+8                                                              
         ICM   R0,3,PKGLINES                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PKGAREA+4(3),DUB                                                 
         CLI   BYTE,8                                                           
         BNE   GETCAPX                                                          
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         GOTO1 DATCON,DMCB,(X'02',BDMGDATE),(X'08',PKGAREA+8)                   
         B     GETCAPX                                                          
*                                                                               
GETCAP4  L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         CLI   PNOMKGD,C'Y'                                                     
         BE    GETCAPX                                                          
         CLI   BDMGDATE,X'C0'                                                   
         BNH   GETCAPX                                                          
         L     RE,=A(MGTABLE)                                                   
         CLI   0(RE),C'A'                                                       
         BL    GETCAPX                                                          
         MVC   PKGAREA(7),=C'**MKGD '                                           
         MVC   PKGAREA+7(6),=C'GROUP '                                          
         MVC   PKGAREA+13(2),BDMGDATE                                           
         MVI   PKGAREA+15,C'*'                                                  
         MVC   PKGAREA+13(2),0(RE)                                              
GETCAPX  DS    0H                                                               
         DROP  R5,R6,RE                                                         
         EJECT                                                                  
GETPIG   L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING PBELEM,R5                                                        
         LR    R9,R5                                                            
         USING BDELEM,R9                                                        
         XC    PIGAREA,PIGAREA                                                  
         MVC   PIGPRNT,SPACES                                                   
         CLI   BDTIME,0            FAST CHECK FOR PIGGYBACKS                    
         BE    GETPIGX                                                          
         SPACE 2                                                                
GETPIGA  CLI   0(R5),0                                                          
         BE    GETPIGX                                                          
         CLI   0(R5),X'04'                                                      
         BE    GETPIGA1                                                         
         LLC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     GETPIGA                                                          
GETPIGA1 CLI   KEY+10,0                                                         
         BE    GETPIG1B                                                         
         CLI   KEY+10,X'FF'        BYPASS IF POL                                
         BE    GETPIGX                                                          
         LA    R3,PIGAREA                                                       
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         LA    RF,CLIST                                                         
         CLC   KEY+10(1),3(RF)                                                  
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     *-14                                                             
         MVC   PIGAREA(3),0(RF)                                                 
         MVI   PIGAREA+3,C'/'                                                   
         LLC   R0,BDTIME                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(2,R3),DUB+6(2)                                                 
         MVI   6(R3),C'-'                                                       
         SR    R0,R0               SET PARTNER ESTIMATE                         
         IC    R0,KEY+11                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PIGAREA+7(3),DUB+6(2)                                            
         MVI   PIGAREA+10,C'A'                                                  
         LA    R3,PIGAREA+8                                                     
         MVC   PIGPRNT,PIGAREA                                                  
         B     GETPIGX                                                          
GETPIG1B LA    R3,PIGAREA                                                       
         LA    R7,PIGAREAL         GET PARTNER LENGTH                           
         SR    R4,R4                                                            
         IC    R4,PBLEN                                                         
         SH    R4,=H'2'                                                         
         LA    R1,2(R5)                                                         
         LA    R6,3                                                             
GETPIGB1 MVC   0(1,R7),2(R1)                                                    
         MVC   0(3,R3),4(R1)                                                    
         MVI   3(R3),C'/'                                                       
         LLC   R0,2(R1)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(2,R3),DUB+6(2)                                                 
         MVI   6(R3),C'-'                                                       
         LLC   R0,1(R1)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  7(3,R3),DUB+6(2)                                                 
         MVI   10(R3),C'P'                                                      
         MVC   PIGPRNT,PIGAREA                                                  
GETPIGX  DS    0H                                                               
         DROP  RE                                                               
         EJECT                                                                  
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING COMELEM,R5                                                       
         LA    RE,COMAREA                                                       
         LA    RF,400                                                           
         XCEF                                                                   
GETCOM2  CLI   CMCODE,0                                                         
         BE    GETCOMX                                                          
         CLI   CMCODE,X'66'                                                     
         BE    GETCOM4                                                          
GETCOM3  SR    R0,R0                                                            
         IC    R0,CMLEN                                                         
         AR    R5,R0                                                            
         B     GETCOM2                                                          
*                                                                               
GETCOM4  LA    R7,PROGPROF                                                      
         USING PROFDSCT,R7                                                      
         CLC   QPROG,=C'D7'        D7 CAN HAVE A REQUEST OVERRIDE               
         BE    *+14                                                             
         CLC   QPROG,=C'DX'                                                     
         BNE   *+18                                                             
         CLI   Q2USER+15,C' '                                                   
         BNH   *+10                                                             
         MVC   PROFCC,Q2USER+15                                                 
         CLI   PROFCC,C'N'         VALUE ONLY SET FROM Q2USER (COM=NO)          
         BE    GETCOM3             ZAP ALL COMMNETS                             
         CLI   PROFCC,C'1'                                                      
         BNE   GETCOM5                                                          
         CLI   CMDATA,C'$'         PRINT ONLY ACCOUNTING COMMENTS               
         BE    *+8                                                              
         CLI   CMNUM,4                                                          
         BE    *+8                                                              
         CLI   CMNUM,5                                                          
         BNE   GETCOM3                                                          
GETCOM5  CLI   PROFCC,C'2'                                                      
         BNE   *+14                                                             
         CLC   CMDATA(8),=C'COMMENT-'                                           
         BNE   GETCOM3                                                          
         CLC   =C'D7',QPROG        D7/DX PRINT X- COMMENTS                      
         BE    GETCOM7                                                          
         CLC   =C'DX',QPROG                                                     
         BE    GETCOM7                                                          
         CLC   =C'X-',CMDATA       NEVER PRINT THESE COMMENTS                   
         BE    GETCOM3                                                          
         DROP  R7                                                               
GETCOM7  LA    R4,COMAREA                                                       
         CLI   CMNUM,5             GET COMMENT SLOT                             
         BH    GETCOM3                                                          
         SR    R7,R7                                                            
         IC    R7,CMNUM                                                         
         BCTR  R7,0                                                             
         MH    R7,=H'80'                                                        
         AR    R4,R7                                                            
         SR    R7,R7                                                            
         IC    R7,CMLEN                                                         
         SH    R7,=H'4'                                                         
         LTR   R7,R7                                                            
         BM    GETCOM3                                                          
         EX    R7,*+8                                                           
         B     GETCOM3                                                          
         MVC   0(0,R4),CMDATA                                                   
GETCOMX  XIT1                                                                   
         LTORG                                                                  
         DROP  R9                                                               
         EJECT                                                                  
GETSADDR NTR1  BASE=*,LABEL=*                                                   
         L     R6,=A(DICSECT)                                                   
         USING DICSECT,R6                                                       
*                                                                               
         MVI   FORCEMID,C'Y'       SET UP STATION LINE                          
         XC    SPBUFSTA,SPBUFSTA   CLEAR SPILL STATION BUFFER                   
         MVI   SPLPRINT,1          SET TO PRINT                                 
         CLI   MAXPASS,1                                                        
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XC    MID1,MID1                                                        
*                                                                               
         MVC   P,SPACES                                                         
         MVC   MID2,SPACES                                                      
         MVC   MID2+10(9),BIGSTA                                                
         MVC   SVBIGSTA,BIGSTA                                                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MAXLINES                                                      
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         SR    R0,RE                                                            
         CHI   R0,10                                                            
         BH    *+12                                                             
         MVI   LINE,99                                                          
         MVI   FORCEMID,C'N'                                                    
*                                                                               
         CLI   QMED,C'C'                                                        
         BNE   GETS2                                                            
*                                                                               
         L     RE,ADBUY                                                         
         MVC   HALF,0(RE)                                                       
         NI    HALF,X'0F'                                                       
         MVC   STACAP(7),=C' *SPOT*'                                            
         CLI   HALF,1                                                           
         BE    *+10                                                             
         MVC   STACAP(7),=C' *NTWK*'                                            
*                                                                               
GETS2    MVC   MID2(7),STACAP      MOVE 'STATION' OR 'NETWORK'                  
*                                                                               
         L     RE,ADSTAT                                                        
         USING STAREC,RE                                                        
*                                                                               
         CLC   SCHNL,=C'    '                                                   
         BE    GETS20                                                           
         CLC   SCHNL,=C'0000'                                                   
         BE    GETS20                                                           
         OC    SCHNL,SCHNL                                                      
         BZ    GETS20                                                           
         CLI   QMED,C'T'                                                        
         BNE   GETS10                                                           
         MVC   MID2+22(2),SCHNL                                                 
*---->   MVC   MID2+19(2),=C'CH'                                                
         MVC   MID2+19(L'SP@CH),SP@CH                                           
         B     GETS20                                                           
*                                                                               
GETS10   CLI   QMED,C'R'                                                        
         BNE   GETS20                                                           
*---->   MVC   MID2+19(4),=C'FREQ'                                              
         MVC   MID2+19(L'SP@FREQ),SP@FREQ                                       
         MVC   MID2+24(4),SCHNL                                                 
*                                                                               
GETS20   CLI   MODE,PROCBUY        HEADEND TOTALS ENTER WITH PROCBUY            
         BE    GETS30                                                           
         XC    STAGRID(56),STAGRID                                              
         XC    STASPOT,STASPOT                                                  
         XC    STADEMS(32),STADEMS                                              
         XC    STACOST(8),STACOST                                               
GETS30   DS    0H                                                               
         CLI   BIGSTA,C'0'         TEST CABLE STATION                           
         BL    GETS40                                                           
         MVI   MID2+19,C' '        YES-PRINT THE CABLE SYSTEM NAME              
         MVC   MID2+20(L'SSYSNAME),SSYSNAME                                     
         MVC   SVCBLLIN,MID2           SAVE FOR MGA REPORT                      
         MVC   SVCBLLIN+14(4),SPACES   SUPPRESS NETWORK                         
         B     GETS50                                                           
*                                                                               
GETS40   CLI   D2ASUPA,C'Y'        SUPPRESS THE AFFILAIATION                    
         BE    GETS50                                                           
*                                                                               
         LA    R4,SNETWRK          US NETWORK AFFIL (3)                         
         LHI   R5,3                                                             
*                                                                               
         L     RF,ADAGY            TEST CANADIAN AGENCY                         
         CLI   AGYPROF+7-AGYHDR(RF),C'C'                                        
         BNE   GETS42                                                           
         LA    R4,SCANNTWK         CN NETWORK AFFIL (4)                         
         LHI   R5,4                                                             
*                                                                               
GETS42   OC    0(3,R4),0(R4)       BINARY ZEROS DON'T PRINT                     
         BZ    GETS50                                                           
         CLC   0(3,R4),=C'000'     CHAR ZEROS DON'T PRINT                       
         BE    GETS50                                                           
         CLC   0(3,R4),SPACES      SPACES DON'T PRINT                           
         BE    GETS50                                                           
*---->   MVC   MID2+29(9),=C'AFFILIATE'                                         
         MVC   MID2+29(L'SP@AFFIL),SP@AFFIL                                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     GETS50                                                           
         MVC   MID2+39(0),0(R4)                                                 
*                                                                               
GETS50   CLC   SCONREP,=C'000'                                                  
         BE    GETS60                                                           
         CLC   QPROG,=C'CP'        FOR CP REPORT, REP NAME PRINTING IS          
         BNE   GETS52              CONTROLLED BY CP PROFILE                     
         CLI   PROGPROF,C'N'                                                    
         BE    GETS60                                                           
         B     GETS54                                                           
*                                                                               
GETS52   LA    R5,PROGPROF                                                      
         USING PROFDSCT,R5                                                      
         CLI   PROFPRN,C'N'                                                     
         BE    GETS60                                                           
         DROP  R5                                                               
*                                                                               
GETS54   OC    SCONREP,SCONREP                                                  
         BZ    GETS60                                                           
         MVC   WORK(3),SCONREP                                                  
         MVI   REPFLAG,C'N'                                                     
         GOTO1 VGETREP                                                          
GETS60   CLC   QPROG(2),=C'D7'     IF D7 OR DX                                  
         BE    GETSX               TEST PAGE BREAK IN STALAST MODE              
         CLC   QPROG(2),=C'DX'     (M10)                                        
         BE    GETSX                                                            
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         CLI   BIGSTA+4,C'/'       TEST THIS IS A CABLE STATION                 
         BNE   GETS62              NO                                           
         CLC   BIGSTA(4),LASTBIG   YES, DON'T PAGE BREAK IF THERE'S NO          
         BE    GETSX               CHANGE IN CABLE SYSTEM                       
*                                                                               
GETS62   LA    R5,PROGPROF                                                      
         USING PROFDSCT,R5                                                      
         CLI   PROFPAG,C'Y'        TEST PAGE BREAK AT STALAST                   
         BNE   GETSX               YES                                          
         DROP  R5                                                               
*                                                                               
GETS70   MVI   FORCEHED,C'Y'                                                    
*                                                                               
GETSX    MVC   SVSTLINE,MID2                                                    
         MVC   SVSTAT,BIGSTA       SAVE THIS STATION                            
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
COMPRNT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,PROGPROF                                                      
         USING PROFDSCT,R1                                                      
         CLI   PROFID,C'Y'                                                      
         BNE   GETIDX                                                           
         DROP  R1                                                               
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
GETID    CLI   0(RE),0                                                          
         BE    GETIDX                                                           
         CLI   0(RE),X'70'                                                      
         BE    GETID1                                                           
         LLC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETID                                                            
GETID1   L     R6,FULL                                                          
GETID2   MVC   11(12,R6),3(RE)                                                  
         MVI   10(R6),C'='                                                      
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVC   0(10,R6),CTITLE                                                  
         MVI   132(R6),0                                                        
         DROP RE                                                                
GETIDX   DS    0C                                                               
         LA    R4,COMAREA                                                       
         LA    R5,5                                                             
         L     R6,FULL                                                          
COMPRNT1 OC    0(76,R4),0(R4)                                                   
         BZ    *+14                                                             
         MVC   0(76,R6),0(R4)                                                   
         LA    R6,132(R6)                                                       
         LA    R4,80(R4)                                                        
         BCT   R5,COMPRNT1                                                      
*                                                                               
         ST    R6,FULL                                                          
         L     R7,MEDBUFF          PRINT OUT MISSED GROUP CAPTION               
         USING MEDBLOCK,R7                                                      
         CLI   PNOMKGD,C'Y'          SUPPRESS MISSED CAPTIONS                   
         BE    CPRMGAX                IF NOT PRINTING MAKEGOODS                 
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         CLI   PROFHMS,C'N'         SUPPRESS MISSED CAPTIONS                    
         BE    CPRMGAX               IF NOT PRINTING MISSED                     
         DROP  RE                                                               
         L     R9,=A(MGTABLE)                                                   
         USING MGENTRYD,R9                                                      
         OC    0(L'MGECODE,R9),0(R9) EMPTY - JUST BYPASS                        
         BZ    CPRMGAX                                                          
         L     R6,FULL                                                          
         MVI   0(R6),0                                                          
CPRMGA1  LA    R1,P14              CHECK FOR OVERFLOW                           
         CR    R6,R1                                                            
         BH    CPRMGA6             JUST STOP IT IF OVERFLOW                     
         CLI   MGETYPE,X'00'       ONLY FOR MISSED SPOTS                        
         BNE   CPRMGA3                                                          
         CLC   MGECODE,=C'PR'      TEST TYPE = PREEMPT                          
         BE    CPRMGA3             YES - BYPASS                                 
         CLC   MGECODE,=C'*P'                                                   
         BE    CPRMGA3             YES - BYPASS                                 
         CLI   MGAECOD,X'FE'       TEST OLD MAKEGOOD                            
         BE    CPRMGA3                                                          
         CLC   MGEDATE,MEDPERD     IN REQUESTED TIME PERIOD                     
         BL    CPRMGA3                                                          
         CLC   MGEDATE,MEDPERD+2                                                
         BH    CPRMGA3                                                          
         MVI   0(R6),C','                                                       
         C     R6,FULL                                                          
         BNE   *+14                                                             
         MVC   0(20,R6),=C'MSSD SPOTS IN GROUPS'                                
         LA    R6,20(R6)                                                        
         GOTO1 DATCON,DMCB,(2,MGEDATE),(4,1(R6))                                
         LA    R6,6(R6)                                                         
         MVI   0(R6),C'-'                                                       
         LLC   R0,MGESPNUM                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R6),DUB                                                      
*                                                                               
CPRMGA2  MVI   3(R6),C'='                                                       
         MVC   4(2,R6),0(R9)                                                    
         LA    R6,6(R6)                                                         
*                                                                               
CPRMGA3  L     RF,FULL             HANDLE LINE SPANNING                         
         LA    RF,90(RF)                                                        
         CR    R6,RF                                                            
         BL    CPRMGA4                                                          
         L     R6,FULL                                                          
         LA    R6,132(R6)                                                       
         ST    R6,FULL                                                          
         LA    R0,P14              NO MORE THAN 14 LINES!                       
         CR    R6,R0                                                            
         BH    COMPEX                                                           
*                                                                               
CPRMGA4  LA    R9,MGERECL(R9)      GET NEXT ITEM                                
         OC    0(L'MGECODE,R9),0(R9)                                            
         BNZ   CPRMGA1                                                          
         C     R6,FULL                                                          
         BE    CPRMGAX                                                          
CPRMGA6  L     R6,FULL                                                          
         LA    R6,132(R6)                                                       
         ST    R6,FULL                                                          
         DROP  R9                                                               
         DROP  R7                                                               
CPRMGAX  DS    0H                                                               
*                                                                               
         CLI   COMAREA,0                                                        
         BE    *+8                                                              
         MVI   0(R6),0                                                          
         L     RE,ADBUY            LOOK FOR ORBIT ELEMENT                       
         LA    RE,24(RE)                                                        
         USING ORBELEM,RE                                                       
GETORB   CLI   0(RE),0             END                                          
         BE    GETORBX              YES -EXIT                                   
         CLI   0(RE),X'67'                                                      
         BE    GETORB1                                                          
         LLC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETORB                                                           
GETORB1  LA    R5,ORBDAY                                                        
         USING ORBDAY,R5                                                        
GETORB1A CLC   1(130,R6),0(R6)                                                  
         BE    *+12                                                             
         LA    R6,132(R6)                                                       
         B     GETORB1A                                                         
         LLC   R7,1(RE)            GET END OF ELEMENT                           
         AR    R7,RE                                                            
         L     RF,=A(DICSECT)                                                   
         USING DICSECT,RF                                                       
*---->   MVC   0(8,R6),=C'*ORBIT* '                                             
         MVC   0(L'SP@ORBIT,R6),SP@ORBIT                                        
         DROP  RF                                                               
         LA    R6,9(R6)                                                         
GETORB2  LR    R4,R6                                                            
         LA    R3,4                                                             
         LA    R3,3                                                             
GETORB3  DS    0C                                                               
         LR    R9,R6                                                            
         CR    R5,R7                                                            
         BNL   GETORBX                                                          
         XC    DMCB(24),DMCB                                                    
         GOTO1 CODAY,DMCB,ORBDAY,(R6)                                           
         LA    R6,9(R6)                                                         
         GOTO1 UNTIME,DMCB,ORBTIME,(R6)                                         
         LA    R6,12(R6)                                                        
         MVC   0(7,R6),ORBDESC                                                  
         LA    R6,8(R6)                                                         
         CLI   DETOPTS+1,1                                                      
         BNE   GETORB4                                                          
         MVC   HALF,ORBDEM                                                      
         NI    HALF,X'3F'                                                       
         MVI   CURTAB+3,1                                                       
         TM    ORBDEM,X'40'        TEST 2-DECIMAL                               
         BZ    *+8                                                              
         MVI   CURTAB+3,2                                                       
         CURED HALF,(6,(R6)),CURTAB,DMCB=CURDMCB                                
GETORB4  LA    R6,7(R6)                                                         
         GOTO1 SQUASHER,DMCB,(R9),36                                            
         LA    R5,16(R5)                                                        
         BCT   R3,GETORB3                                                       
         LR    R6,R4                                                            
         LA    R6,132(R6)                                                       
         B     GETORB2                                                          
GETORBX  DS    0H                                                               
         EJECT                                                                  
* SET UP UPGRADE EXPRESSION                                                     
         CLI   QBOOK1,C' '         NEVER PRINT THE UPGRADE EXPRESSION           
         B     GETEXCH                                                          
         CLI   QPROG,C'U'                                                       
         BNE   GETEXCH                                                          
         GOTO1 VUDESC,DMCB,(RA),(R6)                                            
         LA    R6,132(R6)                                                       
         MVI   0(R6),0                                                          
         SPACE 1                                                                
* SET CANADIAN C58 TAX AND MEDIA SERVICE FEE                                    
*                                                                               
GETEXCH  L     RE,ADAGY            TEST CANADIAN AGENCY                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C'                                        
         BNE   COMPEX                                                           
         L     RE,ADBUY            TEST EXCHANGE ELEMENT                        
         TM    BDCIND2-BUYREC(RE),X'40'                                         
         BZ    COMPEX                                                           
         SR    RF,RF               YES-LOCATE EXCHANGE ELEMENT                  
         LA    RE,24(RE)                                                        
*                                                                               
GETEXCH2 CLI   0(RE),0                                                          
         BE    COMPEX                                                           
         CLI   0(RE),XCHCODEQ                                                   
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETEXCH2                                                         
         USING XCHELEM,RE                                                       
         LR    R4,R6                                                            
         OC    XCHRATE,XCHRATE     TEST EXCHANGE RATE                           
         BZ    GETEXCH4                                                         
         L     R3,=A(DICSECT)                                                   
         USING DICSECT,R3                                                       
*---->   MVC   0(9,R4),=C'EXCHANGE='                                            
         MVC   0(L'SP@EXCHG,R4),SP@EXCHG                                        
         DROP  R3                                                               
         LR    R3,R4                                                            
         LA    R3,L'SP@EXCHG-1(R3)                                              
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'='                                                       
         LA    R4,9(R4)                                                         
         EDIT  (2,XCHRATE),(7,(R4)),4,TRAIL=C'%'                                
         LA    R4,8(R4)                                                         
*                                                                               
GETEXCH4 OC    XCHC58,XCHC58       TEST C58 TAX                                 
         BZ    COMPEX                                                           
         MVC   0(4,R4),=C'C58='                                                 
         LA    R4,4(R4)                                                         
         EDIT  (2,XCHC58),(6,(R4)),2,TRAIL=C'%'                                 
         DROP  RE                                                               
*                                                                               
COMPEX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
GETGL    NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         MVC   MEDNUMWK,=F'60'                                                  
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         LA    RE,KEY                                                           
         L     RE,ADGOAL                                                        
         USING GOALREC,RE                                                       
         MVC   MEDBRAND,BPRD                                                    
         MVC   MEDSPTLN,GKEYSLN                                                 
         CLI   QPWCV,C'Y'                                                       
         BE    *+12                                                             
         CLI   QCOST2,C'Y'                                                      
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
         DROP  RE                                                               
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    MEDGLD(12),MEDGLD                                                
         BZ    GOALX                                                            
         L     R9,WEIGHT                                                        
         GOTO1 MEDMKTWT,DMCB,(RA),(R9)                                          
         LA    R3,PRTLINE                                                       
         L     R5,MEDAFRST                                                      
         CLI   MRPTTYP,C'5'        DOESN'T NEED WEEKLY                          
         BNE   M163                                                             
         LA    R5,MEDMON01                                                      
M163     MVI   SUMCODE,X'90'                                                    
         MVC   SUMDPGNO(8),MEDDPGNO     SET DAYPART                             
         MVC   SUMSLN,MEDSPTLN                                                  
         L     R4,4(R5)                                                         
         LTR   R4,R4                                                            
         BZ    M165                                                             
         MVI   SUMRTYP,1                                                        
         LA    RF,MEDPERD                                                       
         CR    R5,RF                                                            
         BH    M16X                END                                          
         BNE   M163A                                                            
         MVI   SUMRTYP,3                                                        
         MVC   SUMDT,=X'FFFFFFFF'                                               
         B     M164                                                             
M163A    OC    0(4,R5),0(R5)                                                    
         BZ    M165                                                             
         LA    RF,MEDMON01                                                      
         CR    R5,RF               MONTHLY                                      
         BL    *+8                                                              
         MVI   SUMRTYP,2            YES-SET RECORD CODE                         
         MVC   SUMDT,0(R5)                                                      
M164     LA    RE,SUMKEY           SET UP DATA ITEM DISPLACEMENTS               
         USING SUMDATA,RE                                                       
         OC    MEDGLD(16),MEDGLD                                                
         BZ    M165                                                             
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     RE,BUFFLKEY                                                      
         DROP  RF                                                               
         XC    SUMDATA,SUMDATA                                                  
         MVC   SUMGDL,MEDGLD                                                    
         MVC   SUMGDLE,MEDGLDEQ                                                 
         MVC   SUMGD1,MEDGL1                                                    
         MVC   SUMGD1E,MEDGL1EQ                                                 
         LA    R8,PROGPROF                                                      
         USING PROFDSCT,R8                                                      
         CLI   PROFDPT,C'Y'        DAYPART ANALYSIS REQUIRED                    
         BNE   M164T                NO - PUT OUT TOTALS ONLY                    
         DROP  R8                                                               
         L     R8,BUFFBUFF                                                      
         BAS   R9,M16PUT                                                        
* PUT OUT PRODUCT GROUP DETAILS                                                 
         MVI   SUMCODE,X'93'                                                    
         BAS   R9,M16PUT                                                        
M164T    L     R8,BUFFBUFF                                                      
*                                                                               
         CLI   DPSUMSW,C'Y'        SET SPOT LENGTH SUMMARY                      
         BNE   *+16                                                             
         MVI   SUMSLN,X'FF'                                                     
         MVI   SUMCODE,X'97'                                                    
         BAS   R9,M16PUT                                                        
         L     R8,BUFFBUFF                                                      
*                                                                               
         MVC   SUMSLN,MEDSPTLN     RESTORE THE SPOT LENGTH                      
*                                                                               
         CLI   SLSUMSW,C'Y'        SET SPOT LENGTH SUMMARY                      
         BNE   *+18                                                             
         MVC   SUMDPGNO(8),=8X'FF'                                              
         MVI   SUMCODE,X'96'                                                    
         BAS   R9,M16PUT                                                        
         L     R8,BUFFBUFF                                                      
*                                                                               
         MVC   SUMDPGNO(9),=9X'FF'                                              
         MVI   SUMCODE,X'90'                                                    
         BAS   R9,M16PUT                                                        
         MVI   SUMCODE,X'93'                                                    
         BAS   R9,M16PUT                                                        
***      MAY NEED ORIGINATING LINE SO SEED GOALS                                
         CLI   SPOTPROF+5,0        NO ORIG REPORTING                            
         BE    M165                                                             
         CLI   SPOTPROF+5,2        NO ORIG REPORTING                            
         BE    M165                                                             
         MVI   SUMCODE,X'89'       PUT OUT ORIGINATING LINES                    
         MVI   SUMSLN,X'FE'                                                     
         BAS   R9,M16PUT                                                        
         MVI   SUMCODE,X'92'                                                    
         BAS   R9,M16PUT                                                        
***                                                                             
* GET NEXT BUFFALO ITEM                                                         
M165     LA    R5,12(R5)                                                        
         B     M163                                                             
M16X     B     GOALX                                                            
         DROP  RE                                                               
M16PUT   CLI   SUMCODE,X'93'                                                    
         BER   R9                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),PRTLINE                                
         BR    R9                                                               
GOALX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
GETREP   NTR1  BASE=*,LABEL=*                                                   
         MVC   RPKYSAVE,KEY                                                     
         LA    R6,KEY                                                           
         USING REPREC,R6                                                        
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,QMED                                                     
         MVC   REPKREP,WORK                                                     
         MVC   REPKAGY,AGY                                                      
         GOTO1 READREP                                                          
         MVC   KEY,RPKYSAVE                                                     
         L     R6,ADREP                                                         
         CLI   REPFLAG,C'N'        SAVE REP NAME?                               
         BE    GETREP10            NO                                           
         MVC   SVSPREPN,RNAME      REP NAME                                     
         J     GETREPX                                                          
                                                                                
GETREP10 L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*---->   MVC   MID2+43(4),=C'REP-'                                              
         MVC   MID2+43(L'SP4REP),SP4REP                                         
         MVC   MID2+48(L'RNAME),RNAME                                           
GETREPX  XIT1                                                                   
         DROP  R5,R6                                                            
         LTORG                                                                  
RPKYSAVE DS    CL32                                                             
         EJECT                                                                  
STATOT   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
*                                                                               
STOT2    OC    STASPOT,STASPOT                                                  
         BZ    STOT44                                                           
*                                                                               
         MVC   LASTBIG,BIGSTA                                                   
         CLI   QOPT1,C'A'          REPORT OPTION A SPECIAL                      
         BE    STOT40                                                           
*                                                                               
         MVI   P1,0                                                             
         LLC   RE,MAXLINES                                                      
         LLC   RF,LINE                                                          
         SR    RE,RF                                                            
         CHI   RE,5                                                             
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
*                                                                               
         LA    R5,MEDPERD                                                       
         GOTO1 DATCON,DMCB,(X'02',(R5)),(X'08',P3+14)                           
*                                                                               
         MVI   P3+22,C'-'                                                       
         GOTO1 DATCON,DMCB,(X'02',2(R5)),(X'08',P3+23)                          
*                                                                               
         MVC   P2+14(7),STACAP                                                  
         MVC   P2+14(9),BIGSTA                                                  
*                                                                               
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*---->   MVC   P2+24(3),=C'TOT'                                                 
         MVC   P2+24(L'SP3TOTAL),SP3TOTAL                                       
         CLI   QBYID,C'Y'                                                       
         BNE   STOT4                                                            
         XC    P2(29),P2                                                        
         MVC   P2+4(12),BUYIDNAM                                                
         MVC   P2+17(12),BUYID                                                  
*                                                                               
STOT4    DS    0C                                                               
         LA    R6,P2+31                                                         
         C     R6,PSTASPT                                                       
         BL    STOT10                                                           
*---->   MVC   P2+37(6),=C'TLCSTS'                                              
         MVC   P2+37(L'SP@TLCST),SP@TLCST                                       
         CLI   QMED,C'R'                                                        
         BE    *+12                                                             
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
*---->   MVC   P2+37(7),=C'BRDCSTS'                                             
         MVC   P2+37(L'SP@BRDCS),SP@BRDCS                                       
         DROP  R5                                                               
*                                                                               
STOT10   L     R6,PSTASPT                                                       
         EDIT  STASPOT,(5,(R6))                                                 
         CLI   DETOPTS+2,0         SUPPRESS COST                                
         BE    STOT20               YES                                         
         L     R6,PSTACOST                                                      
         CLC   QPROG,=C'D7'                                                     
         BE    STOT10A                                                          
*                                                                               
         CLC   QPROG,=C'DX'                                                     
         BNE   STOT10B                                                          
         CLI   DRCSHTRD,C'R'       TEST TRADE SIDE OF CASH/TRD                  
         BNE   STOT10A                                                          
         CLI   DARPROF+12,C'Y'    YES, ZERO COST TRADE?                         
         BNE   STOT10A             NO                                           
         XC    STACOST,STACOST                                                  
*                                                                               
STOT10A  LA    R6,2(R6)                                                         
         B     STOT14                                                           
*                                                                               
STOT10B  CLI   PENNYSW,1                                                        
         BNE   STOT14                                                           
         L     RF,STACOST          SUPPRESS PENNIES                             
         CHI   RF,-1                                                            
         BNE   STOT11                                                           
         MVC   0(5,R6),=C'*****'   $ TOTALLING OVERFLOW                         
         MVI   HADOFLOW,C'Y'                                                    
         B     STOT20                                                           
*                                                                               
STOT11   M     RE,=F'2'                                                         
         D     RE,=F'100'                                                       
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         C     RF,=F'999999'       NOT ENOUGH ROOM FOR THIS                     
         BH    STOT12                                                           
         MVI   CURTAB+3,0                                                       
         CURED (RF),(7,(R6)),CURTAB,DMCB=CURDMCB,MINUS=YES,            X        
               CURSYMB=YES                                                      
         B     STOT20                                                           
*                                                                               
STOT12   MVI   CURTAB+3,0                                                       
         BCTR  R6,0                ADJ FOR BIG NUM(NO MINUS)                    
         CURED (RF),(8,(R6)),DMCB=CURDMCB,CURTAB                                
         LA    R6,1(R6)                                                         
         B     STOT20                                                           
*                                                                               
STOT14   L     RF,STACOST                                                       
         CHI   RF,-1                                                            
         BNE   STOT15                                                           
         MVC   0(5,R6),=C'*****'   $ TOTALLING OVERFLOW                         
         MVI   HADOFLOW,C'Y'                                                    
         B     STOT20                                                           
*                                                                               
STOT15   C     RF,=F'99999999'                                                  
         BH    STOT16                                                           
         MVI   CURTAB+3,2                                                       
*                                  PRINT PENNIES                                
         CURED STACOST,(10,(R6)),CURTAB,DMCB=CURDMCB,MINUS=YES                  
         B     STOT20                                                           
*                                                                               
STOT16   L     RF,STACOST          DROP PENNIES                                 
         M     RE,=F'2'                                                         
         D     RE,=F'100'                                                       
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         MVI   CURTAB+3,0                                                       
         CURED (RF),(10,(R6)),CURTAB,DMCB=CURDMCB,MINUS=YES,           X        
               CURSYMB=YES                                                      
                                                                                
*==========================================================                     
* BUILD GRID                                                                    
*==========================================================                     
                                                                                
STOT20   LA    R5,STAGRID                                                       
         LA    R1,14                                                            
         L     R6,PSTAGRID                                                      
*                                                                               
STOT22   OC    0(4,R5),0(R5)                                                    
         BZ    STOT24                                                           
         L     R8,0(R5)                                                         
         EDIT  (R8),(4,(R6))                                                    
*                                                                               
STOT24   LA    R6,4(R6)                                                         
         LA    R5,4(R5)                                                         
         BCT   R1,STOT22                                                        
* CALCULATE DEMOS AND CPM                                                       
         LA    R3,PRTLINE                                                       
         XC    PRTLINE,PRTLINE                                                  
         MVC   SUMDL(8),STACOST                                                 
         CLC   QPROG,=C'CP'                                                     
         BE    STOT26                                                           
         CLC   QPROG,=C'D7'                                                     
         BE    STOT26                                                           
         CLC   QPROG,=C'DX'                                                     
         BNE   STOT28                                                           
*                                                                               
STOT26   MVC   SUMDL+1(8),STACOST  LINEUP STATION COST                          
*                                                                               
STOT28   MVC   SUMD1(32),STADEMS                                                
         MVC   FULL,=F'1'                                                       
         GOTO1 VCALCPP,DMCB,FULL,(R3)                                           
         GOTO1 VEDTDEMS                                                         
*                                                                               
         LA    R8,P2                                                            
         CLI   DETOPTS+1,1         DEMOS REQUESTED                              
         BNE   STOT34                                                           
*                                                                               
         CLC   QPROG,=C'D6'                                                     
         BNE   *+12                                                             
         CLI   PROGPROF+6,C'Y'     TEST PRINT DEMOS ON SEP LINES                
         BE    STOT30D6                                                         
*                                                                               
         CLI   Q2FAXDEM,C'Y'                                                    
         BE    STOT28DX                                                         
*                                                                               
         MVC   108(5,R8),PLD1      MOVE DEMOS TO PRINT LINE                     
         MVC   114(5,R8),PLD2                                                   
         MVC   120(5,R8),PLD3                                                   
         MVC   126(5,R8),PLD4                                                   
         LA    R8,132(R8)                                                       
         B     STOT30                                                           
*                                                                               
STOT28DX L     R1,=A(DXDEMDSP)                                                  
         CLI   PRDEMCNT,4                                                       
         BL    *+8                                                              
         LA    R1,8(R1)                                                         
*                                                                               
         LA    RE,P                                                             
         AH    RE,0(R1)                                                         
         MVC   0(5,RE),PLD1                                                     
         LA    RE,P                                                             
         AH    RE,2(R1)                                                         
         MVC   0(5,RE),PLD2                                                     
         LA    RE,P                                                             
         AH    RE,4(R1)                                                         
         MVC   0(5,RE),PLD3                                                     
         LA    RE,P                                                             
         AH    RE,6(R1)                                                         
         MVC   0(5,RE),PLD4                                                     
*                                                                               
STOT30   CLI   DETOPTS+3,1         CPP/M REQUESTED                              
         BNE   STOT34                                                           
*                                                                               
         MVC   107(6,R8),PLD1CP    MOVE CPP/M TO PRINT LINE                     
         CLC   SUMDL,SUMDLEQ                                                    
         BE    *+8                                                              
         MVI   113(R8),C'+'                                                     
*                                                                               
         MVC   119(6,R8),PLD3CP                                                 
         OC    PLD3CP,PLD3CP                                                    
         BZ    *+8                                                              
         MVI   125(R8),C'+'                                                     
*                                                                               
         LR    RE,R8                                                            
         MVC   114(5,RE),PLD2CP+1                                               
         CLI   BIGCPP,C'Y'                                                      
         BNE   *+14                                                             
         LA    RE,132(RE)                                                       
         MVC   113(6,RE),PLD2CP                                                 
*                                                                               
         OC    PLD2CP,PLD2CP                                                    
         BZ    *+8                                                              
         MVI   119(RE),C'+'                                                     
*                                                                               
         LR    RE,R8                                                            
         MVC   126(5,RE),PLD4CP+1                                               
         CLI   BIGCPP,C'Y'                                                      
         BNE   *+14                                                             
         LA    RE,132(RE)                                                       
         MVC   125(6,RE),PLD4CP                                                 
*                                                                               
         OC    PLD4CP,PLD4CP                                                    
         BZ    *+8                                                              
         MVI   131(RE),C'+'                                                     
         B     STOT34                                                           
*                                                                               
STOT30D6 DS    0H                  STACKED DEMOS FOR D6 ONLY                    
         MVC   109(7,R8),DNAME1                                                 
         MVC   117(5,R8),PLD1                                                   
         MVC   122(1,R8),OVRFLAG                                                
         CLI   DETOPTS+3,1         CPP/M REQUESTED                              
         BNE   *+10                                                             
         MVC   125(6,R8),PLD1CP    MOVE CPP/M TO PRINT LINE                     
*                                                                               
         LA    R8,132(R8)                                                       
         MVC   109(7,R8),DNAME2                                                 
         MVC   117(5,R8),PLD2                                                   
         MVC   122(1,R8),OVRFLAG+1                                              
         CLI   DETOPTS+3,1                                                      
         BNE   *+10                                                             
         MVC   125(6,R8),PLD2CP                                                 
*                                                                               
         LA    R8,132(R8)                                                       
         MVC   109(7,R8),DNAME3                                                 
         MVC   117(5,R8),PLD3                                                   
         MVC   122(1,R8),OVRFLAG+2                                              
         CLI   DETOPTS+3,1                                                      
         BNE   *+10                                                             
         MVC   125(6,R8),PLD3CP                                                 
*                                                                               
         LA    R8,132(R8)                                                       
         MVC   109(7,R8),DNAME4                                                 
         MVC   117(5,R8),PLD4                                                   
         MVC   122(1,R8),OVRFLAG+3                                              
         CLI   DETOPTS+3,1                                                      
         BNE   *+10                                                             
         MVC   125(6,R8),PLD4CP                                                 
*                                                                               
STOT34   GOTO1 REPORT                                                           
*                                                                               
         GOTO1 VFOOT,DMCB,(RA)                                                  
*                                                                               
STOT40   CLI   PASS,X'FF'                                                       
         BE    STOT46                                                           
         CLI   MAXPASS,1           ONLY ONE PASS                                
         BE    STOT46                                                           
*                                                                               
         LA    RE,STATOTS           NO - ADD TO OVERALL TOTALS                  
         LA    R1,STTTOTS                                                       
         LHI   R0,11                                                            
*                                                                               
STOT42   L     R9,0(R1)                                                         
         CHI   R9,-1               PRESERVE ANY ACCUM OVERFLOW                  
         BE    STOT43                                                           
         CLC   0(4,RE),=F'-1'                                                   
         BE    *+12                                                             
         A     R9,0(RE)                                                         
         BNO   *+8                 DETECT OVERFLOW                              
         LHI   R9,-1                                                            
         ST    R9,0(R1)                                                         
*                                                                               
STOT43   LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,STOT42                                                        
                                                                                
*=================================================================              
* SET UP FOR NEXT PASS                                                          
*=================================================================              
                                                                                
STOT44   CLI   MAXPASS,1                                                        
         BE    STOT46                                                           
         CLI   PASS,X'FF'                                                       
         BE    STOT46                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PASS                                                          
         LA    RE,1(RE)                                                         
         STC   RE,PASS                                                          
         CLC   PASS,MAXPASS        TRUE END OF STATION                          
         BL    STOT54                                                           
*                                                                               
         MVI   PASS,X'FF'                                                       
         XC    STAGRID(56),STAGRID                                              
         MVC   STATOTS,STTTOTS                                                  
         XC    STTTOTS,STTTOTS                                                  
         MVI   MID1,C'-'                                                        
         MVC   MID1+1(131),MID1                                                 
         MVI   FORCEMID,C'Y'                                                    
         MVC   MEDPERD,SVRDTE                                                   
         B     STOT2                                                            
*                                                                               
STOT46   MVI   PASS,0              YES - RESET AND EXIT                         
         OC    TAXAMT,TAXAMT                                                    
         BZ    STOT50                                                           
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P(53),=C'***TAX OF XXXXX.XX EXCLUDED FROM THIS REPORT'           
         MVC   P(L'SP@TAX02),SP@TAX02                                           
         DROP  RE                                                               
         MVI   CURTAB+3,2                                                       
         CURED TAXAMT,(8,P+10),CURTAB,DMCB=CURDMCB                              
         GOTO1 REPORT                                                           
         XC    TAXAMT,TAXAMT                                                    
*                                                                               
STOT50   CLI   QPROG,C'U'                                                       
         BNE   STOT52                                                           
         GOTO1 VREVBUY,DMCB,(RA)                                                
*                                                                               
STOT52   DS    0H                  REAL END OF STATION                          
         MVC   WORK(12),PASSTAB                                                 
         MVC   MID1,SPACES                                                      
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         B     STOT60                                                           
*                                                                               
STOT54   MVI   MODE,REREAD                                                      
         TM    RQOPTS,RQOPTS_HDEND TOTAL SYS CODE ONLY                          
         BZ    STOT60                                                           
         TM    BIGSTA,X'F0'        AND ITS LOCAL CABLE                          
         BNO   STOT60                                                           
         CLI   QCBLNET,C'A'        AND WE'RE NOT DOING ONE NETWORK              
         BNL   *+8                                                              
         NI    SVBUYKEY+8,X'80'    THEN SET TO REREAD THE WHOLE HDEND           
*                                                                               
STOT60   XC    LASTBIG,LASTBIG                                                  
         SR    R6,R6                                                            
         IC    R6,PASS                                                          
         MHI   R6,268                                                           
         LA    R6,PASSTAB(R6)                                                   
         USING PASSTABD,R6                                                      
         MVC   WORK(12),0(R6)                                                   
         MVC   MEDNUMWK(L'PASSP1),PASSP1                                        
         MVC   MEDWEEKS(L'PASSP2),PASSP2                                        
         MVC   MEDMON01(L'PASSP3),PASSP3                                        
         MVC   MEDPERD(L'PASSP4),PASSP4                                         
         XC    STAGRID(56),STAGRID                                              
         XC    STATOTS,STATOTS                                                  
*                                                                               
         L     R1,AHDATES                                                       
         GOTO1 VHDATES                                                          
         CLI   FORCEHED,C'Y'                                                    
         BE    STOT70                                                           
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,MAXLINES                                                      
         SR    RF,RE                                                            
         CHI   RF,14                                                            
         BL    STOT80                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'Y'                                                    
         MVI   MID1,C'-'                                                        
         MVC   MID1+1(131),MID1                                                 
*                                                                               
STOT70   GOTO1 VHDATES                                                          
         MVI   ALLOWLIN,14                                                      
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
STOT80   GOTO1 DATCON,DMCB,WORK,(X'03',PASSSD3)                                 
         GOTO1 DATCON,DMCB,WORK+6,(X'03',PASSED3)                               
         LLC   RE,MAXLINES                                                      
         LLC   RF,LINE                                                          
         SR    RE,RF                                                            
         CHI   RE,8                                                             
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
EDTDEMSC NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
*                                                                               
         LA    R4,SVDEMS                                                        
         LA    R5,PLDEMS                                                        
         XC    PLDEMS,PLDEMS                                                    
         MVI   BIGCPP,C'N'         SET NO 6 CHAR CPP'S                          
         MVI   DEMINDX,0                                                        
*                                                                               
EDTDEM2  SR    R8,R8                                                            
         ICM   R9,15,0(R4)         GET DEMO VALUE                               
         BZ    EDTDEM20                                                         
*                                                                               
         GOTO1 =A(SETPTDEM)        ON RETURN, RF=A(3-BYTE DEMO ENTRY)           
*                                                                               
         CLI   RATING,C'Y'                                                      
         BNE   EDTDEM3                                                          
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC RATINGS                           
         BZ    EDTDEM10                                                         
         CLI   MODE,PROCBUY        TEST BUY DETAILS                             
         BE    EDTDEM4             YES                                          
         J     *+12                                                             
*                                                                               
EDTDEM3  TM    RQOPTS,RQOPTS_2DECIMP   TEST 2-DEC IMPS                          
         JZ    EDTDEM10                NO                                       
*                                                                               
         CLI   MODE,PROCBUY        TEST BUY DETAILS                             
         BE    EDTDEM4             YES                                          
*                                                                               
         M     R8,=F'2'            DROP 1 DECIMAL                               
         D     R8,=F'10'                                                        
         AHI   R9,1                                                             
         SRL   R9,1                                                             
         SR    R8,R8                                                            
         B     EDTDEM10                                                         
*                                                                               
* >99999 PRINTS NO DEC, >9999 PRINTS 1 DEC, ELSE PRINT 2 DEC                    
*                                                                               
EDTDEM4  C     R9,=F'99999'                                                     
         BNH   EDTDEM5                                                          
         LHI   R0,100              SET DIVISOR                                  
         B     EDTDEM16                                                         
*                                                                               
EDTDEM5  C     R9,=F'9999'                                                      
         BNH   EDTDEM6                                                          
         AR    R9,R9                                                            
         D     R8,=F'10'                                                        
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         CURED (R9),(5,(R5)),1,DMCB=CURDMCB                                     
         B     EDTDEM20                                                         
*                                                                               
EDTDEM6  CURED (R9),(5,(R5)),2,DMCB=CURDMCB                                     
         B     EDTDEM20                                                         
*                                                                               
* VALUE HAS ONLY 1 DECIMAL                                                      
*                                                                               
EDTDEM10 C     R9,=F'9999'         1 DECIMAL PRECISION OK                       
         BH    EDTDEM14            NO - SCALE                                   
         CURED (R9),(5,(R5)),1,DMCB=CURDMCB                                     
         B     EDTDEM20                                                         
*                                                                               
EDTDEM14 LHI   R0,10               SET DIVISOR                                  
*                                                                               
EDTDEM16 M     R8,=F'2'            X 2                                          
         DR    R8,R0                                                            
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         EDIT  (R9),(5,(R5))                                                    
*                                                                               
EDTDEM20 LA    R5,5(R5)             GET CPP                                     
         LA    R4,4(R4)                                                         
         ICM   R9,15,0(R4)                                                      
         BZ    EDTDEM30                                                         
*                                                                               
EDTDEM22 C     R9,=F'99999'                                                     
         BH    EDTDEM24                                                         
         C     R9,=F'9999'         TEST 6 CHARS WITH PENNIES                    
         BNH   *+8                                                              
         MVI   BIGCPP,C'Y'         SET TO STAGGER PRINT LINES                   
         CURED (R9),(6,(R5)),2,DMCB=CURDMCB                                     
         B     EDTDEM30                                                         
*                                                                               
EDTDEM24 M     R8,=F'2'                                                         
         D     R8,=F'100'  <<<< DROP 2 DECIMALS                                 
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         CURED (R9),(6,(R5)),0,DMCB=CURDMCB                                     
*                                                                               
         CLI   0(R5),C' '                                                       
         BH    EDTDEM30                                                         
* I CAN'T MAKE CURED PUT IN THE $ SIGN                                          
         LA    RF,5(R5)                                                         
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   0(RF),C'$'                                                       
*                                                                               
EDTDEM30 LA    R4,4(R4)                                                         
         LA    R5,6(R5)                                                         
         IC    RF,DEMINDX                                                       
         AHI   RF,1                                                             
         STC   RF,DEMINDX                                                       
         CLI   DEMINDX,4                                                        
         BL    EDTDEM2                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
MLASTC   NTR1  BASE=*,LABEL=*                                                   
         MVC   WEIGHT,SPWEIGHT                                                  
         CLI   NOSUM,1                                                          
         BE    MLASTBYS                                                         
         GOTO1 VSUMMRY                                                          
MLASTBYS L     R8,BUFFBUFF                                                      
         MVC   DMCB+8(20),LVCNTRL                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',(BUFCDE,(R8))                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',1)                   
         LLC   RE,BUFCDE                                                        
         LA    RE,3(RE)                                                         
         STC   RE,BUFCDE                                                        
         GOTO1 BUFFALO,DMCB,=C'ADD',(BUFCDE,(R8))                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',1)                   
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
EXTRCT   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         STM   RA,RC,SPTHKRA                                                    
         ST    R2,SPTHKR2                                                       
         LA    RE,MEDWEEKS                                                      
         LA    RF,672                                                           
         XCEF                                                                   
         SR    RF,RF                                                            
         IC    RF,PASS                                                          
         MH    RF,=H'268'                                                       
         LA    RF,PASSTAB(RF)                                                   
         USING PASSTABD,RF                                                      
         MVC   MEDNUMWK(L'PASSP1),PASSP1                                        
         MVC   MEDWEEKS(L'PASSP2),PASSP2                                        
         XC    MEDMON01(144),MEDMON01                                           
         MVC   MEDMON01(L'PASSP3),PASSP3                                        
         MVC   MEDPERD(L'PASSP4),PASSP4                                         
         DROP  RF                                                               
         USING PASSTABD,R6                                                      
         MVC   MEDBRAND,BPRD                                                    
         MVC   BYTE,MEDSPTLN                                                    
         MVI   MEDSPTLN,0                                                       
         MVI   MEDEXTDM,4                                                       
         MVC   MEDEXTDM,PRDEMCNT                                                
         MVC   MEDEXTPW,QPWCV                                                   
         XC    SVPERD,SVPERD                                                    
         LA    R5,2                SET DEMO LOOKUP CODE                         
         CLI   QRERATE,C' '                                                     
         BE    EXTRCT2                                                          
         CLI   QRERATE,C'A'        ADJUST ONLY                                  
         BNE   EXTRCT1                                                          
         LA    R5,5                                                             
         B     EXTRCT2                                                          
EXTRCT1  LA    R5,3                SET FOR PURCHASED RERATED                    
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R5,1(R5)            SET FOR ADJUSTMENTS                          
         CLI   QRERATE,C'I'        SET FOR AFFID RERATE                         
         BNE   *+8                                                              
         LA    R5,3(R5)                                                         
*                                                                               
EXTRCT2  DS    0C                                                               
EXTRCT2A L     RE,SVPSLIST                                                      
EXTRCT2B MVC   MEDSPTLN,1(RE)      SET CURRENT PRD/SPT LEN                      
EXTRCT2C MVC   MEDEXTAX,SPOTPROF+12  SET FOR TAX EXCLUSION                      
         ST    R5,FULL             SAVE THE LOOKUP CONTROL                      
         MVI   MEDEXCH,0                                                        
         L     RE,ADAGY            TEST CANADIAN AGENCY                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C'                                        
         BNE   EXTRCT2D                                                         
         L     RE,ADBUY            YES-TEST EXCHANGE ELEMENT                    
         TM    BDCIND2-BUYREC(RE),X'40'                                         
         BZ    EXTRCT2D                                                         
         MVI   MEDEXCH,C'C'        YES-SET EXCHANGE TO CANADIAN DOLLARS         
*                                                                               
EXTRCT2D XC    SPOTHOOK,SPOTHOOK                                                
         CLC   QPROG,=C'DX'        DX AND CP ALWAYS FILTER PIGS/SOLOS           
         BE    EXTRCT2E                                                         
         CLC   QPROG,=C'CP'                                                     
         BE    EXTRCT2E                                                         
         CLI   BPRD2,0             TEST SECOND REQUEST PRODUCT                  
         BE    EXTRCT2F            NO -                                         
*                                                                               
EXTRCT2E LA    R1,SPTHK            YES-PASS A(SPOT HOOK)                        
         ST    R1,SPOTHOOK                                                      
*                                                                               
EXTRCT2F MVI   MEDEXTAC,C'G'                                                    
         CLI   Q2NET,C'B'                                                       
         BE    *+8                                                              
         CLI   Q2NET,C'Y'                                                       
         BE    *+8                                                              
         MVI   MEDEXTAC,C' '                                                    
*                                                                               
         L     RE,ADBUY                                                         
         SR    R0,R0                                                            
         IC    R0,BDPURP-BUYREC(RE)                                             
         MVI   BDPURP-BUYREC(RE),X'FD' TELL GETRATE TO GIVE COSTS               
*                                                                               
EXTR30   GOTO1 MEDGETBY,DMCB,(RA),(R5)                                          
         CLC   QMKT,=C'0000'                                                    
         BNE   EXTR32                                                           
         CLI   QMED,C'N'                                                        
         BNE   EXTR32                                                           
*                                                                               
         L     RE,ADBUY            RESTORE PURPOSE CODE                         
         STC   R0,BDPURP-BUYREC(RE)                                             
*                                                                               
EXTR32   CLI   MEDSPILL,C'Y'                                                    
         BNE   EXTR34                                                           
         CLI   SPOTPROF+5,0                                                     
         BE    EXTRCTX                                                          
*                                                                               
EXTR34   L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   BYTE,BDSEC                                                       
         DROP  R5                                                               
         BAS   RE,GETHP                                                         
         EJECT                                                                  
*===================================================================*           
* DEAL WITH MARKET OVERIDES                                         *           
* THIS CODE EXECUTES FOR EVERY BUY RECORD, BUT THE DOLLARS ARE      *           
* CLEARED ON THE FIRST PASS.                                        *           
*===================================================================*           
         SPACE 1                                                                
         L     R1,=A(PWDLTAB)                                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    EXTPWOVX                                                         
*                                                                               
         LA    R5,MEDMON01         CLEAR TOTALING ACCUMULATORS                  
         LA    RF,MEDPERD          FOR MONTHS/QRTS/PERIODS                      
EXTPWCL  CR    R5,RF                                                            
         BH    EXTPWCLX                                                         
         ICM   R4,15,4(R5)                                                      
         BZ    *+16                                                             
         XC    MEDBYD,MEDBYD                                                    
         XC    MEDGLD,MEDGLD                                                    
         LA    R5,12(R5)                                                        
         B     EXTPWCL                                                          
EXTPWCLX DS    0H                                                               
         L     R5,MEDAFRST                                                      
EXTPWOV  L     R1,=A(PWDLTAB)      PW MARKET LEVEL OVERRIDES                    
         ICM   R4,15,4(R5)                                                      
         BZ    EXTPWOV1A                                                        
         OC    0(4,R5),0(R5)                                                    
         BZ    EXTPWOV1A                                                        
*                                                                               
EXTPWOV1 OC    0(4,R1),0(R1)       ENTRY NOT THERE                              
         BZ    EXTPWOV1A                                                        
         CLC   0(4,R1),0(R5)       HAVE ENTRY FOR THIS DATE PAIR                
         BE    *+12                                                             
         LA    R1,8(R1)                                                         
         B     EXTPWOV1                                                         
         CLC   4(4,R1),=X'80000000'                                             
         BNE   *+8                                                              
         MVI   4(R1),0                                                          
         MVC   MEDBYD,4(R1)        YES - OVERRIDE DOLLARS AND RESET             
         XC    4(4,R1),4(R1)                                                    
*                                                                               
         CLI   Q2NET,C' '          TEST DOLLARS REQUIRE ADJUSTMENT              
         BE    EXTPWOV1A           NO                                           
*                                                                               
         MVC   GROSS,MEDBYD                                                     
         BAS   RE,ADJPW            ADJUST TO NET OR BILL FORMULA                
         MVC   MEDBYD,GROSS        NET/GETBF RETURN ADJ DOLS IN GROSS           
*                                                                               
EXTPWOV1A LA   R5,12(R5)                                                        
         LA    R8,MEDMON01                                                      
         CR    R5,R8                                                            
         BL    EXTPWOV                                                          
         L     R5,MEDAFRST                                                      
*                                                                               
* SUM WEEKS TO MONTHS AND MONTHS TO REQUEST PERIOD                              
*                                                                               
EXTPWOV2 LA    R8,MEDQRT01         STOP AT FIRST QTRLY ACCUM                    
         CR    R5,R8                                                            
         BNL   EXTPWOVX                                                         
         L     R4,4(R5)                                                         
         ICM   R8,15,8(R5)                                                      
         BZ    EXTPWOV3                                                         
*                                                                               
         L     R0,MEDBYD               GET CURRENT DOLLARS                      
         A     R0,MEDBYD-MEDDATA(R8)   ADD TO PERIOD                            
         ST    R0,MEDBYD-MEDDATA(R8)   STORE PERIOD                             
*                                                                               
EXTPWOV3 LA    R5,12(R5)                                                        
         B     EXTPWOV2                                                         
         EJECT                                                                  
EXTPWOVX DS    0H                                                               
         SPACE 2                                                                
* DEAL WITH BILLING ADJUSTMENTS - ONLY REPORT IN TOTALS                         
*                                                                               
EXTPWBO  LA    R5,MEDMON01                                                      
*                                                                               
EXTPWBO2 LA    R8,MEDQRT01         SET LIMIT                                    
         CR    R5,R8                                                            
         BNL   EXTPWBOX                                                         
         L     R1,=A(PWADJTAB)     POINT TO BILLING ADJUSTMENTS                 
*                                                                               
EXTPWBO3 OC    0(4,R1),0(R1)                                                    
         BZ    EXTPWBO6                                                         
         CLC   0(2,R1),0(R5)       FIND THIS MONTH                              
         BL    *+14                 CHECK AGAINST START                         
         CLC   0(2,R1),2(R5)       FIND THIS MONTH                              
         BNH   *+12                 CHECK AGAINST END                           
PWB03NX  LA    R1,8(R1)                                                         
         B     EXTPWBO3                                                         
*                                                                               
         CLC   4(4,R1),=X'80000000'                                             
         BNE   *+10                                                             
         XC    4(4,R1),4(R1)                                                    
*                                                                               
         OC    4(4,R1),4(R1)       TEST NO ADJ                                  
         BZ    PWB03NX                                                          
*                                                                               
         MVC   GROSS,4(R1)         MOVE DOLLARS                                 
         XC    4(4,R1),4(R1)        AND CLEAR AMOUNT                            
*                                                                               
         CLI   Q2NET,C' '          TEST ADJUSTMENT REQUIRED                     
         BE    EXTPWBO4            NO                                           
         BAS   RE,ADJPW                                                         
*                                                                               
         CLI   Q2NET,C'Y'                                                       
         BNE   *+10                                                             
         MVC   GROSS,NET           FOR NET REQUEST REPLACE GROSS                
*                                                                               
         L     R4,4(R5)            POINT TO MONTH DATA                          
         L     R0,MEDBYD                                                        
         A     R0,GROSS                                                         
         ST    R0,MEDBYD                                                        
*                                                                               
EXTPWBO4 L     R4,MEDPERD+4        POINT TO PERIOD DATA                         
         L     R0,MEDBYD                                                        
         A     R0,GROSS            ADD TO PERIOD BUCKETS                        
         ST    RF,MEDBYD                                                        
*                                                                               
EXTPWBO6 LA    R5,12(R5)           NEXT MONTH                                   
         B     EXTPWBO2                                                         
         EJECT                                                                  
*==================================================================*            
* ON ENTRY,  GROSS CONTAINS GROSS DOLLARS.                         *            
* ON EXIT, GROSS CONTAINS NET OR BILLFORM ADJUSTED DOLLARS         *            
*          NET CONTAINS NET DOLLARS                                *            
*==================================================================*            
         SPACE 1                                                                
ADJPW    NTR1                                                                   
         L     R1,GROSS                                                         
         M     R0,=F'170'          X 85 X 2                                     
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BP    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,NET                                                           
         CLI   Q2NET,C'Y'                                                       
         BNE   ADJPW2                                                           
         ST    R1,GROSS            RETURN NET AS GROSS                          
         B     ADJPWX                                                           
*                                                                               
ADJPW2   CLI   Q2NET,C'B'          TEST DO BILL FORMULA                         
         BNE   ADJPWX                                                           
*                                                                               
         XC    SVBFORM,SVBFORM                                                  
         GOTO1 GETBF,DMCB,(BPRD,GROSS),SVBFORM                                  
ADJPWX   XIT1                                                                   
*                                                                               
EXTPWBOX DS    0H                                                               
         EJECT                                                                  
* NOW DO NORMAL PROCESSING                                                      
         LA    R5,MEDPERD          COUNT MARKETS AND WEIGHTS                    
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD   ONLY IF THERE IS ACTIVITY                    
         BZ    EXTRCT3                                                          
         GOTO1 MEDMKTWT,DMCB,(RA),(R9)                                          
*                                                                               
EXTRCT3  MVC   WEIGHT,SPWEIGHT                                                  
         XC    PRTLINE,PRTLINE                                                  
         LA    R3,PRTLINE                                                       
         L     R5,MEDAFRST         RELEASE DATA TO BUFFALO                      
         CLI   MRPTTYP,C'1'        RPT 1 NEEDS WEEKLYS ALWAYS                   
         BE    EXTRCT3A                                                         
         TM    D2APROF+3,X'11'     DISTR. NEEDS WEEKLYS (OPTIONAL)              
         BNZ   EXTRCT3A                                                         
         CLI   MRPTTYP,C'2'        RPT 2 DOESNT NEED WEEKLYS                    
         BE    *+8                                                              
         CLI   MRPTTYP,C'5'        RPT 5 DOESNT NEED WEEKLYS                    
         BNE   EXTRCT3A                                                         
         LA    R5,MEDMON01                                                      
EXTRCT3A MVI   SUMCODE,X'90'                                                    
         MVC   SUMDPGNO(8),MEDDPGNO     SET DAYPART                             
         MVC   SUMSLN,BYTE                                                      
         MVC   MEDSPTLN,BYTE                                                    
         L     R4,4(R5)                                                         
         MVI   SUMRTYP,1                                                        
         LA    RF,MEDPERD                                                       
         CR    R5,RF               END                                          
         BH    EXTRCTX                                                          
         BNE   EXTRCT3B            WEEKLY OR MONTHLY                            
         OC    MEDBYD(12),MEDBYD                                                
         BZ    EXTRCT5              NO - EXIT                                   
         MVI   SUMRTYP,3                                                        
         MVC   SUMDT,=X'FFFFFFFF'                                               
         B     EXTRCT4                                                          
*                                                                               
EXTRCT3B OC    0(4,R5),0(R5)       ACTIVE SLOT                                  
         BZ    EXTRCT5                                                          
         LA    RF,MEDMON01                                                      
         CR    R5,RF               MONTHLY                                      
         BL    EXTRCT3C                                                         
         MVI   SUMRTYP,2            YES-SET RECORD CODE                         
         TM    D2APROF+3,X'22'     MONTHLIES FOR DISTR. RPTS                    
         BNZ   EXTRCT3C                                                         
         CLI   MRPTTYP,C'1'        MONTHLYS FOR RPT 1                           
         BE    EXTRCT3C                                                         
         CLI   MRPTTYP,C'2'        MONTHLYS FOR RPT 2                           
         BE    EXTRCT3C                                                         
         CLI   MRPTTYP,C'4'        MONTHLYS FOR RPT 4                           
         BE    EXTRCT3C                                                         
         CLI   MRPTTYP,C'5'        MONTHLYS FOR RPT 5                           
         BE    EXTRCT3C                                                         
         CLI   MRPTTYP,C'6'        MONTHLYS FOR RPT 6                           
         BE    EXTRCT3C                                                         
         LA    R5,MEDPERD                                                       
         B     EXTRCT3A                                                         
EXTRCT3C MVC   SUMDT,0(R5)                                                      
*                                                                               
EXTRCT4  LA    RE,SUMKEY           SET UP DATA ITEM DISPLACEMENTS               
         USING SUMDATA,RE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     RE,BUFFLKEY                                                      
         LA    R8,MEDPERD          ADD IN TAX AMOUNT                            
         CR    R5,R8                                                            
         BNE   *+16                                                             
         L     R8,TAXAMT                                                        
         A     R8,MEDMSTAX                                                      
         ST    R8,TAXAMT                                                        
*                                                                               
         XC    SUMDATA,SUMDATA                                                  
         MVC   SUMSPOTS,MEDBYSPT   MOVE IN DATA                                 
         MVC   SUMDL,MEDBYD                                                     
         MVC   SUMDLEQ,MEDBYDEQ                                                 
         MVC   SUMD1,MEDBY1                                                     
         MVC   SUMD1EQ,MEDBY1EQ                                                 
*                                                                               
         OC    SUMDLEQ,SUMDLEQ                                                  
         BNZ   *+10                                                             
         MVC   SUMDLEQ,SUMDL                                                    
*                                                                               
         MVC   SUMD2,MEDBY2                                                     
         MVC   SUMD2EQ,MEDBY2EQ                                                 
         MVC   SUMD3,MEDBY3                                                     
         MVC   SUMD3EQ,MEDBY3EQ                                                 
         MVC   SUMD4,MEDBY4                                                     
         MVC   SUMD4EQ,MEDBY4EQ                                                 
         LA    R8,PROGPROF                                                      
         USING PROFDSCT,R8                                                      
         CLI   PROFDPT,C'Y'        DAYPART ANALYSIS REQUIRED                    
         BNE   EXTRCT4T             NO - PUT OUT TOTALS ONLY                    
         DROP  R8                                                               
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
         CLI   SPOTPROF+5,1                                                     
         BE    EXTRCT4T                                                         
         L     R8,BUFFBUFF                                                      
         BAS   R9,EXTPUT                                                        
* PUT OUT PRODUCT GROUP DETAILS                                                 
         MVI   SUMCODE,X'93'                                                    
         BAS   R9,EXTPUT                                                        
EXTRCT4T L     R8,BUFFBUFF                                                      
         MVC   WORK(9),SUMDPGNO                                                 
*        CHECK DISTRIBUTION REPORTING PERIOD                                    
         CLI   SUMRTYP,1           WEEKLY FOR EITHER REPORT                     
         BNE   *+12                                                             
         TM    D2APROF+3,X'11'                                                  
         BZ    EXTNOR                                                           
*                                                                               
         CLI   SUMRTYP,2           MONTHLY FOR EITHER REPORT                    
         BNE   *+12                                                             
         TM    D2APROF+3,X'22'                                                  
         BZ    EXTNOR                                                           
*                                                                               
         CLI   SLSUMSW,C'Y'        SPOT LENGTH DISTRIBUTION REPORT              
         BNE   EXTDPR                                                           
         MVI   SUMCODE,X'96'                                                    
         MVC   SUMSLN,BYTE                                                      
         MVC   SUMDPGNO(8),=9X'FF'                                              
         BAS   R9,EXTPUT2                                                       
         MVI   SUMCODE,X'96'                                                    
         MVC   SUMDPGNO(8),=9X'FF'                                              
         MVI   SUMSLN,0                                                         
         BAS   R9,EXTPUT2                                                       
EXTDPR   DS    0C                                                               
         MVC   SUMDPGNO(9),WORK                                                 
         CLI   DPSUMSW,C'Y'        DAYPART DISTRIBUTION REPORT                  
         BNE   EXTNOR                                                           
         MVI   SUMCODE,X'97'                                                    
         MVI   SUMSLN,X'FF'                                                     
         BAS   R9,EXTPUT2                                                       
         MVI   SUMCODE,X'97'                                                    
         XC    SUMDPGNO(8),SUMDPGNO                                             
         MVI   SUMSLN,X'00'                                                     
         BAS   R9,EXTPUT2                                                       
EXTNOR   DS    0C                                                               
         MVC   SUMDPGNO(9),WORK                                                 
         MVC   SUMDPGNO(9),=9X'FF'                                              
         MVI   SUMCODE,X'90'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'93'                                                    
         BAS   R9,EXTPUT                                                        
         CLI   SPOTPROF+5,2       DO WE NEED SPILL OR ORIG                      
         BE    EXTRCT5             NO                                           
         CLI   SPOTPROF+5,0                                                     
         BE    EXTRCT5                                                          
         CLI   MEDSPILL,C'Y'       IS THIS SPILL                                
         BE    EX4SPL              YES                                          
         OI    SPILORIG,X'01'                                                   
         MVI   SUMSLN,X'FE'        PUT OUT ORIG TOTALS                          
         MVI   SUMCODE,X'89'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'92'                                                    
         BAS   R9,EXTPUT                                                        
         B     EXTRCT5                                                          
EX4SPL   CLI   SPOTPROF+5,1        DO WE NEED SPILL TOTALS                      
         BE    *+8                                                              
         CLI   SPOTPROF+5,3                                                     
         BNE   EXTRCT5                                                          
         OI    SPILORIG,X'02'                                                   
         MVI   SUMSLN,X'FD'        PUT OUT SPILL TOTALS                         
         MVI   SUMCODE,X'88'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,EXTPUT                                                        
* GET NEXT BUFFALO ITEM                                                         
EXTRCT5  LA    R5,12(R5)                                                        
         B     EXTRCT3A                                                         
EXTPUT   CLI   SUMCODE,X'90'                                                    
         BHR   R9                                                               
EXTPUT2  OC    SUMSPOTS(12),SUMSPOTS                                            
         BZR   R9                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),PRTLINE                                
         BR    R9                                                               
         SPACE 2                                                                
EXTRCTX  XIT1                                                                   
         EJECT                                                                  
         DROP  RE                                                               
         EJECT                                                                  
* MEDGETBY HOOK                                                                 
*                                                                               
         USING *,RF                                                             
SPTHK    NTR1                                                                   
         LM    RA,RC,SPTHKRA                                                    
         L     R2,SPTHKR2                                                       
         B     SPTHK2                                                           
SPTHKRA  DC    3F'0'                                                            
SPTHKR2  DC    F'0'                                                             
         DROP  RF                                                               
*                                                                               
SPTHK2   L     R5,SPOTADDR                                                      
         USING REGELEM,R5                                                       
         CLI   RCODE,11            CHECK IT'S A POL BUY ELEMENT                 
         BL    SPTHKY                                                           
         CLI   RCODE,13                                                         
         BH    SPTHKY                                                           
         CLI   RLEN,14             TEST IT'S A PIGGYBACK SPOT                   
         BNH   SPTHK4              NO                                           
* PIGGYBACK SPOT                                                                
         CLI   BPRD2,0             TEST PIGGYBACK REQ                           
         BE    SPTHKN              NO                                           
         CLC   BPRD2,10(R5)        TEST ONE OF THE PRODUCTS IS THE              
         BE    SPTHKY              SECOND REQUESTED PRODUCT                     
         CLC   BPRD2,14(R5)                                                     
         BE    SPTHKY                                                           
         B     SPTHKN              NO-REJECT                                    
*                                                                               
SPTHK4   CLI   BPRD2,0             SPOT NOT PIGGY/TEST PIGGY REQ                
         BNE   SPTHKN              PIGGY REQ - REJECT SOLO SPOT                 
*                                                                               
SPTHKY   MVI   SPOTYORN,C'Y'       ACCEPT                                       
         B     SPTHKX                                                           
*                                                                               
SPTHKN   MVI   SPOTYORN,C'N'       REJECT                                       
*                                                                               
SPTHKX   XIT1  ,                                                                
         EJECT                                                                  
GETHP    NTR1                                                                   
         MVI   MGSW,0                                                           
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         L     R3,ADBUY                                                         
         USING BUYREC,R3                                                        
         L     R5,MEDAFRST                                                      
         MVC   STRDTE,0(R5)                                                     
         L     R5,MEDALAST                                                      
         MVC   ENDDTE,2(R5)                                                     
         LA    R5,BDELEM                                                        
         USING REGELEM,R5                                                       
REGNXT   LLC   RE,RLEN                                                          
         AR    R5,RE                                                            
         CLI   0(R5),0                                                          
         BE    GETHPX                                                           
         CLI   RCODE,6                                                          
         BL    REGNXT                                                           
         CLI   RCODE,13                                                         
         BH    REGNXT                                                           
         CLC   RDATE,STRDTE                                                     
         BL    REGNXT                                                           
         CLC   RDATE,ENDDTE                                                     
         BH    REGNXT                                                           
         CLI   RCODE,11            BRAND ELEMENT                                
         BL    GETHP2               OK                                          
         CLI   MEDBRAND,X'FF'      POL PRODUCT                                  
         BE    GETHP2               OK                                          
         CLI   BRDPOLSW,C'0'       BRAND POL                                    
         BNE   GETHP2               YES - LOOK FOR HIATUS                       
         CLI   RLEN,14             UNALLOCATED                                  
         BL    REGNXT               BYPASS                                      
         BE    *+14                                                             
         CLC   MEDBRAND,RPPRD+4    CHECK PARTNER                                
         BE    *+10                                                             
         CLC   MEDBRAND,RPPRD      WRONG BRAND                                  
         BNE   REGNXT              BYPASS                                       
         SPACE 2                                                                
GETHP2   TM    RSTATUS,X'04'                                                    
         BO    SETHIA                                                           
         TM    RSTATUS,X'40'                                                    
         BO    SETPREM                                                          
         B     REGNXT                                                           
         SPACE 2                                                                
SETHIA   LA    RF,HIATAB           SET FOR HIATUS                               
         B     FLAGHP                                                           
SETPREM  LA    RF,PREMTAB          SET FOR PREMTED                              
         TM    RSTATUS,X'02'                                                    
         BZ    FLAGHP                                                           
         MVI   MGSW,1                                                           
FLAGHP   LA    R4,MEDWEEKS                                                      
FLAGHP1  CLC   2(2,R4),RDATE       FIND PROPER SLOT                             
         BNL   FLAGHP2                                                          
         LA    RF,4(RF)                                                         
         LA    R4,12(R4)                                                        
         CLI   0(R4),0             CHECK FOR ERROR                              
         BNE   FLAGHP1                                                          
         DC    H'0'                DATE OUTSIDE OF RANGE                        
FLAGHP2  L     R6,0(RF)                                                         
         L     R1,ADBUY                                                         
         LA    RE,1                                                             
         TM    BDSTAT-BUYREC(R1),X'80'                                          
         BZ    FLAGHP3                                                          
         LLC   RE,RPCOST           ADD RADIO POL NPW                            
         SRL   RE,2                                                             
FLAGHP3  AR    R6,RE                                                            
         ST    R6,0(RF)                                                         
         TM    RSTATUS,X'02'       MADEGOOD                                     
         BZ    *+8                  SET FLAG                                    
         OI    0(RF),X'80'                                                      
         B     REGNXT                                                           
         SPACE 2                                                                
GETHPX   LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         CLI   SVQOPT1,C'A'        SUPPRESS FOR QOPT1 = A                       
         BE    *+8                                                              
         CLI   SVQOPT1,C'B'        SUPPRESS FOR QOPT1 = B                       
         BE    *+12                                                             
         CLI   PROFHMS,C'Y'                                                     
         BE    GETHPX2                                                          
         XC    HIATAB,HIATAB                                                    
         XC    PREMTAB,PREMTAB                                                  
         MVI   MGSW,0                                                           
GETHPX2  XIT1                                                                   
         DROP  R6                                                               
         DROP  R5                                                               
         DROP  R3                                                               
         DROP  RE                                                               
         LTORG                                                                  
         EJECT                                                                  
GETBUF   NTR1  BASE=*,LABEL=*                                                   
         USING SUMDSECT,R3                                                      
         CLI   BUFHI,1                                                          
         BNE   GETBUF1                                                          
         XC    MYBUFIO(30),MYBUFIO                                              
         MVC   MYBUFIO(1),BUFCDE                                                
         MVI   BUFHI,0                                                          
         L     R8,BUFFBUFF                                                      
         SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',(BUFCDE,(R8)),MYBUFIO,(R9)                 
         B     GETBUF2                                                          
GETBUF1  L     R8,BUFFBUFF                                                      
         SR    R9,R9                                                            
         IC    R9,LEVEL                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFCDE,(R8)),MYBUFIO,(R9)                  
GETBUF2  CLC   MYBUFIO(1),BUFCDE                                                
         BNE   *+12                                                             
         TM    DMCB+8,X'80'                                                     
         BZ    GETBUF3                                                          
         XC    MYBUFIO(30),MYBUFIO                                              
         B     GETBUFX                                                          
GETBUF3  MVC   PRTLINE,MYBUFIO                                                  
         LA    R3,MYBUFIO                                                       
         XC    MYBUFIO(8),MYBUFIO                                               
         MVI   SUMRPT,1                                                         
         MVC   SUMKEY,PRTLINE                                                   
         LA    R8,PRTLINE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     R8,BUFFLKEY                                                      
         DROP  RF                                                               
         MVC   SUMDATA,0(R8)                                                    
         CLI   SPOTPROF+1,C'N'                                                  
         BE    GETBUFX                                                          
                                                                                
* NOTE THAT CODE BELOW USES DNAMES RATHER THAN DEMO LIST                        
* BECAUSE OTHERWISE USER DEMOS DO NOT GET UNWEIGHTED PROPERLY !                 
                                                                                
         LA    R4,DNAMES                                                        
         LA    R5,4                                                             
         LA    R6,SUMD1                                                         
*                                                                               
GBUF4    CLI   SPOTPROF+1,C'D'     UNWEIGHT TOTALS                              
         BE    GBUF4A                                                           
         CLI   0(R4),C'R'                                                       
         BE    GBUF4A                                                           
         CLI   0(R4),C'E'                                                       
         BNE   GBUF5                                                            
*                                                                               
GBUF4A   OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   GBUF4B                                                           
         XC    0(8,R6),0(R6)                                                    
         B     GBUF5                                                            
*                                                                               
GBUF4B   L     RF,0(R6)                                                         
         M     RE,=F'2'                                                         
         D     RE,SPWEIGHT                                                      
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,0(R6)                                                         
*                                                                               
         L     RF,4(R6)                                                         
         M     RE,=F'2'                                                         
         D     RE,SPWEIGHT                                                      
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,4(R6)                                                         
*                                                                               
GBUF5    LA    R4,7(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,GBUF4                                                         
*                                                                               
         CLI   SPOTPROF+1,C'D'                                                  
         BE    GBUF5A                                                           
*                                                                               
         CLI   DNAMES,C'R'                                                      
         BE    GBUF5A                                                           
         CLI   DNAMES,C'E'                                                      
         BNE   GETBUFX                                                          
*                                                                               
GBUF5A   OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   GBUF5B                                                           
         XC    SUMGD1(8),SUMGD1                                                 
         B     GETBUFX                                                          
*                                                                               
GBUF5B   L     RF,SUMGD1                                                        
         M     RE,=F'2'                                                         
         D     RE,SPWEIGHT                                                      
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,SUMGD1                                                        
*                                                                               
         L     RF,SUMGD1E                                                       
         M     RE,=F'2'                                                         
         D     RE,SPWEIGHT                                                      
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,SUMGD1E                                                       
*                                                                               
GETBUFX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*========================================================                       
* BRAND TIME SCHEDULE HEADLINES                                                 
*========================================================                       
                                                                                
BTSHEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*---->   MVC   H1+58(16),=C'BRAND TIMESHEETS'                                   
         MVC   H1+58(L'SP@BRNTH),SP@BRNTH                                       
BTSHEAD1 CLI   SPOTPROF+12,C'Y'                                                 
         BNE   BTSHEAD2                                                         
*---->   MVC   H8+18(18),=C'***TAX EXCLUDED***'                                 
         LA    R1,H8+18                                                         
         TM    Q2USER+16,X'02'      UDEF=E2?                                    
         BZ    *+8                  NO                                          
         LA    R1,H9+18             YES - UDEF ON H8 - DON'T CLOBBER!           
         MVC   0(L'SP@TXEX,R1),SP@TXEX                                          
BTSHEAD2 CLI   MODE,STALAST                                                     
         BH    BTSHX                                                            
         MVC   H10+45(18),DASH                                                  
*---->   MVC   H10+63(19),=C'NUMBER OF TELECASTS'                               
         MVC   H10+63(L'SP@NMTLC),SP@NMTLC                                      
         CLI   QMED,C'R'                                                        
         BE    BTSHD2A                                                          
         CLI   QMED,C'X'                                                        
         BNE   BTSHD2B                                                          
*---->   MVC   H10+63(20),=C'NUMBER OF BROADCASTS'                              
BTSHD2A  MVC   H10+63(L'SP@NMBRC),SP@NMBRC                                      
BTSHD2B  MVC   H10+84(17),DASH                                                  
         CLI   DETOPTS+2,0                                                      
         BE    *+10                                                             
*---->   MVC   H13+102(4),=C'COST'                                              
         MVC   H13+102(L'SP@COST),SP@COST                                       
         LA    R1,H11+44                                                        
         ST    R1,AHDATES                                                       
         GOTO1 VHDATES                                                          
         CLI   DETOPTS+1,0                                                      
         BNE   *+12                                                             
         CLI   DETOPTS+3,0                                                      
         BE    BTSHEAD3                                                         
*---->   MVC   H10+107(21),=C'DEMOGRAPHICS(IMP)/CPP'                            
         MVC   H10+107(L'SP@DMGR3),SP@DMGR3                                     
         MVC   H11+107(7),DNAME1                                                
         MVC   H11+119(7),DNAME3                                                
         MVC   H12+113(7),DNAME2                                                
         MVC   H12+125(7),DNAME4                                                
BTSHEAD3 DS    0H                                                               
*---->   MVC   H12(3),=C'DAY'                                                   
         MVC   H12(L'SP@DAY),SP@DAY                                             
         MVC   H13(3),DASH                                                      
*---->   MVC   H12+9(4),=C'TIME'                                                
         MVC   H12+9(L'SP@TIME),SP@TIME                                         
         MVC   H13+9(4),DASH                                                    
*---->   MVC   H12+21(11),=C'PROGRAMMING'                                       
         MVC   H12+21(L'SP@PROG),SP@PROG                                        
         MVC   H13+21(11),DASH                                                  
*---->   MVC   H12+38(07),=C'LNG/D/P'                                           
         MVC   H12+38(L'SP@LNGDP),SP@LNGDP                                      
         MVC   H13+38(07),DASH                                                  
*                                                                               
         CLI   D2APROF+9,C'N'         SUPPRESS DAYPART/PURPOSE                  
         BNE   *+16                                                             
         MVC   H12+38+3(4),SPACES                                               
         MVC   H13+38+3(4),SPACES                                               
*                                                                               
         CLI   QBYID,C'Y'                                                       
         BNE   BTSHX                                                            
         MVC   H3(12),BUYIDNAM                                                  
         MVC   H3+13(12),BUYID                                                  
BTSHX    XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* SALESPERSONS HEADLINES                                                        
*==============================================================                 
                                                                                
SALHEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*                                                                               
SALH02   CLC   QPROG,=C'CP'                                                     
         BE    SALH06                                                           
         CLI   SPOTPROF+12,C'Y'                                                 
         BNE   SALH03                                                           
*---->   MVC   H8(18),=C'***TAX EXCLUDED***'                                    
         LA    R1,H8                                                            
         TM    Q2USER+16,X'02'      UDEF=E2?                                    
         BZ    *+8                  NO                                          
         LA    R1,H9                YES - UDEF ON H8 - DON'T CLOBBER!           
         MVC   0(L'SP@TXEX,R1),SP@TXEX                                          
*                                                                               
SALH03   CLC   QPROG,=C'D7'        D7 GETS SPECIAL HEADLINES                    
         BE    *+14                                                             
         CLC   QPROG,=C'DX'                                                     
         BNE   SALH06                                                           
*                                                                               
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
*                                                                               
*---->   MVC   H1+53(24),=C'CONFIRMATION OF PURCHASE'                           
         MVC   H1+53(L'SP@CNFPU),SP@CNFPU                                       
         XC    H4+99(32),H4+99                                                  
         MVI   H5+99,C'_'                                                       
         MVC   H5+100(32),H5+99                                                 
*                                                                               
         MVI   SALZEN,C'Y'         SET ZENITH SAL REPORT FLAG                   
         CLC   QAGY(2),=C'TH'      ONLY IF A ZENITH REQUEST                     
         BE    SALH20                                                           
         MVI   SALZEN,C'N'                                                      
*                                                                               
         MVI   SALOPT,C'Y'         SET OPTIMEDIA SAL REPORT FLAG                
         CLC   QAGY(2),=C'PC'      ONLY IF A OPTIMEDIA REQUEST                  
         BNE   SALH04                                                           
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
         CLI   COFFICE,C'I'        AND OFFICE IS I                              
         BE    SALH20                                                           
SALH04   MVI   SALOPT,C'N'                                                      
         DROP  RF                                                               
*                                                                               
*---->   MVC   H6+99(22),=C'SALESPERSONS SIGNATURE'                             
         MVC   H6+99(L'SP@SLPSI),SP@SLPSI                                       
         XC    H7+99(33),H7+99                                                  
*                                                                               
         CLC   Q2ADDS,SPACES       TEST REVISION DATA                           
         BE    SALH20                                                           
         LA    R4,H6+73            POINT TO PRINT POSITION                      
         BRAS  RE,PRTVRSN                                                       
         B     SALH20                                                           
*                                                                               
SALH06   CLC   RTYPE,=C'BDS'                                                    
         BNE   SALH10                                                           
*---->   MVC   H1+55(19),=C'DAILY TIME SCHEDULE'                                
         MVC   H1+55(L'SP@DYTS),SP@DYTS                                         
         B     SALH20                                                           
SALH10   DS    0H                                                               
*---->   MVC   H1+51(26),=C'SALESPERSONS TIME SCHEDULE'                         
         MVC   H1+51(L'SP@SLPTS),SP@SLPTS                                       
         CLI   PWPREFIX,C' '                                                    
         BE    SALH20                                                           
         MVC   H1+51(L'SP@SLPTS),SPACES                                         
         MVC   H1+51(08),=C'SCHEDULE'                                           
*                                                                               
SALH20   CLI   MODE,STALAST                                                     
         BH    SALHX                                                            
         MVC   H10+43(17),DASH                                                  
*---->   MVC   H10+61(19),=C'NUMBER OF TELECASTS'                               
         MVC   H10+61(L'SP@NMTLC),SP@NMTLC                                      
         CLI   QMED,C'R'                                                        
         BE    SALH22                                                           
         CLI   QMED,C'X'                                                        
         BNE   SALH24                                                           
*---->   MVC   H10+61(20),=C'NUMBER OF BROADCASTS'                              
SALH22   MVC   H10+61(L'SP@NMBRC),SP@NMBRC                                      
*                                                                               
SALH24   MVC   H10+82(16),DASH                                                  
*---->   MVC   H12(3),=C'DAY'                                                   
         MVC   H12(L'SP@DAY),SP@DAY                                             
         MVC   H13(L'SP@DAY),DASH                                               
*---->   MVC   H12+9(4),=C'TIME'                                                
         MVC   H12+9(L'SP@TIME),SP@TIME                                         
         MVC   H13+9(L'SP@TIME),DASH                                            
*---->   MVC   H12+21(11),=C'PROGRAMMING'                                       
         CLI   D2APROF+9,C'Y'                                                   
         BNE   *+16                                                             
         MVC   H12+37(L'SP@DP),SP@DP                                            
         MVC   H13+37(L'SP@DP),DASH                                             
         MVC   H12+21(L'SP@PROG),SP@PROG                                        
         MVC   H13+21(L'SP@PROG),DASH                                           
*---->   MVC   H12+40(3),=C'LNG'                                                
         MVC   H12+40(L'SP@LNG),SP@LNG                                          
         MVC   H13+40(L'SP@LNG),DASH                                            
         CLC   QPROG,=C'D6'                                                     
         BE    SALH25                                                           
         CLI   D2APROF+9,C'Y'                                                   
         BNE   *+20                                                             
         MVC   H12+38(4),=C'DP/L'                                               
         MVC   H13+38(4),DASH                                                   
         MVI   H13+42,C' '                                                      
SALH25   CLI   SVQOPT1,C'A'        SPECIAL REPORT                               
         BE    *+16                                                             
         MVI   H10+98,C'T'                                                      
         MVI   H11+99,C'O'                                                      
         MVI   H12+100,C'T'                                                     
         CLI   DETOPTS+2,0         SUPPRESS COST                                
         BE    SALH30               YES                                         
         CLC   QPROG,=C'CP'        CP REPORT SUPPRESSES TITLE                   
         BE    SALH30                                                           
*---->   MVC   H12+102(4),=C'COST'                                              
         MVC   H12+102(L'SP@COST),SP@COST                                       
         MVC   H13+102(4),DASH                                                  
         CLC   QPROG,=C'D7'                                                     
         BE    *+14                                                             
         CLC   QPROG,=C'DX'                                                     
         BNE   SALH30                                                           
*                                                                               
*---->   MVC   H12+102(6),=C'  COST'                                            
         MVC   H12+102(L'SP@COST),SP@COST                                       
*---->   MVC   H13+102(6),=C'  ----'                                            
         MVC   H13+102(L'SPUCOST),SPUCOST                                       
*                                                                               
*                                                                               
SALH30   CLI   DETOPTS+1,1         PRINT DEMOS                                  
         BE    *+12                                                             
         CLI   DETOPTS+3,0                                                      
         BE    SALH40                                                           
         CLI   DETOPTS+1,0         DEMS - SEED DEMO INFO                        
         BE    SALH40                                                           
*                                                                               
         CLI   Q2FAXDEM,C'Y'                                                    
         BNE   SALH32                                                           
* DEMO TITLES FOR DX                                                            
         LA    R1,DXDEMDSH         POINT TO LIST OF DSPLS                       
*                                                                               
         LA    RE,H10-1                                                         
         AH    RE,0(R1)                                                         
*---->   MVC   H10+1??(21),=C'DEMOGRAPHICS(IMP=000)'                            
         MVC   0(L'SP@DMGR4,RE),SP@DMGR4                                        
*                                                                               
         LA    RE,H11-1                                                         
         AH    RE,0(R1)                                                         
         MVC   0(7,RE),DNAME1                                                   
*                                                                               
         LA    RE,H11-1                                                         
         AH    RE,2(R1)                                                         
         MVC   0(7,RE),DNAME2                                                   
*                                                                               
         LA    RE,H11-1                                                         
         AH    RE,4(R1)                                                         
         MVC   0(7,RE),DNAME3                                                   
*                                                                               
         LA    RE,H11-1                                                         
         AH    RE,6(R1)                                                         
         MVC   0(7,RE),DNAME4                                                   
         B     SALH50                                                           
* FOR D6 ONLY                                                                   
*---->   MVC   H10+107(21),=C'DEMOGRAPHICS(IMP=000)'                            
SALH32   MVC   H10+107(L'SP@DMGR4),SP@DMGR4                                     
         CLC   QPROG,=C'D6'                                                     
         BNE   *+12                                                             
         CLI   PROGPROF+6,C'Y'     TEST PRINT DEMOS ON SEP LINES                
         BE    SALH50                                                           
         MVC   H11+106(7),DNAME1                                                
         MVC   H12+112(7),DNAME2                                                
         MVC   H11+118(7),DNAME3                                                
         MVC   H12+124(7),DNAME4                                                
         B     SALH50                                                           
*                                                                               
SALH40   CLC   H3+98(6),=C'******'                                              
         BE    SALH50                                                           
         XC    H4+99(33),H4+99     NO DEMS - SUPRESS DEMO INFO                  
         XC    H5+99(33),H5+99                                                  
         CLC   QPROG,=C'CP'                                                     
         BE    *+10                                                             
         CLC   QPROG,=C'D7'                                                     
         BE    *+10                                                             
         CLC   QPROG,=C'DX'                                                     
         BE    *+10                                                             
         XC    H7+99(33),H7+99                                                  
*                                                                               
SALH50   LA    R1,H11+42                                                        
         ST    R1,AHDATES                                                       
         GOTO1 VHDATES                                                          
         CLI   QBYID,C'Y'                                                       
         BNE   SALH60                                                           
         MVC   H3(12),BUYIDNAM                                                  
         MVC   H3+13(12),BUYID                                                  
*                                                                               
* FOR DX MOVE H9-H13 DOWN ONE LINE SO CAN PRINT ON H8/H9                        
*                                                                               
SALH60   CLC   QPROG,=C'DX'                                                     
         BNE   SALH70                                                           
         CLC   Q2DARORD,SPACES                                                  
         BE    SALH70                                                           
*                                                                               
         MVC   H14,H13                                                          
         MVC   H13,H12                                                          
         MVC   H12,H11                                                          
         MVC   H11,H10                                                          
         MVC   H10,H9                                                           
         MVC   H9,SPACES                                                        
* PRINT DX ORDER/REVISION STATUS/CASH TRADE IN MIDLINES                         
         MVC   H8(37),=CL38'** DARE ORDER 12345678 - ORIGINAL **'               
         MVC   H8+14(8),Q2DARORD                                                
         CLI   Q2ADDS,0                                                         
         BNE   SALH62                                                           
         CLI   Q2ADDS+2,0          TEST DATE PRESENT                            
         BE    SALH66              NO                                           
         MVC   H8+34(12),=CL12'- RESEND **'                                     
         B     SALH66                                                           
*                                                                               
SALH62   MVC   H8+25(14),SPACES                                                 
         MVC   H8+25(10),=C'REV 001 **'                                         
         LLC   R0,Q2ADDS                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H8+29(3),DUB                                                     
         CLC   Q2ADDS(1),Q2ADDS+1   TEST SAME VERSION AGAIN                     
         BNE   SALH64               NO                                          
         MVC   H8+33(12),=CL12'- RESEND **'                                     
         B     SALH66                                                           
*                                                                               
SALH64   MVC   H9+3(20),=C'REPLACES REV 000  OF'                                
         LLC   R0,Q2ADDS+1                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H9+16(3),DUB                                                     
         CLI   Q2ADDS+1,0          TEST REPLACES ORIGINAL                       
         BNE   *+10                                                             
         MVC   H9+12(8),=C'ORIGINAL'                                            
         GOTO1 DATCON,DMCB,(2,Q2ADDS+2),(5,H9+24)                               
*                                                                               
SALH66   CLI   DRCSHTRD,0          TEST CASH OR TRADE ORDER                     
         BE    SALHX                                                            
         MVC   H8+51(30),=CL30'*****  TRADE SPOTS ONLY  *****'                  
         CLI   DRCSHTRD,C'R'                                                    
         BE    SALHX                                                            
         MVC   H8+51(30),=CL30'*****  CASH  SPOTS ONLY  *****'                  
         B     SALHX                                                            
*                                                                               
SALH70   CLI   SVQOPT1,C'B'                                                     
         BNE   SALHX                                                            
         XC    H9+50(50),H9+50                                                  
         MVC   H1+25(84),H1+48                                                  
         MVC   H2+25(84),H2+48                                                  
         MVC   H3+25(84),H3+48                                                  
         MVC   H3+76(33),H9+99                                                  
         XC    H8+99(33),H8+99                                                  
         XC    H1+109(23),H1+109                                                
         XC    H2+109(23),H2+109                                                
         XC    H3+109(23),H3+109                                                
         DROP  R5                                                               
SALHX    XIT1                                                                   
*                                                                               
DXDEMDSH DC    AL2(111,121,243,253)    PRINT POSNS FOR 4 DEMOS                  
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* PRINT VERSION DATA FOR CP AND DX REPORTS                                      
* ON ENTRY R4 POINTS TO PRINT POSITION (DIFFERENT FOR ZENITH)                   
* Q2ADDS+0(1) = CURRENT VERSION NUMBER                                          
*       +1(1) = PREVIOUS VERSION (IF = CURRENT AND NOT 0, RESEND)               
*       +2(2) = PREVIOUS VERSION DATE                                           
*================================================================               
                                                                                
PRTVRSN  NTR1  BASE=*,LABEL=*                                                   
         MVC   0(20,R4),=C'** ORIGINAL ORDER **'                                
         CLI   Q2ADDS,1                                                         
         BNE   PRTVRS2                                                          
         CLI   Q2ADDS+1,1          TEST RESEND                                  
         BNE   PRTVRSX                                                          
         MVC   0(27,R4),=C'** ORIGINAL ORDER-RESEND **'                         
         B     PRTVRSX                                                          
*                                                                               
PRTVRS2  MVC   0(21,R4),=C'**** VERSION 001 ****'                               
         LLC   R0,Q2ADDS                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  13(3,R4),DUB                                                     
         CLC   Q2ADDS(1),Q2ADDS+1   TEST RESEND                                 
         BNE   PRTVRS4              NO                                          
         MVC   0(22,R4),=C'* VERSION 001 RESEND *'                              
         UNPK  10(3,R4),DUB                                                     
         J     EXIT                                                             
*                                                                               
PRTVRS4  MVC   132(19,R4),=C'REPLACES VER 000 OF'                               
         LLC   R0,Q2ADDS+1                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  132+13(3,R4),DUB                                                 
         GOTO1 DATCON,DMCB,(2,Q2ADDS+2),(5,132+20(R4))                          
*                                                                               
PRTVRSX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* BRS TURNAROUND BUYSHEETS                                                      
BRSHEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*---->   MVC   H1+54(19),=C'BRAND TIME SCHEDULE'                                
         MVC   H1+54(L'SP@BRNTS),SP@BRNTS                                       
*                                                                               
BRSHEAD1 CLI   SPOTPROF+12,C'Y'                                                 
         BNE   *+10                                                             
*---->   MVC   H9(18),=C'***TAX EXCLUDED***'                                    
         MVC   H9(L'SP@TXEX),SP@TXEX                                            
         CLI   MODE,STALAST                                                     
         BH    BRSHX                                                            
*---->   MVC   H10(28),=C'EST-LIN  BUY PERIOD  WKS DAY'                         
         MVC   H10(L'SP@EST03),SP@EST03                                         
*---->   MVC   H10+32(08),=C'N/W TIME'                                          
         MVC   H10+32(L'SP@NWTM),SP@NWTM                                        
*---->   MVC   H10+47(2),=C'DP'                                                 
         MVC   H10+47(L'SP@DP),SP@DP                                            
         MVC   H10+50(17),DASH                                                  
*---->   MVC   H10+68(19),=C'NUMBER OF TELECASTS'                               
         MVC   H10+68(L'SP@NMTLC),SP@NMTLC                                      
         CLI   QMED,C'R'                                                        
         BE    BRSHD1A                                                          
         CLI   QMED,C'X'                                                        
         BNE   BRSHD1B                                                          
*---->   MVC   H10+68(20),=C'NUMBER OF BROADCASTS'                              
BRSHD1A  MVC   H10+68(L'SP@NMBRC),SP@NMBRC                                      
BRSHD1B  MVC   H10+89(17),DASH                                                  
         MVC   H10+89(17),DASH                                                  
         MVC   H11(49),DASH                                                     
         LA    R1,H11+49                                                        
         ST    R1,AHDATES                                                       
         GOTO1 VHDATES                                                          
*                                                                               
*---->   MVC   H12(24),=C' BOOK    LNG PROGRAMMING'                             
         MVC   H12(L'SP@BK02),SP@BK02                                           
*---->   MVC   H12+36(13),=C'COST   PT PKG'                                     
         MVC   H12+36(L'SP@COST3),SP@COST3                                      
         CLI   DETOPTS+1,0         PRINT DEMOS                                  
         BNE   *+8                                                              
         CLI   DETOPTS+3,0                                                      
         BE    BRSHX                                                            
*---->   MVC   H10+107(25),=C'DEMOGRAPHICS(IMP=000)/CPP'                        
         MVC   H10+107(L'SP@DMGR5),SP@DMGR5                                     
         MVC   H11+107(7),DNAME1                                                
         MVC   H11+119(7),DNAME3                                                
         MVC   H12+113(7),DNAME2                                                
         MVC   H12+125(7),DNAME4                                                
         CLI   QBYID,C'Y'                                                       
         BNE   BRSHX                                                            
         MVC   H3(12),BUYIDNAM                                                  
         MVC   H3+13(12),BUYID                                                  
         DROP  R5                                                               
BRSHX    XIT1                                                                   
BKCAP    DC    C'**ALL MAKEGOODS MUST RUN WITHIN THE FLIGHT DATES OF THX        
               E ORIGINAL PROMOTION**'                                          
         LTORG                                                                  
         EJECT                                                                  
* BUILD GRID DATES IN HEADLINES                                                 
HDATES   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,MEDWEEKS                                                      
         LA    R6,14                                                            
         LR    R9,R1                                                            
         LR    R8,R1               SET PRINT LINE ADDRESS                       
         XC    0(56,R8),0(R8)      CLEAR DATE PORTION OF PL                     
         XC    133(55,R8),133(R8)                                               
**NOP**  XC    132(56,R8),132(R8)                                               
HDATES1  GOTO1 DATCON,DMCB,(X'02',(R5)),(X'07',WORK)                            
         MVC   1(3,R8),WORK                                                     
         MVC   134(2,R8),WORK+3                                                 
         LA    R5,12(R5)                                                        
         OC    0(4,R5),0(R5)                                                    
         BZ    HDATES2                                                          
         LA    R8,4(R8)                                                         
         BCT   R6,HDATES1                                                       
*                                                                               
HDATES2  MVC   CURPDAT1(56),0(R9)                                               
         MVC   CURPDAT2(56),133(R9)                                             
         OC    CURPDAT1(112),SPACES                                             
         CLC   CURPDAT1(112),CURHDAT1                                           
         BE    *+10                                                             
         XC    CURHDAT1,CURHDAT1                                                
*                                                                               
HDATESX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* SALESPERSONS SUMMARY                                                          
SALSUM   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         MVI   SALACTIV,0                                                       
         MVC   SVSUMOPT,SUMOPTS                                                 
         MVC   P,SPACES                                                         
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         MVI   BRSPCAP,C' '                                                     
         MVC   BRSPCAP+1(L'BRSPCAP-1),BRSPCAP                                   
         CLI   MODE,STALAST                                                     
         BNE   *+10                                                             
         MVC   BRSPCAP(L'SP@STATL),SP@STATL   ***STATION TOTAL***               
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
         MVC   BRSPCAP(L'SP@MKTTL),SP@MKTTL   ***MARKET TOTAL***                
         CLI   MODE,PRDLAST                                                     
         BNE   *+14                                                             
         MVC   BRSPCAP(L'SP@PROTL),SP@PROTL   ***PRODUCT TOTAL***               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   D2APROF+6,0                                                      
         BNE   *+8                                                              
         MVI   D2APROF+6,X'70'                                                  
         MVC   BRSPASS2(1),D2APROF+6                                            
         NI    BRSPASS2,X'0F'      EXTRACT R2 DATE BITS                         
         LLC   RE,D2APROF+6        SET THE DATE TYPES                           
         SRL   RE,4                                                             
         STC   RE,BRSDATE                                                       
*                                                                               
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         CLI   BUFCDE,X'96'                                                     
         BE    SALSL                                                            
         CLI   BUFCDE,X'97'                                                     
         BE    SALDP                                                            
*                                                                               
         CLI   MODE,STALAST        ONLY FIRST REPORT                            
         BH    *+8                                                              
         MVI   BRSPASS2,0                                                       
*                                                                               
         CLI   SVQOPT1,C'B'                                                     
         BE    SALSUMX                                                          
         OC    SUMOPTS,SUMOPTS                                                  
         BZ    SALSUMX                                                          
         MVI   BUFHI,1             SET FOR READ HI                              
         MVI   BUFRTYP,1                                                        
*                                                                               
SALDPT   DS    0H                  DAYPART PRINT RETURN                         
         MVI   ALLOWLIN,6                                                       
         MVI   FORCEMID,C'Y'                                                    
         MVC   MID1+56(L'BRSPCAP),BRSPCAP                                       
*                                                                               
SALDPT1  DS    0H                                                               
         CLI   MID1,C' '                                                        
         BNE   *+8                                                              
         MVI   MID1,0                                                           
         MVI   FORCEMID,C'Y'                                                    
         CLI   SUMOPTS,0           SUPPRESS TELECASTS                           
         BE    SALSUMA              YES                                         
*---->   MVC   MID2+29(09),=C'NO TLCSTS'                                        
         MVC   MID2+29(L'SP@NOTLC),SP@NOTLC                                     
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
*---->   MVC   MID2+29(10),=C'NO BRDCSTS'                                       
         MVC   MID2+29(L'SP@NOBRD),SP@NOBRD                                     
SALSUMA  CLI   SUMOPTS+2,0         SUPPRESS DEMOS                               
         BE    SALSUMB              YES                                         
         MVC   MID2+49(7),DNAME1                                                
         CLI   DNAME1,C'R'                                                      
         BE    *+16                                                             
         MVC   MID2+44(7),DNAME1                                                
         MVC   MID2+51(5),=C'(000)'                                             
         CLI   DNAME1,0                                                         
         BNE   *+10                                                             
         XC    MID2+44(12),MID2+44                                              
         MVC   MID2+64(7),DNAME2                                                
         CLI   DNAME2,C'R'                                                      
         BE    *+16                                                             
         MVC   MID2+59(7),DNAME2                                                
         MVC   MID2+66(5),=C'(000)'                                             
         CLI   DNAME2,0                                                         
         BNE   *+10                                                             
         XC    MID2+59(12),MID2+59                                              
         MVC   MID2+79(7),DNAME3                                                
         CLI   DNAME3,C'R'                                                      
         BE    *+16                                                             
         MVC   MID2+74(7),DNAME3                                                
         MVC   MID2+81(5),=C'(000)'                                             
         CLI   DNAME3,0                                                         
         BNE   *+10                                                             
         XC    MID2+74(12),MID2+74                                              
         MVC   MID2+94(7),DNAME4                                                
         CLI   DNAME4,C'R'                                                      
         BE    *+16                                                             
         MVC   MID2+89(7),DNAME4                                                
         MVC   MID2+96(5),=C'(000)'                                             
         CLI   DNAME4,0                                                         
         BNE   *+10                                                             
         XC    MID2+89(12),MID2+89                                              
SALSUMB  CLI   SUMOPTS+1,0         SUPPRESS DOLLARS                             
         BE    *+10                 YES                                         
*---->   MVC   MID2+110(7),=C'DOLLARS'                                          
         MVC   MID2+110(L'SP7DOLLA),SP7DOLLA                                    
         XC    SALSDATA,SALSDATA                                                
*                                                                               
         CLI   PWPRESW,C'Y'                                                     
         BNE   SALSUM1                                                          
         LA    R6,MID2+110                                                      
         MVC   0(10,R6),=C'GROSS COST'                                          
         CLI   Q2NET,C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(10,R6),=C'  NET COST'                                          
         CLI   Q2NET,C'B'                                                       
         BNE   *+10                                                             
         MVC   0(10,R6),=C' *NET COST'                                          
SALSUM1  GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   SUMRTYP,0                                                        
         BE    SALSUMX                                                          
         MVI   SALACTIV,1                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    SALSUM1A                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    SALSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    SALSUM1                                                          
SALSUM1A DS    0H'0'                                                            
         MVC   MID1+48(9),SPACES                                                
         CLI   BUFCDE,X'88'        SET SPILL MID LINE                           
         BNE   *+10                                                             
*---->   MVC   MID1+48(9),=C'***SPILL '                                         
         MVC   MID1+48(L'SP8SPILL),SP8SPILL                                     
         CLI   BUFCDE,X'89'        SET ORIG. MID LINE                           
         BNE   *+10                                                             
*---->   MVC   MID1+48(9),=C'***ORIG. '                                         
         MVC   MID1+48(L'SP8ORIG),SP8ORIG                                       
         DROP  R5                                                               
         CLI   SUMRTYP,2                                                        
         BE    SALSUM1                                                          
         CLI   SUMRTYP,3           TOTAL RECORD                                 
         BE    SALSUM2              YES - PRINT SUMMARY                         
         OC    SUMSPOTS,SUMSPOTS   ANY SPOTS IN WEEK                            
         BZ    SALSUM1              NO - BYPASS RECORD                          
         L     RE,SALSWKS           YES - ACCUMMULATE WEEKLY AVERAGE            
         LA    RE,1(RE)                                                         
         ST    RE,SALSWKS                                                       
         L     RE,SALSSPT                                                       
         A     RE,SUMSPOTS                                                      
         ST    RE,SALSSPT                                                       
         L     RE,SALSD1                                                        
         A     RE,SUMD1                                                         
         ST    RE,SALSD1                                                        
         L     RE,SALSD2                                                        
         A     RE,SUMD2                                                         
         ST    RE,SALSD2                                                        
         L     RE,SALSD3                                                        
         A     RE,SUMD3                                                         
         ST    RE,SALSD3                                                        
         L     RE,SALSD4                                                        
         A     RE,SUMD4                                                         
         ST    RE,SALSD4                                                        
         L     RE,SALSDL                                                        
         A     RE,SUMDL                                                         
         ST    RE,SALSDL                                                        
         B     SALSUM1                                                          
*                                                                               
SALSUM2  OC    SALSWKS,SALSWKS                                                  
         BZ    SALSUM1                                                          
*                                                                               
         CLI   SUMDPART,C''       SPECIAL DETAIL SUPPRESS FLAG                 
         BE    SALSUM1                                                          
         CLI   SUMDPGRP,C''                                                    
         BE    SALSUM1                                                          
*                                                                               
         LA    R5,SALSSPT                                                       
         LA    R6,6                                                             
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   SUMDPGNO,X'FF'                                                   
         BNE   SALSUM2A                                                         
*---->   MVC   MID2(7),=C'MGR TOT'                                              
         MVC   MID2(L'SP@MGRTL),SP@MGRTL                                        
         CLI   MODE,STALAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID2(7),=C'STA TOT'                                              
         MVC   MID2(L'SP7STATL),SP7STATL                                        
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID2(7),=C'MKT TOT'                                              
         MVC   MID2(L'SP7MKTTL),SP7MKTTL                                        
         CLI   MODE,PRDLAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID2(7),=C'PRD TOT'                                              
         MVC   MID2(L'SP7PROTL),SP7PROTL                                        
         B     SALSUM3                                                          
         DROP  RE                                                               
SALSUM2A MVC   MID2(3),SUMDPART                                                 
         EDIT  (1,SUMSLN),(3,MID2+4)                                            
         MVI   MID2+3,C'-'                                                      
SALSUM3  L     RE,0(R5)            CALCULATE WEEKLY AVERAGES                    
         SRDA  RE,31                                                            
         D     RE,SALSWKS                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,0(R5)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,SALSUM3                                                       
* PRINT SALESPERSONS SUMMARY                                                    
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P1(5),=C'TOTAL'                                                  
         MVC   P1(L'SP5TOTAL),SP5TOTAL                                          
         DROP  RE                                                               
         CLI   SUMOPTS,0                                                        
         BE    SALSUM10                                                         
         EDIT  (4,SUMSPOTS),(8,P1+30)                                           
SALSUM10 CLI   SUMOPTS+1,0                                                      
         BE    SALSUM11                                                         
         CLI   HADOFLOW,C'Y'       $ TOTALLING OVERFLOW                         
         BNE   *+14                                                             
         MVC   P1+112(5),=C'*****'                                              
         B     SALSUM11                                                         
         MVI   CURTAB+3,2                                                       
         CURED (4,SUMDL),(12,P1+105),CURTAB,DMCB=CURDMCB,CURSYMB=YES            
*                                                                               
SALSUM11 LA    R4,DNAME1                                                        
         LA    R5,P1+47                                                         
         LHI   R6,4                                                             
         LA    R7,SUMD1                                                         
*                                                                               
         CLI   SUMOPTS+2,0                                                      
         BE    SALSUM18                                                         
*                                                                               
SALSUM12 CLI   0(R4),0             TEST LAST DEMO                               
         BE    SALSUM18                                                         
*                                                                               
         CLI   0(R4),C'R'                                                       
         BE    *+12                                                             
         CLI   0(R4),C'E'                                                       
         BNE   SALSUM14                                                         
*                                                                               
         MVI   CURTAB+3,2                                                       
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC RTGS ACTIVE                       
         BO    SALSUM16                                                         
         MVI   CURTAB+3,1                                                       
         B     SALSUM16                                                         
*                                                                               
SALSUM14 MVI   CURTAB+3,2              DEMO IS AN IMP                           
         TM    RQOPTS,RQOPTS_2DECIMP   TEST 2-DEC IMPS ACTIVE                   
         JZ    SALSUM16                                                         
         MVI   CURTAB+3,1                                                       
*                                                                               
SALSUM16 CURED (4,(R7)),(9,(R5)),CURTAB,DMCB=CURDMCB                            
         AHI   R4,7                NEXT DEMO NAME                               
         AHI   R5,15               NEXT PRINT POSITION                          
         AHI   R7,8                                                             
         BCT   R6,SALSUM12                                                      
*                                                                               
SALSUM18 DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,0                                                       
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P1(8),=C'WKLY AVG'                                               
         MVC   P1(L'SP@WKAVG),SP@WKAVG                                          
         DROP  RE                                                               
         CLI   SUMOPTS,0                                                        
         BE    SALSUM20                                                         
         EDIT  (4,SALSSPT),(8,P+30)                                             
SALSUM20 CLI   SUMOPTS+1,0                                                      
         BE    SALSUM21                                                         
         CLI   HADOFLOW,C'Y'       $ TOTALLING OVERFLOW                         
         BNE   *+14                                                             
         MVC   P1+112(5),=C'*****'                                              
         B     SALSUM21                                                         
         CURED (4,SALSDL),(12,P1+105),CURTAB,DMCB=CURDMCB,CURSYMB=YES           
*                                                                               
SALSUM21 LA    R4,DNAME1                                                        
         LA    R5,P1+47                                                         
         LHI   R6,4                                                             
         LA    R7,SUMD1                                                         
*                                                                               
         CLI   SUMOPTS+2,0                                                      
         BE    SALSUM30                                                         
*                                                                               
SALSUM22 CLI   0(R4),0             TEST LAST DEMO                               
         BE    SALSUM30                                                         
*                                                                               
         CLI   0(R4),C'R'                                                       
         BE    *+12                                                             
         CLI   0(R4),C'E'                                                       
         BNE   SALSUM24                                                         
*                                                                               
         MVI   CURTAB+3,2                                                       
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMALS ACTIVE                       
         BZ    SALSUM26                                                         
         MVI   CURTAB+3,1                                                       
*                                                                               
SALSUM24 MVI   CURTAB+3,2                                                       
         TM    RQOPTS,RQOPTS_2DECIMP  TEST 2-DEC IMPS ACTIVE                    
         JZ    SALSUM26                                                         
         MVI   CURTAB+3,1                                                       
*                                                                               
SALSUM26 CURED (4,(R7)),(9,(R5)),CURTAB,DMCB=CURDMCB                            
         AHI   R4,7                NEXT DEMO NAME                               
         AHI   R5,15               NEXT PRINT POSITION                          
         AHI   R7,8                                                             
         BCT   R6,SALSUM22                                                      
*                                                                               
SALSUM30 DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,6                                                       
         B     SALDPT1                                                          
SALSUMX  CLI   BRSPASS2,0          NO SECOND PASS                               
         BE    SALSUMX1                                                         
         CLI   SALACTIV,0          NO RECORDS FOR THIS PASS                     
         BE    SALSUMX1                                                         
         MVI   ALLOWLIN,10                                                      
         MVC   BRSPASS2,D2APROF+7                                               
         NI    BRSPASS2,X'0F'                                                   
         LLC   R5,BRSPASS2                                                      
         MH    R5,=H'3'                                                         
         LA    R5,SUMOPT(R5)                                                    
         MVC   SUMOPTS,0(R5)                                                    
         CLI   QOPT4,C'Y'                                                       
         BNE   *+8                                                              
         MVI   SUMOPTS+1,0                                                      
         OC    SUMOPTS(3),SUMOPTS  ANYTHING TO PRINT                            
         BZ    SALSUMX1            NO - DON'T SET TITLES                        
         MVI   BRSPASS2,0                                                       
         MVC   BRSDATE,D2APROF+6                                                
         NI    BRSDATE,X'0F'                                                    
         MVI   BUFHI,1             SET FOR READ HI                              
         MVI   BUFRTYP,1                                                        
         L     R5,=A(DICSECT)                                                   
*                                  SET RECAP TITLE                              
         MVI   BRSPCAP,C' '                                                     
         MVC   BRSPCAP+1(L'BRSPCAP-1),BRSPCAP                                   
         MVI   FORCEMID,C'Y'                                                    
         LA    RE,P3                                                            
         CLI   SUMOPTS,0                                                        
         BE    *+14                                                             
         MVC   0(8,RE),=C'TELECAST'                                             
         LA    RE,9(RE)                                                         
         CLI   SUMOPTS+1,0                                                      
         BE    *+14                                                             
         MVC   0(6,RE),=C'DOLLAR'                                               
         LA    RE,7(RE)                                                         
         CLI   SUMOPTS+2,0                                                      
         BE    *+14                                                             
         MVC   0(11,RE),=C'DEMOGRAPHIC'                                         
         LA    RE,12(RE)                                                        
         MVC   0(5,RE),=C'RECAP'                                                
         MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   P,0                                                              
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
         MVI   ALLOWLIN,10                                                      
         B     SALDPT                                                           
*                                                                               
SALSUMX1 MVC   SUMOPTS,SVSUMOPT                                                 
         XIT1                                                                   
*                                                                               
SALSL    DS    0C                                                               
SALDP    DS    0C                                                               
         BRAS  RE,DSTSUM                                                        
         B     SALSUMX1                                                         
SALACTIV DS    C                                                                
         LTORG                                                                  
         EJECT                                                                  
BRSSUM   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         SPACE 2                                                                
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         MVC   SVSUMOPT,SUMOPTS                                                 
         MVC   P,SPACES                                                         
*                                                                               
         MVI   BRSPCAP,C' '                                                     
         MVC   BRSPCAP+1(L'BRSPCAP-1),BRSPCAP                                   
         CLI   MODE,STALAST                                                     
         BNE   *+10                                                             
         MVC   BRSPCAP(L'SP@STATL),SP@STATL   ***STATION TOTAL***               
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
         MVC   BRSPCAP(L'SP@MKTTL),SP@MKTTL   ***MARKET TOTAL***                
         CLI   MODE,PRDLAST                                                     
         BNE   *+14                                                             
         MVC   BRSPCAP(L'SP@PROTL),SP@PROTL   ***PRODUCT TOTAL***               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   D2APROF+6,0                                                      
         BNE   *+8                                                              
         MVI   D2APROF+6,X'70'                                                  
         MVC   BRSPASS2(1),D2APROF+6                                            
         NI    BRSPASS2,X'0F'      EXTRACT R2 DATE BITS                         
         LLC   RE,D2APROF+6        SET THE DATE TYPES                           
         SRL   RE,4                                                             
         STC   RE,BRSDATE                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    BRSMKT                                                           
         MVI   BRSPASS2,0                                                       
         CLI   BUFCDE,X'88'        SPILL DATA                                   
         BE    BRSSUMX                                                          
         CLI   BUFCDE,X'89'        ORIG DATA                                    
         BE    BRSSUMX                                                          
*RECREATE TOTAL MEDTAB                                                          
BRSMKT   MVC   MEDNUMWK,=F'60'                                                  
         MVC   MEDNUMMO,=F'0'                                                   
*                                                                               
         CLI   MRPTTYP,C'6'        SET TO DO MONTHLY                            
         BE    *+8                                                              
         CLI   MRPTTYP,C'5'        SET TO DO MONTHLY                            
         BNE   *+10                                                             
         MVC   MEDNUMMO,=F'13'                                                  
*                                                                               
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVI   BUFHI,1                                                          
         MVI   BUFRTYP,1                                                        
         GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   SUMRTYP,0                                                        
         BE    BRSSUMX                                                          
         CLI   SUMCODE,X'96'                                                    
         BE    BRSSL                                                            
         CLI   SUMCODE,X'97'                                                    
         BE    BRSDP                                                            
*                                                                               
NOHEX    MVI   BUFHI,1                                                          
         MVC   P+56(L'BRSPCAP),BRSPCAP                                          
*                                                                               
BRSDPT   DS    0H                                                               
         CLI   BUFCDE,X'88'        SET SPILL MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***SPILL '                                            
         MVC   P+48(L'SP8SPILL),SP8SPILL                                        
         CLI   BUFCDE,X'89'        SET ORIG. MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***ORIG. '                                            
         MVC   P+48(L'SP8ORIG),SP8ORIG                                          
         DROP  R5                                                               
*                                                                               
         L     R5,MEDAFRST         CLEAR MEDBLOCK                               
         LA    R6,MEDPERD                                                       
*                                                                               
CLRBRS   L     RE,4(R5)                                                         
         LTR   RE,RE                                                            
         BZ    CLRBRS2                                                          
         L     RF,MEDLCHNK                                                      
         XCEF                                                                   
*                                                                               
CLRBRS2  LA    R5,12(R5)                                                        
         OC    4(4,R5),4(R5)                                                    
         BNZ   *+10                                                             
         XC    0(12,R5),0(R5)                                                   
         CR    R5,R6                                                            
         BNH   CLRBRS                                                           
*                                                                               
BRSSUM1  GOTO1 VGETBUF                                                          
*                                                                               
         LA    R3,MYBUFIO                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    BRSSUM1A                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    BRSSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    BRSSUM1                                                          
*                                                                               
BRSSUM1A CLI   SUMDPART,C''       SPECIAL DETAIL SUPPRESS                      
         BE    BRSSUM1                                                          
         CLI   SUMDPGRP,C''                                                    
         BE    BRSSUM1                                                          
*                                                                               
         L     R5,MEDAFRST                                                      
         CLI   SUMRTYP,0                                                        
         BE    BRSSUMX                                                          
*                                                                               
         CLI   SUMRTYP,1           WEEKLY                                       
         BNE   *+12                                                             
         TM    BRSDATE,X'01'                                                    
         BZ    BRSSUM1                                                          
*                                                                               
         CLI   SUMRTYP,2           MONTHLY                                      
         BNE   *+12                                                             
         TM    BRSDATE,X'02'                                                    
         BZ    BRSSUM1                                                          
*                                                                               
         CLI   SUMRTYP,3           PERIOD                                       
         BNE   *+12                                                             
         TM    BRSDATE,X'04'                                                    
         BZ    BRSSUM1                                                          
*                                                                               
         CLI   MRPTTYP,C'5'        MONTHLY                                      
         BE    *+8                                                              
         CLI   MRPTTYP,C'6'        MONTHLY                                      
         BE    *+12                                                             
         CLI   SUMRTYP,2           BYPASS MONTHLY                               
         BE    BRSSUM1                                                          
*                                                                               
         CLI   SUMRTYP,2                                                        
         BNE   *+8                                                              
         LA    R5,MEDMON01                                                      
*                                                                               
         MVI   ANYPRINT,0                                                       
         CLI   SUMRTYP,3           SET FOR PERIOD                               
         BNE   BRSSUM2                                                          
         LA    R5,MEDPERD                                                       
         B     BRSSUM3                                                          
*                                                                               
BRSSUM2  CLC   2(2,R5),SUMDT+2     FIND CORRECT BUFFER SLOT                     
         BE    BRSSUM3                                                          
         CLC   0(2,R5),SUMDT                                                    
         BE    BRSSUM3                                                          
         LA    R5,12(R5)                                                        
         LA    RE,MEDPERD                                                       
         CR    R5,RE                                                            
         BNL   BRSSUM1                                                          
         B     BRSSUM2                                                          
*                                                                               
BRSSUM3  L     R4,4(R5)            INSERT DATA INTO MEDBLOCK                    
         MVC   MEDGLD,SUMGDL                                                    
         MVC   MEDGL1,SUMGD1                                                    
         MVC   MEDBYSPT,SUMSPOTS                                                
         MVC   MEDBY1,SUMD1                                                     
         MVC   MEDBYD,SUMDL                                                     
         CLI   SUMRTYP,3           PERIOD RECORD                                
         BE    *+8                                                              
         B     BRSSUM1                                                          
*                                                                               
         L     R5,MEDAFRST         PRINT BRS SUMMARY                            
         CLI   MRPTTYP,C'5'                                                     
         BNE   *+12                                                             
         LA    R5,MEDMON01                                                      
         B     BRSSUM4                                                          
*                                                                               
BRSSUM4  L     R4,4(R5)                                                         
         LTR   R4,R4                                                            
         BZ    BRSSUM8A                                                         
         LA    R8,15                                                            
         LA    R9,P2+11                                                         
         L     RF,=A(DICSECT)                                                   
         USING DICSECT,RF                                                       
*--->    MVC   P2(7),=C'MKT TOT'                                                
         MVC   P2(L'SP7MKTTL),SP7MKTTL                                          
*                                                                               
BRSSUM41 DS    0H                                                               
*---->   MVC   P2(7),=C'*TOTAL*'                                                
         MVC   P2(L'SP7TOTAL),SP7TOTAL                                          
         MVI   ALLOWLIN,10                                                      
         CLI   SUMDPGRP,X'FF'                                                   
         BE    BRSSUM4A                                                         
         MVC   P2(3),SUMDPART                                                   
         EDIT  (1,SUMSLN),(3,P2+4)                                              
         L     RF,=A(DICSECT)                                                   
         MVI   P2+3,C'-'                                                        
*                                                                               
BRSSUM4A MVI   P3,0                                                             
         LA    R6,P4                                                            
         CLI   SUMOPTS,0                                                        
         BE    BRSSUM4B                                                         
*---->   MVC   0(9,R6),=C'NO TLCSTS'                                            
         MVC   0(L'SP@NOTLC,R6),SP@NOTLC                                        
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
*---->   MVC   0(9,R6),=C'NO BRDCSTS'                                           
         MVC   0(L'SP@NOBRD,R6),SP@NOBRD                                        
         LA    R6,132(R6)                                                       
*                                                                               
BRSSUM4B CLI   SUMOPTS+2,0                                                      
         BE    *+14                                                             
         MVC   0(7,R6),DNAME1                                                   
         LA    R6,132(R6)                                                       
         CLI   SUMOPTS+1,0                                                      
         BE    BRSSUM4D                                                         
*---->   MVC   0(7,R6),=C'DOLLARS'                                              
         MVC   0(L'SP7DOLLA,R6),SP7DOLLA                                        
*                                                                               
         CLI   PWPRESW,C'Y'                                                     
         BNE   BRSSUM4C                                                         
         MVC   0(10,R6),=C'GROSS COST'                                          
         CLI   Q2NET,C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(10,R6),=C'  NET COST'                                          
         CLI   Q2NET,C'B'                                                       
         BNE   *+10                                                             
         MVC   0(10,R6),=C' *NET COST'                                          
*                                                                               
BRSSUM4C LA    R6,132(R6)                                                       
*                                                                               
BRSSUM4D CLI   Q2NET,C'Y'                                                       
         BE    BRSSUM5                                                          
         CLI   MODE,MKTLAST        NO GOALS AT STATION LEVEL                    
         BL    BRSSUM5                                                          
         CLI   SUMOPTS+2,0                                                      
         BE    *+14                                                             
*---->   MVC   0(9,R6),=C'GOAL DEMO'                                            
         MVC   0(L'SP@GOADE,R6),SP@GOADE                                        
         LA    R6,132(R6)                                                       
         CLI   SUMOPTS+1,0                                                      
         BE    *+10                                                             
*---->   MVC   0(6,R6),=C'GOAL $'                                               
         MVC   0(L'SP@GOALM,R6),SP@GOALM                                        
*                                                                               
BRSSUM5  LA    R6,MEDPERD                                                       
         L     RF,=A(DICSECT)                                                   
         OC    0(4,R5),0(R5)       SLOT ACTIVE                                  
         BZ    BRSSUM8A             NO - BYPASS                                 
         CR    R5,R6                                                            
         BL    BRSSUM6                                                          
         BH    BRSSUM9                                                          
*---->   MVC   WORK(5),=C'TOTAL'                                                
         MVC   WORK(L'SP5TOTAL),SP5TOTAL                                        
         B     BRSSUM7                                                          
         DROP  RF                                                               
*                                                                               
BRSSUM6  LA    R6,MEDMON01                                                      
         CR    R5,R6                                                            
         BL    BRSSUM6A                                                         
         GOTO1 DATCON,DMCB,(X'02',2(R5)),(X'06',WORK)  MONTH DATE               
         MVC   WORK+3(1),WORK+4                                                 
         MVC   WORK+4(1),WORK+5                                                 
         MVI   WORK+5,C' '                                                      
         B     BRSSUM7                                                          
*                                                                               
BRSSUM6A GOTO1 DATCON,DMCB,(X'02',(R5)),(X'04',WORK)  WEEK DATE                 
*                                                                               
BRSSUM7  DS    0H                                                               
         OC    MEDGLD,MEDGLD                                                    
         BNZ   *+14                                                             
         OC    MEDBYD(12),MEDBYD                                                
         BZ    BRSSUM8A                                                         
         MVI   ANYPRINT,1                                                       
         MVC   3(5,R9),WORK                                                     
         ST    R9,PRTADDR                                                       
         LA    R9,132(R9)                                                       
         LA    R9,132(R9)                                                       
         CLI   SUMOPTS,0                                                        
         BE    BRSSUM7A                                                         
         EDIT  (4,MEDBYSPT),(5,3(R9))                                           
         LA    R9,132(R9)                                                       
*                                                                               
BRSSUM7A CLI   SUMOPTS+2,0                                                      
         BE    BRSSUM7D                                                         
*                                                                               
         MVI   DEMINDX,0                                                        
         GOTO1 =A(SETPTDEM)        POINT RF TO DEMO LIST                        
*                                                                               
         LA    R0,10                                                            
         CLI   RATING,C'Y'                                                      
         BNE   BRSSUM7B                                                         
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    BRSSUM7C                                                         
         B     *+12                                                             
*                                                                               
BRSSUM7B TM    RQOPTS,RQOPTS_2DECIMP  TEST 2-DEC IMPS                           
         BZ    BRSSUM7C                                                         
         LA    R0,100              SET FOR 2-DEC                                
*                                                                               
BRSSUM7C L     RF,MEDBY1                                                        
         M     RE,=F'2'                                                         
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
*                                                                               
BRSSUM7D CLI   SUMOPTS+1,0                                                      
         BE    BRSSUM7E                                                         
         L     RF,MEDBYD                                                        
         CVDX  DUB,MEDBYD                                                       
*                                  DO PACKED DIVIDE                             
         ZAP   WORK(16),DUB                                                     
         AP    WORK(16),=PL2'50'                                                
         DP    WORK(16),=PL8'100'                                               
         ZAP   DUB,WORK(8)                                                      
         CVB   RF,DUB                                                           
         MVI   CURTAB+3,0                                                       
         CURED (RF),(8,(R9)),CURTAB,DMCB=CURDMCB,FLOAT=-                        
         LA    R9,132(R9)                                                       
*                                                                               
BRSSUM7E CLI   MODE,MKTLAST        NO GOALS AT STATION LEVEL                    
         BL    BRSSUM8                                                          
         CLI   SUMOPTS+2,0                                                      
         BE    BRSSUM7H                                                         
*                                                                               
         GOTO1 =A(SETPTDEM)        POINT TO DEMO LIST                           
*                                                                               
         LA    R0,10               SET DIVISOR                                  
         CLI   RATING,C'Y'                                                      
         BNE   BRSSUM7F                                                         
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    BRSSUM7G                                                         
         B     *+12                                                             
*                                                                               
BRSSUM7F TM    RQOPTS,RQOPTS_2DECIMP                                            
         BZ    BRSSUM7G                                                         
         LA    R0,100                                                           
*                                                                               
BRSSUM7G L     RF,MEDGL1                                                        
         M     RE,=F'2'                                                         
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
*                                                                               
BRSSUM7H CLI   SUMOPTS+1,0                                                      
         BE    BRSSUM8                                                          
         L     RF,MEDGLD                                                        
         M     RE,=F'2'                                                         
         D     RE,=F'100'                                                       
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
*                                                                               
BRSSUM8  L     R9,PRTADDR          SET NEXT SLOT                                
         LA    R9,8(R9)                                                         
*                                                                               
BRSSUM8A LA    R5,12(R5)                                                        
         L     R4,4(R5)                                                         
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BH    BRSSUM9                                                          
         OC    0(4,R5),0(R5)                                                    
         BZ    BRSSUM8A                                                         
         LA    R6,MEDMON01                                                      
         CLI   MRPTTYP,C'5'                                                     
         BE    *+8                                                              
         CLI   MRPTTYP,C'6'                                                     
         BNE   *+8                                                              
         LA    R6,MEDQRT01                                                      
         BCTR  R6,0                                                             
         CR    R5,R6                                                            
         BH    *+12                                                             
         BCT   R8,BRSSUM5                                                       
         B     BRSSUM9                                                          
*                                                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         BCT   R8,BRSSUM5                                                       
*                                                                               
BRSSUM9  CLI   ANYPRINT,0                                                       
         BE    BRSSUM10                                                         
         GOTO1 REPORT                                                           
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
*                                                                               
BRSSUM10 MVI   ALLOWLIN,0                                                       
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BNH   BRSSUM4                                                          
         B     BRSDPT                                                           
*                                                                               
BRSSUMX  CLI   BRSPASS2,0                                                       
         BE    BRSSUMX1                                                         
         MVI   BRSPCAP,C' '                                                     
         MVC   BRSPCAP+1(L'BRSPCAP-1),BRSPCAP                                   
         MVC   BRSPASS2,D2APROF+7                                               
         NI    BRSPASS2,X'0F'                                                   
         LLC   R5,BRSPASS2                                                      
         MH    R5,=H'3'                                                         
         LA    R5,SUMOPT(R5)                                                    
         MVC   SUMOPTS,0(R5)                                                    
         CLI   QOPT4,C'Y'                                                       
         BNE   *+8                                                              
         MVI   SUMOPTS+1,0                                                      
         OC    SUMOPTS(3),SUMOPTS                                               
         BZ    BRSSUMX1                                                         
         MVI   BRSPASS2,0                                                       
         MVC   BRSDATE,D2APROF+6                                                
         NI    BRSDATE,X'0F'                                                    
*                                  SET RECAP TITLE                              
         LA    RE,P                                                             
         CLI   SUMOPTS,0                                                        
         BE    *+14                                                             
         MVC   0(8,RE),=C'TELECAST'                                             
         LA    RE,9(RE)                                                         
         CLI   SUMOPTS+1,0                                                      
         BE    *+14                                                             
         MVC   0(6,RE),=C'DOLLAR'                                               
         LA    RE,7(RE)                                                         
         CLI   SUMOPTS+2,0                                                      
         BE    *+14                                                             
         MVC   0(11,RE),=C'DEMOGRAPHIC'                                         
         LA    RE,12(RE)                                                        
         MVC   0(5,RE),=C'RECAP'                                                
*                                                                               
         L     R5,=A(DICSECT)                                                   
         B     BRSMKT                                                           
*                                                                               
BRSSUMX1 MVC   SUMOPTS,SVSUMOPT                                                 
*                                                                               
BRSSUMX2 XIT1                                                                   
*                                                                               
BRSSL    DS    0C                                                               
BRSDP    DS    0C                                                               
         BRAS  RE,DSTSUM                                                        
         B     BRSSUMX2                                                         
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* DISTRIBUTION SUMMARIES                                                        
*============================================================                   
                                                                                
DSTSUM   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*                                                                               
         XC    DSTCNT,DSTCNT                                                    
         XC    SUMVPCT(15*4),SUMVPCT                                            
         MVI   BRSDACT,0                                                        
         MVC   P,SPACES                                                         
         MVC   BRSDATE,D2APROF+3   SELECT DATE BITS                             
*                                                                               
         MVC   BDSCTLSW(1),D2APROF+5                                            
         CLC   RTYPE,=C'BDS'                                                    
         BNE   *+8                                                              
         NI    BRSDATE,X'55'                                                    
*                                                                               
         LLC   RE,BRSDATE                                                       
         SRL   RE,4                                                             
         NI    BRSDATE,X'0F'                                                    
         CLI   BUFCDE,X'97'                                                     
         BE    *+14                                                             
         STC   RE,BRSDATE                                                       
         MVC   BDSCTLSW(1),D2APROF+4                                            
*                                                                               
         MVI   IDXRPT,C'N'         SET UP SWITCHES                              
         MVI   RAWGRP,C'N'                                                      
         MVI   HORIDX,C'N'                                                      
         TM    BDSCTLSW,X'01'                                                   
         BZ    *+8                                                              
         MVI   IDXRPT,C'Y'                                                      
         TM    BDSCTLSW,X'02'                                                   
         BZ    *+8                                                              
         MVI   RAWGRP,C'Y'                                                      
         TM    BDSCTLSW,X'04'                                                   
         BZ    *+8                                                              
         MVI   HORIDX,C'Y'                                                      
*                                                                               
         MVC   MEDNUMWK,=F'60'                                                  
*                                                                               
         MVC   MEDNUMMO,=F'13'                                                  
*                                                                               
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
*                                                                               
         MVI   BUFHI,1                                                          
         GOTO1 VGETBUF                                                          
*                                                                               
         LA    R3,MYBUFIO                                                       
         CLI   SUMRTYP,0           END OF BUFFER                                
         BE    DSTSUMX                                                          
         MVI   P,0                                                              
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
         MVI   BUFHI,1                                                          
*                                                                               
         MVI   DPPASS,0                                                         
         MVI   DPPASST,C'N'                                                     
*                                                                               
         L     RE,=A(SUMBUF)                                                    
         LHI   RF,SUMBUFX-SUMBUF                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   P(132),SPACES                                                    
         LA    R5,P                                                             
         MVC   P(7),=C'WEEKLY '                                                 
         CLC   QPROG,=C'D6'                                                     
         BNE   *+10                                                             
         MVC   P(7),=C'DAILY  '                                                 
         LA    R5,7(R5)                                                         
         TM    BRSDATE,X'01'                                                    
         BO    BRDDPH1                                                          
         MVC   P(7),=C'MONTHLY '                                                
         LA    R5,1(R5)                                                         
         TM    BRSDATE,X'02'                                                    
         BO    BRDDPH1                                                          
         MVC   P(7),=C'PERIOD '                                                 
         BCTR  R5,0                                                             
*                                                                               
BRDDPH1  CLI   RAWGRP,C'Y'                                                      
         BNE   *+14                                                             
         MVC   0(6,R5),=C'GRP''S '                                              
         LA    R5,6(R5)                                                         
         CLI   RAWGRP,C'Y'                                                      
         BNE   *+8                                                              
         CLI   IDXRPT,C'Y'                                                      
         BNE   *+14                                                             
         MVC   0(4,R5),=C'AND '                                                 
         LA    R5,4(R5)                                                         
         CLI   IDXRPT,C'Y'                                                      
         BNE   *+14                                                             
         MVC   0(2,R5),=C'% '                                                   
         LA    R5,2(R5)                                                         
         MVC   0(13,R5),=C'DISTRIBUTION '                                       
         LA    R5,13(R5)                                                        
         MVC   0(L'DNAME1,R5),DNAME1                                            
*                                                                               
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         CLI   MODE,STALAST                                                     
         BNE   *+10                                                             
*---->   MVC   P+56(19),=C'***STATION TOTAL***'                                 
         MVC   P+56(L'SP@STATL),SP@STATL                                        
*                                                                               
         CLI   MODE,MKTLAST                                                     
         B     *+10                                                             
*---->   MVC   P+56(18),=C'***MARKET TOTAL***'                                  
         MVC   P+56(L'SP@MKTTL),SP@MKTTL                                        
*                                                                               
         CLI   MODE,PRDLAST                                                     
         BNE   BRSDDPT                                                          
         MVI   FORCEHED,C'Y'                                                    
*---->   MVC   P+56(19),=C'***PRODUCT TOTAL***'                                 
         MVC   P+56(L'SP@PROTL),SP@PROTL                                        
*                                                                               
BRSDDPT  DS    0H                                                               
         CLI   BUFCDE,X'88'        SET SPILL MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***SPILL '                                            
         MVC   P+48(L'SP8SPILL),SP8SPILL                                        
*                                                                               
         CLI   BUFCDE,X'89'        SET ORIG. MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***ORIG. '                                            
         MVC   P+48(L'SP8ORIG),SP8ORIG                                          
         DROP  R5                                                               
*                                                                               
         L     R5,MEDAFRST         CLEAR MEDBLOCK                               
         LA    R6,MEDPERD                                                       
*                                                                               
CLRBRSD  ICM   RE,15,4(R5)                                                      
         BZ    CLRBRSD2                                                         
         L     RF,MEDLCHNK                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
CLRBRSD2 LA    R5,12(R5)                                                        
         OC    4(4,R5),4(R5)                                                    
         BNZ   *+10                                                             
         XC    0(12,R5),0(R5)                                                   
         CR    R5,R6                                                            
         BNH   CLRBRSD                                                          
*                                                                               
BRSDUM1  GOTO1 VGETBUF                                                          
*                                                                               
         LA    R3,MYBUFIO                                                       
         CLI   SUMRTYP,0           END OF BUFFER                                
         BE    BRSDX                                                            
         CLI   SUMSLN,1                                                         
         BE    BRSDUM1                                                          
         OC    SUMSPOTS,SUMSPOTS                                                
         BZ    BRSDUM1                                                          
*                                                                               
         CLI   SUMRTYP,1           WEEK FILTER                                  
         BNE   *+12                                                             
         TM    BRSDATE,X'01'                                                    
         BZ    BRSDUM1                                                          
*                                                                               
         CLI   SUMRTYP,2           MONTH FILTER                                 
         BNE   *+12                                                             
         TM    BRSDATE,X'02'                                                    
         BZ    BRSDUM1                                                          
*                                                                               
         CLI   SUMSLN,X'00'        TOTAL DATA  - BUFFER FOR INDEXING            
         BNE   BRSDSVX                                                          
         L     RF,=A(SUMBUF)                                                    
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         LA    RF,75(RF)                                                        
         B     *-12                                                             
*                                                                               
         MVC   0(75,RF),SUMKEY                                                  
         DROP  R3                                                               
*                                                                               
         USING SUMDSECT,RF                                                      
         CLC   SUMDT,=X'FFFFFFFF'                                               
         BNE   BRSDUM1                                                          
         LA    RE,MEDPERD                                                       
         MVC   SUMDT,0(RE)                                                      
         DROP  RF                                                               
         B     BRSDUM1                   (NOT SUPPORTED YET)                    
*                                                                               
         USING SUMDSECT,R3                                                      
BRSDSVX  CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    BRSDUM1A                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    BRSDUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    BRSDUM1                                                          
*                                                                               
BRSDUM1A CLI   SUMDPART,C''       SPECIAL DETAIL SUPPRESS                      
         BE    BRSDUM1                                                          
         CLI   SUMDPGRP,C''                                                    
         BE    BRSDUM1                                                          
*                                                                               
         L     R5,MEDAFRST                                                      
*                                                                               
         CLI   SUMRTYP,2                                                        
         BNE   *+8                                                              
         LA    R5,MEDMON01                                                      
*                                                                               
         CLI   SUMRTYP,3           SET FOR PERIOD                               
         BNE   BRSDUM2                                                          
         LA    R5,MEDPERD                                                       
         B     BRSDUM3                                                          
*                                                                               
BRSDUM2  CLC   2(2,R5),SUMDT+2     FIND CORRECT BUFFER SLOT                     
         BE    BRSDUM3                                                          
         CLC   0(2,R5),SUMDT                                                    
         BE    BRSDUM3                                                          
         LA    R5,12(R5)                                                        
         LA    RE,MEDPERD                                                       
         CR    R5,RE                                                            
         BNL   BRSDUM1                                                          
         B     BRSDUM2                                                          
*                                                                               
BRSDUM3  L     R4,4(R5)            INSERT DATA INTO MEDBLOCK                    
         MVC   MEDGLD,SUMGDL                                                    
         MVC   MEDGL1,SUMGD1                                                    
         MVC   MEDBYSPT,SUMSPOTS                                                
         MVC   MEDBY1,SUMD1                                                     
         MVC   MEDBYD,SUMDL                                                     
         CLI   SUMRTYP,3           PERIOD RECORD                                
         BNE   BRSDUM1                                                          
                                                                                
* PRINT BRS SUMMARY                                                             
                                                                                
BRSDUM3A LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    MEDBYSPT,MEDBYSPT   NO SPOTS - NEXT DPT                          
         BZ    BRSDUM9                                                          
*                                                                               
         L     R5,MEDAFRST                                                      
         CLI   SUMRTYP,1                                                        
         BE    *+8                                                              
         LA    R5,MEDMON01                                                      
*                                                                               
BRSDUM4  ICM   R4,15,4(R5)                                                      
         BZ    BRSDUM8A                                                         
         LA    R8,18                                                            
         LA    R9,SUMVPCT                                                       
         ST    R9,SUMVPAD                                                       
         CLI   DPPASS,0                                                         
         BE    *+12                                                             
         LA    R9,P+11                                                          
         B     BRSDUM5                                                          
*                                                                               
         LA    R9,P2+11                                                         
BRSDUM41 DS    0H                                                               
         MVI   ALLOWLIN,10                                                      
         MVI   P3,0                                                             
         XC    HALF2,HALF2                                                      
         MVC   P2(7),=C'DAYPART'                                                
         CLI   BUFCDE,X'96'                                                     
         BNE   *+10                                                             
         MVC   P2(7),=C'LENGTH '                                                
         LA    R9,P2+11                                                         
         ST    R5,PRTADDR                                                       
*                                                                               
BRSDUM6  L     RF,=A(SUMBUF)                                                    
         DROP  R3                                                               
         USING SUMDSECT,RF                                                      
         MVI   BYTE,0                                                           
*                                                                               
BRSDCH1  CLI   0(RF),0                                                          
         BE    BRSDUM6C                                                         
         CLC   0(4,R5),SUMDT                                                    
         BE    *+12                                                             
         LA    RF,75(RF)                                                        
         B     BRSDCH1                                                          
         DROP  RF                                                               
         USING SUMDSECT,R3                                                      
*                                                                               
         MVI   BYTE,1                                                           
         CLC   HALF2,=H'17'                                                     
         BNE   *+8                                                              
         LA    R9,P3+11                                                         
         LA    R6,MEDMON01                                                      
         CR    R5,R6                                                            
         BL    BRSDUM6A                                                         
         MVC   WORK,SPACES                                                      
         TM    BRSDATE,X'02'     PERIOD ACTIVE                                  
         BZ    BRSDUM6B                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(X'02',2(R5)),(X'06',WORK)  MONTH DATE               
         MVC   WORK+3(1),WORK+4                                                 
         MVC   WORK+4(1),WORK+5                                                 
         MVI   WORK+5,C' '                                                      
         B     BRSDUM6B                                                         
*                                                                               
BRSDUM6A GOTO1 DATCON,DMCB,(X'02',(R5)),(X'04',WORK)  WEEK DATE                 
*                                                                               
BRSDUM6B MVC   3(5,R9),WORK                                                     
         LA    RE,MEDPERD                                                       
         CR    R5,RE                                                            
         BNE   BRSDUM6C                                                         
         MVC   3(5,R9),=C'     '   CLEAR OUT CAPTION                            
         TM    BRSDATE,X'04'                                                    
         BZ    *+10                                                             
         MVC   3(5,R9),=C'TOTAL'                                                
*                                                                               
BRSDUM6C LA    R5,12(R5)                                                        
         LA    RE,MEDPERD                                                       
         CR    R5,RE                                                            
         BH    BRSDUM6D                                                         
         OC    0(4,R5),0(R5)                                                    
         BZ    BRSDUM6C                                                         
         CLI   BYTE,0                                                           
         BE    BRSDUM6                                                          
         LH    R6,HALF2                                                         
         LA    R6,1(R6)                                                         
         STH   R6,HALF2                                                         
         LA    R9,7(R9)                                                         
         B     BRSDUM6                                                          
*                                                                               
BRSDUM6D GOTO1 REPORT                                                           
*                                                                               
         MVI   DPPASS,1                                                         
         LA    RE,SUMVPCT                                                       
         ST    RE,SUMVPAD                                                       
         LA    R9,P+11                                                          
         L     R5,PRTADDR                                                       
         MVI   BRSDACT,0                                                        
*                                                                               
BRSDUM5  LA    R6,MEDPERD                                                       
         OC    0(4,R5),0(R5)       SLOT ACTIVE                                  
         BZ    BRSDUM8A             NO - BYPASS                                 
         CR    R5,R6                                                            
         BE    *+12                                                             
         BNH   BRSDUM7                                                          
         B     BRSDUM9                                                          
*                                                                               
         TM    BRSDATE,X'04'     PERIOD ACTIVE                                  
         BZ    BRSDUM8A                                                         
*                                                                               
BRSDUM7  DS    0H                                                               
         MVC   P+2(3),SUMDPART                                                  
         CLI   BUFCDE,X'96'                                                     
         BNE   BRSDUM7A                                                         
         CLI   DPPASST,C'Y'        PRINT "TOT" AS LEN                           
         BE    BRSDUM7A                                                         
         EDIT  SUMSLN,(3,P+2)                                                   
         DROP  R3                                                               
*                                                                               
BRSDUM7A ST    R9,PRTADDR                                                       
*                                                                               
         L     RF,=A(SUMBUF)                                                    
         USING SUMDSECT,RF                                                      
*                                                                               
BRSDIX1  CLI   0(RF),0                                                          
         BE    BRSDIX1A                                                         
         CLC   0(4,R5),SUMDT                                                    
         BE    BRSDIX1X                                                         
         LA    RF,75(RF)                                                        
         B     BRSDIX1                                                          
*                                                                               
BRSDIX1A LA    R5,12(R5)           BUMP MEDDATE SLOT                            
         L     R4,4(R5)                                                         
         B     BRSDUM5                                                          
*                                                                               
BRSDIX1X CLI   IDXRPT,C'Y'                                                      
         BNE   BRSDIX2X                                                         
         CLI   DPPASST,C'Y'        ALREADY SEEDED IF TOT PASS                   
         BE    BRSDIX2X                                                         
         MVC   FULL,SUMD1                                                       
         OC    FULL,FULL                                                        
         BNZ   *+10                                                             
         MVC   FULL,=F'1'                                                       
         DROP  RF                                                               
*                                                                               
         USING SUMDSECT,R3                                                      
BRSDIX2X OC    MEDGLD,MEDGLD                                                    
         BNZ   *+14                                                             
         OC    MEDBYSPT,MEDBYSPT                                                
         BZ    BRSDUM8                                                          
         CLI   RAWGRP,C'Y'                                                      
         BNE   BRSDRGX                                                          
*                                                                               
         MVI   DEMINDX,0                                                        
         GOTO1 =A(SETPTDEM)        POINT RF TO DEMO LIST                        
*                                                                               
         LA    R0,10                                                            
         CLI   RATING,C'Y'                                                      
         BNE   BRSDIX3                                                          
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    BRSDIX4                                                          
         B     *+12                                                             
*                                                                               
BRSDIX3  TM    RQOPTS,RQOPTS_2DECIMP                                            
         BZ    BRSDIX4                                                          
         LA    R0,100              SET FOR 2 DECIMALS                           
*                                                                               
BRSDIX4  L     RF,MEDBY1                                                        
         M     RE,=F'2'            PRINT THE RAW DEMOS                          
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         EDIT  (RF),(6,(R9))                                                    
         MVI   BRSDACT,1                                                        
*                                                                               
BRSDRGX  CLI   HORIDX,C'Y'         OVERRIDE THE FACT THAT WIM SUCKS             
         BE    *+12                                                             
         CLI   DPPASST,C'Y'        DON'T PRINT THIS GREAT REPORT                
         BE    BRSDVP              DAMN - WIM SUCKS                             
*                                                                               
         ICM   RF,15,MEDBY1        PRINT THE INDEXES                            
         BZ    BRSDUM8                                                          
         CLI   IDXRPT,C'Y'                                                      
         BNE   BRSDUM8                                                          
         M     RE,=F'1000'                                                      
         D     RE,FULL                                                          
         CLI   RAWGRP,C'Y'         PRINTING GRPS                                
         BNE   *+8                                                              
         LA    R9,132(R9)          YES - BUMP THE PRINT LINE                    
         MVI   6(R9),C'%'                                                       
*                                                                               
         M     RE,=F'2'                                                         
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         EDIT  (RF),(6,(R9))                                                    
*                                                                               
         L     R1,SUMVPAD                                                       
         L     RE,0(R1)                                                         
         AR    RE,RF                                                            
         ST    RE,0(R1)                                                         
         MVI   BRSDACT,1                                                        
         B     BRSDUM8                                                          
*                                                                               
BRSDVP   L     R1,SUMVPAD                                                       
         CLI   RAWGRP,C'Y'                                                      
         BNE   BRSDUM8                                                          
         CLI   IDXRPT,C'Y'                                                      
         BNE   BRSDUM8                                                          
         LA    R9,132(R9)                                                       
         L     RF,0(R1)                                                         
         MVI   6(R9),C'%'                                                       
         EDIT  (RF),(6,(R9))                                                    
         MVI   BRSDACT,1                                                        
*                                                                               
BRSDUM8  L     R9,SUMVPAD          SET NEXT SLOT                                
         LA    R9,4(R9)                                                         
         ST    R9,SUMVPAD                                                       
         L     R9,PRTADDR          SET NEXT SLOT                                
         LA    R9,7(R9)                                                         
*                                                                               
BRSDUM8A LA    R5,12(R5)                                                        
         L     R4,4(R5)                                                         
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BH    BRSDUM9                                                          
         OC    0(4,R5),0(R5)                                                    
         BZ    BRSDUM8A                                                         
*                                                                               
         LA    R6,MEDQRT01                                                      
         BCTR  R6,0                                                             
         CR    R5,R6                                                            
         BH    *+12                                                             
         BCT   R8,BRSDUM5                                                       
         B     BRSDUM9                                                          
*                                                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         BCT   R8,BRSDUM5                                                       
*                                                                               
BRSDUM9  CLI   BRSDACT,1                                                        
         BNE   BRSDUM91                                                         
         LH    RE,DSTCNT                                                        
         LA    RE,1(RE)                                                         
         STH   RE,DSTCNT                                                        
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
*                                                                               
BRSDUM91 MVI   ALLOWLIN,0                                                       
         MVI   BRSDACT,0                                                        
         MVC   P1,SPACES                                                        
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BNH   BRSDUM4                                                          
         B     BRSDDPT                                                          
*                                                                               
BRSDX    CLI   DPPASST,C'Y'                                                     
         BE    DSTSUMX                                                          
         CLC   DSTCNT,=H'1'                                                     
         BE    DSTSUMX                                                          
         MVI   DPPASST,C'Y'                                                     
         MVC   SUMDPART(3),=C'TOT'                                              
         DROP  R3                                                               
*                                                                               
         L     RF,=A(SUMBUF)                                                    
         USING SUMDSECT,RF                                                      
*                                                                               
BRSDTTL  CLI   0(RF),0                                                          
         BE    BRSDUM3A                                                         
         L     R5,MEDAFRST                                                      
         CLC   0(4,R5),SUMDT                                                    
         BE    *+12                                                             
         LA    R5,12(R5)                                                        
         B     *-14                                                             
         L     R4,4(R5)                                                         
         MVC   MEDGLD,SUMGDL                                                    
         MVC   MEDGL1,SUMGD1                                                    
         MVC   MEDBYSPT,SUMSPOTS                                                
         MVC   MEDBY1,SUMD1                                                     
         MVC   MEDBYD,SUMDL                                                     
         MVC   FULL,SUMD1                                                       
         LA    RF,75(RF)                                                        
         B     BRSDTTL                                                          
         DROP  RF                                                               
*                                                                               
DSTSUMX  MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
DPPASS   DS    C                                                                
DPPASST  DS    C                                                                
BRSDACT  DS    C                                                                
DSTCNT   DS    H                                                                
SUMVPAD  DS    F                                                                
SUMVPCT  DS    16F                                                              
         EJECT                                                                  
*=================================================================              
* BTS SUMMARY                                                                   
*=================================================================              
                                                                                
BTSSUM   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         MVC   SVSUMOPT,SUMOPTS                                                 
         MVC   BRSPASS2(1),D2APROF+7                                            
         MVI   BTSACTIV,0                                                       
         MVC   P,SPACES                                                         
*                                                                               
         MVI   BRSPCAP,C' '                                                     
         MVC   BRSPCAP+1(L'BRSPCAP-1),BRSPCAP                                   
         CLI   MODE,STALAST                                                     
         BNE   *+10                                                             
         MVC   BRSPCAP(L'SP@STATL),SP@STATL   ***STATION TOTAL***               
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
         MVC   BRSPCAP(L'SP@MKTTL),SP@MKTTL   ***MARKET TOTAL***                
         CLI   MODE,PRDLAST                                                     
         BNE   *+14                                                             
         MVC   BRSPCAP(L'SP@PROTL),SP@PROTL   ***PRODUCT TOTAL***               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   D2APROF+6,0                                                      
         BNE   *+8                                                              
         MVI   D2APROF+6,X'70'                                                  
         MVC   BRSPASS2(1),D2APROF+6                                            
         NI    BRSPASS2,X'0F'      EXTRACT R2 DATE BITS                         
         LLC   RE,D2APROF+6        SET THE DATE TYPES                           
         SRL   RE,4                                                             
         STC   RE,BRSDATE                                                       
*                                                                               
         CLI   BUFCDE,X'96'                                                     
         BE    BTSSL                                                            
         CLI   BUFCDE,X'97'                                                     
         BE    BTSDP                                                            
         OC    SUMOPTS,SUMOPTS                                                  
         BZ    BTSSUMX                                                          
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    BTSMKT                                                           
         MVI   BRSPASS2,0                                                       
         CLI   BUFCDE,X'88'        SPILL DATA                                   
         BE    BTSSUMX                                                          
         CLI   BUFCDE,X'89'        ORIG DATA                                    
         BE    BTSSUMX                                                          
BTSMKT   MVI   BUFHI,1                                                          
         MVI   BUFRTYP,2                                                        
         GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   SUMRTYP,0                                                        
         BE    BTSSUMX                                                          
         MVI   BTSACTIV,1                                                       
         MVI   BUFHI,1             SET FOR READ HI                              
         MVI   BUFRTYP,2                                                        
         MVC   P+56(L'BRSPCAP),BRSPCAP                                          
*                                                                               
BTSDPT   DS    0H                                                               
         CLI   SUMSLN,X'FD'        SET SPILL MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***SPILL '                                            
         MVC   P+48(L'SP8SPILL),SP8SPILL                                        
         CLI   SUMSLN,X'FE'        SET ORIG. MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***ORIG. '                                            
         MVC   P+48(L'SP8ORIG),SP8ORIG                                          
         MVI   DPTSW,1                                                          
         MVI   ALLOWLIN,17                                                      
         L     RE,ACTMO                                                         
         LA    RE,5(RE)                                                         
         STC   RE,ALLOWLIN                                                      
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,0                                                       
         SPACE 2                                                                
         MVI   FORCEMID,C'Y'                                                    
         CLI   SUMOPTS,0                                                        
         BE    BTSSUMA                                                          
*---->   MVC   MID1+20(9),=C'NO TLCSTS'                                         
         L     R5,=A(DICSECT)                                                   
         MVC   MID1+20(L'SP@NOTLC),SP@NOTLC                                     
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
*---->   MVC   MID1+20(9),=C'NO BRCSTS'                                         
         MVC   MID1+20(L'SP9BRDCS),SP9BRDCS                                     
         MVC   MID2+20(9),=9C'-'                                                
BTSSUMA  CLI   SUMOPTS+1,0                                                      
         BE    BTSSUMB                                                          
*---->   MVC   MID1+34(7),=C'DOLLARS'                                           
         MVC   MID1+34(L'SP7DOLLA),SP7DOLLA                                     
         MVC   MID2+34(7),=7C'-'                                                
*                                                                               
         LA    R6,MID1+34                                                       
         MVC   0(10,R6),=C'GROSS COST'                                          
         CLI   Q2NET,C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(10,R6),=C' NET COST '                                          
         CLI   Q2NET,C'B'                                                       
         BNE   *+10                                                             
         MVC   0(10,R6),=C'*NET COST '                                          
         MVC   MID2+34(10),=10C'-'                                              
*                                                                               
BTSSUMB  CLI   SUMOPTS+2,0                                                      
         BE    BTSSUMC                                                          
*                                                                               
         LA    RE,MID1+49                                                       
         LA    R9,MID2+49                                                       
         LA    RF,DNAME1                                                        
         LA    R0,4                                                             
*                                                                               
BTSSUMB1 CLI   0(RF),0                                                          
         BE    BTSSUMC                                                          
         MVC   0(7,RE),0(RF)                                                    
         MVC   0(7,R9),=7C'-'                                                   
         LA    RE,20(RE)                                                        
         LA    RF,7(RF)                                                         
         LA    R9,20(R9)                                                        
         BCT   R0,BTSSUMB1                                                      
*                                                                               
BTSSUMC  DS    0H                                                               
*---->   MVC   P1(5),=C'MONTH'                                                  
         MVC   P1(L'SP@MONTH),SP@MONTH                                          
*                                                                               
BTSSUM1  GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    BTSSUM11                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    BTSSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    BTSSUM1                                                          
*                                                                               
BTSSUM11 CLI   SUMRTYP,0                                                        
         BE    BTSSUMX                                                          
         CLI   SUMRTYP,1                                                        
         BE    BTSSUM1                                                          
         OC    SUMSPOTS(8),SUMSPOTS                                             
         BZ    BTSSUM1                                                          
*                                                                               
         CLI   SUMDPART,C''       SPECIAL DETAIL SUPPRESS                      
         BE    BTSSUM1                                                          
         CLI   SUMDPGRP,C''                                                    
         BE    BTSSUM1                                                          
*                                                                               
         CLI   DPTSW,1                                                          
         BNE   BTSSUM1A                                                         
*---->   MVC   P1+7(3),=C'TOT'                                                  
         MVC   P1+7(L'SP3TOTAL),SP3TOTAL                                        
*---->   MVC   MID1(7),=C'*TOTAL*'                                              
         MVC   MID1(L'SP7TOTAL),SP7TOTAL                                        
*                                                                               
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID1(7),=C'MKT TOT'                                              
         MVC   MID1(L'SP7MKTTL),SP7MKTTL                                        
*                                                                               
         CLI   MODE,PRDLAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID1(7),=C'PRD TOT'                                              
         MVC   MID1(L'SP7PROTL),SP7PROTL                                        
*                                                                               
         CLI   SUMDPGNO,X'FF'                                                   
         BE    BTSSUM1A                                                         
         MVC   MID1(3),SUMDPART                                                 
         MVI   MID1+3,C'-'                                                      
         EDIT  (1,SUMSLN),(3,MID1+4)                                            
*                                                                               
BTSSUM1A DS    0H                                                               
         MVI   DPTSW,0                                                          
*---->   MVC   P+7(3),=C'TOT'                                                   
         MVC   P+7(L'SP3TOTAL),SP3TOTAL                                         
         DROP  R5                                                               
         CLI   SUMRTYP,3                                                        
         BE    BTSSUM2                                                          
         GOTO1 DATCON,DMCB,(X'02',SUMDT+2),(X'04',WORK)                         
         MVC   P+7(3),WORK                                                      
         CLI   SPOTPROF+8,1        FUNNY WEEK START/END                         
         BH    *+12                                                             
         CLI   SPOTPROF+2,0        OR BROADCAST MONTHS                          
         BE    BTSSUM2                                                          
         GOTO1 DATCON,DMCB,(X'02',SUMDT),(X'05',P)                              
         MVI   P+8,C'-'                                                         
         GOTO1 DATCON,DMCB,(X'02',SUMDT+2),(X'05',P+9)                          
*                                                                               
BTSSUM2  CLI   SUMOPTS,0                                                        
         BE    BTSSUM2B                                                         
         EDIT  SUMSPOTS,(10,P+19)                                               
*                                                                               
BTSSUM2B CLI   SUMOPTS+1,0                                                      
         BE    BTSSUM2C                                                         
         L     R9,SUMDL                                                         
         M     R8,=F'2'                                                         
         D     R8,=F'100'                                                       
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         MVI   CURTAB+3,0                                                       
         CURED (R9),(9,P+33),CURTAB,DMCB=CURDMCB,FLOAT=-                        
*                                                                               
BTSSUM2C MVC   FULL,=F'1'          SET SPOT COUNT FOR NOT AVERAGING             
         MVC   DMCB+12(4),WEIGHT                                                
         MVC   WEIGHT,=F'1'                                                     
         GOTO1 VCALCPP,DMCB,FULL,(R3)                                           
         MVC   WEIGHT(4),DMCB+12                                                
*                                                                               
         LA    R4,SVDEMS                                                        
         LA    R5,P+44                                                          
         MVI   DEMINDX,0                                                        
*                                                                               
BTSSUM3  GOTO1 =A(SETPTDEM)        GET DEMO LIST ADDRESS                        
*                                                                               
         OC    0(4,R4),0(R4)                                                    
         BZ    BTSSUM5                                                          
         CLI   SUMOPTS+2,0                                                      
         BE    BTSSUM7                                                          
*                                                                               
         L     R9,0(R4)            GET DEMO VALUE                               
*                                                                               
         CLI   RATING,C'Y'                                                      
         BNE   BTSSUM3A                                                         
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    BTSSUM3B                                                         
         B     *+12                                                             
*                                                                               
BTSSUM3A TM    RQOPTS,RQOPTS_2DECIMP                                            
         BZ    BTSSUM3B                                                         
*                                                                               
         M     R8,=F'2'                                                         
         D     R8,=F'10'                                                        
         AHI   R9,1                                                             
         SRL   R9,1                                                             
*                                                                               
BTSSUM3B C     R9,=F'99999'        DECIMAL PRECISION OK                         
         BH    BTSSUM4              NO - DIVIDE BY 10                           
         MVI   CURTAB+3,1                                                       
         CURED (R9),(7,(R5)),CURTAB,DMCB=CURDMCB                                
         B     BTSSUM5                                                          
*                                                                               
BTSSUM4  M     R8,=F'2'            DROP DECIMAL PRECISION                       
         D     R8,=F'10'                                                        
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         EDIT  (R9),(7,(R5))                                                    
*                                                                               
BTSSUM5  LA    R5,7(R5)                                                         
         LA    R4,4(R4)            POINT TO CPP                                 
*                                                                               
         ICM   R9,15,0(R4)                                                      
         BZ    BTSSUM6                                                          
*                                                                               
         CLI   SUMOPTS+2,2         PRINT DEMOS ONLY                             
         BE    BTSSUM5A                                                         
         CLI   SUMOPTS+1,1         PRINT CPP                                    
         BNE   BTSSUM6                                                          
         MVI   CURTAB+3,2                                                       
         CURED (R9),(7,(R5)),CURTAB,DMCB=CURDMCB                                
         CLC   SUMDL,SUMDLEQ                                                    
         BE    *+8                                                              
         MVI   7(R5),C'+'                                                       
*                                                                               
BTSSUM5A C     R9,=F'99999'                                                     
         BNH   BTSSUM6                                                          
         M     R8,=F'2'                                                         
         D     R8,=F'10'                                                        
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         MVI   CURTAB+3,1                                                       
         CURED (R9),(7,(R5)),CURTAB,DMCB=CURDMCB                                
*                                                                               
BTSSUM6  LA    R4,4(R4)                                                         
         LA    R5,13(R5)                                                        
         LLC   RE,DEMINDX                                                       
         LA    RE,1(RE)                                                         
         STC   RE,DEMINDX                                                       
         CLI   DEMINDX,4                                                        
         BL    BTSSUM3                                                          
*                                                                               
BTSSUM7  GOTO1 REPORT                                                           
*                                                                               
         L     R7,MEDBUFF          RESTORE MEDBUFF REGISTER                     
*                                                                               
         GOTO1 VFOOT,DMCB,(RA)                                                  
*                                                                               
         CLI   SUMRTYP,3                                                        
         BNE   BTSSUM1                                                          
         CLI   SUMDPGNO,X'FF'                                                   
         BE    BTSSUMX                                                          
         B     BTSDPT                                                           
*                                                                               
BTSSUMX  CLI   BRSPASS2,0                                                       
         BE    BTSSUMX1                                                         
         CLI   BTSACTIV,0                                                       
         BE    BTSSUMX1                                                         
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   BRSPASS2,D2APROF+7                                               
         NI    BRSPASS2,X'0F'                                                   
*                                                                               
         SR    R5,R5                                                            
         IC    R5,BRSPASS2                                                      
         MHI   R5,3                                                             
         LA    R5,SUMOPT(R5)                                                    
         MVC   SUMOPTS,0(R5)                                                    
         CLI   QOPT4,C'Y'                                                       
         BNE   *+8                                                              
         MVI   SUMOPTS+1,0                                                      
*                                                                               
         OC    SUMOPTS(3),SUMOPTS                                               
         BZ    BTSSUMX1                                                         
         MVI   BRSPASS2,0                                                       
         MVC   BRSDATE,D2APROF+6                                                
         NI    BRSDATE,X'0F'                                                    
         L     R5,=A(DICSECT)                                                   
*                                  SET RECAP TITLE                              
         MVI   BRSPCAP,C' '                                                     
         MVC   BRSPCAP+1(L'BRSPCAP-1),BRSPCAP                                   
         MVI   P,0                                                              
         LA    RE,P2                                                            
         CLI   SUMOPTS,0                                                        
         BE    *+14                                                             
         MVC   0(8,RE),=C'TELECAST'                                             
         LA    RE,9(RE)                                                         
         CLI   SUMOPTS+1,0                                                      
         BE    *+14                                                             
         MVC   0(6,RE),=C'DOLLAR'                                               
         LA    RE,7(RE)                                                         
         CLI   SUMOPTS+2,0                                                      
         BE    *+14                                                             
         MVC   0(11,RE),=C'DEMOGRAPHIC'                                         
         LA    RE,12(RE)                                                        
         MVC   0(5,RE),=C'RECAP'                                                
         B     BTSMKT                                                           
*                                                                               
BTSSUMX1 MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   SUMOPTS,SVSUMOPT                                                 
*                                                                               
BTSSUMX2 XIT1                                                                   
*                                                                               
BTSSL    DS    0C                                                               
BTSDP    DS    0C                                                               
         GOTO1 =A(DSTSUM)                                                       
         B     BTSSUMX2                                                         
BTSACTIV DS    C                                                                
         LTORG                                                                  
         EJECT                                                                  
BDSSUM   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         SPACE 2                                                                
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         CLI   PROFMSR,0                                                        
         BE    BDSDP                                                            
         DROP  RE                                                               
* RECREATE TOTAL MEDTAB                                                         
         CLI   BUFCDE,X'96'        SUMMARIES WITH OTHER CONTROLS                
         BE    BDS2                                                             
         CLI   BUFCDE,X'97'                                                     
         BE    BDS2                                                             
         OC    SUMOPTS,SUMOPTS     TEST SUPPRESS SUMMARY                        
         BZ    BDSSUMX2                                                         
*                                                                               
BDS2     L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         MVI   BDSACTIV,0                                                       
         MVC   SVSUMOPT,SUMOPTS                                                 
         MVC   P,SPACES                                                         
         CLI   MODE,PRDLAST                                                     
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLI   D2APROF+6,0         DEFAULT DATE OVERRIDE                        
         BNE   *+8                                                              
         MVI   D2APROF+6,X'50'     DEFAULT WEEK/PERIOD                          
         MVC   BRSPASS2(1),D2APROF+6 ELSE - SET FROM PROFILE                    
         NI    BRSPASS2,X'0F'                                                   
         LLC   RE,D2APROF+6                                                     
         SRL   RE,4                                                             
         STC   RE,BRSDATE                                                       
*                                                                               
         MVI   BRSPCAP,C' '                                                     
         MVC   BRSPCAP+1(L'BRSPCAP-1),BRSPCAP                                   
         CLI   MODE,STALAST                                                     
         BNE   *+10                                                             
         MVC   BRSPCAP(L'SP@STATL),SP@STATL   ***STATION TOTAL***               
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
         MVC   BRSPCAP(L'SP@MKTTL),SP@MKTTL   ***MARKET TOTAL***                
         CLI   MODE,PRDLAST                                                     
         BNE   *+10                                                             
         MVC   BRSPCAP(L'SP@PROTL),SP@PROTL   ***PRODUCT TOTAL***               
         MVC   BRSPASS2(1),D2APROF+7                                            
         CLI   BUFCDE,X'96'                                                     
         BE    BDSSL                                                            
         CLI   BUFCDE,X'97'                                                     
         BE    BDSDP                                                            
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    BDSMKT                                                           
         MVI   BRSPASS2,0                                                       
         CLI   BUFCDE,X'88'        SPILL DATA                                   
         BE    BDSSUMX                                                          
         CLI   BUFCDE,X'89'        ORIG DATA                                    
         BE    BDSSUMX                                                          
BDSMKT   MVC   MEDNUMWK,=F'60'                                                  
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVI   BUFHI,1                                                          
         MVI   BUFRTYP,1                                                        
         GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   SUMRTYP,0                                                        
         BE    BDSSUMX                                                          
         MVI   BDSACTIV,1                                                       
         MVI   BUFHI,1                                                          
         MVC   P+56(L'BRSPCAP),BRSPCAP                                          
BDSDPT   DS    0H                                                               
         CLI   SUMSLN,X'FD'        SET SPILL MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***SPILL '                                            
         MVC   P+48(L'SP8SPILL),SP8SPILL                                        
         CLI   SUMSLN,X'FE'        SET ORIG. MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***ORIG. '                                            
         MVC   P+48(L'SP8ORIG),SP8ORIG                                          
         DROP  R5                                                               
         L     R5,MEDAFRST         CLEAR MEDBLOCK                               
         LA    R6,MEDPERD                                                       
CLRBDS   L     RE,4(R5)                                                         
         LTR   RE,RE                                                            
         BZ    CLRBDS2                                                          
         L     RF,MEDLCHNK                                                      
         XCEF                                                                   
CLRBDS2  LA    R5,12(R5)                                                        
         CR    R5,R6                                                            
         BH    CLRBDS3                                                          
         B     CLRBDS                                                           
*                                                                               
CLRBDS3  DS    0H                                                               
         L     R5,MEDAFRST                                                      
BDSSUM1  GOTO1 VGETBUF                                                          
         L     R5,MEDAFRST                                                      
         LA    R3,MYBUFIO                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    BDSSUM1A                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    BDSSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    BDSSUM1                                                          
*                                                                               
BDSSUM1A DS    0H'0'                                                            
         CLI   SUMRTYP,0                                                        
         BE    BDSSUMX                                                          
         CLI   SUMRTYP,2           BYPASS MONTHLY                               
         BNE   *+12                                                             
         TM    BRSDATE,X'02'                                                    
         BZ    BDSSUM1                                                          
         CLI   SUMRTYP,1           WEEKS                                        
         BNE   *+12                                                             
         TM    BRSDATE,X'01'                                                    
         BZ    BDSSUM1                                                          
*                                                                               
         CLI   SUMDPART,C''       SPECIAL DETAIL SUPPRESS                      
         BE    BDSSUM1                                                          
         CLI   SUMDPGRP,C''                                                    
         BE    BDSSUM1                                                          
*                                                                               
         CLI   SUMRTYP,2           SET FOR MONTHLY                              
         BNE   *+12                                                             
         LA    R5,MEDMON01                                                      
         B     BDSSUM2                                                          
*                                                                               
         CLI   SUMRTYP,3           SET FOR PERIOD                               
         BNE   BDSSUM2                                                          
         LA    R5,MEDPERD                                                       
         B     BDSSUM3                                                          
*                                                                               
BDSSUM2  CLC   2(2,R5),SUMDT+2     FIND CORRECT BUFFER SLOT                     
         BE    BDSSUM3                                                          
         CLC   0(2,R5),SUMDT                                                    
         BE    BDSSUM3                                                          
         CLC   SUMDT(2),MEDPERD+2  WHEN SUMMARIES ARE RUN WITH D6               
         BH    BDSSUM1              THERE MAY BE EXTRA DAYS                     
         LA    R5,12(R5)                                                        
         B     BDSSUM2                                                          
*                                                                               
BDSSUM3  L     R4,4(R5)            INSERT DATA INTO MEDBLOCK                    
         MVC   MEDBYSPT,SUMSPOTS                                                
         MVC   MEDBY1,SUMD1                                                     
         MVC   MEDBY2,SUMD2                                                     
         MVC   MEDBY3,SUMD3                                                     
         MVC   MEDBY4,SUMD4                                                     
         MVC   MEDBYD,SUMDL                                                     
         CLI   SUMRTYP,3           PERIOD RECORD                                
         BE    *+8                                                              
         B     BDSSUM1                                                          
* PRINT SUMMARY                                                                 
         L     R5,MEDAFRST                                                      
*                                                                               
BDSSUM4  L     R4,4(R5)                                                         
         LTR   R4,R4                                                            
         BZ    BDSSUM8A                                                         
         LA    R8,15                                                            
         LA    R9,P2+11                                                         
         CLI   SUMDPGRP,X'FF'                                                   
         BE    BDSSUM4A                                                         
         MVC   P2(3),SUMDPART                                                   
         EDIT  (1,SUMSLN),(3,P2+4)                                              
         MVI   P2+3,C'-'                                                        
*                                                                               
BDSSUM4A MVI   P3,0                                                             
         LA    R6,P4                                                            
         MVI   ALLOWLIN,10                                                      
         L     RF,=A(DICSECT)                                                   
         USING DICSECT,RF                                                       
         CLI   SUMOPTS,0                                                        
         BE    *+14                                                             
*---->   MVC   0(7,R6),=C'  SPOTS'                                              
         MVC   0(L'SP5SPOTS,R6),SP5SPOTS                                        
         LA    R6,132(R6)                                                       
         CLI   SUMOPTS+1,0                                                      
         BE    BDSSUM4C                                                         
*---->   MVC   0(7,R6),=C'   COST'                                              
         MVC   0(L'SP@COST,R6),SP@COST                                          
*                                                                               
         CLI   PWPRESW,C'Y'                                                     
         BNE   BDSSUM4B                                                         
         MVC   0(10,R6),=C'GROSS COST'                                          
         CLI   Q2NET,C'Y'                                                       
         BNE   *+10                                                             
         MVC   0(10,R6),=C'  NET COST'                                          
         CLI   Q2NET,C'B'                                                       
         BNE   *+10                                                             
         MVC   0(10,R6),=C' *NET COST'                                          
*                                                                               
BDSSUM4B LA    R6,132(R6)                                                       
*                                                                               
BDSSUM4C CLI   SUMOPTS+2,0                                                      
         BE    BDSSUM5                                                          
         MVC   0(7,R6),DNAME1                                                   
         LA    R6,132(R6)                                                       
         MVC   0(7,R6),DNAME2                                                   
         LA    R6,132(R6)                                                       
         MVC   0(7,R6),DNAME3                                                   
         LA    R6,132(R6)                                                       
         MVC   0(7,R6),DNAME4                                                   
*                                                                               
BDSSUM5  LA    R6,MEDPERD                                                       
         L     RF,=A(DICSECT)                                                   
         OC    0(4,R5),0(R5)       SLOT ACTIVE                                  
         BZ    BDSSUM8A             NO - BYPASS                                 
         CR    R5,R6                                                            
         BL    BDSSUM6                                                          
         BH    BDSSUM9                                                          
*---->   MVC   3(5,R9),=C'TOTAL'                                                
         MVC   3(L'SP5TOTAL,R9),SP5TOTAL                                        
         DROP  RF                                                               
         B     BDSSUM7                                                          
*                                                                               
BDSSUM6  GOTO1 DATCON,DMCB,(X'02',(R5)),(X'04',WORK)   PRINT MMMDD              
         MVC   3(5,R9),WORK                                                     
         LA    R6,MEDMON01                                                      
         CR    R5,R6                                                            
         BL    BDSSUM7                                                          
* GET BRDCST MONTH IN WHICH END DATE FALLS                                      
         L     RF,ADCONLST                                                      
         L     RF,VBRDMON-SPADCONS(RF)                                          
         GOTO1 (RF),DMCB,(X'FF',2(R5)),WORK                                     
* NOW GET MMMYY OF THE BROADCAST MONTH DATE                                     
         GOTO1 DATCON,DMCB,(X'02',WORK),(X'06',WORK+6)                          
         MVC   3(5,R9),SPACES                                                   
         MVC   4(3,R9),WORK+6                                                   
*                                                                               
BDSSUM7  ST    R9,PRTADDR                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BNZ   *+12                                                             
         LA    R8,1(R8)                                                         
         B     BDSSUM8A                                                         
         LA    R9,132(R9)                                                       
         LA    R9,132(R9)                                                       
         CLI   SUMOPTS,0           SPOTS                                        
         BE    BDSSUM7A                                                         
         EDIT  (4,MEDBYSPT),(5,3(R9))                                           
         LA    R9,132(R9)                                                       
*                                                                               
BDSSUM7A CLI   SUMOPTS+1,0         COST                                         
         BE    BDSSUM7B                                                         
         L     RF,MEDBYD                                                        
         M     RE,=F'2'                                                         
         D     RE,=F'100'                                                       
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
*                                                                               
BDSSUM7B CLI   SUMOPTS+2,0         DEMOS                                        
         BE    BDSSUM8                                                          
         LA    R6,MEDBY1                                                        
         MVI   DEMINDX,0                                                        
*                                                                               
BDSSUM7C GOTO1 =A(SETPTDEM)        GET DEMO LIST ADDRESS                        
*                                                                               
         LA    R0,10                                                            
         CLI   RATING,C'Y'                                                      
         BNE   BDSSUM7D                                                         
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    BDSSUM7E                                                         
         B     *+12                                                             
*                                                                               
BDSSUM7D TM    RQOPTS,RQOPTS_2DECIMP                                            
         BZ    BDSSUM7E                                                         
         LA    R0,100                                                           
*                                                                               
BDSSUM7E L     RF,0(R6)                                                         
         M     RE,=F'2'                                                         
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
*                                                                               
         LA    R9,132(R9)                                                       
         LA    R6,8(R6)                                                         
         LLC   RE,DEMINDX                                                       
         LA    RE,1(RE)                                                         
         STC   RE,DEMINDX                                                       
         CLI   DEMINDX,4                                                        
         BL    BDSSUM7C                                                         
*                                                                               
BDSSUM8  L     R7,MEDBUFF          RESTORE MEDBLOCK REG                         
         L     R9,PRTADDR          SET NEXT SLOT                                
         LA    R9,8(R9)                                                         
*                                                                               
BDSSUM8A LA    R5,12(R5)                                                        
         L     R4,4(R5)                                                         
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BH    BDSSUM9                                                          
         OC    0(4,R5),0(R5)                                                    
         BZ    BDSSUM8A                                                         
         LTR   R4,R4                                                            
         BZ    BDSSUM8A                                                         
*                                                                               
         LA    R6,MEDMON01                                                      
         TM    BRSDATE,X'02'       CHECK IF DOING MONTHS                        
         BZ    *+8                                                              
         LA    R6,MEDQRT01            YES - ALLOW THEM TO PRINT                 
         BCTR  R6,0                                                             
         CR    R5,R6                                                            
         BH    *+12                                                             
         BCT   R8,BDSSUM5                                                       
         B     BDSSUM9                                                          
*                                                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         BCT   R8,BDSSUM5                                                       
*                                                                               
BDSSUM9  GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,0                                                       
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BNH   BDSSUM4                                                          
         B     BDSDPT                                                           
*                                                                               
BDSSUMX  CLI   BRSPASS2,0                                                       
         BE    BDSSUMX1                                                         
         CLI   BDSACTIV,0                                                       
         BE    BDSSUMX1                                                         
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
*                                                                               
         MVI   ALLOWLIN,10                                                      
         NI    BRSPASS2,X'0F'                                                   
         LLC   R5,BRSPASS2                                                      
         MH    R5,=H'3'                                                         
         LA    R5,SUMOPT(R5)                                                    
         MVC   SUMOPTS,0(R5)                                                    
         CLI   QOPT4,C'Y'                                                       
         BNE   *+8                                                              
         MVI   SUMOPTS+1,0                                                      
         OC    SUMOPTS(3),SUMOPTS                                               
         BZ    BDSSUMX1                                                         
         MVI   BRSPASS2,0                                                       
         MVC   BRSDATE,D2APROF+6                                                
         NI    BRSDATE,X'0F'                                                    
         CLI   BRSDATE,0                                                        
         BE    BDSSUMX1                                                         
         L     R5,=A(DICSECT)                                                   
*                                  SET RECAP TITLE                              
         MVI   BRSPCAP,C' '                                                     
         MVC   BRSPCAP+1(L'BRSPCAP-1),BRSPCAP                                   
         LA    RE,P                                                             
         CLI   SUMOPTS,0                                                        
         BE    *+14                                                             
         MVC   0(8,RE),=C'TELECAST'                                             
         LA    RE,9(RE)                                                         
         CLI   SUMOPTS+1,0                                                      
         BE    *+14                                                             
         MVC   0(6,RE),=C'DOLLAR'                                               
         LA    RE,7(RE)                                                         
         CLI   SUMOPTS+2,0                                                      
         BE    *+14                                                             
         MVC   0(11,RE),=C'DEMOGRAPHIC'                                         
         LA    RE,12(RE)                                                        
         MVC   0(5,RE),=C'RECAP'                                                
*                                                                               
         B     BDSMKT                                                           
*                                                                               
BDSSUMX1 MVC   SUMOPTS,SVSUMOPT                                                 
BDSSUMX2 XIT1                                                                   
*                                                                               
BDSSL    DS    0C                                                               
BDSDP    DS    0C                                                               
         BRAS  RE,DSTSUM                                                        
         B     BDSSUMX2                                                         
BDSACTIV DS    C                                                                
         LTORG                                                                  
         EJECT                                                                  
CALCPP   NTR1  BASE=*,LABEL=*      PARAM = SPOT COUNT FOR DEMO AVERAGE          
         USING MEDBLOCK,R7                                                      
*                                                                               
         L     RF,0(R1)                                                         
         MVC   SPOTS,0(RF)         SAVE NUMBER OF SPOTS                         
*                                                                               
         L     R3,4(R1)                                                         
         USING SUMDSECT,R3                                                      
*                                                                               
         ICM   RE,15,SUMDL                                                      
         BM    CALCPPX                                                          
*                                                                               
         CLI   SPOTPROF+1,C'N'                                                  
         BE    UNWTX                                                            
         CLI   SPOTPROF+1,0                                                     
         BE    UNWTX                                                            
         OC    WEIGHT,WEIGHT                                                    
         BZ    UNWTX                                                            
                                                                                
* NOTE MUST USE DEMO NAMES BECAUSE OF USER DEMOS                                
                                                                                
         LA    R4,DNAMES                                                        
         LA    R5,4                                                             
         LA    R6,SUMD1                                                         
*                                                                               
UNWT1    CLI   SPOTPROF+1,C'D'     UNWEIGHT DEMOS                               
         BE    UNWT2                                                            
         CLI   0(R4),C'R'                                                       
         BE    UNWT2                                                            
         CLI   0(R4),C'E'                                                       
         BNE   UNWT4                                                            
*                                                                               
UNWT2    L     RF,0(R6)                                                         
         M     RE,=F'2'                                                         
         D     RE,WEIGHT                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,0(R6)                                                         
*                                                                               
         L     RF,4(R6)                                                         
         M     RE,=F'2'                                                         
         D     RE,WEIGHT                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,4(R6)                                                         
*                                                                               
UNWT4    LA    R4,7(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,UNWT1                                                         
*                                                                               
UNWTX    DS    0H                                                               
         XC    SVDEMS,SVDEMS                                                    
         LA    R4,DNAMES                                                        
         LA    R5,SVDEMS                                                        
         LA    R6,SUMD1                                                         
         MVI   DEMINDX,0                                                        
         GOTO1 =A(SETPTDEM)        GET PTBUFF DEMO ADDRESS IN RF                
*                                                                               
CALCPP1  L     RE,SUMDL            GET DOLLARS                                  
         CLI   CPPSW,C'D'                                                       
         BNE   *+8                                                              
         L     RE,SUMDLEQ          USE EQUIVALENCED DOLLARS                     
*                                                                               
         L     R8,0(R6)            GET DEMO VALUE                               
         CLI   CPPSW,C'D'                                                       
         BE    *+8                                                              
         L     R8,4(R6)            USE EQUIVALENCED DEMOS                       
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BNE   CALCPP1A                                                         
         L     R8,0(R6)            USE UNEQV DEMO                               
         L     RE,SUMDL            AND UNEQV DOLLARS                            
*                                                                               
CALCPP1A CLI   0(R4),C'R'          TEST RATING                                  
         JE    *+12                                                             
         CLI   0(R4),C'E'          OR EXTENDED RATING                           
         BNE   CALCPP1B                                                         
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMALS ACTIVE                       
         BO    CALCPP1C                                                         
         B     CALCPP1D                                                         
*                                                                               
CALCPP1B TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC IMPS ACTIVE                       
         JZ    CALCPP1D                                                         
*                                                                               
CALCPP1C MHI   RE,10               ADJUST DOLS FOR EXTRA DEC PLACE              
*                                                                               
CALCPP1D LTR   R8,R8                                                            
         BZ    CALCPP2                                                          
*                                                                               
         CVD   RE,DUB                                                           
         ZAP   P16,DUB                                                          
*                                                                               
         CVD   R8,DUB                                                           
         MP    P16,=P'20'          X 2 X 10                                     
*                                                                               
         DP    P16,DUB                                                          
         ZAP   DUB,P16(8)                                                       
         AP    DUB,=P'1'                                                        
         ZAP   P16,DUB                                                          
         ZAP   DUB,=P'2'                                                        
         DP    P16,DUB                                                          
         ZAP   DUB,P16(8)                                                       
         XR    RF,RF                                                            
         CP    DUB,=P'2147483647'                                               
         BH    *+8                                                              
         CVB   RF,DUB                                                           
         ST    RF,4(R5)            SAVE CPP                                     
*                                                                               
CALCPP2  L     R9,0(R6)            CALCULATE DEMO AVERAGES                      
         CLC   SPOTS,=F'1'                                                      
         BE    CALCPP3X                                                         
         M     R8,=F'2'                                                         
         OC    SPOTS,SPOTS         TEST FOR SPOTS                               
         BNZ   CALCPP3                                                          
         SR    R9,R9                                                            
         B     CALCPP3X                                                         
*                                                                               
CALCPP3  D     R8,SPOTS            DIVIDE BY SPOTS                              
         AHI   R9,1                                                             
         SRA   R9,1                                                             
CALCPP3X ST    R9,0(R5)            SAVE DEMO                                    
*                                                                               
CALCPP4  LA    R4,7(R4)            NEXT DEMO NAME                               
         LA    R5,8(R5)            GET NEXT                                     
         LA    R6,8(R6)                                                         
         LLC   RE,DEMINDX                                                       
         LA    RE,1(RE)                                                         
         STC   RE,DEMINDX                                                       
         CLI   DEMINDX,4                                                        
         BL    CALCPP1                                                          
*                                                                               
         OC    SUMGDL(16),SUMGDL                                                
         BZ    CALCPPX                                                          
         L     RE,SUMGDL                                                        
         CLI   CPPSW,C'D'                                                       
         BNE   *+8                                                              
         L     RE,SUMGDLE                                                       
*                                                                               
         L     R8,SUMGD1                                                        
         CLI   CPPSW,C'D'                                                       
         BE    *+8                                                              
         L     R8,SUMGD1E                                                       
*                                                                               
         LTR   R8,R8                                                            
         BZ    CALCPPX                                                          
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         DR    RE,R8                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,4(R5)                                                         
CALCPPX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
BFLOAT   NTR1  BASE=*,LABEL=*                                                   
         L     R6,BUFFBUFF                                                      
         USING BUFFALOD,R6                                                      
         MVC   4(8,R1),ABUFF       SET PREV. BUFF                               
         OC    ABUFF,ABUFF                                                      
         BNZ   BFLOAT2                                                          
         MVC   LNBUFF,=F'50000'    MINIMUM BUFFER                               
         GOTO1 =V(COVAIL),DMCB,C'LOOK'                                          
         L     R9,8(R1)            GET AMOUNT OF CORE LEFT                      
         C     R9,=F'750000'       ENOUGH FOR ALLOCATION                        
         BL    *+12                                                             
         S     R9,=F'700000'       LEAVE ENOUGH FOR SUBPROGRAMS                 
         ST    R9,LNBUFF                                                        
         MVC   ABUFF,=F'50000'     MINIMUM BUFFER                               
         GOTO1 =V(COVAIL),DMCB,C'GET',ABUFF,LNBUFF                              
BFLOAT2  OC    4(8,R1),4(R1)       ALLOCATE OK                                  
         BNZ   *+6                                                              
         DC    H'0'                NOT ENOUGH CORE LEFT                         
         MVC   ABUFF,4(R1)                                                      
         L     R9,4(R1)            SHIFT BUFFALO TO NEW AREA                    
         MVC   0(255,R9),0(R6)                                                  
         ST    R9,BUFFBUFF                                                      
         LR    R6,R9                                                            
         MVC   BUFFADDR,4(R1)      SET BUFFER ADDRESS                           
         L     R9,8(R1)            GET LENGTH OF BUFFER                         
         SR    R8,R8                                                            
         D     R8,BUFFLALL         DIVIDE BY RECORD LENGTH                      
         ST    R9,BUFFCRMX                                                      
         XIT1                                                                   
         DROP  R6                                                               
ABUFF    DC    A(0)                ADDRESS OF BUFFER                            
LNBUFF   DC    A(0)                LENGTH OF BUFFER                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* S/R TO PRINT ENTRY IN TABLE                                                   
*===============================================================                
         SPACE 1                                                                
PRTMGA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,=A(MGTABST)                                                   
         USING MGENTRYD,R4                                                      
*                                                                               
         OC    MGECODE,MGECODE     NOTHING THERE JUST GET OUT                   
         BZ    MGASX10                                                          
*                                                                               
         CLI   COUNTRY,C'C'        TEST CANADA                                  
         BE    MGAS01                                                           
         CLI   SVBUYKEY+6,X'E8'    TEST CABLE                                   
         BL    MGAS01                                                           
         CLI   MODE,CBHLAST                                                     
         BNE   MGASX10                                                          
*                                                                               
MGAS01   MVC   SVRTYPE,RTYPE                                                    
         MVC   RTYPE,=C'MGA'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   P1,C' '                                                          
         GOTO1 REPORT                                                           
*                                                                               
         L     R4,=A(MGTABST)                                                   
         SR    R5,R5                                                            
         XC    MGA2CNT,MGA2CNT                                                  
         XC    MGATCNT,MGATCNT                                                  
*                                                                               
MGAS02   OC    MGECODE,MGECODE                                                  
         BZ    MGAS03                                                           
         LA    R4,MGERECL(R4)                                                   
         BCT   R5,MGAS02                                                        
*                                                                               
MGAS03   LPR   R5,R5                                                            
         ST    R5,FULL             SAVE NUMBER OF ENTRIES                       
         L     R4,=A(MGTABST)                                                   
*                                                                               
         CLI   MODE,CBHLAST        TEST CABLE                                   
         BNE   MGAS03X                                                          
                                                                                
*=====================================================================          
* REPLACE 3-BYTE HEX STA WITH ALPHA NET SO GET NETWORKS IN ALPHA SEQ            
* MODE OF CBHLAST TELLS US ITS CABLE!                                           
*=====================================================================          
                                                                                
MGAS03A  XC    WORK,WORK                                                        
         MVC   WORK+2(3),MGESTA                                                 
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+10,WORK+15                         
         MVC   MGESTA,WORK+20      MOVE NETWORK                                 
         LA    R4,MGERECL(R4)                                                   
         BCT   R5,MGAS03A                                                       
*                                                                               
MGAS03X  L     R4,=A(MGTABST)                                                   
         L     R5,FULL                                                          
*                                                                               
         GOTO1 XSORT,DMCB,(R4),(R5),MGERECL,MGEKEYL,0                           
*                                                                               
         LA    R8,P                                                             
         USING LINED,R8                                                         
         MVC   MGA2CNT(2),MGECODE                                               
*                                                                               
MGAS04   CLC   MGA2CNT(2),MGECODE  CHANGE OF CODE                               
         BE    MGAS06                                                           
*                                                                               
         MVC   LTIME(09),=C'**TOTAL**'  PRINT TOTALS                            
         LA    R5,LMSCOST                                                       
         L     R1,MGA2CNT+4                                                     
         BAS   RE,EDTMGCOS                                                      
*                                                                               
         LA    R5,LMGCOST                                                       
         L     R1,MGA2CNT+8                                                     
         BAS   RE,EDTMGCOS                                                      
*                                                                               
         LA    R5,LMSRTG                                                        
         L     R1,MGA2CNT+12                                                    
         BAS   RE,EDTMGTOT                                                      
*                                                                               
         LA    R5,LMGRTG                                                        
         L     R1,MGA2CNT+16                                                    
         BAS   RE,EDTMGTOT                                                      
         GOTO1 REPORT                                                           
*                                                                               
         LA    RE,MGA2CNT          SUM INTO TOTAL BUCKETS                       
         LA    RF,MGATCNT                                                       
         LA    R0,5                                                             
*                                                                               
MGATOT   L     R1,0(RE)                                                         
         A     R1,0(RF)                                                         
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,MGATOT                                                        
*                                                                               
         XC    MGA2CNT,MGA2CNT                                                  
         MVI   P+1,0                                                            
         GOTO1 REPORT                                                           
*                                                                               
MGAS06   OC    MGECODE,MGECODE                                                  
         BZ    MGASX                                                            
*                                                                               
         MVC   LCODE,MGECODE                                                    
         CLI   MGETYPE,0                                                        
         BNE   MGAS10                                                           
         CLI   MGETYPE,X'FE'       TEST TOTAL                                   
         BNL   MGAS07                                                           
*                                                                               
         MVC   LTYPE,=C'MS- '                                                   
         CLI   MODE,CBHLAST        TEST CABLE                                   
         BNE   MGAS07                                                           
         MVI   LTYPE,C'-'                                                       
         MVC   LTYPE+1(3),MGESTA   MOVE NETWORK                                 
*                                                                               
MGAS07   L     R1,MGERTGB                                                       
         LA    R5,LMSRTG                                                        
         BAS   RE,EDTMGDEM         EDIT DEMO VALUE TO PRINT LINE                
*                                                                               
         L     R1,MGERTGB                                                       
         A     R1,MGA2CNT+12                                                    
         ST    R1,MGA2CNT+12                                                    
         B     MGAS20                                                           
*                                                                               
MGAS10   CLI   MGETYPE,X'FE'                                                    
         BE    MGAS15                                                           
*                                                                               
         MVC   LTYPE,=C'MG+ '                                                   
         CLI   MODE,CBHLAST        TEST CABLE                                   
         BNE   MGAS12                                                           
         MVI   LTYPE,C'+'                                                       
         MVC   LTYPE+1(3),MGESTA       MOVE NETWORK                             
*                                                                               
MGAS12   L     R1,MGERTGB                                                       
         LA    R5,LMGRTG                                                        
         BAS   RE,EDTMGDEM                                                      
*                                                                               
         L     R1,MGERTGB                                                       
         A     R1,MGA2CNT+16                                                    
         ST    R1,MGA2CNT+16                                                    
         B     MGAS20                                                           
*                                                                               
MGAS15   MVC   LTYPE(5),=C'TOTAL'                                               
*                                                                               
MGAS20   EDIT  MGELINE,(3,LLINE),FILL=0                                         
*                                                                               
         EDIT  (B1,MGEUSER),(3,LEST),FILL=0                                     
         MVI   LDASH,C'-'                                                       
         CLC   MGEDATE,=X'FFFF'                                                 
         BNE   MGAS22                                                           
         MVC   LDATE(8),=C'*MISSED*'                                            
         B     MGAS60                                                           
*                                                                               
MGAS22   DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,MGEDATE),(4,LDATE)                                
         GOTO1 UNTIME,DMCB,MGETIME,LTIME,0                                      
         CLI   MGESPNUM,1                                                       
         BE    MGAS30                                                           
         MVI   LDATE+5,C'-'                                                     
         EDIT  MGESPNUM,(2,LDATE+6),FILL=0                                      
*                                                                               
MGAS30   EDIT  MGESLN,(3,LSLN)                                                  
         MVC   MGA2CNT(2),MGECODE                                               
*                                                                               
         LA    RE,MGA2CNT+4        POINT TO TOTAL MISSED                        
         LA    R5,LMSCOST          AND PRINT POSITION                           
         CLI   MGETYPE,0                                                        
         BE    *+12                                                             
         LA    RE,MGA2CNT+8        POINT TO TOTAL MADEGOOD                      
         LA    R5,LMGCOST                                                       
*                                                                               
         L     RF,0(RE)                                                         
         A     RF,MGECOST                                                       
         ST    RF,0(RE)                                                         
*                                                                               
         L     R1,MGECOST                                                       
         BAS   RE,EDTMGCOS                                                      
*                                                                               
MGAS50   MVC   LPGMNM,MGEPGMNM       PROGRAM                                    
         OC    MGESTA,MGESTA                                                    
         BZ    MGAS60                                                           
         CLI   MODE,CBHLAST        TEST CABLE                                   
         BE    MGAS60                                                           
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MGESTA                                                 
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+10,WORK+15                         
         MVC   LSTAR,=C'**'                                                     
         MVC   LSTA,WORK+15                                                     
*                                                                               
MGAS60   GOTO1 REPORT                                                           
         LA    R4,MGERECL(R4)                                                   
         B     MGAS04                                                           
*                                                                               
MGASX    MVC   LTIME(13),=C'*GRAND TOTAL*'  TOTALS                              
*                                                                               
         L     R1,MGATCNT+4                                                     
         LA    R5,LMSCOST                                                       
         BAS   RE,EDTMGCOS                                                      
*                                                                               
         L     R1,MGATCNT+8                                                     
         LA    R5,LMGCOST                                                       
         BAS   RE,EDTMGCOS                                                      
*                                                                               
         L     R1,MGATCNT+12                                                    
         LA    R5,LMSRTG                                                        
         BAS   RE,EDTMGTOT                                                      
*                                                                               
         L     R1,MGATCNT+16                                                    
         LA    R5,LMGRTG                                                        
         BAS   RE,EDTMGTOT                                                      
*                                                                               
         GOTO1 REPORT                                                           
         XC    MGATCNT,MGATCNT                                                  
*                                                                               
         MVC   RTYPE,SVRTYPE                                                    
         MVI   FORCEHED,C'Y'                                                    
         L     RE,=A(MGTABST)                                                   
         L     RF,=A(MGTABSTX-MGTABST)                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   SVCBLLIN,SPACES     CLEAR HDND AND NAME                          
         MVI   LINE,99             FORCE NEW PAGE WITH FORCEHED                 
*                                                                               
MGASX10  XIT1                                                                   
*                                                                               
EDTMGDEM L     RF,=A(MGABLK)                                                    
         USING MGABLKD,RF                                                       
         L     RF,MGADEM                                                        
         DROP  RF                                                               
         CLI   1(RF),C'R'                                                       
         BE    *+12                                                             
         CLI   1(RF),C'E'                                                       
         BNE   EDTMGDE2                                                         
         TM    RQOPTS,RQOPTS_2DEC     TEST 2-DEC RTGS ACTIVE                    
         BO    EDTMGDE4                                                         
         B     EDTMGDE6                                                         
*                                                                               
EDTMGDE2 TM    RQOPTS,RQOPTS_2DECIMP  TEST 2-DEC IMPS ACTIVE                    
         BO    EDTMGDE6                                                         
*                                                                               
EDTMGDE4 EDIT  (R1),(5,(R5)),1                                                  
         BR    RE                                                               
*                                                                               
EDTMGDE6 EDIT  MGERTGB,(5,(R5)),2                                               
         BR    RE                                                               
                                                                                
*============================================================                   
* ROUND RATING TOTAL IN R1 TO ONE DECIMAL AND FORMAT TO PRINT                   
*============================================================                   
                                                                                
EDTMGTOT L     RF,=A(MGABLK)                                                    
         USING MGABLKD,RF                                                       
         L     RF,MGADEM                                                        
         DROP  RF                                                               
         CLI   1(RF),C'R'                                                       
         BE    *+12                                                             
         CLI   1(RF),C'E'                                                       
         BNE   EDTMGT02                                                         
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC RTGS ACTIVE                       
         BZ    EDTMGT06                                                         
         B     EDTMGT04                                                         
*                                                                               
EDTMGT02 TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC IMPS ACTIVE                       
         JZ    EDTMGT06                                                         
*                                                                               
EDTMGT04 M     R0,=F'2'            ROUND TO 1-DEC                               
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
EDTMGT06 EDIT  (R1),(5,(R5)),1                                                  
         BR    RE                                                               
*                                                                               
EDTMGCOS M     R0,=F'2'            ROUND TO DROP PENNIES                        
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(7,0(R5)),FLOAT=$                                           
         BR    RE                                                               
         DROP  R4,R8                                                            
*                                                                               
SVRTYPE  DS    CL3                                                              
         DS    0F                                                               
MGA2CNT  DS    CL20                                                             
MGATCNT  DS    CL20                                                             
DEMDISP  EQU   28                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
PRTSTCOM NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SPD2WK,R2                                                        
*                                                                               
         CLC   QPROG,=C'D2'                                                     
         BE    PRTST2                                                           
         CLC   QPROG,=C'D4'                                                     
         BE    PRTST2                                                           
         CLC   QPROG,=C'D7'                                                     
         BNE   PRTST2                                                           
         CLC   QPROG,=C'DX'                                                     
         BNE   PRTST2                                                           
         B     PRTSTX                                                           
*                                                                               
PRTST2   CLI   Q2SCOM,C'Y'         ONLY PRINT BY REQUEST                        
         BNE   PRTSTX                                                           
*                                                                               
         MVI   BCMTYPE,C'T'        SET FOR STATION COMMENTS                     
         MVC   BCMCLT,BCLT                                                      
         XC    BCMPGR,BCMPGR                                                    
         MVC   BCMPRD,BPRD                                                      
         MVC   BCMEST,BEST                                                      
         MVC   BCMMKT,BMKT                                                      
         MVC   BCMSTA,BSTA                                                      
         GOTO1 GETCOM                                                           
*                                                                               
         L     R6,ADCOMREC                                                      
         OC    0(13,R6),0(R6)      TEST COMMENTS FOUND                          
         BZ    PRTSTX                                                           
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         SR    R0,R0               AND MOVE TO PRINT LINES                      
         LA    R1,P1                                                            
         LA    R6,24(R6)           FIND THE COMMENT ELEMENTS                    
*                                                                               
PRTST10  CLI   0(R6),0                                                          
         BE    PRTST14                                                          
         CLI   0(R6),5                                                          
         BNE   PRTST12                                                          
         LLC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         BM    PRTST12                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),2(R6)                                                    
         LA    R1,132(R1)                                                       
*                                                                               
PRTST12  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PRTST10                                                          
*                                                                               
PRTST14  CLC   P1,SPACES           TEST ANYTHING TO PRINT                       
         BE    PRTST16                                                          
         MVI   0(R1),0                                                          
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PRTST16  L     R6,ADCOMREC                                                      
         XC    0(13,R6),0(R6)      CLEAR THE COMMENT AREA                       
*                                                                               
PRTSTX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
PRTMCOM  NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SPD2WK,R2                                                        
*                                                                               
         CLI   D2APROF+2,C'Y'      TEST MEDIA COMMENTS NEEDED                   
         BNE   PRTMCX                                                           
         CLC   QPROG,=C'DX'                                                     
         BNE   *+12                                                             
         CLI   QOPT1,C'D'          TEST DARE ORDER                              
         BE    PRTMCX              YES - DARE COMMENTS ONLY                     
*                                                                               
         MVI   BCMTYPE,C'M'        SET COMMENT BLOCK                            
         MVC   BCMCLT,BCLT                                                      
         XC    BCMPGR,BCMPGR                                                    
         MVC   BCMPRD,BPRD                                                      
         MVC   BCMEST,BEST                                                      
         CLI   BEST,0                                                           
         BNE   PRTMC2                                                           
         LA    R1,ESTLST                                                        
         CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         MVC   BCMEST,0(R1)                                                     
*                                                                               
PRTMC2   XC    BCMMKT,BCMMKT                                                    
         MVI   BCMSTA,0                                                         
         MVC   BCMSTA+1(2),BMKT                                                 
         GOTO1 GETCOM              GET MEDIA COMMENTS                           
*                                                                               
         L     R6,ADCOMREC                                                      
         OC    0(13,R6),0(R6)      TEST ANY MEDIA COMMENTS NEEDED               
         BZ    PRTMCX                                                           
         LA    R6,24(R6)           YES-FIND THE COMMENT ELEMENTS                
         SR    R0,R0                   AND MOVE TO PRINT LINES                  
         LA    R1,P2                                                            
         LA    RF,10               MAX 10 COMMENTS                              
*                                                                               
PRTMC4   CLI   0(R6),0                                                          
         BE    PRTMC8                                                           
         CLI   0(R6),5                                                          
         BNE   PRTMC6                                                           
         LLC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         BM    PRTMC6                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),2(R6)                                                    
         LA    R1,132(R1)                                                       
         BCT   RF,PRTMC6                                                        
         B     PRTMC8                                                           
*                                                                               
PRTMC6   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PRTMC4                                                           
*                                                                               
PRTMC8   CLC   P2,SPACES           PRINT THE MEDIA COMMENTS                     
         BE    PRTMCX                                                           
         MVI   P1,0                                                             
         MVI   0(R1),0                                                          
         GOTO1 REPORT                                                           
*                                                                               
PRTMCX   L     R6,ADCOMREC                                                      
         XC    0(13,R6),0(R6)                                                   
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* THIS ROUTINE IS CALLED THE FIRST TIME THERE IS BUY ACTIVITY FOR  *            
* A STATION FOR THE DX REPORT, AND ALSO TO CLOSE A FAX REPORT      *            
*===================================================================            
         SPACE 1                                                                
DXFAX    NTR1  BASE=*,LABEL=*                                                   
         CLC   QPROG,=C'DX'                                                     
         BNE   DXFAXX                                                           
         CLI   QOPT1,C'D'         TEST DARE ORDER                               
         BNE   DXFAX1                                                           
*                                                                               
         L     R6,=A(DAREIO)                                                    
         MVI   ELCODE,X'50'        FIND DOWIGEL                                 
         BRAS  RE,GETEL                                                         
         BE    DXFN                YES - USE NEW SEND LOGIC                     
*                                                                               
DXFAX1   L     R4,VFAXINFO         ADDRESS FAXLINK INFO BLOCK                   
         USING FAXINFOD,R4                                                      
*                                                                               
         CLI   QOPT1,C'D'          TEST DARE FAX                                
         BE    DXFAX4                                                           
*                                                                               
         TM    DXSW,DXOPEN         TEST BEEN HERE ONCE BEFORE                   
         BZ    DXFAX2                                                           
         MVI   FXISTAT,FXISCLOS    YES-CALL FAXLINK TO CLOSE FOR LAST           
         GOTO1 VFAXLINK                STATION                                  
*                                                                               
DXFAX2   OI    DXSW,DXOPEN                                                      
*                                                                               
DXFAX4   NI    DXSW,255-DXNEWSTA                                                
         XC    FAXINFOD(FXILEN),FAXINFOD   SET UP FAXINFO BLOCK                 
         CLI   QOPT1,C'D'                                                       
         BE    *+8                                                              
         MVI   FXISTAT,FXISPEND            PENDING INITIALIZATION               
         L     R1,VMASTC                                                        
         MVC   FXISIDNO,MCORIGID-MCBLOCK(R1)   ORIGINATING ID                   
         MVC   FXISAGY,QAGY                                                     
         MVC   FXISFXCD,SPACES                                                  
         MVC   FXISFXCD(5),STA     FAX REC CODE = STATION CALL LETTERS          
         CLI   STA+3,C' '                                                       
         BH    *+16                                                             
         CLI   STA+4,C' '                                                       
         BNH   *+8                                                              
         MVI   FXISFXCD+3,C'-'                                                  
         MVC   FXISRQST,SVQUEST    REQUESTOR CODE                               
         MVC   FXISTN,STA          DESTINATION IS STATION                       
         CLI   STA+4,C' '                                                       
         BH    *+8                                                              
         MVI   FXISTN+4,C'T'                                                    
         CLI   STA+3,C' '                                                       
         BH    *+8                                                              
         MVI   FXISTN+3,C'9'                                                    
         CLI   FXISTN,C'0'         TEST CABLE                                   
         BL    *+14                                                             
         MVI   FXISTN,C'T'         YES-MAKE IT TNNNN                            
         MVC   FXISTN+1(4),STA                                                  
         OI    FXIOPTS,FXIOEASY+FXIOSUBC+FXIOSPLT                               
*                                                                               
         MVC   FXISSUBC(3),CLT      USE CLIENT CODE AS SUBCODE                  
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVI   FXISSUBC+3,C'*'                                                  
         MVC   FXISSUBC+4(1),COFFICE                                            
         DROP  R6                                                               
         CLI   FXPROF+0,C'Y'       TEST FAX BY OFFICE/OFFICELIST                
         BNE   DXFAX6                                                           
         MVC   FXISSUBC(3),FXISSUBC+3  MOVE OFFICE TO PRIMARY                   
         MVI   FXISSUBC+3,C'$'                                                  
         MVC   FXISSUBC+4(1),FXPROF+1 SET OFFICE LIST                           
*                                                                               
DXFAX6   OC    FXISSUBC,SPACES                                                  
         MVC   FXITRN(14),DXTRAN                                                
         LA    R1,FXITRAPP         APPLICATION TRANSACTION DATA                 
         BAS   RE,DXFMTAPP                                                      
         LA    R1,FXIEBILL         EASYLINK BILLING INFORMATION                 
         MVC   0(1,R1),QMED                                                     
         MVC   1(3,R1),QCLT                                                     
*                                                                               
         CLI   QOPT1,C'D'          TEST DARE FAX                                
         BE    DXFAX8                                                           
*                                                                               
         GOTO1 VFAXLINK            CALL FAXLINK                                 
         CLI   FXISTAT,FXISACT     TEST FAX RECORD FOUND                        
         BE    DXFAXX              YES-DONE                                     
*                                                                               
         MVI   FXISTAT,FXISPEND    PENDING INITIALIZATION                       
         NI    FXIOPTS,X'FF'-FXIOSUBC   TRY FINDING W/O SUB-CODE                
         GOTO1 VFAXLINK            CALL FAXLINK                                 
         CLI   FXISTAT,FXISACT     TEST FAX RECORD FOUND                        
         BE    DXFAXX              YES-DONE                                     
         MVI   FXISTAT,FXISACT     NO-MARK ACTIVE SO CLOSE WILL OCCUR           
         B     DXFAX10                                                          
*                                                                               
DXFAX8   CLI   FXPROF+2,C'Y'       TEST TRY TO EMAIL                            
         BNE   DXFAX10             NO                                           
*                                                                               
         BRAS  RE,DXCHKEM          SEE IF EMAIL ADDRESS IN STA ADDR             
         CLI   DXEMAIL,C'Y'                                                     
         BNE   DXFAX10                                                          
*                                                                               
         LA    R1,P                                                             
         MVC   4(15,R1),=C'*HDR*EDICT=*BDE'                                     
         B     DXFAX14                                                          
*                                                                               
DXFAX10  LA    R1,P                AND WE'LL DO IT OURSELVES                    
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(5,R1),STA         EASYLINK'S STATION DESTINATION               
         CLI   STA+4,C' '                                                       
         BH    *+8                                                              
         MVI   13(R1),C'T'                                                      
*                                                                               
         CLI   QOPT1,C'D'          DARE FAXING?                                 
         BE    DXFAX12             YES, DON'T WORRY ABOUT 3 LETTER STA          
         CLI   STA+3,C' '                                                       
         BH    *+8                                                              
         MVI   12(R1),C'9'                                                      
*                                                                               
DXFAX12  CLI   STA,C'0'                 TEST CABLE                              
         BL    *+14                                                             
         MVI   9(R1),C'T'               YES-MAKE IT TNNNN                       
         MVC   10(4,R1),STA                                                     
*                                                                               
DXFAX14  MVI   34(R1),C'W'              132 CHARS WIDE                          
         MVI   35(R1),C'P'              PAGE BREAKS                             
         MVC   38(L'BIGSTA,R1),BIGSTA   FORMATTED DESTINATION NAME              
* SPECIAL FOR DARE                                                              
         CLI   QOPT1,C'D'                                                       
         BNE   *+8                                                              
         MVI   67(R1),C'D'                                                      
*                                                                               
         LA    R1,54(R1)                                                        
         MVC   0(1,R1),QMED                                                     
         MVC   1(3,R1),QCLT                                                     
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         MVI   LINE,0                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(14),DXTRAN                                                     
         LA    R1,P+15                                                          
         BAS   RE,DXFMTAPP                                                      
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QOPT1,C'D'                                                       
         BNE   DXFAX15                                                          
         MVC   P(14),=C'++DDS      DXC'                                         
         MVC   P+15(6),FXISSUBC    DARE FAX SUBCODES                            
         GOTO1 REPORT                                                           
*                                                                               
DXFAX15  CLI   DXEMAIL,C'Y'                                                     
         BNE   DXFAX20                                                          
         MVC   P(14),=C'++DDS      RCP'                                         
         L     R6,ADSTATAD                                                      
         USING ADDRRECD,R6                                                      
         MVC   P+15(40),ADREMAIL                                                
         DROP  R6                                                               
         GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,DXFMTORD        FORMAT ++DDS /SUB AND ++DDS /FIL              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
DXFAX20  MVI   SKIPSPEC,C'N'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
         CLI   QOPT1,C'D'          TEST DARE ORDER                              
         BNE   DXFAXX                                                           
         L     RE,=A(DAREIO)                                                    
         OC    0(13,RE),0(RE)      TEST READ ORDER RECORD                       
         BZ    *+8                 NO - THEN NO COMMENTS                        
         BRAS  RE,DARCMT                                                        
         B     DXFAXX                                                           
         EJECT                                                                  
*===================================================================            
* THIS ROUTINE IS USED FOR NEW FAX/EMAIL REPORTS BY ORDER MANAGER  *            
* ON ENTRY R6 POINTS TO WIGEL                                                   
*===================================================================            
         SPACE 1                                                                
         USING DOWIGELD,R6                                                      
DXFN     NI    DXSW,255-DXNEWSTA        TURN OFF NEW STATION FLAG               
         MVI   DXFNSW,C'Y'         SET TO SAVE DATE STUFF                       
*                                                                               
         LA    R1,P                                                             
         MVC   4(15,R1),=C'*HDR*EDICT=*BDE'                                     
         CLI   DOWIGMTH,C'E'            TEST EMAIL                              
         BE    DXFN1                                                            
         MVC   9(4,R1),=C'FAX '         OVERWRITE EDICT=                        
         MVC   13(16,R1),DOWIGFXN        AND THEN THE FAX NUMBER                
         OC    13(16,R1),SPACES                                                 
*                                                                               
DXFN1    MVI   34(R1),C'W'              132 CHARS WIDE                          
         MVI   35(R1),C'P'              PAGE BREAKS                             
         MVC   38(L'BIGSTA,R1),BIGSTA   FORMATTED DESTINATION NAME              
         MVI   67(R1),C'D'              THE D FOR YI IF E                       
         MVI   70(R1),C'N'              SUPPRESS EDICT COVER PAGE               
*                                                                               
         LA    R1,54(R1)                ADD BILLING INFO                        
         MVC   0(1,R1),QMED                                                     
         MVC   1(3,R1),QCLT                                                     
*                                                                               
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         MVI   LINE,0                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(14),DXTRAN                                                     
         LA    R1,P+15                                                          
         BAS   RE,DXFMTAPP                                                      
         OC    P,SPACES                                                         
         GOTO1 REPORT                                                           
*                                                                               
         CLI   DOWIGMTH,C'E'       TEST EMAIL                                   
         BNE   DXFN2                                                            
         MVC   P(14),=C'++DDS      RCP'                                         
         MVC   P+15(40),DOWIGEML   MOVE EMAIL ADDRESS                           
         OC    P,SPACES                                                         
         GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,DXFMTORD                                                      
         OC    P,SPACES                                                         
         GOTO1 REPORT                                                           
         EJECT                                                                  
*================================================================*              
* NOW PRINT DARE COVER PAGE AND COMMENTS                                        
*                                                                               
*                                                                               
* -- COVER SHEET - RECEIVER DATA --                                             
*    STATION CALL LETTERS                                                       
*    STATION FAX NUMBER (FROM OM LAST METHOD)                                   
*    ATTENTION  (FROM OM LAST METHOD OR ORDER)                                  
*    TEL NUMBER (FROM OM DESTIN RECORD)                                         
*                                                                               
* -- COVER SHEET SENDER DATA --                                                 
*    BUYER NAME (FROM OM BUYER REC)/AGENCY/AGENCY ADDR                          
*    ORIGINATING ID                                                             
*    TEL NUMBER   "  "                                                          
*    FAX NUMBER   "  "                                                          
*    EMAIL ADDR   "  "                                                          
*    STD COMMENT  "  "                                                          
*    ORD COMMENT  "  "                                                          
*                                                                               
* -- DX REPORT --                                                               
*    OCOM COMMENTS (VIA SPGETOMCOM)                                             
*    ORDER STD COMMENTS                                                         
*    ORDER COMMENTS                                                             
*================================================================*              
         SPACE 1                                                                
DXFN2    MVI   SKIPSPEC,C'N'                                                    
         MVI   LINE,0                                                           
*                                                                               
         MVC   P1+33(10),=C'COVER PAGE'                                         
         MVC   P2+33(10),=C'----- ----'                                         
         GOTO1 REPORT                                                           
*                                                                               
         LA    R4,P1                                                            
         MVC   20(8,R4),=C'SENT TO:'                                            
         MVC   33(8,R4),BIGSTA                                                  
*                                                                               
         LA    R4,132(R4)                                                       
         CLI   DOWIGMTH,C'E'       TEST EMAIL                                   
         BNE   DXFN4                                                            
         MVC   33(64,R4),DOWIGEML                                               
         OC    P,SPACES                                                         
         B     DXFN6                                                            
*                                                                               
DXFN4    MVC   33(12,R4),DOWIGFXN   FAX NUMBER                                  
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   33(24,R4),DOWIGFXA   FAX ATTN NAME                               
         OC    0(132,R4),SPACES                                                 
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   33(12,R4),DOWIGFXT   TELEPHONE NUMBER                            
         OC    0(132,R4),SPACES                                                 
*                                                                               
DXFN6    GOTO1 REPORT                                                           
         MVI   LINE,0                                                           
         EJECT                                                                  
K        USING BYRKEY,KEY                                                       
*                                                                               
         L     R6,=A(DAREIO)       POINT TO ORDER RECORD                        
         LA    R6,24(R6)           POINT TO ID ELEMENT                          
         USING DOIDELD,R6                                                       
         CLI   0(R6),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*=========================================================                      
* READ BUYER RECORD INTO ADBUY                                                  
*=========================================================                      
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D31'                                                  
         MVC   K.BYRKAM,BAGYMD                                                  
         MVC   K.BYRKBYR,DOIDBYR                                                
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BYRDSCD,R6                                                       
*                                                                               
         LA    R4,P1                                                            
         MVC   20(10,R4),=C'SENT FROM:'                                         
         MVC   33(24,R4),BYRFNAME                                               
*                                                                               
         MVC   60(4,R4),=C'(ID='                                                
         L     RE,VMASTC                                                        
         USING MASTD,RE                                                         
         MVC   64(10,R4),MCUSERID                                               
         DROP  RE                                                               
*                                                                               
         LA    R1,74(R4)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C')'                                                       
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   36(33,R4),AGYNM     INDENT AGENCY SLIGHTLY                       
         OC    0(132,R4),SPACES                                                 
         LA    R4,132(R4)                                                       
         MVC   36(33,R4),AGYADR                                                 
         OC    0(132,R4),SPACES                                                 
*                                                                               
         LA    R4,132(R4)                                                       
         MVC   20(10,R4),=C'TELEPHONE:'                                         
         MVC   33(12,R4),BYRPHONE   BUYER TELEPHONE                             
*                                                                               
         L     R6,=A(DAREIO)                                                    
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL            FIND TRANSMISSION ELEMENT                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING DOXMTELD,R6                                                      
*                                                                               
         LA    R4,P5                                                            
         MVC   20(8,R4),=C'SENT ON:'                                            
         GOTO1 DATCON,DMCB,(8,DOXMTYMD),(8,33(R4))                              
*                                                                               
         LA    R4,P6                                                            
         MVC   20(8,R4),=C'SENT AT:'                                            
**WRONG! XC    DUB,DUB                                                          
**WRONG! MVC   DUB(2),DOXMTTIM                                                  
**WRONG! GOTO1 UNTIME,DMCB,DUB,33(R4)                                           
         GOTO1 HEXOUT,DMCB,DOXMTTIM,DUB,2,=C'TOG'                               
         MVC   33(2,R4),DUB                                                     
         MVI   35(R4),C'.'                                                      
         MVC   36(2,R4),DUB+2                                                   
         MVI   132(R4),0           FORCE A BLANK LINE AFTER                     
         GOTO1 REPORT                                                           
         MVI   LINE,0                                                           
         DROP  R6                                                               
         EJECT                                                                  
*===========================================================                    
* NOW PRINT BUYER COMMENTS                                                      
*===========================================================                    
         SPACE 1                                                                
         L     R6,ADBUY            POINT TO BUYER RECORD                        
         MVI   ELCODE,X'15'        FIND BYRCMTD                                 
         BRAS  RE,GETEL                                                         
         BNE   DXFN14                                                           
*                                                                               
         USING BYRCMTD,R6                                                       
*                                                                               
         CLI   BYRSCMNT,C' '       TEST STANDARD COMMENT PRESENT                
         BNH   DXFN10                                                           
         LA    R1,BYRSCMNT                                                      
         BAS   RE,DARSTD           READ STANDARD COMMENT                        
*                                                                               
         MVI   ELCODE,X'10'        STANDARD COMMENT TEXT ELEM                   
         BAS   RE,DARPRT           PRINT STANDARD COMMENT                       
*                                                                               
DXFN10   MVC   P(70),BYROCMT1                                                   
         CLC   P(70),SPACES        TEST FOR A BUYER COMMENT                     
         BNH   DXFN12                                                           
         OC    P,SPACES                                                         
         GOTO1 REPORT                                                           
*                                                                               
DXFN12   MVC   P(70),BYROCMT2                                                   
         CLC   P(70),SPACES                                                     
         BNH   DXFN14                                                           
         OC    P,SPACES                                                         
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
DXFN14   OC    DARECON,DARECON     TEST CONTRACT PRESENT                        
         BZ    DXFN20                                                           
*                                                                               
         MVC   P(46),=C'*** ORDER PREVIOUSLY SET TO REP FIRM, CONTRACT'         
         MVC   P+48(8),DARECON                                                  
         MVC   P+57(3),=C'***'                                                  
         GOTO1 REPORT                                                           
         EJECT                                                                  
*===========================================================                    
* BUILD A BUFFER OF OCOMS THAT APPLY TO THIS MD/CLT/PRD/EST                     
* ALL OF THESE COMMENTS PRINT AFTER THE REPORT HEADLINES                        
*===========================================================                    
         SPACE 1                                                                
DXFN20   MVI   FORCEHED,C'Y'       START A NEW PAGE                             
*                                                                               
         L     R6,=A(DAREIO)       POINT TO ORDER RECORD                        
         LA    R6,24(R6)                                                        
         USING DOIDELD,R6                                                       
*                                                                               
         XC    MYBUFIO,MYBUFIO                                                  
         LA    R1,MYBUFIO                                                       
         USING OMCOMD,R1                                                        
*                                                                               
         MVI   OMCACT,C'G'                                                      
         MVC   OMCLBUFF,=H'6100'                                                
         MVC   OMCABUFF,ADBUY      OVERWRITE ADBUY                              
         MVC   OMCACOMF,ACOMFACS                                                
         MVC   OMCAGMD,BAGYMD                                                   
         MVC   OMCCLT,BCLT                                                      
         MVC   OMCPRD,BPRD                                                      
         MVC   OMCEST,BEST                                                      
         MVC   OMCFLT,DOIDFLTN                                                  
*                                                                               
         GOTO1 =V(SPOMCOM),(R1)                                                 
*                                                                               
         OC    OMCCOUNT,OMCCOUNT   TEST ANY COMMENTS                            
         BZ    DXFN30                                                           
*                                                                               
         L     R4,ADBUY                                                         
         LH    R5,OMCCOUNT                                                      
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
DXFN22   MVC   P(72),8(R4)         MOVE TEXT                                    
         MVC   P+73(8),0(R4)       AND SOURCE LEVEL                             
*                                                                               
         CLC   0(8,R4),80(R4)      TEST NEXT COM HAS SAME SOURCE                
         BE    *+8                                                              
         MVI   P2,0                ELSE FORCE BLANK LINE                        
         GOTO1 REPORT                                                           
**NOP**  MVI   LINE,0                                                           
*                                                                               
         LA    R4,80(R4)                                                        
         BCT   R5,DXFN22                                                        
*                                                                               
DXFN30   LA    R1,DOISCOM1         STANDARD COMMENT 1                           
         LA    R0,3                MAX 3 STANDARD COMMENTS                      
         MVI   ELCODE,X'10'        STANDARD COMMENT TEXT ELEM                   
*                                                                               
DXFN40   CLI   0(R1),C' '                                                       
         BNH   DXFN42                                                           
*                                                                               
         BAS   RE,DARSTD           READ STANDARD COMMENT                        
         BAS   RE,DARPRT           PRINT STANDARD COMMENT                       
*                                                                               
DXFN42   LA    R1,8(R1)                                                         
         BCT   R0,DXFN40                                                        
         SPACE 1                                                                
*============================================================                   
* NOW PRINT ORDER COMMENTS                                                      
*============================================================                   
         SPACE 1                                                                
         L     R6,=A(DAREIO)       POINT TO ORDER RECORD                        
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)       MOVE ORDER KEY                               
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         MVI   DOKCMT,X'01'        SET FOR COMMENT RECORD                       
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND ANY COMMENTS                      
         BNE   DXFAXX                                                           
*                                                                               
         MVI   ELCODE,X'30'        ORDER COMMENT ELCODE                         
         BAS   RE,DARPRT           DO GETREC AND PRINT                          
*                                                                               
DXFAXX   CLI   DXFNSW,C'Y'         TEST NEW DARE DX                             
         BNE   DXFAXX4                                                          
         CLI   DARESVSW,C'Y'       TEST SAVED HEADLINES                         
         BNE   DXFAXX4                                                          
*                                                                               
         L     RE,=A(DAREHDSV)     GET SAVED DATES/TITLES                       
         LA    R1,P                PRINT A BLANK LINE TO PRINT SAVE             
         LHI   R0,DAREHDLQ                                                      
*                                                                               
DXFAXX2  OC    0(132,R1),0(RE)     DID I JUST MOVE A SPACE?                     
         BE    DXFAXX3             YES                                          
         MVC   0(132,RE),SPACES                                                 
         AHI   R1,132                                                           
DXFAXX3  AHI   RE,132                                                           
         BCT   R0,DXFAXX2                                                       
*                                                                               
         CLC   0(L'P1,R1),SPACES                                                
         BE    *+8                                                              
         AHI   R1,L'P1                                                          
*                                                                               
         MVI   0(R1),0             FORCE SPACE                                  
         AHI   R1,L'P1                                                          
*                                                                               
         MVC   0(L'P1,R1),0(RE)                                                 
         MVC   0(132,RE),SPACES    PRINT THE SAVE MIDLINE                       
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
DXFAXX4  MVC   KEY,SVBUYKEY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DXFAXX6             MUST BE DX WITH NO BUYS                      
         GOTO1 GETBUY                                                           
         BRAS  RE,FIXCOVRD                                                      
*                                                                               
DXFAXX6  MVI   DXFNSW,C'N'         NO SPECIAL PRINTING AFTER PAGE 1             
         MVI   DARESVSW,C'N'                                                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* FORMAT ++DDS   SUB                                                            
*        ++DDS   FIL                                                            
*=============================================================                  
         SPACE 1                                                                
DXFMTORD NTR1                                                                   
         MVC   P(14),=C'++DDS      SUB'                                         
         MVC   P+15(5),=C'ORDER'                                                
*                                                                               
         LA    RE,P+21                                                          
         MVC   0(3,RE),QCLT                                                     
         LA    RE,3(RE)                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         MVC   2(3,RE),QPRD                                                     
         LA    RE,5(RE)                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(3,RE),DUB                                                      
*                                                                               
         LA    RE,6(RE)                                                         
         ST    RE,DMCB+4           SET OUTPUT DATE POSN                         
         MVI   DMCB+4,11           SET TYPE=MMMDD/YY                            
         GOTO1 DATCON,DMCB,QSTART                                               
         L     RE,DMCB+4           POINT TO THE OUTPUT                          
         ICM   RF,3,6(RE)          GET THE YY                                   
         MVC   5(2,RE),=C'20'      OVERWRITE THE /                              
         STCM  RF,3,7(RE)                                                       
*                                                                               
         MVC   P2(14),=C'++DDS      FIL'                                        
*                                                                               
         MVC   P2+15(33),AGYNM                                                  
         LA    RE,P2+15                                                         
         CLC   AGYNM,SPACES                                                     
         BE    DXFMT02                                                          
*                                                                               
         AHI   RE,33                                                            
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
DXFMT02  MVI   1(RE),C'-'                                                       
         MVC   2(3,RE),SVQUEST                                                  
*                                                                               
         MVI   5(RE),C'{'                                                       
         MVC   6(30,RE),P+21       PRINT CLT/PRD/EST/DATE AGAIN !               
         XIT1                                                                   
         EJECT                                                                  
*=============================================================                  
* FORMAT APPL PART OF ++DDS REC RECORD                                          
*=============================================================                  
         SPACE 1                                                                
DXFMTAPP DS    0H                  FORMAT APPLICATION PART OF ++DDS REC         
         USING SPEDICTD,R1                                                      
         MVI   SPCPTYPE,SPCPDATQ                                                
         MVC   SPCPMED,MED                                                      
         MVC   SPCPCLT,CLT                                                      
         MVC   SPCPPRD,PRD                                                      
         MVC   SPCPEST,EST                                                      
         MVC   SPCPMKT,MKT                                                      
         MVC   SPCPSTA,STA                                                      
         MVC   SPCPRQST,SVQUEST                                                 
         MVC   SPCDRORD,Q2DARORD                                                
         LA    R1,SPCDRUSR                                                      
         USING RTN2SNDR,R1                                                      
         MVC   RTNSYSID,Q2DARSID                                                
         MVC   RTNPWRCD,QAGY                                                    
         MVC   RTNAGYMD,Q2DARAM                                                 
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 2                                                                
DXTRAN   DC    CL14'++DDS SPXDXTRN'                                             
DXEMAIL  DC    C'N'                                                             
         EJECT                                                                  
*===============================================================                
* READ STATION ADDRESS RECORD TO CHECK FOR EMAIL ADDRESS                        
* THIS DOES NOT APPLY TO ORDER MANAGER                                          
*===============================================================                
         SPACE 1                                                                
DXCHKEM  NTR1                                                                   
         MVI   DXEMAIL,C'N'        RESET SWITCH                                 
         XC    KEY,KEY                                                          
         MVI   KEY,C'A'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),STA                                                     
         CLI   KEY+6,C' '                                                       
         BH    *+8                                                              
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),QAGY                                                    
         GOTO1 HIGHSTAD                                                         
         L     R6,ADSTATAD                                                      
         USING ADDRRECD,R6                                                      
         CLC   KEY(9),0(R6)                                                     
         BNE   DXCHKEMX                                                         
         CLI   ADREMAIL,C' '                                                    
         BNH   *+8                                                              
         MVI   DXEMAIL,C'Y'                                                     
DXCHKEMX XIT1                                                                   
         DROP R6                                                                
         EJECT                                                                  
       ++INCLUDE DDFAXINFOD                                                     
*                                                                               
SPD202   CSECT                                                                  
         EJECT                                                                  
*================================================================*              
* PRINT DARE STANDARD COMMENTS AND ORDER COMMENTS                               
* THIS IS **NOT** USED BY ORDER MANAGER DX REPORT                               
*================================================================*              
         SPACE 1                                                                
DARCMT   NTR1  BASE=*,LABEL=*                                                   
*&&DO                                                                           
         MVC   P(40),=C'*******   DARE ORDER 12345678   ********'               
         MVC   P+21(8),Q2DARORD                                                 
         MVI   P2,0                AND SKIP A LINE                              
*&&                                                                             
         OC    DARECON,DARECON     TEST CONTRACT PRESENT                        
         BZ    DARCMT2                                                          
*                                                                               
         MVC   P3(36),=C'*** ORDER PREVIOUSLY SET TO REP FIRM'                  
         MVC   P3+37(10),=C' CONTRACT '                                         
         MVC   P3+48(8),DARECON                                                 
         MVC   P3+57(3),=C'***'                                                 
         MVI   P4,0                AND SKIP A LINE                              
*                                                                               
DARCMT2  GOTO1 REPORT                                                           
*                                                                               
         L     R6,=A(DAREIO)                                                    
         LA    R6,24(R6)           POINT TO ID ELEMENT                          
         USING DOIDELD,R6                                                       
         CLI   0(R6),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,DOISCOM1         STANDARD COMMENT 1                           
         LA    R0,3                MAX 3 STANDARD COMMENTS                      
         MVI   ELCODE,X'10'        STANDARD COMMENT TEXT ELEM                   
DARCMT10 CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         BAS   RE,DARSTD           READ STANDARD COMMENT RECORD                 
         BAS   RE,DARPRT           DO GETREC AND PRINT                          
         LA    R1,8(R1)                                                         
         BCT   R0,DARCMT10                                                      
*********                                                                       
* NOW PRINT ORDER COMMENTS                                                      
*********                                                                       
         L     R6,ADBUY            POINT TO ORDER RECORD                        
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)       MOVE ORDER KEY                               
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         MVI   DOKCMT,X'01'        SET FOR COMMENT RECORD                       
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND ANY COMMENTS                      
         BNE   DARCMTX                                                          
*                                                                               
         MVI   ELCODE,X'30'        ORDER COMMENT ELCODE                         
         BAS   RE,DARPRT           DO GETREC AND PRINT                          
*                                                                               
DARCMTX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* READ STANDARD COMMENT RECORD                                                  
*================================================================               
         SPACE 1                                                                
DARSTD   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING COMRECD,R6                                                       
         MVI   COMKTYP,X'0D'                                                    
         MVI   COMKSUB,X'33'                                                    
         MVC   COMKAM,BAGYMD                                                    
         MVC   COMKCOM,0(R1)                                                    
         OC    COMKCOM,=CL8' '     SHOULDN'T NEED THIS !                        
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* PRINT A COMMENT RECORD. ELCODE HAS TEXT ELEMENT ID                            
*===============================================================                
         SPACE 2                                                                
DARPRT   NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADBUY            READ COMMENT TO ADBUY+ 2000                  
         LA    R6,2000(R6)                                                      
         ST    R6,AREC                                                          
         MVI   ALLOWLIN,8          ALLOW 8 LINES WITHOUT A BREAK                
**NOP**  MVI   LINE,0              SUPPRESS CHANGE OF PAGE                      
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DARPRT2  BRAS  RE,NEXTEL                                                        
         BNE   DARPRTX                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-4               ADJUST FOR OVERHEAD                          
         BM    DARPRT2                                                          
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),3(R6)                                                       
         OC    P,SPACES                                                         
         GOTO1 REPORT                                                           
         B     DARPRT2                                                          
*                                                                               
DARPRTX  DS    0H                                                               
         GOTO1 REPORT              SKIP A LINE AFTER COMMENT                    
         XIT1                                                                   
         LTORG                                                                  
*===============================================================                
* READ DARE ORDER RECORD VIA PASSIVE KEY INTO DAREIO                            
*                                                                               
* CLOBBERS   FULL, WORD, & DUB                                                  
*===============================================================                
GETDRORD NTR1  BASE=*,LABEL=*                                                   
*********                                                                       
         XC    DARECON,DARECON                                                  
         XC    DRTRDDTA,DRTRDDTA                                                
         MVI   DRCSHTRD,0                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,WORD)   GET CURRENT DATE                   
         ZAP   FULL,WORD           CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,Q2DARORD,DUB,8  SAVE AS IF ENTRY WAS HEX              
         MVC   WORD,DUB            CONVERT IT TO PACK                           
         OI    WORD+3,X'0F'                                                     
         SRP   WORD,58,0       ISOLATE THE YEAR                                 
         MVC   WORD+2(1),FULL+2 COPY TODAY'S CENTURY                            
*                                                                               
         CP    WORD+3(1),FULL+3(1) LESS 10 YEARS?                               
         BNH   *+10                                                             
         SP    WORD,=P'10'         YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    WORD,=P'90'         CALCULATE FROM 1990                          
         SRP   WORD,4,0                                                         
         OC    WORD+1(2),DUB       STICK IN DAYS IN YEAR                        
         SRP   WORD,63,0                                                        
*                                                                               
         ZAP   DUB,WORD            SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=X'FFFF'                                                
*                                                                               
         PACK  DUB,Q2DARORD+4(4)   SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=X'FFFF'                                                
*********                                                                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DAREORDD,R6                                                      
         MVI   DOKTYPE,DOKTYPQ     X'0D'                                        
         MVI   DOKSUBTY,DOKSTYPQ   X'34'                                        
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER   ELIMINATES PROBLEMS WITH FLT & TRADE         
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   GETDRORX            EXIT WITH CC NEQ                             
*                                                                               
         MVC   AREC,=A(DAREIO)                                                  
         GOTO1 GET                                                              
*                                                                               
         L     R6,=A(DAREIO)       POINT TO ORDER RECORD                        
         LA    R6,24(R6)                                                        
         USING DOIDELD,R6                                                       
*                                                                               
         MVC   DARECON,DOIDCON     SAVE CONTRACT NUMBER                         
*                                                                               
         SR    R0,R0                                                            
GETDROR2 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GETDROR8                                                         
         CLI   0(R6),3             FIND DOSPELQ                                 
         BNE   GETDROR2                                                         
*                                                                               
         USING DOSPELD,R6                                                       
         CLI   DOSPTMTH,C'R'       UPPERCASE R                                  
         BE    *+12                                                             
         CLI   DOSPTMTH,C'R'-X'40' LOWERCASE R                                  
         BNE   GETDROR8                                                         
*                                                                               
         MVC   DRCSHTRD,DOSPTMTH   SAVE TRADE METHOD                            
         GOTO1 VRCPACK,DMCB,(C'P',DOSPTDAT),DRTRDDTA  SAVE PACKED REP           
         MVC   DRTRDDTA+2(3),DOSPTDAT                                           
*                                                                               
GETDROR8 CR    RB,RB               SEQ CC EQ                                    
*                                                                               
GETDRORX XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
*====================================================================*          
* SCAN BUY RECORD FOR COST OVERRIDES THAT APPLY ONLY TO COST1/COST2  *          
* SINCE THIS PROGRAM DOES **NOT** WRITE RECORDS BACK TO THE FILE     *          
* WE WILL SIMPLY RESET THE COST OVERRIDE INDICATOR AS REQUIRED       *          
*====================================================================*          
         SPACE 1                                                                
FIXCOVRD NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
FIXCOV2  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    FIXCOVX                                                          
         CLI   0(R6),X'0B'                                                      
         BL    FIXCOV2                                                          
         CLI   0(R6),X'0C'                                                      
         BH    FIXCOV2                                                          
         TM    6(R6),X'20'                                                      
         BZ    FIXCOV2                                                          
*                                                                               
         LR    R7,R6                                                            
FIXCOV4  IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'10'                                                      
         BL    FIXCOV2                                                          
         CLI   0(R7),X'13'                                                      
         BL    FIXCOV4                                                          
         BH    FIXCOV2                                                          
* X'13' ELEMENT FOUND                                                           
         TM    2(R7),X'80'         TEST COST1 OVERRIDE ONLY                     
         BZ    FIXCOV6             NO                                           
         CLI   QPWCV,C'Y'          TEST CLIENT VERSION                          
         BNE   FIXCOV2             NO - KEEP OVERRIDE                           
         NI    6(R6),X'DF'         UNSET OVERRIDE IN ORIGINAL ELEMENT           
         XC    7(3,R6),7(R6)       AND CLEAR AMOUNT                             
         B     FIXCOV2                                                          
*                                                                               
FIXCOV6  TM    2(R7),X'40'         TEST COST2 OVERRIDE ONLY                     
         BZ    FIXCOV2             NO                                           
         CLI   QCOST2,C'Y'         TEST CLIENT VERSION                          
         BE    FIXCOV2             YES - KEEP OVERRIDE                          
         NI    6(R6),X'DF'         UNSET OVERRIDE IN ORIGINAL ELEMENT           
         XC    7(3,R6),7(R6)       AND CLEAR AMOUNT                             
         B     FIXCOV2                                                          
*                                                                               
FIXCOVX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* CHECK FOR SUPERDESK AUTHORIZATIONS                                            
*=============================================================                  
         SPACE 1                                                                
CHKAUTH  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDR,R6                                                        
         TM    EFLAG1,EF1SDE       SUPERDESK AUTHORIZATION OPEN?                
         BO    CAUT04              IF YES, PROCEED TO SPAUTH CALL               
*                                  IF NO, READ POL ESTIMATE RECORD              
         LA    R6,KEY              READ ESTIMATE RECORD                         
         XC    KEY,KEY             IF CLIENT IS TRUE POL                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BEST                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BNE   CHKAUTHX                                                         
*                                                                               
         L     R6,=A(DAREIO)                                                    
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         TM    EFLAG1,EF1SDE       SDESK AUTH OPEN FOR POL ESTIMATE?            
         BNO   CHKAUTHX            NO                                           
         DROP  R6                                                               
*                                                                               
         PUSH  USING                                                            
S        USING SPAUTHD,MYBUFIO                                                  
CAUT04   XC    MYBUFIO,MYBUFIO     CALL SPAUTH                                  
         MVC   S.SPACOM,ACOMFACS                                                
         L     RF,=A(DAREIO)                                                    
         ST    RF,S.SPAIO                                                       
         MVC   S.SPAKAM,BAGYMD                                                  
         MVC   S.SPAKCLT,BCLT                                                   
         MVC   S.SPAKPRD,BPRD                                                   
         MVC   S.SPAKPRD2,BPRD2                                                 
         MVC   S.SPAKEST,BEST                                                   
         MVI   S.SPAUPDT,SPAUPSDX  INDICATE UPDATE COMING FROM DX               
*                                                                               
         CLC   QPRD2,=C'   '      ANY PIGGYBACKS?                               
         BE    CAUT06                                                           
         CLC   QPRD,QPRD2          IF PRODUCTS NOT IN ALPHABETICAL              
         BL    CAUT06              ORDER, SWAP THEM                             
         XC    S.SPAKPRD2,S.SPAKPRD                                             
         XC    S.SPAKPRD,S.SPAKPRD2                                             
         XC    S.SPAKPRD2,S.SPAKPRD                                             
*                                                                               
CAUT06   GOTO1 DATCON,DMCB,(0,QSTART),(2,S.SPASDTE)                             
         GOTO1 DATCON,DMCB,(0,QEND),(2,S.SPAEDTE)                               
*                                                                               
         MVC   S.SPAKMKT(5),BMKTSTA                                             
         CLI   RCWRITE,C'N'                                                     
         BNE   *+8                                                              
*        OI    S.SPAFLAG,SPAFNOWR    TELL SPAUTH WRITE=NO                       
         OI    S.SPAFLAG,X'04'                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,0,X'D9000AB7'                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            GET SPAUTH ADDRESS                           
         LA    R1,MYBUFIO                                                       
         BASR  RE,RF                                                            
*                                                                               
         CLI   S.SPAERR,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  S                                                                
         POP   USING                                                            
*                                                                               
CHKAUTHX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* SUBROUTINE TO POINT RF TO DEMO LIST ENTRY FOR CURRENT PRODUCT                 
* AND INDEX BY VALUE IN DEMINDEX                                                
* ON EXIT                                                                       
* RETURN Y/N IN THE FIELD CALLED RATING                                         
*                                                                               
* REGISTER USAGE - RE IS RETURN REG                                             
*                  RF POINTS TO 3-BYTE ENTRY IN DEMO LIST                       
*                  R0/R1 ARE NOT PRESERVED                                      
*==============================================================                 
                                                                                
SETPTDEM DS    0H                                                               
         MVI   RATING,C'N'                                                      
         SR    RF,RF                                                            
         IC    RF,BPRD                                                          
         CLI   BPRD,X'FF'          GET DEMO LIST FOR PRODUCT                    
         JNE   *+8                                                              
         LHI   RF,220                                                           
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         AHI   RF,PTDEMO-PTBUFFD   POINT TO DEMO LIST                           
*                                                                               
         LLC   R0,DEMINDX                                                       
         MHI   R0,3                                                             
         AR    RF,R0               POINT R1 TO DEMO IN LIST                     
*                                                                               
         CLI   2(RF),0             TEST NONT DEMO                               
         JE    SETPTDM2                                                         
*                                                                               
         CLI   1(RF),C'R'                                                       
         JE    *+12                                                             
         CLI   1(RF),C'E'                                                       
         JNE   *+8                                                              
         MVI   RATING,C'Y'                                                      
*                                                                               
         CLI   1(RF),X'21'         TEST USER DEMO                               
         JNE   SETPTDMX            NO                                           
         LLC   R0,2(RF)            GET USER DEMO INDEX                          
         MHI   R0,7                                                             
         L     R1,ADEST                                                         
         AHI   R1,EUSRNMS-ESTHDR                                                
         AR    R1,R0               POINT TO THIS DEMO NAME                      
         J     SETPTDM4                                                         
*                                                                               
SETPTDM2 LLC   R0,1(RF)            GET NONT DEMO INDEX                          
         BCTR  R0,0                                                             
         SLL   R0,3                X 8                                          
         L     R1,ADEST                                                         
         AHI   R1,ENONTDMS-ESTHDR                                               
         AR    R1,R0                                                            
*                                                                               
SETPTDM4 CLI   0(R1),C'R'                                                       
         JE    *+12                                                             
         CLI   0(R1),C'E'                                                       
         JNE   SETPTDMX                                                         
         MVI   RATING,C'Y'                                                      
*                                                                               
SETPTDMX BR    RE                                                               
         EJECT                                                                  
*=========================================================                      
* GET PW FACTORS                                                                
*=========================================================                      
                                                                                
GETPW    NTR1  BASE=*,LABEL=*                                                   
         USING SPWORKD,RA,RC                                                    
         USING SPD2WK,R2                                                        
*                                                                               
         MVI   D2ADJCR,C'N'        RESET D2 ADJUSTMENT SWITCH                   
         LA    RE,PWDLTAB                                                       
         L     RF,=F'5000'                                                      
         XCEF                                                                   
         L     RE,=A(PWADJTAB)                                                  
         L     RF,=F'5000'                                                      
         XCEF                                                                   
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         MVC   MEDNUMWK,=F'60'     CREATE DATE TABLES                           
         MVC   MEDNUMMO,=F'13'                                                  
*        MVC   MEDNUMQT,=F'5'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
*                                                                               
         L     RE,ADCONLST         EXTRACT PWREC ADDRESS FROM                   
         USING SPADCONS,RE         EXTENDED ADCONS                              
         L     R4,ADPWREC                                                       
         ST    R4,APWREC           SAVE ADDR IN LOCAL STORAGE                   
         DROP  RE                                                               
*                                                                               
         L     RE,ADEST                                                         
         USING ESTHDR,RE                                                        
         OC    ECOST2,ECOST2       TEST COST2 ESTIMATE                          
         BNZ   GETPW09             CLEAR RECORD AND EXIT                        
         DROP  RE                                                               
*                                                                               
         XC    PWKEY,PWKEY                                                      
         LA    R5,PWKEY                                                         
         USING PWRECD,R5                                                        
*                                                                               
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD(1),SVAGYMD  AGMD                                         
         MVC   PWKCLT(2),SVCLT     CLT                                          
         MVC   PWKPRD,SVPRDCD      MOVE PRODUCT FROM REGEL                      
         MVC   PWKEST,SVEST        ESTIMATE                                     
         MVC   PWKMKT(2),SVMKT     MARKET                                       
*                                                                               
         XC    0(256,R4),0(R4)     CLEAR PW RECORD AREA                         
         MVC   0(13,R4),PWKEY      SAVE CURRENT KEY                             
         GOTO1 DATAMGR,PWDMCB,DMRDHI,SPTDIR,(R4),PWKEY                          
         CLC   0(13,R4),PWKEY      NO PW REC - USE EST PCT                      
         BNE   GETPW09                                                          
*                                                                               
         GOTO1 DATAMGR,PWDMCB,GETREC,SPTFILE,PWKEY+14,(R4),DMWORK               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R4                                                            
         LA    R5,PWEL                                                          
         USING PWDOLEL,R5                                                       
GPWEL    CLI   PWDOLCD,0                                                        
         BE    GPWELX                                                           
         CLI   PWDOLCD,PWDOLCDQ                                                 
         BE    GPWEL2                                                           
GPWEL1   LLC   RE,PWDOLLEN                                                      
         AR    R5,RE                                                            
         B     GPWEL                                                            
*                                                                               
GPWEL2   B     GPWEL21                                                          
         GOTO1 HEXOUT,DMCB,PWDOLCD,P,34                                         
         OC    PWDOLBIL,PWDOLBIL                                                
         BZ    *+10                                                             
         MVC   P+70(4),=C'BILL'                                                 
         GOTO1 REPORT                                                           
GPWEL21  L     RE,=A(PWADJTAB)                                                  
*                                                                               
GPWEL3   OC    0(4,RE),0(RE)                                                    
         BZ    GPWEL4                                                           
         CLC   0(2,RE),PWDOLWK                                                  
         BE    GPWEL4                                                           
         LA    RE,8(RE)                                                         
         B     GPWEL3                                                           
GPWEL4   DS    0H                                                               
         MVC   0(2,RE),PWDOLWK                                                  
         ICM   RF,15,PWDOLBIL                                                   
         BZ    *+10                                                             
         MVC   D2ADJCR,PWADJCR     ONLY IF CLIENT OPTION ON                     
         A     RF,4(RE)                                                         
         ST    RF,4(RE)                                                         
         B     GPWEL1                                                           
*                                                                               
*                                                                               
GPWELX   LR    R5,R4                                                            
         USING PWRECD,R5                                                        
         LA    R5,PWEL                                                          
         USING PWCLCEL,R5                                                       
GPWELA   CLI   PWCLCCD,0                                                        
         BE    GETPWX                                                           
         CLI   PWCLCCD,PWCLCCDQ                                                 
         BE    GPWELA2                                                          
GPWELA1  LLC   RE,PWCLCLEN                                                      
         AR    R5,RE                                                            
         B     GPWELA                                                           
*                                                                               
GPWELA2  DS    0H                                                               
         MVC   GPWIDAT,PWCLCWK                                                  
         BAS   RE,GPWDAT                                                        
         B     GPWELA21                                                         
         GOTO1 HEXOUT,DMCB,PWCLCCD,P,30                                         
         MVC   P+62(4),=C'OVR*'                                                 
         GOTO1 HEXOUT,DMCB,GPWODAT,P+68,8                                       
         GOTO1 REPORT                                                           
GPWELA21 LA    RE,PWDLTAB                                                       
GPWELA3  OC    0(4,RE),0(RE)                                                    
         BZ    GPWELA4                                                          
         CLC   0(4,RE),GPWODAT                                                  
         BE    GPWELA4                                                          
         LA    RE,8(RE)                                                         
         B     GPWELA3                                                          
GPWELA4  MVC   0(4,RE),GPWODAT                                                  
         ICM   RF,15,PWCLCAMT                                                   
         A     RF,4(RE)                                                         
         ST    RF,4(RE)                                                         
         B     GPWELA1                                                          
*                                                                               
GETPW09  XC    0(256,R4),0(R4)     CLEAR PW RECORD AREA                         
         B     GETPWX                                                           
*                                                                               
GETPWX   XIT1                                                                   
*                                                                               
GPWDAT   NTR1                                                                   
         L     RE,MEDAFRST                                                      
         LA    RF,MEDMON01                                                      
         XC    GPWODAT,GPWODAT                                                  
         XC    GPWOMON,GPWOMON                                                  
         XC    GPWOPER,GPWOPER                                                  
GPWDAT1  CR    RE,RF                                                            
         BNL   GPWDAT3A                                                         
         OC    0(4,RE),0(RE)                                                    
         BZ    GPWDAT2                                                          
         CLC   GPWIDAT,0(RE)       TEST PRIOR TO START OF MONTH                 
         BL    GPWDAT2             YES - SKIP                                   
         CLC   GPWIDAT,2(RE)       TEST PRIOR TO END OF MONTH                   
         BNH   GPWDAT3             YES  USE THIS MONTH                          
GPWDAT2  LA    RE,12(RE)                                                        
         B     GPWDAT1                                                          
*                                                                               
GPWDAT3  MVC   GPWODAT,0(RE)       MOVE OUT DATE                                
GPWDAT3A LA    RE,MEDMON01                                                      
         LA    RF,MEDPERD                                                       
         MVC   GPWOPER,0(RF)      SET PERIOD DATE                               
         MVC   GPWOPER,=X'FFFFFFFF' OVERRIDE IT TO WHAT REALLY IS               
GPWDAT4  CR    RE,RF                                                            
         BNL   GPWDATX                                                          
         OC    0(4,RE),0(RE)                                                    
         BZ    GPWDAT5                                                          
         CLC   GPWIDAT,2(RE)                                                    
         BNH   *+12                                                             
GPWDAT5  LA    RE,12(RE)                                                        
         B     GPWDAT4                                                          
         MVC   GPWOMON,0(RE)                                                    
*                                                                               
GPWDATX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
GPWIDAT  DS    CL2                                                              
GPWODAT  DS    CL4                                                              
GPWOMON  DS    CL4                                                              
GPWOPER  DS    CL4                                                              
VPWCALC1 DS    A                                                                
APWREC   DS    A                                                                
PWDMCB   DS    6F                                                               
         DS    0D                                                               
         DC    CL8'*PWBLOCK'                                                    
PWBLOCK  DS    XL48                                                             
         DS    0D                                                               
         DC    CL8'**PWKEY*'                                                    
PWKEY    DS    XL18                                                             
         DS    0F                                                               
PWDLTAB  DS    5000C                                                            
PWADJTAB DS    5000C                                                            
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPOMCOMD                                                       
       ++INCLUDE SPADAVCOM                                                      
       ++INCLUDE SPADBUYER                                                      
         EJECT                                                                  
LINED    DSECT                                                                  
         DS    CL1                                                              
LCODE    DS    CL2                                                              
         DS    CL1                                                              
LTYPE    DS    CL4                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
LDASH    DS    CL1                                                              
LLINE    DS    CL3                                                              
         DS    CL1                                                              
LDATE    DS    CL8                                                              
         DS    CL1                                                              
LSLN     DS    CL3                                                              
         DS    CL1                                                              
LTIME    DS    CL11                                                             
         DS    CL1                                                              
LMSCOST  DS    CL7                                                              
         DS    CL3                                                              
LMSRTG   DS    CL5                                                              
         DS    CL2                                                              
LMGCOST  DS    CL7                                                              
         DS    CL2                                                              
LMGRTG   DS    CL5                                                              
         DS    CL2                                                              
LPGMNM   DS    CL14                                                             
         ORG   LPGMNM+4                                                         
LSTAR    DS    CL2                                                              
LSTA     DS    CL8                                                              
         EJECT                                                                  
SPD202   CSECT                                                                  
DICSECT  DS    0D                                                               
         PRINT GEN                                                              
DCLIST   DS    0C                                                               
         DCDDL SP#AFFIL,9                                                       
         DCDDL SP#BK02,24                                                       
         DCDDL SP#BRDCS,7,LABEL=SP7BRDCS                                        
         DCDDL SP#BRDCS,9,LABEL=SP9BRDCS                                        
         DCDDL SP#BRDCS,10                                                      
         DCDDL SP#BRNTH,16                                                      
         DCDDL SP#BRNTS,22,C                                                    
         DCDDL SP#BRNTS,22,CU,LABEL=SPUBRNTS                                    
         DCDDL SP#CH,2                                                          
         DCDDL SP#CLITL,18,C                                                    
         DCDDL SP#CNFPU,24                                                      
         DCDDL SP#COST,4,C                                                      
         DCDDL SP#COST,4,CU,LABEL=SPUCOST                                       
         DCDDL SP#COST2,20,C                                                    
         DCDDL SP#COST3,13                                                      
         DCDDL SP#DAY,4                                                         
         DCDDL SP#DMGR3,21                                                      
         DCDDL SP#DMGR4,21                                                      
         DCDDL SP#DMGR5,25                                                      
         DCDDL SP#DOLLA,7,LABEL=SP7DOLLA                                        
         DCDDL SP#DP,2                                                          
         DCDDL SP#DYTS,19                                                       
         DCDDL SP#EST03,28                                                      
         DCDDL SP#EXCHG,8                                                       
         DCDDL SP#FREQ,4                                                        
         DCDDL SP#GOADE,9                                                       
         DCDDL SP#GOALM,6                                                       
         DCDDL SP#LNG,3                                                         
         DCDDL SP#LNGDP,7                                                       
         DCDDL SP#MG,4,C                                                        
         DCDDL SP#MGRTL,7                                                       
         DCDDL SP#MKTTL,7,LABEL=SP7MKTTL                                        
         DCDDL SP#MKTTL,18,C                                                    
         DCDDL SP#MONTH,5                                                       
         DCDDL SP#MST,4                                                         
         DCDDL SP#NMTLC,20         NUMBER OF TELECASTS                          
         DCDDL SP#NMBRC,20         NUMBER OF BROADCASTS                         
         DCDDL SP#NOBRD,10                                                      
         DCDDL SP#NOTLC,9                                                       
         DCDDL SP#NTWRK,7                                                       
         DCDDL SP#NWTM,12                                                       
         DCDDL SP#ORBIT,8                                                       
         DCDDL SP#ORIG,9,LABEL=SP8ORIG                                          
         DCDDL SP#ORMST,7                                                       
         DCDDL SP#PIGGB,20,C                                                    
         DCDDL SP#PKMST,7                                                       
         DCDDL SP#PROG,13                                                       
         DCDDL SP#PROTL,7,LABEL=SP7PROTL                                        
         DCDDL SP#PROTL,19,C                                                    
         DCDDL SP#PRTNR,5,LABEL=SP5PRTNR                                        
         DCDDL SP#PRTNR,7                                                       
         DCDDL SP#REP,4,LABEL=SP4REP                                            
         DCDDL SP#RVMST,7                                                       
         DCDDL SP#SLPSI,22                                                      
         DCDDL SP#SLPTS,26                                                      
         DCDDL SP#SPILL,7,LABEL=SP7SPILL                                        
         DCDDL SP#SPILL,9,LABEL=SP8SPILL                                        
         DCDDL SP#SPILL,12,C                                                    
         DCDDL SP#SPLFR,17                                                      
         DCDDL SP#SPOTS,4,LABEL=SP4SPOTS                                        
         DCDDL SP#SPOTS,5,LABEL=SP5SPOTS                                        
         DCDDL SP#SPREP,21,C                                                    
         DCDDL SP#STATL,7,LABEL=SP7STATL                                        
         DCDDL SP#STATL,19,C                                                    
         DCDDL SP#STATN,7                                                       
         DCDDL SP#SUM,17                                                        
         DCDDL SP#TAX02,53                                                      
         DCDDL SP#TIME,5                                                        
         DCDDL SP#TLCST,6                                                       
         DCDDL SP#TOTAL,3,LABEL=SP3TOTAL                                        
         DCDDL SP#TOTAL,5,LABEL=SP5TOTAL                                        
         DCDDL SP#TOTAL,7,LABEL=SP7TOTAL                                        
         DCDDL SP#TXEX,18,C                                                     
         DCDDL SP#WKAVG,8                                                       
DCLISTX  DC    X'00'                                                            
*                                                                               
DSLIST   DS    0C                                                               
         DSDDL PRINT=YES                                                        
DSLISTX  EQU   *                                                                
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*SPD2WK*'                                                    
SPD2WK   DS    0D                                                               
SVPERD   DS    CL256                                                            
DBLKAREA DS    CL256                                                            
MYBUFIO  DS    CL200                                                            
SVPSLIST DS    F                                                                
RTYPE    DS    CL3                                                              
BUFHI    DS    C                                                                
BUFCDE   DS    C                                                                
BUFRTYP  DS    C                                                                
LEVEL    DS    C                   LEVEL CODE                                   
ESTACT   DS    CL1                                                              
BRDPOLSW DS    CL1                                                              
CPPSW    DC    C'D'                                                             
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
         SPACE 2                                                                
D2APROF  DS    CL16                                                             
         ORG   D2APROF                                                          
PNOMKGD  DS    C                                                                
D2ASUPA  DS    C                                                                
         ORG   D2APROF+L'D2APROF                                                
FXPROF   DS    CL16                                                             
DARPROF  DS    CL16                                                             
         SPACE 2                                                                
PRTLINE  DS    CL132                                                            
SAVLINE  DS    CL132                                                            
DNAMES   DS    0CL28                                                            
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
SUBPROG  DS    0CL8                                                             
         DC    C'SP'                                                            
SUBPROG1 DC    C'M2'                                                            
SUBPROG2 DC    C'01'                                                            
SUBPROGT DC    C'  '                                                            
DASH     DC    50C'-'                                                           
FIRST    DS    C                                                                
PASS     DS    C                                                                
MAXPASS  DS    C                   HIGHEST REQUIRED PASS                        
SPDWSW   DS    C                                                                
SPILORIG DS    C                   BIT MASK SPIL/ORIG SP=2 ORIG=1'              
NOSUM    DS    C                   SUPPRESS SUMMARY SWITCH                      
ANYPRINT DS    C                   ANY ACTIVE PRINT LINES                       
PAGEONE  DS    C                   SET TO Y ON STAFRST                          
SALZEN   DS    C                   SET TO Y FOR ZENITH/SAL REPORT               
SALOPT   DS    C                   SET TO Y FOR OPTIMEDIA/SAL REPORT            
DXFNSW   DS    C                                                                
DARESVSW DS    C                                                                
CALLMGBY DS    C                                                                
VCALCPP  DC    A(0)                                                             
VSUMMRY  DC    A(0)                                                             
VUDESC   DC    A(0)                                                             
ABRSDESC DC    A(0)                                                             
ABTSDESC DC    A(0)                                                             
ASALDESC DC    A(0)                                                             
PSTASPT  DC    A(0)                                                             
PSTACOST DC    A(0)                                                             
PSTAGRID DC    A(0)                                                             
DSTAGRID DC    A(0)                A(DETAIL STATION GRID PRINT)                 
DDESC    DC    A(0)                A(DETAIL DESCRIPTION PRINT)                  
DTOTSPT  DC    A(0)                A(DETAIL TOTAL SPOTS)                        
TAXAMT   DC    F'0'                                                             
PENNYSW  DS    C                                                                
SPACESW  DS    CL1                                                              
         DS    0F                                                               
SALSDATA DS    0CL28               SALESPERSONS WORK AREA                       
SALSWKS  DS    F                                                                
SALSSPT  DS    F                                                                
SALSD1   DS    F                                                                
SALSD2   DS    F                                                                
SALSD3   DS    F                                                                
SALSD4   DS    F                                                                
SALSDL   DS    F                                                                
PRTADDR  DS    F                                                                
SVPH01   DS    F                                                                
SVPH02   DS    F                                                                
SVPH04   DS    F                                                                
SVSPECS  DS    F                                                                
SVMDTAB  DS    F                                                                
SVRDTE   DS    F                                                                
MSBFHOOK DC    F'0'                                                             
VMDADDWT DC    A(0)                                                             
VSTATOT  DC    A(0)                                                             
VEDTDEMS DC    A(0)                                                             
VHDATES  DC    A(0)                                                             
VGETBUF  DC    A(0)                                                             
VSUBPARA DC    A(0)                                                             
VREVBUY  DC    A(0)                                                             
AHDATES  DC    A(0)                                                             
VCOMPRNT DC    A(0)                                                             
VGETCOVR DC    A(0)                                                             
VBRSDSCC DC    A(0)                                                             
REPCALOV DC    A(0)                                                             
SVQOPT1  DS    C                                                                
SVRCSUB  DS    C                                                                
MSRCSUB  DS    C                                                                
SVSUPMKT DS    C                                                                
MSSUPMKT DS    C                                                                
DPTSW    DS    C                   DAYPART CHANGE SWITCH                        
MRPTTYP  DS    C                                                                
BUYACT   DS    C                                                                
CFDS     DS    C                                                                
CFDE     DS    C                                                                
RATING   DS    C                                                                
OVRFLAG  DS    CL10                                                             
STRDTE   DS    CL2                                                              
ENDDTE   DS    CL2                                                              
SVMGC1   DS    F                                                                
SVMGC2   DS    F                                                                
SVPGC1   DS    F                                                                
SVPGC2   DS    F                                                                
MSHDHOOK DC    F'0'                                                             
SVHDHOOK DC    F'0'                                                             
MSSPHK   DC    F'0'                                                             
SVSPHK   DC    F'0'                                                             
VGETREP  DC    F'0'                                                             
MYSPTHOK DC    F'0'                                                             
VFOOT    DC    F'0'                                                             
DARECON  DS    CL8                                                              
DRTRDDTA DC    XL8'00'             DARE CASH/TRADE DATA                         
DRCSHTRD DC    X'00'               DARE CASH/TRADE FLAG                         
* HCAPSUM  DC    C'**** SUMMARY ****   '                                        
HCAPSUM  DC    AL3(SP@SUM)                                                      
* PCAPSRP  DC    C'***SPECIAL REP=   ***'                                       
PCAPSRP  DC    AL3(SP@SPREP)                                                    
*                                                                               
* DETAIL OPTIONS - CONTROLLED BY QOPT1                                          
*                     1=YES,0=NO                                                
*                                                                               
*               FIELD  OPTION                                                   
*               -----  ------                                                   
*                 1    PRINT '*' NEXT TO OVERRIDEN DEMOS                        
*                 2    PRINT DEMO VALUES                                        
*                 3    PRINT COST                                               
*                 4    PRINT CPP                                                
DETOPTS  DS    CL4                 CURRENT OPTIONS SAVE                         
DETOPT   DC    AL1(1,1,1,1)        0 OPTION TABLE                               
         DC    AL1(0,1,1,1)        1                                            
         DC    AL1(0,0,1,1)        2                                            
         DC    AL1(0,1,0,0)        3                                            
         DC    AL1(1,1,0,0)        4                                            
         DC    AL1(0,0,0,0)        5                                            
         DC    AL1(1,1,1,0)        6                                            
         DC    AL1(0,1,1,0)        7                                            
         DC    AL1(0,0,1,0)        8                                            
*                                                                               
* TOTAL OPTIONS - CONTROLLED BY QOPT2                                           
*                        1=YES,0=NO                                             
*                                                                               
*               FIELD  OPTION                                                   
*               -----  ------                                                   
*                 1    PRINT TELECASTS                                          
*                 2    PRINT DOLLARS                                            
*                 3    PRINT DEMOS                                              
*                                                                               
SUMOPTS  DS    CL3                                                              
SUMOPT   DC    AL1(1,1,1)                                                       
         DC    AL1(1,1,0)                                                       
         DC    AL1(1,0,0)                                                       
         DC    AL1(1,0,1)                                                       
         DC    AL1(0,0,1)                                                       
         DC    AL1(0,1,0)                                                       
         DC    AL1(0,1,1)                                                       
         DC    AL1(0,0,0)                                                       
         DC    AL1(1,1,2)                                                       
LITMESS  DC    CL29'MUST INCLUDE REFERENCE NUMBER'                              
REALFRST DC    C'Y'                                                             
BIGCPP   DC    C'N'                                                             
*                                                                               
CURPDAT1 DS    CL56                DATES FOR PASS                               
CURPDAT2 DS    CL56                                                             
CURHDAT1 DS    CL56                DATES PRINTED IN HEADS                       
CURHDAT2 DS    CL56                                                             
*                                                                               
SAVEID   DS    CL12                                                             
SVBIGSTA DS    CL9                                                              
PWPREFIX DS    CL5                                                              
ELCODE   DS    X                                                                
PWPRESW  DS    C                                                                
BRSPASS2 DS    C                                                                
BRSDATE  DS    C                                                                
BDSCTLSW DS    C                                                                
PWADJCR  DS    C                                                                
D2ADJCR  DS    C                   D2S PRINT MARKET SPECIFIC HEADLINES          
BRSPCAP  DS    CL30                                                             
SVSUMOPT DS    CL3                                                              
*                                                                               
BINORDER DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                  DATE PORTION                                
BINORDSQ DS    XL2                  SEQUENCE PORTION                            
*                                                                               
CURDMCB  DS    6F                                                               
         DS    0D                                                               
P16      DS    L                                                                
         DS    0D                                                               
SVDEMS   DS    0XL32                                                            
SVD1     DS    F                   DEMO 1 VALUE                                 
SVD1CP   DS    F                   DEMO 1 CPP/CPM                               
SVD2     DS    F                   DEMO 2 VALUE                                 
SVD2CP   DS    F                   DEMO 2 CPP/CPM                               
SVD3     DS    F                   DEMO 3 VALUE                                 
SVD3CP   DS    F                   DEMO 3 CPP/CPM                               
SVD4     DS    F                   DEMO 4 VALUE                                 
SVD4CP   DS    F                   DEMO 4 CPP/CPM                               
*                                                                               
SVDG     DS    F                                                                
SVDGCP   DS    F                                                                
ACTMO    DS    F                                                                
UNIVERSE DS    F                                                                
WEIGHT   DS    F                                                                
MCOUNT   DS    F                                                                
PLDEMS   DS    0CL44                                                            
PLD1     DS    CL5                                                              
PLD1CP   DS    CL6                                                              
PLD2     DS    CL5                                                              
PLD2CP   DS    CL6                                                              
PLD3     DS    CL5                                                              
PLD3CP   DS    CL6                                                              
PLD4     DS    CL5                                                              
PLD4CP   DS    CL6                                                              
STAGRID  DS    14F                                                              
*                                                                               
         DS    0D                                                               
STATOTS  DS    0XL44                                                            
STASPOT  DS    F                   STATION TOTAL SPOTS                          
STADEMS  DS    8F                  STATION TOTAL DEMOS                          
STACOST  DS    2F                  STATION TOTAL DOLLARS                        
*                                                                               
         DS    0D                                                               
STTTOTS  DS    0XL44                                                            
STTSPOT  DS    F                   OVERALL STATION COST                         
STTDEMS  DS    8F                  OVERALL STATION DEMOS                        
STTCOST  DS    2F                  OVERALL STATION DOLLARS                      
*                                                                               
VSVMDBLK DS    F                                                                
STACAP   DS    CL7                                                              
MSOPT    DS    CL7                                                              
SVOPTS   DS    CL7                                                              
SUBPSW   DS    CL1                                                              
MSPROF   DS    CL16                                                             
SVPROF   DS    CL16                                                             
PASSSD3  DS    CL3                                                              
PASSED3  DS    CL3                                                              
PKGAREA  DS    CL16                BUY TYPE CAPTIONS                            
PIGAREA  DS    CL32                PIGGYBACK AREA                               
PIGAREAL DS    CL3                 PIGGYBACK LENGTH                             
PIGPRNT  DS    CL11                PIGGYBACK PRINT                              
SPLPRINT DS    C                                                                
SPSTPL   DS    CL132                                                            
SPBUFSTA DS    CL90                SAVE AREA FOR ORIG. STATIONS                 
         DS    0F                                                               
PREMTAB  DS    CL64                                                             
HIATAB   DS    CL64                                                             
COMAREA  DS    CL400               COMMENT AREA                                 
COVRHLD  DS    600C                COST OVERRIDE HOLD AREA                      
COVRFRST DS    C                   COST OVR FIRST TIME                          
OVRCNT   DS    C                   PL OVERRIDE COUNT                            
SVMAXLIN DS    C                                                                
MGSW     DS    C                                                                
HORIDX   DS    C                                                                
IDXRPT   DS    C                                                                
SLSUMSW  DS    C                                                                
DPSUMSW  DS    C                                                                
RAWGRP   DS    C                                                                
PRDEMCNT DS    C                   NUMBER OF DEMOS TO PRINT                     
APL      DS    F'0'                A(PL) FOR COST OVERRIDES                     
SVQEND   DS    CL6                                                              
SVSPREP  DS    H                                                                
SVSTLINE DS    CL132                                                            
SVCBLLIN DS    CL44                                                             
CURPGPTR DS    F                   START OF PROGRAM AREA SAVE                   
CURPH01  DS    CL12                BUFFER ADDRESS/LEN/LOADER ADDRESS            
CURPH02  DS    CL12                                                             
CURPH04  DS    CL12                                                             
PSLIST   DS    CL150                                                            
NEWMKT   DS    CL1                 MARKET FIRST SWITCH                          
DXSW     DC    XL1'00'             DX ACTIVITY SWITCH                           
DXOPEN   EQU   X'80'                                                            
DXNEWSTA EQU   X'40'                                                            
SVQUEST  DS    CL3                 SAVED REQUESTOR INITIALS                     
SVSTAT   DS    CL8                 SAVED STATION                                
LASTBIG  DS    CL8                 LAST BIG PRINTED                             
HADOFLOW DS    CL1                 'Y' IF $ ACCUM OVERFLOW                      
SVSPREPN DS    CL22                REP NAME                                     
REPFLAG  DS    CL1                 REP FLAG                                     
PASSTAB  DS    1072C                                                            
SPBUFMKT DS    CL500               SAVE AREA FOR ORIG. MARKETS                  
*                                                                               
SVMDBLK  DS    0D                                                               
         DS    1208C               SAVE MEDIA SUMMARY MEDBLOCK                  
                                                                                
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'*DAREIO*'                                                    
DAREIO   DS    4096C               SAVE AREA FOR DARE ORDER                     
         DS    0D                                                               
DAREHDLQ EQU   5                                                                
DAREHDSV DS    (DAREHDLQ)CL132     ROOM TO SAVE 5 HEADLINES                     
DAREMDSV DS    CL132               ROOM TO SAVE 1 MIDLINE                       
DARESAVX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*SUMBUF*'                                                    
SUMBUF   DS    6000C                                                            
SUMBUFX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'SUBPAREA'                                                    
SUBPAREA DS    0D                                                               
         DC    90000X'00'                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'*MGABLK*'                                                    
MGABLK   DS    CL192                                                            
MGTABLE  DS    60000C                                                           
MGTABLEX EQU   *                                                                
MGTABST  DS    800000C                                                          
MGTABSTX EQU   *                                                                
*                                                                               
PASSTABD DSECT                     SAVE MEDBLOCKS FOR PASSES                    
PASSSD   DS    CL6                 START DATE                                   
PASSED   DS    CL6                 END DATE                                     
PASSP1   DS    CL28                MEDBLOCK LEADER                              
PASSP2   DS    CL168               WEEKS                                        
PASSP3   DS    CL48                MONTHS                                       
PASSP4   DS    CL12                PERIOD                                       
PASSEND  DS    0C                                                               
         EJECT                                                                  
BPRTD    DSECT                                                                  
BPRTSPT  DS    CL10                 0                                           
BPRTD1   DS    CL7                 11                                           
BPRTDL   DS    CL9                 18                                           
BPRTD1C  DS    CL8                 27                                           
BPRTD2   DS    CL7                 35                                           
BPRTD2C  DS    CL8                 43                                           
BPRTD3   DS    CL7                 51                                           
BPRTD3C  DS    CL8                 58                                           
BPRTD4   DS    CL7                 68                                           
BPRTD4C  DS    CL8                 76                                           
         EJECT                                                                  
PROFDSCT DSECT                  ***PROGRAM PROFILE 1 DSECT***                   
PROFDPC  DS    CL1                 DETAIL PRINT CONTROL                         
PROFPAG  DS    CL1                 PAGE CONTROL                                 
PROFID   DS    CL1                 PRINT BUYLINE IDS                            
PROFPRN  DS    CL1                 PRINT REP NAME                               
PROFCC   DS    CL1                 COMMENT CONTROL                              
PROFHMS  DS    CL1                 FLAG HIATUS AND MISSED SPOTS                 
PROFFTA  DS    CL1                 FLAG TODAYS ACTIVITY                         
PROFMSR  DS    CL1                 MARKET/STATION RECAP                         
PROFMPC  DS    CL1                 MARKET PRINT CONTROL                         
PROFDPT  DS    CL1                 MARKET DAYPART TOTAL CONTROL                 
PROFMTR  DS    CL1                 MARKET TOTAL REPORT                          
PROFMSP  DS    CL1                 MEDIA SUMMARY PREFIX                         
PROFBMS  DS    CL1                 BRAND MEDIA SUMMARY NUMBER                   
PROFPMS  DS    CL1                 POL MEDIA SUMMARY NUMBER                     
PROFSPR  DS    CL1                 PRINT SPECIAL REP                            
         DS    CL1                 SPARE                                        
         EJECT                                                                  
SUMDSECT DSECT                                                                  
SUMKEY   DS    0CL15                                                            
SUMCODE  DS    CL1                 X'90'                                        
SUMDPGNO DS    CL1                 DAYPART GROUP NO.                            
SUMDPGRP DS    CL3                 DAYPART GROUP CODE                           
SUMDPNO  DS    CL1                 DAYPART NO.                                  
SUMDPART DS    CL3                 DAYPART CODE                                 
SUMSLN   DS    CL1                 SPOT LENGTH                                  
SUMRTYP  DS    CL1                 1=WEEKLY,2=MONTHLY,3=PERIOD                  
SUMDT    DS    CL4                 START-END DATES(FFFF FOR TOTAL)              
SUMRPT   DS    CL1                 REPORT CODE                                  
SUMDATA  DS    0CL60                                                            
SUMSPOTS DS    CL4                 SPOTS                                        
SUMDL    DS    CL4                 DOLLARS                                      
SUMDLEQ  DS    CL4                 DOLLARS EQU                                  
SUMD1    DS    CL4                 DEMO 1                                       
SUMD1EQ  DS    CL4                 DEMO 1 EQU                                   
SUMD2    DS    CL4                 DEMO 2                                       
SUMD2EQ  DS    CL4                 DEMO 2 EQU                                   
SUMD3    DS    CL4                 DEMO 3                                       
SUMD3EQ  DS    CL4                 DEMO 3 EQU                                   
SUMD4    DS    CL4                 DEMO 4                                       
SUMD4EQ  DS    CL4                 DEMO 4 EQU                                   
SUMGDL   DS    CL4                 GOAL $                                       
SUMGDLE  DS    CL4                 GOAL $ EQU                                   
SUMGD1   DS    CL4                 GOAL DEMO                                    
SUMGD1E  DS    CL4                 GOAL DEMO EQU                                
       ++INCLUDE SPREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPREPPTBUF                                                     
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPMEDBDESD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE SPDARDARED                                                     
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE SPAUTHD                                                        
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ADDRRECD DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
* SPDDEQUS                                                                      
       ++INCLUDE SPDDEQUS                                                       
* DDDICTATED                                                                    
       ++INCLUDE DDDICTATED                                                     
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
       ++INCLUDE SPGENDESTN                                                     
         PRINT ON                                                               
       ++INCLUDE SPMGADN                                                        
SPWORKD  DSECT                                                                  
         ORG   Q2USER                                                           
Q2C58    DS    CL1                                                              
         ORG   Q2USER COL                                                       
Q2ADDS   DS    CL4    21                                                        
Q2MCOM   DS    CL1    25           MARKET COMMENTS                              
Q2BYRFLT DS    CL3    26           NWS BUYER FILTER                             
Q2CAMFLT DS    CL5    29           NWS CAMPAIGN FILTER                          
Q2LPP    DS    CL2    34           LINES PER PAGE                               
Q2MCOMM  DS    CL1    36           COMMENT OPTION                               
Q2NET    DS    CL1    37           NET DOLLARS                                  
Q2MTR    DS    CL1    38           MARKET TOTAL REPORT                          
Q2FAXDEM DS    CL1    39           FIRST DEMO ON FAX REPORT                     
Q2DARORD DS    CL8    40           DARE ORDER NUMBER                            
Q2DARSID DS    CL2    48           ORIGIN SYSTEM ID                             
Q2DARAM  DS    CL2    50           EBCDIC AGENCY/MEDIA                          
Q2DARPR2 DS    CL3    52           PIGGYBACK PRODUCT FILTER                     
Q2SCOM   DS    CL1    55           STATION COMMENTS                             
Q2OMREQ  DS    CL1    56           C'Y' = ORDER MANAGER REQUEST                 
*                                                                               
         ORG   Q2USER+19           NOTE COLUMNS BLANKED ON REQFRST              
Q2UNWIND DS    CL1    40           C'U' FOR UNWIND REQUEST                      
Q2UNWMG  DS    CL1    41           UNWIND MAKEGOODS                             
Q2UNWPLS DS    CL1    42           UNWIND +OTOS                                 
Q2UNWMIN DS    CL1    43           UNWIND -OTOS                                 
Q2UNWSTR DS    CL6    44           UNWIND START DATE                            
Q2UNWEND DS    CL6    50           UNWIND END DATE                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096SPREPD202 11/19/19'                                      
         END                                                                    
