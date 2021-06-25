*          DATA SET SPREPD502  AT LEVEL 048 AS OF 11/19/19                      
*PHASE SPD502T                                                                  
*INCLUDE MEDBDESC                                                               
*INCLUDE REVBUY                                                                 
*INCLUDE SPRPFOOT                                                               
*INCLUDE REPSPILL                                                               
*INCLUDE REPUDESC                                                               
*INCLUDE REPCALOV                                                               
*INCLUDE COVAIL                                                                 
         TITLE 'SPREPD502- POL TIMESHEETS'                                      
         PRINT NOGEN                                                            
SPD502   CSECT                                                                  
         NMOD1 0,SPD502,R3                                                      
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
*                                                                               
         L     R2,=A(SPD5WK)                                                    
         USING SPD5WK,R2                                                        
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
*                                                                               
         USING MEDDATA,R4                                                       
         STM   RA,RC,SP60RA                                                     
         STM   R2,R3,SP60R2                                                     
         MVI   FORCEOPT,C'N'                                                    
         B     SPD502A                                                          
         EJECT                                                                  
* HEADLINE ROUTINES                                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SP60RB                                                      
         DROP  RF                                                               
         LM    R2,R3,SP60R2                                                     
         LM    RA,RC,SP60RA                                                     
         CLI   MODE,PROCBUY                                                     
         BNE   MYHDNOW                                                          
         MVC   MID1,SVMID1                                                      
*                                                                               
MYHDNOW  DS    0H                                                               
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   MODE,MGR1LAST                                                    
         BNH   *+10                                                             
*---->   MVC   H7+50(20),SUMCAP                                                 
         MVC   H7+50(L'SP@SUM),SP@SUM                                           
         DROP  RE                                                               
         CLC   RTYPE,=C'MGA'                                                    
         BE    MYHEAD4                                                          
         CLC   RTYPE,=C'RS '                                                    
         BE    MYHEAD2                                                          
         CLC   RTYPE,=C'RY '                                                    
         BE    MYHEAD2                                                          
         GOTO1 =A(PTSHEAD)                                                      
         CLI   FRMTOPT+3,1                                                      
         BNE   MYHEADX                                                          
         MVC   WORK(18),H10+104                                                 
         MVC   H10+104(18),H10+103                                              
         MVC   H10+113(18),WORK                                                 
         MVC   H11+93(20),SPACES                                                
         MVC   H12+93(20),SPACES                                                
         B     MYHEADX                                                          
MYHEAD2  GOTO1 =A(PRSHEAD)                                                      
         B     MYHEADX                                                          
*                                                                               
MYHEAD4  CLC   RTYPE,=C'MGA'                                                    
         BNE   MYHEAD5                                                          
         MVC   H1+54(24),=C'MISSED/MAKEGOOD ANALYSIS'                           
         MVC   H2+54(24),=C'------------------------'                           
         MVC   H10(34),=C' CD TYPE EST-LIN DATE     LEN TIME'                   
         MVC   H11(34),=C' -- ---- ------- ----     --- ----'                   
         MVC   H10+41(33),=C'MSD COST MSD RTG MGD COST MGD RTG'                 
         MVC   H11+41(33),=C'-------- ------- -------- -------'                 
         MVC   H10+75(12),=C'PROGRAM NAME'                                      
         MVC   H11+75(12),=C'------------'                                      
*                                                                               
         L     RE,ADSTAT                                                        
         USING STAREC,RE                                                        
*                                                                               
         MVC   MID2+10(4),BIGSTA   PRINT HEADEND/NO NETWORK                     
         MVC   MID2+16(L'SSYSNAME),SSYSNAME                                     
         DROP  RE                                                               
*                                                                               
MYHEAD5  DS    0H                                                               
MYHEADX  XIT1                                                                   
*                                                                               
SP60RA   DC    A(0)                                                             
SP60RB   DC    A(0)                                                             
SP60RC   DC    A(0)                                                             
SP60R2   DC    A(0)                                                             
SP60R3   DC    A(0)                                                             
         EJECT                                                                  
SPD502A  CLI   MODE,REQFRST                                                     
         BNE   M00                                                              
*                                                                               
         OI    RQOPTS,RQOPTS_CBHLAST  ASK FOR HDND LAST MODE                    
         MVI   RQALPHA,C'Y'        AND STATIONS IN ALPHA SEQ                    
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
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
         DROP  R5                                                               
         XC    FOOT1(110),FOOT1                                                 
         B     M2                                                               
*                                                                               
M00      CLI   MODE,MKTLAST                                                     
         BL    BYPW                                                             
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         MVC   WEIGHT,SPWEIGHT                                                  
BYPW     DS    0H                                                               
         CLI   MODE,ESTFRST                                                     
         BNH   *+12                                                             
         CLI   QSTART,C' '         WAS REQUEST VALID                            
         BE    M34                 ALWAYS DO REQLAST                            
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
         CLI   MODE,RUNFRST                                                     
         BNE   M1                                                               
         LA    RE,D5WSTRT          CLEAR THE WORK AREA                          
         L     RF,D5WLEN                                                        
         XCEF                                                                   
         MVC   SVMAXLIN,MAXLINES                                                
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         OI    RQOPT2,RQOPT2_NETBUYS                                            
         MVI   FIRST,1                                                          
         B     EXIT                                                             
         SPACE 2                                                                
M1       CLI   MODE,MKTFRST                                                     
         BL    M2                                                               
         CLI   MODE,REQLAST        ALWAYS PROCESS REQLAST                       
         BE    M34                                                              
*                                                                               
         CLI   ESTACT,0            ANY ESTIMATES FOR PRODUCT                    
         BE    EXIT                                                             
*                                                                               
         CLI   MODE,CBHLAST                                                     
         BNE   M2                                                               
         BRAS  RE,PRTMGA                                                        
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M4                                                               
         MVC   REASTART(12),QSTART                                              
         LA    RF,PTSDESC                                                       
         ST    RF,APTSDESC                                                      
         LA    RF,PRSDESC                                                       
         ST    RF,APRSDESC                                                      
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
*                                                                               
         GOTO1 =A(RQFIRST)                                                      
         GOTO1 =A(RQFRSTA)                                                      
*                                                                               
         CLC   RTYPE,=C'RS '                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '                                                    
         BNE   M2A                                                              
         MVI   SUBPSW,0                                                         
         CLI   QPNAME,C' '         TEST PRODUCT LEGENDS OPTION SET              
         BH    M2A                                                              
         LA    RE,PROGPROF         NO-GET IT FROM RS PROFILE                    
         MVC   QPNAME(1),PROFPLEG-PROFDSCT(RE)                                  
*                                                                               
M2A      MVI   MODE,RUNFRST                                                     
         BAS   R9,GOTOSUB                                                       
         MVI   MODE,REQFRST                                                     
         GOTO1 =A(BFLOAT)          SET BUFFALO LENGTH                           
         BAS   R9,GOTOSUB                                                       
         MVI   FIRST,0                                                          
         MVC   NUMWK,=F'60'                                                     
         B     EXIT                                                             
         EJECT                                                                  
M4       CLI   MODE,ESTFRST                                                     
         BNE   M5                                                               
         CLI   REASTART,C' '                                                    
         BE    *+10                                                             
         MVC   QSTART(12),REASTART                                              
*                                                                               
         L     RE,ADEST            OUT OF WEEK ROTATOR START DAY                
         USING ESTHDR,RE                                                        
         CLI   EOWSDAY,0           ONLY IF THERE IS ONE INPUT                   
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         B     *+8                 TESTING ONLY                                 
         MVI   SPOTPROF+8,3        FORCE A DAY                                  
         DROP  RE                                                               
*                                                                               
         MVC   MSSPPROF,SPOTPROF   INITIALIZE MEDIA SMRY PROFILE                
         MVC   MSSTART,QSTART                                                   
         MVI   ESTACT,1                                                         
         MVI   PASS,0                                                           
         BAS   R9,GOTOSUB                                                       
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVC   BRDPOLSW,CPROF                                                   
         DROP  R6                                                               
         SPACE 2                                                                
         GOTO1 =A(EFRSTC)                                                       
         B     EXIT                                                             
         SPACE 2                                                                
M5       CLI   MODE,PRDFRST                                                     
         BNE   M6                                                               
         MVI   PASS,0                                                           
         MVI   ESTACT,0            RESET ESTIMATE ACTIVE SWITCH                 
         CLC   QPROG,=C'RS'        RS REPORT?                                   
         BE    *+14                YES                                          
         CLC   QPROG,=C'RY'        RY REPORT?                                   
         BNE   M5A                 NO                                           
         CLC   QPRD2,SPACES        HAVE A PIGGY-BACK PRODUCT?                   
         BNH   M5A                 NO                                           
         BAS   RE,GETPROD                                                       
*                                                                               
M5A      BAS   R9,GOTOSUB                                                       
         B     EXIT                                                             
         EJECT                                                                  
M6       CLI   MODE,PROCBUY                                                     
         BNE   M7                                                               
         BRAS  RE,FIXCOVRD                                                      
*                                                                               
M6A      MVI   SORTPASS,1          SET SORT FOR STORE                           
* TEST CONFIRMED ONLY                                                           
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVI   BDPURP,X'FD'        SET TO GET NETWORK COSTS                     
         XC    CFDS(2),CFDS                                                     
         TM    BDCFD,1                                                          
         BZ    *+8                                                              
         MVI   CFDE,C')'                                                        
         TM    BDCFD,2                                                          
         BZ    *+8                                                              
         MVI   CFDS,C'('                                                        
         CLI   QPRGTYPE,C' '       PROGRAM TYPE FILTER                          
         BE    *+14                                                             
         CLC   BDPROGT,QPRGTYPE                                                 
         BNE   EXIT                                                             
         SPACE 2                                                                
         SPACE 2                                                                
         MVC   SVSPREP,BDREP                                                    
         BAS   R9,GOTOSUB                                                       
M6A1SRT1 L     RE,VPGRID                                                        
         L     RF,=F'20000'                                                     
         XCEF                                                                   
         XC    PGNOENT,PGNOENT                                                  
         XC    HIATAB,HIATAB                                                    
         XC    PREMTAB,PREMTAB                                                  
M6A1SRT2 CLI   SORTREQ,1                                                        
         BNE   M6A1SRT3                                                         
         GOTO1 VRSORT                                                           
         CLI   SORTPASS,1          BUILD PASS                                   
         BE    SORTX                YES - EXIT                                  
         CLI   SORTPASS,3          END OF SORT                                  
         BE    SORTX                YES - EXIT                                  
M6A1SRT3 DS    0H                                                               
*                                                                               
M6WIM    L     R5,=A(MGABLK)                                                    
         USING MGABLKD,R5                                                       
         L     R1,=A(MGTABLE)                                                   
         XC    0(MGERECL,R1),0(R1)                                              
         XC    0(MGALNQ,R5),0(R5)                                               
         MVI   MGAACT,MGAQBLN                                                   
         MVC   MGAACOM,ACOMFACS                                                 
         MVC   MGABUY,ADBUY                                                     
         MVC   MGGETBUY,VGETBUY    MOVES MG1OR2 AND VGETBUY                     
*                                                                               
         L     R1,=A(MGTABLE)                                                   
         ST    R1,MGATAB                                                        
         LR    RE,R1                                                            
         L     RF,=A(MGTABLEX-MGTABLE)                                          
         ST    RF,MGATABLN                                                      
         OI    MGAOPT,MGOFULN      SET FLAG FOR FULLWORD LEN                    
         XCEF                                                                   
*                                                                               
         MVC   MGAAGMD,SVAGYMD                                                  
         MVC   MGACLT,SVCLT                                                     
         MVC   MGAPRD,SVPRDCD                                                   
         MVC   MGASTA(2),SVMKT                                                  
         MVC   MGASTA+2(3),SVSTA                                                
         MVC   MGAEST,SVEST                                                     
         L     RF,PRDBUFF                                                       
         ZIC   RE,SVPRDCD                                                       
         CLI   SVPRDCD,X'FF'                                                    
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         LA    RF,DEMDISP(RE,RF)                                                
         ST    RF,MGADEM                                                        
         ST    RF,MGABRDEM                                                      
         OI    MGAOPT,MGONEDIT                                                  
*                                                                               
         L     RE,MGABUY                                                        
         SR    R0,R0                                                            
         ICM   R0,3,BUYKBUY-BUYREC(RE)       GET LINE NUMBER                    
         TM    BUYRCNTL-BUYREC(RE),BUYRLN2   TEST 2-BYTE LINE NUMBER            
         BO    M6MGA2                                                           
         LR    RF,R0                                                            
         SRL   RF,8                                                             
         STCM  RF,3,BUYKBUY-BUYREC(RE)    ALWAYS SET 2-BYTE LINE NUMBER         
*                                                                               
M6MGA2   MVI   MG1OR2,2                   AND TELL MGABLD                       
         GOTO1 VBLDMGN,MGABLKD                                                  
         L     RE,MGABUY                                                        
         STCM  R0,3,BUYKBUY-BUYREC(RE)    AND RESTORE ORIGINAL LINE NUM         
*                                                                               
         CLI   MGAERR,0            IGNORE FOR NOW                               
         BE    *+4                                                              
         CLI   QMGA,C'Y'                                                        
         BNE   M6MGAX                                                           
         L     RE,MGATAB           SAVE IN STATION TABLE                        
         USING MGENTRYD,RE                                                      
         OC    0(L'MGECODE,RE),0(RE)                                            
         BZ    M6MGAX                                                           
         L     RF,=A(MGTABST)                                                   
M6MGA4   OC    0(4,RF),0(RF)                                                    
         BZ    *+12                                                             
         LA    RF,MGERECL(RF)                                                   
         B     M6MGA4                                                           
         CLI   MGETYPE,X'FE'                                                    
         BE    M6MGA6                                                           
         CLI   PASS,0              ONLY SAVE ON PASS ZERO                       
         BH    M6MGA6                                                           
*                                                                               
         L     R1,ADBUY                                                         
         USING BUYREC,R1                                                        
         MVC   MGEUSER(1),BUYKEST                                               
         MVC   0(MGERECL,RF),0(RE)                                              
M6MGA6   LA    RE,MGERECL(RE)                                                   
         OC    0(L'MGECODE,RE),0(RE)                                            
         BNZ   M6MGA4                                                           
         DROP  RE                                                               
         DROP  R1                                                               
M6MGAX   DS    0H                                                               
         GOTO1 =A(EXTRCT)                                                       
         SPACE 1                                                                
         CLI   MEDSPILL,C'Y'                                                    
         BNE   M6NOSPL                                                          
*                                                                               
         L     RE,=A(MGTABST)      NO MGA FOR SPILL                             
         L     RF,=A(MGTABSTX-MGTABST)                                          
         XCEF                                                                   
*                                                                               
         CLI   SPOTPROF+5,0                                                     
         BE    EXIT                                                             
         CLI   SPLPRINT,1          SAVE SPILL IF SPILL PRINT ACTIVE             
         BNE   M6BYPORG                                                         
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'01',SPBUFSTA),0                        
         L     RE,=A(SPBUFMKT)                                                  
         GOTO1 (RF),DMCB,(RA),(X'01',(RE))                                      
         MVI   SPLPRINT,2                                                       
         SPACE 1                                                                
M6BYPORG CLI   SPOTPROF+5,3        PRINT SPILL IF REQUESTED                     
         BNE   EXIT                                                             
         CLI   SPLPRINT,2          PRINT SPILL CAPTION FIRST TIME               
         BNE   M6NOSPL                                                          
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFSTA),MID1                     
*---->   MVC   MID1(11),=C'***SPILL***'                                         
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         MVC   MID1(L'SP@SPILL),SP@SPILL                                        
         DROP  RE                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVC   SVMID1,MID1                                                      
         SPACE 1                                                                
M6NOSPL  CLI   SORTREQ,1                                                        
         BE    M6A1SR30                                                         
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
M6A1SR30 OC    PGNOENT,PGNOENT                                                  
         BZ    SORTX                                                            
         CLI   IDSW,1              MULTIPLE IDS ON ONE PAGE                     
         BNE   M6A1SR31                                                         
         CLI   FBSTA,C'Y'          FIRST BUY ON CONTRACT                        
         BNE   M6A1SR31                                                         
         CLI   FORCEHED,C'Y'       HEADLINES WILL SET IT                        
         BE    M6A1SR31                                                         
         LA    RE,MID1             FIND PRINT POSITION                          
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(12,RE),BUYIDNAM                                                
         MVC   13(12,RE),BUYID                                                  
         MVI   FORCEMID,C'Y'                                                    
         SPACE 2                                                                
M6A1SR31 MVI   FBSTA,C'N'          RESET FIRST BUY ON CONTRACT                  
         CLI   PGCNDSW,0           CONDENSE REQUIRED                            
         BE    M6A1SRT4             NO - PRINT LINE                             
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    SORTX                                                            
         CLC   QPROG,=C'RS'                                                     
         BE    *+14                                                             
         CLC   QPROG,=C'RY'                                                     
         BNE   M6A1SRT4                                                         
         CLI   SORTREQ,1                                                        
         BNE   M6A1SRT4                                                         
         BAS   RE,CSDEMCP                                                       
         CLC   PGNOENT,=F'450'                                                  
         BH    M6A1SRT4                                                         
         CLC   CURRSORT,NEXTSORT                                                
         BE    M6A1SRT2                                                         
M6A1SRT4 CLI   QBOOK1,C' '                                                      
         BNE   M6AA1                                                            
         CLI   DETOPTS,1                                                        
         BNE   M6AA1                                                            
*                                  GET DEMO OVERRIDES                           
         L     RE,MEDADEMO                                                      
         L     R6,0(RE)                                                         
         USING NDELEM,R6                                                        
         LA    RF,NDEMNO           SET START ADDRESS                            
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               SET END ADDRESS                              
         XC    OVRFLAG(L'OVRFLAG),OVRFLAG                                       
         LA    RE,OVRFLAG                                                       
M6AA     CR    RF,R6                                                            
         BNL   M6AA1                                                            
         TM    4(RF),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(RE),C'*'                                                       
         LA    RF,8(RF)                                                         
         LA    RE,1(RE)                                                         
         B     M6AA                                                             
         SPACE 1                                                                
M6AA1    DS    0H                                                               
         DROP  R6                                                               
         CLI   MKTACT,0            TEST FIRST ACTIVITY FOR MARKET               
         BNE   M6AA1A                                                           
         GOTO1 =A(MKTFIRST)        YES                                          
*                                                                               
M6AA1A   TM    RSSW,RSNEWSTA       TEST FIRST STATION                           
         BZ    M6AA2                                                            
         GOTO1 =A(RYFAX)           CHECK FAX                                    
*                                                                               
M6AA2    MVI   BUYACT,1            TELL EVERYONE WE HAVE SOMETHING              
         MVI   MKTACT,1                                                         
         DROP  R5                                                               
*                                                                               
         CLI   QPROG,C'U'          TURNAROUNDS PRINT REVS.                      
         BNE   M6ANOR                                                           
         GOTO1 VREVBUY,DMCB,(RA)                                                
*                                                                               
M6ANOR   MVI   SPLPRINT,0          DO OPTIONAL REPORTS                          
         MVI   P1,0                                                             
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'7'                                                         
         BH    HLOK                                                             
         OC    OPTRPT2,OPTRPT2                                                  
         BZ    M6NOOPT2                                                         
         GOTO1 OPTRPT,DMCB,(RA)                                                 
M6NOOPT2 MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         EJECT                                                                  
HLOK     DS    0H                  PRINT DETAIL LINES                           
         OC    PGNOENT,PGNOENT                                                  
         BZ    SORTX                                                            
         MVI   SPLPRINT,0                                                       
         GOTO1 =V(VMDBDESC),DMCB,(RA),PRTLINE                                   
         GOTO1 =A(GETCAP)                                                       
         MVI   CURRSORT,X'FF'                                                   
         LA    R5,PRTLINE                                                       
         L     RF,DDESC            MOVE IN DESCRIPTION                          
         BASR  R9,RF                                                            
*                                                                               
         L     RE,VPLAREA          CLEAR PRINT LINE SAVE AREA                   
         L     RF,=F'8000'                                                      
         XCEF                                                                   
         L     R6,DSTAGRID         SET GRID ADDRESS                             
         CLC   RTYPE,=C'RS '                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '                                                    
         BNE   M6AAB                                                            
         CLI   PGCNDSW,0           NOT CONDENSING                               
         BE    *+12                 CALC STATION TOTALS                         
         CLI   SORTREQ,1                                                        
         BE    *+8                                                              
         BAS   RE,CSDEMCP                                                       
         BAS   R9,BGRID                                                         
         B     SORTX                                                            
M6AAB    BAS   RE,CSDEMCP          GET DEMOS                                    
         GOTO1 VEDTDEMS            EDIT DEMO/CPP                                
         BAS   R9,BTSPD            MOVE DEMOS/CPP TO PRINT LINE                 
         BAS   R9,BGRID                                                         
         SPACE 2                                                                
         B     SORTX                                                            
         SPACE 2                                                                
SORTX    CLI   SORTREQ,1                                                        
         BNE   EXIT                                                             
         CLI   SORTPASS,2          GET PASS                                     
         BNE   EXIT                 NO - EXIT                                   
         CLC   CURRSORT,NEXTSORT                                                
         BE    M6A1SRT2                                                         
         B     M6A1SRT1                                                         
         EJECT                                                                  
M7       CLI   MODE,STAFRST                                                     
         BNE   M8                                                               
         CLC   QPROG,=C'RY'        TEST RY REPORT                               
         BNE   M7B                                                              
         OI    RSSW,RSNEWSTA       YES/INDICATE NEW STATION                     
*                                                                               
M7B      BAS   R9,GOTOSUB                                                       
         GOTO1 =A(GETSADDR)                                                     
*                                                                               
         CLI   BIGSTA,C'0'         TEST CABLE                                   
         BNL   M7C                                                              
         L     RE,=A(MGTABST)      FOR CABLE CLEAR AT CBHLAST ONLY              
         L     RF,=A(MGTABSTX-MGTABST)                                          
         XCEF                                                                   
*                                                                               
M7C      MVC   SVMID1,SPACES                                                    
         XC    PDNCNTR,PDNCNTR                                                  
         XC    SSCNTR,SSCNTR                                                    
         L     RE,VPRDLST                                                       
         LA    RF,PRDLSTL                                                       
         XCEFL ,                                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
M8       CLI   MODE,STALAST                                                     
         BNE   M12                                                              
         CLI   SORTREQ,1           SORT REQUIRED                                
         BNE   M8A                                                              
         MVI   SORTPASS,2           YES - SET SORT PASS FOR EXTRACT             
         MVI   MODE,PROCBUY                                                     
         GOTO1 SORT2                                                            
         MVI   MODE,STALAST                                                     
*                                                                               
M8A      OC    OPTRPT,OPTRPT       CHECK FOR OPTIONAL REPORT                    
         BZ    M8B                                                              
         MVI   OPTRMODE,PROCBUY    FORCE FOOTING IF REQUIRED                    
         GOTO1 OPTRPT,DMCB,(RA)                                                 
*                                                                               
M8B      GOTO1 VSTATOT                                                          
         GOTO1 =A(SIGNRPT)                                                      
*                                                                               
         L     RF,=A(MGTABST)                                                   
         OC    0(40,RF),0(RF)                                                   
         BZ    M8BMGA                                                           
         CLI   PASS,0                                                           
         BNE   M8BMGA                                                           
         GOTO1 =A(PRTMGA)                                                       
*                                                                               
M8BMGA   CLC   RTYPE,=C'RS '       TEST RS REPORT AND PRODUCT NAME              
         BE    *+14                LEGEND REQUIRED                              
         CLC   RTYPE,=C'RY '       TEST RY REPORT AND PRODUCT NAME              
         BNE   M9                  LEGEND REQUIRED                              
         CLI   QPNAME,C'Y'                                                      
         BNE   M9                                                               
         L     R4,VPRDLST                                                       
         SR    R8,R8                                                            
         CLI   0(R4),0                                                          
         BE    *+16                                                             
         LA    R4,3(R4)                                                         
         LA    R8,1(R8)                                                         
         B     *-16                                                             
         LTR   R8,R8                                                            
         BZ    M9                                                               
         L     R4,VPRDLST                                                       
         GOTO1 XSORT,DMCB,(R4),(R8),3,3,0                                       
         LA    R8,3(R8)                                                         
         SRL   R8,1                                                             
         LA    R8,3(R8)                                                         
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CR    RE,R8                                                            
         BNL   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   P,0                                                              
         MVC   P2,SPACES                                                        
         GOTO1 REPORT                                                           
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         MVI   P,0                                                              
*---->   MVC   P2(24),=C'*****PRODUCT LEGEND*****'                              
         MVC   P2(L'SP@PROLE),SP@PROLE                                          
         DROP  RE                                                               
         GOTO1 REPORT                                                           
*                                                                               
M8E      MVC   WORK(12),0(R4)                                                   
         LA    R5,P2                                                            
         LA    R8,WORK                                                          
         LA    R0,4                                                             
*                                                                               
M8F      MVC   0(3,R5),0(R8)                                                    
         MVI   3(R5),C'-'                                                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PKEY,R6                                                          
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   4(7,R5),=C'UNKNOWN'                                              
         MVC   4(L'SP@UNKN,R5),SP@UNKN                                          
         DROP  RE                                                               
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,0(R8)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   M8G                                                              
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         MVC   4(20,R5),PNAME                                                   
*                                                                               
M8G      LA    R8,3(R8)                                                         
         CLI   0(R8),0                                                          
         BE    *+12                                                             
         LA    R5,26(R5)                                                        
         BCT   R0,M8F                                                           
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         LA    R4,12(R4)                                                        
         CLI   0(R4),0                                                          
         BNE   M8E                                                              
         L     RE,VPRDLST                                                       
         LA    RF,PRDLSTL                                                       
         XCEFL ,                                                                
*                                                                               
M9       OC    OPTRPT,OPTRPT                                                    
         BZ    M9B                                                              
         MVI   OPTRMODE,STALAST                                                 
         GOTO1 OPTRPT,DMCB,(RA)                                                 
M9B      DS    0C                                                               
         BAS   R9,GOTOSUB                                                       
*                                                                               
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLI   CONEND,C'Y'         IS IT END OF CONTRACT                        
         BNE   *+12                NO                                           
         CLI   IDTOTALS,C'Y'       DO WE WANT CONTRACT TOTALS                   
         BNE   EXIT                NO                                           
         CLC   RTYPE,=C'RS '                                                    
         BE    EXIT                                                             
         CLC   RTYPE,=C'RY '                                                    
         BE    EXIT                                                             
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         CLI   PASS,0                                                           
         BNE   *+12                                                             
         CLI   PROFMSR,C'S'                                                     
         BE    M14A                                                             
         B     EXIT                                                             
*                                                                               
GETPROD  NTR1                                                                   
*                                                                               
         L     R5,ADCLT                                                         
         USING CLTHDR,R5                                                        
         LA    R6,CLIST+880                                                     
         LA    R5,CLIST            LIST OF PRODUCTS                             
*                                                                               
GETP10   OC    0(3,R5),0(R5)       PAST THE LAST PRODUCT?                       
         BNZ   *+6                 NO                                           
         DC    H'0'                YES - PRODUCT NOT FOUND                      
         CR    R5,R6               END OF LIST?                                 
         BL    *+6                                                              
         DC    H'0'                PRODUCT NOT FOUND                            
*                                                                               
         CLC   QPRD2,0(R5)         MATCH ON PIGGY PRODUCT?                      
         BNE   GETP20              NOT THIS ONE, GET NEXT                       
*                                                                               
         MVC   BPRD2,3(R5)         SAVE 1-BYTE PRODUCT CODE                     
         B     EXIT                AND EXIT                                     
*                                                                               
GETP20   DS    0H                                                               
         LA    R5,4(R5)            ADVANCE TO NEXT PRODUCT IN LIST              
         B     GETP10                                                           
         DROP  R5                                                               
*                                                                               
*        TRAP FINAL SORT EXIT                                                   
SORT2    NTR1                                                                   
         B     M6A1SRT1                                                         
         SPACE 2                                                                
         SPACE 2                                                                
M12      CLI   MODE,MKTFRST                                                     
         BNE   M14                                                              
         MVI   BUYACT,0                                                         
         MVI   MKTACT,0                                                         
         L     RE,=A(SPBUFMKT)                                                  
         LA    RF,500                                                           
         XCEF                                                                   
         L     RE,VFLMPRD                                                       
         LA    RF,450                                                           
         XCEF                                                                   
M12A     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         BAS   R9,GOTOSUB                                                       
         B     EXIT                                                             
         SPACE 2                                                                
M14      CLI   MODE,MKTLAST                                                     
         BNE   M16                                                              
         CLC   RTYPE,=C'RS '                                                    
         BE    M14B                                                             
         CLC   RTYPE,=C'RY '                                                    
         BE    M14B                                                             
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
M14A     MVI   BUFCDE,X'90'                                                     
         MVI   LEVEL,1                                                          
         MVC   MCOUNT,=F'1'                                                     
         CLI   MODE,MKTLAST        MAY BE STALAST AT THIS POINT                 
         BE    *+12                                                             
         CLI   BUYACT,1            CHECK FOR STATION BUY ACTIVITY               
         BNE   M15                                                              
         MVI   BUFCDE,X'88'        SPILL                                        
         GOTO1 =A(MLASTC)                                                       
         MVI   BUFCDE,X'89'        ORIG                                         
         GOTO1 (RF)                                                             
         MVI   BUFCDE,X'90'        TOTAL                                        
         GOTO1 (RF)                                                             
         L     RE,=A(SPBUFMKT)                                                  
         OC    0(10,RE),0(RE)      PRINT OUT ORIGINATING MARKETS                
         BZ    M14A1A              IF THERE WAS ANY SPILL                       
*---->   MVC   P1(11),=C'***SPILL***'                                           
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         MVC   P1(L'SP@SPILL),SP@SPILL                                          
         DROP  RE                                                               
         L     RE,=A(SPBUFMKT)                                                  
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',(RE)),P1                           
         GOTO1 REPORT                                                           
         XC    P1,P1                                                            
M14A1A   MVI   BUYACT,0                                                         
M14MACHK CLI   MODE,MKTLAST                                                     
         BNE   M15                                                              
*                                                                               
M14B     L     R6,ADCOMREC                                                      
         OC    0(13,R6),0(R6)      TEST ANY MEDIA COMMENTS NEEDED               
         BZ    M14F                                                             
         LA    R6,24(R6)           YES-FIND THE COMMENT ELEMENTS                
         SR    R0,R0                   AND MOVE TO PRINT LINES                  
         LA    R1,P2                                                            
         LA    RF,10               MAX 10 COMMENTS                              
*                                                                               
M14C     CLI   0(R6),0                                                          
         BE    M14E                                                             
         CLI   0(R6),5                                                          
         BNE   M14D                                                             
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'3'                                                         
         BM    M14D                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),2(R6)                                                    
         LA    R1,132(R1)                                                       
         BCT   RF,M14D                                                          
         B     M14E                                                             
*                                                                               
M14D     IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     M14C                                                             
*                                                                               
M14E     CLC   P2,SPACES           PRINT THE MEDIA COMMENTS                     
         BE    M14F                                                             
         MVI   P1,0                                                             
         MVI   0(R1),0                                                          
         GOTO1 REPORT                                                           
*                                                                               
M14F     BAS   R9,GOTOSUB                                                       
*                                                                               
M15      MVI   PASS,0                                                           
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
M16      CLI   MODE,PROCGOAL       PROCESS THE GOAL RECORD                      
         BNE   M18                                                              
         CLI   BUYACT,1            ONLY IF THERE HAS BEEN ACTIVITY              
         BNE   EXIT                                                             
         BAS   R9,GOTOSUB                                                       
         GOTO1 =A(GETGL)                                                        
         B     EXIT                                                             
         EJECT                                                                  
*        HANDLE BREAKS                                                          
M18      MVI   BUFCDE,X'90'                                                     
         CLI   MODE,PRDLAST                                                     
         BNE   M20                                                              
         MVI   LEVEL,2                                                          
         B     M32A                                                             
*                                                                               
M20      CLI   MODE,MGR1LAST                                                    
         BNE   M22                                                              
         MVI   LEVEL,3                                                          
         B     M32A                                                             
*                                                                               
M22      CLI   MODE,MGR2LAST                                                    
         BNE   M24                                                              
         MVI   LEVEL,4                                                          
         B     M32A                                                             
*                                                                               
M24      CLI   MODE,MGR3LAST                                                    
         BNE   M26                                                              
         MVI   LEVEL,5                                                          
         B     M32A                                                             
         SPACE 2                                                                
M26      MVI   BUFCDE,X'93'                                                     
         CLI   MODE,CLTLAST                                                     
         BNE   M28                                                              
         CLC   QPRD,=C'ALL'                                                     
         BNE   EXIT                                                             
         MVI   LEVEL,2                                                          
         B     M32A                                                             
*                                                                               
M28      CLI   MODE,PGR1LAST                                                    
         BNE   M30                                                              
         MVI   LEVEL,3                                                          
         B     M32A                                                             
*                                                                               
M30      CLI   MODE,PGR2LAST                                                    
         BNE   M32                                                              
         MVI   LEVEL,4                                                          
         B     M32A                                                             
*                                                                               
M32      CLI   MODE,PGR3LAST                                                    
         BNE   M34                                                              
         MVI   LEVEL,5                                                          
*                                                                               
M32A     BAS   RE,DOSUM                                                         
         B     EXIT                                                             
*                                                                               
M34      CLI   MODE,REQLAST                                                     
         BNE   M36                                                              
         CLI   FOOT1,C' '                                                       
         BE    RESPRGM                                                          
         MVI   FORCEHED,C'N'                                                    
         MVI   P,0                                                              
         MVC   P2(132),FOOT1                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
RESPRGM  CLI   ESTACT,0            BYPASS SUBORDINATE IF NO ACTIVITY            
         BE    *+8                                                              
         BAS   R9,GOTOSUB                                                       
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
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
CURTAB1  DC    X'00000000116C4040'                                              
         EJECT                                                                  
GOTOSUB  CLI   PASS,0              PASS = 0                                     
         BNER  R9                   NO - BYPASS SUBPROGRAM                      
         CLI   SUBPSW,0                                                         
         BNE   *+10                                                             
         CLI   MODE,ESTFRST                                                     
         BHR   R9                                                               
         GOTO1 =A(GOSUB)                                                        
         BR    R9                                                               
         EJECT                                                                  
*===================================================================            
* BUILD WEEKLY GRID AND SUM INTO STATION BUCKETS  R6=PRINT POSITION             
*===================================================================            
*                                                                               
BGRID    LA    R5,MEDMON01                                                      
         LA    R8,STAGRID                                                       
         ST    R6,FULL             SAVE PRINT LINE ADDRESS                      
BGRID2   L     R4,4(R5)                                                         
         LA    RE,MEDMON13                                                      
         CR    R5,RE                                                            
         BH    BGRID5                                                           
         OC    0(4,R5),0(R5)                                                    
         BZ    BGRID4A                                                          
BGRID4   L     RE,0(R8)            SUM WEEKLY SPOTS                             
         A     RE,MEDBYSPT                                                      
         ST    RE,0(R8)                                                         
         LA    R6,4(R6)                                                         
         LA    R8,4(R8)                                                         
BGRID4A  LA    R5,12(R5)                                                        
         B     BGRID2                                                           
BGRID5   CLI   MODE,PROCBUY        PRINT PREMPTIONS                             
         BNE   BGRIDX                                                           
         GOTO1 =A(PTSGRID),DMCB,(LENGRID,(RA))                                  
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,PASSSD2)  GET 2-BYTE PASS DATES            
         GOTO1 (RF),(R1),QEND,(2,PASSED2)                                       
         BRAS  RE,PRCOSTOV                                                      
*                                                                               
BGRIDX   BR    R9                                                               
         EJECT                                                                  
PTSDESC  GOTO1 =A(SUBR01),DMCB,('PTSDESCE',(RA))                                
         BR    R9                                                               
*              PRINT STATION ROTATION DESCRIPTION                               
         USING BDEXTD,R5                                                        
PRSDESC  MVC   P1(L'BDPDAY),BDPDAY                                              
         MVC   P1+9(L'BDPTIME),BDPTIME                                          
         MVC   P1+21(L'BDPPROG),BDPPROG                                         
         MVC   P1+37(L'BDPSLN),BDPSLN                                           
         MVC   SVP1(40),P1                                                      
*                                                                               
         CLI   RSLINNO,C'Y'        PRINT EST-LIN NO ON RS                       
         BNE   PRSDESC2                                                         
         MVC   P2(L'BDPEST),BDPEST                                              
         MVI   P2+3,C'-'                                                        
         MVC   P2+4(L'BDPLIN),BDPLIN                                            
*                                                                               
PRSDESC2 CLI   RSCOST,C'Y'         PRINT COST                                   
         BNE   PRSDESCX                                                         
         MVC   P2+8(10),BDPCOST+2                                               
PRSDESCX MVC   SVP2(40),P2                                                      
         BR    R9                                                               
         EJECT                                                                  
* PRINT DEMOS AND CPP FOR BRS,BTS,BDS,SAL                                       
BTSPD    LA    R8,P1               SET PRINT LINE                               
         CLI   MODE,STALAST                                                     
         BNE   *+8                                                              
         LA    R8,P2                                                            
         CLI   DETOPTS+1,1         DEMOS REQUESTED                              
         BNE   BTSPDX                                                           
         L     R6,NODEMS                                                        
         ST    R8,FULL                                                          
         LA    RE,PLD1                                                          
         LA    RF,OVRFLAG                                                       
         LA    R1,DNAMES                                                        
         CLI   FRMTOPT+3,2         DEMOS 2 UP                                   
         BNE   P1PD                 NO - TRY FOR ONE UP                         
PTSPD1   MVC   101(5,R8),0(RE)     MOVE DEMOS TO PRINT LINE                     
         MVC   106(1,R8),0(RF)                                                  
         MVC   93(7,R8),0(R1)                                                   
         BCT   R6,*+8                                                           
         B     PTSPD1A                                                          
         MVC   113(7,R8),7(R1)                                                  
         MVC   126(1,R8),1(RF)                                                  
         MVC   121(5,R8),11(RE)                                                 
         LA    RE,22(RE)                                                        
         LA    RF,2(RF)                                                         
         LA    R1,14(R1)                                                        
         LA    R8,132(R8)                                                       
         BCT   R6,PTSPD1                                                        
         B     *+8                                                              
PTSPD1A  LA    R8,132(R8)                                                       
         MVC   113(12,R8),HLDPNAM                                               
         MVC   126(6,R8),HLDBOOK                                                
         LA    R8,260(R8)                                                       
         MVI   0(R8),0                                                          
         SPACE 2                                                                
         L     R8,FULL                                                          
         L     R6,NODEMS                                                        
         LA    RE,PLD1CP+1                                                      
PTSPD2   CLI   DETOPTS+3,1                                                      
         BNE   BTSPDX                                                           
PTSPD3   MVC   107(5,R8),0(RE)                                                  
         BCT   R6,*+8                                                           
         B     BTSPDX                                                           
         MVC   127(5,R8),11(RE)                                                 
         LA    R8,132(R8)                                                       
         LA    RE,22(RE)                                                        
         BCT   R6,PTSPD3                                                        
BTSPDX   BR    R9                                                               
         SPACE 2                                                                
* PRINT DEMOS 1 UP                                                              
P1PD     DS    0H                                                               
P1PD1    MVC   113(7,R8),0(R1)     MOVE DEMOS - ONE UP                          
         MVC   126(1,R8),0(RF)                                                  
         MVC   121(5,R8),0(RE)                                                  
         LA    RE,11(RE)                                                        
         LA    RF,1(RF)                                                         
         LA    R1,7(R1)                                                         
         LA    R8,132(R8)                                                       
         BCT   R6,P1PD1                                                         
         MVC   113(12,R8),HLDPNAM                                               
         MVC   126(6,R8),HLDBOOK                                                
         LA    R8,260(R8)                                                       
         MVI   0(R8),0                                                          
*        PRINT CPP/CPM                                                          
         L     R8,FULL                                                          
         L     R6,NODEMS                                                        
*        SLL   R6,1                                                             
         LA    RE,PLD1CP+1                                                      
         CLI   DETOPTS+3,1                                                      
         BNE   BTSPDX                                                           
P1PD2    MVC   127(5,R8),0(RE)                                                  
         LA    R8,132(R8)                                                       
         LA    RE,11(RE)                                                        
         BCT   R6,P1PD2                                                         
         BR    R9                                                               
         EJECT                                                                  
* CALCULATE AND SAVE DEMOS AND CPP/CPM                                          
CSDEMCP  NTR1                                                                   
         USING MEDDATA,R4                                                       
         LA    R5,MEDPERD                                                       
         L     R4,4(R4)                                                         
         L     R4,4(R5)                                                         
         XC    SVD1(112),SVD1                                                   
         OC    MEDBYD(12),MEDBYD                                                
         BZ    CSDEMCPX                                                         
         L     RE,STASPOT                                                       
         A     RE,MEDBYSPT                                                      
         ST    RE,STASPOT                                                       
         L     RE,STACOST                                                       
         A     RE,MEDBYD                                                        
         ST    RE,STACOST                                                       
         L     RE,STACOST+4                                                     
         A     RE,MEDBYDEQ                                                      
         ST    RE,STACOST+4                                                     
         L     RE,UNATOT                                                        
         A     RE,STAUNA                                                        
         ST    RE,STAUNA                                                        
         XC    UNATOT,UNATOT                                                    
         LA    R0,8                                                             
         LA    RF,STADEMS                                                       
         LA    R6,MEDBY1                                                        
CSSTA    L     RE,0(RF)                                                         
         A     RE,0(R6)                                                         
         ST    RE,0(RF)                                                         
         LA    RF,4(RF)                                                         
         LA    R6,4(R6)                                                         
         BCT   R0,CSSTA                                                         
         XC    PRTLINE,PRTLINE                                                  
         LA    RE,PRTLINE                                                       
         USING SUMDSECT,RE                                                      
         MVC   SUMDL(8),MEDBYD                                                  
         MVC   SUMD1(112),MEDBY1                                                
         LR    R6,R3               R3 IS USED FOR SUMDSECT                      
         LR    R3,RE               CALCPP DEPENDS ON THIS                       
         DROP  RE                                                               
         GOTO1 VCALCPP,DMCB,MEDBYSPT                                            
         LR    R3,R6               RESTORE R3                                   
CSDEMCPX XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* DO SUMMARIES FOR VARIOUS BREAKS                                               
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
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
GOSUB    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   D5START,QSTART                                                   
         MVC   QSTART(12),MSSTART                                               
         MVC   SVOPTS,QOPT1                                                     
         MVC   QOPT1(7),MSOPT                                                   
         MVC   SVPROF,PROGPROF                                                  
         MVC   SVSPPROF,SPOTPROF                                                
         MVC   SPOTPROF,MSSPPROF                                                
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
*        L     R7,MEDBUFF                                                       
*        USING MEDBLOCK,R7                                                      
GOSUB01  DS    0C                                                               
*        CLI   MODE,STAFRST        BYPASS CLEARING BUFFER                       
*        BL    GOTOSUB1                                                         
         CLI   MODE,MKTLAST                                                     
         BL    GOTOSUB1                                                         
         MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   FORCEHED,C'Y'                                                    
         B     GOTOSUB1                                                         
         SPACE 2                                                                
GOTOSUB1 MVC   SVRCSUB,RCSUBPRG                                                 
         L     RF,MEDBUFF          RESTORE MEDIA SUMMARY DATES                  
         L     RE,VSVMDBLK                                                      
         LA    R1,1272                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   SVSPHK,SPOTHOOK                                                  
         MVC   SPOTHOOK,MSSPHK                                                  
         MVC   SVHDHOOK,HEADHOOK                                                
         MVC   HEADHOOK,MSHDHOOK                                                
         MVC   RCSUBPRG,MSRCSUB                                                 
         MVC   SPSUPMKT,MSSUPMKT                                                
         L     RF,SVPH02                                                        
         GOTO1 (RF),DMCB,(RA)      GO TO MEDIA SUMMARIES                        
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   MSBFHOOK,BUFFHOOK                                                
         XC    BUFFHOOK,BUFFHOOK                                                
         DROP  RF                                                               
         MVC   MSSTART,QSTART                                                   
         MVC   QSTART(12),D5START                                               
         MVC   MSOPT,QOPT1                                                      
         MVC   MSOPT+4(1),QOPT5+1                                               
         MVC   QOPT1(7),SVOPTS                                                  
         MVC   MSSUPMKT,SPSUPMKT                                                
         MVC   SPSUPMKT,SVSUPMKT                                                
         MVC   PROGPROF,SVPROF                                                  
         MVC   MSSPPROF,SPOTPROF                                                
         MVC   SPOTPROF,SVSPPROF                                                
         L     RE,MEDBUFF                                                       
         L     RF,VSVMDBLK                                                      
         LA    R1,1272                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
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
BFLOAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,BUFFBUFF                                                      
         USING BUFFALOD,R6                                                      
         MVC   4(8,R1),ABUFF       SET PREV. BUFFERS                            
         OC    ABUFF,ABUFF                                                      
         BNZ   BFLOAT2                                                          
         MVC   LNBUFF,=F'50000'    MINIMUM BUFFER                               
         GOTO1 =V(COVAIL),DMCB,C'LOOK'                                          
         L     R9,8(R1)            GET AMOUNT OF CORE LEFT                      
         C     R9,=F'750000'       ENOUGH FOR ALLOCATION                        
         BL    *+12                                                             
         S     R9,=F'700000'       LEAVE ENOUGH SUBPROGRAMS                     
         ST    R9,LNBUFF                                                        
         MVC   ABUFF,=F'50000'     MINIMUM BUFFER                               
         GOTO1 =V(COVAIL),DMCB,C'GET',ABUFF,LNBUFF                              
BFLOAT2  OC    4(8,R1),4(R1)       ALLOCATE OK                                  
         BNZ   *+6                                                              
         DC    H'0'                NOT ENOUGH CORE                              
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
RQFIRST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVQUEST,QUESTOR                                                  
         MVC   RTYPE(2),QPROG                                                   
         MVI   RTYPE+2,C' '                                                     
         CLI   FIRST,1                                                          
         BNE   NOTFRST                                                          
*                                                                               
         L     RF,=A(PNTABLE)                                                   
         ST    RF,VPNTABLE                                                      
*                                                                               
         L     RF,=A(SSTABLE)                                                   
         ST    RF,VSSTABLE                                                      
*                                                                               
         L     RF,=A(SORTC)                                                     
         ST    RF,VRSORT                                                        
*                                                                               
         L     RF,=V(SPRPFOOT)                                                  
         ST    RF,VFOOT                                                         
*                                                                               
         L     RF,=A(SVMDBLK)                                                   
         ST    RF,VSVMDBLK                                                      
*                                                                               
         L     RF,=A(GETBUF)                                                    
         ST    RF,VGETBUF                                                       
*                                                                               
         L     RF,=A(CALCPP)                                                    
         ST    RF,VCALCPP                                                       
*                                                                               
         L     RF,=A(STATOTC)                                                   
         ST    RF,VSTATOT                                                       
*                                                                               
         L     RF,=A(EDTDEMSC)                                                  
         ST    RF,VEDTDEMS                                                      
*                                                                               
         L     RF,=A(SUBPAREA)                                                  
         ST    RF,VSUBPARA                                                      
*                                                                               
         L     RE,=A(GETREP)                                                    
         ST    RE,VGETREP                                                       
*                                                                               
         L     RE,=A(FLMPRD)                                                    
         ST    RE,VFLMPRD                                                       
*                                                                               
         L     RE,=A(PRDLST)                                                    
         ST    RE,VPRDLST                                                       
*                                                                               
         L     RE,=V(REVBUY)                                                    
         ST    RE,VREVBUY                                                       
*                                                                               
         L     RE,=A(COMPRNT)                                                   
         ST    RE,VCOMPRNT                                                      
*                                                                               
         L     RE,=A(MRGPL)                                                     
         ST    RE,VMRGPL                                                        
*                                                                               
         L     RE,=A(PLAREA)                                                    
         ST    RE,VPLAREA                                                       
*                                                                               
         L     RE,=V(REPCALOV)                                                  
         ST    RE,REPCALOV                                                      
*                                                                               
NOTFRST  XC    OPTRPT,OPTRPT                                                    
         XC    OPTRPT2,OPTRPT2                                                  
*                                                                               
         CLC   RTYPE,=C'RS '       CHECK FOR KRAFT REPORT                       
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '       CHECK FOR KRAFT REPORT                       
         BNE   M2NORPT                                                          
         CLC   QAGY,=C'YN'         Y&R HAS KRAFT                                
         BE    *+10                                                             
         CLC   QAGY,=C'NW'         NW AYER HAD KRAFT                            
         BNE   M2OPTDFS                                                         
         CLC   QCLT,=C'KP '        SPECAIL REPORT FOR KRAFT                     
         BNE   M2NORPT                                                          
         L     RE,=A(KRFTRPT)                                                   
         ST    RE,OPTRPT                                                        
         SPACE 2                                                                
M2OPTDFS CLC   QAGY,=C'DF'                                                      
         BNE   M2NORPT                                                          
         CLC   QCLT,=C'CO '                                                     
         BE    *+10                                                             
         CLC   QCLT,=C'AO '                                                     
         BNE   M2NORPT                                                          
         L     RE,=A(DFSRPT)                                                    
         ST    RE,OPTRPT                                                        
         SPACE 2                                                                
M2NORPT  LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         TM    PROFMTR,X'0F'                                                    
         BNZ   *+10                                                             
         MVC   PROFMTR,HALF        SET MARKET TOTAL FROM PROFILE                
         L     RF,=A(SALSUM)       SET MARKET TOTAL REPORT                      
         CLI   PROFMTR,C'3'                                                     
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
         L     RF,=A(PGRIDC)                                                    
         ST    RF,VPGRID                                                        
         MVC   MRPTTYP,PROFMTR                                                  
         SPACE 2                                                                
         L     RF,=A(DICSECT)                                                   
         USING DICSECT,RF                                                       
*---->   MVC   STACAP(7),=C'STATION'                                            
         MVC   STACAP(L'SP@STATN),SP@STATN                                      
         DROP  RF                                                               
         MVI   QCOMPARE,C'A'                                                    
         CLI   QRERATE,C'I'                                                     
         BNE   *+8                                                              
         MVI   QCOMPARE,C'B'                                                    
RQ1      DS    0H                                                               
         MVC   MAXLINES,SVMAXLIN                                                
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   FOOT1,C' '                                                       
         BE    RQ1NOFT                                                          
         ZIC   R0,MAXLINES                                                      
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
         LA    R1,H11+35                                                        
         ST    R1,AHDATES                                                       
         MVI   SPACESW,1                                                        
         L     RF,APTSDESC                                                      
         ST    RF,DDESC                                                         
         LA    RF,P1+34                                                         
         ST    RF,DSTAGRID                                                      
         LA    RF,P2+31                                                         
         ST    RF,PSTASPT                                                       
         LA    RF,P2+101                                                        
         ST    RF,PSTACOST                                                      
         LA    RF,P2+31                                                         
         ST    RF,PSTAGRID                                                      
         MVI   PENNYSW,1                                                        
         MVC   MEDNUMWK,=F'60'                                                  
         MVI   SVXLIN,0                                                         
         CLI   QOPT1,C' '                                                       
         BE    *+10                                                             
         MVC   SVXLIN,QOPT1                                                     
         NI    SVXLIN,X'0F'        MAKE NUMERIC                                 
         CLI   QOPT2,C' '                                                       
         BE    *+10                                                             
         MVC   PROFFRMT,QOPT2                                                   
         CLI   QOPT3,C' '                                                       
         BE    *+10                                                             
         MVC   PROFCNDS,QOPT3                                                   
         CLI   QOPT4,C' '                                                       
         BE    *+10                                                             
         MVC   PROFDPC,QOPT4                                                    
         MVC   SUBPROG1,=C'D5'                                                  
         MVC   SUBPROG2,=C'05'                                                  
         L     R6,SPECS                                                         
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         MVC   SPECS,DMCB+4                                                     
         LA    RE,PROGPROF                                                      
         SPACE 2                                                                
         LA    RF,DATEOPTS         SET UP OPTIONS                               
RQPR1    CLI   0(RF),X'FF'         DATE OPTIONS                                 
         BE    RQPR1X                                                           
         CLC   0(1,RF),PROFDCTL                                                 
         BE    RQPR1X                                                           
         LA    RF,3(RF)                                                         
         B     RQPR1                                                            
RQPR1X   MVC   DATEOPT,1(RF)                                                    
         SPACE 2                                                                
         LA    RF,SORTOPTS                                                      
RQPR2    CLI   0(RF),X'FF'         SORT OPTIONS                                 
         BE    RQPR2X                                                           
         CLC   0(1,RF),PROFSORT                                                 
         BE    RQPR2X                                                           
         LA    RF,2(RF)                                                         
         B     RQPR2                                                            
RQPR2X   MVC   SORTOPT,1(RF)                                                    
         SPACE 2                                                                
         LA    RF,FRMTOPTS                                                      
RQPR3    CLI   0(RF),X'FF'         FORMAT OPTIONS                               
         BE    RQPR3X                                                           
         CLC   0(1,RF),PROFFRMT                                                 
         BE    RQPR3X                                                           
         LA    RF,5(RF)                                                         
         B     RQPR3                                                            
RQPR3X   MVC   FRMTOPT,1(RF)                                                    
         ZIC   RF,FRMTOPT          GET LENGTH OF EACH GRID SLOT                 
         ZIC   RE,FRMTOPT+1        GET NUMBER FOR PTS REPORT                    
         CLC   RTYPE,=C'RS '                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '                                                    
         BNE   RQPR3X2                                                          
         ZIC   RE,FRMTOPT+2        GET NUMBER FOR RS REPORT                     
RQPR3X2  MR    RE,RE               GET LENGTH OF TOTAL GRID                     
         BCTR  RF,0                                                             
         STH   RF,MGLENLIN                                                      
         SPACE 2                                                                
         LA    RF,CNDSOPTS                                                      
         LA    RE,PROGPROF                                                      
RQPR4    CLI   0(RF),X'FF'                                                      
         BE    RQPR4X                                                           
         CLC   0(1,RF),PROFCNDS                                                 
         BE    RQPR4X                                                           
         LA    RF,2(RF)                                                         
         B     RQPR4                                                            
RQPR4X   MVC   CNDSOPT,1(RF)                                                    
         CLI   RSLINNO,C'Y'        CANNOT CONDENSE IF PRINTING                  
         BNE   *+8                    LINE NUMBER                               
         MVI   CNDSOPT,0                                                        
         CLI   RSCOST,C'Y'         OR LINE COST                                 
         BNE   *+8                                                              
         MVI   CNDSOPT,0                                                        
         EJECT                                                                  
*        SET UP OPTON SWITCHS                                                   
         MVC   VARFRMT,DATEOPT                                                  
         MVC   SCNDDTSW,DATEOPT+1                                               
         MVC   SORTFRMT,SORTOPT                                                 
         MVI   SORTREQ,0                                                        
         CLI   SORTFRMT,0                                                       
         BE    *+8                                                              
         MVI   SORTREQ,1                                                        
         MVI   SORTPASS,1                                                       
         MVC   LENGRID,FRMTOPT                                                  
         XC    NOINGRID,NOINGRID                                                
         MVC   NOINGRID+1(1),FRMTOPT+1                                          
         MVC   PGCNDSW,CNDSOPT                                                  
         EJECT                                                                  
         L     RF,APRSDESC                                                      
         CLC   QPROG,=C'RS'                                                     
         BE    *+14                                                             
         CLC   QPROG,=C'RY'                                                     
         BNE   M2RPTX                                                           
         ST    RF,DDESC                                                         
         LA    RF,P1+41                                                         
         ST    RF,DSTAGRID                                                      
         MVC   MEDNUMWK,=F'60'                                                  
         MVC   SUBPROG1,=C'D5'     GET ROTATION SCHEDULE SPECS                  
         MVC   SUBPROG2,=C'01'                                                  
         L     R6,SPECS                                                         
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         MVC   SPECS,DMCB+4                                                     
         XC    NOINGRID,NOINGRID                                                
         MVC   NOINGRID+1(1),FRMTOPT+2                                          
         SPACE 2                                                                
         SPACE 2                                                                
M2RPTX   DS    0H                                                               
         MVC   SVSGRID,DSTAGRID                                                 
         MVC   DSTAGRID,VPLAREA                                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
RQFRSTA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,PROGPROF                                                      
         USING PROFDSCT,RE                                                      
         MVI   RSLINNO,0                                                        
         MVI   RSCOMCTL,0                                                       
         MVI   RSCOST,0                                                         
         CLC   QPROG,=C'RS'                                                     
         BE    *+14                                                             
         CLC   QPROG,=C'RY'                                                     
         BNE   RQFRSTA1                                                         
         MVC   RSLINNO,PROFID      SET LINE NUMBER OPTION                       
         MVC   RSCOMCTL,PROFMSR    SET RS COMMENT CONTROL                       
         MVC   RSCOST,PROFMPC      SET RS COST CONTROL                          
         MVI   PROFID,0                                                         
         MVI   PROFMSR,0                                                        
         MVI   PROFMPC,0                                                        
RQFRSTA1 MVC   DETOPTS,=X'01010101'  SET DETAIL OPTIONS                         
         CLI   QOPT4,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT4,PROFDPC       DETAIL PRINT CONTROL                         
         TM    QOPT4,X'F0'                                                      
         BNO   M21                                                              
         PACK  DUB,QOPT4                                                        
         CVB   R6,DUB                                                           
         SLL   R6,2                                                             
         LA    R6,DETOPT(R6)                                                    
         MVC   DETOPTS,0(R6)                                                    
         CLC   QPROG,=C'RS'                                                     
         BE    *+14                                                             
         CLC   QPROG,=C'RY'                                                     
         BNE   M21                                                              
         MVI   DETOPTS+1,0         RS SUPPRESSES DEMOS                          
*                                                                               
M21      CLI   QOPT7,C'Y'          TEST SUPPRESS COST AND CPP                   
         BNE   *+10                                                             
         XC    DETOPTS+2(2),DETOPTS+2  YES                                      
*                                                                               
         XC    SUMOPTS,SUMOPTS                                                  
         CLI   PROFMSR,C'S'                                                     
         BE    *+14                                                             
         CLC   QSTA(3),=C'ALL'                                                  
         BNE   M2AA                                                             
         MVC   SUMOPTS,=X'010101'                                               
         CLI   QOPT5,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT5,PROFMPC       MARKET PRINT CONTROL                         
         TM    QOPT5,X'F0'                                                      
         BNO   M2AA                                                             
         PACK  DUB,QOPT5                                                        
         CVB   R6,DUB                                                           
         MH    R6,=H'3'                                                         
         LA    R6,SUMOPT(R6)                                                    
         MVC   SUMOPTS,0(R6)                                                    
*                                                                               
M2AA     CLI   QOPT7,C'Y'          TEST SUPPRESS COST AND CPP                   
         BNE   *+8                                                              
         MVI   SUMOPTS+1,0         YES-SUPPRESS DOLLARS                         
*                                                                               
         MVI   SUBPSW,0                                                         
         MVC   MSOPT,SPACES        INITIALIZE MS OPTIONS                        
         MVC   SVOPTS,QOPT1        SAVE REPORT OPTIONS                          
         MVC   SUBPROG1,=C'M2'                                                  
         CLC   QSTA(3),=C'ALL'                                                  
         BNE   M2AA3                                                            
         CLC   QPRD,=C'POL'        POL REQUEST                                  
         BNE   M2AA1                NO                                          
         TM    PROFPMS,X'0F'        YES - ANY MEDIA SUMMARY                     
         BZ    M2AA3                                                            
         MVC   SUBPROG1+1(1),PROFPMS                                            
         B     M2AA2                                                            
M2AA1    TM    PROFBMS,X'0F'                                                    
         BZ    M2AA3                                                            
         MVC   SUBPROG1+1(1),PROFBMS                                            
M2AA2    MVI   SUBPSW,1                                                         
*                                                                               
M2AA3    CLI   QOPT7,C'Y'          TEST OPTION TO SUPPRESS DOLLARS              
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
         CLI   SUBPROG1,C'C'                                                    
         BNE   *+8                                                              
         MVI   SUBPROG1,C'M'                                                    
         MVC   WORK+2(2),SUBPROG1                                               
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         CLI   COFFICE,X'41'                                                    
         BL    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  R6                                                               
         GOTO1 GETPROF,DMCB,WORK,MSPROF,DATAMGR                                 
         MVC   WORK(4),=C'SD5A'                                                 
         NI    WORK,X'BF'                                                       
         XC    D5APROF,D5APROF                                                  
         GOTO1 GETPROF,DMCB,WORK,D5APROF,DATAMGR                                
         CLI   QMCOM,C' '          TEST MEDIA COMMENTS OPTION SET               
         BNH   *+10                                                             
         MVC   D5AMCOM,QMCOM       YES-OVERRIDE THE D5A OPTION                  
         CLI   SVXLIN,0                                                         
         BE    *+10                                                             
         MVC   D5AXLIN,SVXLIN                                                   
         NI    D5APROF,X'0F'                                                    
         CLI   D5APROF,4                                                        
         BH    *+8                                                              
         MVI   D5APROF,4                                                        
         LA    R1,3                SET DEFAULT VALUES                           
         LA    R8,D5ASUPR                                                       
         BAS   R9,MAKYORN                                                       
*                                                                               
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
         SPACE 2                                                                
         MVC   SUBPROG2,=C'01'                                                  
         L     R6,VSUBPARA                                                      
         ST    R6,CURPGPTR         SAT TO START OF SAVE AREA                    
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RE,DMCB+4                                                        
         ST    RE,SVPH01                                                        
         MVC   CURPH01(4),CURPGPTR                                              
         MVC   CURPH01+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         MVC   SUBPROG2,=C'02'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
         L     RE,DMCB+4                                                        
         ST    RE,SVPH02                                                        
         MVC   CURPH02(4),CURPGPTR                                              
         MVC   CURPH02+4(8),DMCB                                                
         L     R6,CURPGPTR                                                      
         A     R6,DMCB                                                          
         ST    R6,CURPGPTR                                                      
         MVC   SUBPROG2,=C'04'                                                  
         GOTO1 REPCALOV,DMCB,(RA),SUBPROG                                       
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
         LA    R1,1272                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   SVPH04,DMCB+4                                                    
         XIT1                                                                   
         DROP  RE                                                               
         EJECT                                                                  
MAKYORN  CLI   0(R8),C'Y'          SET DEFUALT PROFILE OPTS                     
         BE    *+8                                                              
         MVI   0(R8),C'N'                                                       
         LA    R8,1(R8)                                                         
         BCT   R1,MAKYORN                                                       
         BR    R9                                                               
         LTORG                                                                  
         EJECT                                                                  
EFRSTC   NTR1  BASE=*,LABEL=*                                                   
         CLC   QPRD,=C'ALL'                                                     
         BE    EFPOLOK                                                          
         CLC   QPRD,=C'POL'                                                     
         BE    EFPOLOK                                                          
         MVC   REASTART(3),QPRD    FORCE TO READ ALL PRODUCTS                   
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   QPRD,=C'POL'                                                     
         MVC   QPRD(L'SP@POL),SP@POL                                            
         DROP  RE                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         MVC   QPRD,REASTART                                                    
EFPOLOK  DS    0C                                                               
         MVC   REASTART,QSTART                                                  
         MVC   PASSQST(12),QSTART                                               
         MVC   PASSTAB(12),QSTART                                               
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
         MVC   MEDEXTAX,SPOTPROF+12                                             
         MVC   MEDNUMWK,=F'60'     CREATE BTS TABLES                            
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
EFRSTA   DS    0H                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   SVRDTE,MEDPERD      SAVE REQUEST DATES                           
         MVI   PASS,0                                                           
         MVI   MAXPASS,1                                                        
         CLI   VARFRMT,0           FIXED FORMAT                                 
         BNE   EFRSTX               NO - EXIT                                   
         LA    RE,1                                                             
         LA    R6,PASSTAB                                                       
         L     R9,MEDAFRST                                                      
SETPASS  STC   RE,MAXPASS          SAVE HIGHEST PASS                            
         LH    R8,NOINGRID         SET TO NUMBER OF WEEKS IN PASS               
         GOTO1 DATCON,DMCB,(X'02',(R9)),(X'00',0(R6))                           
*                                                                               
SETPASS1 GOTO1 DATCON,DMCB,(X'02',2(R9)),(X'00',6(R6))                          
*                                                                               
SETPASS2 LA    R9,12(R9)                                                        
         C     R9,MEDALAST                                                      
         BH    SETPASSX                                                         
         CLI   0(R9),0                                                          
         BE    SETPASS2                                                         
         BCT   R8,SETPASS1                                                      
*                                                                               
         ZIC   RE,MAXPASS                                                       
         LA    RE,1(RE)            BUMP MAXPASS                                 
         LA    R6,12(R6)           BUMP DATE SAVE                               
         B     SETPASS                                                          
*                                                                               
SETPASSX MVC   PASSQST(12),PASSTAB                                              
*                                                                               
EFRSTX   MVC   QSTART(12),PASSQST                                               
*                                                                               
* INITIALIZE D5 PROFILES                                                        
*                                                                               
         MVI   IDSW,0              RESET MULTIPLE IDS ON PAGE                   
         MVI   IDTOTALS,C'Y'       AND DEFAULT TO TOTALS BY ID                  
         CLI   QBYID,C'Y'          ID SEQUENCE REQUESTED                        
         BNE   M4A                                                              
         CLI   PROGPROF+14,C'N'    MULTIPLE IDS ON PAGE OPT.                    
         BNE   M4A                 NO                                           
         MVI   IDSW,1              TURN ON MULTIPLE IDS ON PAGE                 
         MVI   IDTOTALS,C'N'       PRINT ID TOTALS                              
*                                                                               
M4A      CLI   QOPT5+1,C' '        SPILL REPORTING OVERRIDE                     
         BE    *+14                                                             
         MVC   PROGPROF+15(1),QOPT5+1                                           
         NI    PROGPROF+15,X'0F'                                                
         CLI   PROGPROF+15,C'0'    SPILL OVERRIDE                               
         BE    *+10                                                             
         MVC   SPOTPROF+5(1),PROGPROF+15                                        
         CLI   SPOTPROF+1,0                                                     
         BNE   *+8                                                              
         MVI   SPOTPROF+1,C'N'                                                  
         CLI   SPOTPROF+5,20                                                    
         BL    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         SR    RE,RE                                                            
         IC    RE,BPRD                                                          
         CLI   BPRD,X'FF'          GET DEMO NAMES FOR PRODUCT                   
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,28(RE,RF)                                                     
         XC    DNAMES,DNAMES                                                    
         NI    D5APROF,X'0F'                                                    
         LA    R9,14                                                            
         CLI   BPRD,X'FF'                                                       
         BE    *+10                                                             
         ZIC   R9,D5APROF                                                       
         SPACE 2                                                                
* LOOK UP NEW FORMAT DEMO NAMES                                                 
NEWDNAM  ST    RE,FULL                                                          
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         XC    0(255,R6),0(R6)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
*        CANADA USES A DIFFERENT DEMO LIST                                      
         L     RF,ADCLT            SET UP FOR PARAMETER LIST                    
         USING CLTHDR,RF                                                        
         CLI   CEXTRA,C'U'         TEST US DEMOS                                
         BE    NEWDCDX                                                          
         DROP  RF                                                               
         L     RF,ADAGY                                                         
         LA    RF,AGYPROF+7-AGYHDR(RF)                                          
         CLI   0(RF),C'C'          TEST CANADIAN AGY                            
         BNE   NEWDCDX                                                          
         MVI   DBSELMED,C'C'       SET CANADIAN MEDIA                           
         DROP  R6                                                               
NEWDCDX  DS    0C                                                               
                                                                                
         L     R8,ADEST                                                         
         USING ESTHDR,R8                                                        
         L     R6,FULL                                                          
         GOTO1 DEMOCON,DMCB,((R9),(R6)),(2,DNAMES),(C'S',ADBLOCK),     *        
               EUSRNMS,VNONTNMS                                                 
         DROP  R8                                                               
NEWDNAM1 L     RE,FULL                                                          
         SR    R1,R1                                                            
NEWDNAM2 CLI   1(RE),0                                                          
         BE    NEWDNAM3                                                         
         LA    R1,1(R1)                                                         
         LA    RE,3(RE)                                                         
         BCT   R9,NEWDNAM2                                                      
*EWDNAM2 LA    R1,1(R1)                                                         
*        SRL   R1,1                                                             
NEWDNAM3 LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                                                             
         ST    R1,NODEMS                                                        
         XIT1                                                                   
         LTORG                                                                  
********************************************************************            
* THIS ROUTINE IS CALLED THE FIRST TIME THERE IS BUY ACTIVITY FOR  *            
* A MARKET                                                         *            
********************************************************************            
         SPACE 1                                                                
MKTFIRST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,ADCOMREC         CLEAR THE COMMENT RECORD                     
         XC    0(13,R1),0(R1)                                                   
         CLI   D5AMCOM,C'Y'        TEST MEDIA COMMENTS NEEDED                   
         BNE   GETMCX                                                           
         MVI   BCMTYPE,C'M'        SET COMMENT BLOCK                            
         MVC   BCMCLT,BCLT                                                      
         XC    BCMPGR,BCMPGR                                                    
         MVC   BCMPRD,BPRD                                                      
         MVC   BCMEST,BEST                                                      
         CLI   BEST,0                                                           
         BNE   GETMC2                                                           
         LA    R1,ESTLST                                                        
         CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         MVC   BCMEST,0(R1)                                                     
*                                                                               
GETMC2   XC    BCMMKT,BCMMKT                                                    
         MVI   BCMSTA,0                                                         
         MVC   BCMSTA+1(2),BMKT                                                 
         GOTO1 GETCOM              GET MEDIA COMMENTS                           
*                                                                               
GETMCX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
GETCAP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
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
         L     R7,=A(DICSECT)                                                   
         USING DICSECT,R7                                                       
         CLI   PKGIND,1                                                         
         BNE   *+14                                                             
*---->   MVC   PKGAREA(7),=C'PKG MST'                                           
         MVC   PKGAREA(L'SP@PKMST),SP@PKMST                                     
         B     GETCAPX                                                          
         CLI   PKGIND,3                                                         
         BNE   *+14                                                             
*---->   MVC   PKGAREA(7),=C'ORB MST'                                           
         MVC   PKGAREA(L'SP@ORMST),SP@ORMST                                     
         B     GETCAPX                                                          
         CLI   PKGIND,5                                                         
         BNE   *+14                                                             
*---->   MVC   PKGAREA(7),=C'REV MST'                                           
         MVC   PKGAREA(L'SP@RVMST),SP@RVMST                                     
         B     GETCAPX                                                          
         CLI   PKGIND,7                                                         
         BE    GETCAPX                                                          
*                                                                               
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         CLI   BDMGDATE,X'C0'                                                   
         BH    GETCAP4                                                          
         DROP  RE                                                               
         CLI   PKGIND,8                                                         
         BNE   GETCAP3                                                          
         MVC   PKGAREA(L'SP@MG),SP@MG                                           
         B     *+10                                                             
GETCAP3  DS    0H                                                               
         MVC   PKGAREA(L'SP@MST),SP@MST                                         
         DROP  R7                                                               
         SR    R0,R0                                                            
         IC    R0,PKGLINES                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PKGAREA+4(3),DUB+6(2)                                            
         CLI   PKGIND,8                                                         
         BNE   GETCAPX                                                          
         DROP  R5                                                               
         L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING BDELEM,R5                                                        
         GOTO1 DATCON,DMCB,(X'02',BDMGDATE),(X'08',PKGAREA+8)                   
         B     GETCAPX                                                          
         DROP  R5                                                               
*                                                                               
GETCAP4  L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING BDELEM,RE                                                        
         CLI   QMGA,C'Y'           MAKE OPTIONAL FOR NOW                        
         BNE   GETCAPX                                                          
         CLI   D5ASUPMG,C'Y'       TEST SUPPRESS MAKEGOOD CAPTION               
         BE    GETCAPX                                                          
         CLI   BDMGDATE,X'C0'                                                   
         BNH   GETCAPX                                                          
         L     RE,=A(MGTABLE)                                                   
         CLI   0(RE),C'*'                                                       
         BL    GETCAPX                                                          
         MVC   PKGAREA(7),=C'**MKGD '                                           
         MVC   PKGAREA+7(6),=C'GROUP '                                          
         MVC   PKGAREA+13(2),BDMGDATE                                           
         MVI   PKGAREA+15,C'*'                                                  
         MVC   PKGAREA+13(2),0(RE)                                              
GETCAPX  DS    0H                                                               
         DROP  RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
GETCMT   L     R5,ADBUY                                                         
         LA    R5,24(R5)                                                        
         USING COMELEM,R5                                                       
         LA    RE,COMAREA                                                       
         LA    RF,400                                                           
         XCEF                                                                   
         MVI   NUMCOM,0                                                         
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
         CLI   RSCOMCTL,C'1'                                                    
         BE    *+8                                                              
         CLI   PROFCC,C'1'                                                      
         BNE   GETCOM5                                                          
         CLI   CMDATA,C'$'         ACCEPT COMMENT IF $,4, OR 5                  
         BE    *+8                                                              
         CLI   CMNUM,4                                                          
         BE    *+8                                                              
         CLI   CMNUM,5                                                          
         BNE   GETCOM3                                                          
GETCOM5  CLI   RSCOMCTL,C'2'                                                    
         BE    *+8                                                              
         CLI   PROFCC,C'2'                                                      
         BNE   *+14                                                             
         CLC   CMDATA(8),=C'COMMENT-'                                           
         BNE   GETCOM3                                                          
         CLC   =C'X-',CMDATA       NEVER PRINT COMMENTS THAT                    
         BE    GETCOM3             START WITH X-                                
         DROP  R7                                                               
         LA    R4,COMAREA                                                       
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
         ZIC   RE,NUMCOM           BUMP COMMENT COUNTER                         
         LA    RE,1(RE)                                                         
         STC   RE,NUMCOM                                                        
         EX    R7,*+8                                                           
         B     GETCOM3                                                          
         MVC   0(0,R4),CMDATA                                                   
GETCOMX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
         EJECT                                                                  
GETSADDR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
GETS10   XC    SPBUFSTA,SPBUFSTA                                                
         MVI   SPLPRINT,1          SET TO PRINT ORIG.                           
         MVI   FBSTA,C'Y'                                                       
         MVI   INVFILM,0                                                        
         XC    STAUNA,STAUNA                                                    
         XC    UNATOT,UNATOT                                                    
         MVC   SAKYSAVE,KEY                                                     
         L     R6,ADREP                                                         
         USING REPREC,R6                                                        
         XC    REPKREP,REPKREP                                                  
         MVC   RNAME,SPACES                                                     
         MVC   REPNM,SPACES                                                     
         DROP  R6                                                               
*                                                                               
         L     R6,ADSTAT                                                        
         USING STAREC,R6                                                        
         MVC   WORK,STRFREP                                                     
         CLC   STRFREP,=C'000'                                                  
         BNE   GETS20                                                           
         DROP  R6                                                               
*                                                                               
         LA    R6,KEY                                                           
         USING ADDRREC,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   ADDKTYPE,C'A'                                                    
         MVC   ADDKMED,QMED                                                     
         MVC   ADDKCALL,STA                                                     
         CLI   ADDKCALL+4,C' '                                                  
         BNE   *+10                                                             
         MVC   ADDKCALL+4(1),QMED                                               
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         MVI   ADDKCALL+4,C'C'                                                  
         MVC   ADDKAGY,AGY                                                      
*                                                                               
         GOTO1 HIGHSTAD                                                         
         L     R6,ADSTATAD                                                      
         CLC   ADDKCALL(4),STA                                                  
         BE    GETS22                                                           
         MVC   ANAME(86),SPACES                                                 
         L     RF,=A(DICSECT)                                                   
         USING DICSECT,RF                                                       
*---->   MVC   ANAME(15),=C'ADDRESS MISSING'                                    
         MVC   ANAME(L'SP@ADRMS),SP@ADRMS                                       
         DROP  RF                                                               
         B     GETS22                                                           
*                                                                               
GETS20   GOTO1 VGETREP             GET REP ADDRESS                              
*                                                                               
GETS22   DS    0H                                                               
         CLC   PRVSTA,STAPRINT     SAME STATION                                 
         BNE   *+12                                                             
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    GETS30                                                           
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVI   LINE,0                                                           
*                                                                               
GETS30   DS    0H                                                               
         MVC   PRVSTA,STAPRINT                                                  
*                                                                               
         L     R6,ADSTATAD                                                      
         LA    RF,A3LINE                                                        
         SH    RF,=H'4'                                                         
GETS32   CLI   0(RF),0                                                          
         BE    *+8                                                              
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     GETS32                                                           
         LA    RF,1(RF)                                                         
         MVI   0(RF),C','                                                       
         MVC   1(3,RF),A3LINE                                                   
         MVC   A3LINE(L'ABIGZIP),ABIGZIP                                        
         DROP  R6                                                               
*                                                                               
GETS34   L     R6,ADSTAT                                                        
         USING STAREC,R6                                                        
         MVC   WORK,SCONREP                                                     
         CLC   SCONREP,=C'000'                                                  
         BE    GETSX                                                            
         GOTO1 VGETREP                                                          
*                                                                               
GETSX    MVC   KEY,SAKYSAVE                                                     
         XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         DROP  R6                                                               
SAKYSAVE DS    CL32                                                             
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
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETID                                                            
GETID1   L     R6,FULL                                                          
         CLI   0(R6),0                                                          
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
         CLC   1(80,R6),0(R6)                                                   
         BE    GETID2                                                           
         LA    R6,132(R6)                                                       
         B     *-14                                                             
GETID2   MVC   11(12,R6),3(RE)                                                  
         MVI   10(R6),C'='                                                      
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVC   0(10,R6),CTITLE                                                  
         DROP RE                                                                
GETIDX   DS    0C                                                               
         LA    R4,COMAREA                                                       
         LA    R5,5                                                             
         L     R6,FULL                                                          
         CLI   0(R6),0                                                          
         BNE   *+8                                                              
         MVI   0(R6),C' '                                                       
         CLC   1(76,R6),0(R6)                                                   
         BE    COMPRNT1                                                         
         LA    R6,132(R6)                                                       
         B     *-14                                                             
COMPRNT1 OC    0(76,R4),0(R4)                                                   
         BZ    *+14                                                             
         MVC   0(76,R6),0(R4)                                                   
         LA    R6,132(R6)                                                       
         LA    R4,80(R4)                                                        
         BCT   R5,COMPRNT1                                                      
                                                                                
* PRINT OUT NEW MISSED GROUP CAPTIONS                                           
         ST    R6,FULL                                                          
         CLI   QMGA,C'Y'           MAKE IT OPTIONAL FOR NOW                     
         BNE   CPRMGAX                                                          
         CLI   D5ASUPMG,C'Y'       TEST SUPPRESS MAKEGOOD CAPTIONS              
         BE    CPRMGAX                                                          
         L     R7,MEDBUFF          PRINT OUT MISSED GROUP CAPTION               
         USING MEDBLOCK,R7                                                      
         L     R9,=A(MGTABLE)                                                   
         USING MGENTRYD,R9                                                      
         OC    0(L'MGECODE,R9),0(R9) EMPTY - JUST BYPASS                        
         BZ    CPRMGAX                                                          
         L     R6,FULL                                                          
         MVI   0(R6),0                                                          
CPRMGA1  CLI   MGETYPE,X'00'       ONLY FOR MISSED SPOTS                        
         BNE   CPRMGA4                                                          
         CLC   MGECODE,=C'PR'      TEST TYPE = PREEMPT                          
         BE    CPRMGA4             YES - BYPASS                                 
         CLC   MGECODE,=C'*P'                                                   
         BE    CPRMGA4             YES - BYPASS                                 
         CLC   MGEDATE,MEDPERD     IN REQUESTED TIME PERIOD                     
         BL    CPRMGA4                                                          
         CLC   MGEDATE,MEDPERD+2                                                
         BH    CPRMGA4                                                          
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
         L     RF,FULL             HANDLE LINE SPANNING                         
         LA    RF,90(RF)                                                        
         CR    R6,RF                                                            
         BL    CPRMGA4                                                          
*                                                                               
CPRMGA3  L     R6,FULL                                                          
         LA    R6,132(R6)                                                       
         ST    R6,FULL                                                          
         LA    R0,P14                                                           
         CR    R6,R0                                                            
         BH    COMPEX                                                           
*                                                                               
CPRMGA4  LA    R9,MGERECL(R9)      GET NEXT ITEM                                
         OC    0(L'MGECODE,R9),0(R9)                                            
         BNZ   CPRMGA1                                                          
         C     R6,FULL                                                          
         BE    CPRMGAX                                                          
         L     R6,FULL                                                          
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
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETORB                                                           
GETORB1  LA    R5,ORBDAY                                                        
         USING ORBDAY,R5                                                        
GETORB1A CLC   1(130,R6),0(R6)                                                  
         BE    *+12                                                             
         LA    R6,132(R6)                                                       
         B     GETORB1A                                                         
         ZIC   R7,1(RE)            GET END OF ELEMENT                           
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
         GOTO1 CODAY,DMCB,ORBDAY,(R6)                                           
         LA    R6,9(R6)                                                         
         XC    DMCB,DMCB                                                        
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
         CURED HALF,(6,(R6)),CURTAB,DMCB=CDMCB                                  
         MVI   CURTAB+3,0                                                       
GETORB4  LA    R6,7(R6)                                                         
         GOTO1 SQUASHER,DMCB,(R9),36                                            
         LA    R5,16(R5)                                                        
         BCT   R3,GETORB3                                                       
         LR    R6,R4                                                            
         LA    R6,132(R6)                                                       
         MVI   0(R6),0                                                          
         B     GETORB2                                                          
GETORBX  DS    0C                                                               
         EJECT                                                                  
* SET UP UPGRADE EXPRESSION                                                     
         CLI   QPROG,C'U'                                                       
         BNE   COMPEX                                                           
         CLI   QBOOK1,C' '                                                      
         BNE   COMPEX                                                           
         CLI   DETOPTS+1,1                                                      
         BNE   COMPEX                                                           
         GOTO1 VUDESC,DMCB,(RA),(R6)                                            
COMPEX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* PRINT NETWORK COST OVERRIDES AS CAPTIONS BELOW THE GRID                       
*=================================================================              
                                                                                
PRCOSTOV NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,=A(NETCOVR)                                                   
*                                                                               
PRCOV2   OC    0(6,R4),0(R4)                                                    
         JZ    EXIT                                                             
         CLC   0(2,R4),PASSSD2     TEST PRIOR TO PASS START                     
         BNL   PRCOV4                                                           
         AHI   R4,6                                                             
         B     PRCOV2                                                           
*                                                                               
PRCOV4   CLC   0(2,R4),PASSED2     TEST AFTER PASS END                          
         JH    EXIT                YES - DONE                                   
*                                                                               
PRCOV6   MVC   P1+4(20),=C'** COST OVERRIDES **'                                
*                                                                               
         LA    R5,P1+30                                                         
*                                                                               
PRCOV10  ST    R5,FULL             SAVE PRINT LINE ADDR                         
         LHI   R6,4                                                             
*                                                                               
PRCOV14  GOTO1 DATCON,DMCB,(2,0(R4)),(4,0(R5))  GET MMMDD                       
*                                                                               
         LA    RF,6(R5)            POINT TO NEXT PRINT POSN                     
         BCTR  RF,0                BACK UP ONE POSN                             
         MVI   0(RF),C'-'                                                       
         SR    R0,R0                                                            
         IC    R0,2(R4)           SPOT SEQNUM                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,RF),DUB                                                      
         AHI   RF,4                                                             
*                                                                               
PRCOV16  SR    R0,R0                                                            
         ICM   R0,7,3(R4)          GET COST AMOUNT                              
         BNZ   PRCOV18                                                          
         MVC   0(3,RF),=C'$0 '                                                  
         B     PRCOV20                                                          
*                                                                               
PRCOV18  EDIT  (R0),(9,0(RF)),2,ALIGN=LEFT,FLOAT=$,ZERO=NOBLANK                 
         AR    RF,R0               POINT TO END                                 
         AHI   RF,-3                                                            
         CLC   0(3,RF),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   0(3,RF),SPACES                                                   
*                                                                               
PRCOV20  AHI   R4,6                NEXT COST OVERRIDE                           
         OC    0(6,R4),0(R4)                                                    
         BZ    PRCOVX                                                           
         CLC   0(2,R4),PASSED2     TEST AFTER PASS END                          
         BH    PRCOVX                                                           
         AHI   R5,20               NEXT PRINT POSN                              
         BCT   R6,PRCOV14                                                       
*                                                                               
         L     R5,FULL                                                          
         AHI   R5,132              NEXT PRINT LINE                              
         LA    R6,P14              LAST PRINT LINE                              
         CR    R5,R6               LESS THAN OR EQUAL TO P14?                   
         BNH   PRCOV10             YES - OK TO PROCEED                          
         GOTO1 REPORT              PRINT THE 14 LINES                           
         LA    R5,P1+30            START PRINTING FROM P1                       
         MVI   LINE,1              SO WE DON'T PRINT HEADLINES AGAIN            
         B     PRCOV10             NOW - OK TO PROCEED                          
*                                                                               
PRCOVX   GOTO1 REPORT                                                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
GETGL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         MVC   MEDNUMWK,=F'60'                                                  
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         LA    RE,KEY                                                           
         L     RE,ADGOAL                                                        
         USING GOALREC,RE                                                       
         MVC   MEDBRAND,BPRD                                                    
         MVC   MEDSPTLN,GKEYSLN                                                 
         DROP  RE                                                               
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         L     R9,WEIGHT                                                        
         GOTO1 MEDMKTWT,DMCB,(RA),(R9)                                          
         LA    R3,PRTLINE                                                       
         L     R5,MEDAFRST                                                      
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
         CLC   QPROG,=C'RS'                                                     
         BE    M164T                                                            
         CLC   QPROG,=C'RY'                                                     
         BE    M164T                                                            
         CLI   PROFDPT,C'Y'        DAYPART ANALYSIS REQUIRED                    
         BNE   M164T                NO - PUT OUT TOTALS ONLY                    
         DROP  R8                                                               
         L     R8,BUFFBUFF                                                      
         BAS   R9,M16PUT                                                        
* PUT OUT PRODUCT GROUP DETAILS                                                 
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,M16PUT                                                        
M164T    L     R8,BUFFBUFF                                                      
         MVC   SUMDPGNO(9),=9X'FF'                                              
         MVI   SUMCODE,X'90'                                                    
         BAS   R9,M16PUT                                                        
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,M16PUT                                                        
* GET NEXT BUFFALO ITEM                                                         
M165     LA    R5,12(R5)                                                        
         B     M163                                                             
M16X     B     GOALX                                                            
         DROP  RE                                                               
M16PUT   CLI   SUMCODE,X'91'                                                    
         BER   R9                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),PRTLINE                                
         BR    R9                                                               
GOALX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
GETREP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
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
         L     R6,ADREP                                                         
         L     RE,ADSTAT                                                        
         USING STAREC,RE                                                        
         CLC   REPKREP,WORK                                                     
         BE    GETREP2                                                          
         MVC   RNAME,SPACES                                                     
         MVC   REPNM,SPACES                                                     
         B     GETRX                                                            
GETREP2  DS    0H                                                               
         CLC   REPKREP,STRFREP                                                  
         BNE   GETRX                                                            
         DROP  RE                                                               
         L     R6,ADREP                                                         
         L     RE,ADSTATAD                                                      
         USING ADDRREC,RE                                                       
         MVC   ANAME,RNAME                                                      
         MVC   A1LINE,R1LINE                                                    
         MVC   A2LINE,R2LINE                                                    
         MVC   A3LINE(8),R3LINE                                                 
         MVC   ABIGZIP,RBIGZIP                                                  
GETRX    MVC   KEY,RPKYSAVE                                                     
         XIT1                                                                   
         DROP  R6                                                               
         DROP  RE                                                               
         LTORG                                                                  
RPKYSAVE DS    CL32                                                             
         EJECT                                                                  
STATOTC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         MVI   CONEND,C'N'         SET END OF STATION                           
         OC    SVIDADR,SVIDADR     CHECK FOR CONTRACTS ACTIVE                   
         BZ    STNOCON             NO CONTRACTS ACTIVE                          
         MVI   CONEND,C'Y'         SET CONTRACT END                             
         SPACE 2                                                                
         L     R5,BUYLIST          CHECK FOR END OF STATION                     
         CLC   SVIDADR(1),0(R5)                                                 
         BE    *+12                                                             
         LA    R5,13(R5)                                                        
         B     *-14                                                             
         CLI   13(R5),0            TEST ANY MORE IDS                            
         BNE   *+8                                                              
         MVI   CONEND,C'N'         SET END OF STATION                           
*                                                                               
         CLI   IDTOTALS,C'Y'       WANT CONTRACT TOTALS                         
         BE    STNOCON             YES - TREAT AS END OF STATION                
         CLI   CONEND,C'Y'                                                      
         BE    STBYPID                                                          
*                                                                               
STNOCON  L     R6,=A(DICSECT)                                                   
         USING DICSECT,R6                                                       
         CLI   INVFILM,0                                                        
         BE    STAINVFX                                                         
*---->   MVC   P(45),=C'***THERE ARE    INVALID FILMS ON THIS STATION'          
         MVC   P(L'SP@THE01),SP@THE01                                           
         EDIT  INVFILM,(2,P+13)                                                 
         CLI   INVFILM,1                                                        
         BNE   *+10                                                             
*---->   MVC   P(45),=C'*** THERE IS  1 INVALID FILM ON THIS STATION '          
         MVC   P(L'SP@THE02),SP@THE02                                           
         GOTO1 REPORT                                                           
STAINVFX DS    0C                                                               
         OC    STAUNA,STAUNA                                                    
         BZ    UNAEXIT                                                          
         EDIT  STAUNA,(4,P)                                                     
*---->   MVC   P+5(17),=C'UNALLOCATED SPOTS'                                    
         MVC   P+5(L'SP@UNSPT),SP@UNSPT                                         
         GOTO1 REPORT                                                           
UNAEXIT  DS    0C                                                               
         XC    STAUNA,STAUNA                                                    
         MVC   MEDNUMWK,NUMWK      SET UP FOR MEDDATE                           
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'4'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'128'                                                 
         CLI   D5APROF,5                                                        
         BL    *+10                                                             
         MVC   MEDLCHNK,=F'136'    MAX 6 DEMOS                                  
         MVC   QSTART(12),PASSQST  SET PASS START AND END                       
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   QSTART(12),REASTART RESTORE REQUEST START AND END                
STATOT1  OC    STASPOT,STASPOT                                                  
         BZ    M10B53                                                           
         MVI   P1,0                                                             
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'5'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XC    P1,P1                                                            
         XC    P2,P2                                                            
         XC    P3,P3                                                            
         LA    R5,MEDPERD                                                       
         GOTO1 DATCON,DMCB,(X'02',(R5)),(X'08',P2+14)                           
         MVI   P2+22,C'-'                                                       
         GOTO1 DATCON,DMCB,(X'02',2(R5)),(X'08',P2+23)                          
         MVC   P2(7),STACAP                                                     
         MVC   P2(9),BIGSTA                                                     
*---->   MVC   P2+10(3),=C'TOT'                                                 
         MVC   P2+10(L'SP3TOTAL),SP3TOTAL                                       
         CLI   CONEND,C'Y'         END OF CONTRACT                              
         BNE   STATOT2                                                          
         CLI   QBYID,C'Y'                                                       
         BNE   STATOT2                                                          
         MVI   P2+12,C' '                                                       
         MVC   P2(12),BUYIDNAM                                                  
*---->   MVC   P3(09),=C'**TOTAL**'                                             
         MVC   P3(L'SP9TOTAL),SP9TOTAL                                          
         MVI   P5,0                                                             
         MVI   P6,0                                                             
STATOT2  DS    0C                                                               
*---->   MVC   P4+25(6),=C'TLCSTS'                                              
         MVC   P4+25(L'SP@TLCST),SP@TLCST                                       
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
*---->   MVC   P4+25(7),=C'BRDCSTS'                                             
         MVC   P4+25(L'SP@BRDCS),SP@BRDCS                                       
         DROP  R6                                                               
         EDIT  STASPOT,(4,P4+20)                                                
*                                                                               
         CLC   RTYPE,=C'RS '                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '                                                    
         BNE   STATOT10                                                         
         CLI   RSCOST,C'Y'         RS USES THIS FLAG FOR COSTS                  
         BE    STATOT12                                                         
         B     M10B                                                             
*                                                                               
STATOT10 CLI   DETOPTS+2,0         SUPPRESS COST                                
         BE    M10B                 YES                                         
*                                                                               
STATOT12 LA    R6,P3+14                                                         
*                                                                               
         L     RF,STACOST                                                       
         C     RF,=F'99999999'                                                  
         BH    M101A                                                            
*---->   EDIT  STACOST,(10,(R6)),2,MINUS=YES,FLOAT=$                            
         MVI   CURTAB+3,2                                                       
         CURED STACOST,(10,(R6)),CURTAB,DMCB=CDMCB,CURSYMB=YES,FLOAT=-          
         MVI   CURTAB+3,0                                                       
         B     M10B                                                             
         SPACE 2                                                                
M101A    L     RF,STACOST          DROP PENNIES                                 
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
*---->   EDIT  (RF),(10,(R6)),,MINUS=YES,FLOAT=$                                
         MVI   CURTAB+3,0          SET FOR NO DECIMALS                          
         CURED (RF),(10,(R6)),CURTAB,DMCB=CDMCB,CURSYMB=YES,FLOAT=-             
         SPACE 2                                                                
* BUILD GRID                                                                    
M10B     CLC   RTYPE,=C'RS '                                                    
         BE    M10B52                                                           
         CLC   RTYPE,=C'RY '                                                    
         BE    M10B52                                                           
         LA    R5,STAGRID                                                       
         LA    R1,14                                                            
         L     R6,PSTAGRID                                                      
         LA    R4,MEDMON01                                                      
M10B1    OC    0(4,R5),0(R5)                                                    
         BZ    M10B2                                                            
         GOTO1 DATCON,DMCB,(X'02',2(R4)),(X'08',WORK)                           
         MVC   1(3,R6),WORK                                                     
         LA    R6,264(R6)                                                       
         L     R8,0(R5)                                                         
         EDIT  (R8),(4,(R6))                                                    
         SH    R6,=H'264'                                                       
         LA    R6,4(R6)                                                         
         BCT   R1,M10B2                                                         
         B     M10B2A2                                                          
M10B2    LA    R5,4(R5)                                                         
M10B2A1  LA    R4,12(R4)                                                        
         LA    RE,MEDMON13                                                      
         CR    R4,RE                                                            
         BH    M10B2A                                                           
         OC    0(4,R4),0(R4)                                                    
         BZ    M10B2A1                                                          
         B     M10B1                                                            
M10B2A2  L     R6,PSTAGRID                                                      
         A     R6,=F'528'                                                       
         LA    R1,14                                                            
         B     M10B1                                                            
         B     M10B2                                                            
M10B2A   DS    0H                                                               
         SPACE 2                                                                
* CALCULATE DEMOS AND CPM                                                       
         LA    R3,PRTLINE                                                       
         XC    PRTLINE,PRTLINE                                                  
         MVC   SUMDL(8),STACOST                                                 
         MVC   SUMD1(32),STADEMS                                                
         MVC   FULL,=F'1'                                                       
         GOTO1 VCALCPP,DMCB,FULL                                                
         GOTO1 VEDTDEMS                                                         
         LA    R8,P2                                                            
         CLI   DETOPTS+1,1         DEMOS REQUESTED                              
         BNE   M10B52                                                           
         ST    R8,FULL             MOVE DEMOS AND CPP TO PRINT                  
         LA    RE,PLD1                                                          
         LA    R1,DNAMES                                                        
         LA    R6,2                                                             
M10B21   MVC   101(5,R8),0(RE)                                                  
         MVC   93(7,R8),0(R1)                                                   
         MVC   121(5,R8),11(RE)                                                 
         MVC   113(7,R8),7(R1)                                                  
         LA    R1,14(R1)                                                        
         LA    RE,22(RE)                                                        
         LA    R8,132(R8)                                                       
         BCT   R6,M10B21                                                        
         L     R8,FULL                                                          
M10B51   CLI   DETOPTS+3,1         CPP/M REQUESTED                              
         BNE   M10B52                                                           
         MVI   BYTE,C' '           SET FOR EQUIVALENCE FLAG                     
         CLC   SUMDL,SUMDLEQ                                                    
         BE    *+8                                                              
         MVI   BYTE,C'+'                                                        
         LA    R6,2                PRINT CPP/M                                  
         LA    RE,PLD1CP+1                                                      
M10B51A  MVC   107(5,R8),0(RE)                                                  
         MVC   112(1,R8),BYTE                                                   
         MVC   127(5,R8),11(RE)                                                 
         LA    R8,132(R8)                                                       
         LA    RE,22(RE)                                                        
         BCT   R6,M10B51A                                                       
M10B52   GOTO1 REPORT                                                           
         CLI   PASS,X'FF'                                                       
         BE    M10B54                                                           
         CLI   MAXPASS,1           ONLY ONE PASS                                
         BE    M10B54                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
STBYPID  LA    RE,STASPOT           NO - ADD TO OVERALL TOTALS                  
         LA    R1,STTSPOT                                                       
         LA    R0,11                                                            
M10B53A  L     R9,0(R1)                                                         
         A     R9,0(RE)                                                         
         ST    R9,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,M10B53A                                                       
         SPACE 2                                                                
* SET UP FOR NEXT PASS                                                          
M10B53   CLI   MAXPASS,1                                                        
         BE    M10B54                                                           
         CLI   PASS,X'FF'                                                       
         BE    M10B54                                                           
         SR    RE,RE                                                            
         IC    RE,PASS                                                          
         LA    RE,1(RE)                                                         
         STC   RE,PASS                                                          
         CLC   PASS,MAXPASS        TRUE END OF STATION                          
         BL    M10B6                                                            
         MVI   PASS,X'FF'                                                       
         CLI   CONEND,C'Y'         END OF CONTRACT                              
         BNE   *+12                                                             
         CLI   IDTOTALS,C'Y'                                                    
         BNE   M10B55                                                           
         MVC   MEDPERD,SVRDTE                                                   
         XC    STAGRID(224),STAGRID                                             
         MVC   STASPOT(44),STTSPOT                                              
         XC    STTSPOT(44),STTSPOT                                              
         B     STATOT1                                                          
M10B54   MVI   PASS,0              YES - RESET AND EXIT                         
         CLI   DETOPTS+2,0         NO COSTS PRINTING                            
         BE    M10B54A                                                          
         OC    TAXAMT,TAXAMT                                                    
         BZ    M10B54A                                                          
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P(53),=C'***TAX OF XXXXX.XX EXCLUDED FROM THIS REPORT'           
         MVC   P(L'SP@TAX02),SP@TAX02                                           
         DROP  RE                                                               
***      LA    R0,P                                                             
***      CLI   0(R0),C'@'                                                       
***      BE    *+12                                                             
***      LA    R0,1(R0)                                                         
***      B     *-12                                                             
         MVI   CURTAB+3,2                                                       
         PRINT GEN                                                              
         CURED TAXAMT,(8,P+10),CURTAB,DMCB=CDMCB                                
         PRINT NOGEN                                                            
         MVI   CURTAB+3,0                                                       
         GOTO1 REPORT                                                           
         XC    TAXAMT,TAXAMT                                                    
M10B54A  GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   QPROG,C'U'                                                       
         BNE   M10B55                                                           
         GOTO1 VREVBUY,DMCB,(RA)                                                
M10B55   DS    0H                                                               
         MVI   PASS,0                                                           
         MVC   WORK(12),PASSTAB                                                 
         MVC   PASSQST(12),PASSTAB                                              
         MVC   MID1,SPACES                                                      
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         XC    STAGRID(224),STAGRID                                             
         XC    STASPOT,STASPOT                                                  
         XC    STADEMS(32),STADEMS                                              
         XC    STACOST(8),STACOST                                               
         B     M10EXIT                                                          
M10B6    MVI   MODE,REREAD                                                      
         SR    R6,R6                                                            
         IC    R6,PASS                                                          
         MH    R6,=H'12'                                                        
         LA    R6,PASSTAB(R6)                                                   
         USING PASSTABD,R6                                                      
         MVC   WORK(12),0(R6)                                                   
         MVC   PASSQST(12),0(R6)   SET PASS START AND END                       
         XC    STAGRID(224),STAGRID                                             
         XC    STASPOT,STASPOT                                                  
         XC    STADEMS(32),STADEMS                                              
         XC    STACOST(8),STACOST                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,MAXLINES                                                      
         SR    RF,RE                                                            
         L     R1,AHDATES                                                       
         CLI   FORCEHED,C'Y'                                                    
         BE    M10B7                                                            
         MVI   FORCEHED,C'Y'                                                    
         C     RF,=F'14'                                                        
         BL    M10EXIT                                                          
         MVI   FORCEHED,C'N'                                                    
         L     R1,PSTAGRID                                                      
M10B7    DS    0H                                                               
         MVI   ALLOWLIN,14                                                      
         MVI   FORCEMID,C'Y'                                                    
M10EXIT  GOTO1 DATCON,DMCB,WORK,(X'03',PASSSD3)                                 
         GOTO1 DATCON,DMCB,WORK+6,(X'03',PASSED3)                               
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         C     RE,=F'8'                                                         
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         LTORG                                                                  
* CURTAB   DC    X'00000002115B4040'                                            
         EJECT                                                                  
EDTDEMSC NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
*                                                                               
         MVI   DEMINDX,0                                                        
         LA    R4,SVDEMS                                                        
         LA    R5,PLDEMS                                                        
         XC    PLDEMS,PLDEMS                                                    
         LA    R6,14                                                            
*                                                                               
EDTDEM2  OC    0(4,R4),0(R4)                                                    
         BZ    EDTDEM30                                                         
*                                                                               
         SR    R8,R8                                                            
         ICM   R9,15,0(R4)                                                      
         BZ    EDTDEM30                                                         
*                                                                               
         LLC   RF,DEMINDX          GET DEMO SEQNUM                              
         MHI   RF,7                NAMES ARE 7 BYTES                            
         LA    RF,DNAMES(RF)                                                    
         CLI   0(RF),C'R'          TEST RATING                                  
         JE    *+12                                                             
         CLI   0(RF),C'E'          OR EXTENDED RATING                           
         JNE   EDTDEM4             DEMO IS NOT A RATING                         
         TM    RQOPTS,RQOPTS_2DEC  TEST 2 DECIMAL RATINGS                       
         JZ    EDTDEM20                                                         
         J     EDTDEM6                                                          
*                                                                               
EDTDEM4  TM    RQOPTS,RQOPTS_2DECIMP   TEST 2-DEC IMPRESSIONS                   
         JZ    EDTDEM20                                                         
*                                                                               
EDTDEM6  CLI   MODE,PROCBUY        TEST BUY DETAILS                             
         BE    EDTDEM12            YES                                          
                                                                                
         M     R8,=F'2'            DROP 1 DECIMAL                               
         D     R8,=F'10'                                                        
         AHI   R9,1                                                             
         SRL   R9,1                                                             
         C     R9,=F'9999'         DECIMAL PRECISION OK                         
         BH    EDTDEM12             NO - DIVIDE BY 10                           
         MVI   CURTAB+3,1                                                       
*                                   YES - MAINTAIN PRECISION                    
         CURED (R9),(5,(R5)),CURTAB,DMCB=CDMCB                                  
         MVI   CURTAB+3,0                                                       
         B     EDTDEM20                                                         
*                                                                               
* >99999 PRINTS NO DEC, >9999 PRINTS 1 DEC, ELSE PRINT 2 DEC                    
*                                                                               
EDTDEM12 C     R9,=F'99999'                                                     
         BNH   EDTDEM14                                                         
         LHI   R0,100              SET DIVISOR                                  
         B     EDTDEM24                                                         
*                                                                               
EDTDEM14 C     R9,=F'9999'                                                      
         BNH   EDTDEM16                                                         
         M     R8,=F'2'                                                         
         D     R8,=F'10'                                                        
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         CURED (R9),(5,(R5)),1,DMCB=CDMCB                                       
         B     EDTDEM30                                                         
*                                                                               
EDTDEM16 CURED (R9),(5,(R5)),2,DMCB=CDMCB                                       
         B     EDTDEM30                                                         
*                                                                               
* VALUE HAS ONLY 1 DECIMAL                                                      
*                                                                               
EDTDEM20 C     R9,=F'9999'         1 DECIMAL PRECISION OK                       
         BH    EDTDEM22            NO - SCALE                                   
         CURED (R9),(5,(R5)),1,DMCB=CDMCB                                       
         B     EDTDEM30                                                         
*                                                                               
EDTDEM22 LHI   R0,10               SET DIVISOR                                  
*                                                                               
EDTDEM24 M     R8,=F'2'             X 2                                         
         DR    R8,R0                                                            
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         EDIT  (R9),(5,(R5))                                                    
*                                                                               
EDTDEM30 LA    R5,5(R5)             GET CPP                                     
         LA    R4,4(R4)                                                         
         ICM   R9,15,0(R4)                                                      
         BZ    EDTDEM40                                                         
*                                                                               
EDTDEM32 C     R9,=F'9999'        TEST MORE THAN 5 CHARS WITH PENNIES           
         BH    EDTDEM34                                                         
         MVI   CURTAB+3,2                                                       
         CURED (R9),(6,(R5)),CURTAB,DMCB=CDMCB                                  
         MVI   CURTAB+3,0                                                       
         B     EDTDEM40                                                         
*                                                                               
EDTDEM34 M     R8,=F'2'                                                         
         D     R8,=F'100'  <<<< DROP 2 DECIMALS                                 
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         MVI   CURTAB+3,0                                                       
         CURED (R9),(6,(R5)),CURTAB,DMCB=CDMCB                                  
*                                                                               
         CLI   0(R5),C' '                                                       
         BH    EDTDEM40                                                         
* I CAN'T MAKE CURED PUT IN THE $ SIGN                                          
         LA    RF,5(R5)                                                         
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   0(RF),C'$'                                                       
*                                                                               
EDTDEM40 LA    R4,4(R4)                                                         
         LA    R5,6(R5)                                                         
         IC    RF,DEMINDX                                                       
         AHI   RF,1                                                             
         STC   RF,DEMINDX                                                       
         BCT   R6,EDTDEM2                                                       
         XIT1                                                                   
*************                                                                   
         LTORG                                                                  
         EJECT                                                                  
MLASTC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WEIGHT,SPWEIGHT                                                  
         GOTO1 VSUMMRY                                                          
         L     R8,BUFFBUFF                                                      
         MVC   DMCB+8(20),LVCNTRL                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',(BUFCDE,(R8))                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',1)                   
         ZIC   RE,BUFCDE                                                        
         LA    RE,3(RE)                                                         
         STC   RE,BUFCDE                                                        
         GOTO1 BUFFALO,DMCB,=C'ADD',(BUFCDE,(R8))                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R8)),(X'80',1)                   
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
EXTRCT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         CLC   QPROG,=C'RS'        RS REPORT?                                   
         BE    *+14                YES                                          
         CLC   QPROG,=C'RY'        RY REPORT?                                   
         BNE   EXTSL00             NO                                           
         LA    R1,SPTHK            YES-PASS A(SPOT HOOK)                        
         ST    R1,SPOTHOOK                                                      
         STM   RA,RC,SPTHKRA                                                    
*                                                                               
EXTSL00  MVC   QSTART(12),PASSQST                                               
         MVC   MEDNUMWK,NUMWK                                                   
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'128'                                                 
         CLI   D5APROF,5                                                        
         BL    *+10                                                             
         MVC   MEDLCHNK,=F'136'    MAX 6 DEMOS                                  
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   MEDBRAND,BPRD                                                    
         MVC   BYTE,MEDSPTLN                                                    
         MVI   MEDSPTLN,0                                                       
         MVC   MEDEXTDM,D5APROF                                                 
         XC    SVPERD,SVPERD                                                    
*                                                                               
EXTSL    XC    PSLIST(L'PSLIST),PSLIST                                          
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         XC    SVPSLIST,SVPSLIST                                                
         CLI   BPRD,X'FF'                                                       
         BE    EXTSLX                                                           
         LA    RE,PSLIST           GET SPOT LENGTH FOR PRODUCT                  
EXTSL1   CLI   0(RE),0                                                          
         BE    EXTSLX                                                           
         CLC   0(1,RE),BPRD                                                     
         BE    EXTSL2                                                           
         LA    RE,2(RE)                                                         
         B     EXTSL1                                                           
EXTSL2   MVC   MEDSPTLN,1(RE)                                                   
         MVC   BYTE,1(RE)                                                       
         LA    RF,2(RE)                                                         
         ST    RF,SVPSLIST                                                      
*                                                                               
EXTSLX   DS    0C                                                               
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
EXTRCT2  MVI   MEDEXCH,0                                                        
         L     RE,ADAGY            TEST CANADIAN AGENCY                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C'                                        
         BNE   EXTRCT2A                                                         
         L     RE,ADBUY            YES-TEST EXCHANGE ELEMENT                    
         TM    BDCIND2-BUYREC(RE),X'40'                                         
         BZ    EXTRCT2A                                                         
         MVI   MEDEXCH,C'C'        YES-SET EXCHANGE TO CANADIAN DOLLARS         
*                                                                               
EXTRCT2A GOTO1 MEDGETBY,DMCB,(RA),(R5)                                          
         CLI   MEDSPILL,C'Y'                                                    
         BNE   EXTRCT2D                                                         
         CLI   SPOTPROF+5,0                                                     
         BE    EXTRCTX                                                          
EXTRCT2D GOTO1 MEDMKTWT,DMCB,(RA),(R9)                                          
         MVC   PTSSPILL,MEDSPILL                                                
         MVC   ASVPRD,MEDBRAND     GET 14 DEMOS FOR PERIOD                      
         MVC   ASVLCHNK,MEDLCHNK                                                
         MVC   ASVFRST,MEDAFRST                                                 
         MVC   ASVLAST,MEDALAST                                                 
         MVI   MEDEXTDM,14                                                      
         MVC   MEDBRAND,BPRD                                                    
         MVC   MEDLCHNK,=F'200'                                                 
         LA    RE,MEDPERD                                                       
         ST    RE,MEDAFRST                                                      
         ST    RE,MEDALAST                                                      
         L     R4,4(RE)            SAVE TAX DOLLARS WHEN DOING 14 DEMOS         
         MVC   SVMSTAX,MEDMSTAX     DEMO 13 IS OVERLAYED BY TAX TOTAL           
         MVC   SVMEDBYD,MEDBYD                                                  
         MVC   SVEXTAX,MEDEXTAX                                                 
         MVI   MEDEXTAX,C'N'                                                    
         CLI   BPRD,X'FF'                                                       
         BNE   BYPUNA0                                                          
         MVI   MEDBRAND,219        COUNT UNALOCATED SPOTS                       
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
         LA    RE,MEDPERD                                                       
         L     R4,4(RE)                                                         
         L     R1,MEDBYSPT                                                      
         A     R1,UNATOT                                                        
         ST    R1,UNATOT                                                        
BYPUNA0  MVC   MEDBRAND,BPRD                                                    
         GOTO1 MEDGETBY,DMCB,(RA),(R5)                                          
         MVC   MEDMSTAX,SVMSTAX    RESTORE TAX DOLLARS                          
         MVC   MEDBYD(8),SVMEDBYD                                               
         MVC   MEDEXTAX,SVEXTAX                                                 
         GOTO1 MEDMKTWT,DMCB,(RA),(R9)                                          
BYPUNA   DS    0C                                                               
         MVI   MEDEXTDM,4                                                       
         MVC   MEDLCHNK,=F'128'                                                 
         MVC   MEDBRAND,ASVPRD                                                  
         MVC   MEDAFRST,ASVFRST                                                 
         MVC   MEDALAST,ASVLAST                                                 
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   BYTE,BDSEC                                                       
         DROP  R5                                                               
         CLC   QPROG,=C'RS'                                                     
         BE    *+14                                                             
         CLC   QPROG,=C'RY'                                                     
         BNE   BYPUNB                                                           
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD                                                
         BZ    EXTRCTX                                                          
BYPUNB   DS    0H'0'                                                            
         L     R0,=A(NETCOVR)                                                   
         LHI   R1,NETCOVRX-NETCOVR                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,GETHP                                                         
         OC    UNATOT,UNATOT       ANY UNALLOCATED                              
         BNZ   HAVUNA               YES - OK TO PROCESS                         
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD   ANY SPOTS OR DOOLARS                         
         BZ    EXTRCTX              NO - EXIT                                   
HAVUNA   DS    0H'0'                                                            
         MVC   WEIGHT,SPWEIGHT                                                  
         XC    PRTLINE,PRTLINE                                                  
         LA    R3,PRTLINE                                                       
         L     R5,MEDAFRST         RELEASE DATA TO BUFFALO                      
         CLI   MRPTTYP,C'2'        RPT 2 DOESNT NEED WEEKLYS                    
         BNE   EXTRCT3                                                          
         LA    R5,MEDMON01                                                      
EXTRCT3  MVI   SUMCODE,X'90'                                                    
         MVC   SUMDPGNO(8),MEDDPGNO     SET DAYPART                             
         MVC   SUMSLN,BYTE                                                      
         MVC   MEDSPTLN,BYTE                                                    
         L     R4,4(R5)                                                         
         MVI   SUMRTYP,1                                                        
         LA    RF,MEDPERD                                                       
         CR    R5,RF               END                                          
         BH    EXTRCTX                                                          
         BNE   EXTRCT3A            WEEKLY OR MONTHLY                            
         OC    MEDBYD(12),MEDBYD                                                
         BZ    EXTRCT5              NO - EXIT                                   
         MVI   SUMRTYP,3                                                        
         MVC   SUMDT,=X'FFFFFFFF'                                               
         B     EXTRCT4                                                          
EXTRCT3A OC    0(4,R5),0(R5)       ACTIVE SLOT                                  
         BZ    EXTRCT5                                                          
         LA    RF,MEDMON01                                                      
         CR    R5,RF               MONTHLY                                      
         BL    EXTRCT3B                                                         
         MVI   SUMRTYP,2            YES-SET RECORD CODE                         
         CLI   MRPTTYP,C'2'        MONTHLYS FOR RPT 2 ONLY                      
         BE    EXTRCT3B                                                         
         LA    R5,MEDPERD                                                       
         B     EXTRCT3                                                          
EXTRCT3B MVC   SUMDT,0(R5)                                                      
EXTRCT4  LA    RE,SUMKEY           SET UP DATA ITEM DISPLACEMENTS               
         USING SUMDATA,RE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         A     RE,BUFFLKEY                                                      
         LA    R8,MEDPERD                                                       
         CR    R5,R8                                                            
         BNE   *+16                                                             
         L     R8,TAXAMT                                                        
         A     R8,MEDMSTAX                                                      
         ST    R8,TAXAMT                                                        
         XC    SUMDATA,SUMDATA                                                  
         MVC   SUMSPOTS,MEDBYSPT   MOVE IN DATA                                 
         MVC   SUMDL,MEDBYD                                                     
         MVC   SUMDLEQ,MEDBYDEQ                                                 
         MVC   SUMD1,MEDBY1                                                     
         MVC   SUMD1EQ,MEDBY1EQ                                                 
         MVC   SUMD2,MEDBY2                                                     
         MVC   SUMD2EQ,MEDBY2EQ                                                 
         MVC   SUMD3,MEDBY3                                                     
         MVC   SUMD3EQ,MEDBY3EQ                                                 
         MVC   SUMD4,MEDBY4                                                     
         MVC   SUMD4EQ,MEDBY4EQ                                                 
         MVI   BUYACT,1                                                         
         EJECT                                                                  
*        SEND TOTALS TO BUFFALO                                                 
         LA    R8,PROGPROF                                                      
         USING PROFDSCT,R8                                                      
         CLC   QPROG,=C'RS'                                                     
         BE    EXTRCT4T                                                         
         CLC   QPROG,=C'RY'                                                     
         BE    EXTRCT4T                                                         
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
         MVC   SUMDPGNO(9),=9X'FF'                                              
         MVI   SUMCODE,X'90'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'93'                                                    
         BAS   R9,EXTPUT                                                        
         CLI   SPOTPROF+5,2        DO WE NEED SPILL OR ORIG                     
         BE    EXTRCT5             NO                                           
         CLI   SPOTPROF+5,0                                                     
         BE    EXTRCT5                                                          
         CLI   MEDSPILL,C'Y'       IS THIS SPILL                                
         BE    EX4SPL              YES                                          
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
         MVI   SUMSLN,X'FD'        PUT OUT SPILL TOTALS                         
         MVI   SUMCODE,X'88'                                                    
         BAS   R9,EXTPUT                                                        
         MVI   SUMCODE,X'91'                                                    
         BAS   R9,EXTPUT                                                        
* GET NEXT BUFFALO ITEM                                                         
EXTRCT5  LA    R5,12(R5)                                                        
         B     EXTRCT3                                                          
EXTPUT   CLI   SUMCODE,X'90'                                                    
         BHR   R9                                                               
         OC    SUMSPOTS(12),SUMSPOTS                                            
         BZR   R9                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),PRTLINE                                
         BR    R9                                                               
         EJECT                                                                  
EXTRCTX  L     RE,SVPSLIST         TAKE CARE OF DIFFERENT SPOT                  
         LTR   RE,RE               LENGTHS FOR SAME BRAND ON                    
         BZ    EXTRCTX3            SAME BUYLINE                                 
*                                                                               
         LA    R5,MEDPERD          SUM THE PERIOD TOTALS ACROSS SPOT            
         L     RF,4(R5)            LENGTHS SO WE CAN GENERATE                   
         LA    RE,SVPERD           CPP/M FOR DETAIL LINE AND                    
         L     R9,MEDLCHNK         STATION TOTALS                               
*                                                                               
         SRL   R9,2                ADD IN PREVIOUS DATA                         
EXTRCTX1 L     R1,0(RF)                                                         
         A     R1,0(RE)                                                         
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R9,EXTRCTX1                                                      
         LA    R5,MEDPERD          SAVE FOR NEXT ROUND                          
         L     RF,4(R5)                                                         
         L     R9,MEDLCHNK                                                      
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   SVPERD(0),0(RF)                                                  
         L     RE,SVPSLIST         RESTORE PRD/SPOT LENGTH POINTER              
         SPACE 1                                                                
EXTRCTX2 CLC   0(1,RE),BPRD        CHECK FOR MORE TO DO                         
         BE    EXBGRD                                                           
         LA    RE,2(RE)                                                         
         CLI   0(RE),0             END OF LIST - JUST EXIT                      
         BE    EXTRCTX3                                                         
         B     EXTRCTX2                                                         
         SPACE 1                                                                
* BUILD WEEKLY GRID AND SUM INTO STATION BUCKETS                                
EXBGRD   LA    R5,MEDMON01                                                      
         LA    R8,STAGRID                                                       
EXBGRD2  L     R4,4(R5)                                                         
         LA    RE,MEDMON13                                                      
         CR    R5,RE                                                            
         BH    EXBGRD6             DO NEXT SPOT LENGTH                          
         OC    0(4,R5),0(R5)                                                    
         BZ    EXBGRD5                                                          
EXBGRD4  L     RE,0(R8)            SUM WEEKLY SPOTS                             
         A     RE,MEDBYSPT         FOR THIS SPOT LENGTH                         
         ST    RE,0(R8)                                                         
         LA    R6,4(R6)                                                         
         LA    R8,4(R8)                                                         
EXBGRD5  LA    R5,12(R5)                                                        
         B     EXBGRD2                                                          
EXBGRD6  L     RE,SVPSLIST                                                      
         B     EXTSL1                                                           
*                                                                               
EXTRCTX3 XIT1                                                                   
         DROP  RE                                                               
         EJECT                                                                  
* MEDGETBY HOOK                                                                 
*                                                                               
         USING *,RF                                                             
SPTHK    NTR1                                                                   
         LM    RA,RC,SPTHKRA                                                    
         B     SPTHK2                                                           
SPTHKRA  DC    3F'0'                                                            
         DROP  RF                                                               
*                                                                               
SPTHK2   L     R5,SPOTADDR                                                      
         USING REGELEM,R5                                                       
         CLI   RCODE,11            POL BUY ELEMENT?                             
         BL    SPTHKY              NO - LET THIS SPOT PASS                      
         CLI   RCODE,13            POL BUY ELEMENT?                             
         BH    SPTHKY              NO - LET THIS SPOT PASS                      
         CLI   Q2USER+5,C'Y'       REPORT ONLY ONE BRAND?                       
         BNE   SPTHK3              NO                                           
         CLI   RLEN,14             YES - IS THIS A PIGGYBACK SPOT?              
         BH    SPTHKN              YES - REJECT!                                
         B     SPTHKY              NO - LET THIS SPOT PASS                      
*                                                                               
SPTHK3   CLI   BPRD2,0             REQUESTED A PIGGY?                           
         BE    SPTHKY              NO - THIS SPOT PASSES                        
         CLI   RLEN,14             TEST IT'S A PIGGYBACK SPOT                   
         BNH   SPTHKN              NO - REJECT!                                 
*                                                                               
         CLC   BPRD2,10(R5)        PRD MATCH PIGGY?                             
         BE    SPTHKY              YES - THIS SPOT PASSES                       
         CLC   BPRD2,14(R5)        PRD2 MATCH PIGGY?                            
         BNE   SPTHKN              NO-REJECT                                    
*                                                                               
SPTHKY   MVI   SPOTYORN,C'Y'       ACCEPT                                       
         B     SPTHKX                                                           
*                                                                               
SPTHKN   MVI   SPOTYORN,C'N'       REJECT                                       
         MVI   0(R5),X'FF'                                                      
*                                                                               
SPTHKX   XIT1  ,                                                                
*                                                                               
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
         OC    PGNOENT,PGNOENT                                                  
         BNZ   GETHP1                                                           
         XC    HPSNO,HPSNO                                                      
         MVC   LASTGSLT,VPGRID                                                  
         XC    PGNOENT,PGNOENT                                                  
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
GETHP1   LA    R5,BDELEM                                                        
         USING REGELEM,R5                                                       
*                                                                               
REGNXT   ZIC   RE,RLEN                                                          
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
         TM    RSTATUS,X'80'                                                    
         BO    REGNXT                                                           
*                                                                               
         IC    R0,ELEMNUM          GET SPOT NUMBER THIS DATE                    
         CLC   ELEMDT,RDATE                                                     
         BE    *+6                                                              
         SR    R0,R0                                                            
         AHI   R0,1                                                             
         STC   R0,ELEMNUM                                                       
         MVC   ELEMDT,RDATE                                                     
*                                                                               
         CLI   RCODE,11                                                         
         BL    GETHP2                                                           
         CLI   MEDBRAND,X'FF'      POL PRODUCT                                  
         BE    GHPPGRP             POL - CHECK FOR PRODUCT GROUPS               
         CLI   RLEN,14             UNALLOCATED                                  
         BL    REGNXT               BYPASS                                      
         BE    *+14                                                             
         CLC   MEDBRAND,RPPRD+4    CHECK PARTNER                                
         BE    *+10                                                             
         CLC   MEDBRAND,RPPRD      WRONG BRAND                                  
         BNE   REGNXT              BYPASS                                       
GHPPGRP  CLI   MEDSPTLN,0          DON'T CARE ABOUT LENGTH                      
         BE    *+14                                                             
         CLC   MEDSPTLN,RPTIME     CHECK FOR CORRECT LENGTH                     
         BNE   REGNXT                                                           
         CLI   QPGR,C' '           PRODUCT GROUPS REQUESTED                     
         BE    GETHP2               NO - ALLOW ALL BRANDS                       
         CLI   RLEN,14             BYPASS UNALLOCATED                           
         BL    REGNXT                                                           
         LA    R8,PSLIST                                                        
GHPPGRP2 CLI   0(R8),0                                                          
         BE    REGNXT                                                           
         CLC   RPPRD,0(R8)         PRODUCT IN LIST                              
         BE    GETHP2               YES - PROCESS                               
         CLI   RLEN,18                                                          
         BL    *+14                                                             
         CLC   RPPRD+4(1),0(R8)    CHECK THE PARTNER                            
         BE    GETHP2                                                           
         LA    R8,2(R8)                                                         
         B     GHPPGRP2                                                         
         SPACE 2                                                                
GETHP2   LA    R8,WORK                                                          
         USING PGDWK,R8                                                         
         USING PGSORT,R8                                                        
         ZIC   RE,HPSNO                                                         
         LA    RE,1(RE)                                                         
         STC   RE,HPSNO                                                         
         XC    WORK,WORK                                                        
         CLC   RTYPE,=C'RS '                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '                                                    
         BNE   GETHPFX                                                          
         ZIC   R9,1(R5)                                                         
         AR    R9,R5                                                            
         CLI   0(R9),0                                                          
         BE    GETHPFX                                                          
         CLI   0(R9),X'0F'                                                      
         BL    GETHPFX                                                          
GETHPF   CLI   0(R9),X'12'         FILM ELEMENT                                 
         BNE   GETHPF1                                                          
         USING FLMELEM,R9                                                       
         MVC   PGDFDAY,FLMDAY       YES - SAVE FILM                             
         MVC   PGDFNO,FLMNUM                                                    
         B     GETHPFX                                                          
GETHPF1  ZIC   R0,1(R9)                                                         
         AR    R9,R0                                                            
         CLI   0(R9),X'0F'                                                      
         BH    GETHPF                                                           
GETHPFX  DS    0H                                                               
         MVC   PGDWK,RDATE                                                      
         ST    R5,FULL                                                          
         MVC   PGDELAD,FULL        SAVE ELEMENT ADDRESS                         
         CLC   RTYPE,=C'RS '                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '                                                    
         BNE   GETHP2A0                                                         
         TM    RSTATUS,X'40'                                                    
         BO    REGNXT                                                           
GETHP2A0 DS    0H                                                               
         MVI   PGDNOSP,1                                                        
         CLI   RCODE,10                                                         
         BH    *+10                                                             
         MVC   PGDNOSP,RNUM                                                     
         TM    BDSTAT,X'80'        POL RADIO                                    
         BZ    GETHP2A1                                                         
         ZIC   R1,RPCOST                                                        
         SRL   R1,2                                                             
         STC   R1,PGDNOSP                                                       
         LR    R1,R5               TAKE CARE OF OTO'S (RADIO POL)               
*                                                                               
GRPHP1   ZIC   R0,1(R5)            CHECK NEXT ELEMENT FOR                       
         AR    R5,R0                                                            
         CLI   0(R5),X'0C'         OTO ELEMENT                                  
         BNE   GRPHPX                                                           
         CLC   RDATE,2(R1)         WITH SAME DATE                               
         BNE   GRPHPX                                                           
         ZIC   RE,RPCOST           EXTRACT SPOTS                                
         SRL   RE,2                                                             
         ZIC   R0,PGDNOSP          GET OLD COUNT                                
         TM    6(R5),X'80'                                                      
         BO    *+12                                                             
         AR    R0,RE               INC FOR PLUS OTO                             
         LR    R1,R5                                                            
         B     *+6                                                              
         SR    R0,RE               DEC FOR MINUS OTO                            
         STC   R0,PGDNOSP          SAVE RUNNING TOTAL                           
         B     GRPHP1              AND TRY FOR MORE                             
*                                                                               
GRPHPX   LR    R5,R1               RESTORE ELEMENT                              
*                                                                               
GETHP2A1 DS    0C                                                               
         CLC   RTYPE,=C'RS '                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '                                                    
         BNE   GETHP2A6                                                         
         CLI   RCODE,10            BACK OUT MINUS SPOTS IF ROT. SKED            
         BH    GETHP2A6                                                         
         LR    R1,R5               SAVE ELEMENT ADDRESS                         
GETHP2A2 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),7             OTO                                          
         BNE   GETHP2A3                                                         
         CLC   RDATE,2(R1)         SAME DATE AS ORIG                            
         BNE   GETHP2A3             NO-END OF REGELEMS                          
         ZIC   RE,RNUM             GET SPOT COUNT-THIS ELEM                     
         ZIC   R0,PGDNOSP          GET SPOT COUNT THIS DATE                     
         TM    6(R5),X'80'         MINUS SPOT                                   
         BO    *+12                 YES-DECREMENT SPOT COUNT                    
         AR    R0,RE                NO - INCREMENT SPOT COUNT                   
         LR    R1,R5                     SET TO NEXT DATE                       
         B     *+6                                                              
         SR    R0,RE                                                            
         STC   R0,PGDNOSP                                                       
         B     GETHP2A2                                                         
GETHP2A3 LR    R5,R1                                                            
*                                                                               
GETHP2A6 DS    0C                                                               
         CLI   PGDNOSP,0           HAVE ALL SPOTS BEEN MINUSED                  
         BE    REGNXT               YES - GET NEXT ELEMENT                      
         CLI   SORTREQ,0                                                        
         BE    *+10                                                             
         XC    PGDELAD,PGDELAD                                                  
         MVC   PGDSBRN,RPPRD                                                    
         TM    RSTATUS,X'20'       SAVE COST OVERRIDES                          
         BZ    GETCOVX                                                          
         MVC   PGD2COVR,RPCOST                                                  
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF-AGYHDR+7(RE),C'C' TEST CANADA                            
         BNE   GETCOVX                                                          
         CLI   QMED,C'N'                                                        
         BNE   GETCOVX                                                          
         CLI   RSCOST,C'Y'         TEST TO PRINT COSTS                          
         BNE   GETCOVX                                                          
*                                                                               
         L     RE,=A(NETCOVR)                                                   
         LHI   RF,(NETCOVRX-NETCOVR)/6                                          
*                                                                               
GETCOV2  OC    0(6,RE),0(RE)                                                    
         BZ    GETCOV4                                                          
         AHI   RE,6                                                             
         BCT   RF,GETCOV2                                                       
         DC    H'0'                TABLE NOT BIG ENOUGH                         
*                                                                               
GETCOV4  MVC   0(2,RE),ELEMDT                                                   
         MVC   2(1,RE),ELEMNUM                                                  
         MVC   3(3,RE),RPCOST                                                   
                                                                                
GETCOVX  TM    BDSTAT,X'80'                                                     
         BZ    *+8                                                              
         NI    PGD2COVR,B'00000011'                                             
         CLI   RLEN,15                                                          
         BL    *+16                                                             
         MVC   PGD2BRN,RPPRD+4                                                  
         MVC   PGD2SLN,RPTIME+4                                                 
         CLI   RCODE,10                                                         
         BL    GETHP2A                                                          
         CLI   RLEN,13                                                          
         BH    GETHP2A                                                          
         CLC   RTYPE,=C'RS '                                                    
         BE    REGNXT                                                           
         CLC   RTYPE,=C'RY '                                                    
         BE    REGNXT                                                           
         MVI   PGDSBRN,X'FF'                                                    
         MVI   PGDSSLN,0                                                        
         MVC   PGDSNO,HPSNO                                                     
         TM    RSTATUS,X'40'                                                    
         BZ    *+8                                                              
         MVI   PGDIND,X'01'        SET MISSED                                   
         TM    RSTATUS,X'42'                                                    
         BNO   *+12                                                             
         MVI   PGDIND,X'04'        SET PREMPT                                   
         B     GETHP2C                                                          
         SPACE 2                                                                
GETHP2A  TM    RSTATUS,X'04'                                                    
         BZ    GETHP2B                                                          
         MVI   PGDSBRN,X'FF'                                                    
         MVI   PGDSSLN,0                                                        
         MVC   PGDSNO,HPSNO                                                     
         MVI   PGDIND,X'08'        SET HIATUS INDICATOR                         
         B     GETHP2C                                                          
         SPACE 2                                                                
GETHP2B  TM    RSTATUS,X'40'                                                    
         BZ    *+8                                                              
         MVI   PGDIND,X'01'        SET MISSED INDICATOR                         
         TM    RSTATUS,X'42'                                                    
         BNO   *+8                                                              
         MVI   PGDIND,X'04'        SET PREMPT INDICATOR                         
         CLC   RTYPE,=C'RS '                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '                                                    
         BNE   GETHP2BA                                                         
         CLI   PGDIND,0            RS BYPASSES MINUS AND HIATUS SPOTS           
         BNE   REGNXT                                                           
GETHP2BA CLI   RCODE,10                                                         
         BL    GETHP2B1                                                         
         CLI   RLEN,14             TAKE CARE OF UNALLOCATED                     
         BL    GETHP2B1                                                         
         MVC   PGDSBRN,RPPRD                                                    
         MVC   PGDSSLN,RPTIME                                                   
         MVC   PGDSNO,HPSNO                                                     
         B     GETHP2C                                                          
GETHP2B1 MVC   PGDSBRN,BPRD                                                     
         MVC   PGDSSLN,BYTE                                                     
         MVC   PGDSNO,HPSNO                                                     
         SPACE 2                                                                
GETHP2C  L     RE,LASTGSLT         SET TO LAST GRID SLOT                        
         L     RF,PGNOENT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PGNOENT                                                       
         STC   RF,PGLSLOT                                                       
         CLI   RCODE,10                                                         
         BH    *+10                                                             
         MVC   PGDSBRN,BPRD                                                     
         MVC   0(PGDLEN,RE),WORK                                                
         LA    RE,PGDLEN(RE)                                                    
         ST    RE,LASTGSLT                                                      
*                                                                               
         CLC   RTYPE,=C'RS '       TEST RS REPORT AND PRODUCT NAME              
         BE    *+14                LEGEND REQUIRED                              
         CLC   RTYPE,=C'RY '       TEST RY REPORT AND PRODUCT NAME              
         BNE   REGNXT              LEGEND REQUIRED                              
         CLI   QPNAME,C'Y'                                                      
         BNE   REGNXT                                                           
         XC    HALF,HALF           YES-ADD PRODUCT(S) TO PRODUCT LIST           
         MVC   HALF(1),PGDSBRN                                                  
         MVC   HALF+1(1),PGD2BRN                                                
         LA    R1,HALF                                                          
         LA    RF,2                                                             
*                                                                               
GETHP3   SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         BZ    GETHP3B                                                          
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         MVC   FULL(3),1(RE)                                                    
         L     RE,VPRDLST                                                       
*                                                                               
GETHP3A  CLI   0(RE),0                                                          
         BNE   *+14                                                             
         MVC   0(3,RE),FULL                                                     
         B     GETHP3B                                                          
         CLC   0(3,RE),FULL                                                     
         BE    GETHP3B                                                          
         LA    RE,3(RE)                                                         
         B     GETHP3A                                                          
*                                                                               
GETHP3B  LA    R1,1(R1)                                                         
         BCT   RF,GETHP3                                                        
         B     REGNXT                                                           
         SPACE 2                                                                
GETHPX   CLI   VARFRMT,1           VARIABLE FORMAT                              
         BE    GETHPX2              YES - ALLOW VARIABLE SLOT                   
         L     R5,MEDAFRST         NO - FORCE TO DATE SLOT                      
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    GETHPX2                                                          
         L     R8,VPGRID                                                        
         LA    R2,1                                                             
GETHPS   CLC   PGDWK,0(R5)                                                      
         BNL   *+12                                                             
         LA    R2,1                                                             
         L     R5,MEDAFRST                                                      
         OC    0(4,R5),0(R5)                                                    
         BZ    GETHPS1                                                          
         CLC   PGDWK,2(R5)         SET FIXED SLOT FOR WEEK                      
         BNH   GETHPS2                                                          
         LA    R2,1(R2)                                                         
GETHPS1  LA    R5,12(R5)                                                        
         B     GETHPS                                                           
GETHPS2  STC   R2,PGLSLOT                                                       
         LA    R8,PGDLEN(R8)                                                    
         BCT   R1,GETHPS                                                        
GETHPX2  XIT1                                                                   
SVPSLIST DS    F                                                                
PSLIST   DS    CL220                                                            
         DROP  R6                                                               
         DROP  R5                                                               
         DROP  R3                                                               
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
GETBUF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SUMDSECT,R3                                                      
         XC    DMCB,DMCB                                                        
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
         LA    R4,DNAMES           SET UP FOR UNWEIGHTING                       
         LA    R5,4                                                             
         LA    R6,SUMD1                                                         
GBUF4    CLI   SPOTPROF+1,C'D'     UNWEIGHT TOTALS                              
         BE    GBF4                                                             
         CLI   0(R4),C'E'          EXTENDED RATINGS                             
         BE    *+8                                                              
         CLI   0(R4),C'R'                                                       
         BNE   GBUF5                                                            
GBF4     OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   GBUF4A                                                           
         XC    0(8,R6),0(R6)                                                    
         B     GBUF5                                                            
GBUF4A   DS    0H                                                               
         L     RE,0(R6)                                                         
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R6)                                                         
         L     RE,4(R6)                                                         
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R6)                                                         
GBUF5    LA    R4,7(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,GBUF4                                                         
         CLI   SPOTPROF+1,C'D'                                                  
         BE    GBF5                                                             
         CLI   DNAMES,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAMES,C'R'                                                      
         BNE   GETBUFX                                                          
GBF5     OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   GBUF5A                                                           
         XC    SUMGD1(8),SUMGD1                                                 
         B     GETBUFX                                                          
GBUF5A   DS    0H                                                               
         L     RE,SUMGD1                                                        
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SUMGD1                                                        
         L     RE,SUMGD1E                                                       
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SUMGD1E                                                       
GETBUFX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* POOL TIMESHEET HEADLINES                                                      
PTSHEAD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*                                                                               
         CLI   QMGR,C' '           CHECK FOR MGR REQUEST                        
         BE    PTNOMGR                                                          
         MVC   H1+23(24),MGR1NM                                                 
         MVC   H2+23(24),MGR2NM                                                 
         MVC   H3+23(24),MGR3NM                                                 
PTNOMGR  DS    0H                                                               
*---->   MVC   H1+58(15),=C'POOL TIME SHEET'                                    
         MVC   H1+58(L'SP@PLTS),SP@PLTS                                         
         MVC   H2+58(L'SPUPLTS),SPUPLTS                                         
         CLI   QPROG,C'U'                                                       
         BNE   *+10                                                             
*---->   MVC   H1+58(15),=C'POOL TURNAROUND'                                    
         MVC   H1+58(L'SP@PLTU),SP@PLTU                                         
         MVC   H2+58(L'SPUPLTU),SPUPLTU                                         
         CLI   MODE,STALAST                                                     
         BH    SCALLP2                                                          
         LA    R1,H1+56                                                         
         LA    R8,BIGSTA+8                                                      
         LA    R6,9                                                             
SCALLP   CLI   0(R8),C' '                                                       
         BE    SCALLP1                                                          
         MVC   0(1,R1),0(R8)                                                    
         MVI   132(R1),C'-'                                                     
         BCTR  R1,0                                                             
SCALLP1  BCTR  R8,0                                                             
         BCT   R6,SCALLP                                                        
SCALLP2  CLI   SPOTPROF+12,C'Y'                                                 
         BNE   *+10                                                             
*---->   MVC   H9(18),=C'***TAX EXCLUDED***'                                    
         MVC   H9(L'SP@TXEX),SP@TXEX                                            
         MVC   H10(32),DASH                                                     
*---->   MVC   H10+9(15),=C'BUY DESCRIPTION'                                    
         MVC   H10+9(L'SP@BYDES),SP@BYDES                                       
         MVC   H10+35(62),DASH                                                  
*---->   MVC   H10+56(16),=C'ROTATION PATTERN'                                  
         MVC   H10+56(L'SP@ROTPA),SP@ROTPA                                      
         MVC   H10+93(40),DASH                                                  
*---->   MVC   H10+104(18),=C'DEMOGRAPHICS (XXX)'                               
         MVC   H10+104(L'SP@DMGR2),SP@DMGR2                                     
*---->   MVC   H10+118(3),=C'NSI'  SET RATING SERVICE                           
         MVC   H10+118(L'SP@NSI),SP@NSI                                         
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         CLI   CPROF+3,X'F0'                                                    
         BE    RSEX                                                             
*---->   MVC   H10+118(3),=C'ARB'                                               
         MVC   H10+118(L'SP@ARB),SP@ARB                                         
         DROP  R6                                                               
         L     R6,ADAGY                                                         
         USING AGYRECD,R6                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BNE   RSEX                                                             
*---->   MVC   H10+118(3),=C'BBM'                                               
         MVC   H10+118(L'SP@BBM),SP@BBM                                         
         DROP  R6                                                               
RSEX     DS    0H                                                               
*---->   MVC   H11(32),=C'EST START-END   WKS  DAYS    N/W'                     
         MVC   H11(L'SP@EST02),SP@EST02                                         
*---->   MVC   H11+93(39),=C'NAME     GRPS   CPP NAME     GRPS   CPP'           
         MVC   H11+93(L'SP@NAM01),SP@NAM01                                      
*---->   MVC   H12+93(39),=C'         IMPS   CPM          IMPS   CPM'           
         MVC   H12+102(L'SP@IMP01),SP@IMP01                                     
*---->   MVC   H13+113(19),=C'RATED PROGRAM  BOOK'                              
         MVC   H13+113(L'SP@RAPRB),SP@RAPRB                                     
*---->   MVC   H12(24),=C'LIN TIME        DPT  LEN'                             
         MVC   H12(L'SP@LIN01),SP@LIN01                                         
*---->   MVC   H13+4(28),=C'PROGRAMMING      ADJ    COST'                       
         MVC   H13+4(L'SP@PRO02),SP@PRO02                                       
*---->   MVC   H14+4(8),=C'COMMENTS'                                            
         MVC   H14+4(L'SP@COMM),SP@COMM                                         
         CLI   MODE,STALAST                                                     
         BH    AFFDONE                                                          
         BE    RSEX1                                                            
         MVC   P(33),SVP1                                                       
         MVC   P2(33),SVP2                                                      
         MVC   P3(32),SVP3                                                      
         MVC   P4(32),SVP4                                                      
         MVC   P5(32),SVP5                                                      
RSEX1    MVC   SVP1(80),SPACES                                                  
         MVC   SVP3(80),SPACES                                                  
         MVC   SVP5,SPACES                                                      
         L     RE,ADSTATAD                                                      
         USING ADDRREC,RE                                                       
         MVI   H5+42,C' '                                                       
         MVI   H6+42,C' '                                                       
         MVI   H7+42,C' '                                                       
         MVI   H8+42,C' '                                                       
         MVC   H5+43(20),ANAME                                                  
         CLI   BIGSTA,C'0'         TEST CABLE STATION                           
         BL    *+14                                                             
         L     R1,ADSTAT           YES-PRINT CABLE SYSTEM NAME                  
         MVC   H5+43(24),SSYSNAME-STAREC(R1)                                    
         MVC   H6+43(24),A1LINE                                                 
         MVC   H7+43(24),A2LINE                                                 
         MVC   H8+43(L'ABIGZIP),A3LINE                                          
         DROP  RE                                                               
         CLI   D5ASUPR,C'Y'        SUPPRESS REPS                                
         BE    PTSHBYR                                                          
         L     RE,ADREP                                                         
         USING REPREC,RE                                                        
         OC    RNAME,RNAME         DO WE HAVE A REP NAME                        
         BZ    PTSHBYR              NO -BYPASS                                  
         CLC   RNAME,SPACES                                                     
         BE    PTSHBYR                                                          
*---->   MVC   H5+96(3),=C'REP'                                                 
         MVC   H5+96(L'SP@REP),SP@REP                                           
         MVC   H5+104(3),REPKREP                                                
         MVC   H5+108(L'RNAME),RNAME                                            
         DROP  RE                                                               
PTSHBYR  L     RE,ADSTAT                                                        
         USING STAREC,RE                                                        
         CLC   SCHNL,=C'    '                                                   
         BE    SCHDONE                                                          
         CLC   SCHNL,=C'0000'                                                   
         BE    SCHDONE                                                          
         OC    SCHNL,SCHNL                                                      
         BZ    SCHDONE                                                          
*---->   MVC   H6+96(7),=C'CHANNEL'                                             
         MVC   H6+96(L'SP@CHANN),SP@CHANN                                       
         MVC   H6+105(2),SCHNL                                                  
         CLI   QMED,C'R'                                                        
         BNE   SCHDONE                                                          
*---->   MVC   H6+96(7),=C'FREQ   '                                             
         MVC   H6+96(L'SP@FREQ),SP@FREQ                                         
         MVC   H6+103(4),SCHNL                                                  
SCHDONE  DS    0H                                                               
         CLI   D5ASUPA,C'Y'        SUPPRESS AFFILIATES                          
         BE    AFFDONE                                                          
         OC    SNETWRK,SNETWRK                                                  
         BZ    AFFDONE                                                          
         CLC   SNETWRK,=C'000'                                                  
         BE    AFFDONE                                                          
         CLC   SNETWRK,=C'   '                                                  
         BE    AFFDONE                                                          
*---->   MVC   H6+108(9),=C'AFFILIATE'                                          
         MVC   H6+108(L'SP@AFFIL),SP@AFFIL                                      
         MVC   H6+118(3),SNETWRK                                                
         DROP  R5                                                               
AFFDONE  DS    0H                                                               
         SPACE 2                                                                
         CLI   MODE,STALAST        BYPASS CONTRACT GT. STALAST                  
         BH    PTHEXIT                                                          
         CLI   IDSW,1              MULTIPLE IDS ON PAGE                         
         BE    PTHID4                                                           
PTHID1   CLI   QBYID,C'Y'                                                       
         BNE   *+16                                                             
         MVC   H4(12),BUYIDNAM                                                  
         MVC   H4+13(12),BUYID                                                  
         B     PTHEXIT                                                          
         DROP  RE                                                               
         SPACE 2                                                                
PTHID4   MVC   MID1(12),BUYIDNAM                                                
         MVC   MID1+13(12),BUYID                                                
         MVI   FORCEMID,C'Y'                                                    
         SPACE 2                                                                
PTHEXIT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRSHEAD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*---->   MVC   H12(3),=C'DAY'                                                   
         MVC   H12(L'SP@DAY),SP@DAY                                             
         LA    R0,L'SP@DAY                                                      
         GOTO1 UNDERLIN,DMCB,((R0),H12),H13                                     
*---->   MVC   H12+9(4),=C'TIME'                                                
         MVC   H12+9(L'SP@TIME),SP@TIME                                         
         LA    R0,L'SP@TIME                                                     
         GOTO1 UNDERLIN,DMCB,((R0),H12+9),H13+9                                 
*---->   MVC   H12+21(11),=C'PROGRAMMING'                                       
         MVC   H12+21(L'SP@PROG),SP@PROG                                        
         LA    R0,L'SP@PROG                                                     
         GOTO1 UNDERLIN,DMCB,((R0),H12+21),H13+21                               
*---->   MVC   H12+37(3),=C'LEN'                                                
         MVC   H12+37(L'SP@LEN),SP@LEN                                          
         MVC   H13+37(3),DASH                                                   
         MVI   H12+41,C'-'                                                      
         SPACE 2                                                                
*        CENTER FRID HEADLINES                                                  
         ZIC   RF,LENGRID          MOVE IN DASHES                               
         MH    RF,NOINGRID                                                      
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   H12+42(0),H12+41                                                 
         LA    RE,H12+42           CENTER CAPTION                               
         SH    RE,=H'7'                                                         
         SRL   RF,1                                                             
         AR    RE,RF                                                            
*---->   MVC   0(17,RE),=C'ROTATION SCHEDULE'                                   
         MVC   0(L'SP@ROTSC,RE),SP@ROTSC                                        
*---->   MVC   H1+37(25),=C'STATION ROTATION SCHEDULE'                          
         MVC   H1+37(L'SP@STARS),SP@STARS                                       
         MVI   H2+37,C'-'                                                       
         DROP  R5                                                               
         LA    R1,H1+35                                                         
         LA    R8,BIGSTA+8                                                      
         LA    R6,9                                                             
RSSCALP  CLI   0(R8),C' '                                                       
         BE    RSSCALP1                                                         
         MVC   0(1,R1),0(R8)                                                    
         MVI   132(R1),C'-'                                                     
         BCTR  R1,0                                                             
RSSCALP1 BCTR  R8,0                                                             
         BCT   R6,RSSCALP                                                       
         MVC   H2+38(24),H2+37                                                  
         L     RE,ADSTATAD                                                      
         USING ADDRREC,RE                                                       
         MVC   H5+76(33),SPACES                                                 
         MVC   H6+76(20),ANAME                                                  
         CLI   BIGSTA,C'0'         TEST CABLE STATION                           
         BL    *+14                                                             
         L     R1,ADSTAT           YES-PRINT CABLE SYSTEM NAME                  
         MVC   H6+76(24),SSYSNAME-STAREC(R1)                                    
         MVC   H7+76(24),A1LINE                                                 
         MVC   H8+76(24),A2LINE                                                 
         MVC   H9+76(L'ABIGZIP),A3LINE                                          
         MVC   P1(40),SVP1                                                      
         MVC   P2(40),SVP2                                                      
         DROP  RE                                                               
         SPACE 2                                                                
         CLI   MODE,STALAST        BYPASS CONTRACT GT. STALAST                  
         BH    RSSCALP2                                                         
         CLI   QBYID,C'Y'                                                       
         BNE   *+16                                                             
         MVC   H4(12),BUYIDNAM                                                  
         MVC   H4+13(12),BUYID                                                  
*                                                                               
RSSCALP2 CLC   =C'RS',RTYPE        IF RS FAX REPORT                             
         BE    *+10                                                             
         CLC   =C'RY',RTYPE        IF RY FAX REPORT                             
         BNE   RSSCALP5                                                         
         CLI   IDSW,1              AND MULTIPLE ID'S ON PAGE                    
         BNE   RSSCALP5                                                         
         XC    H4(25),H4           CLEAR CONTRACT                               
         MVC   MID1(12),BUYIDNAM                                                
         MVC   MID1+13(12),BUYID                                                
RSSCALP5 DS    0H                                                               
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* SALESPERSONS SUMMARY                                                          
SALSUM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         XC    SVP1,SVP1                                                        
         OC    SUMOPTS,SUMOPTS                                                  
         BZ    SALSUMX2                                                         
         MVI   BUFHI,1             SET FOR READ HI                              
         MVI   BUFRTYP,1                                                        
SALDPT   DS    0H                  DAYPART PRINT RETURN                         
         MVI   ALLOWLIN,6                                                       
         MVI   FORCEMID,C'Y'                                                    
         CLI   MODE,STALAST                                                     
         BNE   SALMODE                                                          
*---->   MVC   MID1+56(19),=C'***STATION TOTAL***'                              
         MVC   MID1+56(L'SP@STATL),SP@STATL                                     
         CLI   CONEND,C'Y'         CAN THIS BE A CONTRACT TOTAL                 
         BNE   SALMODE             NO                                           
         CLI   QBYID,C'Y'                                                       
         BNE   SALMODE                                                          
         MVC   MID1+38(3),=C'***'                                               
         MVC   MID1+41(12),BUYIDNAM                                             
         MVC   MID1+54(12),BUYID                                                
         GOTO1 SQUASHER,DMCB,MID1+38,38                                         
SALMODE  CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID1+56(18),=C'***MARKET TOTAL***'                               
         MVC   MID1+56(L'SP@MKTTL),SP@MKTTL                                     
         CLI   MODE,PRDLAST                                                     
         BNE   SALDPT1                                                          
*---->   MVC   MID1+56(19),=C'***PRODUCT TOTAL***'                              
         MVC   MID1+56(L'SP@PROTL),SP@PROTL                                     
         MVI   FORCEHED,C'Y'                                                    
SALDPT1  DS    0H                                                               
         CLI   SUMOPTS,0           SUPPRESS TELECASTS                           
         BE    SALSUMA              YES                                         
         MVI   MID1,0                                                           
         MVI   FORCEMID,C'Y'                                                    
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
         CLI   DNAME1,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME1,C'R'                                                      
         BE    *+16                                                             
         MVC   MID2+44(7),DNAME1                                                
         MVC   MID2+51(5),=C'(000)'                                             
         CLI   DNAME1,0                                                         
         BNE   *+10                                                             
         XC    MID2+44(12),MID2+44                                              
         MVC   MID2+64(7),DNAME2                                                
         CLI   DNAME2,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME2,C'R'                                                      
         BE    *+16                                                             
         MVC   MID2+59(7),DNAME2                                                
         MVC   MID2+66(5),=C'(000)'                                             
         CLI   DNAME2,0                                                         
         BNE   *+10                                                             
         XC    MID2+59(12),MID2+59                                              
         MVC   MID2+79(7),DNAME3                                                
         CLI   DNAME3,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME3,C'R'                                                      
         BE    *+16                                                             
         MVC   MID2+74(7),DNAME3                                                
         MVC   MID2+81(5),=C'(000)'                                             
         CLI   DNAME3,0                                                         
         BNE   *+10                                                             
         XC    MID2+74(12),MID2+74                                              
         MVC   MID2+94(7),DNAME4                                                
         CLI   DNAME4,C'E'                                                      
         BE    *+8                                                              
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
SALSUM1  GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTS            
         BH    SALSUM1A                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    SALSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    SALSUM1                                                          
SALSUM1A DS    0H                                                               
         CLI   SUMSLN,X'FD'        SET SPILL MID LINE                           
         BNE   *+10                                                             
*---->   MVC   MID1+48(9),=C'***SPILL '                                         
         MVC   MID1+48(L'SP8SPILL),SP8SPILL                                     
         CLI   SUMSLN,X'FE'        SET ORIG MID LINE                            
         BNE   *+10                                                             
*---->   MVC   MID1+48(9),=C'***ORIG. '                                         
         MVC   MID1+48(L'SP@ORIG),SP@ORIG                                       
         DROP  R5                                                               
         CLI   SUMRTYP,0                                                        
         BE    SALSUMX                                                          
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
         CLI   SUMDPART,C''       SPECIAL DETAIL SUPPRESS                      
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
         MVI   MID1,0                                                           
         MVI   FORCEMID,C'Y'                                                    
*---->   MVC   MID2(7),=C'MGR TOT'                                              
         MVC   MID2(L'SP7MGRTL),SP7MGRTL                                        
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID2(7),=C'MKT TOT'                                              
         MVC   MID2(L'SP7MKTTL),SP7MKTTL                                        
         CLI   MODE,PRDLAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID2(7),=C'PRD TOT'                                              
         MVC   MID2(L'SP7PROTL),SP7PROTL                                        
         B     SALSUM3                                                          
SALSUM2A MVC   MID2(3),SUMDPART                                                 
         EDIT  (1,SUMSLN),(3,MID2+4)                                            
         MVI   MID2+3,C'-'                                                      
         MVI   FORCEMID,C'Y'                                                    
         MVI   MID1,0                                                           
SALSUM3  L     RF,0(R5)            CALCULATE WEEKLY AVERAGES                    
         LTR   RF,RF                                                            
         BM    SALSUM3A                                                         
         SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,SALSWKS                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R5)                                                         
SALSUM3A LA    R5,4(R5)                                                         
         BCT   R6,SALSUM3                                                       
* PRINT SALESPERSONS SUMMARY                                                    
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P1(5),=C'TOTAL'                                                  
         MVC   P1(L'SP5TOTAL),SP5TOTAL                                          
         CLI   SUMOPTS,0                                                        
         BE    SALSUM10                                                         
         EDIT  (4,SUMSPOTS),(8,P1+30)                                           
SALSUM10 CLI   SUMOPTS+1,0                                                      
         BE    SALSUM11                                                         
         MVI   CURTAB+3,2                                                       
         CURED (4,SUMDL),(12,P1+105),CURTAB,DMCB=CDMCB,CURSYMB=YES              
SALSUM11 CLI   SUMOPTS+2,0                                                      
         BE    SALSUM12                                                         
         CLI   DNAME1,0                                                         
         BE    SALSUM12                                                         
         MVI   CURTAB+3,1                                                       
         CURED (4,SUMD1),(9,P1+47),CURTAB,DMCB=CDMCB                            
         CLI   DNAME2,0                                                         
         BE    SALSUM12                                                         
         CURED (4,SUMD2),(9,P1+62),CURTAB,DMCB=CDMCB                            
         CLI   DNAME3,0                                                         
         BE    SALSUM12                                                         
         CURED (4,SUMD3),(9,P1+77),CURTAB,DMCB=CDMCB                            
         CLI   DNAME4,0                                                         
         BE    SALSUM12                                                         
         CURED (4,SUMD4),(9,P1+92),CURTAB,DMCB=CDMCB                            
SALSUM12 DS    0H                                                               
         MVI   CURTAB+3,0                                                       
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
         MVI   CURTAB+3,2                                                       
         CURED (4,SALSDL),(12,P1+105),CURTAB,DMCB=CDMCB,CURSYMB=YES             
SALSUM21 CLI   SUMOPTS+2,0                                                      
         BE    SALSUM22                                                         
         CLI   DNAME1,0                                                         
         BE    SALSUM22                                                         
         MVI   CURTAB+3,1                                                       
         CURED (4,SALSD1),(9,P1+47),CURTAB,DMCB=CDMCB                           
         CLI   DNAME2,0                                                         
         BE    SALSUM22                                                         
         CURED (4,SALSD2),(9,P1+62),CURTAB,DMCB=CDMCB                           
         CLI   DNAME3,0                                                         
         BE    SALSUM22                                                         
         CURED (4,SALSD3),(9,P1+77),CURTAB,DMCB=CDMCB                           
         CLI   DNAME4,0                                                         
         BE    SALSUM22                                                         
         CURED (4,SALSD4),(9,P1+92),CURTAB,DMCB=CDMCB                           
SALSUM22 DS    0H                                                               
         MVI   CURTAB+3,0                                                       
         GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,6                                                       
SALSUMX  MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   FORCEMID,C'N'                                                    
         XC    SALSDATA,SALSDATA                                                
         CLI   SUMRTYP,0                                                        
         BE    *+12                                                             
         CLI   SUMDPGNO,X'FF'                                                   
         BNE   SALSUM1                                                          
SALSUMX2 XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
BRSSUM   NTR1  BASE=*,LABEL=*                                                   
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         OC    SUMOPTS,SUMOPTS                                                  
         BZ    BRSSUMX                                                          
         SPACE 2                                                                
*RECREATE TOTAL MEDTAB                                                          
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVI   BUFHI,1                                                          
         MVI   BUFRTYP,1                                                        
         GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         MVI   BUFHI,1                                                          
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTSALS         
         BH    BRSMKT                                                           
         CLI   BUFCDE,X'88'        SPILL DATA                                   
         BE    BRSSUMX                                                          
         CLI   BUFCDE,X'89'        ORIG DATA                                    
         BE    BRSSUMX                                                          
BRSMKT   DS    0H                                                               
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         CLI   MODE,STALAST                                                     
         BNE   BRSMODE                                                          
*---->   MVC   P+56(19),=C'***STATION TOTAL***'                                 
         MVC   P+56(L'SP@STATL),SP@STATL                                        
         CLI   CONEND,C'Y'         CAN THIS BE A CONTRACT TOTAL                 
         BNE   BRSMODE             NO                                           
         CLI   QBYID,C'Y'                                                       
         BNE   BRSMODE                                                          
         MVC   P+38(3),=C'***'                                                  
         MVC   P+41(12),BUYIDNAM                                                
         MVC   P+54(12),BUYID                                                   
         GOTO1 SQUASHER,DMCB,P+38,38                                            
BRSMODE  CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
*---->   MVC   P+56(18),=C'***MARKET TOTAL***'                                  
         MVC   P+56(L'SP@MKTTL),SP@MKTTL                                        
         CLI   MODE,PRDLAST                                                     
         BNE   BRSDPT                                                           
         MVI   FORCEHED,C'Y'                                                    
*---->   MVC   P+56(19),=C'***PRODUCT TOTAL***'                                 
         MVC   P+56(L'SP@PROTL),SP@PROTL                                        
BRSDPT   DS    0H                                                               
         CLI   SUMSLN,X'FD'        SET SPILL MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***SPILL '                                            
         MVC   P+48(L'SP8SPILL),SP8SPILL                                        
         CLI   SUMSLN,X'FE'        SET ORIG. MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***ORIG. '                                            
         MVC   P+48(L'SP@ORIG),SP@ORIG                                          
         DROP  R5                                                               
         L     R5,MEDAFRST         CLEAR MEDBLOCK                               
         LA    R6,MEDPERD                                                       
CLRBRS   L     RE,4(R5)                                                         
         LTR   RE,RE                                                            
         BZ    CLRBRS2                                                          
         L     RF,MEDLCHNK                                                      
         XCEF                                                                   
CLRBRS2  LA    R5,12(R5)                                                        
         OC    4(4,R5),4(R5)                                                    
         BNZ   *+10                                                             
         XC    0(12,R5),0(R5)                                                   
         CR    R5,R6                                                            
         BH    CLRBRS3                                                          
         B     CLRBRS                                                           
*                                                                               
CLRBRS3  DS    0H                                                               
BRSSUM1  GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTSALS         
         BH    BRSSUM1A                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    BRSSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    BRSSUM1                                                          
BRSSUM1A DS    0H'0'                                                            
*                                                                               
         CLI   SUMDPART,C''       SPECIAL DETAIL SUPPRESS                      
         BE    BRSSUM1                                                          
         CLI   SUMDPGRP,C''                                                    
         BE    BRSSUM1                                                          
*                                                                               
         L     R5,MEDAFRST                                                      
         CLI   SUMRTYP,0                                                        
         BE    BRSSUMX                                                          
         CLI   SUMRTYP,2           BYPASS MONTHLY                               
         BE    BRSSUM1                                                          
         CLI   SUMRTYP,3           SET FOR PERIOD                               
         BNE   BRSSUM2                                                          
         LA    R5,MEDPERD                                                       
         B     BRSSUM3                                                          
BRSSUM2  CLC   2(2,R5),SUMDT+2     FIND CORRECT BUFFER SLOT                     
         BE    BRSSUM3                                                          
         LA    R5,12(R5)                                                        
         B     BRSSUM2                                                          
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
* PRINT BRS SUMMARY                                                             
         L     R5,MEDAFRST                                                      
BRSSUM4  L     R4,4(R5)                                                         
         LTR   R4,R4                                                            
         BZ    BRSSUM8A                                                         
         LA    R8,15                                                            
         LA    R9,P2+11                                                         
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   P2(7),=C'MKT TOT'                                                
         MVC   P2(L'SP7MKTTL),SP7MKTTL                                          
BRSSUM41 DS    0H                                                               
*---->   MVC   P2(7),=C'*TOTAL*'                                                
         MVC   P2(L'SP7TOTAL),SP7TOTAL                                          
         MVI   ALLOWLIN,10                                                      
         CLI   SUMDPGRP,X'FF'                                                   
         BE    BRSSUM4A                                                         
         MVC   P2(3),SUMDPART                                                   
         EDIT  (1,SUMSLN),(3,P2+4)                                              
         L     RE,=A(DICSECT)                                                   
         MVI   P2+3,C'-'                                                        
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
BRSSUM4B CLI   SUMOPTS+2,0                                                      
         BE    *+14                                                             
         MVC   0(7,R6),DNAME1                                                   
         LA    R6,132(R6)                                                       
         CLI   SUMOPTS+1,0                                                      
         BE    *+14                                                             
*---->   MVC   0(7,R6),=C'DOLLARS'                                              
         MVC   0(L'SP7DOLLA,R6),SP7DOLLA                                        
         LA    R6,132(R6)                                                       
         CLI   SUMOPTS+2,0                                                      
         BE    *+14                                                             
*---->   MVC   0(9,R6),=C'GOAL DEMO'                                            
         MVC   0(L'SP@GOADE,R6),SP@GOADE                                        
         LA    R6,132(R6)                                                       
         CLI   SUMOPTS+1,0                                                      
         BE    *+10                                                             
*---->   MVC   0(6,R6),=C'GOAL $'                                               
         MVC   0(L'SP@GOALM,R6),SP@GOALM                                        
BRSSUM5  LA    R6,MEDPERD                                                       
         L     RE,=A(DICSECT)                                                   
         OC    0(4,R5),0(R5)       SLOT ACTIVE                                  
         BZ    BRSSUM8A             NO - BYPASS                                 
         CR    R5,R6                                                            
         BL    BRSSUM6                                                          
         BH    BRSSUM9                                                          
*---->   MVC   WORK(5),=C'TOTAL'                                                
         MVC   WORK(L'SP5TOTAL),SP5TOTAL                                        
         DROP  RE                                                               
         B     BRSSUM7                                                          
*                                                                               
BRSSUM6  GOTO1 DATCON,DMCB,(X'02',(R5)),(X'04',WORK)                            
BRSSUM7  DS    0H                                                               
         OC    MEDGLD,MEDGLD                                                    
         BNZ   *+14                                                             
         OC    MEDBYSPT,MEDBYSPT                                                
         BZ    BRSSUM8A                                                         
         MVC   3(5,R9),WORK                                                     
         ST    R9,PRTADDR                                                       
         LA    R9,132(R9)                                                       
         LA    R9,132(R9)                                                       
         CLI   SUMOPTS,0                                                        
         BE    BRSSUM7A                                                         
         EDIT  (4,MEDBYSPT),(5,3(R9))                                           
         LA    R9,132(R9)                                                       
BRSSUM7A CLI   SUMOPTS+2,0                                                      
         BE    BRSSUM7B                                                         
         L     RF,MEDBY1                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
BRSSUM7B CLI   SUMOPTS+1,0                                                      
         BE    BRSSUM7C                                                         
         L     RF,MEDBYD                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
BRSSUM7C CLI   SUMOPTS+2,0                                                      
         BE    BRSSUM7D                                                         
         L     RF,MEDGL1                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
BRSSUM7D CLI   SUMOPTS+1,0                                                      
         BE    BRSSUM8                                                          
         L     RF,MEDGLD                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
*                                                                               
BRSSUM8  L     R9,PRTADDR          SET NEXT SLOT                                
         LA    R9,8(R9)                                                         
BRSSUM8A LA    R5,12(R5)                                                        
         L     R4,4(R5)                                                         
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BH    BRSSUM9                                                          
         OC    0(4,R5),0(R5)                                                    
         BZ    BRSSUM8A                                                         
         LA    R6,MEDMON01                                                      
         BCTR  R6,0                                                             
         CR    R5,R6                                                            
         BH    *+12                                                             
         BCT   R8,BRSSUM5                                                       
         B     BRSSUM9                                                          
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         BCT   R8,BRSSUM5                                                       
*                                                                               
BRSSUM9  GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         MVI   ALLOWLIN,0                                                       
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BNH   BRSSUM4                                                          
         B     BRSDPT                                                           
BRSSUMX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* BTS SUMMARY                                                                   
BTSSUM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
         OC    SUMOPTS,SUMOPTS                                                  
         BZ    BTSSUMX                                                          
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTSALS         
         BH    BTSMKT                                                           
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
         MVI   P1,0                                                             
         MVI   BUFHI,1             SET FOR READ HI                              
         MVI   BUFRTYP,2                                                        
         CLI   MODE,STALAST                                                     
         BNE   BTSMODE                                                          
*---->   MVC   P+56(19),=C'***STATION TOTAL***'                                 
         MVC   P+56(L'SP@STATL),SP@STATL                                        
         CLI   CONEND,C'Y'         CAN THIS BE A CONTRACT TOTAL                 
         BNE   BTSMODE             NO                                           
         CLI   QBYID,C'Y'                                                       
         BNE   BTSMODE                                                          
         MVC   P+38(3),=C'***'                                                  
         MVC   P+41(12),BUYIDNAM                                                
         MVC   P+54(12),BUYID                                                   
         GOTO1 SQUASHER,DMCB,P+38,38                                            
BTSMODE  CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
*---->   MVC   P+56(18),=C'***MARKET TOTAL***'                                  
         MVC   P+56(L'SP@MKTTL),SP@MKTTL                                        
         CLI   MODE,PRDLAST                                                     
         BNE   BTSDPT                                                           
*---->   MVC   P+56(19),=C'***PRODUCT TOTAL***'                                 
         MVC   P+56(L'SP@PROTL),SP@PROTL                                        
         CLI   MODE,CLTLAST                                                     
         BNE   BTSDPT                                                           
*---->   MVC   P+56(18),=C'***CLIENT TOTAL***'                                  
         MVC   P+56(L'SP@CLITL),SP@CLITL                                        
BTSDPT   DS    0H                                                               
         CLI   SUMSLN,X'FD'        SET SPILL MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***SPILL '                                            
         MVC   P+48(L'SP8SPILL),SP8SPILL                                        
         CLI   SUMSLN,X'FE'        SET ORIG. MID LINE                           
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***ORIG. '                                            
         MVC   P+48(L'SP@ORIG),SP@ORIG                                          
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
         MVC   MID1+20(L'SP@NOTLC),SP@NOTLC                                     
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         CLI   QMED,C'X'                                                        
         BNE   *+10                                                             
*---->   MVC   MID1+20(9),=C'NO BRCSTS'                                         
         MVC   MID1+20(L'SP@NOBRD),SP@NOBRD                                     
         MVC   MID2+20(9),=9C'-'                                                
BTSSUMA  CLI   SUMOPTS+1,0                                                      
         BE    BTSSUMB                                                          
*---->   MVC   MID1+34(7),=C'DOLLARS'                                           
         MVC   MID1+34(L'SP7DOLLA),SP7DOLLA                                     
         MVC   MID2+34(7),=7C'-'                                                
BTSSUMB  CLI   SUMOPTS+2,0                                                      
         BE    BTSSUMC                                                          
         LA    RE,MID1+49                                                       
         LA    R9,MID2+49                                                       
         LA    RF,DNAME1                                                        
         LA    R0,4                                                             
BTSSUMB1 CLI   0(RF),0                                                          
         BE    BTSSUMC                                                          
         MVC   0(7,RE),0(RF)                                                    
         MVC   0(7,R9),=7C'-'                                                   
         LA    RE,20(RE)                                                        
         LA    RF,7(RF)                                                         
         LA    R9,20(R9)                                                        
         BCT   R0,BTSSUMB1                                                      
BTSSUMC  DS    0H                                                               
*---->   MVC   P1(5),=C'MONTH'                                                  
         MVC   P1(L'SP@MONTH),SP@MONTH                                          
BTSSUM1  GOTO1 VGETBUF                                                          
         LA    R3,MYBUFIO                                                       
         CLI   SUMRTYP,0                                                        
         BE    BTSSUMX                                                          
         CLI   SUMRTYP,1                                                        
         BE    BTSSUM1                                                          
         OC    SUMSPOTS,SUMSPOTS                                                
         BZ    BTSSUM1                                                          
*                                                                               
         CLI   SUMDPART,C''       SPECIAL DETAIL SUPPRESS                      
         BE    BTSSUM1                                                          
         CLI   SUMDPGRP,C''                                                    
         BE    BTSSUM1                                                          
*                                                                               
         CLI   DPTSW,1                                                          
         BNE   BTSSUM1A                                                         
         MVC   P1+7(3),=C'TOT'                                                  
*---->   MVC   MID1(7),=C'*TOTAL*'                                              
         MVC   MID1(L'SP7TOTAL),SP7TOTAL                                        
         CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID1(7),=C'MKT TOT'                                              
         MVC   MID1(L'SP7MKTTL),SP7MKTTL                                        
         CLI   MODE,PRDLAST                                                     
         BNE   *+10                                                             
*---->   MVC   MID1(7),=C'PRD TOT'                                              
         MVC   MID1(L'SP7PROTL),SP7PROTL                                        
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
*                                                                               
BTSSUM2  CLI   SUMOPTS,0                                                        
         BE    BTSSUM2B                                                         
         EDIT  SUMSPOTS,(10,P+19)                                               
*                                                                               
BTSSUM2B CLI   SUMOPTS+1,0                                                      
         BE    BTSSUM2C                                                         
         L     R9,SUMDL                                                         
         SR    R8,R8                                                            
         SLA   R9,1                                                             
         D     R8,=F'100'                                                       
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         EDIT  (R9),(9,P+33),FLOAT=-                                            
*                                                                               
BTSSUM2C MVC   FULL,=F'1'          SET SPOT COUNT FOR NOT AVERAGING             
         MVC   WEIGHT,=F'1'                                                     
         GOTO1 VCALCPP,DMCB,FULL                                                
*                                                                               
         LA    R4,SVDEMS                                                        
         LA    R5,P+44                                                          
         LA    R6,4                                                             
         LA    R7,DNAMES                                                        
*                                                                               
BTSSUM3  OC    0(4,R4),0(R4)                                                    
         BZ    BTSSUM5                                                          
         CLI   SUMOPTS+2,0                                                      
         BE    BTSSUM7                                                          
*                                                                               
         L     R8,0(R4)            GET DEMO VALUE                               
         CLI   1(R7),C'R'          TEST RATING                                  
         BE    *+12                                                             
         CLI   1(R7),C'E'          OR EXTENDED RATING                           
         BNE   BTSSUM3A                                                         
         M     R8,=F'2'                                                         
         D     R8,=F'10'                                                        
         AHI   R9,1                                                             
         SRL   R9,1                                                             
*                                                                               
BTSSUM3A C     R8,=F'99999'        WILL VALUE FIT WITH 1 DECIMAL                
         BH    BTSSUM4              NO - DIVIDE BY 10                           
         MVI   CURTAB+3,1                                                       
*                                   YES - MAINTAIN PRECISION                    
         CURED (R8),(7,(R5)),CURTAB,DMCB=CDMCB                                  
         MVI   CURTAB+3,0                                                       
         B     BTSSUM5                                                          
*                                                                               
BTSSUM4  M     R8,=F'2'            DROP DECIMAL PRECISION                       
         D     R8,=F'10'                                                        
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         EDIT  (R9),(7,(R5))                                                    
*                                                                               
BTSSUM5  LA    R5,7(R5)                                                         
         LA    R4,4(R4)                                                         
         L     R8,0(R4)                                                         
         LTR   R8,R8                                                            
         BZ    BTSSUM6                                                          
         CLI   SUMOPTS+1,0                                                      
         BE    BTSSUM6                                                          
         MVI   CURTAB+3,2                                                       
         CURED (R8),(7,(R5)),CURTAB,DMCB=CDMCB                                  
         MVI   CURTAB+3,0                                                       
         CLC   SUMDL,SUMDLEQ                                                    
         BE    *+8                                                              
         MVI   7(R5),C'+'                                                       
         C     R8,=F'99999'                                                     
         BNH   BTSSUM6                                                          
         SRDA  R8,32                                                            
         SLA   R9,1                                                             
         D     R8,=F'10'                                                        
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         MVI   CURTAB+3,1                                                       
         CURED (R9),(7,(R5)),CURTAB,DMCB=CDMCB                                  
         MVI   CURTAB+3,0                                                       
BTSSUM6  LA    R4,4(R4)                                                         
         LA    R5,13(R5)                                                        
         BCT   R6,BTSSUM3                                                       
BTSSUM7  GOTO1 REPORT                                                           
         GOTO1 VFOOT,DMCB,(RA)                                                  
         CLI   SUMRTYP,3                                                        
         BNE   BTSSUM1                                                          
         CLI   SUMDPGNO,X'FF'                                                   
         BE    BTSSUMX                                                          
         B     BTSDPT                                                           
BTSSUMX  MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
BDSSUM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         SPACE 2                                                                
* RECREATE TOTAL MEDTAB                                                         
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMQT,=F'0'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVI   BUFHI,1                                                          
         MVI   BUFRTYP,1                                                        
         L     R8,=A(DICSECT)                                                   
         USING DICSECT,R8                                                       
         CLI   MODE,STALAST                                                     
         BNE   BDSMODE                                                          
*---->   MVC   P+56(19),=C'***STATION TOTAL***'                                 
         MVC   P+56(L'SP@STATL),SP@STATL                                        
         CLI   CONEND,C'Y'         CAN THIS BE A CONTRACT TOTAL                 
         BNE   BDSMODE                                                          
         CLI   QBYID,C'Y'                                                       
         BNE   BDSMODE                                                          
         MVC   P+38(3),=C'***'                                                  
         MVC   P+41(12),BUYIDNAM                                                
         MVC   P+54(12),BUYID                                                   
         GOTO1 SQUASHER,DMCB,P+38,23                                            
BDSMODE  CLI   MODE,MKTLAST                                                     
         BNE   *+10                                                             
*---->   MVC   P+56(18),=C'***MARKET TOTAL***'                                  
         MVC   P+56(L'SP@MKTTL),SP@MKTTL                                        
         CLI   MODE,PRDLAST                                                     
         BNE   *+14                                                             
         MVI   FORCEHED,C'Y'                                                    
*---->   MVC   P+56(19),=C'***PRODUCT TOTAL***'                                 
         MVC   P+56(L'SP@PROTL),SP@PROTL                                        
BDSDPT   DS    0H                                                               
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
         LA    R3,MYBUFIO                                                       
         CLI   MODE,STALAST        BYPASS SPILL AND ORIG ON STA TOTALS          
         BH    BDSSUM11                                                         
         CLI   SUMSLN,X'FD'        SPILL DATA                                   
         BE    BDSSUM1                                                          
         CLI   SUMSLN,X'FE'        ORIG DATA                                    
         BE    BDSSUM1                                                          
BDSSUM11 DS    0H'0'                                                            
*                                                                               
         CLI   SUMDPART,C''       SPECIAL DETAIL SUPPRESS                      
         BE    BDSSUM1                                                          
         CLI   SUMDPGRP,C''                                                    
         BE    BDSSUM1                                                          
*                                                                               
         CLI   SUMSLN,X'FD'        SET SPILL MIDLINE                            
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***SPILL '                                            
         MVC   P+48(L'SP8SPILL),SP8SPILL                                        
         CLI   SUMSLN,X'FE'        SET ORIG MID LINE                            
         BNE   *+10                                                             
*---->   MVC   P+48(9),=C'***ORIG. '                                            
         MVC   P+48(L'SP@ORIG),SP@ORIG                                          
         DROP  R8                                                               
         CLI   SUMRTYP,0                                                        
         BE    BDSSUMX                                                          
         CLI   SUMRTYP,2           BYPASS MONTHLY                               
         BE    BDSSUM1                                                          
         CLI   SUMRTYP,3           SET FOR PERIOD                               
         BNE   BDSSUM2                                                          
         LA    R5,MEDPERD                                                       
         B     BDSSUM3                                                          
BDSSUM2  CLC   2(2,R5),SUMDT+2     FIND CORRECT BUFFER CSLOT                    
         BE    BDSSUM3                                                          
         LA    R5,12(R5)                                                        
         B     BDSSUM2                                                          
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
*                                                                               
* PRINT SUMMARY                                                                 
         L     R5,MEDAFRST                                                      
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
BDSSUM4A MVI   P3,0                                                             
         LA    R6,P4                                                            
         MVI   ALLOWLIN,10                                                      
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   SUMOPTS,0                                                        
         BE    *+14                                                             
*---->   MVC   0(7,R6),=C'  SPOTS'                                              
         MVC   0(L'SP@SPOTS,R6),SP@SPOTS                                        
         LA    R6,132(R6)                                                       
         CLI   SUMOPTS+1,0                                                      
         BE    *+14                                                             
*---->   MVC   0(7,R6),=C'   COST'                                              
         MVC   0(L'SP@COST,R6),SP@COST                                          
         LA    R6,132(R6)                                                       
         CLI   SUMOPTS+2,0                                                      
         BE    BDSSUM5                                                          
         MVC   0(7,R6),DNAME1                                                   
         LA    R6,132(R6)                                                       
         MVC   0(7,R6),DNAME2                                                   
         LA    R6,132(R6)                                                       
         MVC   0(7,R6),DNAME3                                                   
         LA    R6,132(R6)                                                       
         MVC   0(7,R6),DNAME4                                                   
         SPACE 2                                                                
BDSSUM5  LA    R6,MEDPERD                                                       
         L     RE,=A(DICSECT)                                                   
         OC    0(4,R5),0(R5)       SLOT ACTIVE                                  
         BZ    BDSSUM8A             NO - BYPASS                                 
         CR    R5,R6                                                            
         BL    BDSSUM6                                                          
         BH    BDSSUM9                                                          
*---->   MVC   3(5,R9),=C'TOTAL'                                                
         MVC   3(L'SP5TOTAL,R9),SP5TOTAL                                        
         DROP  RE                                                               
         B     BDSSUM7                                                          
*                                                                               
BDSSUM6  GOTO1 DATCON,DMCB,(X'02',(R5)),(X'04',WORK)                            
         MVC   3(5,R9),WORK                                                     
BDSSUM7  ST    R9,PRTADDR                                                       
         OC    MEDBYSPT,MEDBYSPT                                                
         BNZ   *+12                                                             
         LA    R8,1(R8)                                                         
         B     BDSSUM8A                                                         
         LA    R9,132(R9)                                                       
         LA    R9,132(R9)                                                       
         CLI   SUMOPTS,0           SPOTS                                        
         BE    BDSSUM7A                                                         
         EDIT  (4,MEDBYSPT),(5,3(R9))                                           
         LA    R9,132(R9)                                                       
BDSSUM7A CLI   SUMOPTS+1,0         COST                                         
         BE    BDSSUM7B                                                         
         L     RF,MEDBYD                                                        
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         D     RE,=F'100'                                                       
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
*                                                                               
BDSSUM7B CLI   SUMOPTS+2,0         DEMOS                                        
         BE    BDSSUM8                                                          
         LA    R1,4                                                             
         LA    R6,MEDBY1                                                        
         LA    R7,DNAMES                                                        
*                                                                               
BDSSUM7C LHI   R0,10                                                            
         CLI   1(R7),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R7),C'E'                                                       
         BNE   BDSSUM7D                                                         
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC RTGS                              
         BZ    BDSSUM7E            NO                                           
         LHI   R0,100                                                           
         B     BDSSUM7E                                                         
*                                                                               
BDSSUM7D TM    RQOPTS,RQOPTS_2DECIMP  TEST 2-DEC IMPS                           
         JZ    BDSSUM7E                                                         
         LHI   R0,100                                                           
*                                                                               
BDSSUM7E L     RF,0(R6)                                                         
         M     RE,=F'2'                                                         
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         EDIT  (RF),(8,(R9))                                                    
         LA    R9,132(R9)                                                       
         LA    R6,8(R6)                                                         
         LA    R7,7(R7)                                                         
         BCT   R1,BDSSUM7C                                                      
         SPACE 2                                                                
BDSSUM8  L     R7,MEDBUFF          RESTORE MEDBLOCK REG                         
         L     R9,PRTADDR          SET NEXT SLOT                                
         LA    R9,8(R9)                                                         
BDSSUM8A LA    R5,12(R5)                                                        
         L     R4,4(R5)                                                         
         LA    R6,MEDPERD                                                       
         CR    R5,R6                                                            
         BH    BDSSUM9                                                          
         OC    0(4,R5),0(R5)                                                    
         BZ    BDSSUM8A                                                         
         LA    R6,MEDMON01                                                      
         BCTR  R6,0                                                             
         CR    R5,R6                                                            
         BH    *+12                                                             
         BCT   R8,BDSSUM5                                                       
         B     BDSSUM9                                                          
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
BDSSUMX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CALCPP   NTR1  BASE=*,LABEL=*      PARAM = SPOT COUNT FOR DEMO AVERAGE          
         USING SUMDSECT,R3                                                      
         L     RE,SUMDL                                                         
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         BNP   CALCPPX                                                          
         CLI   SPOTPROF+1,C'N'                                                  
         BE    UNWTX                                                            
         CLI   SPOTPROF+1,0                                                     
         BE    UNWTX                                                            
         OC    WEIGHT,WEIGHT                                                    
         BZ    UNWTX                                                            
         LA    R4,DNAMES                                                        
         LA    R5,14                                                            
         LA    R6,SUMD1                                                         
*                                                                               
UNWT1    CLI   SPOTPROF+1,C'D'     UNWEIGHT DEMOS                               
         BE    UNW1                                                             
         CLI   0(R4),C'E'                                                       
         BE    *+8                                                              
         CLI   0(R4),C'R'                                                       
         BNE   UNWT2                                                            
*                                                                               
UNW1     L     RF,0(R6)                                                         
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
UNWT2    LA    R4,7(R4)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,UNWT1                                                         
*                                                                               
UNWTX    DS    0H                                                               
         XC    SVD1(112),SVD1                                                   
         LHI   R0,14                                                            
         LA    R5,SVD1                                                          
         LA    R6,SUMD1                                                         
         LA    R7,DNAMES                                                        
*                                                                               
CALCPP1  L     RF,SUMDL            GET DOLLARS                                  
         CLI   CPPSW,C'D'                                                       
         BNE   *+8                                                              
         L     RF,SUMDLEQ          USE EQUIVALENCED DOLLARS                     
*                                                                               
         L     R8,0(R6)            GET DEMOS                                    
         CLI   CPPSW,C'D'                                                       
         BE    *+8                                                              
         L     R8,4(R6)            USE EQUIVALENCED DEMOS                       
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BNE   CALCPP1A                                                         
*                                                                               
         L     RF,SUMDL            USE UNEQV DOLLARS                            
         L     R8,0(R6)            AND DEMO                                     
*                                                                               
CALCPP1A CLI   0(R7),C'R'          TEST DEMO HAS 2 DEC                          
         BE    *+12                                                             
         CLI   0(R7),C'E'                                                       
         BNE   CALCPP1B                                                         
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC RTGS                              
         BZ    CALCPP1C                                                         
         MHI   RF,10               ADJUST DOLS FOR EXTRA DEC                    
         B     CALCPP1C                                                         
*                                                                               
CALCPP1B TM    RQOPTS,RQOPTS_2DECIMP   TEST 2-DEC IMPS                          
         BZ    CALCPP1C                                                         
         MHI   RF,10               ADJUST DOLS FOR EXTRA DEC                    
*                                                                               
CALCPP1C LTR   R8,R8               IF NO POINTS, STOP                           
         BZ    CALCPP4                                                          
*                                                                               
         M     RE,=F'20'           X 2 X 10 FOR DEMO DECIMAL                    
         DR    RE,R8                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,4(R5)            SAVE CPP                                     
*                                                                               
CALCPP2  L     R9,0(R6)            CALCULATE DEMO AVERAGES                      
         M     R8,=F'2'                                                         
         L     RF,0(R1)                                                         
         OC    0(4,RF),0(RF)                                                    
         BZ    CALCPPX                                                          
         D     R8,0(RF)                                                         
         AHI   R9,1                                                             
         SRA   R9,1                                                             
         ST    R9,0(R5)            SAVE DEMO                                    
*                                                                               
CALCPP4  LA    R5,8(R5)            GET NEXT                                     
         LA    R6,8(R6)                                                         
         LA    R7,7(R7)            NEXT DEMO NAME                               
         BCT   R0,CALCPP1                                                       
*                                                                               
         OC    SUMGDL(16),SUMGDL                                                
         BZ    CALCPPX                                                          
*                                                                               
         L     RE,SUMGDL                                                        
         CLI   CPPSW,C'D'                                                       
         BNE   *+8                                                              
         L     RF,SUMGDLE                                                       
*                                                                               
         L     R8,SUMGD1                                                        
         CLI   CPPSW,C'D'                                                       
         BE    *+8                                                              
         L     RF,SUMGD1E                                                       
         LTR   R8,R8                                                            
         BZ    CALCPPX                                                          
*                                                                               
         M     RE,=F'2'                                                         
         DR    RE,R8                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,4(R5)                                                         
*                                                                               
CALCPPX  XIT1                                                                   
         LTORG                                                                  
               EJECT                                                            
* PRINT POOL BUYSHEET GRID                                                      
*        0 - LENGTH OF GRID BLOCK                                               
*       1-3- A(SPWORK)                                                          
         SPACE 2                                                                
PTSGRID  NMOD1 10,PTSGRID                                                       
         L     RA,0(R1)                                                         
         LA    RA,0(RA)                                                         
         LR    R3,RC                                                            
         USING PTSGD,R3                                                         
         MVC   GRIDLEN,0(R1)                                                    
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SPD5WK,R2                                                        
         L     R6,VPGRID                                                        
         USING PGRIDD,R6                                                        
         EJECT                                                                  
*        SORT SPOTS INTO DAY/BRAND/SPOT LENGTH/TYPE ORDER                       
         L     R6,VPGRID           SET UP SORT KEYS                             
         LR    RE,R6                                                            
         USING PGSRT1D,RE                                                       
         L     R1,PGNOENT                                                       
         LTR   R1,R1                                                            
         BZ    PTSGRIDX                                                         
         LA    R7,1                                                             
PGCNDSRT XC    PGSORT,PGSORT                                                    
         MVC   PGDS1WK,PGDWK                                                    
         MVC   PGDS1DY,PGDFDAY                                                  
         XI    PGDS1DY,X'FF'                                                    
         MVI   PGDS1SLT,0                                                       
         CLI   PGCNDSW,1           CONDENSE REQUIRED                            
         BE    *+8                  YES - DONT SET SPOT NUMDBER                 
         STC   R7,PGDS1SLT          NO - SET SPOT NUMBER                        
         MVC   PGDSNO,PGDS1SLT                                                  
         MVC   PGDS1BR,PGDSBRN                                                  
         MVC   PGDS1PIG,PGD2BRN                                                 
         MVC   PGDS1SL,PGDSSLN                                                  
         MVC   PGDS1IND,PGDIND                                                  
         LA    R7,1(R7)            BUMP SPOT NUMBER                             
         LA    RE,PGDLEN(RE)                                                    
         LR    R6,RE                                                            
         BCT   R1,PGCNDSRT                                                      
         DROP  RE                                                               
         L     R8,PGNOENT                                                       
         L     R6,VPGRID                                                        
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
*        CONDENSE LIKE SPOTS                                                    
         CLI   PGCNDSW,1           CONDENSE - REQUIRED                          
         BNE   PGCNDX               NO - CHECK FORMAT TYPE                      
*              TABLE SORTED NOW CONDENSE                                        
         L     R6,VPGRID                                                        
PGCND1   OC    0(PGDLEN,R6),0(R6)  END                                          
         BZ    PGCND2                                                           
         MVI   PGDSNO,1            SET SPOT NUMBER TO 1                         
         LA    R6,PGDLEN(R6)                                                    
         B     PGCND1                                                           
         SPACE 2                                                                
PGCND2   L     R6,VPGRID                                                        
         LR    RE,R6                                                            
         LR    R4,R6                                                            
PGCND3   LA    R4,PGDLEN(R4)       SET POINTER TO NEXT SLOT                     
         OC    0(PGDLN1,R4),0(R4)                                               
         BZ    PGCND4                                                           
         CLC   0(L'PGSORT,RE),0(R4)     THIS SLOT TO NEXT                       
         BNE   PGCND3A                                                          
         LR    R6,R4               GET NUMBER OF SPOTS                          
         MVC   HLDNOSP,PGDNOSP                                                  
         LR    R6,RE                    POINT TO ADD SLOT                       
         ZIC   RF,PGDNOSP          BUMP BY NUMBER OF SPOTS                      
         ZIC   R0,HLDNOSP                                                       
         AR    RF,R0                                                            
         STC   RF,PGDSNO                                                        
         STC   RF,PGDNOSP                                                       
         LR    R6,R4                                                            
         MVI   PGSORT,X'FF'             ELIMINATE NEXT ELEMENT                  
         B     PGCND3                                                           
PGCND3A  LR    RE,R4                                                            
         B     PGCND3                                                           
PGCND4   L     R6,VPGRID                                                        
         L     R8,PGNOENT                                                       
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         L     R6,VPGRID                DELETE DUPS                             
         L     R1,PGNOENT                                                       
PGCND5   CLI   0(R6),0                                                          
         BE    PGCNDX                                                           
         CLI   0(R6),X'FF'                                                      
         BNE   PGCND6                                                           
         XC    0(PGDLEN,R6),0(R6)                                               
         BCTR  R1,0                                                             
         ST    R1,PGNOENT                                                       
PGCND6   LA    R6,PGDLEN(R6)                                                    
         B     PGCND5                                                           
*                                                                               
PGCNDX   DS    0H                                                               
*              CHECK FOR VARIABLE FORMAT                                        
         CLI   VARFRMT,1                                                        
         BNE   VAR2                                                             
         LA    R5,1                INITIALIZE SLOT NUMBER                       
         L     R6,VPGRID                                                        
*                                                                               
VARF1    OC    PGSORT,PGSORT                                                    
         BZ    VARFX                                                            
         STC   R5,PGLSLOT          SET SLOT NUMBER                              
         CLC   PGDLEN(10,R6),PGSORT                                             
         BNE   VARF2                                                            
         LA    R6,PGDLEN(R6)                                                    
         B     VARF1                                                            
VARF2    LA    R6,PGDLEN(R6)                                                    
         LA    R5,1(R5)                                                         
         B     VARF1                                                            
*                                                                               
VAR2     CLI   VARFRMT,2           VARIABLE FORMAT WITH FIXED SLOTS             
         BNE   VARFX                                                            
         LA    R5,1                INITIALIZE SLOT NUMBER                       
         L     R6,VPGRID                                                        
*                                                                               
VAR2F1   OC    PGSORT,PGSORT                                                    
         BZ    VARFX                                                            
         STC   R5,PGLSLOT          SET SLOT NUMBER                              
         CLC   PGDLEN(2,R6),PGSORT SLOT BY DATE ONLY                            
         BNE   VAR2F2                                                           
         LA    R6,PGDLEN(R6)                                                    
         B     VAR2F1                                                           
VAR2F2   LA    R6,PGDLEN(R6)                                                    
         LA    R5,1(R5)                                                         
         B     VAR2F1                                                           
*                                                                               
VARFX    DS    0H                                                               
*              SORT INTO SLOT NUMBER ORDER                                      
         L     R6,VPGRID                                                        
         LR    RE,R6                                                            
         USING PGSRT2D,RE                                                       
         L     R1,PGNOENT                                                       
PGSLTSRT XC    PGSORT,PGSORT                                                    
         LR    RE,R6                                                            
         MVC   PGDS2SLT,PGLSLOT                                                 
         MVC   PGDS2WK,PGDWK                                                    
         MVC   PGDS2DY,PGDFDAY                                                  
         XI    PGDS2DY,X'FF'                                                    
         MVI   PGDS2SNO,0                                                       
         CLI   PGCNDSW,1                                                        
         BE    *+10                                                             
         MVC   PGDS2SNO,PGDSNO                                                  
         MVC   PGDS2BR,PGDSBRN                                                  
         MVC   PGDS2PIG,PGD2BRN                                                 
         MVC   PGDS2SL,PGDSSLN                                                  
         MVC   PGDS2IND,PGDIND                                                  
         LA    R6,PGDLEN(R6)                                                    
         BCT   R1,PGSLTSRT                                                      
         DROP  RE                                                               
         L     R8,PGNOENT                                                       
         L     R6,VPGRID                                                        
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
*                                                                               
*              TABLE SORTED INTO SLOT NUMBER ORDER                              
*               NOW ASSIGN LINE NUMBERS                                         
PGSLNO   L     R6,VPGRID                                                        
         LA    R5,1                                                             
         LH    RF,NOINGRID                                                      
         ST    RF,PGWMAX                                                        
PGSLNO1  MVC   WORK(L'PGSORT),PGSORT                                            
         STC   R5,PGSUBLI          SET SUB-LINE                                 
         ZIC   RF,PGLSLOT                                                       
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         D     RE,PGWMAX           GET LINE NUMBER                              
         STC   RF,PGLINNO                                                       
         STC   RE,PGLSLOT                                                       
         LA    R6,PGDLEN(R6)                                                    
         CLC   PGSORT(1),WORK                                                   
         BNE   PGSLNO2                                                          
         LA    R5,1(R5)                                                         
         B     PGSLNO1                                                          
         SPACE 2                                                                
PGSLNO2  LA    R5,1                                                             
         OC    PGSORT,PGSORT                                                    
         BNZ   PGSLNO1                                                          
         SPACE 2                                                                
*              SORT INTO LINE/SUBLINE/SLOT NUMBER ORDER                         
         L     R6,VPGRID                                                        
         L     R1,PGNOENT                                                       
PGSRTPR  XC    PGSORT,PGSORT                                                    
         MVC   PGSORT(3),PGLINNO                                                
         LA    R6,PGDLEN(R6)                                                    
         BCT   R1,PGSRTPR                                                       
         SPACE 2                                                                
         L     R8,PGNOENT                                                       
         L     R6,VPGRID                                                        
         GOTO1 XSORT,DMCB,(R6),(R8),PGDLEN,PGSRTLN,0                            
         EJECT                                                                  
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         MVC   PGPRLNO,PGLINNO                                                  
         XC    PRPGWMAX,PRPGWMAX                                                
         BAS   R9,SETMGMS          SET UP MG/MSSD SPACING                       
         MVC   GRIDSLN,BDSEC                                                    
         DROP  RE                                                               
PTSGRDA  MVC   GRIDST,DSTAGRID                                                  
         XC    PGWKCNT,PGWKCNT                                                  
         XC    PGWMAX,PGWMAX                                                    
         SPACE 2                                                                
PTSGRID1 L     R4,GRIDST           SET BEGINNING OF BLOCK                       
         ZIC   RF,PGLSLOT                                                       
         ST    RF,PGWKCNT                                                       
         ZIC   RF,GRIDLEN                                                       
         MH    RF,PGWKCNT+2         SET BEGINNING OF THIS SLOT                  
         AR    R4,RF                                                            
PTSGRID2 OC    PGDWK,PGDWK         END                                          
         BZ    PTSGRIDX                                                         
         CLC   PGLINNO(2),PGPRLNO  SAME LINE/SUBLINE                            
         BE    PTSGRID3                                                         
         MVC   PGPRLNO,PGLINNO                                                  
         BAS   R9,SETMGMS          SET UP MG/MSSD SPACING                       
         L     RE,PGWMAX            YES - SET NEXT GRID                         
         XC    PGWMAX,PGWMAX                                                    
         A     RE,PRPGWMAX                                                      
         ST    RE,PRPGWMAX                                                      
         OC    OPTRPT,OPTRPT                                                    
         BNZ   PTSGRD2                                                          
         CHI   RE,93               CAN ONLY HANDLE 100 LINES                    
         BNH   PTSGRD2A                                                         
PTSGRD2  DS    0H                                                               
         GOTO1 VMRGPL                                                           
         XC    PRPGWMAX,PRPGWMAX                                                
         L     RE,VPLAREA                                                       
         L     RF,=F'8000'                                                      
         XCEF                                                                   
         B     PTSGRDA                                                          
PTSGRD2A MH    RE,MGLENLIN         BUMP TO NEXT PRINT LINE                      
         A     RE,DSTAGRID                                                      
         ST    RE,GRIDST                                                        
         B     PTSGRID1                                                         
         EJECT                                                                  
* SET ALLOCATIONS IN PRINT LINE                                                 
PTSGRID3 L     R5,PGDELAD                                                       
         USING REGELEM,R5                                                       
         LTR   R5,R5               BUILD ELEMENT IF NOT PRESENT                 
         BZ    *+12                                                             
         CLI   0(R5),11                                                         
         BL    *+14                                                             
         OC    PGDELAD,PGDELAD     HAVE ELEMENT ADDRESS                         
         BNZ   PTSG3EOK             YES - PROCESS                               
         LA    R5,PGELEM            NO - BUILD DUMMY ELEMENT                    
         XC    PGELEM,PGELEM                                                    
         MVC   RDATE,PGDWK                                                      
         MVC   RPPRD,PGDSBRN                                                    
         MVC   RPCOST,PGD2COVR                                                  
         OC    RPCOST,RPCOST                                                    
         BZ    *+8                                                              
         OI    RSTATUS,X'20'       SET FOR COST OVERRIDE                        
         MVC   PGELEM(2),=X'0B0E'                                               
         CLI   PGD2BRN,0                                                        
         BE    PTSGPBOK                                                         
         MVC   PGELEM(2),=X'0B12'                                               
         MVC   RPPRD+4(1),PGD2BRN                                               
         MVC   RPTIME(1),PGDSSLN                                                
PTSGPBOK MVC   RPTIME+4(1),PGD2SLN                                              
         CLI   PGDIND,1                                                         
         BNE   *+8                                                              
         MVI   RSTATUS,X'40'                                                    
         CLI   PGDIND,4                                                         
         BNE   *+8                                                              
         MVI   RSTATUS,X'42'                                                    
         CLI   PGDIND,X'08'                                                     
         BNE   *+8                                                              
         MVI   RSTATUS,X'04'                                                    
         CLI   PGDSBRN,X'FF'                                                    
         BNE   *+8                                                              
         MVI   PGELEM+1,X'0A'                                                   
PTSG3EOK DS    0H                                                               
         CLI   PTSSPILL,C'Y'                                                    
         BNE   *+8                                                              
         NI    RSTATUS,B'11011111' SUPPRESS COST OVERRIDES IF SPILL             
         MVI   PGWNOL,0                                                         
         CLI   PGCNDSW,1           CONDENSE                                     
         BE    *+8                  YES                                         
         MVI   PGDSNO,1            NO - PGDSNO HAS SUBLINE-RESET                
         XC    DUB,DUB                                                          
         CLI   GRIDLEN,13                                                       
         BNE   *+14                                                             
         OC    PGDFNO,PGDFNO       FORCE SECOND DATE IF FILM                    
         BNZ   PTSG3DT                                                          
         CLI   SCNDDTSW,1          PRINT DATES ON SECOND LINE                   
         BE    *+12                 YES                                         
         CLI   PGSUBLI,1           FIRST SUBLINE - PRINT DATE                   
         BNE   PTSGRD3A            NO                                           
PTSG3DT  GOTO1 DATCON,DMCB,(2,RDATE),(4,DUB)                                    
PTSGRD3A CLI   GRIDLEN,6                                                        
         BE    P06GRID                                                          
         MVC   FILMNO,SPACES                                                    
         CLI   GRIDLEN,10                                                       
         BE    P10GRID                                                          
         CLI   GRIDLEN,13                                                       
         BE    P13GRID                                                          
         BAS   R9,LINEA                                                         
         CLI   DUB,0                                                            
         BE    PTSG3A1                                                          
         MVC   0(3,RE),DUB                                                      
         AH    RE,MGLENLIN                                                      
         MVC   0(2,RE),DUB+3                                                    
         BAS   R9,INCR                                                          
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
PTSG3A1  DS    0C                                                               
         L     R7,=A(DICSECT)                                                   
         USING DICSECT,R7                                                       
         TM    RSTATUS,X'40'            PRE-EMPT                                
         BZ    *+10                                                             
*---->   MVC   0(3,RE),=C'*P*'                                                  
         MVC   0(L'SP@P,RE),SP@P                                                
         TM    RSTATUS,X'02'            MAKEGOOD                                
         BZ    *+10                                                             
*---->   MVC   0(3,RE),=C'*M*'                                                  
         MVC   0(L'SP@M,RE),SP@M                                                
         CLI   MGMSDSW,1           ANY *M* OR *P* SPOTS ON LINE                 
         BNE   *+8                                                              
         BAS   R9,INCR              YES - BUMP LINE                             
         BAS   R9,LINEA                                                         
         TM    RSTATUS,X'04'            HIATUS                                  
         BZ    *+10                                                             
*---->   MVC   0(3,RE),=C'*H*'                                                  
         MVC   0(L'SP@H,RE),SP@H                                                
         DROP  R7                                                               
PTSGRD3B DS    0C                                                               
         SPACE 2                                                                
PTSGRID4 BAS   R9,LINEA                                                         
         CLI   RLEN,14                                                          
         BL    PTSGRD4C            UNALLOCATED                                  
         ZIC   RF,RPPRD                 GET PRODUCT CODE                        
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,RPTIME                                                      
         BZ    PTSGRID5                                                         
         TM    RSTATUS,X'04'                                                    
         BO    PTSGRID5                                                         
*                                                                               
         CLC   GRIDSLN,RPTIME                                                   
         BE    PTSGRID5                                                         
*                                                                               
         BAS   R9,PBEDIT           PRINT SLN EVEN IF IT'S A SPOD                
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
*                                                                               
         CLC   RTYPE,=C'RS'        NO SPOD TESTS FOR RS/RY                      
         BE    PTSGRD4A                                                         
         CLC   RTYPE,=C'RY'                                                     
         BE    PTSGRD4A                                                         
*                                                                               
         MVI   SPOD,C'N'                                                        
         L     R9,ADBUY                                                         
         USING BUYREC,R9                                                        
         CLI   RLEN,14             TEST ONE PRODUCT ALLOCATED                   
         BNE   PTSGRD4A            NO                                           
         TM    BDSTAT3,BDST3_SPODS TEST SPODS ACTIVE                            
         BZ    PTSGRD4A            NO                                           
         MVI   SPOD,C'Y'                                                        
         BAS   R9,INCR                                                          
         B     PTSGRID5                                                         
         DROP  R9                                                               
*                                                                               
PTSGRD4A BAS   R9,INCR             DO PIGGYBACK PRD                             
         BAS   R9,LINEA                                                         
         ZIC   RF,RPPRD+4                                                       
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         ZIC   RE,RPTIME+4                                                      
         BAS   R9,PBEDIT                                                        
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
PTSGRD4B BAS   R9,INCR                                                          
         B     PTSGRID5                                                         
         SPACE 2                                                                
PTSGRD4C TM    RSTATUS,X'04'                                                    
         BNZ   PTSGRID5                                                         
         L     RF,=A(DICSECT)                                                   
         USING DICSECT,RF                                                       
*---->   MVC   0(3,RE),=C'*U*'                                                  
         MVC   0(L'SP@U,RE),SP@U                                                
         DROP  RF                                                               
         B     PTSGRD4B                                                         
         SPACE 2                                                                
PTSGRID5 TM    RSTATUS,X'20'       COST OVERRIDES ACTIVE                        
         BZ    PTSGRID6                                                         
*                                                                               
         L     RE,ADAGY            DON'T PRINT NETWORK OVRDS NOW                
         CLI   AGYPROF-AGYHDR+7(RE),C'C'                                        
         BNE   *+12                                                             
         CLI   QMED,C'N'                                                        
         BE    PTSGRID6                                                         
*                                                                               
         CLC   RTYPE,=C'RS '                                                    
         BE    PTSGRID6                                                         
         CLC   RTYPE,=C'RY '                                                    
         BE    PTSGRID6                                                         
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),RPCOST                                                 
         L     R9,ADBUY                                                         
         USING BUYREC,R9                                                        
         TM    BDSTAT,X'80'                                                     
         BZ    *+8                                                              
         NI    FULL+1,B'00000011'                                               
         DROP  R9                                                               
         L     R9,FULL                                                          
         MVI   CURTAB+3,2                                                       
         CURED (R9),(7,DUB),CURTAB,DMCB=CDMCB                                   
         MVI   CURTAB+3,0                                                       
         LTR   R9,R9                                                            
         BNZ   *+10                                                             
         MVC   DUB+2(5),=C'$0.00'                                               
         BAS   R9,LINEA                                                         
         CLI   PGLSLOT,0           FIRST SLOT                                   
         BE    *+8                  DONT INCREMENT                              
         SHI   RE,3                                                             
         CLI   0(RE),0                                                          
         BNE   PTSGRD5A                                                         
         MVC   0(7,RE),DUB                                                      
         BAS   R9,INCR                                                          
         B     PTSGRID6                                                         
PTSGRD5A BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         CLI   PGLSLOT,0                                                        
         BE    *+8                                                              
         SHI   RE,3                                                             
         CLI   0(RE),0                                                          
         BNE   PTSGRD5A                                                         
         MVC   0(7,RE),DUB                                                      
         BAS   R9,INCR                                                          
PTSGRID6 CLI   PGDNOSP,1                                                        
         BE    PTSGRID7                                                         
         BAS   R9,LINEA                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,PGDNOSP          GET NUMBER OF SPOTS                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(RE),C'('                                                       
*                                                                               
         UNPK  1(1,RE),DUB                                                      
         MVI   2(RE),C')'                                                       
*                                                                               
         CHI   R0,10               TEST NEED 2 CHARS                            
         BL    *+14                                                             
         UNPK  1(2,RE),DUB                                                      
         MVI   3(RE),C')'                                                       
*                                                                               
         BAS   R9,INCR                                                          
*                                                                               
PTSGRID7 BAS   R9,LINEA                                                         
         CLI   0(RE),C' '          ANY DATA ON NEXT PRINT LINE                  
         BNE   *+8                                                              
         MVI   0(RE),0                                                          
         BAS   R9,INCR                                                          
*                                                                               
         CLI   D5AXLIN,0           EXTRA LINES FOR NOTES                        
         BE    PTSGRID9                                                         
         ZIC   R1,D5AXLIN                                                       
PTSGRID8 BAS   R9,LINEA            SET TO LINE                                  
         CLI   0(RE),C' '          PROTECT WHAT'S THERE                         
         BNE   *+8                                                              
         MVI   0(RE),0             FORCE EXTRA LINE                             
         BAS   R9,INCR                                                          
         BCT   R1,PTSGRID8                                                      
*                                                                               
PTSGRID9 C     RE,PGWMAX           MAX THIS BLOCK GT. THAN PREV BLOCK           
         BNH   *+8                                                              
         ST    RE,PGWMAX                                                        
         LA    R6,PGDLEN(R6)                                                    
         B     PTSGRID1                                                         
         SPACE 2                                                                
PTSGRIDX GOTO1 VMRGPL                                                           
         MVC   SVP1,SPACES                                                      
PTSEXIT  XIT1                                                                   
         EJECT                                                                  
P06GRID  BAS   R9,LINEA                                                         
         CLI   DUB,0               DATE PRESENT                                 
         BE    P06GRID1             NO                                          
         MVC   0(5,RE),DUB                                                      
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
P06GRID1 DS    0C                                                               
         L     R7,=A(DICSECT)                                                   
         USING DICSECT,R7                                                       
         TM    RSTATUS,X'40'                                                    
         BZ    *+10                                                             
*---->   MVC   0(3,RE),=C'*P*'                                                  
         MVC   0(L'SP@P,RE),SP@P                                                
         TM    RSTATUS,X'02'                                                    
         BZ    *+10                                                             
*---->   MVC   0(3,RE),=C'*M*'                                                  
         MVC   0(L'SP@M,RE),SP@M                                                
         CLI   MGMSDSW,1           ANY *M* OR *P* SPOTS ON LINE                 
         BNE   *+8                                                              
         BAS   R9,INCR              YES - BUMP LINE                             
         BAS   R9,LINEA                                                         
         L     R7,=A(DICSECT)                                                   
*---->   MVC   0(3,RE),=C'*U*'                                                  
         MVC   0(L'SP@U,RE),SP@U                                                
         TM    RSTATUS,X'04'                                                    
         BZ    P06GR1                                                           
*---->   MVC   0(3,RE),=C'*H*'                                                  
         MVC   0(L'SP@H,RE),SP@H                                                
         DROP  R7                                                               
         BAS   R9,INCR             BUMP LINE IF STATUS BIT ON                   
         BAS   R9,LINEA                                                         
         MVI   0(RE),0                                                          
P06GR1   DS    0C                                                               
         CLI   RLEN,X'0E'                                                       
         BL    P06GRD1A                                                         
         CLI   RPPRD,0                                                          
         BE    P06GRD1A                                                         
         ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         CLI   PGD2BRN,0                                                        
         BE    P06GRD1A                                                         
         BAS   R9,INCR                                                          
         ZIC   RE,RPTIME                                                        
         BAS   R9,PBEDIT                                                        
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         ZIC   RF,RPPRD+4                                                       
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         ZIC   RE,RPTIME+4                                                      
         BAS   R9,PBEDIT                                                        
         BAS   R9,LINEA                                                         
         MVC   0(3,RE),DUB                                                      
P06GRD1A DS    0C                                                               
         CLI   PGDNOSP,1                                                        
         BE    P06GRID3                                                         
         LA    R9,3(RE)            PRINT NUMBER OF SPOTS                        
         CLI   2(RE),C' '                                                       
         BNE   *+8                                                              
         LA    R9,2(RE)                                                         
         MVI   0(R9),C'-'                                                       
         LA    R9,1(R9)                                                         
         MVI   CURTAB+3,0                                                       
         CURED PGDNOSP,(2,(R9)),CURTAB,DMCB=CDMCB,ALIGN=LEFT                    
P06GRID3 DS    0H                                                               
         BAS   R9,INCR                                                          
         TM    RSTATUS,X'20'       COST OVERRIDES ACTIVE                        
         BZ    P06GRID4                                                         
         CLC   RTYPE,=C'RS '                                                    
         BE    P06GRID4                                                         
         CLC   RTYPE,=C'RY '                                                    
         BE    P06GRID4                                                         
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),RPCOST                                                 
         L     R9,ADBUY                                                         
         USING BUYREC,R9                                                        
         TM    BDSTAT,X'80'                                                     
         BZ    *+8                                                              
         NI    FULL+1,B'00000011'                                               
         DROP  R9                                                               
         L     R9,FULL                                                          
         MVI   CURTAB+3,2                                                       
         CURED (R9),(7,DUB),CURTAB,DMCB=CDMCB                                   
         MVI   CURTAB+3,0                                                       
         LTR   R9,R9                                                            
         BNZ   *+10                                                             
         MVC   DUB+2(5),=C'$0.00'                                               
         BAS   R9,LINEA                                                         
         CLI   PGLSLOT,0           FIRST SLOT                                   
         BE    *+8                  DONT DECREMENT                              
         SH    RE,=H'2'                                                         
         CLI   0(RE),0                                                          
         BNE   P06GRD3A                                                         
         MVC   0(7,RE),DUB                                                      
         BAS   R9,INCR                                                          
         B     P06GRID4                                                         
P06GRD3A BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         CLI   PGLSLOT,0                                                        
         BE    *+8                                                              
         SH    RE,=H'2'                                                         
         MVC   0(7,RE),DUB                                                      
         BAS   R9,INCR                                                          
P06GRID4 DS    0H                                                               
         B     PTSGRID7                                                         
         SPACE 2                                                                
P10GRID  BAS   R9,LINEA                                                         
         OC    PGDFNO,PGDFNO       FILM PRESENT                                 
         BZ    P10GRID1                                                         
         ST    RE,FULL              YES - GET CANNISTER                         
         BAS   RE,GETFPROF                                                      
         L     RE,FULL                                                          
P10GRID1 DS    0H                                                               
         L     R7,=A(DICSECT)                                                   
         USING DICSECT,R7                                                       
         TM    RSTATUS,X'40'                                                    
         BZ    *+10                                                             
*---->   MVC   0(3,RE),=C'*P*'                                                  
         MVC   0(L'SP@P,RE),SP@P                                                
         TM    RSTATUS,X'02'                                                    
         BZ    *+10                                                             
*---->   MVC   0(3,RE),=C'*M*'                                                  
         MVC   0(L'SP@M,RE),SP@M                                                
         CLI   MGMSDSW,1           ANY *M* OR *P* SPOTS ON LINE                 
         BNE   *+8                                                              
         BAS   R9,INCR              YES - BUMP LINE                             
         BAS   R9,LINEA                                                         
         L     R7,=A(DICSECT)                                                   
*---->   MVC   6(3,RE),=C'*U*'                                                  
         MVC   6(L'SP@U,RE),SP@U                                                
         TM    RSTATUS,X'04'                                                    
         BZ    P10GR1                                                           
*---->   MVC   6(3,RE),=C'*H*'                                                  
         MVC   6(L'SP@H,RE),SP@H                                                
         DROP  R7                                                               
         BAS   R9,INCR             BUMP LINE IF STATUS BIT ON                   
         BAS   R9,LINEA                                                         
         MVI   0(RE),0                                                          
P10GR1   DS    0C                                                               
         MVC   0(5,RE),DUB                                                      
         MVI   5(RE),C'-'                                                       
         ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   6(3,RE),1(RF)                                                    
         BAS   R9,INCR                                                          
         BAS   R9,LINEA                                                         
         OC    PGDFNO,PGDFNO                                                    
         BZ    P10GRID2                                                         
         MVC   0(8,RE),FILMNO    PRINT FILM NUMBER                              
         BAS   R9,LINEA                                                         
P10GRID2 DS    0H                                                               
         CLI   PGDNOSP,1                                                        
         BE    PTSGRID7                                                         
         BAS   R9,LINEA                                                         
         MVC   4(4,RE),=C'(  )'                                                 
         LA    R9,5(RE)                                                         
         EDIT  PGDNOSP,(2,(R9))                                                 
         BAS   R9,INCR                                                          
         CLC   RTYPE,=C'RS '                                                    
         BE    PTSGRID7                                                         
         CLC   RTYPE,=C'RY '                                                    
         BE    PTSGRID7                                                         
         TM    RSTATUS,X'20'       COST OVERRIDES                               
         BZ    PTSGRID7                                                         
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF-AGYHDR+7(RE),C'C'                                        
         BNE   *+12                                                             
         CLI   QMED,C'N'                                                        
         BE    PTSGRID7                                                         
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),RPCOST                                                 
         L     R9,ADBUY                                                         
         USING BUYREC,R9                                                        
         TM    BDSTAT,X'80'                                                     
         BZ    *+8                                                              
         NI    FULL+1,B'00000011'                                               
         DROP  R9                                                               
         L     R9,FULL                                                          
         MVI   CURTAB+3,2                                                       
         CURED (R9),(7,DUB),CURTAB,DMCB=CDMCB                                   
         MVI   CURTAB+3,0                                                       
         LTR   R9,R9                                                            
         BNZ   *+10                                                             
         MVC   DUB+2(5),=C'$0.00'                                               
         BAS   R9,LINEA                                                         
         MVC   0(7,RE),DUB                                                      
         BAS   R9,INCR                                                          
         B     PTSGRID7                                                         
         SPACE 2                                                                
P13GRID  BAS   R9,LINEA                                                         
         CLI   DUB,0                                                            
         BE    P13GRID1                                                         
         MVC   0(5,RE),DUB                                                      
         ZIC   R9,PGWNOL                                                        
         LA    R9,1(R9)                                                         
         STC   R9,PGWNOL                                                        
P13GRID1 DS    0C                                                               
         OC    PGDFNO,PGDFNO       FILM PRESENT                                 
         BZ    P13GRID2                                                         
         ST    RE,FULL              YES - GET CANNISTER                         
         BAS   RE,GETFPROF                                                      
         L     RE,FULL                                                          
P13GRID2 DS    0H                                                               
         CLI   PGDFDAY,0                                                        
         BE    P13GRID3                                                         
* GET FILM DAY                                                                  
         GOTO1 DATCON,DMCB,(2,RDATE),DUB                                        
         GOTO1 GETDAY,DMCB,DUB,WORK                                             
         NI    DMCB,X'0F'                                                       
         MVC   WORK(6),DUB                                                      
         ZIC   RE,DMCB                                                          
         ZIC   R1,PGDFDAY                                                       
         SLL   R1,25                                                            
         SR    R9,R9                                                            
P13GRD2A LTR   R1,R1                                                            
         BZ    P13GRD2B                                                         
         SLL   R1,1                                                             
         LA    R9,1(R9)                                                         
         B     P13GRD2A                                                         
P13GRD2B DS    0C                                                               
         SR    R9,RE                                                            
         GOTO1 ADDAY,DMCB,WORK,DUB,(R9)                                         
         L     R9,FULL                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(4,(R9))                                     
         GOTO1 CODAY,DMCB,PGDFDAY,DUB                                           
         L     RE,FULL                                                          
         MVI   5(RE),C'-'                                                       
         MVC   6(3,RE),DUB                                                      
P13GRID3 BAS   R9,LINEA                                                         
         ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   0(3,RE),1(RF)                                                    
         OC    PGDFNO,PGDFNO                                                    
         BZ    *+10                                                             
         MVC   0(8,RE),FILMNO      PRINT FILM NUMBER                            
         ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
         B     PTSGRID7                                                         
         EJECT                                                                  
GETFPROF NTR1                                                                   
         XC    FILMNO,FILMNO                                                    
         BRAS  RE,GETNFLM          TRY FOR NEW FORMAT FILM                      
         OC    FILMNO,FILMNO                                                    
         BNZ   GTFPROF0                                                         
         SPACE 2                                                                
* DID NOT FIND NEW FILM PROFILE TRY OLD                                         
         XC    KEY,KEY                                                          
         ZIC   RF,RPPRD                                                         
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   THISPRD,1(RF)                                                    
         CLI   THISPRD+2,C' '      2 CHAR PRODUCT                               
         BE    GFEXIT               YES - BYPASS EQUIVALENCE                    
         LA    R8,KEY              3 CHAR PRODUCT                               
         USING TEKEY,R8             GET TRAFFIC EQUIVALENCE                     
         MVC   TEFCODE(2),=X'0A04'                                              
         MVC   TEAGY,BAGY                                                       
         MVC   TEMED,QMED                                                       
         MVC   TECLT,BCLT                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GTFPERR                                                          
         LA    RE,COMAREA                                                       
         ST    RE,AREC                                                          
         GOTO1 GET                                                              
         L     R8,AREC                                                          
         LA    R1,TEQELEM                                                       
         USING TEQELEM,R1                                                       
GFE1     CLI   0(R1),X'30'                                                      
         BE    GFE2                                                             
         CLI   0(R1),0                                                          
         BE    GTFPERR                                                          
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GFE1                                                             
GFE2     ZIC   RF,1(R1)                                                         
         AR    RF,R1               SET ELEM END                                 
GFE3     CLC   TEQACTP,THISPRD                                                  
         BE    GFE4                                                             
         LA    R1,7(R1)                                                         
         CR    R1,RF                                                            
         BL    GFE3                                                             
         B     GTFPERR                                                          
GFE4     MVC   THISPRD,TEQISCP                                                  
         DROP  R8                                                               
         DROP  R1                                                               
GFEXIT   DS    0H'0'                                                            
         MVC   KEY(2),=X'0A01'                                                  
         MVC   KEY+2(1),BAGY                                                    
         MVC   KEY+3(1),QMED                                                    
         MVC   KEY+4(2),BCLT                                                    
         MVC   KEY+6(2),THISPRD                                                 
         MVC   KEY+8(2),PGDFNO                                                  
         MVC   KEY+10(1),PGDSSLN                                                
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   GTFPERR             NOT FOUND - DO ERROR                         
         MVC   FILMNO(2),QCLT                                                   
         MVC   FILMNO+2(2),KEY+6                                                
         MVC   HALF,PGDFNO                                                      
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FILMNO+4(4),DUB+5(3)                                             
         LA    RE,COMAREA                                                       
         ST    RE,AREC                                                          
         GOTO1 GET                                                              
         L     RE,AREC                                                          
         MVC   AREC,ADBUY                                                       
         LA    RE,24(RE)                                                        
         CLC   25(8,RE),=C'        '                                            
         BE    *+10                                                             
         MVC   FILMNO(8),25(RE)                                                 
GTFPROF0 L     RE,VFLMPRD                                                       
GTFPROF1 CLI   0(RE),0             SAVE ISCI PRODUCT                            
         BE    GTFPROF2                                                         
         CLC   FILMNO+2(2),0(RE)                                                
         BE    GTFPROF2                                                         
         LA    RE,2(RE)                                                         
         B     GTFPROF1                                                         
         SPACE 2                                                                
GTFPERR  MVC   FILMNO(2),QCLT      INVALID FILM                                 
         MVC   FILMNO+2(2),THISPRD                                              
         MVC   FILMNO+4(4),=C'....'                                             
         ZIC   RE,INVFILM                                                       
         LA    RE,1(RE)                                                         
         STC   RE,INVFILM                                                       
         B     GTFPROFX                                                         
         SPACE 2                                                                
GTFPROF2 MVC   0(2,RE),FILMNO+2                                                 
GTFPROFX XIT1                                                                   
         EJECT                                                                  
LINEA    LR    RE,R4                                                            
         ZIC   RF,PGWNOL                                                        
         MH    RF,MGLENLIN                                                      
         AR    RE,RF                                                            
         BR    R9                                                               
INCR     ZIC   RE,PGWNOL                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PGWNOL                                                        
         BR    R9                                                               
PBEDIT   MVI   CURTAB+3,0                                                       
         CURED (RE),(3,DUB),CURTAB,DMCB=CDMCB,ALIGN=LEFT                        
         BR    R9                                                               
SETMGMS  LR    RF,R6               CHECK FOR ANY                                
         MVI   MGMSDSW,0            MAKEGOOD OR MISSED SPOTS IN                 
SETMGMS1 CLC   PGLINNO(2),PGPRLNO   CURRENT BLOCK                               
         BNE   SETMGMSX                                                         
         CLI   PGDIND,1            MISSED                                       
         BNE   *+8                                                              
         MVI   MGMSDSW,1                                                        
         CLI   PGDIND,4            MAKEGOOD ON NEW LINE                         
         BNE   *+8                                                              
         MVI   MGMSDSW,1                                                        
         LA    R6,PGDLEN(R6)                                                    
         B     SETMGMS1                                                         
SETMGMSX LR    R6,RF                                                            
         BR    R9                                                               
         LTORG                                                                  
PGELEM   DS    CL30                                                             
         EJECT                                                                  
GETNFLM  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             THIS CODE COPIED FROM BUY PROGRAM            
         MVC   KEY(2),=X'0AA1'     BUILD THE KEY                                
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+6(2),PGDFNO                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     EXIT IF NOT FOUND                            
         BNE   GETNFLMX                                                         
         LA    RE,COMAREA                                                       
         ST    RE,AREC                                                          
         GOTO1 GET                 GET THE RECORD                               
         MVC   FILMNO,COMAREA+5    MOVE OUT FILM NUMBER                         
         LA    R0,COMAREA                                                       
         LHI   R1,400                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
GETNFLMX XIT1                                                                   
         EJECT                                                                  
MRGPL    NTR1  BASE=*,LABEL=*                                                   
         MVC   DSTAGRID,SVSGRID    RESTORE GRID ADDRESS                         
         MVC   SVSGRID,VPLAREA                                                  
*                                                                               
MRGPL1   ZIC   RE,MAXLINES                                                      
         MVI   ALLOWLIN,0                                                       
         ZIC   RF,NUMCOM           ADJUST FOR COMMENT LINES                     
         SR    RE,RF                                                            
         ZIC   RF,LINE                                                          
         L     R7,PRPGWMAX                                                      
         A     R7,PGWMAX           ADD IN FINAL BLOCK                           
         ZIC   R0,NUMCOM           ADD IN COMMENT LINES                         
         AR    R0,R7                                                            
         ST    R0,PRPGWMAX                                                      
         CLC   QPROG,=C'U5'                                                     
         BNE   MRGPL1A                                                          
         AH    R7,=H'4'                                                         
         CLI   NUMCOM,0                                                         
         BE    MRGPL1A                                                          
         AH    R0,=H'4'            ADD IN SPACE LINES                           
         ST    R0,PRPGWMAX                                                      
MRGPL1A  SR    RE,RF               NUMBER OF LINES LEFT ON PAGE                 
         C     RE,PRPGWMAX         WILL ENTIRE LINE FIT                         
         BH    MRGPL2                                                           
MRGPL1B  MVI   FORCEHED,C'Y'        NO - FORCE TO NEW PAGE                      
MRGPL2   LA    R5,14               NUMBER OF PRINT LINES AVAILABLE              
         L     RE,DSTAGRID                                                      
         L     RF,SVSGRID                                                       
MRGPL3   LH    R6,MGLENLIN         SET LENGTH OF LINE                           
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         BCT   R7,*+8              END OF PRINT BUFFER                          
         B     MRGPL4                                                           
         LA    RE,132(RE)                                                       
         LA    RF,1(R6,RF)                                                      
         ST    RF,SVSGRID                                                       
         BCT   R5,MRGPL3                                                        
         BAS   R9,MRGPLR                                                        
         B     MRGPL2                                                           
MRGPL4   BAS   R9,MRGPLR                                                        
         CLC   RTYPE,=C'RS '                                                    
         BE    *+14                                                             
         CLC   RTYPE,=C'RY '                                                    
         BNE   MRGPL4A                                                          
         CLI   RSCOMCTL,C'Y'                                                    
         BNE   MRGPL6                                                           
MRGPL4A  CLI   PKGAREA,0                                                        
         BE    MRGPL5                                                           
         MVC   P1+31(16),PKGAREA                                                
         BAS   R9,MRGPLR                                                        
MRGPL5   DS    0H                                                               
         LA    R1,P1+4                                                          
         CLC   P1,SPACES                                                        
         BNE   *+8                                                              
         LA    R1,P1+33                                                         
         ST    R1,FULL                                                          
         GOTO1 VCOMPRNT                                                         
         CLC   P1,SPACES                                                        
         BE    MRGPL6                                                           
         BAS   R9,MRGPLR                                                        
MRGPL6   DS    0C                                                               
         OC    OPTRPT,OPTRPT       OPTIONAL REPORTS PRINT BLOCK HERE            
         BNZ   MRGPLX                                                           
         MVC   SVP1(80),SPACES                                                  
         MVC   SVP3(80),SPACES                                                  
         MVC   SVP5,SPACES                                                      
         MVI   P1,0                                                             
         BAS   R9,MRGPLR                                                        
MRGPLX   MVC   SVSGRID,DSTAGRID                                                 
         MVC   DSTAGRID,VPLAREA                                                 
         XIT1                                                                   
         SPACE 2                                                                
MRGPLR   OC    OPTRPT2,OPTRPT2                                                  
         BZ    MRGPLR2                                                          
         GOTO1 OPTRPT2,DMCB,(RA)                                                
MRGPLR2  GOTO1 REPORT                                                           
         BR    R9                                                               
         LTORG                                                                  
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
SORTC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SORTPASS,1          INPUT PHASE                                  
         BNE   SORTOUT              NO - DO OUTPUT                              
         OC    SSCNTR,SSCNTR                                                    
         BNZ   SORTIN1                                                          
         L     RE,VSSTABLE                                                      
         LHI   RF,SSTABLEX-SSTABLE                                              
         XCEF                                                                   
         L     RE,VPNTABLE                                                      
         L     RF,=F'2600'                                                      
         XCEF                                                                   
SORTIN1  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING PNAMD,RE                                                         
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   HALF,BDTIMST        CALCULATE START/END QTR HR                   
         LH    RF,HALF                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         MH    RF,=H'4'                                                         
         LR    R0,RF                                                            
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         AR    R0,RF                                                            
         STC   R0,FULL                                                          
         MVC   HALF,BDTIMEND                                                    
         LH    RF,HALF                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         MH    RF,=H'4'                                                         
         LR    R0,RF                                                            
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         AR    R0,RF                                                            
         STC   R0,FULL+1                                                        
         LA    RE,WORK                                                          
         MVC   WORK(1),PDNCNTR+3                                                
         MVC   PNDNAME,BDPROG+4                                                 
         MVC   PNDNAME+15(1),BDSEC                                              
         XC    PNDNAME+16(3),PNDNAME+16                                         
         CLI   RSCOST,C'Y'         NOT NEEDED IF COST IS SUPRESSED              
         BNE   *+10                                                             
         MVC   PNDNAME+16(3),BDCOST                                             
         L     R9,PDNCNTR                                                       
         L     R8,VPNTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,WORK),(R8),(R9),21,(1,20),255                    
         OC    DMCB,DMCB                                                        
         BNZ   *+6                                                              
         DC    H'0'                PROGRAM NAME TABLE IS FULL                   
         MVC   PDNCNTR,DMCB+8                                                   
         L     R1,DMCB                                                          
         MVC   CURRPNUM,0(R1)                                                   
         XC    WORK,WORK                                                        
         DROP  RE                                                               
         LA    RE,WORK                                                          
         USING SQSTART,RE                                                       
         MVC   SORTKLEN,=F'4'                                                   
         MVC   SORTRLEN,=F'8'                                                   
         MVC   DADRDISP,=F'4'                                                   
         CLI   SORTFRMT,1                                                       
         BNE   SORTIN2                                                          
         MVC   SQ1DAY,BDDAY                                                     
         XI    SQ1DAY,X'FF'                                                     
         MVC   SQ1TIME,FULL                                                     
         MVC   SQ1PNUM,CURRPNUM                                                 
         MVC   SQ1DADDR,KEY+14                                                  
         B     SORTADD                                                          
         SPACE 2                                                                
SORTIN2  CLI   SORTFRMT,2                                                       
         BNE   SORTIN3                                                          
         MVC   SQ2DAY,BDDAY                                                     
         XI    SQ2DAY,X'FF'                                                     
         MVC   SQ2TIME,FULL                                                     
         MVC   SQ2PNUM,CURRPNUM                                                 
         MVC   SQ2DADDR,KEY+14                                                  
         B     SORTADD                                                          
SORTIN3  B     SORTCX                                                           
         SPACE 2                                                                
* ADD A RECORD TO THE SORT BUFFER                                               
SORTADD  L     RF,SSCNTR                                                        
         SR    RE,RE                                                            
         M     RE,SORTRLEN                                                      
         A     RF,VSSTABLE                                                      
         MVC   0(20,RF),WORK                                                    
         L     RF,SSCNTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SSCNTR                                                        
         MVI   SOUTFRST,1                                                       
         B     SORTCX                                                           
         EJECT                                                                  
SORTOUT  CLI   SOUTFRST,1                                                       
         BNE   SRTOUT1                                                          
         L     R4,VSSTABLE                                                      
         L     R5,SSCNTR                                                        
         L     R6,SORTRLEN                                                      
         L     R7,SORTKLEN                                                      
         GOTO1 XSORT,DMCB,(R4),(R5),(R6),(R7),0                                 
         MVI   SOUTFRST,0                                                       
         MVC   NEXTSSLT,VSSTABLE                                                
         L     RE,ADBUY                                                         
         ST    RE,AREC                                                          
         SPACE 2                                                                
SRTOUT1  L     RE,NEXTSSLT                                                      
         A     RE,DADRDISP                                                      
         MVC   KEY+14(4),0(RE)                                                  
         OC    KEY+14(4),KEY+14                                                 
         BNZ   SRTOUT2                                                          
         MVI   SORTPASS,3                                                       
         MVI   SOUTFRST,1                                                       
         XC    SSCNTR,SSCNTR                                                    
         B     SORTCX                                                           
SRTOUT2  L     RE,ADBUY            GET A BUY RECORD                             
         ST    RE,AREC                                                          
         GOTO1 GET                                                              
         L     RE,ADBUY                                                         
         MVC   KEY(13),0(RE)                                                    
         L     RE,NEXTSSLT                                                      
         XC    CURRSORT,CURRSORT   SET CURRENT KEY AND NEXT KEY                 
         XC    NEXTSORT,NEXTSORT                                                
         L     R9,SORTKLEN                                                      
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   CURRSORT(0),0(RE)                                                
         A     RE,SORTRLEN                                                      
         ST    RE,NEXTSSLT                                                      
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   NEXTSORT(0),0(RE)                                                
         MVI   SORTPASS,2                                                       
SORTCX   XIT1                                                                   
         LTORG                                                                  
SORTKLEN DC    F'0'                SORT KEY LENGTH                              
SORTRLEN DC    F'0'                SORT RECORD LENGTH                           
DADRDISP DC    F'0'                DISK ADDRESS DISPLACEMENT                    
NEXTSSLT DC    F'0'                NEXT SORT SLOT                               
SOUTFRST DC    X'00'               FIRST TIME SWITCH                            
         EJECT                                                                  
PTSGD    DSECT                                                                  
GRIDST   DS    F                   START OF GRID                                
PRPGWMAX DS    F                                                                
PRPGDWK  DS    CL2                 PREVIOUS BLOCK DATES                         
PGPRLNO  DS    CL3                 PREVIOUS LINE/SUN LINE/SLOT                  
PGWNOL   DS    C                   NUMBER OF LINES IN THIS BLOCK                
GRIDLEN  DS    C                                                                
GRIDSW1  DS    C                                                                
GRIDSLN  DS    C                                                                
PGWKCNT  DS    F                   WEEKLY SLOT COUNTER                          
         EJECT                                                                  
SPD502   CSECT                                                                  
*                                                                               
KRFTRPT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ALLOWLIN,0                                                       
         CLI   FORCEHED,C'Y'                                                    
         BE    KRFTXIT                                                          
         CLI   OPTRMODE,PROCBUY                                                 
         BE    KRFT0                                                            
         CLI   OPTRMODE,STALAST                                                 
         BNE   KRFTXIT                                                          
         L     R4,VFLMPRD                                                       
         SR    R7,R7                                                            
KRFTSRT  CLI   0(R4),0                                                          
         BE    KRFTSRT1                                                         
         LA    R4,2(R4)                                                         
         LA    R7,1(R7)                                                         
         B     KRFTSRT                                                          
KRFTSRT1 LTR   R7,R7                                                            
         BZ    KRFTSRTX                                                         
         L     R4,VFLMPRD                                                       
         GOTO1 XSORT,DMCB,(R4),(R7),2,2,0                                       
KRFTSRTX DS    0C                                                               
         ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         LR    R4,RE                                                            
         CH    R4,=H'14'                                                        
         BH    KRFT3A                                                           
         B     KRFT1                                                            
KRFT0    ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         CH    RE,=H'15'                                                        
         BH    KRFTXIT                                                          
         LR    R4,RE                                                            
         LTR   RE,RE                                                            
         BM    KRFTXIT                                                          
KRFT1    CH    R4,=H'13'                                                        
         BNH   KRFT2                                                            
         XC    P,P                                                              
         MVC   P2,SPACES                                                        
         GOTO1 REPORT                                                           
         BCTR  R4,0                                                             
         B     KRFT1                                                            
* IF RCLANG = 'E' THEN R4 <-- KRFTCOM ELSE <-- KRFTCOMF                         
KRFT2    LA    R4,KRFTCOM                                                       
         LA    R5,P1                                                            
         LA    R6,13                                                            
KRFT3    MVC   0(110,R5),0(R4)                                                  
         LA    R5,132(R5)                                                       
         LA    R4,110(R4)                                                       
         BCT   R6,KRFT3                                                         
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         L     R4,VFLMPRD          IF NO FILMS IN TABLE EXIT                    
         CLI   0(R4),0             ELSE PRINT PRODUCTS AND FILMS                
         BE    KRFTXIT                                                          
         CLI   OPTRMODE,STALAST                                                 
         BNE   KRFTXIT                                                          
KRFT3A   L     R4,VFLMPRD                                                       
         CLI   0(R4),0                                                          
         BE    KRFT8                                                            
         XC    SVP1,SVP1                                                        
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   SVP1(24),=C'*****PRODUCT LEGEND*****'                            
         MVC   SVP1(L'SP@PROLE),SP@PROLE                                        
         DROP  RE                                                               
         MVC   P(24),SVP1                                                       
         GOTO1 REPORT                                                           
KRFT4    MVC   DUB(8),0(R4)                                                     
         LA    R5,P                                                             
         LA    R6,4                                                             
         LA    R7,DUB                                                           
KRFT5    MVC   0(2,R5),0(R7)                                                    
         MVI   2(R5),C'-'                                                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PKEY,R3                                                          
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
*---->   MVC   3(7,R5),=C'UNKNOWN'                                              
         MVC   3(L'SP@UNKN,R5),SP@UNKN                                          
         DROP  RE                                                               
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,0(R7)                                                    
         MVI   PKEYPRD+2,C' '                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   KRFT6                                                            
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         L     R3,AREC                                                          
         MVC   3(20,R5),PNAME                                                   
KRFT6    LA    R7,2(R7)                                                         
         CLI   0(R7),0                                                          
         BE    KRFT7                                                            
         LA    R5,24(R5)                                                        
         BCT   R6,KRFT5                                                         
KRFT7    GOTO1 REPORT                                                           
         LA    R4,8(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   KRFT4                                                            
         L     RE,VFLMPRD                                                       
         LA    RF,450                                                           
         XCEF                                                                   
KRFT8    ZIC   RE,MAXLINES                                                      
         ZIC   RF,LINE                                                          
         SR    RE,RF                                                            
         LR    R4,RE                                                            
         MVI   OPTRMODE,0                                                       
         B     KRFT1                                                            
         SPACE 2                                                                
KRFTXIT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
KRFTCOM  DS    0C                                                               
         DC    CL110'***TRAFFIC DESK ***'                                       
         DC    CL110'IMPORTANT - M-F/M-SU ROTATORS DO NOT HAVE TO RUN OX        
               N EXACT DATES LISTED ABOVE, BUT MUST RUN'                        
         DC    CL110'            WITHIN THE ROTATOR DAYS FOR THAT BROADX        
               CAST WEEK OR PAYMENT WILL NOT BE MADE.'                          
         DC    CL110'WARNING   - THESE SPOTS ARE NOT TO BE POSITIONED IX        
               N OR ADJACENT TO ANY PROGRAM OR MOVIE WHICH '                    
         DC    CL110'            FEATURES EXCESSIVE VIOLENCE OR HAS NEGX        
               ATIVE MORAL OVERTONES.'                                          
         DC    CL110'            WHEN IN DOUBT,CALL US.'                        
         DC    CL110'HOLD ALL COMMERCIALS FOR 90 DAYS. CONSULT US BEFOR'        
               E DESTROYING COMMERCIALS.'                                       
         DC    X'00',CL109' '                                                   
         DC    CL110'*** BILLING DESK *** INVOICES MUST SHOW DATES, EXA'        
               CT TIMES AND PRODUCT FOR EACH SPOT OR THEY WILL'                 
         DC    CL110'                     NOT BE PAID'                          
         DC    CL110'QUESTIONS CALL MICHAEL LUONGO, 212-210-4160'               
         DC    CL110'               LISA RIVERA,    212-210-5492'               
         DC    CL110' '                                                         
*                                                                               
KRFTCOMF DS    0C                                                               
         DC    CL110'***TRAFFIC DESK ***'                                       
         DC    CL110'IMPORTANT - ROTATIONS DE L-V/L-D DOIVEUT ETRE ASSUX        
               RE DANS LES JOURS D''UNE SEMAINE DE BROADCAST'                   
         DC    CL110' MAIS PAS NECESSAIREAUET DANS LES DATES EXACHES  OX        
               U PAYMENT NE SERA PAS MADE.'                                     
         DC    CL110'WARNING - CES ANNONCES PUBLICITAIRES NE DOIVEUT PAX        
               S ETRE XXXX DANS, OU CONCURRENT'                                 
         DC    CL110'A VU PROGRAMME OU FILM QUI CONTIEUT DE LA VIOLENCEX        
               EXCESSIVE OU DES TENDANCES MORALES NEGATIVES.'                   
         DC    CL110'      SI IL Y A DOUBTE, APPLEZ NOUS.'                      
         DC    CL110'RETEVEZ LES ANNONCES PENDANTS 90 JOURS. CONSULTEZ X        
               NOUS AVANT DE DETROINE TOUTES ANNOUNCES'                         
         DC    X'00',CL109' '                                                   
         DC    CL110'** BUREAU DE DEVIS ** DEVIS DOIVEUT INDIQUER LA DAX        
               TE, HEURE EXACTE ET PRODUIT PEUR CLIQUES ANNONCES'               
         DC    CL110' OU IL NE SERA PAS PAYE'                                   
         DC    CL110'POUR QUESTIONS APPELEZ MICHAEL LUONGO 12-210-4160'         
         DC    CL110'                       LISA RIVERA,   12-210-5492'         
         DC    CL110' '                                                         
*                                                                               
         EJECT                                                                  
DFSRPT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   OPTRPT2,OPTRPT      SET TO DO BEFORE EACH PL                     
         MVI   ALLOWLIN,3                                                       
         MVI   MAXLINES,57                                                      
* IF RCLANG = 'E' THEN MOVE DFSCOM ELSE MOVE DFSCOMF                            
         MVC   FOOT1(110),DFSCOM                                                
DFSXIT   XIT1                                                                   
         SPACE 2                                                                
DFSCOM   DC    CL110'***NOTE*** BRANDS RUNNING MORE THAN ONE PER WEEK, X        
               M-F, SHOULD BE SCHEDULED ON SEPARATE DAYS'                       
DFSCOMF  DC    CL110'***NOTE*** TOUS PRODUIT APPARAISSAUT PLUS D''VUE FX        
               OIS PAS SEMAINE, L-V, DOIVEUT ETRES, '                           
         DC    CL110'REPARTIS SUR DES JOURS DIFFERENTS.'                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
SIGNRPT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,=A(DICSECT)                                                   
         USING DICSECT,R5                                                       
*                                                                               
         CLC   QPROG,=C'D5'        ONLY DO FOR D5 REPORT                        
         BNE   SIGNRPTX                                                         
         CLI   D5ASIG,C'Y'                                                      
         BNE   SIGNRPTX                                                         
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         CLC   QAGY,=C'BP'         SPECIAL COMMENTS FOR BDAT                    
         BE    BPSPC                                                            
*---->   MVC   P3(110),SIGNCOM                                                  
         MVC   P3(L'SP@SIGN),SP@SIGN                                            
         MVC   P3+59(L'SP@DATE2),SP@DATE2                                       
         B     SIGNRPTP                                                         
         SPACE 1                                                                
BPSPC    DS    0H                                                               
         MVC   P3(110),BPSPCOM1                                                 
         MVI   P4,0                                                             
*---->   MVC   P5(110),BPSPCOM2                                                 
         MVC   P5(L'SP@ACC01),SP@ACC01                                          
         MVC   P5+59(L'SP@DATE2),SP@DATE2                                       
         MVI   P6,0                                                             
*---->   MVC   P7(110),BPSPCOM3                                                 
         MVC   P7(L'SP@ACC02),SP@ACC02                                          
         MVC   P7+59(L'SP@DATE2),SP@DATE2                                       
SIGNRPTP GOTO1 REPORT                                                           
         DROP  R5                                                               
SIGNRPTX XIT1                                                                   
         SPACE 2                                                                
SIGNCOM  DC    AL3(SP@SIGN)                                                     
SIGNCOM2 DC    AL3(SP@DATE2)                                                    
         SPACE 2                                                                
*      *****************SPECIAL BDAT COMMENTS***************                    
BPSPCOM1 DC    CL110'AAAA SPOT CONTRACT APPLIES AS WELL AS AMENDMENTS OX        
               N REVERSE SIDE           '                                       
BPSPCOM2 DC    AL3(SP@ACC01)                                                    
*        DC    AL3(SP@DATE2)                                                    
BPSPCOM3 DC    AL3(SP@ACC02)                                                    
*        DC    AL3(SP@DATE2)                                                    
         LTORG                                                                  
         EJECT                                                                  
SUBR01   NMOD1 0,**SR01**                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R1,0(R1)                                                         
         SRL   R1,24               GET THE ROUTINE NUMBER                       
         SLL   R1,2                AND BRANCH ADDRESS                           
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
         SPACE 1                                                                
PTSDESCE EQU   (PTSDESC#-*)/4+1                                                 
         SPACE 1                                                                
PTSDESC# B     PTSDESC0            PRINT POL TIMESHEET DESCRIPTION              
         SPACE 1                                                                
SR1XIT   XIT1                                                                   
CURTAB2  DC    X'00000000116C4040'                                              
         EJECT                                                                  
         DS    0H                                                               
PTSDESC0 L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         BAS   R9,PTSDESCG         MAKE THE RELOCATION EASY FOR ME              
         B     SR1XIT                                                           
         USING BDEXTD,R5                                                        
PTSDESCG MVC   P1(L'BDPEST),BDPEST      SET BUY DESCRIPTION                     
         MVC   P2(L'BDPLIN),BDPLIN                                              
         MVC   P1+4(L'BDPBDATE),BDPBDATE                                        
         MVC   P1+3(1),CFDS                                                     
         MVC   P1+15(1),CFDE                                                    
         MVC   P1+16(L'BDPWKS),BDPWKS                                           
         CLI   BDPWIND,C'O'                                                     
         BE    *+10                                                             
         MVC   P1+18(L'BDPWIND),BDPWIND                                         
         MVC   P1+22(L'BDPDAY),BDPDAY                                           
         MVC   P1+30(L'BDPNPWK),BDPNPWK                                         
         MVC   P2+4(L'BDPTIME),BDPTIME                                          
         MVC   P2+17(L'BDPDPT),BDPDPT                                           
         MVC   P2+21(L'BDPSLN),BDPSLN                                           
         MVC   P3+4(L'BDPPROG),BDPPROG                                          
         MVC   P3+22(L'BDPPTYP),BDPPTYP                                         
         OC    BDPADJ,BDPADJ                                                    
         BZ    *+10                                                             
         MVC   P3+22(L'BDPADJ),BDPADJ                                           
         L     RE,=A(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   MEDSPILL,C'Y'       OVERRIDE COST FOR SPILL                      
         BNE   *+14                                                             
*---->   MVC   P3+25(8),=C'*SPILL* '                                            
         MVC   P3+25(L'SP7SPILL),SP7SPILL                                       
         DROP  RE                                                               
         B     PTSNCST1                                                         
         SPACE 1                                                                
         CLI   DETOPTS+2,0         SUPPRESS COST                                
         BE    PTSNCST1                                                         
         MVC   P3+25(8),BDPCOST+4  PRINT COST AND TAX                           
         CLI   BDPCOST+3,C' '                                                   
         BE    *+10                                                             
         MVC   P3+25(8),BDPCOST+3                                               
         CLI   BDPCOST+2,C' '                                                   
         BE    *+10                                                             
         MVC   P3+25(8),BDPCOST+2                                               
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
*        OC    BDTAX,BDTAX          *********NO LONGER VALID******              
*        BZ    PTSNOCST                                                         
*        MVI   P4+26,C'T'                                                       
*        ZIC   RF,BDTAX                                                         
*        MH    RF,=H'5'                                                         
*        EDIT  (RF),(4,P4+27),2,ALIGN=LEFT   *********************              
         SPACE 1                                                                
PTSNOCST DS    0C                  CHECK FOR NEW STYLE TAX                      
         OC    BDNTAX,BDNTAX                                                    
         BZ    PTSNCST1                                                         
         MVI   P4+25,C'T'                                                       
         SR    RF,RF                                                            
         ICM   RF,3,BDNTAX                                                      
         MVI   CURTAB+3,3                                                       
         CURED (RF),(5,P4+26),CURTAB,DMCB=CDMCB,ALIGN=LEFT                      
         MVI   CURTAB+3,0                                                       
         DROP  RE                                                               
         SPACE 1                                                                
PTSNCST1 MVI   P4,0                                                             
         OC    SVSPREP,SVSPREP                                                  
         BZ    PTSDESC1                                                         
         L     RF,=A(DICSECT)                                                   
         USING DICSECT,RF                                                       
*---->   MVC   P4(21),SREPCAP                                                   
         MVC   P4(L'SP@SPREP),SP@SPREP                                          
         DROP  RF                                                               
         GOTO1 VRCPACK,DMCB,(C'U',SVSPREP),P4+15                                
PTSDESC1 DS    0H                                                               
         CLI   QPROG,C'U'                                                       
         BNE   *+10                                                             
         MVC   P2+3(1),BDPAST                                                   
*                                                                               
         L     RE,ADAGY            TEST CANADIAN AGENCY                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C'                                        
         BNE   PTSDESC4                                                         
         L     RE,ADBUY            YES-TEST EXCHANGE ELEMENT                    
         TM    BDCIND2-BUYREC(RE),X'40'                                         
         BZ    PTSDESC4                                                         
         SR    RF,RF               YES-LOCATE EXCHANGE ELEMENT                  
         LA    RE,24(RE)                                                        
*                                                                               
PTSDESC2 CLI   0(RE),0                                                          
         BE    PTSDESC4                                                         
         CLI   0(RE),XCHCODEQ                                                   
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     PTSDESC2                                                         
         USING XCHELEM,RE                                                       
         LA    R6,P4                                                            
         CLI   P4,0                                                             
         BE    *+8                                                              
         LA    R6,P5                                                            
         CLC   RQCRRNCY,XCHDTYP                                                 
         BE    PTSDESC3                                                         
         OC    XCHRATE,XCHRATE     TEST EXCHANGE RATE                           
         BZ    PTSDESC3                                                         
         L     R4,=A(DICSECT)                                                   
         USING DICSECT,R4                                                       
*---->   MVC   0(9,R6),=C'EXCHANGE='                                            
         MVC   0(L'SP@EXCHG,R6),SP@EXCHG                                        
         LA    R6,L'SP@EXCHG-1(R6)                                              
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C'='                                                       
         LA    R6,2(R6)                                                         
         MVI   CURTAB2+3,4                                                      
         LR    R4,RE                                                            
         CURED (2,XCHRATE),(7,(R6)),CURTAB2,DMCB=CDMCB,CURSYMB=%                
         LA    R6,7(R6)                                                         
         LR    RE,R4                                                            
         DROP  R4                                                               
*                                                                               
PTSDESC3 OC    XCHC58,XCHC58       TEST C58 TAX                                 
         BZ    PTSDESC4                                                         
         MVC   0(4,R6),=C'C58='                                                 
         LA    R4,4(R6)                                                         
         MVI   CURTAB2+3,2                                                      
         CURED (2,XCHC58),(6,(R4)),CURTAB2,DMCB=CDMCB,CURSYMB=%                 
         DROP  RE                                                               
*                                                                               
PTSDESC4 MVI   P6,0                                                             
         SPACE 2                                                                
* FIND DEMO ELEMENT AND EXTRACT BOOK AND NAME                                   
         L     RE,ADBUY                                                         
         LA    RE,24(RE)                                                        
         USING NDELEM,RE                                                        
PTSDSC2  CLI   NDCODE,2                                                         
         BE    PTSDSC2A                                                         
         ZIC   R0,NDLEN                                                         
         AR    RE,R0                                                            
         B     PTSDSC2                                                          
PTSDSC2A MVC   HLDPNAM,NDPROG                                                   
         CLI   NDBOOK,0                                                         
         BE    PTSDSC2B                                                         
         GOTO1 DATCON,DMCB,(X'03',NDBOOK),(X'09',HLDBOOK)                       
*          DATA SET SPREPD202  AT LEVEL 108 AS OF 08/06/92                      
*                                                                               
         CLI   MEDSPILL,C'Y'       SPILL IS SLIGHTLY DIFFERENT                  
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
BRSD2C   MVC   HLDBOOK+3(2),HLDBOOK+4  SLIDE YEAR                               
         MVC   HLDBOOK+5(1),2(RE)  MOVE SS BEHIND BOOK                          
         B     BRSD3                                                            
*                                                                               
BRSD2E   DS    0H                  THIS PART FOR SPILL                          
         L     RE,MEDADEMO                                                      
         L     RE,4(RE)                                                         
         MVC   HLDBOOK+3(2),HLDBOOK+4   SLIDE YEAR                              
         MVC   HLDBOOK+5(1),NDBOOK+6    MOVE SS BEHIND BOOK                     
BRSD3    DS    0H                                                               
PTSDSC2B DS    0H                                                               
         MVC   SVP1,P1                                                          
         MVC   SVP2,P2                                                          
         MVC   SVP3,P3                                                          
         MVC   SVP4,P4                                                          
         MVC   SVP5,P5                                                          
         DROP  RE                                                               
         SPACE 2                                                                
         BR    R9                                                               
         SPACE 2                                                                
         LTORG                                                                  
*          DATA SET SPREPD202  AT LEVEL 090 AS OF 11/04/13                      
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
         SR    R5,R5                                                            
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
         XC    MGA2CNT,MGA2CNT                                                  
         XC    MGATCNT,MGATCNT                                                  
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
         MVC   LTYPE+1(3),MGESTA       MOVE NETWORK                             
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
MGASX10  XIT1                                                                   
         EJECT                                                                  
EDTMGDEM L     RF,=A(MGABLK)                                                    
         USING MGABLKD,RF                                                       
         L     RF,MGADEM                                                        
         DROP  RF                                                               
         CLI   1(RF),C'R'                                                       
         BE    *+12                                                             
         CLI   1(RF),C'E'                                                       
         BNE   EDTMGDE1                                                         
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    EDTMGDE2                                                         
         B     EDTMGDE4                                                         
*                                                                               
EDTMGDE1 TM    RQOPTS,RQOPTS_2DECIMP                                            
         BZ    EDTMGDE2                                                         
         B     EDTMGDE4                                                         
*                                                                               
EDTMGDE2 EDIT  (R1),(5,(R5)),1                                                  
         BR    RE                                                               
*                                                                               
EDTMGDE4 EDIT  MGERTGB,(5,(R5)),2                                               
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
         TM    RQOPTS,RQOPTS_2DEC                                               
         BO    EDTMGT04                                                         
         B     EDTMGT06                                                         
*                                                                               
EDTMGT02 TM    RQOPTS,RQOPTS_2DECIMP                                            
         BZ    EDTMGT06                                                         
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
********************************************************************            
* THIS ROUTINE IS CALLED THE FIRST TIME THERE IS BUY ACTIVITY FOR  *            
* A STATION FOR THE RY REPORT, AND ALSO TO CLOSE A FAX REPORT      *            
********************************************************************            
         SPACE 1                                                                
RYFAX    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   QPROG,=C'RY'        CHECK IT'S THE RY REPORT                     
         BNE   RSFAXX                                                           
*                                                                               
         L     R4,VFAXINFO         ADDRESS FAXLINK INFO BLOCK                   
         USING FAXINFOD,R4                                                      
         TM    RSSW,RSOPEN         TEST BEEN HERE ONCE BEFORE                   
         BO    RSFAX00                                                          
         MVC   FAXSTA,STAPRINT                                                  
         B     RSFAX2                                                           
*                                                                               
RSFAX00  CLI   IDSW,1              MULTIPLE ID'S ON A PAGE                      
         BNE   RSFAX1              NO                                           
         CLC   FAXSTA,STAPRINT                                                  
         BE    RSFAXX                                                           
         MVC   FAXSTA,STAPRINT                                                  
*                                                                               
RSFAX1   MVI   FXISTAT,FXISCLOS    CALL FAXLINK TO CLOSE FOR LAST               
         GOTO1 VFAXLINK                STATION                                  
*                                                                               
RSFAX2   OI    RSSW,RSOPEN                                                      
         NI    RSSW,255-RSNEWSTA                                                
*                                                                               
         XC    FAXINFOD(FXILEN),FAXINFOD   SET UP FAXINFO BLOCK                 
         MVI   FXISTAT,FXISPEND    PENDING INITIALIZATION                       
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
         CLI   STA+3,C' '                                                       
         BH    *+8                                                              
         MVI   FXISTN+3,C'9'                                                    
         CLI   FXISTN,C'0'         TEST CABLE                                   
         BL    *+14                                                             
         MVI   FXISTN,C'T'         YES-MAKE IT TNNNN                            
         MVC   FXISTN+1(4),STA                                                  
         OI    FXIOPTS,FXIOEASY                                                 
         MVC   FXITRN(14),RSTRAN                                                
         LA    R1,FXITRAPP         APPLICATION TRANSACTION DATA                 
         BAS   RE,RSFMTAPP                                                      
         LA    R1,FXIEBILL         EASYLINK BILLING INFORMATION                 
         BAS   RE,RSFMTBIL                                                      
         GOTO1 VFAXLINK            CALL FAXLINK                                 
         CLI   FXISTAT,FXISACT     TEST FAX RECORD FOUND                        
         BE    RSFAXX              YES-DONE                                     
****     DC    H'0'                                                             
         MVI   FXISTAT,FXISACT     NO-MARK ACTIVE SO CLOSE WILL OCCUR           
*                                                                               
         LA    R1,P                AND WE'LL DO IT OURSELVES                    
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(5,R1),STA         EASYLINK'S STATION DESTINATION               
*                                                                               
         CLI   STA+4,C' '                                                       
         BH    *+8                                                              
         MVI   13(R1),C'T'                                                      
*                                                                               
         CLI   STA+3,C' '                                                       
         BH    *+8                                                              
         MVI   12(R1),C'9'                                                      
*                                                                               
         CLI   STA,C'0'            TEST CABLE                                   
         BL    RSFAX10                                                          
         MVI   9(R1),C'T'          YES-MAKE IT TNNNN                            
         MVC   10(4,R1),STA                                                     
         B     RSFAX12                                                          
*                                                                               
RSFAX10  CLI   STA+4,C'/'          TEST / FOR CANADIAN NETWORK                  
         BNE   RSFAX12                                                          
         MVI   13(R1),C'N'         THEN MAKE IT N                               
*                                                                               
RSFAX12  MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVI   35(R1),C'P'         PAGE BREAKS                                  
         MVC   38(L'BIGSTA,R1),BIGSTA   FORMATTED DESTINATION NAME              
         LA    R1,54(R1)                                                        
         BAS   RE,RSFMTBIL                                                      
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         GOTO1 REPORT                                                           
         MVC   P(14),RSTRAN                                                     
         LA    R1,P+15                                                          
         BAS   RE,RSFMTAPP                                                      
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         GOTO1 REPORT                                                           
         MVI   SKIPSPEC,C'N'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
RSFAXX   XIT1                                                                   
         SPACE 2                                                                
RSFMTBIL DS    0H                  FORMAT EASYLINK BILLING INFO                 
         MVC   0(1,R1),QMED                                                     
         MVC   1(3,R1),QCLT                                                     
         BR    RE                                                               
         SPACE 2                                                                
RSFMTAPP NTR1                      FORMAT APPLICATION PART OF ++DDS REC         
         USING SPEDICTD,R1                                                      
         MVI   SPCPTYPE,SPCPDATQ                                                
         MVC   SPCPMED,MED                                                      
         MVC   SPCPCLT,CLT                                                      
         MVC   SPCPPRD,PRD                                                      
         MVC   SPCPEST,EST                                                      
         MVC   SPCPMKT,MKT                                                      
         MVC   SPCPSTA,STA                                                      
         MVC   SPCPRQST,SVQUEST                                                 
         LA    R2,SPCDRUSR+15                                                   
         GOTO1 DATCON,DMCB,(2,MEDPERD),(7,0(R2))                                
         MVI   5(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,MEDPERD+2),(7,6(R2))                              
         XIT1                                                                   
         DROP  R1                                                               
         SPACE 2                                                                
RSTRAN   DC    CL14'++DDS SPYRYTRN'                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDFAXINFOD                                                     
*                                                                               
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
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
*                                                                               
SPD502   CSECT                                                                  
*                                                                               
DICSECT  DS    0D                                                               
DCLIST   DS    0C                                                               
         DCDDL SP#ACC01,59                                                      
         DCDDL SP#ACC02,59                                                      
         DCDDL SP#ADRMS,15                                                      
         DCDDL SP#AFFIL,9                                                       
         DCDDL SP#ARB,3                                                         
         DCDDL SP#BBM,3                                                         
         DCDDL SP#BRDCS,7                                                       
         DCDDL SP#BYDES,19         *                                            
         DCDDL SP#CHANN,7                                                       
         DCDDL SP#CLITL,18,C                                                    
         DCDDL SP#COMM,12          *                                            
         DCDDL SP#COST,7,R                                                      
         DCDDL SP#DATE2,25                                                      
         DCDDL SP#DAY,4                                                         
         DCDDL SP#DMGR2,18,R                                                    
         DCDDL SP#DOLLA,7,LABEL=SP7DOLLA                                        
         DCDDL SP#EST02,32                                                      
         DCDDL SP#EXCHG,8                                                       
         DCDDL SP#FREQ,4                                                        
         DCDDL SP#GOADE,9                                                       
         DCDDL SP#GOALM,6                                                       
         DCDDL SP#H,3                                                           
         DCDDL SP#IMP01,30,R                                                    
         DCDDL SP#LEN,3                                                         
         DCDDL SP#LIN01,24                                                      
         DCDDL SP#M,3                                                           
         DCDDL SP#MG,4,C                                                        
         DCDDL SP#MGRTL,7,LABEL=SP7MGRTL                                        
         DCDDL SP#MKTTL,7,LABEL=SP7MKTTL                                        
         DCDDL SP#MKTTL,18,C,LABEL=SP@MKTTL                                     
         DCDDL SP#MONTH,5                                                       
         DCDDL SP#MST,4                                                         
         DCDDL SP#NAM01,39,R                                                    
         DCDDL SP#NOBRD,10                                                      
         DCDDL SP#NOTLC,9                                                       
         DCDDL SP#NSI,3                                                         
         DCDDL SP#ORBIT,7,C                                                     
         DCDDL SP#ORIG,8                                                        
         DCDDL SP#ORMST,7                                                       
         DCDDL SP#P,3                                                           
         DCDDL SP#PKMST,7                                                       
         DCDDL SP#PLTS,15,C                                                     
         DCDDL SP#PLTS,15,CU,LABEL=SPUPLTS                                      
         DCDDL SP#PLTU,15,C                                                     
         DCDDL SP#PLTU,15,CU,LABEL=SPUPLTU                                      
         DCDDL SP#POL,3                                                         
         DCDDL SP#PRO02,28                                                      
         DCDDL SP#PROG,13                                                       
         DCDDL SP#PROLE,24,C                                                    
         DCDDL SP#PROTL,7,LABEL=SP7PROTL                                        
         DCDDL SP#PROTL,19,C,LABEL=SP@PROTL                                     
         DCDDL SP#RAPRB,19                                                      
         DCDDL SP#REP,3                                                         
         DCDDL SP#ROTPA,32                                                      
         DCDDL SP#ROTSC,17                                                      
         DCDDL SP#RVMST,7                                                       
         DCDDL SP#SIGN,59                                                       
         DCDDL SP#SPILL,7,C,LABEL=SP7SPILL                                      
         DCDDL SP#SPILL,8,LABEL=SP8SPILL                                        
         DCDDL SP#SPILL,11,C,LABEL=SP@SPILL                                     
         DCDDL SP#SPOTS,7,R                                                     
         DCDDL SP#SPREP,21,C                                                    
         DCDDL SP#STARS,25                                                      
         DCDDL SP#STATL,19,C                                                    
         DCDDL SP#STATN,7                                                       
         DCDDL SP#SUM,17,C                                                      
         DCDDL SP#TAX02,53                                                      
         DCDDL SP#THE01,45                                                      
         DCDDL SP#THE02,45                                                      
         DCDDL SP#TIME,5                                                        
         DCDDL SP#TLCST,6                                                       
         DCDDL SP#TOTAL,3,LABEL=SP3TOTAL                                        
         DCDDL SP#TOTAL,5,LABEL=SP5TOTAL                                        
         DCDDL SP#TOTAL,7,LABEL=SP7TOTAL                                        
         DCDDL SP#TOTAL,9,LABEL=SP9TOTAL                                        
         DCDDL SP#TRA01,19,C                                                    
         DCDDL SP#TXEX,18,C                                                     
         DCDDL SP#U,3                                                           
         DCDDL SP#UNKN,7                                                        
         DCDDL SP#UNSPT,17                                                      
         DCDDL SP#WKAVG,8                                                       
DCLISTX  DC    X'00'                                                            
*                                                                               
DSLIST   DS    0C                                                               
         DSDDL PRINT=YES                                                        
DSLISTX  EQU   *                                                                
         PRINT NOGEN                                                            
*                                                                               
         EJECT                                                                  
FLMPRD   DS    0D                                                               
         DS    450C                                                             
*                                                                               
PRDLST   DS    0D                                                               
         DS    670C                                                             
PRDLSTL  EQU   *-PRDLST                                                         
*                                                                               
         DS    0D                                                               
         DC    CL8'*MGABLK*'                                                    
MGABLK   DS    CL150                                                            
MGTABLE  DS    60000C              THIS IS TABLE PASSED TO BLDMGA               
MGTABLEX EQU   *                                                                
*                                                                               
         DS    0D                                                               
MGTABST  DS    800000C                                                          
MGTABSTX EQU   *                                                                
*                                                                               
         EJECT                                                                  
SPD5WK   DS    0D                                                               
*                                                                               
* DETAIL OPTIONS - CONTROLLED BY QOPT2                                          
*                     1=YES,0=NO                                                
*                                                                               
*               FIELD  OPTION                                                   
*               -----  ------                                                   
*                 1    PRINT '*' NEXT TO OVERRIDEN DEMOS                        
*                 2    PRINT DEMO VALUES                                        
*                 3    PRINT COST                                               
*                 4    PRINT CPP                                                
DETOPTS  DS    CL4                 CURRENT OPTIONS SAVE                         
DETOPT   DC    AL1(1,1,1,1)        OPTION TABLE                                 
         DC    AL1(0,1,1,1)                                                     
         DC    AL1(0,0,1,1)                                                     
         DC    AL1(0,1,0,0)                                                     
         DC    AL1(1,1,0,0)                                                     
         DC    AL1(0,0,0,0)                                                     
*                                                                               
* TOTAL OPTIONS - CONTROLLED BY QOPT5                                           
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
*                                                                               
* DATE OPTIONS - CONTROLLED BY PROFDCTL                                         
*                        SETS VARFRMT AND SCNDDTSW                              
DATEOPTS DC    X'00',AL1(0,0)                                                   
         DC    C' ',AL1(0,0)                                                    
         DC    C'0',AL1(0,0)                                                    
         DC    C'1',AL1(0,1)                                                    
         DC    C'2',AL1(1,0)                                                    
         DC    C'3',AL1(1,1)                                                    
         DC    C'4',AL1(2,0)                                                    
         DC    C'5',AL1(2,1)                                                    
         DC    X'FF',AL1(0,0)                                                   
DATEOPT  DS    CL2                                                              
* SORT OPTIONS      0 = NO SORT                                                 
*                   1 = DAY/TIME/PROGRAM                                        
*                   2 = TIME/DAY/PROGRAM                                        
SORTOPTS DC    C' ',AL1(0)                                                      
         DC    C'1',AL1(1)                                                      
         DC    C'2',AL1(2)                                                      
         DC    X'FF',AL1(0)                                                     
SORTOPT  DS    CL1                                                              
*                                                                               
* REPORT FORMAT OPTIONS                                                         
*                   FIELD1 = LENGTH OF GRID                                     
*                   FIELD2 = NUMBER IN GRID FOR PTS                             
*                   FIELD3 = NUMBER IN GRID FOR RS                              
*                  FIELD4 = DEMOGRAPHIC FORMAT FOR PTS                          
*                                                                               
FRMTOPTS DC    X'00',AL1(4,14,14,2)                                             
         DC    C'0',AL1(4,14,14,2)                                              
         DC    C'1',AL1(6,10,11,2)                                              
         DC    C'2',AL1(10,6,6,2)                                               
         DC    C'3',AL1(13,5,5,2)                                               
         DC    C'4',AL1(4,19,14,1)                                              
         DC    C'5',AL1(6,13,11,1)                                              
         DC    C'6',AL1(10,8,6,1)                                               
         DC    C'7',AL1(13,6,6,1)                                               
         DC    X'FF',AL1(4,14,14,2)                                             
FRMTOPT  DS    CL4                                                              
* CONDENSE OPTIONS                                                              
*                        SETS PGCNDSW                                           
*                                                                               
CNDSOPTS DC    X'00',AL1(0)                                                     
         DC    C'1',AL1(1)                                                      
         DC    C'2',AL1(2)                                                      
         DC    X'FF',AL1(0)                                                     
*----> SREPCAP  DC    C'***SPECIAL REP=   ***'                                  
SREPCAP  DC    AL3(SP@SPREP)                                                    
*----> SUMCAP   DC    CL20'*** SUMMARY ***   '                                  
SUMCAP   DC    AL3(SP@SUM)                                                      
RTYPE    DS    CL3                                                              
RSLINNO  DS    CL1                 RS REPORT PRINT LINE NO                      
RSCOMCTL DS    CL1                 RS REPORT COMMENT CONTROL                    
RSCOST   DS    CL1                 RS REPORT COST CONTROL                       
SVXLIN   DS    C                                                                
BUFHI    DS    C                                                                
BUFCDE   DS    C                                                                
BUFRTYP  DS    C                                                                
LEVEL    DS    C                   LEVEL CODE                                   
SPOD     DS    C                                                                
ESTACT   DS    CL1                                                              
CPPSW    DC    C'D'                                                             
BRDPOLSW DC    X'00'                                                            
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
CURPGPTR DS    F                   START OF PROGRAM SAVE AREA                   
CURPH01  DS    CL12                BUFFER ADDRESS/LEN/LOADER ADDRESS            
CURPH02  DS    CL12                                                             
CURPH04  DS    CL12                                                             
DNAMES   DS    0CL98                                                            
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
         DS    CL70                DEMOS 5-14                                   
SUBPROG  DS    0CL8                                                             
         DC    C'SP'                                                            
SUBPROG1 DC    C'M2'                                                            
SUBPROG2 DC    C'01'                                                            
         DC    C'  '                                                            
DASH     DC    80C'-'                                                           
NUMCOM   DS    X'00'               CURRENT NUMBER OF COMMENT LINES              
SCNDDTSW DC    X'01'               PRINT DATE ON SECOND LINE                    
RSSW     DC    X'00'               RS/RY ACTIVITY SWITCH                        
SVQUEST  DS    CL3                                                              
D5WLEN   DC    A(D5WEND-D5WSTRT)                                                
*                                                                               
QOPT7    EQU   QOPT5+2             Y=SUPPRESS ALL DOLLARS                       
QPNAME   EQU   Q2USER+0            Y=PRINT PRODUCT NAME LEGEND                  
QMCOM    EQU   Q2USER+4            Y/N PRINT/SUPPRESS MEDIA COMMENTS            
RSNEWSTA EQU   X'40'                                                            
RSOPEN   EQU   X'80'                                                            
         EJECT                                                                  
D5WSTRT  DS    0C                                                               
MYBUFIO  DS    CL200                                                            
SVPERD   DS    CL256                                                            
FIRST    DS    C                                                                
PASS     DS    C                                                                
MAXPASS  DS    C                   HIGHEST REQUIRED PASS                        
IDSW     DS    C                                                                
CONEND   DS    C                   END OF CONTRACT SWITCH                       
IDTOTALS DS    C                   PRINT ID TOTALS SWITCH                       
FBSTA    DS    C                                                                
PRVSTA   DS    CL7                                                              
FAXSTA   DS    CL7                                                              
PRTLINE  DS    CL150                                                            
CDMCB    DS    6F                                                               
VUDESC   DS    A(0)                                                             
VCALCPP  DC    A(0)                                                             
VSUMMRY  DC    A(0)                                                             
APTSDESC DC    A(0)                                                             
APRSDESC DC    A(0)                                                             
PSTASPT  DC    A(0)                                                             
PSTACOST DC    A(0)                                                             
PSTAGRID DC    A(0)                                                             
DSTAGRID DC    A(0)                A(DETAIL STATION GRID PRINT)                 
DDESC    DC    A(0)                A(DETAIL DESCRIPTION PRINT)                  
DTOTSPT  DC    A(0)                A(DETAIL TOTAL SPOTS)                        
MSBFHOOK DC    A(0)                                                             
REPCALOV DC    A(0)                                                             
ASVFRST  DS    F                                                                
ASVLAST  DS    F                                                                
ASVLCHNK DS    F                                                                
SVMSTAX  DS    CL4                                                              
SVMEDBYD DS    CL8                                                              
SVEXTAX  DS    CL1                                                              
ASVPRD   DS    C                                                                
PENNYSW  DS    C                                                                
SPACESW  DS    CL1                                                              
MGMSDSW  DC    X'00'                                                            
HLDPNAM  DS    CL14                                                             
HPSNO    DS    C                                                                
LASTGSLT DS    F                                                                
HLDBOOK  DS    CL2                                                              
         DS    CL12                                                             
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
MSSTART  DS    CL12                                                             
D5START  DS    CL12                                                             
TAXAMT   DC    F'0'                                                             
VMDADDWT DC    F'0'                                                             
VSTATOT  DC    F'0'                                                             
VEDTDEMS DC    F'0'                                                             
VGETBUF  DC    F'0'                                                             
VFLMPRD  DS    F                                                                
VPRDLST  DS    F                                                                
VSUBPARA DC    F'0'                                                             
VPGRID   DS    F                                                                
VCOMPRNT DS    F                                                                
PGHILNO  DS    F                   HIGHEST LINE NUMBER                          
PGNOENT  DS    F                                                                
PGCNDSW  DS    C                   CONDENSE LIKE SPOTS                          
PGCURLNO DS    F                   CURRENT LINE #                               
PGWMAX   DS    F                   MAXIMUM SLOTS                                
VARFRMT  DS    C                   VARIABLE FORMAT                              
AHDATES  DC    F'0'                                                             
SVRCSUB  DS    C                                                                
MSRCSUB  DS    C                                                                
SVSUPMKT DS    C                                                                
MSSUPMKT DS    C                                                                
DPTSW    DS    C                   DAYPART CHANGE SWITCH                        
MRPTTYP  DS    C                                                                
BUYACT   DS    C                                                                
MKTACT   DS    C                                                                
FORCEOPT DS    C                                                                
CFDS     DS    C                                                                
CFDE     DS    C                                                                
OVRFLAG  DS    CL16                                                             
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
VREVBUY  DC    F'0'                                                             
SVSGRID  DC    F'0'                                                             
VPLAREA  DC    F'0'                                                             
VMRGPL   DC    F'0'                                                             
MGLENLIN DS    H                                                                
CNDSOPT  DS    CL1                                                              
SVDEMS   DS    0F                                                               
SVD1     DS    F                   DEMO 1 VALUE                                 
SVD1CP   DS    F                   DEMO 1 CPP/CPM                               
SVD2     DS    F                   DEMO 2 VALUE                                 
SVD2CP   DS    F                   DEMO 2 CPP/CPM                               
SVD3     DS    F                   DEMO 3 VALUE                                 
SVD3CP   DS    F                   DEMO 3 CPP/CPM                               
SVD4     DS    F                   DEMO 4 VALUE                                 
SVD4CP   DS    F                   DEMO 4 CPP/CPM                               
SVDG     DS    F                                                                
SVDGCP   DS    F                                                                
         DS    CL80                DEMOS 5-14 VALUE / CPP-M                     
ACTMO    DS    F                                                                
UNIVERSE DS    F                                                                
WEIGHT   DS    F                                                                
MCOUNT   DS    F                                                                
NODEMS   DS    F                                                                
PLDEMS   DS    0CL154                                                           
PLD1     DS    CL5                                                              
PLD1CP   DS    CL6                                                              
PLD2     DS    CL5                                                              
PLD2CP   DS    CL6                                                              
PLD3     DS    CL5                                                              
PLD3CP   DS    CL6                                                              
PLD4     DS    CL5                                                              
PLD4CP   DS    CL6                                                              
         DS    CL110                                                            
STAGRID  DS    56F                                                              
STASPOT  DS    F                   STATION TOTAL SPOTS                          
STADEMS  DS    8F                  STATION TOTAL DEMOS                          
STACOST  DS    2F                  STATION TOTAL DOLLARS                        
STTSPOT  DS    F                   OVERALL STATION COST                         
STTDEMS  DS    8F                  OVERALL STATION DEMOS                        
STTCOST  DS    2F                  OVERALL STATION DOLLARS                      
VSVMDBLK DS    F                                                                
FILMNO   DS    CL8                                                              
STACAP   DS    CL7                                                              
MSOPT    DS    CL7                                                              
SVOPTS   DS    CL7                                                              
SUBPSW   DS    CL1                                                              
MSPROF   DS    CL16                                                             
SVPROF   DS    CL16                                                             
SVSPPROF DS    CL16                                                             
MSSPPROF DS    CL16                                                             
*                                                                               
D5APROF  DS    CL16                SECOND REPORT PROFILE                        
         ORG   D5APROF                                                          
D5ANODM  DS    C                   BRAND MODE NO. OF DEMOS                      
D5ASUPR  DS    C                   SUPPRESS REP                                 
D5ASUPA  DS    C                   SUPPRESS AFFILIATE                           
D5ASIG   DS    C                   PRINT SIGNATURE LINE                         
D5AXLIN  DS    C                   XTRA SPACING BETWEEN BLOCKS                  
D5AMCOM  DS    C                   PRINT MEDIA COMMENTS                         
D5ASUPMG DS    C                   SUPPRESS MAKEGOOD CAPTION                    
         ORG   D5APROF+16                                                       
*                                                                               
PASSSD3  DS    CL3                                                              
PASSED3  DS    CL3                                                              
PASSSD2  DS    XL2                                                              
PASSED2  DS    XL2                                                              
ELEMDT   DS    XL2                                                              
ELEMNUM  DS    XL1                                                              
PKGAREA  DS    CL16                BUY TYPE CAPTIONS                            
PIGAREA  DS    CL32                PIGGYBACK AREA                               
PIGAREAL DS    CL3                 PIGGYBACK LENGTH                             
PIGPRNT  DS    CL11                PIGGYBACK PRINT                              
INVFILM  DS     C                                                               
SPLPRINT DS    C                   PRINT ORIGINATING SWITCH                     
SPSTPL   DS    CL132                                                            
PTSSPILL DS    C                                                                
         DS    0F                                                               
PREMTAB  DS    CL64                                                             
HIATAB   DS    CL64                                                             
COVRHLD  DS    396C                COST OVERRIDE HOLD AREA                      
COVRFRST DS    C                   COST OVR FIRST TIME                          
OVRCNT   DS    C                   PL OVERRIDE COUNT                            
SVMAXLIN DS    C                                                                
MGSW     DS    C                                                                
APL      DS    F'0'                A(PL) FOR COST OVERRIDES                     
SVQEND   DS    CL6                                                              
SVSPREP  DS    H                                                                
SVP1     DS    CL40                                                             
SVP2     DS    CL40                                                             
SVP3     DS    CL40                                                             
SVP4     DS    CL40                                                             
SVP5     DS    CL40                                                             
NUMWK    DS    F                                                                
NOINGRID DS    H                                                                
LENGRID  DS    C                                                                
HLDNOSP  DS    C                                                                
CURRSORT DS    CL20                                                             
NEXTSORT DS    CL20                                                             
OPTRMODE DS    C                                                                
OPTRPT   DC    F'0'                                                             
OPTRPT2  DC    F'0'                                                             
STAUNA   DC    F'0'                                                             
UNATOT   DC    F'0'                                                             
VPNTABLE DC    F'0'                                                             
PDNCNTR  DC    F'0'                                                             
VSSTABLE DC    F'0'                                                             
SSCNTR   DC    F'0'                                                             
VRSORT   DC    F'0'                                                             
SORTPASS DS    C                                                                
SORTREQ  DS    C                                                                
SORTFRMT DS    C                                                                
CURRPNUM DS    C                                                                
THISPRD  DS    CL3                                                              
PASSTAB  DS    CL144               LIST OF PASS START-END DATES                 
PASSQST  DS    CL12                THIS PASS START-END                          
REASTART DS    CL12                REQUEST START-END DATES                      
SPBUFSTA DS    CL90                SAVE AREA FOR ORIG. STATIONS                 
SVMID1   DS    CL132                                                            
COMAREA  DS    CL400               COMMENT AREA                                 
SPBUFMKT DS    CL500               SAVE AREA FOR ORIG. MARKETS                  
D5WEND   DS    0C                  END OF WORK AREA                             
         EJECT                                                                  
PASSTABD DSECT                     SAVE MEDBLOCKS FOR PASSES                    
PASSSD   DS    CL6                 START DATE                                   
PASSED   DS    CL6                 END DATE                                     
PASSP1   DS    CL28                MEDBLOCK LEADER                              
PASSP2   DS    CL168               WEEKS                                        
PASSP3   DS    CL48                MONTHS                                       
PASSP4   DS    CL12                PERIOD                                       
PASSEND  DS    0C                                                               
         EJECT                                                                  
SPD502   CSECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'*PNTABLE'                                                    
PNTABLE  DS    4600C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*SSTABLE'                                                    
SSTABLE  DS    7100C                                                            
SSTABLEX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*PGRIDC*'                                                    
PGRIDC   DS    20000C                                                           
*                                                                               
SVMDBLK  DS    0D                                                               
         DS    1272C               SAVE MEDIA SUMMARY MEDBLOCK                  
*                                                                               
PLAREA   DS    0D                                                               
         DS    8000C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*NETCOVR'                                                    
NETCOVR  DS    200XL6                                                           
NETCOVRX EQU   *                                                                
*                                                                               
SUBPAREA DS    0D                                                               
         DS    90000C                                                           
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
PGRIDD   DSECT                                                                  
PGSORT   DS    CL11                                                             
PGLINNO  DS    C                   PRINT BLOCK LINE NUMBER                      
PGSUBLI  DS    C                   SUB-LINE NUMBER                              
PGLSLOT  DS    C                   PRINT BLOCK SLOT NUMBER                      
PGDWK    DS    CL2                 WEEK OF                                      
PGDIND   DS    CL1                 REG/MISSD/MG INDICATOR                       
PGDSBRN  DS    CL1                 SORT BRAND                                   
PGDSSLN  DS    CL1                 SORT SPOT LENGTH                             
PGDSNO   DS    CL1                 SORT SPOT NUMBER                             
PGDELAD  DS    CL4                 ELEMENT ADDRESS                              
PGDFDAY  DS    CL1                                                              
PGDFNO   DS    CL2                                                              
PGDNOSP  DS    CL1                 NUMBER OF SPOTS                              
PGD2BRN  DS    CL1                 PIGGYBACK BRAND                              
PGD2SLN  DS    CL1                 PIGGYBACK SPOT LENGTH                        
PGD2COVR DS    CL3                 COST OVERRIDES                               
PGDEND   DS    0C                                                               
PGDLEN   EQU   PGDEND-PGSORT                                                    
PGDLN1   EQU   PGDELAD-PGLINNO                                                  
PGSRTLN  EQU   L'PGSORT                                                         
         SPACE 2                                                                
PGSRT1D  DSECT                                                                  
PGDS1WK  DS    CL2                                                              
PGDS1DY  DS    C                                                                
PGDS1SLT DS    C                                                                
PGDS1BR  DS    CL1                                                              
PGDS1PIG DS    CL1                                                              
PGDS1SL  DS    CL1                                                              
PGDS1IND DS    CL1                                                              
         SPACE 2                                                                
PGSRT2D  DSECT                                                                  
PGDS2SLT DS    CL1                                                              
PGDS2WK  DS    CL2                                                              
PGDS2DY  DS    C                                                                
PGDS2SNO DS    C                                                                
PGDS2BR  DS    CL1                                                              
PGDS2PIG DS    CL1                                                              
PGDS2SL  DS    CL1                                                              
PGDS2IND DS    CL1                                                              
         EJECT                                                                  
PROFDSCT DSECT                  ***PROGRAM PROFILE 1 DSECT***                   
PROFSORT DS    CL1                 SORT CONTROL                                 
PROFFRMT DS    CL1                 PRINT FORMAT                                 
PROFCNDS DS    CL1                 CONDENSE CONTROL                             
PROFDCTL DS    CL1                 DATE CONTROL                                 
PROFDPC  DS    CL1                 DETAIL PRINT CONTROL                         
PROFCC   DS    CL1                 COMMENT CONTROL                              
PROFID   DS    CL1                 PRINT IDS ON DETAILS                         
PROFMSR  DS    CL1                 MARKET/STATION RECAP                         
PROFMPC  DS    CL1                 MARKET PRINT CONTROL                         
PROFDPT  DS    CL1                 MARKET DAYPART PRINT CONTROL                 
         ORG   PROFDPT                                                          
PROFPLEG DS    CL1                 PRINT PRODUCT LEGENDS FOR RS RPT             
         ORG                                                                    
PROFMTR  DS    CL1                 MARKET TOTAL REPORT                          
         DS    CL1                                                              
PROFBMS  DS    CL1                 BRAND MEDIA SUMMARY NUMBER                   
PROFPMS  DS    CL1                 POL MEDIA SUMMARY NUMBER                     
         DS    CL3                                                              
         EJECT                                                                  
PNAMD    DSECT                                                                  
PNDCODE  DS    CL1                 PROGRAM NUMBER                               
PNDNAME  DS    CL20                                                             
         SPACE 2                                                                
SEQSORT  DSECT                                                                  
SQSTART  DS    0C                                                               
SQ1DAY   DS    CL1                 DAY                                          
SQ1TIME  DS    CL2                 START-END QUARTER HOURS                      
SQ1PNUM  DS    CL1                 PROGRAM NUMBER                               
SQ1DADDR DS    CL4                 DISK ADDRESS                                 
SQ1END   DS    0C                                                               
         ORG   SQSTART                                                          
SQ2TIME  DS    CL2                                                              
SQ2DAY   DS    CL1                                                              
SQ2PNUM  DS    CL1                                                              
SQ2DADDR DS    CL4                                                              
SQ2END   DS    0C                                                               
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
       ++INCLUDE TEKEY                                                          
       ++INCLUDE TEQELEM                                                        
*        PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPMEDBDESD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
AGYRECD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
* DDDICTATED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
       ++INCLUDE SPMGADN                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048SPREPD502 11/19/19'                                      
         END                                                                    
