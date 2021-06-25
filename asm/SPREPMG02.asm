*          DATA SET SPREPMG02  AT LEVEL 035 AS OF 08/11/14                      
*PHASE SPMG02B                                                                  
         SPACE 1                                                                
*==============================================================*                
* QOPT1 = N TO SUPPRESS NO CHARGE SPOTS                        *                
* QOPT2 = N TO SUPPRESS PRE-EMPTS, O FOR PRE-EMPTS ONLY        *                
* QOPT3 = Y TO PRINT MARKET TOTALS                             *                
* QOPT4 = N TO SUPPRESS COSTS                                  *                
* QOPT5 = N TO SUPPRESS DEMOS                                  *                
*==============================================================*                
         TITLE 'SPMG02 - MGA ANALYSIS REPORT'                                   
SPMG02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPMG02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPMG02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,ESTFRST        GET DEMOS                                    
         BE    PEST                                                             
*                                                                               
         CLI   MODE,MKTFRST        CLR VALUES AT MARKET LEVEL                   
         BE    PMKT                                                             
*                                                                               
         CLI   MODE,STAFRST        CLR VALUES AT STATION LEVEL                  
         BE    PSTA                                                             
*                                                                               
         CLI   MODE,PROCBUY        MAKE SURE ALL SAVED SET                      
         BE    PBUY                                                             
*                                                                               
         CLI   MODE,STALAST                                                     
         BE    PSTAL                                                            
*                                                                               
         CLI   MODE,MKTLAST                                                     
         BE    PMKTL                                                            
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    PRUN                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
PRUN     LA    R0,MGHDHOOK                                                      
         ST    R0,HEADHOOK                                                      
         MVI   STAFLAG,C'N'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PROC EST - GET DEMOS                                                          
*                                                                               
PEST     DS    0H                                                               
         OI    RQOPTS,RQOPTS_HDEND  SET BREAK FOR HEADEND ONLY                  
         MVI   RCSUBPRG,0          RESET SPROG                                  
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         CLI   CPROF,C'0'          TEST BRAND POL CLIENT                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,1                                                       
         DROP  R6                                                               
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
         XC    SVDEMOS,SVDEMOS                                                  
         MVC   SVDEMLST(60),EDEMLST  LEAVE 3X'00' AS E-O-L FLAG                 
         MVC   SVWGTLST,EWGTLST                                                 
         MVC   SVUSRNMS(35),EUSRNMS                                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BQSTARTP)                              
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)                                  
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* PROC MARKET FIRST                                                             
*                                                                               
PMKT     DS    0H                                                               
         XC    MKTMSCST,MKTMSCST   CLEAR MKT TOTS                               
         XC    MKTMGCST,MKTMGCST                                                
         XC    MKTMSRTG,MKTMSRTG                                                
         XC    MKTMGRTG,MKTMGRTG                                                
         MVI   PRTMKT,C'N'                                                      
         B     EXIT                                                             
         EJECT                                                                  
* PROC STATION FIRST                                                            
                                                                                
*====================================================================           
* CALL MEDGETBY WITH X'00' IN BUY RECORD KEY TO GET IT TO SET                   
* THE 2-DECIMAL DEMO OPTION                                                     
*====================================================================           
*                                                                               
PSTA     DS    0H                                                               
*                                                                               
         MVC   STASAVE,BIGSTA                                                   
         CLI   STASAVE,C'0'        TEST CABLE                                   
         BL    *+10                  NO                                         
         MVC   STASAVE+4(5),SPACES   CABLE PRINTS HEADEND ONLY                  
*                                                                               
         XC    HALF,HALF           CLEAR MGCNT                                  
         LA    R4,MGTABLE                                                       
         LHI   R5,1000                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  R4,R0                                                            
*                                                                               
         L     RE,ADBUY                                                         
         MVI   0(RE),0                                                          
         GOTO1 MEDGETBY,DMCB,(RA),0                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PROCBUY                                                                       
*                                                                               
PBUY     DS    0H                                                               
         L     R3,ADBUY                                                         
         CLI   SVBUYKEY,X'10'      BUY KEY                                      
         BNH   PBX                  NO - DONT CHECK FOR SPILL                   
         LA    RF,SVBUYKEY+4       MARKET ON NORMAL POINTER                     
         TM    SVBUYKEY,X'0C'                                                   
         BNO   *+8                                                              
         LA    RF,SVBUYKEY+6       MARKET ON ID POINTER                         
         CLC   4(5,R3),0(RF)       TEST VS BUYREC MARKET                        
         BNE   PBX                                                              
*                                                                               
         LA    R5,MGABLK           CALL MGABLD TO BUILD TABLE                   
         USING MGABLKD,R5                                                       
         XC    0(MGALNQ,R5),0(R5)                                               
         MVC   MGACNT,HALF         RESTORE COUNT OF ITEMS                       
         MVI   MGAACT,MGAQBLN      SET ACTION - BUILD A LINE                    
         OI    MGAOPT,MGOPENTB     PUT AT END OF TABLE                          
*                                                                               
         OI    MGAOPT,MGOFLTDT     FILTER ON REQUEST DATES                      
         MVC   MGSFLTDT,BQSTARTP                                                
         MVC   MGEFLTDT,BQENDP                                                  
*                                                                               
         MVC   MGAACOM,ACOMFACS    SET A(COMFASCS)                              
         MVC   MGGETBUY,VGETBUY    SETS MG1OR2 AND V(GETBUY)                    
         MVC   MGABUY,ADBUY        A(IO AREA)                                   
*                                                                               
         CLI   QOPT1,C'N'          SKIP NO CHARGE                               
         BNE   *+8                                                              
         OI    MGAOPT,MGONONC                                                   
         CLI   QOPT2,C'N'          SKIP PREEMPTS                                
         BNE   *+8                                                              
         OI    MGAOPT,MGONOPR                                                   
*                                                                               
         CLI   QOPT2,C'O'          PREEMPTS ONLY                                
         BNE   *+8                                                              
         OI    MGAOPT,MGOPRONL                                                  
*                                                                               
         CLI   QOPT4,C'N'          SUPPRESS COSTS                               
         BNE   *+8                                                              
         OI    MGAOPT2,MGAOPT2_NOCOST                                           
*                                                                               
         CLI   QOPT5,C'N'          SUPPRESS DEMOS                               
         BNE   *+8                                                              
         OI    MGAOPT2,MGAOPT2_NODEMS                                           
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    *+8                                                              
         OI    MGAOPT2,MGAOPT2_2DEC                                             
*                                                                               
         MVC   SVMGAOPT,MGAOPT     SAVE OPTION VALUES                           
         MVC   SVMGAOP2,MGAOPT2                                                 
*                                                                               
         LA    R1,SVDEMOS                                                       
         ST    R1,MGABRDEM                                                      
         ST    R1,MGADEM                                                        
         LA    R1,MGTABLE                                                       
         ST    R1,MGATAB                                                        
         MVC   MGATABLF,=AL4(MGTABLEX)                                          
         OI    MGAOPT,MGOFULN      USE FULL WORD LENGTH                         
*                                                                               
         MVC   MGAAGMD,SVAGYMD     SET AGY/MED                                  
         MVC   MGACLT,SVCLT            CLIENT                                   
         MVC   MGAPRD,SVPRDCD          PRODUCT                                  
         MVC   MGASTA(2),SVMKT         MKT/STA                                  
         MVC   MGASTA+2(3),SVSTA                                                
         MVC   MGAEST,SVEST            ESTIMATE                                 
*                                                                               
         L     RE,MGABUY                                                        
         SR    R0,R0                                                            
         ICM   R0,3,BUYKBUY-BUYREC(RE)       GET LINE NUMBER                    
         TM    BUYRCNTL-BUYREC(RE),BUYRLN2   TEST 2-BYTE LINE NUMBER            
         BO    PB2                                                              
         LR    RF,R0                                                            
         SRL   RF,8                                                             
         STCM  RF,3,BUYKBUY-BUYREC(RE)    ALWAYS SET 2-BYTE LINE NUMBER         
PB2      MVI   MG1OR2,2                   AND TELL MGABLD                       
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         L     RF,VBLDMGN                                                       
         GOTO1 (RF),MGABLKD                                                     
*                                                                               
         L     RE,MGABUY                                                        
         STCM  R0,3,BUYKBUY-BUYREC(RE)    AND RESTORE ORIGINAL LINE NUM         
*                                                                               
         MVC   HALF,MGACNT                                                      
         CLI   MGAERR,0                                                         
         BE    PBX                                                              
         CLI   MGAERR,MGAQTFUL     TABLE FULL                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PBX      B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* STATION LAST - PRINT STATION TOTALS                                           
*                                                                               
PSTAL    DS    0H                                                               
*                                                                               
         LA    R5,MGABLK           CALL MGABLD TO BUILD TOTALS                  
         USING MGABLKD,R5                                                       
         XC    0(MGALNQ,R5),0(R5)                                               
         MVC   MGACNT,HALF         RESTORE COUNT OF ITEMS                       
         MVI   MGAACT,MGAQTOT      SET ACTION - GET TOTALS                      
         MVC   MGAACOM,ACOMFACS    SET A(COMFAS)                                
         MVC   MGGETBUY,VGETBUY    SETS MG1OR2 AND V(GETBUY)                    
         OI    MGAOPT,MGOPENTB     PUT AT END                                   
         OC    MGAOPT,SVMGAOPT     'OR' IN PREVIOUS VALUES                      
         OC    MGAOPT2,SVMGAOP2                                                 
         LA    R1,SVDEMOS                                                       
         ST    R1,MGABRDEM                                                      
         ST    R1,MGADEM                                                        
         LA    R1,MGTABLE                                                       
         ST    R1,MGATAB                                                        
         MVC   MGATABLF,=AL4(MGTABLEX)                                          
         OI    MGAOPT,MGOFULN      USE FULL WORD LENGTH                         
*                                                                               
         MVC   MGAAGMD,SVAGYMD     SET AGY/MED                                  
         MVC   MGACLT,SVCLT            CLIENT                                   
         MVC   MGAPRD,SVPRDCD          PRODUCT                                  
         MVC   MGASTA(2),SVMKT         MKT/STA                                  
*        MVC   MGASTA+2(3),SVSTA                                                
         MVC   MGASTA+2(3),SVBUYKEY+6                                           
         MVC   MGAEST,SVEST            ESTIMATE                                 
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         L     RF,VBLDMGN                                                       
         GOTO1 (RF),MGABLKD                                                     
*                                                                               
         CLI   MGAERR,0                                                         
         BE    PSL10                                                            
         CLI   MGAERR,MGAQTFUL     TABLE FULL                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSL10    LA    R4,MGTABLE                                                       
*                                                                               
         OC    0(MGERECL,R4),0(R4) END OF TABLE                                 
         BZ    PSLX                                                             
*                                                                               
         ZIC   R0,LINE                                                          
         AHI   R0,1                                                             
         ZIC   RF,MAXLINES                                                      
         CR    R0,RF               MAKE SURE STATION IS NOT                     
         BL    PSL15               PRINTED ON LAST LINE                         
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
*                                                                               
PSL15    DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(4),STASAVE        MOVE 4 CHARS OF STA                          
         CLI   STASAVE,C'0'        TEST CABLE                                   
         BNL   *+10                YES - THAT'S ALL                             
         MVC   P(L'STASAVE),STASAVE  ELSE PRINT THE STATION                     
         GOTO1 REPORT                                                           
*                                                                               
PSL20    OC    0(MGERECL,R4),0(R4) END OF TABLE                                 
         BZ    PSLX                                                             
*                                                                               
         BAS   RE,PRTIT                                                         
         LA    R4,MGERECL(R4)                                                   
         B     PSL20                                                            
*                                                                               
PSLX     MVI   STAFLAG,C'N'                                                     
         CLI   QOPT6,C'M'                                                       
         BE    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================                          
* MARKET LAST - PRINT MARKET TOTALS                                             
*=====================================================                          
                                                                                
PMKTL    DS    0H                                                               
         CLI   QOPT3,C'Y'          MARKET TOTALS OPTION                         
         BNE   PMKTLX                                                           
         CLI   PRTMKT,C'Y'                                                      
         BNE   PMKTLX                                                           
*                                                                               
         USING MGLINED,R2                                                       
         LA    R2,P                                                             
         MVC   MGLCODE-1(4),=C'MKT '                                            
         MVC   MGLTYPE(3),=C'TOT'                                               
*                                                                               
         LA    R6,MGLMSCST         MISSED TOTAL                                 
         L     R0,MKTMSCST                                                      
         BAS   RE,PRTAMT                                                        
*                                                                               
         LA    R6,MGLMSRTG-1       MISSED RATING TOTAL                          
         L     R0,MKTMSRTG                                                      
         BAS   RE,PRTRTG                                                        
*                                                                               
         LA    R6,MGLMGCST         MAKEGOOD TOTAL                               
         L     R0,MKTMGCST                                                      
         BAS   RE,PRTAMT                                                        
*                                                                               
         LA    R6,MGLMGRTG-1       MAKEGOOD RATING TOTAL                        
         L     R0,MKTMGRTG                                                      
         BAS   RE,PRTRTG                                                        
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         GOTO1 REPORT                                                           
PMKTLX   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
*        EDIT OUT TO TOTAL LINE                                                 
*                                                                               
PRTAMT   NTR1                                                                   
         CLI   QOPT4,C'N'          TEST SUPPRESS COST                           
         BE    EXIT                                                             
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(8,0(R6)),FLOAT=$,ZERO=NOBLANK                              
         B     EXIT                                                             
*                                                                               
*        EDIT OUT RATINGS TO TOTAL LINE                                         
*                                                                               
PRTRTG   NTR1                                                                   
         CLI   QOPT5,C'N'          TEST SUPPRESS RATING                         
         BE    EXIT                                                             
         EDIT  (R0),(8,0(R6)),1                                                 
         B     EXIT                                                             
         EJECT                                                                  
* S/R TO PRINT ENTRY IN TABLE                                                   
*                                                                               
PRTIT    NTR1                                                                   
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BL    PRT10                                                            
*                                                                               
         MVC   P(L'STASAVE),STASAVE                                             
         GOTO1 REPORT                                                           
         MVI   STAFLAG,C'N'                                                     
*                                                                               
PRT10    DS    0H                                                               
         LA    R5,MGABLK           CALL MGABLD TO BUILD TABLE                   
         USING MGABLKD,R5                                                       
         XC    0(MGALNQ,R5),0(R5)                                               
         MVI   MGAACT,MGAQPRNT     SET ACTION - SET UP PRINT LINE               
         MVC   MGAACOM,ACOMFACS    SET A(COMFACS)                               
         MVC   MGGETBUY,VGETBUY    SETS MG1OR2 AND V(GETBUY)                    
         MVC   MGUNTIME,UNTIME     SET A(UNTIME)                                
         MVC   MGMSUNPK,MSUNPK     SET A(MSUNPK)                                
         OC    MGAOPT,SVMGAOPT                                                  
         OC    MGAOPT2,SVMGAOP2                                                 
         USING MGENTRYD,R4                                                      
         MVC   MGAENTRY,0(R4)                                                   
*                                                                               
         LA    R2,P                                                             
         USING MGLINED,R2                                                       
         ST    R2,MGALINE                                                       
         MVC   0(L'P,R2),SPACES                                                 
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         L     RF,VBLDMGN                                                       
         GOTO1 (RF),MGABLKD                                                     
*                                                                               
         CLC   =C'TOT',MGLTYPE                                                  
         BNE   PRT15                                                            
         MVI   SPACING,2           LEAVE XTRA SPACE AFTER TOTS                  
*                                                                               
         CLC   =C'GRND',MGLCODE-1  TOTAL FOR STATION?                           
         BNE   PRT20                                                            
*                                                                               
         L     R1,MGETMISS         ADD STATION TOTALS TO MKT TOTS               
         A     R1,MKTMSCST                                                      
         ST    R1,MKTMSCST                                                      
         L     R1,MGETMG                                                        
         A     R1,MKTMGCST                                                      
         ST    R1,MKTMGCST                                                      
         L     R1,MGEMSRTG                                                      
         A     R1,MKTMSRTG                                                      
         ST    R1,MKTMSRTG                                                      
         L     R1,MGEMGRTG                                                      
         A     R1,MKTMGRTG                                                      
         ST    R1,MKTMGRTG                                                      
         B     PRT20                                                            
*                                                                               
PRT15    CLI   MGEPRD1,0                                                        
         BE    EXIT                                                             
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         CLI   CPROF,C'0'          TEST BRAND POL CLIENT                        
         BNE   PRT20                                                            
         LA    R6,CLIST            YES, SHOW PRODUCTS NOT STATION               
         DROP  R6                                                               
*                                                                               
         MVC   MGLSTA,SPACES       DISPLAY PRODUCTS NOT STATION                 
         MVC   FULL(1),MGEPRD1                                                  
         BAS   RE,GETPR                                                         
         MVC   MGLSTA(3),FULL+1                                                 
         CLI   MGEPRD2,0                                                        
         BE    PRT20                                                            
         MVC   FULL(1),MGEPRD2                                                  
         BAS   RE,GETPR                                                         
         LA    R1,MGLSTA+2                                                      
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   0(R1),C'-'                                                       
         MVC   1(3,R1),FULL+1                                                   
*                                                                               
PRT20    DS    0H                                                               
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         CLI   CPROF,C'0'          TEST BRAND POL CLIENT                        
         BE    *+16                                                             
         MVC   MGLSTA(14),MGLPGMNM                                              
         MVC   MGLSTA+14(11),SPACES                                             
         DROP  R6                                                               
         GOTO1 REPORT                                                           
         MVI   PRTMKT,C'Y'                                                      
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
* GET PRODUCT CODE USING CLIST AT R6 AND BIN# AT FULL                           
* RETURN PRODUCT IN FULL+1                                                      
         SPACE 1                                                                
GETPR    MVC   FULL+1(3),=C'???'                                                
GETPR2   CLI   0(R6),C'A'                                                       
         BLR   RE                                                               
         CLC   FULL(1),3(R6)                                                    
         BE    GETPR4                                                           
         LA    R6,4(R6)                                                         
         B     GETPR2                                                           
GETPR4   MVC   FULL+1(3),0(R6)     MOVE PRODUCT CODE                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* HEADHOOK                                                       *              
*================================================================*              
         SPACE 1                                                                
MGHDHOOK NMOD1 0,**MGHD**                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,DUB)                                       
         MVC   H4+113(4),DUB       UPDATE THE YEAR!                             
*                                                                               
         CLI   QOPT4,C'N'          TEST SUPPRESS DOLLARS                        
         BNE   HDHK2                                                            
         MVC   H8+52(8),SPACES                                                  
         MVC   H8+75(8),SPACES                                                  
         MVC   H9+52(8),SPACES                                                  
         MVC   H9+75(8),SPACES                                                  
         MVC   H10+52(8),SPACES                                                 
         MVC   H10+75(8),SPACES                                                 
*                                                                               
HDHK2    CLI   QOPT5,C'N'          TEST SUPPRESS DEMOS                          
         BNE   HDHKX                                                            
         MVC   H9+66(8),SPACES                                                  
         MVC   H9+90(8),SPACES                                                  
         MVC   H10+66(8),SPACES                                                 
         MVC   H10+90(8),SPACES                                                 
*                                                                               
HDHKX    XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
STASAVE  DS    CL9                                                              
STAFLAG  DS    XL1                                                              
*                                                                               
SVDEMOS  DS    XL118                                                            
         ORG   SVDEMOS                                                          
SVDEMLST DS    CL63                60 BYTE LIST + 3X'00'                        
SVWGTLST DS    CL20                                                             
SVUSRNMS DS    CL28                                                             
SVWGTNM  DS    CL7                                                              
*                                                                               
*                                                                               
PRTMKT   DS    X                                                                
MKTMSCST DS    F                   MARKET TOTAL - MISSED COST                   
MKTMSRTG DS    F                   MARKET TOTAL - MISSED RATING POINTS          
MKTMGCST DS    F                   MARKET TOTAL - MAKEGOOD COST                 
MKTMGRTG DS    F                   MARKET TOTAL - MG RATING POINTS              
*                                                                               
SVMGAOPT DS    X                                                                
SVMGAOP2 DS    X                                                                
         DS    0D                                                               
         DC    CL8'*MGABLK*'                                                    
MGABLK   DS    XL200                                                            
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
QOPT6    EQU   QOPT5+1                                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'**MGTAB*'                                                    
MGTABLE  DS    5000CL(MGERECL)                                                  
MGTABLEX EQU   *-MGTABLE                                                        
         PRINT OFF                                                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
LINED    DSECT                                                                  
         DS    CL1                                                              
LCODE    DS    CL2                                                              
         DS    CL2                                                              
         DS    CL1                                                              
LTYPE    DS    CL1                                                              
LNET     DS    CL3                                                              
         DS    CL4                                                              
LLINE    DS    CL3                                                              
         DS    CL4                                                              
LDATE    DS    CL8                                                              
         DS    CL3                                                              
LSLN     DS    CL3                                                              
         DS    CL3                                                              
LTIME    DS    CL11                                                             
         DS    CL2                                                              
LMSCOST  DS    CL8                                                              
         DS    CL4                                                              
LMSRTG   DS    CL4                                                              
         DS    CL4                                                              
LMGCOST  DS    CL8                                                              
         DS    CL4                                                              
LMGRTG   DS    CL4                                                              
         DS    CL3                                                              
LSTA     DS    CL8                                                              
         DS    CL3                                                              
LPGMNM   DS    CL14                                                             
         EJECT                                                                  
TLINED   DSECT                                                                  
LTMISS   DS    CL8                                                              
         DS    CL1                                                              
LTMSRTG  DS    CL5                                                              
         DS    CL1                                                              
LTMG     DS    CL8                                                              
         DS    CL1                                                              
LTMGRTG  DS    CL5                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMGADN                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035SPREPMG02 08/11/14'                                      
         END                                                                    
