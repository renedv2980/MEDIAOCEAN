*          DATA SET PPREPPZ02  AT LEVEL 006 AS OF 02/26/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE PPPZ02A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE DDUCOM                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE PPBVAL                                                                 
*INCLUDE PERVAL                                                                 
         TITLE 'PPPZ02 - PFIZER INTERFACE'                                      
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*        QOPT5 BILL TYPE CHECKS                                                 
*                                                                               
*        QOPT6 Y= TEST RUN - NO TAPE, AND CONTINUE IF ERRORS                    
*                            ARE FOUND                                          
*        QOPT7 P=PDUMP RECORDS                                                  
*                                                                               
*        QSTART(6) = PERIOR START                                               
*        QEND(6) = PERIOD END - MAY BE BLANK                                    
*                                                                               
*        NOTE: PROGRAM USES THE BRAND'S ADDRESS DATA                            
*              NOT THE ONES FROM PRODUCT AAA (LIKE THE SE PROGRAM)              
*              IT ALSO READS BILLS  - NOT INSERTIONS                            
*                                                                               
PPPZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPPZ02,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'      2ND DSECT                                     
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PPPZWRKD,R8                                                      
         MVC   ACONIO1,ACONIO                                                   
         LA    R7,PPPZ02+4095                                                   
         LA    R7,1(R7)                                                         
         USING PPPZ02+4096,R7     **NOTE USE OF R7 AS BASE REG*                 
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,REQFRST                                                     
         BE    FIRSTB                                                           
         CLI   MODE,FBUYCLI                                                     
         BE    FCLI                                                             
         CLI   MODE,FBILEST                                                     
         BE    FEST                                                             
         CLI   MODE,FBUYPRO                                                     
         BE    FPRD                                                             
         CLI   MODE,REQLAST                                                     
         BE    PUTBUFF                                                          
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
         CLI   MODE,PROCBIL                                                     
         BE    PROCESS                                                          
*                                                                               
         CLI   MODE,CLILAST       END OF CLIENT                                 
         BE    LCLI                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                         RUN FIRST                                             
INITIAL  DS    0H                                                               
*                                                                               
         USING IHADCB,RF           USING DCB DSECT                              
         LA    RF,PPZTAPE                                                       
         MVC   DCBLRECL,=Y(OUTRECL)   FORCES CORRECT REC-LENGTH                 
         DROP  RF                                                               
*                                                                               
         MVI   MISSERR,0                                                        
         MVI   SKIPPRD,C'N'                                                     
*                                                                               
         L     R0,=V(GETUSER)                                                   
         A     R0,RELO                                                          
         ST    R0,VGETUSER                                                      
         L     R0,=V(DDUCOM)                                                    
         A     R0,RELO                                                          
         ST    R0,VDDUCOM                                                       
         L     R0,=V(PPFMTINO)                                                  
         A     R0,RELO                                                          
         ST    R0,AFMTINO                                                       
         L     R0,=V(PERVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,APERVAL                                                       
         L     R0,=V(PPBVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,APPBVAL                                                       
         LA    R0,PAGYREC                                                       
         ST    R0,ADAGY                                                         
*                                                                               
         XC    MYDUMP,MYDUMP                                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY1)       YYMMDD                        
         GOTO1 DATCON,DMCB,(5,0),(X'14',TODAYY)   YYYYMMDD                      
         ZAP   TOTCNT,=P'0'                                                     
         MVI   COLSW,C'N'                                                       
         MVI   IHDRSW,C'N'                                                      
         MVI   FRSTBILL,C'N'                                                    
         MVI   LASTBILL,C'N'                                                    
         ZAP   INVTOTD,=P'0'                                                    
         ZAP   INVRCNT,=P'0'                                                    
*                                                                               
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   TAPESW,0                                                         
         MVI   ETAPESW,0                                                        
         MVI   ALLOWSW,0     DYNAMIC ALLOCATION INV. FILE NOT DONE              
*                                                                               
INIT5    DS    0H                                                               
         L     R0,=A(TITLES)                                                    
         A     R0,RELO                                                          
         ST    R0,ATITLES                                                       
*                                                                               
         L     R0,=A(LENTAB)                                                    
         A     R0,RELO                                                          
         ST    R0,ALENTAB                                                       
*                                                                               
         L     R2,ALENTAB          ZAP ACCUMS                                   
         LA    R3,4                                                             
INIT7L   ZAP   4(4,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,INIT7L                                                        
*                                                                               
INIT70   DS    0H                                                               
**                                                                              
INITX    B     EXIT                                                             
         EJECT                                                                  
*                       REQUEST FIRST                                           
FIRSTB   DS    0H                                                               
*                                                                               
         LA    RF,PCLTREC                                                       
         ST    RF,ADCLT                                                         
         LA    RF,PPRDREC                                                       
         ST    RF,ADPRD                                                         
         LA    RF,PESTREC                                                       
         ST    RF,ADEST                                                         
*                                                                               
         MVC   DYNDDN,=CL8'PPZTAPE'                                             
         MVC   DYNDSN,=CL20'PRTTAPE.PP0PZXX1'                                   
         MVC   DYNDSN+13(2),QAGENCY                                             
*                                                                               
REQF10   DS    0H                                                               
         XC    LBILLKEY,LBILLKEY                                                
         XC    LESTOUT,LESTOUT                                                  
         MVC   SVQOPT1,QOPT1      SAVE FILE TYPE                                
         MVC   SVQOPT6,QOPT6      SAVE DO TAPE OPTION                           
         MVC   SVQOPT7,QOPT7      SAVE PDUMPING OPTION                          
*                                                                               
         CLC   QEST(2),=C'NO'                                                   
         BE    REQF65                                                           
         CLC   QEST(3),=C'ALL'                                                  
         BE    REQF65                                                           
*                                                                               
REQF50   CLC   QEST(6),SPACES                                                   
         BNE   REQF55                                                           
         B     REQF65                                                           
REQF55   CLC   QESTEND,SPACES                                                   
         BNE   REQF60                                                           
         MVC   QESTEND,QEST      IF ONLY ONE EST SET QESTEND TO IT              
REQF60   PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,MYBEST                                                        
         PACK  DUB,QESTEND                                                      
         CVB   R0,DUB                                                           
         STH   R0,MYBESTE          NEED TO SET NOW FOR TBCLTF                   
         B     REQF70                                                           
*                                                                               
REQF65   MVC   MYBEST,=H'1'                                                     
         MVC   MYBESTE,=H'999'     MAX ESTIMATE                                 
REQF70   DS    0H                                                               
*                                                                               
         CLI   QOPT6,C'Y'        SEE IF TEST REQUEST                            
         BE    FIRSTB0                                                          
         CLI   TAPESW,C'N'       SEE IF A PRIOR REQUEST WAS TEST                
         BE    MIXERR                                                           
         MVI   TAPESW,C'Y'       SET TAPE BEING PRODUCED                        
*                                                                               
         TM    ALLOWSW,X'01'     DYNAMIC ALLOCATION DONE ALREADY?               
         BO    REQF75                                                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,DYNDDN),(0,DYNDSN)                              
         OI    ALLOWSW,X'01'    SO I WON'T DO AGAIN                             
         PRINT GEN                                                              
         OPEN  (PPZTAPE,OUTPUT)                                                 
         PRINT NOGEN                                                            
         B     REQF75                                                           
*                                                                               
*                                                                               
REQF75   B     FIRSTB0X                                                         
*                                                                               
FIRSTB0  DS    0H                                                               
         CLI   TAPESW,C'Y'      SEE IF A PRIOR REQUEST WAS LIVE                 
         BE    MIXERR                                                           
         MVI   TAPESW,C'N'                                                      
         B     FIRSTB0X                                                         
*                                                                               
MIXERR   MVC   P1(37),=C'*** MIX OF TEST AND LIVE REQUESTS ***'                 
         MVC   P2(37),=C'*** THIS REQUEST HAS BEEN SKIPPED ***'                 
         GOTO1 MYRPT                                                            
         MVI   MODE,REQLAST    SKIP TO NEXT REQUEST                             
         B     EXIT                                                             
*                                                                               
FIRSTB0X DS    0H                                                               
*                             SET MYUSER FROM AGYTAB                            
         LA    R1,AGYTAB                                                        
FIRSTB1  CLI   0(R1),X'FF'            END OF TABLE                              
         BNE   *+6                                                              
         DC    H'0'                   INVALID AGENCY                            
*                                                                               
         CLC   0(2,R1),QAGENCY                                                  
         BE    FIRSTB1D                                                         
         LA    R1,AGYTABL(R1)                                                   
         B     FIRSTB1                                                          
*                                                                               
FIRSTB1D MVC   MYUSER,2(R1)         USER FIELDS IN USE                          
*                                                                               
FIRSTB2  DS    0H                                                               
         XC    ESTU1,ESTU1          CLEAR USER FIELDS                           
         XC    ESTU2,ESTU2                                                      
         XC    PRDU1,PRDU1                                                      
         XC    PRDU2,PRDU2                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   CINVGRS,=P'0'                                                    
         ZAP   CINVBIL,=P'0'                                                    
         ZAP   CINVCD,=P'0'                                                     
         ZAP   CINVRCV,=P'0'                                                    
         MVI   CINVSW,0                                                         
*                                                                               
*                                                                               
         CLC   QEND,SPACES                                                      
         BNE   FIRSTB3C                                                         
         MVC   QEND(6),QSTART     SET END TO START,IF NOT ENTERED               
*                                                                               
FIRSTB3C DS    0H                                                               
         MVC   SVSTART(12),QSTART                                               
*                                                                               
FIRSTB3X LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         B     EXIT                                                             
*                                                                               
FIRSTB4  DS    0H                                                               
FIRSTBX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                  LAST FOR CLIENT                              
LCLI     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                   FIRST FOR CLIENT                            
FCLI     DS    0H                                                               
*                                                                               
         BAS   RE,FNDAAA      FIND PRD AAA AND STORE ITS ADDRESS                
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(2),PCLTKAGY                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   FBC1                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         XC    LINVFULL,LINVFULL      CLEAR FOR NEW REQ                         
         XC    LBQDATE,LBQDATE                                                  
         MVI   FRSTBILL,C'Y'                                                    
         MVI   LASTBILL,C'N'                                                    
         ZAP   INVTOTD,=P'0'     CLEAR TOTAL AMOUNT DUE                         
         XC    INVTAB,INVTAB     CLEAR INVOICE DETAIL TABLE                     
*                                                                               
TBCF60   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
FPRD     DS    0H                  FIRST BUY FOR PRODUCT                        
         MVI   SKIPPRD,C'N'                                                     
*                                      COMPANY AND OFFICE CODE                  
*                                      FROM PRODUCT INTERFACE                   
*                                                                               
         MVC   SVPRDIFC,SPACES                                                  
         LA    R2,PPRDREC+33                                                    
         MVI   ELCODE,X'30'      LOOK FOR INTERFACE CODE ELEMENT                
         BAS   RE,NEXTEL                                                        
         BNE   FPRD5                                                            
         MVC   SVPRDIFC,PPRDINFC-PPRDICEL(R2)   SAVE FIRST 4 CHARACTERS         
*                                                                               
FPRD5    DS    0H                SAVE BRAND'S ADDRESSES                         
         MVC   BPRDBILL,PPRDBILL                                                
         MVC   BPRDBIL2,PPRDBIL2                                                
         MVC   BPRDLIN1,PPRDLIN1                                                
****     MVC   BPRDLIN1,SPACES    NOT USED FOR PFIZER NEXTEL                    
*                                 SINCE LINE ONE IS A P.O. BOX                  
         MVC   BPRDLIN2,PPRDLIN2                                                
         MVC   BPRDATTN,PPRDATTN                                                
*                                                                               
         MVC   PUCOMM1(132),SPACES    CLEAR 4 PRD UCOMMS                        
         MVI   PUCOMM1,0        LENGTHS TO ZERO                                 
         MVI   PUCOMM2,0                                                        
         MVI   PUCOMM3,0                                                        
         MVI   PUCOMM4,0                                                        
*                                                                               
         LA    R3,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R3                                                       
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         MVC   UCACOMF,VCOMFACS     COMFACS                                     
         MVI   UCSYS,C'P'        SYSTEM TO PRINT                                
         MVC   UCAGY,QAGENCY                                                    
         MVC   UCMED,QMEDIA                                                     
         MVC   UCCLT,PCLTKCLT      CLIENT                                       
         MVC   UCPRD,PPRDKPRD      PRODUCT                                      
         OI    UCOPT,UCOPRD        RETURN PRODUCT UCOMMS                        
         GOTO1 =V(DDUCOM),UCOMBLK                                               
         SR    R0,R0                                                            
         CLI   UCERROR,0                                                        
         BNE   FPRDERR      ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOPRD                                                  
         BO    FPRDERR     NO PRD DATA                                          
*                                                                               
         L     R1,UCPDATA     PRD DATA                                          
         MVC   UCOMDATA,0(R1)                                                   
         LA    R1,UCOMDATA                                                      
         LA    RF,UCPLENS     LENGTHS                                           
         LA    RE,PUCOMM1     START OF 1ST PRD UCOMM                            
         LH    R0,=H'1'                                                         
FPRD10   ST    R1,SAVER1                                                        
         ST    RF,SAVERF                                                        
         ST    RE,SAVERE                                                        
         ST    R0,SAVER0                                                        
         CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    FPRDERR        MISSING DATA                                      
         MVC   0(1,RE),0(RF)  SAVE LENGTH                                       
         MVC   1(32,RE),0(R1) AND DATA                                          
         OC    1(32,RE),=32C' '                                                 
FPRD12   LA    RE,33(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         AH    R0,=H'1'                                                         
         CH    R0,=H'4'                                                         
         BNH   FPRD10                                                           
*                                                                               
         DROP  R3                                                               
*                                                                               
FPRD15   MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                RESTORE SEQUENCE                             
         B     FPRDSAVE                                                         
*                                                                               
***********************************************************                     
******  CODE BELOW READ PRODUCT USER FIELDS                                     
***********************************************************                     
         XC    PRDU1,PRDU1                                                      
         CLI   MYUSER,C'Y'        SEE IF USING USER FIELDS                      
         BNE   FPRDSAVE                                                         
         CLC   PPRDKPRD,=C'AAA'    NOT FOR PRD=AAA                              
         BE    FPRDSAVE                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'P',ADPRD),PRDU1,PRDU2              
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BE    FPRDERR                                                          
         CLI   PRDU1+21,C' '    MUST FIND DATA                                  
         BNH   FPRDERR                                                          
         B     FPRDSAVE                                                         
*                                                                               
FPRDERR  DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                RESTORE SEQUENCE                             
         MVI   SKIPPRD,C'Y'        SKIP ANY BILLS FOR THIS PRODUCT              
*                                                                               
         MVC   P1,SPACES                                                        
         MVC   P1(34),=C'*** MISSING PRODUCT UCOMM   DATA ***'                  
         L     R0,SAVER0     PUCOMM #                                           
         LTR   R0,R0                                                            
         BZ    FPRDERR5                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+26(1),DUB+7(1)                                                
FPRDERR5 MVC   P1+42(3),PPRDKPRD                                                
         MVC   P2(34),=C'*** ANY INVOICES WILL BE SKIPPED ***'                  
         MVI   SPACING,2                                                        
         GOTO1 MYRPT                                                            
         B     FPRDE10                                                          
*                                                                               
***      CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
***      BE    FPRDE10               CONTINUE - ELSE DIE                        
***      MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
***      GOTO1 REPORT                                                           
***      DC    H'0'              MUST DIE                                       
*                                                                               
FPRDE10  L     R1,SAVER1       MUST RESTORE R1,RE,RF,R0                         
         L     RE,SAVERE                                                        
         L     RF,SAVERF                                                        
         L     R0,SAVER0                                                        
         C     R0,=F'0'                                                         
         BNH   FPRDSAVE                                                         
         B     FPRD12          KEEP LOOKING                                     
*                                                                               
FPRDSAVE DS    0H                                                               
*                                                                               
         CLC   PPRDKPRD,=C'AAA'                                                 
         BNE   FPRDFORM                                                         
*                                                                               
         MVI   AAAPRD,C'Y'                                                      
         MVC   AAAFORMU(5),PPRDBILP         MOVE FORMULA FOR AAA                
         B     FPRDX                                                            
*                                                                               
FPRDFORM DS    0H                                                               
         MVC   PRDFORMU(5),PPRDBILP                                             
*                                                                               
*                                                                               
FPRDX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FEST     DS    0H                  ESTIMATE FIRST                               
         XC    PREGREC,PREGREC     CLEAR REGION RECORD                          
         CLI   QOPT1,C'E'          ESTIMATE FILE?                               
         BE    FESTX               NOTHINGTO DO                                 
*                                                                               
FEST0    DS    0H                                                               
*                                                                               
         XC    ESTU1,ESTU1       CLEAR EST USER FIELDS                          
         XC    ESTU2,ESTU2                                                      
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'E',ADEST),(C':',ESTU1),(C'+        
               :',ESTU2)                                                        
*                                                                               
FESTX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PUTBUFF  DS    0H      FIRST PRINT TOTAL LINE FOR CURRENT INVS                  
         TM    CINVSW,1                                                         
         BZ    PUTB2                                                            
         MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
         MVC   P1+28(7),=C'*TOTAL*'                                             
         EDIT  (P8,CINVGRS),(14,P1+37),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVBIL),(14,P1+53),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVCD),(14,P1+69),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,CINVRCV),(14,P1+85),2,COMMAS=YES,FLOAT=-                     
         MVI   P1+51,C'*'                                                       
         MVI   P1+67,C'*'                                                       
         MVI   P1+83,C'*'                                                       
         MVI   P1+99,C'*'                                                       
         BAS   RE,MYRPT                                                         
PUTB2    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20                                                      
*                                  PUT BUFFALO RECS TO TAPE                     
*                                  AT LBUYREQ                                   
         ZAP   GTTOTCD,=P'0'                                                    
         ZAP   GTTOTAMT,=P'0'                                                   
         ZAP   GTTOTCOM,=P'0'                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TOTALS   DS    0H                                                               
*                                                                               
TOT0     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         L     R4,ATITLES                                                       
         L     R3,ALENTAB                                                       
         LA    R6,4                FOR BCT                                      
TOT2     MVC   P1+7(30),0(R4)                                                   
         EDIT  (P4,4(R3)),(9,P1+40),0,COMMAS=YES                                
         BAS   RE,MYRPT                                                         
         LA    R4,30(R4)                                                        
         LA    R3,8(R3)                                                         
         BCT   R6,TOT2                                                          
         BAS   RE,MYRPT            SKIP A LINE                                  
         MVC   P1+17(13),=C'TOTAL RECORDS'                                      
         EDIT  TOTCNT,(9,P1+40),0,COMMAS=YES                                    
         MVI   P1+49,C'*'                                                       
         BAS   RE,MYRPT                                                         
*                                                                               
         CLI   MISSERR,C'Y'          BILLS FOR SKIPPED PRDS FOUND               
         BNE   TOT3                                                             
         MVC   P1(42),=C'*** WARNING - BILLS FOR SKIPPED PRDS FOUND'            
         MVC   P2(34),=C'*** THEY ARE NOT INCLUDED IN TOTALS'                   
         BAS   RE,MYRPT                                                         
*                                                                               
TOT3     CLI   TAPESW,C'Y'          SEE IF PRODUCING INV. TAPE                  
         BNE   TOT5                                                             
         CLOSE (PPZTAPE,)                                                       
*                                                                               
TOT5     B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PROCESS  DS    0H       PROCESS BILL                                            
*                                                                               
         CP    PBILLRCV,=P'0'      NON ZERO  PROCESS                            
         BNE   PRB05                                                            
         B     EXIT                SKIP IF ZERO                                 
*                                                                               
PRB05    TM    PBILCMSW,X'02'      IS IT A COMMISION ONLY BILL                  
         BNZ   PRB09               YES, OK                                      
         CLI   QOPT5,C'C'          NO, ARE WE SKIPPING OTHERS                   
         BE    EXIT                                                             
         B     PRB09A                                                           
*                                                                               
PRB09    DS    0H                                                               
         CLI   QOPT5,C'N'          EXCLUDING COMMISSION ONLY?                   
         BE    EXIT                                                             
PRB09A   DS    0H                                                               
*                                                                               
         CLI   QOPT5,C' '            CHECK AOR OPTION                           
         BE    PRB09D                                                           
         CLI   QOPT5,C'A'            ONLY AOR                                   
         BNE   PRB09B                                                           
         TM    PBILCMSW,X'20'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09B   CLI   QOPT5,C'B'            AOR AND AOR/CLIENT                         
         BNE   PRB09C                                                           
         TM    PBILCMSW,X'30'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09C   CLI   QOPT5,C'X'            SEE IF EXCLUDING AOR                       
         BNE   PRB09D                                                           
         TM    PBILCMSW,X'20'                                                   
         BO    EXIT                                                             
*                                                                               
PRB09D   DS    0H                                                               
*                                                                               
         CLC   PBILLDAT,QSTART     DATES FILTER                                 
         BL    EXIT                                                             
         CLI   QEND,C' '                                                        
         BE    PRB09D5                                                          
         CLC   PBILLDAT,QEND                                                    
         BH    EXIT                                                             
*                                                                               
PRB09D5  DS    0H                   MUST BE SURE ESTIMATE IS THERE              
         MVC   WORK(12),PBILLREC                                                
         MVI   WORK+3,7                                                         
         CLC   PESTREC(12),WORK        TEST HAVE EST HDR                        
         BE    PRB09D6             YES                                          
         MVC   WORK(64),KEY        SAVE KEY FOR SEQ                             
         XC    KEY,KEY                                                          
         MVC   KEY(12),PBILLREC                                                 
         MVI   KEY+3,7                                                          
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PRB09D6  DS    0H                                                               
*                                                                               
         XC    ESTU1,ESTU1       CLEAR EST USER FIELDS                          
         XC    ESTU2,ESTU2                                                      
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'E',ADEST),(C':',ESTU1),(C'+        
               :',ESTU2)                                                        
*                                                                               
PRB09DX  GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*****                                                                           
*****    SET EFFECTIVE VALUES INTO PBILLREC                                     
*****                                                                           
         MVC   PBILLGRS,PPBVEBG                                                 
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
*                                 SET MYGST AND MYPST AND MYHST                 
*****    ZAP   MYGST,=P'0'                                                      
*****    ZAP   MYPST,=P'0'                                                      
         L     R0,PPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYGST,DUB                                                        
         L     R0,PPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
         L     R0,PPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYHST,DUB                                                        
*                                                                               
         CLI   SKIPPRD,C'N'                                                     
         BE    PRB20                                                            
         MVC   P1(43),=C'*** WARNING - SKIPPED BILL ENCOUNTERED ***'            
         BAS   RE,MYRPT                                                         
         MVI   MISSERR,C'Y'                                                     
         B     EXIT                                                             
*                                                                               
PRB20    GOTO1 =A(BLDREC)                                                       
         B     EXIT            DONE WITH THIS BILL                              
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE1,0(R2)                                                    
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
FNDAAA   NTR1                                                                   
         MVC   APRDBILL(124),SPACES                                             
*                                                                               
         MVC   PPGKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(7),PCLTKAGY     AGY/MED/CODE/CLT                             
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+7(3),=C'AAA'                                                 
         CLC   PPRDKAGY(7),KEY                                                  
         BE    FNDP10                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    FNDP08                                                           
         B     FNDAX      PRODUCT AAA NOT ON FILE                               
*                                                                               
FNDP08   LA    RE,PPRDREC                                                       
         ST    RE,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
FNDP10   MVC   APRDBILL,PPRDBILL                                                
         MVC   APRDBIL2,PPRDBIL2                                                
         MVC   APRDLIN1,PPRDLIN1                                                
         MVC   APRDLIN1,SPACES    NOT USED FOR PFIZER NEXTEL                    
*                                 SINCE LINE ONE IS A P.O. BOX                  
         MVC   APRDLIN2,PPRDLIN2                                                
         MVC   APRDATTN,PPRDATTN                                                
*                                                                               
FNDAX    MVC   KEY,PPGKEY                                                       
         GOTO1 HIGH                                                             
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*        AGENCY TABLE                                                           
*                                                                               
*        AGENCY CODE/VENDOR/COMPANY/MYUSER VALUE                                
*                                                                               
*                                                                               
AGYTAB   DC    C'UB',C'Y'        CARAT                                          
         DC    C'SJ',C'Y'        SJR                                            
         DC    X'FFFF'                                                          
*                                                                               
AGYTABL  EQU   3                                                                
         EJECT                                                                  
*                                                                               
         DS    1500C   SO THAT MYPRT AND MWRITE CAN BE ACCESSED                 
         PRINT GEN                                                              
PPZTAPE  DCB   DDNAME=PPZTAPE,DSORG=PS,RECFM=VB,MACRF=PM,BLKSIZE=0              
*PZTAPE  DCB   DDNAME=PPZTAPE,DSORG=PS,RECFM=VB,LRECL=00600,                    
*              BLKSIZE=06000,MACRF=PM                                           
         PRINT NOGEN                                                            
*                      BY OTHER ROUTINES                                        
MYRPT    NTR1                                                                   
*                                                                               
         CLI   QOPT6,C'Y'              SEE IF TEST RUN                          
         BE    MYRPT2                                                           
         CLI   SVQOPT6,C'Y'            SEE IF TEST RUN                          
         BNE   *+10                                                             
*                                                                               
MYRPT2   MVC   HEAD4+50(12),=C'**TEST RUN**'                                    
*****    CLI   MODE,RUNLAST                                                     
*****    BNE   *+10                                                             
         MVC   QSTART(12),SVSTART                                               
*                                                                               
MYRPT5   GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
*                          AND BLDREC AS IT'S NOW COVERED BY R7 - THE           
*                          SECOND BASE REGISTER                                 
         EJECT                                                                  
MWRITE   NTR1                      FIND RECORD LENGHT IN LENTAB                 
*                                                                               
         L     R1,ALENTAB                                                       
MWRITE4  CLC   0(2,R1),RECTYPE                                                  
         BE    MWRITE5                                                          
         LA    R1,8(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   MWRITE4                                                          
         DC    H'0'                UNKNOWN TYPE                                 
*                                                                               
MWRITE5  MVC   HALF,2(R1)                                                       
         AP    4(4,R1),=P'1'                                                    
**FB**   LH    R3,HALF                                                          
**FB**   LA    R3,4(R3)                                                         
**FB**   STH   R3,OUTREC-4                                                      
         CLI   SVQOPT7,C'P'                                                     
         BE    WRIT1                                                            
         CLI   QOPT7,C'P'                                                       
         BNE   WRIT2                                                            
WRIT1    MVI   RCSUBPRG,10                                                      
         MVC   P1+1(125),OUTREC                                                 
         MVC   P2+1(125),OUTREC+125                                             
         MVC   P3+1(125),OUTREC+250                                             
         MVC   P4+1(125),OUTREC+375                                             
         MVC   P5+1(100),OUTREC+500                                             
         MVI   P2,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P3,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P4,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P5,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P6,0       SO LINE WILL ALWAYS PRINT                             
*                                                                               
WRIT1A   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
         MVI   RCSUBPRG,10                                                      
*        GOTO1 PRNTBL,DMCB,=C'OUTREC',OUTREC,C'DUMP',OUTRECL,=C'1D'             
*                                                                               
         GOTO1 HEXOUT,DMCB,OUTREC-4,P1+10,54,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+50,P2+18,50,=C'N'                               
         GOTO1 (RF),(R1),OUTREC+100,P3+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+150,P4+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+200,P5+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+250,P6+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+300,P7+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+350,P8+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+400,P9+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+450,P10+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+500,P11+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+550,P12+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+600,P13+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+650,P14+18,50,=C'N'                             
WRIT1C   MVC   P1+1(7),=C'001-050'                                              
         MVC   P2+1(7),=C'051-100'                                              
         MVC   P3+1(7),=C'101-150'                                              
         MVC   P4+1(7),=C'151-200'                                              
         MVC   P5+1(7),=C'201-250'                                              
         MVC   P6+1(7),=C'251-300'                                              
         MVC   P7+1(7),=C'301-350'                                              
         MVC   P8+1(7),=C'351-400'                                              
         MVC   P9+1(7),=C'401-450'                                              
         MVC   P10+1(7),=C'451-500'                                             
         MVC   P11+1(7),=C'501-550'                                             
         MVC   P12+1(7),=C'551-600'                                             
         MVC   P13+1(7),=C'601-650'                                             
         MVC   P14+1(7),=C'651-700'                                             
*                                                                               
WRIT1E   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   SVQOPT6,C'Y'     SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LA    R1,PPZTAPE                                                       
*                                                                               
WRIT2B   LA    R0,OUTREC-4                                                      
         PRINT GEN                                                              
         PUT   (1),(0)                                                          
         PRINT NOGEN                                                            
WRIT3    AP    TOTCNT,=P'1'                                                     
         XIT1                                                                   
         LTORG                                                                  
*                         CREATE COLUMN HEADINGS RECORD                         
COLREC   CSECT                                                                  
         NMOD1 0,COLREC                                                         
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8 AND R9                              
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
         MVC   OUTREC-4(2),=H'4'    STARTING RECORD LENGTH                      
         LA    RE,OUTREC           CLEAR OUTREC                                 
         LHI   RF,OUTRECL                                                       
         XCEF                                                                   
         MVC   OUTREC(45),=C'"UTF-8 ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,X        
               ,,,"'                                                            
         MVC   OUTREC-4(2),=H'49'   RECORD LENGTH                               
         MVC   RECTYPE,=C'H1'       FIRST HEADER LINE                           
         BAS   RE,MWRITE                                                        
*                                                                               
         MVC   OUTREC(101),=C'"_csv_version:1.0","_csv_serial:156804047X        
               2512","_csv_type:invoice",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,X        
               ,,,,"'                                                           
         MVC   OUTREC-4(2),=H'105'  RECORD LENGTH                               
         MVC   RECTYPE,=C'H2'       next HEADER LINE                            
         BAS   RE,MWRITE                                                        
*                                                                               
         MVC   OUTREC-4(2),=H'4'    STARTING RECORD LENGTH                      
         LA    RE,OUTREC           CLEAR OUTREC                                 
         LHI   RF,OUTRECL                                                       
         XCEF                                                                   
*                                                                               
         L     RF,=A(COLHDS)                                                    
         MVC   OUTREC(250),0(RF)                                                
         MVC   OUTREC+250(250),250(RF)                                          
         MVC   OUTREC+500(COLHDLEN-500),500(RF)                                 
         LH    RF,OUTREC-4                                                      
         LA    RF,COLHDLEN(RF)                                                  
         STH   RF,OUTREC-4                                                      
         MVC   RECTYPE,=C'CH'      COLUMN HEADINGS                              
         BAS   RE,MWRITE                                                        
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                         CREATE FILE RECORD                                    
BLDREC   CSECT                                                                  
         NMOD1 0,BLDREC                                                         
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8 AND R9                              
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
         CLI   COLSW,C'Y'       COLUMN HEADINGS SENT?                           
         BE    BLDR1                                                            
         GOTO1 =A(COLREC)                                                       
         MVI   COLSW,C'Y'        SO I WON'T REDO                                
*                                                                               
BLDR1    DS    0H                                                               
         MVC   RECTYPE,=C'IR'       BILLING REC                                 
*                                                                               
         MVC   OUTREC-4(2),=H'4'    STARTING RECORD LENGTH                      
         LA    RE,OUTREC           CLEAR OUTREC                                 
         LHI   RF,OUTRECL                                                       
         XCEF                                                                   
*                                                                               
         LA    R2,OUTREC                                                        
*                                                                               
         GOTO1 =V(PPFMTINO),DMCB,PBILLDAT,(2,PBILKBNO),                X        
               (PBILKMED,B1PROF),B1XPROF                                        
*                                                                               
         MVC   DINVFULL,SPACES                                                  
         L     RF,DMCB                                                          
         LA    R1,DINVFULL                                                      
         CLI   0(RF),C' '       SKIP LEADING BLANK                              
         BH    BLDI1                                                            
         LA    RF,1(RF)                                                         
         LA    R3,9                                                             
         B     BLDI2                                                            
*                                                                               
BLDI1    LA    R3,10                                                            
BLDI2    MVC   0(1,R1),0(RF)                                                    
         B     BLDI5                                                            
*                                                                               
BLDI5    LA    R1,1(R1)                                                         
BLDI8    LA    RF,1(RF)                                                         
         BCT   R3,BLDI2                                                         
         MVI   0(R2),C'"'                                                       
         MVC   1(10,R2),DINVFULL                                                
         LA    R2,11(R2)                                                        
BLDI9    CLI   0(R2),C' '                                                       
         BH    BLDI17                                                           
         SH    R2,=H'1'                                                         
         B     BLDI9                                                            
*                                                                               
BLDI17   MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         GOTO1 DATCON,DMCB,(3,PBILINVD),(X'20',WORK)                            
         MVC   1(2,R2),WORK+2    MM                                             
         MVI   3(R2),C'/'                                                       
         MVC   4(2,R2),WORK+4    DD                                             
         MVI   6(R2),C'/'                                                       
         MVC   7(2,R2),=C'20'    CENTRY 2000                                    
         MVC   9(2,R2),WORK+0    YY                                             
         MVC   11(2,R2),=C'",'                                                  
         LA    R2,13(R2)                                                        
*                                                                               
*        TaxInLine, SpHandlingInLine, ShippingInLine set to N                   
*                                                                               
         MVC   0(12,R2),=C'"Y","Y","Y",'                                        
         LA    R2,12(R2)                                                        
*                                                                               
*        PO#, invoice line,company code,cost center,project#,                   
*        account code, service desc., funtion,task,item comment,                
*        unit price, quantity, item subtotal,unit of measure,currency,          
*        po line number                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         CLI   PUCOMM1,0    ANY DATA?                                           
         BE    BLDI19X                                                          
         ZIC   RE,PUCOMM1   LENGTH OF UCOM P1                                   
         SH    RE,=H'1'     DECREMENT FOR EX                                    
         EX    RE,BLDI19                                                        
         B     *+10                                                             
*                                                                               
BLDI19   MVC   1(0,R2),PUCOMM1+1     DATA                                       
         AR    R2,RE                                                            
         LA    R2,1(R2)                                                         
BLDI19X  MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
*        6 EMPTY FIELDS                                                         
         MVC   0(18,R2),=C'"","","","","","",'                                  
         LA    R2,18(R2)                                                        
*                                                                               
*        shipToCountry                                                          
         MVC   0(05,R2),=C'"US",'                                               
         LA    R2,5(R2)                                                         
*                                                                               
*        5 EMPTY FIELDS                                                         
         MVC   0(15,R2),=C'"","","","","",'                                     
         LA    R2,15(R2)                                                        
*                                                                               
*        shipFromCountry                                                        
         MVC   0(05,R2),=C'"US",'                                               
         LA    R2,5(R2)                                                         
*                                                                               
*        3 EMPTY FIELDS                                                         
         MVC   0(9,R2),=C'"","","",'                                            
         LA    R2,9(R2)                                                         
*                                                                               
         MVC   0(04,R2),=C'"1",'         INVOICE LINE                           
         LA    R2,4(R2)                                                         
         MVC   0(03,R2),=C'"",'      BLANK COMPANY CODE                         
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         CLI   PUCOMM4,0    ANY DATA?                                           
         BE    BLDI20X                                                          
         ZIC   RE,PUCOMM4   LENGTH OF UCOM P4   COST CENTER                     
         SH    RE,=H'1'     DECREMENT FOR EX                                    
         EX    RE,BLDI20                                                        
         B     *+10                                                             
*                                                                               
BLDI20   MVC   1(0,R2),PUCOMM4+1     DATA                                       
         AR    R2,RE                                                            
         LA    R2,1(R2)                                                         
BLDI20X  MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         CLI   PUCOMM2,0    ANY DATA?                                           
         BE    BLDI21X                                                          
         ZIC   RE,PUCOMM2   LENGTH OF UCOM P2   PROJECT#                        
         SH    RE,=H'1'     DECREMENT FOR EX                                    
         EX    RE,BLDI21                                                        
         B     *+10                                                             
*                                                                               
BLDI21   MVC   1(0,R2),PUCOMM2+1     DATA                                       
         AR    R2,RE                                                            
         LA    R2,1(R2)                                                         
BLDI21X  MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
         MVI   0(R2),C'"'                                                       
         CLI   PUCOMM3,0    ANY DATA                                            
         BE    BLDI22X                                                          
         ZIC   RE,PUCOMM3   LENGTH OF UCOM P3  ACCOUNT CODE                     
         SH    RE,=H'1'     DECREMENT FOR EX                                    
         EX    RE,BLDI22                                                        
         B     *+10                                                             
*                                                                               
BLDI22   MVC   1(0,R2),PUCOMM3+1     DATA                                       
         AR    R2,RE                                                            
         LA    R2,1(R2)                                                         
BLDI22X  MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
*                                                                               
***      SERVICE DESCRIPTION                                                    
***                                                                             
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
BLDC1    BAS   RE,GETMEDN     MEDIA NAME RETURNED IN WORK                       
         MVC   0(12,R2),WORK                                                    
         LA    R2,12(R2)                                                        
BLDC2    CLI   0(R2),C' '     SCAN BACKWORDS FOR END                            
         BH    BLDC5                                                            
         BCT   R2,BLDC2                                                         
*                                                                               
BLDC5    MVC   1(2,R2),=C'",'                                                   
         LA    R2,3(R2)                                                         
         MVI   0(R2),C'"'     FUNCTION = MOS                                    
         LA    R2,1(R2)                                                         
         GOTO1 DATCON,DMCB,(3,PBILKMOS),(6,0(R2))  MOS                          
         MVC   6(2,R2),=C'",'                                                   
         LA    R2,8(R2)           PAST MMM/YY                                   
*                                                                               
         MVI   0(R2),C'"'     TASK = MOS ALSO                                   
         LA    R2,1(R2)                                                         
         GOTO1 DATCON,DMCB,(3,PBILKMOS),(6,0(R2))  MOS                          
         MVC   6(2,R2),=C'",'                                                   
         LA    R2,8(R2)           PAST MMM/YY                                   
*                                                                               
         MVC   0(3,R2),=C'"",' EMPTY ITEM COMMENT                               
         LA    R2,3(R2)                                                         
*                                                                               
         ZAP   DUB,PBILLRCV       AMT DUE                                       
         CVB   RF,DUB                                                           
         ST    RF,MYFULL                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         LA    R2,1(R2)                                                         
         L     RF,MYFULL                                                        
         EDIT  (B4,MYFULL),(14,0(R2)),2,ALIGN=LEFT                              
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
*                                                                               
         L     RF,MYFULL                                                        
         MVC   0(4,R2),=C'"1",'  LINE ITEM QUANITY                              
         C     RF,=F'0'                                                         
         BNL   BLDI24            SEE IF NEGATIVE                                
         MVC   0(5,R2),=C'"-1",'                                                
         LA    R2,5(R2)                                                         
         B     BLDI25                                                           
*                                                                               
BLDI24   LA    R2,4(R2)                                                         
BLDI25   DS    0H                                                               
         MVI   0(R2),C'"'          ITEM SUBTOTAL - SAME AS DUE                  
         LA    R2,1(R2)                                                         
         EDIT  (B4,MYFULL),(14,0(R2)),2,ALIGN=LEFT,FLOAT=-                      
         AR    R2,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R2),=C'",'                                                   
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(05,R2),=C'"AU",'                                               
         LA    R2,05(R2)                                                        
         MVC   0(06,R2),=C'"USD",'                                              
         LA    R2,06(R2)                                                        
         MVC   0(03,R2),=C'"1"'                                                 
         LA    R2,3(R2)                                                         
*                                                                               
*        7 EMPTY FIELDS                                                         
*                                                                               
         MVC   0(21,R2),=C',"","","","","","",""'                               
         LA    R2,21(R2)                                                        
         B     BLDREND                                                          
*                                                                               
BLDREND  DS    0H                                                               
         LA    RE,OUTREC-4                                                      
         LR    RF,R2                                                            
         SR    RF,RE      DIFFERENCE SHOULD BE RECORD LENGTH                    
         STH   RF,OUTREC-4                                                      
         BAS   RE,MWRITE                                                        
         B     BLDRX                                                            
         EJECT                                                                  
GETMEDN  DS    0H              BUILD COMMENT LINE                               
*                              NOW JUST MEDIA NAME                              
         MVC   WORK(12),SPACES                                                  
         L     RF,ADAGY                                                         
         MVC   WORK(L'PAGYMED),PAGYMED-PAGYREC(RF)                              
         BR    RE                                                               
*****                                                                           
*****    CODE BELOW FLOATS PRODUCT NAME AFTER CODE                              
*****    AND ESTIMATE NAME AFTER CODE                                           
*****    MVC   WORK+7(20),PPRDNAME                                              
*****    OC    WORK+7(20),SPACES                                                
*****    LA    R1,WORK+30                                                       
***CM5   CLI   0(R1),C' '                                                       
*****    BH    GETCM10                                                          
*****    BCT   R1,GETCM5                                                        
*****                                                                           
***CM10  MVC   HALF,PBILKEST                                                    
****     LH    R0,HALF                                                          
****     CVD   R0,DUB                                                           
****     OI    DUB+7,X'0F'                                                      
****     UNPK  2(3,R1),DUB+6(2)                                                 
****     MVC   6(20,R1),PESTNAME                                                
****     OC    6(20,R1),SPACES                                                  
         BR    RE             RETURN                                            
         EJECT                                                                  
*                                                                               
*        ROUTINE TO INCREMENT RECORD LENGTH                                     
*        AND SET R2 TO BEGINNING OF NEXT FIELD                                  
*                                                                               
ADDLEN   LH    RF,OUTREC-4                                                      
         AR    RF,R1                                                            
         AH    RF,=H'1'      FOR THE NEXT COMMA                                 
         STH   RF,OUTREC-4                                                      
         LA    R2,OUTREC-4                                                      
         AR    R2,RF         POINT R2 TO NEXT FIELD                             
         MVI   0(R2),C','    FIELD DELIMITER                                    
         LA    R2,1(R2)      PAST COMMA                                         
         BR    RE            RETURN                                             
*                                                                               
BLDRX    XMOD1                                                                  
         LTORG                                                                  
*                           PPBYOUT WORKAREA                                    
         DS    0D                                                               
         DC    CL8'PPBYOWRK'                                                    
PPBYOWRK DS    600C                                                             
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
GETMARK  NTR1                                                                   
         L     R6,ELADDR                                                        
         USING PBILELEM,R6                                                      
         MVC   WORK(30),SPACES                                                  
         B     GETMKX                   NO MARKET FOR MINDSHARE                 
*                                                                               
***      MVC   WORK(9),=CL9'CORPORATE'                                          
***      CLC   PBPRD,=C'CON'                                                    
***      BE    GETMKX                                                           
***      CLC   PBPRD,=C'SAM'                                                    
***      BE    GETMKX                                                           
***      CLC   PBPRD,=C'SYO'                                                    
***      BE    GETMKX                                                           
***                                                                             
***      CLC   PBPRD,=C'BIZ'                                                    
***      BE    GETMK5                                                           
***      CLC   PBPRD,=C'PCS'                                                    
***      BE    GETMK5                                                           
***      B     GETMKX          ANY OTHERS GET CORPORATE                         
***                                                                             
***MK5   MVC   WORK(9),=CL9'NATIONAL'                                           
***      CLI   QMEDIA,C'M'                                                      
***      BE    GETMKX                                                           
***      CLI   QMEDIA,C'T'                                                      
***      BE    GETMKX                                                           
***      CLI   QMEDIA,C'S'                                                      
***      BE    GETMKX                                                           
***      CLI   QMEDIA,C'I'                                                      
***      BE    GETMKX                                                           
***      IF    QMEDIA,EQ,C'O',AND,PBPRD,EQ,=C'BIZ',GETMKX                       
***                                                                             
***                                                                             
***      ALL NEWSPAPER PRODUCTS USE THE DISTRICT NAME                           
***      OTHER OUTDOOR PRODUCTS USE THE REGION NAME                             
***                                                                             
***      NOTE: REQUEST BY DIV/REG FOR OUTDOOR ND NEWSPAPERS                     
***            AND DIV/REG/DST FOR NEWSPAPERS                                   
***                                                                             
***      MVC   WORK(20),PDSTNAME                                                
***      OC    WORK(20),SPACES     JUST IN CASE                                 
***      CLI   QMEDIA,C'N'                                                      
***      BE    GETMK20                                                          
***      MVC   WORK(20),PREGNAME    OUTDOOR USES REGIONS                        
***      OC    WORK(20),SPACES     JUST IN CASE                                 
***      B     GETMK20                                                          
***                                                                             
***MK20  CLC   WORK(06),=C'MASTER'   IF MASTER REGION/DISTRICT                  
***      BNE   GETMKX                (UNASSIGNED) MUST CHANGE NAME              
***      MVC   WORK(20),=CL20'UNASSIGNED'                                       
***                                                                             
GETMKX   XIT1                                                                   
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
TITLES   CSECT                                                                  
         DS    0C                                                               
         DC    CL30'           FIRST HEADER'                                    
         DC    CL30'          SECOND HEADER'                                    
         DC    CL30'         COLUMN HEADERS '                                   
         DC    CL30'        INVOICE RECORDS'                                    
*                                                                               
         EJECT                                                                  
*             TABLE OF RECORD TYPES,LENGTHS AND COUNTS                          
LENTAB   CSECT                                                                  
         DC    CL2'H1',AL2(0),PL4'0'                                            
         DC    CL2'H2',AL2(0),PL4'0'                                            
         DC    CL2'CH',AL2(0),PL4'0'                                            
         DC    CL2'IR',AL2(0),PL4'0'                                            
         DC    XL4'00',PL4'0'      EXTRA LINE                                   
         DC    X'FFFF'                                                          
         EJECT                                                                  
PPPZWRKD DSECT                                                                  
NETOPT   DS    CL1                                                              
SVQOPT1  DS    CL1          SO I'LL KNOW AT RUNLAST WHICH FILE TYPE             
SVQOPT6  DS    CL1          SO I'LL KNOW AT RUNLAST IF TEST RUN                 
SVQOPT7  DS    CL1          SO I'LL KNOW AT RUNLAST IF PDUMPING                 
*                                                                               
MYCD     DS    PL6                                                              
*                                                                               
EHDRSW   DS    CL1        GETS SET TO Y WHEN EST FILE HEADER WRITTEN            
IHDRSW   DS    CL1        GETS SET TO Y WHEN INVOICE FILE HDR WRITTEN           
ERRSW    DS    CL1                                                              
COLSW    DS    CL1          Y IF COLUMN HEADERS SENT                            
*                                                                               
SKIPPRD  DS    CL1                                                              
MISSERR  DS    CL1         Y IF BILLS FOR SKIPPED PRD FOUND                     
*                                                                               
ELADDR   DS    A            ADDRESS OF BILLING ELEMENT                          
SAVER0   DS    F                                                                
SAVER1   DS    F                                                                
SAVERE   DS    F                                                                
SAVERF   DS    F                                                                
*                                                                               
RECTYPE  DS    CL2          USED BY MWRITE                                      
*                                                                               
REQUSER  DS    CL10         SET FROM VENTAB                                     
*                                                                               
SVPRDIFC DS    CL4          SAVED FROM PRD INTERFACE CODE ELEMENT               
TOTCNT   DS    PL4'0'                                                           
MYDUB    DS    PL8                                                              
MYDUB2   DS    PL8                                                              
NETDUB   DS    PL8                                                              
CDDUB    DS    PL8                                                              
SDUB     DS    PL8          USED FOR SHARES IN PROCNET                          
SNETDUB  DS    PL8          USED FOR SHARES IN PROCNET                          
SVNUPRD2 DS    XL1          SAVED NUPRD2                                        
*                                                                               
WRKBDATE DS    CL10                                                             
MYDUMP   DS    XL2                                                              
         DS    0H           ALIGN                                               
WK       DS    XL100                                                            
*                                                                               
ALLOWSW  DS    XL1                                                              
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
         DS    0F          ALIGNMENT FOR WK                                     
*                                                                               
MYFULL   DS    F                                                                
MYGST    DS    PL6                                                              
MYHST    DS    PL6                                                              
MYPST    DS    PL6                                                              
WPRD     DS    XL1                                                              
*                                                                               
WRKDATE  DS    XL8                                                              
*                                                                               
PRDCITY  DS    CL30        EXTRACTED FROM PPRDLIN2                              
PRDST    DS    CL30                                                             
PRDZIP   DS    CL30                                                             
*                                                                               
*                                                                               
APRDBILL DS    CL20        SAVED FROM PRD AAA                                   
APRDBIL2 DS    CL20        SAVED FROM PRD AAA                                   
APRDLIN1 DS    CL30        SAVED FROM PRD AAA                                   
APRDLIN2 DS    CL30        SAVED FROM PRD AAA                                   
APRDATTN DS    CL24        SAVED FROM PRD AAA                                   
*                                                                               
BPRDBILL DS    CL20        SAVED FROM BRAND                                     
BPRDBIL2 DS    CL20        SAVED FROM BRAND                                     
BPRDLIN1 DS    CL30        SAVED FROM BRAND                                     
BPRDLIN2 DS    CL30        SAVED FROM BRAND                                     
BPRDATTN DS    CL24        SAVED FROM BRAND                                     
*                                                                               
MYBEST   DS    H           FROM QEST                                            
MYBESTE  DS    H           FROM QESTEND                                         
*                                                                               
AAAPRD   DS    CL1         =Y IF WE HAVE PRD=AAA                                
PRDFORMU DS    CL5         BILLING FORMULA FOR REGULAR PROD                     
ESTFORMU DS    CL5         BILLING FORMULA FOR REGULAR EST                      
AAAFORMU DS    CL5         BILLING FORMULA FOR AAA     PROD                     
AAAESTFR DS    CL5         BILLING FORMULA FOR AAAEST                           
BILLFORM DS    CL5         DEFAULT VALUE FOR FORMULA                            
*                                                                               
B1PROF   DS    CL16                                                             
B1XPROF  DS    CL16                                                             
*                                                                               
DINVFULL DS    CL10                                                             
DINVNO   DS    CL6      SHORT FORMAT                                            
*                                                                               
LINVFULL DS    CL10     SAVED                                                   
LBQDATE  DS    CL6      SAVED                                                   
*                                                                               
INVTOTD  DS    PL5      TOTAL $ FOR INV NUMBER                                  
LASTBILL DS    CL1      Y= LAST BILL                                            
FRSTBILL DS    CL1      Y= FIRST BILL                                           
INVRCNT  DS    PL5      COUNT OF INVOICE FILE RECORDS                           
*                                                                               
       ++INCLUDE PPBVALD                                                        
*                                                                               
WORK2    DS    CL64                                                             
ALENTAB  DS    A                                                                
ATITLES  DS    A                                                                
VGETUSER DS    A                                                                
VGETCOST DS    A                                                                
VDDUCOM  DS    A                                                                
AFMTINO  DS    A                                                                
APERVAL  DS    A                                                                
APPBVAL  DS    A                                                                
ACONIO1  DS    A                                                                
ANXTINV  DS    A                                                                
BUFFIO   DS    A                                                                
BUFFBUFF DS    A                                                                
AREGTAB  DS    A                                                                
ANXTREG  DS    A                                                                
AREGTABX DS    A                                                                
*                                                                               
MYUSER   DS    CL1       SET FROM AGYTAB AT FBUYREQ                             
SVSTART  DS    CL6                                                              
SVEND    DS    CL6                                                              
*                                                                               
ESTU1    DS    CL54      USER FIELDS                                            
ESTU2    DS    CL38                                                             
PRDU1    DS    CL54                                                             
PRDU2    DS    CL38                                                             
*                                                                               
PUCOMM1  DS    CL33      FIRST BYTE IS LENGTH NEXT 32 DATA                      
PUCOMM2  DS    CL33      FIRST BYTE IS LENGTH NEXT 32 DATA                      
PUCOMM3  DS    CL33      FIRST BYTE IS LENGTH NEXT 32 DATA                      
PUCOMM4  DS    CL33      FIRST BYTE IS LENGTH NEXT 32 DATA                      
*                                                                               
MYBILLCD DS    PL8                                                              
MYBILLGR DS    PL8                                                              
*                                                                               
CINVGRS  DS    PL8        CURRENT INVOICE TOTALS                                
CINVBIL  DS    PL8                                                              
CINVCD   DS    PL8                                                              
CINVRCV  DS    PL8                                                              
CINVSW   DS    CL1                                                              
*                                                                               
ADDDEL   DS    CL1                 X'01' IF ADDED + DELETED IN PERIOD           
BILLONLY DS    CL1                 SET IN CKEST Y= BILL ONLY ESTIMATE           
*                                  (NO CHANGES)                                 
SAVMED   DS    CL2               USED IN MWRITE TO SAVE 'REAL' MEDIA            
CKESTREC DS    CL1                                                              
LBILLKEY DS    CL12           KEY OF LAST BILL READ                             
LESTOUT  DS    CL12           KEY OF LAST ESTIMATE OUTPUT                       
*                                                                               
ETOTSW   DS    CL1                                                              
ESTCD    DS    PL8                 ESTIMATE TOTALS FOR PRINTING                 
ESTAMTD  DS    PL8                                                              
ESTCOMM  DS    PL8                                                              
*                                                                               
GTTOTCD  DS    PL8                 REPORT TOTALS                                
GTTOTAMT DS    PL8                                                              
GTTOTCOM DS    PL8                                                              
*                                                                               
TODAY1   DS    CL6                                                              
TODAYY   DS    CL8         YYYYMMDD                                             
NOW      DS    CL6         TIME HHMMSS                                          
ELCODE1  DS    CL1                                                              
SAVCGR   DS    PL8                                                              
*                                                                               
TAPESW   DS    CL1         STARTS AS 0 CHANGED TO N OR Y AT FBUYCLI             
ETAPESW  DS    CL1         STARTS AS 0 CHANGED TO N OR Y AT FBUYCLI             
*                          MIX NOT ALLOWED                                      
*                                                                               
CIRCDAT  DS    CL3                                                              
TRCODE   DS    CL1                                                              
PPGKEY   DS    CL32                                                             
PPGAREC  DS    CL4                                                              
ZEROS    DS    CL30                                                             
LASTEST  DS    CL9          USED TO CHECK CHANGE OF EST                         
LASTEVY  DS    CL4          VALIDITY YEAR                                       
LASTEGL  DS    CL10         GL ACCOUNT                                          
LASTEIO  DS    CL12         IO NUMBER                                           
LASTECC  DS    CL10         COST CENTER                                         
*                           WHEN READING BUFFALO RECS                           
APPBYOWK DS    A                                                                
*                                                                               
BADESTS  DS    CL240            ROOM FOR 40 BAD PRD/ESTS                        
*                                PRD(3)/EST(2)/+ ONE BYTE                       
*                                TO BE USED FOR ERRORS                          
         DS    F                                                                
INVTAB   DS    CL252        ROOM FOR 18 MOS X14                                 
*                           ENTRIES ARE MOS (MY)                                
*                                       BACTP  (ACTUAL AMT DUE)                 
*                                       COMMISSION (BACTP-BNETP)                
INVLEN   EQU   14                                                               
*                                                                               
BUFREC   DS    0CL146                                                           
BUFKEY   DS    0CL22                                                            
BUFTYPE  DS    CL1                 E=ESTIMATE                                   
*                                                                               
BUFMED   DS    CL1                 MEDIA                                        
BUFPRD   DS    CL3                 PRODUCT                                      
BUFYR    DS    CL1                 LAST DIGIT OF YEAR                           
BUFEST   DS    CL3                 EST                                          
BUFMTH   DS    CL6                 MONTH  YYYYMM                                
BUFPUB   DS    CL6                 PUB  (EMPTY FOR EST DETAIL)                  
         DS    CL1                 SPARE                                        
*                                                                               
BUFCOM   DS    0CL100               COMMENT                                     
BUFCOMEN DS    CL20                ESTIMATE NAME                                
BUFCOME1 DS    CL4                 VALIDITY YEAR                                
BUFCOME2 DS    CL10                GL ACCOUNT                                   
BUFCOME3 DS    CL12                INTERNAL ORDER                               
BUFCOME4 DS    CL10                COST CENTER                                  
BUFCOMPN DS    CL20                PUB NAME                                     
BUFCOMPZ DS    CL20                PUB ZONE NAME                                
         DS    CL4                 SPARE                                        
*                                                                               
*                                                                               
BUFCD    DS    PL8                 CASH DISCOUNT                                
BUFAMTD  DS    PL8                 AMOUNT DUE (CALC. WITH BILL. FORM.)          
BUFCOMM  DS    PL8                 COMMISSION (DUE-NET)                         
         ORG                                                                    
*                                                                               
OUTREC   DS    CL800                                                            
OUTRECL  EQU   *-OUTREC                                                         
         DS    0F               FOR ALIGNMENT                                   
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
*                                                                               
UCALL    EQU   *-UCTTLS                                                         
USAVKEY  DS    XL32             TO SAVE CURRENT READ SEQUENCE                   
UCOMQ    EQU   *-UCOMBLK                                                        
*                                                                               
         DS    F                                                                
*                                                                               
*                                                                               
         DS    CL50             SPARE                                           
*                                                                               
         BUFF  LINES=4000,ROWS=1,COLUMNS=3,FLAVOR=PACKED,COMMENT=100,KEX        
               YLIST=(22,A)                                                     
*                                                                               
COLHDS   CSECT                                                                  
*                                                                               
*        HEADER COLUMN HEADINGS                                                 
*                                                                               
         DC    C'"invoiceID",'                                                  
         DC    C'"invoiceDate",'                                                
         DC    C'"isTaxInLine",'                                                
         DC    C'"isSpecialHandlingInLine",'                                    
         DC    C'"isShippingInLine",'                                           
         DC    C'"poNumber",'                                                   
         DC    C'"supplierTaxID",'                                              
         DC    C'"shipToName",'                                                 
         DC    C'"shipToStreet",'                                               
         DC    C'"shipToCity",'                                                 
         DC    C'"shipToState",'                                                
         DC    C'"shipToPostalCode",'                                           
         DC    C'"shipToCountry",'                                              
         DC    C'"shipFromName",'                                               
         DC    C'"shipFromStreet",'                                             
         DC    C'"shipFromCity",'                                               
         DC    C'"shipFromState",'                                              
         DC    C'"shipFromPostalCode",'                                         
         DC    C'"shipFromCountry",'                                            
         DC    C'"payInNumberOfDays",'                                          
         DC    C'"buyerVatID",'                                                 
         DC    C'"supplierVatID",'                                              
         DC    C'"invoiceLine",'                                                
         DC    C'"companyCode",'                                                
         DC    C'"costCenter",'                                                 
         DC    C'"projectNo",'                                                  
         DC    C'"accountCode",'                                                
         DC    C'"serviceDescription_RWT",'                                     
         DC    C'"function",'                                                   
         DC    C'"task",'                                                       
         DC    C'"itemComments",'                                               
         DC    C'"unitPrice",'                                                  
         DC    C'"quantity",'                                                   
         DC    C'"itemSubtotal",'                                               
         DC    C'"unitOfMeasure",'                                              
         DC    C'"currency",'                                                   
         DC    C'"poLineNumber",'                                               
         DC    C'"lineTaxCategory1",'                                           
         DC    C'"lineTaxRate1",'                                               
         DC    C'"lineTaxAmount1",'                                             
         DC    C'"lineTaxDescription1",'                                        
         DC    C'"legalReference1",'                                            
         DC    C'"lineSpecialHandlingAmount",'                                  
         DC    C'"lineShippingAmount"'                                          
COLHDX   EQU   *                                                                
COLHDLEN EQU   *-COLHDS                                                         
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDUCOMD                                                        
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREPPZ02 02/26/20'                                      
         END                                                                    
