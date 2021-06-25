*          DATA SET PPREPPT02  AT LEVEL 006 AS OF 01/05/12                      
*PHASE PPPT02A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE GETCOST                                                                
*INCLUDE DDUCOM                                                                 
*INCLUDE PPFMTINO                                                               
         TITLE 'PPPT02 - PHILIP MORRIS INTERFACE'                               
***************************************************************                 
***************************************************************                 
*        DO NOT RUN A MIXTURE OF ESTIMATE AND INVOICE FILE                      
*        TAPE REQUESTS IN THE SAME JOB STREAM - IT WON'T WORK                   
***************************************************************                 
***************************************************************                 
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*     BPLA   01/05/2012   RELINK WITH NEW GETCOST                               
*                                                                               
*        QOPT1 I=INVOICE FILE ONLY, E=ESTIMATE FILE ONLY,                       
*              B=BOTH (FOR TESTING ONLY?)                                       
*        QOPT5 Y=LIST 'CURRENT' INVOICES                                        
*        QOPT6 Y= TEST RUN - NO TAPE, AND CONTINUE IF ERRORS                    
*                            ARE FOUND                                          
*        QOPT7 P=PDUMP RECORDS                                                  
*                                                                               
*        QSTART(6) = PERIOR START                                               
*        QEND(6) = PERIOD END - MAY BE BLANK                                    
*                                                                               
*                                                                               
PPPT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPPT02,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'      2ND DSECT                                     
         L     R5,PPWORK2C                                                      
         USING PPWORK2D,R5                                                      
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PPPTWRKD,R8                                                      
         MVC   ACONIO1,ACONIO                                                   
         LA    R7,PPPT02+4095                                                   
         LA    R7,1(R7)                                                         
         USING PPPT02+4096,R7     **NOTE USE OF R7 AS BASE REG*                 
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,REQFRST                                                     
         BE    FIRSTB                                                           
         CLI   MODE,FBUYCLI                                                     
         BE    FCLI                                                             
         CLI   MODE,FBILCLI                                                     
         BE    FCLI                                                             
         CLI   MODE,FBUYPRO                                                     
         BE    FPRD                                                             
         CLI   MODE,FBUYEST                                                     
         BE    FEST                                                             
         CLI   MODE,FBILPRO                                                     
         BE    FPRD                                                             
*****    CLI   MODE,FBILEST                                                     
*****    BE    FEST                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PROCESS                                                          
         CLI   MODE,REQLAST                                                     
         BE    PUTBUFF                                                          
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
         CLI   MODE,PROCBIL                                                     
         BE    BILL                                                             
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
         L     R0,=V(GETUSER)                                                   
         A     R0,RELO                                                          
         ST    R0,VGETUSER                                                      
         L     R0,=V(GETCOST)                                                   
         A     R0,RELO                                                          
         ST    R0,VGETCOST                                                      
         L     R0,=V(DDUCOM)                                                    
         A     R0,RELO                                                          
         ST    R0,VDDUCOM                                                       
         L     R0,=V(PPFMTINO)                                                  
         A     R0,RELO                                                          
         ST    R0,AFMTINO                                                       
*                                                                               
         XC    MYDUMP,MYDUMP                                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY1)       YYMMDD                        
         GOTO1 DATCON,DMCB,(5,0),(X'14',TODAYY)   YYYYMMDD                      
         ZAP   TOTCNT,=P'0'                                                     
         MVI   EHDRSW,C'N'                                                      
         MVI   IHDRSW,C'N'                                                      
         MVI   FRSTBILL,C'N'                                                    
         MVI   LASTBILL,C'N'                                                    
         ZAP   INVTOTD,=P'0'                                                    
         ZAP   INVRCNT,=P'0'                                                    
         ZAP   ESTRCNT,=P'0'                                                    
*                                                                               
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   TAPESW,0                                                         
         MVI   ALLOWSW,0           DYNAMIC ALLOCATION NOT DONE                  
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
         LA    R3,9                                                             
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
         MVC   DYNDDN,=CL8'PPTTAPE'                                             
         MVC   DYNDSN,=CL20'PRTTAPE.PP0PTXX1'                                   
         MVC   DYNDSN+13(2),QAGENCY                                             
*                                                                               
REQF10   DS    0H                                                               
         MVI   FCRDBUY,C'Y'                                                     
         MVI   FCRDBILL,C'Y'                                                    
         MVC   SVQOPT1,QOPT1      SAVE FILE TYPE                                
         MVC   SVQOPT6,QOPT6      SAVE DO TAPE OPTION                           
         MVC   SVQOPT7,QOPT7      SAVE PDUMPING OPTION                          
         CLI   QOPT1,C'I'     INVOICE FILE ONLY                                 
         BNE   *+8                                                              
         MVI   FCRDBUY,C'N'   DON'T READ BUYS                                   
         CLI   QOPT1,C'E'     INVOICE FILE ONLY                                 
         BNE   *+8                                                              
         MVI   FCRDBILL,C'N'   DON'T READ BILLS                                 
*                                                                               
         TIME  DEC                                                              
*                                                                               
*   DDS COMPUTER 'CLOCK' STARTS AT 6:00 AM = HOUR 00                            
*   WE MAY NEED TO ADD 6 TO THE HOURS TO GET CORRECT MILITARY TIME.             
*                                                                               
         LR    R1,R0       TIME IN HUNDREDTHS IN R1                             
         SRL   R1,8        TO REMOVE LAST 2 DIGITS.                             
         SLL   R1,4                                                             
         AH    R1,=H'12'    SET LAST HALF BYTE TO C                             
         ST    R1,FULL                                                          
         ZAP   DUB,=P'0'                                                        
         MVC   DUB+4(4),FULL                                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  NOW(6),DUB+4(4)    NOW SHOULD BE HHMMSS                          
*                                                                               
         LA    RE,VENTAB           VENDOR TABLE                                 
REQF1C   CLI   0(RE),X'FF'         AGENCY NOT IN MY TABLE                       
         BNE   *+6                                                              
         DC    H'0'            DIE FOR NOW                                      
         CLC   QAGENCY,0(RE)                                                    
         BE    REQF1D                                                           
         LA    RE,15(RE)                                                        
         B     REQF1C                                                           
*                                                                               
REQF1D   MVC   VENDOR,2(RE)      VENDOR CODE                                    
         MVC   PAYEE,8(RE)       PAYEE CODE                                     
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
         MVI   FCRDACTV,C'N'                                                    
         CLC   QPRODUCT,SPACES                                                  
         BNE   *+8                                                              
         MVI   FCRDACTV,C'Y'                                                    
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
         OPEN  (OUTFILE,OUTPUT)                                                 
*                                                                               
REQF75   B     FIRSTB0X                                                         
*                                                                               
FIRSTB0  CLI   TAPESW,C'Y'      SEE IF A PRIOR REQUEST WAS LIVE                 
         BE    MIXERR                                                           
         MVI   TAPESW,C'N'                                                      
         B     FIRSTB0X                                                         
*                                                                               
MIXERR   MVC   P1(37),=C'*** MIX OF TEST AND LIVE REQUESTS ***'                 
         MVC   P2(37),=C'*** THIS REQUEST HAS BEEN SKIPPED ***'                 
         GOTO1 REPORT                                                           
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
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   CINVGRS,=P'0'                                                    
         ZAP   CINVBIL,=P'0'                                                    
         ZAP   CINVCD,=P'0'                                                     
         ZAP   CINVRCV,=P'0'                                                    
         MVI   CINVSW,0                                                         
         XC    ESTLLST(250),ESTLLST    CLEAR LIST OF UNLOCKED ESTS              
         XC    ESTLLST+250(250),ESTLLST+250                                     
*                                                                               
*                                  SAVE DATES FOR ACTIVITY CHECKING             
         GOTO1 DATCON,DMCB,(0,QSTART),(3,SVQSTART)                              
         MVC   SVQEND,=3X'FF'                                                   
*                                                                               
*                                                                               
FIRSTB2X CLC   QEND,SPACES                                                      
         BE    FIRSTB3                                                          
         GOTO1 DATCON,DMCB,(0,QEND),(3,SVQEND)                                  
FIRSTB3  MVC   SVSTART(12),QSTART    SAVE EBCDIC DATES FOR BILL CHK             
         CLC   QEND,SPACES                                                      
         BNE   FIRSTB3C                                                         
*NOP*    MVC   SVEND(6),=C'999999'                                              
         MVC   SVEND(6),=6X'FF'    FOR 21ST CENTURY                             
*                                                                               
FIRSTB3C DS    0H                                                               
         CLI   QOPT1,C'E'       ESTIMATE RUN?                                   
         BE    FIRSTB3D                                                         
         B     FIRSTB3X                                                         
*                                                                               
*              SET START TO JAN01/80 AND END TO DEC31/2059                      
*              SHOULD GET ME EVERYTHING                                         
*                                                                               
FIRSTB3D MVC   QSTART(12),=X'F8F0F0F1F3F1FFF9F1F2F3F1'                          
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
LCLI     CLI   QOPT1,C'E'          ESTIMATE FILE ONLY?                          
         BE    EXIT                                                             
*                                                                               
         MVI   LASTBILL,C'Y'        LAST BILL FOR CLIENT                        
         GOTO1 =A(INVOICE)                                                      
         B     EXIT                                                             
*                                                                               
*                                   FIRST FOR CLIENT                            
FCLI     XC    BADESTS(240),BADESTS   CLEAR BAD ESTIMATE TABLE                  
*                                                                               
         MVI   BADESTS,X'FF'        SET NEW END                                 
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
         EJECT                                                                  
         CLI   QOPT1,C'E'         ESTIMATE FILE ONLY?                           
         BE    EXIT               DONE                                          
*                                                                               
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
         XC    PRDU1,PRDU1                                                      
         CLI   MYUSER,C'Y'        SEE IF USING USER FIELDS                      
         BNE   FPRDSAVE                                                         
         CLC   PPRDKPRD,=C'AAA'    NOT FOR PRD=AAA                              
         BE    FPRDSAVE                                                         
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'P',PPRDREC),PRDU1,0              
         CLI   DMCB,X'FF'                                                       
         BE    FPRDERR                                                          
         CLI   PRDU1+21,C' '    MUST FIND DATA                                  
         BNH   FPRDERR                                                          
         B     FPRDSAVE                                                         
*                                                                               
FPRDERR  DS    0H                                                               
         MVC   P1,SPACES                                                        
         MVC   P1(36),=C'*** MISSING PRODUCT USER FIELD 1 ***'                  
         MVC   P1+40(3),PPRDKPRD                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    FPRDSAVE              CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
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
         XC    UCOMDATA,UCOMDATA   CLEAR                                        
*                                                                               
         LA    R4,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R4                                                       
*                                                                               
         MVC   USAVKEY,KEY        SAVE PPG'S KEY                                
         MVC   UCPRD,PESTKPRD                                                   
FEST1    MVC   UCACOMF,VCOMFACS     COMFACS                                     
         MVI   UCSYS,C'P'        SYSTEM TO PRINT                                
         MVC   UCAGY,QAGENCY     AGENCY                                         
         MVC   UCMED,PESTKMED    MEDIA                                          
         MVC   UCCLT,PESTKCLT    CLIENT                                         
*                                DO UCOMM FOR PRD AAA                           
         OI    UCOPT,UCOEST     RETURN ESTIMATE UCOMMS                          
         MVC   UCEST,PESTKEST                                                   
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK    NEW UCOM CALL SINCE GOTO MACRO                
         CLI   UCERROR,0         TRASHED WRKING STORAGE USED BY DDUCOM          
         BNE   FEST3X       ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST    NO DATA?                                      
         BNO   FEST2                                                            
         CLC   UCPRD,=C'AAA'      DID I JUST TRY PRD AAA?                       
         BE    FEST2X                                                           
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         MVC   UCPRD,=C'AAA'      IF NOT FOUND TRY PRD AAA                      
         B     FEST1                                                            
*                                                                               
FEST2    XC    UCTTLS(UCALL),UCTTLS                                             
         L     RE,UCETTLS     EST TITLES                                        
         MVC   UCTTLS,0(RE)   SAVE INFO IN MY STORAGE                           
         L     RE,UCEDATA     EST DATA                                          
         MVC   UCOMDATA,0(RE)                                                   
*                                                                               
FEST2X   CLI   QOPT1,C'E'     ESTIMATE FILE ONLY                                
         BE    FEST3X         ANY ERRORS WILL BE CAUGHT LATER                   
*                                                                               
         OC    UCOMDATA(4),UCOMDATA   1ST IS VALIDITY YEAR                      
         BNZ   FEST3X                                                           
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),UCPRD    DISPLAY PRD I LAST SEARCHED FOR                
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+25(3),DUB+6(2)                                                
         MVC   P1+31(24),=C'MISSING VALIDITY YEAR **'                           
         MVI   RCSUBPRG,10                                                      
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'      TEST RUN?                                        
         BE    FEST3X          KEEP GOING                                       
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
FEST3X   DS    0H                                                               
         MVC   KEY,USAVKEY      RESTORE SEQ READ                                
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
BILL     DS    0H                  PROCESS BILL HEADERS                         
         TM    PBILLREC+27,X'80'                                                
         BNZ   EXIT                                                             
         OC    PBILKEST,PBILKEST       IGNORE BILLS WITH NO EST                 
         BZ    EXIT                                                             
         CLC   PBILLDAT,SVSTART                                                 
         BL    EXIT                                                             
         CLC   PBILLDAT,SVEND                                                   
         BH    EXIT                                                             
         TM    PBILCMSW,X'20'     SKIP AOR BILLS                                
         BO    EXIT                                                             
         CLI   PBRETAIL,X'41'     AND CORP RETAIL SUMMARY BILLS                 
         BE    EXIT                                                             
*                                                                               
         CLC   QAGENCY,=C'H9'      STARCOM?                                     
         BNE   BILLA01                                                          
         CP    PBILLRCV,=P'0'      ZERO AMOUNT DUE?                             
         BE    EXIT                THEN SKIP                                    
*                                                                               
BILLA01  GOTO1 =V(PPFMTINO),DMCB,PBILLDAT,(2,PBILKBNO),                X        
               (PBILKMED,B1PROF),B1XPROF                                        
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)      FULL INVOICE NUMBER                          
*                                                                               
         MVI   CKESTREC,C'L'       SET FROM BILL                                
         GOTO1 =A(CKEST)           READ EST AND COMMENT                         
*                                                                               
         MVC   WORK(2),PBILKMOS                                                 
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(X'14',WORK+6)                              
*                                                                               
         CLC   UCOMDATA(4),WORK+6        YEARS MUST MATCH                       
         BE    BILLA10                                                          
*                                                                               
         MVC   P1+2(72),=C'** ERROR - MONTH MM/YYYY NOT IN VALIDITY YEAX        
               R YYYY  - PRD=XXX EST=XXX **'                                    
*                                                                               
         MVC   P1+19(2),WORK+10                                                 
         MVC   P1+22(4),WORK+6                                                  
         MVC   P1+48(4),UCOMDATA                                                
         MVC   P1+60(3),PBILKPRD                                                
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+68(3),DUB                                                     
*                                                                               
         MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
*                                                                               
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    BILLA10               CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
BILLA10  DS    0H                                                               
*                                                                               
         GOTO1 =A(INVOICE)                                                      
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PROCESS  DS    0H                  PROCESS BUYRECS                              
*****                                                                           
         TM    PBUYCNTL,X'80'      IGNORE DELETED BUYS                          
         BO    EXIT                                                             
         CLI   PBDBFD,C'T'         TEST BUY?                                    
         BE    EXIT                                                             
*                                                                               
         MVI   CKESTREC,C'B'       SET FROM BUY                                 
         GOTO1 =A(CKEST)           GO READ EST AND USER FIELDS                  
*                                                                               
         MVC   BILLFORM(5),=X'0505000000'                                       
*                                                                               
         MVC   WORK(4),GROSS         BUILD GR,CD,AG FOR GETCOST                 
         MVC   WORK+4(4),CSHDSC                                                 
         MVC   WORK+8(4),AGYCOM                                                 
*                                                                               
         OC    ESTFORMU,ESTFORMU     CHECK IF EST FORMULA IS SET UP             
         BZ    *+14                                                             
         MVC   BILLFORM,ESTFORMU     MOVE TO BILLFORM                           
         B     GETCST                                                           
*                                                                               
         OC    PRDFORMU,PRDFORMU     CHECK IF PRD FORMULA IS SET UP             
         BZ    *+14                                                             
         MVC   BILLFORM,PRDFORMU     MOVE TO BILLFORM                           
         B     GETCST                                                           
*                                                                               
         CLI   AAAPRD,C'Y'           CHECK IF AAA EXISTS                        
         BNE   GETCST                                                           
*                                                                               
         OC    AAAESTFR,AAAESTFR     CHECK IF EST FORMULA IS SET UP             
         BZ    *+14                                                             
         MVC   BILLFORM,AAAESTFR     MOVE TO BILLFORM                           
         B     GETCST                                                           
*                                                                               
         OC    AAAFORMU,AAAFORMU     CHECK IF EST FORMULA IS SET UP             
         BZ    *+14                                                             
         MVC   BILLFORM,AAAFORMU     MOVE TO BILLFORM                           
         B     GETCST                                                           
*                                                                               
GETCST   DS    0H                                                               
*        DC    H'0'                                                             
         GOTO1 VGETCOST,DMCB,BILLFORM,WORK,PBUYREC                              
*                                                                               
         TM    BILLFORM,X'04'                                                   
         BZ    *+16                                                             
         L     R1,DMCB+4                                                        
         A     R1,CSHDSC                                                        
         ST    R1,DMCB+4                                                        
*                                                                               
         MVC   MYAMTD,DMCB+4                                                    
OVERGTCS DS    0H                                                               
*                                                                               
         CLI   MYUSER,C'E'         SEE IF USING ESTIMATE USER FIELDS            
         BNE   PB205                                                            
*                                                                               
         CLI   ESTU1+25,C'X'          MEANS EXCLUDE                             
         BE    EXIT                                                             
*                                                                               
PB205    XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'E'                                                     
         MVC   BUFMED,QMEDIA                                                    
*                                                                               
****                                                                            
****     DESRIPTION FROM USER FIELD  - NO-OPED                                  
****                                                                            
         B     PB205U                                                           
*                                                                               
****     CLI   MYUSER,C'E'           SEE IF USING USER FIELDS                   
****     BNE   PB205U                                                           
****     MVC   BUFYR(4),ESTU1+21     1ST FOR CHARS OF UDEF1                     
****     MVC   BUFCOM,SPACES                                                    
****     MVC   BUFCOM(3),ESTU1+22    2ND-4TH CHRAS OF EST UDEF1                 
****                                                                            
****     MVC   BUFCOM+20(12),PESTST      ALSO SAVE DATES                        
****                                                                            
****     B     PB205E                                                           
*                                                                               
PB205U   DS    0H                                                               
         MVC   BUFCOMEN(20),PESTNAME                                            
         OC    BUFCOMEN(20),SPACES                                              
*                                                                               
******************************************************************              
*        THE 4 FIELDS BELOW WILL BE SET FROM UCOM                               
******************************************************************              
         MVC   BUFCOME1,UCOMDATA     VALIDITY YEAR                              
         MVC   BUFCOME2(10),UCOMDATA+32     GL ACCOUNT                          
         MVC   BUFCOME3(12),UCOMDATA+64     INTERNAL ORDER NO.                  
         MVC   BUFCOME4(10),UCOMDATA+96     COST CENTER                         
*                                                                               
         LA    RF,1(RC)        SO I CAN ADDRESS PUB FIELDS                      
         LA    RF,4095(RF)                                                      
         USING PPFILED+4096,RF                                                  
         MVC   BUFCOMPN(20),PUBNAME                                             
         OC    BUFCOMPN(20),SPACES    JUST IN CASE THERE ARE X'00'S             
         MVC   BUFCOMPZ(20),PUBZNAME                                            
         OC    BUFCOMPZ(20),SPACES    JUST IN CASE THERE ARE X'00'S             
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   BUFYR,UCOMDATA+3    LAST DIGIT OF VALIDITY YEAR                  
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFEST(3),DUB                                                    
*                                                                               
*                                                                               
PB205E   DS    0H                                                               
         MVC   BUFPRD,PESTKPRD   USE THE PRODUCT I'M PROCESSING                 
*                                                                               
******   CLI   MYUSER,C'Y'                                                      
******   BNE   *+10                                                             
******   MVC   BUFPRD,PRDU1+21                                                  
*                                                                               
*                                                                               
PB206    DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBDBDATE),(X'14',WRKDATE)                         
*                                                                               
         L     R0,MYAMTD                                                        
         CVD   R0,DUB                                                           
         L     R0,GROSS                                                         
         S     R0,AGYCOM                                                        
         CVD   R0,NETDUB                                                        
         L     R0,CSHDSC                                                        
         CVD   R0,CDDUB                                                         
*                                                                               
PB207    DS    0H                                                               
         XC    BUFPUB,BUFPUB     MUST RECLEAR FOR ESTIMATE $                    
         MVC   BUFMTH,WRKDATE    YYYYMM                                         
*                                                                               
                                                                                
         CP    DUB,=P'0'                                                        
         BNE   PB208                                                            
         CP    NETDUB,=P'0'                                                     
         BNE   PB208                SKIP IF NO $                                
         CP    NETDUB,=P'0'                                                     
         BE    PB300                SKIP IF NO $                                
*                                                                               
PB208    ZAP   BUFCD,CDDUB           CASH DISCOUNT                              
         ZAP   BUFAMTD,DUB           AMOUNT DUE                                 
         ZAP   BUFCOMM,DUB           COMMISSION= AMT. DUE - NET-CD              
         SP    BUFCOMM,NETDUB                                                   
         SP    BUFCOMM,CDDUB                                                    
*                                                                               
*                                                                               
PB210    GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         MVC   BUFPUB(L'PBUYKPUB),PBUYKPUB   PUBLICATION                        
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
PB300    DS    0H                                                               
         B     EXIT         DONE WITH BUY'S DATA                                
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
*                                                                               
         CLI   QOPT1,C'I'         INVOICE FILE ONLY?                            
         BE    EXIT               IF YES - THEN DONE                            
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
*                                                                               
         XC    BUFREC,BUFREC                                                    
         B     PUTB100                                                          
         EJECT                                                                  
PUTB100  DS    0H                                                               
*                                                                               
*        NEXT OUTPUT ESTIMATE FILE HEADER RECORD                                
*                                                                               
         CLI   EHDRSW,C'Y'      DID I ALREADY DO IT?                            
         BE    PUTB1X                                                           
         MVI   EHDRSW,C'Y'                                                      
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTEFHD,R6                                                        
         MVI   PTEFHTYP,C'1'                                                    
         MVC   PTEFHAPP,=CL10'ADVERT-EST'                                       
         MVC   PTEFHVER,=C'0001'      VERSION                                   
         MVC   PTEFHDAT(8),TODAYY                                               
         MVC   PTEFHTIM,NOW                                                     
*                                                                               
PUTB1D   MVC   PTEFHVEN,VENDOR     VENDOR CODE                                  
         MVC   RECTYPE,=C'E1'                                                   
         BAS   RE,MWRITE                                                        
         AP    ESTRCNT,=P'1'       DUMP FILE COUNT                              
*                                                                               
         DROP  R6                                                               
*                                                                               
*        READ BUFFALO RECORDS AND OUTPUT ESTIMATE FILE                          
*                                                                               
PUTB1X   XC    BUFREC,BUFREC                                                    
         XC    LASTEST,LASTEST                                                  
         XC    LASTEVY,LASTEVY                                                  
         XC    LASTEGL,LASTEGL                                                  
         XC    LASTEIO,LASTEIO                                                  
         XC    LASTECC,LASTECC                                                  
*                                                                               
         ZAP   ESTCD,=P'0'       CLEAR ESTIMATE TOTALS                          
         ZAP   ESTAMTD,=P'0'                                                    
         ZAP   ESTCOMM,=P'0'                                                    
*                                                                               
         MVI   BUFTYPE,C'E'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     PUTB110                                                          
PUTB105  GOTO1 BUFFALO,DMCB,=C'SEQ',(C'E',BUFFBUFF),BUFREC,0                    
*                                                                               
PUTB110  CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    PUTB200X                                                         
         CLI   BUFTYPE,C'E'                                                     
         BNE   PUTB200X            END OF E'S                                   
         CLI   LASTEST,0           FIRST REC?                                   
         BNE   PUTB112                                                          
         MVC   LASTEST(9),BUFREC                                                
         MVC   LASTEVY,BUFCOME1      VALIDITY YEAR                              
         MVC   LASTEGL,BUFCOME2      GL ACCOUNT                                 
         MVC   LASTEIO,BUFCOME3      IO NUMBER                                  
         MVC   LASTECC,BUFCOME4      COST CENTER                                
         B     PUTB113             GO TO NEW ESTIMATE ROUTINE                   
*                                                                               
PUTB112  CLC   BUFREC(9),LASTEST                                                
         BE    PUTB120             SAME ESTIMATE                                
         MVI   ETOTSW,C'Y'                                                      
         BAS   RE,DISPLAY                                                       
*                                                                               
PUTB113  DS    0H                  NEW ESTIMATE                                 
*                                  MUST OUTPUT ESTIMATE HEADER                  
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTEHD,R6                                                         
         MVI   PTEHTYP,C'4'                                                     
         MVC   PTEHEST(1),QMEDIA                                                
         MVC   PTEHEST+1(3),PCLTKCLT                                            
         MVC   PTEHEST+4(3),BUFPRD                                              
         MVC   PTEHEST+7(3),BUFEST                                              
         MVC   PTEHESTD(20),BUFCOMEN     ESTIMATE NAME                          
         MVC   PTEHVYR,BUFCOME1  VALIDITY YEAR                                  
         MVI   PTEHCAT,C'P'      SYSTEM - PRINT                                 
         MVC   PTEHCAT+1(1),BUFMED  MEDIA                                       
         MVC   PTEHREV,=C'0000'  REVISON NUMBER - ALWAYS 0000 FOR MEDIA         
         MVC   PTEHREVD,TODAYY   USE TODAY                                      
         MVC   PTEHGL,BUFCOME2   GL ACCOUNT                                     
         OC    PTEHGL,SPACES                                                    
         MVC   PTEHIO,BUFCOME3   INTERNAL ORDER #                               
         OC    PTEHIO,SPACES                                                    
*                               IF FOUND  - DON'T SEND COST CENTER              
         CLC   PTEHIO,SPACES                                                    
         BNE   *+10                                                             
*                                                                               
         MVC   PTEHCC,BUFCOME4   COST CENTER                                    
         OC    PTEHCC,SPACES                                                    
         MVC   RECTYPE,=C'E4'                                                   
         BAS   RE,MWRITE                                                        
         AP    ESTRCNT,=P'1'       DUMP FILE COUNT                              
*                                                                               
         MVI   ERRSW,C'N'                                                       
*                                                                               
         OC    BUFCOME1,BUFCOME1   VALIDITY YEAR PRESENT?                       
         BNZ   PUTB113E                                                         
         MVI   ERRSW,C'Y'                                                       
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),BUFPRD                                                  
         MVC   P1+25(3),BUFEST                                                  
         MVC   P1+31(24),=C'MISSING VALIDITY YEAR **'                           
         MVI   RCSUBPRG,20                                                      
         GOTO1 REPORT                                                           
*                                                                               
PUTB113E OC    BUFCOME2,BUFCOME2   GL ACCOUNT PRESENT?                          
         BNZ   PUTB113F                                                         
         MVI   ERRSW,C'Y'                                                       
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),BUFPRD                                                  
         MVC   P1+25(3),BUFEST                                                  
         MVC   P1+31(24),=C'** MISSING GL ACCOUNT **'                           
         MVI   RCSUBPRG,20                                                      
         GOTO1 REPORT                                                           
*                                                                               
PUTB113F OC    BUFCOME3(L'BUFCOME3+L'BUFCOME4),BUFCOME3 I/O+COST C              
         BNZ   PUTB114F                                                         
         MVI   ERRSW,C'Y'                                                       
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),BUFPRD                                                  
         MVC   P1+25(3),BUFEST                                                  
         MVC   P1+31(38),=C'** MISSING BOTH I/O AND COST CENTER **'             
         MVI   RCSUBPRG,20                                                      
         GOTO1 REPORT                                                           
*                                                                               
PUTB114F OC    BUFCOME3,BUFCOME3   I/O #                                        
         BZ    PUTB115                                                          
         OC    BUFCOME4,BUFCOME4   COST CENTER                                  
         BZ    PUTB115                                                          
*******  MVI   ERRSW,C'Y'                                                       
         MVC   P1+2(29),=C'** WARNING - PRD XXX EST NNN - '                     
         MVC   P1+19(3),BUFPRD                                                  
         MVC   P1+27(3),BUFEST                                                  
         MVC   P1+31(38),=C'** BOTH I/O AND COST CENTER PRESENT **'             
         MVI   RCSUBPRG,20                                                      
         GOTO1 REPORT                                                           
*                                                                               
PUTB115  CLI   ERRSW,C'N'            ERROR FOUND?                               
         BE    PUTB120                                                          
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PUTB120               CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
         DROP  R6                                                               
PUTB120  DS    0H                                                               
         MVC   LASTEST,BUFREC                                                   
         MVC   LASTEVY,BUFCOME1      VALIDITY YEAR                              
         MVC   LASTEGL,BUFCOME2      GL ACCOUNT                                 
         MVC   LASTEIO,BUFCOME3      IO NUMBER                                  
         MVC   LASTECC,BUFCOME4      COST CENTER                                
*                                                                               
         CLC   BUFMTH(4),BUFCOME1   SEE IF WITHIN VALIDITY YEAR                 
         BE    PUTB125                                                          
*                                                                               
         MVC   P1+2(72),=C'** ERROR - MONTH MM/YYYY NOT IN VALIDITY YEAX        
               R YYYY  - PRD=XXX EST=XXX **'                                    
         MVC   P1+19(2),BUFMTH+4                                                
         MVC   P1+22(4),BUFMTH                                                  
         MVC   P1+48(4),BUFCOME1                                                
         MVC   P1+60(3),BUFPRD                                                  
         MVC   P1+68(3),BUFEST                                                  
         MVI   RCSUBPRG,20                                                      
         BAS   RE,MYRPT                                                         
*                                                                               
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PUTB125               CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
*                                                                               
PUTB125  DS    0H                                                               
         BAS   RE,DISPLAY          PRINT ESTIMATE/STA $                         
*                                                                               
         OC    BUFPUB,BUFPUB                                                    
         BZ    PUTBEST           ESTIMATE DETAIL LINE                           
         B     PUTBPUB           MUST BE SPEND LINE                             
*                                                                               
         EJECT                                                                  
PUTBEST  DS    0H               ESTIMATE DETAIL LINE                            
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTELD,R6                                                         
         MVI   PTELTYP,C'5'                                                     
         MVC   PTELMMWC(5),=C'MEDIA'                                            
         MVC   PTELMOS,BUFMTH    YYYYMM                                         
*                                                                               
*        NOTE: IT'S POSSIBLE THAT THESE AMOUNTS MIGHT BE NEGATIVE               
*              THEY DON'T HAVE AN INDICATOR ON THIS RECORD                      
*              MAYBE ONE IS NEEDED FOR EACH FIELD?                              
*                                                                               
         EDIT  BUFCD,(12,PTELCD),2,FILL=0,ZERO=NOBLANK                          
         EDIT  BUFAMTD,(12,PTELNET),2,FILL=0,ZERO=NOBLANK                       
         EDIT  BUFCOMM,(12,PTELCOM),2,FILL=0,ZERO=NOBLANK                       
         MVC   RECTYPE,=C'E5'                                                   
         BAS   RE,MWRITE                                                        
         AP    ESTRCNT,=P'1'       DUMP FILE COUNT                              
         B     PUTB105             GO DO NEXT BUFFALO REC                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
PUTBPUB  DS    0H               PUBLICATION SPEND LINE                          
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTESLD,R6                                                        
         MVI   PTESLTYP,C'6'                                                    
         MVC   PTESLCAR(20),BUFCOMPN    PUBNAME                                 
**ZONE   MVC   PTESLCAR+20(20),BUFCOMPZ PUBNAME                                 
         MVC   PTESLMOS,BUFMTH   YYYYMM                                         
         EDIT  BUFAMTD,(12,PTESLNET),2,FILL=0,ZERO=NOBLANK                      
         MVC   RECTYPE,=C'E6'                                                   
         BAS   RE,MWRITE                                                        
         AP    ESTRCNT,=P'1'       DUMP FILE COUNT                              
         B     PUTB105             GO DO NEXT BUFFALO REC                       
*                                                                               
         DROP  R6                                                               
*                                                                               
PUTB200X MVI   ETOTSW,C'Y'         TOTALS FOR LAST ESTIMATE                     
         BAS   RE,DISPLAY                                                       
*                                                                               
         MVC   P1+2(16),=C'**REPORT TOTAL**'                                    
         EDIT  GTTOTCD,(14,P1+49),2,COMMAS=YES,FLOAT=-                          
         EDIT  GTTOTAMT,(14,P1+66),2,COMMAS=YES,FLOAT=-                         
         EDIT  GTTOTCOM,(14,P1+83),2,COMMAS=YES,FLOAT=-                         
         MVI   P1+63,C'*'                                                       
         MVI   P1+64,C'*'                                                       
         MVI   P1+80,C'*'                                                       
         MVI   P1+81,C'*'                                                       
         MVI   P1+97,C'*'                                                       
         MVI   P1+98,C'*'                                                       
         MVI   SPACING,2                                                        
         MVI   RCSUBPRG,20                                                      
         BAS   RE,MYRPT                                                         
         MVC   P1+2(14),=C'UNLOCKED ESTS='                                      
         OC    ESTLLST(2),ESTLLST                                               
         BNZ   PUTB102                                                          
         MVC   P1+17(4),=C'NONE'                                                
         B     PUTB102X                                                         
*                                                                               
PUTB102  LA    R1,P1+17                                                         
         LA    R2,ESTLLST                                                       
PUTB102B OC    0(2,R2),0(R2)                                                    
         BZ    PUTB102X                                                         
         MVC   HALF,0(R2)                                                       
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB                                                      
         OC    2(2,R2),2(R2)  ANY MORE?                                         
         BZ    PUTB102X                                                         
         MVI   3(R1),C','                                                       
         LA    R1,4(R1)                                                         
         LA    R2,2(R2)                                                         
         B     PUTB102B                                                         
*                                                                               
PUTB102X MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
*                                 ESTIMATE FILE CONTROL RECORD                  
*                                 NOW DONE AT RUNLAST                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TOTALS   DS    0H                                                               
*                                                                               
         CLI   SVQOPT1,C'I'       INVOICE FILE RUN?                             
         BNE   TOTE                                                             
*                                 INVOICE FILE CONTROL RECORD                   
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTIFCD,R6                                                        
         MVI   PTIFCTYP,C'9'                                                    
         AP    INVRCNT,=P'1'     FOR THIS RECORD                                
         EDIT  INVRCNT,(5,PTIFCCNT),0,FILL=0,ZERO=NOBLANKS                      
         MVC   RECTYPE,=C'I9'                                                   
         BAS   RE,MWRITE                                                        
         B     TOT0                                                             
*                                                                               
         DROP  R6                                                               
TOTE     CLI   SVQOPT1,C'E'       ESTIMATE FILE RUN?                            
         BNE   TOT0                                                             
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTEFCD,R6                                                        
         MVI   PTEFCTYP,C'9'                                                    
         AP    ESTRCNT,=P'1'     FOR THIS RECORD                                
         EDIT  ESTRCNT,(5,PTEFCCNT),0,FILL=0,ZERO=NOBLANKS                      
         MVC   RECTYPE,=C'E9'                                                   
         BAS   RE,MWRITE                                                        
*                                                                               
         DROP  R6                                                               
TOT0     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         L     R4,ATITLES                                                       
         L     R3,ALENTAB                                                       
         LA    R6,9                FOR BCT                                      
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
         CLI   TAPESW,C'Y'          SEE IF PRODUCING A TAPE                     
         BNE   EXIT                                                             
         CLOSE (OUTFILE,)                                                       
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         DS    XL500    FOR NOW - JUST SO MWRITE AND MYRPT CAN BE               
*                       ADDRESSED BY OTHER MODULES                              
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
         LH    R3,HALF                                                          
         LA    R3,4(R3)                                                         
         STH   R3,OUTREC-4                                                      
         CLI   SVQOPT7,C'P'                                                     
         BE    WRIT1                                                            
         CLI   QOPT7,C'P'                                                       
         BNE   WRIT2                                                            
WRIT1    MVC   P1(125),OUTREC                                                   
         MVC   P2(125),OUTREC+125                                               
         OI    P2,X'01'                                                         
         OI    P3,X'01'                                                         
         GOTO1 HEXOUT,DMCB,OUTREC-4,P4+10,54,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+50,P5+18,50,=C'N'                               
         GOTO1 (RF),(R1),OUTREC+100,P6+18,50,=C'N'                              
         MVC   P4+1(7),=C'001-050'                                              
         MVC   P5+1(7),=C'051-100'                                              
         MVC   P6+1(7),=C'101-150'                                              
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   SVQOPT6,C'Y'     SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LA    R1,OUTFILE                                                       
         LA    R0,OUTREC-4                                                      
         PUT   (1),(0)                                                          
WRIT3    AP    TOTCNT,=P'1'                                                     
         XIT1                                                                   
*                                                                               
MYRPT    NTR1                                                                   
         MVC   QSTART(12),SVSTART      RESTORE FOR HEADLINES                    
*NOP*    CLC   SVEND,=C'999999'                                                 
         CLC   SVEND,=6X'FF'       FOR 21ST CENTURY                             
         BNE   MYRPT5                                                           
         MVC   QEND,SPACES                                                      
MYRPT5   GOTO1 REPORT                                                           
         MVC   QSTART(12),SPACES                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
DISPLAY  NTR1                                                                   
         CLI   BUFREC,X'FF'                                                     
         BE    DPLAYX                                                           
         MVI   RCSUBPRG,20         FOR PRD/EST RECAP                            
*                                                                               
         CLI   ETOTSW,C'Y'          DOING ESTIMATE TOTAL?                       
         BNE   DPLAY10                                                          
         MVC   P1+2(3),LASTEST+2   PRODUCT                                      
         MVC   P1+7(4),LASTEST+5   (YEAR DIGIT AND DDS EST)                     
         MVC   P1+14(6),=C'TOTAL*'                                              
         EDIT  ESTCD,(14,P1+49),2,COMMAS=YES,FLOAT=-                            
         EDIT  ESTAMTD,(14,P1+66),2,COMMAS=YES,FLOAT=-                          
         EDIT  ESTCOMM,(14,P1+83),2,COMMAS=YES,FLOAT=-                          
         MVI   P1+63,C'*'                                                       
         MVI   P1+80,C'*'                                                       
         MVI   P1+97,C'*'                                                       
         MVC   P2+14(14),=C'VALIDITY YEAR='                                     
         MVC   P2+28(4),LASTEVY                                                 
         MVC   P2+33(11),=C'GL ACCOUNT='                                        
         MVC   P2+44(10),LASTEGL                                                
         MVC   P2+55(19),=C'INTERNAL ORDER NO.='                                
         MVC   P2+74(12),LASTEIO                                                
         MVC   P2+87(12),=C'COST CENTER='                                       
         MVC   P2+99(10),LASTECC                                                
         ZAP   ESTCD,=P'0'       CLEAR AFTER PRINTING                           
         ZAP   ESTAMTD,=P'0'                                                    
         ZAP   ESTCOMM,=P'0'                                                    
         MVI   SPACING,2                                                        
         MVI   ETOTSW,C'N'                                                      
         B     DPLAY20                                                          
*                                                                               
DPLAY10  MVC   P1+2(3),BUFPRD                                                   
         MVC   P1+7(4),BUFYR       (YEAR DIGIT AND DDS EST)                     
         OC    BUFPUB,BUFPUB                                                    
         BZ    *+10                                                             
**ZONE                                                                          
**ZONE   CHANGE INSTRUCTION ABOVE TO *+16                                       
**ZONE                                                                          
         MVC   P1+14(20),BUFCOMPN   PUB NAME                                    
**ZONE   MVC   P2+14(20),BUFCOMPZ   ZONE NAME                                   
         MVC   P1+37(2),BUFMTH+4    MONTH                                       
         MVI   P1+39,C'/'                                                       
         MVC   P1+40(4),BUFMTH      YEAR                                        
*                                                                               
         EDIT  BUFCD,(14,P1+49),2,COMMAS=YES,FLOAT=-                            
         EDIT  BUFAMTD,(14,P1+66),2,COMMAS=YES,FLOAT=-                          
         EDIT  BUFCOMM,(14,P1+83),2,COMMAS=YES,FLOAT=-                          
         OC    BUFPUB,BUFPUB     DON'T TOTAL PUB RECORDS                        
         BNZ   DPLAY20                                                          
         AP    ESTCD,BUFCD       ADD TO ESTIMATE TOTALS                         
         AP    ESTAMTD,BUFAMTD                                                  
         AP    ESTCOMM,BUFCOMM                                                  
*                                                                               
         AP    GTTOTCD,BUFCD                                                    
         AP    GTTOTAMT,BUFAMTD                                                 
         AP    GTTOTCOM,BUFCOMM                                                 
DPLAY20  BAS   RE,MYRPT                                                         
DPLAYX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
VENTAB   DS    0C                                                               
         DC    C'H9',CL6'417899',CL7'2103391'   STARCOM/LEO                     
         DC    C'YN',CL6'417650',CL7'2103100'   YOUNG AND RUBICAM               
         DC    C'SJ',CL6'VEN-SJ',CL7'PAY-SJ'    MEDIAVEST                       
         DC    C'TC',CL6'VEN-TC',CL7'PAY-TC'    MEDIAVEST                       
         DC    X'FFFF'         END OF TABLE                                     
*                                                                               
         EJECT                                                                  
*        AGENCY TABLE                                                           
*                                                                               
*        AGENCY CODE/VENDOR/COMPANY/MYUSER VALUE                                
*                                   E=ESTIMATE USER FIELDS                      
*                                                                               
AGYTAB   DC    C'H9',C'E'                                                       
         DC    C'YN',C' '                                                       
         DC    C'TC',C' '                                                       
         DC    C'SJ',C' '                                                       
         DC    X'FFFF'                                                          
*                                                                               
AGYTABL  EQU   3                                                                
         EJECT                                                                  
*                                                                               
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=VB,LRECL=408,             X        
               BLKSIZE=4084,MACRF=PM                                            
*                           NETWORK UNIT PROCESSING                             
         EJECT                                                                  
INVOICE  CSECT                                                                  
         NMOD1 0,INVOICE                                                        
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8 AND R9                              
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
         DS    0H                                                               
*                                                                               
         CLI   FRSTBILL,C'Y'      FIRST BILL RECORD                             
         BNE   BILL5                                                            
*                                                                               
         MVI   FRSTBILL,C'N'      SO I'LL ONLY DO ONCE                          
*                                 INVOICE FILE HEADER                           
         CLI   IHDRSW,C'Y'        HAVE I DONE ONE?                              
         BE    BILL5                                                            
         MVI   IHDRSW,C'Y'                                                      
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTIFHD,R6                                                        
         MVI   PTIFHTYP,C'1'                                                    
         MVC   PTIFHAPP,=CL10'ADVERT-INV'                                       
         MVC   PTIFHVER,=C'0001'      VERSION                                   
         MVC   PTIFHDAT(8),TODAYY                                               
         MVC   PTIFHTIM,NOW         HHMMSS (MILITARY)                           
*                                                                               
         MVC   PTIFHVEN,VENDOR     VENDOR CODE                                  
         MVC   PTIFHPAY,PAYEE      PAYEE CODE                                   
         MVC   RECTYPE,=C'I1'                                                   
         BAS   RE,MWRITE                                                        
         AP    INVRCNT,=P'1'       DUMP FILE COUNT                              
*                                                                               
         DROP  R6                                                               
*                                                                               
BILL5    CLI   LASTBILL,C'Y'                                                    
         BE    BILL10                                                           
*                                                                               
         CLC   LINVFULL,DINVFULL   SAME INVOICE NUMBER?                         
         BNE   BILL8                                                            
         CLC   LBQDATE,PBILLDAT    AND RUN DATE?                                
         BE    BILL50                                                           
*                                                                               
*        NEW INVOICE                                                            
*                                                                               
BILL8    MVC   LINVFULL,DINVFULL                                                
         MVC   LBQDATE,PBILLDAT                                                 
*                                                                               
*   SEE IF I NEED TO WRITE RECORDS FOR LAST INVOICE                             
*                                                                               
BILL10   OC    INVTAB(INVLEN),INVTAB    ANY ENTRIES?                            
         BZ    BILL20                                                           
*                                                                               
*        FIRST OUTPUT INVOICE HEADER                                            
*                                                                               
         LA    R6,OUTREC                                                        
         USING PTIHD,R6                                                         
         MVI   PTIHCMI,C' '      CLEAR CREDIT MEMO INDICATOR                    
         CP    INVTOTD,=P'0'                                                    
         BNL   *+8                                                              
         MVI   PTIHCMI,C'X'      CREDIT                                         
         EDIT  INVTOTD,(12,PTIHNET),2,FILL=0,ZERO=NOBLANK                       
         MVC   RECTYPE,=C'I4'                                                   
         BAS   RE,MWRITE                                                        
         AP    INVRCNT,=P'1'      BUMP FILE REC COUNT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
*        NOW OUTPUT INVOICE DETAIL RECORDS                                      
*                                                                               
         LA    R4,INVTAB                                                        
BILL15   OC    0(INVLEN,R4),0(R4)    ANY ENTRY                                  
         BZ    BILL20                                                           
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTILD,R6                                                         
         MVI   PTILTYP,C'5'                                                     
         MVC   PTILMMWC(5),=C'MEDIA'                                            
         MVC   WORK(2),0(R4)                                                    
         MVI   WORK+2,X'01'    SET DAY TO 1                                     
         GOTO1 DATCON,DMCB,(3,WORK),(X'14',PTILMOS)                             
BILL17   DS    0H                                                               
         MVI   PTILCMI,C' '       MUST CLEAR                                    
         CP    2(6,R4),=P'0'                                                    
         BNL   *+8                                                              
         MVI   PTILCMI,C'X'       NEGATIVE AMOUNT DUE                           
*                                                                               
         MVC   PTILQUAN(5),=C'1.000'    HARD CODED                              
*                                                                               
*        NOTE THAT IT'S POSSIBLE THAT THE COMMISSION                            
*        JUST MIGHT HAVE THE OPPOSITE SIGN OF THAT OF                           
*        THE AMOUNT DUE.                                                        
*        MAYBE ONE IS NEEDED FOR EACH FIELD?                                    
*                                                                               
         MVC   PTILCD,=C'000000000.00'                                          
         EDIT  (P6,2(R4)),(12,PTILNET),2,FILL=0,ZERO=NOBLANK                    
         EDIT  (P6,8(R4)),(12,PTILCOM),2,FILL=0,ZERO=NOBLANK                    
         MVC   RECTYPE,=C'I5'                                                   
         BAS   RE,MWRITE                                                        
         AP    INVRCNT,=P'1'      BUMP FILE REC COUNT                           
         LA    R4,INVLEN(R4)                                                    
         B     BILL15                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
BILL20   CLI   LASTBILL,C'Y'                                                    
         BE    BILL90            DONE - DO FILE CONTROL RECORD                  
*                                                                               
         ZAP   INVTOTD,=P'0'     CLEAR TOTAL AMOUNT DUE                         
         XC    INVTAB,INVTAB     CLEAR INVOICE DETAIL TABLE                     
*                                                                               
         LA    R4,INVTAB                                                        
         ST    R4,ANXTINV                                                       
*                                                                               
*        CREATE INVOICE HEADER - BUT DON'T WRITE TO FILE YET                    
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTIHD,R6                                                         
         MVI   PTIHTYP,C'4'                                                     
         MVC   PTIHINV(L'DINVFULL),DINVFULL                                     
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(X'14',PTIHDAT)                         
         MVC   PTIHEST(1),QMEDIA                                                
         MVC   PTIHEST+1(3),PBILKCLT                                            
         MVC   PTIHEST+4(3),PBILKPRD                                            
*                                                                               
*******  MVC   PTIHEST+7(1),UCOMDATA+3  YEAR DIGIT FROM EST VALIDITY YR         
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PTIHEST+7(3),DUB                                                 
*                                                                               
         CLI   MYUSER,C'E'          SEE IF USING EST USER FIELDS                
         BNE   BILL20C                                                          
         MVC   PTIHEST,SPACES                                                   
         MVC   PTIHEST(L'PTIHEST),ESTU2+21  EST UDEF2                           
         OC    PTIHEST,SPACES                                                   
         B     BILL20C                                                          
*                                                                               
BILL20C  DS    0H                                                               
         EJECT                                                                  
*                                                                               
*        INVOICE DETAIL TABLE                                                   
*                                                                               
BILL50   DS    0H                                                               
         L     R4,ANXTINV    POINT TO NEXT ENTRY                                
         MVC   0(2,R4),PBILKMOS      MOS-YEAR                                   
         MVC   2(6,R4),PBILLRCV      ACTUAL BILL AMOUNT                         
         ZAP   DUB,PBILLRCV                                                     
*                                                                               
         AP    INVTOTD,PBILLRCV    INVOICE TOTAL                                
*                                                                               
         SP    DUB,PBILLNET     ACTUAL-NET=COMMISSION?                          
         MVC   8(6,R4),DUB+2                                                    
         LA    R4,INVLEN(R4)                                                    
         ST    R4,ANXTINV                                                       
*                                                                               
         CLI   QOPT5,C'Y'          SEE IF LISTING CUR INVOICES                  
         BNE   POSTB8                                                           
         MVC   P1+3(3),PBILKPRD                                                 
         MVC   P1+8(1),UCOMDATA+3  YEAR DIGIT FROM VALIDITY YEAR                
         MVC   P1+9(3),=C'000'                                                  
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+9(3),DUB                                                      
*                                                                               
         CLI   MYUSER,C'E'         USING ESTIMATE USER FIELDS?                  
         BNE   INVR10                                                           
         MVC   P2+4(3),=C'PM#'                                                  
         MVC   P2+8(16),ESTU2+21    EST UDEF 2 (PM EST #)                       
*                                                                               
INVR10   DS    0H                                                               
*                                                                               
INVR15   GOTO1 DATCON,DMCB,(3,PBILKMOS),(6,P1+14)                               
*                                                                               
INVR17   DS    0H                                                               
         MVC   P1+23(2),PBILLTYP                                                
         B     INVR20                                                           
INVR20   DS    0H                                                               
         CLI   MYUSER,C'E'                                                      
         BNE   *+20                                                             
         MVC   P1+28(1),DINVFULL                                                
         MVC   P1+29(7),DINVFULL+2      SKIPS FIRST C'-'                        
         B     *+10                                                             
         MVC   P1+28(10),DINVFULL                                               
*                                                                               
         ZAP   MYBILLCD,PBILLGRS                                                
         SP    MYBILLCD,PBILLBIL                                                
         ZAP   MYBILLGR,PBILLRCV                                                
         AP    MYBILLGR,MYBILLCD                                                
*                                                                               
         EDIT  (P8,MYBILLGR),(14,P1+37),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P6,PBILLRCV),(14,P1+53),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P8,MYBILLCD),(14,P1+69),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P6,PBILLRCV),(14,P1+85),2,COMMAS=YES,FLOAT=-                    
*                                                                               
INVR25   GOTO1 DATCON,DMCB,(0,PBILLDAT),(5,P1+101)                              
         GOTO1 DATCON,DMCB,(3,PBILDUED),(5,P1+111)                              
*                                                                               
INVR28   MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
*                                                                               
*                                  ROLL TO CURRENT INV TOTALS                   
INVR30   AP    CINVGRS,MYBILLGR    GROSS                                        
         AP    CINVBIL,PBILLRCV                                                 
         AP    CINVCD,MYBILLCD                                                  
         AP    CINVRCV,PBILLRCV    RECEIVABLE                                   
         OI    CINVSW,1                                                         
*                                                                               
POSTB8   DS    0H                                                               
*                                                                               
         B     BILLX                                                            
         EJECT                                                                  
*                                                                               
BILL90  DS     0H                                                               
*                                                                               
BILLX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CKEST    CSECT                                                                  
         NMOD1 0,CKEST                                                          
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R5, R7, R8, R9, RA                         
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
         MVI   BILLONLY,C'N'                                                    
*                                                                               
         DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
         MVC   PPGAREC,AREC                                                     
         MVI   CKESTSW,0    WILL BE SET TO X'01' IF I READ SOMETHING            
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBUYREC                                                   
         MVI   KEY+3,X'07'         MUST READ EST FOR START YEAR                 
         MVC   KEY+4(6),PBUYREC+4   CLT AND PRD                                 
         MVC   KEY+10(2),PBUYREC+19   EST                                       
         XC    KEY+12(19),KEY+12                                                
         CLI   CKESTREC,C'B'        FROM BUY                                    
         BE    CKEST3                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBILLREC                                                  
         MVI   KEY+3,X'07'         MUST READ EST FOR START YEAR                 
         MVC   KEY+4(6),PBILLREC+4  CLT AND PRD                                 
         MVC   KEY+10(2),PBILKEST     EST                                       
         XC    KEY+12(19),KEY+12                                                
         CLI   CKESTREC,C'L'        FROM BILL                                   
         BE    CKEST3                                                           
         DC    H'0'                 ERROR                                       
*                                                                               
CKEST3   CLC   PESTREC(12),KEY      SEE IF I ALREADY HAVE EST                   
         BE    CKEST5                                                           
         MVI   CKESTSW,1                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                EST MUST BE ON FILE                          
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         BAS   RE,ESTF            ADD TO ESTTABLE                               
*                                                                               
CKEST5   DS    0H                                                               
         CLC   QAGENCY,=C'YN'     SPECIAL CHECK FOR YNR                         
         BNE   CKEST5C                                                          
         CLI   PESTGRPS+1,C'B'    BILL ONLY ESTIMATE                            
         BNE   CKEST5C            (NO CHANGES)                                  
         MVI   BILLONLY,C'Y'                                                    
*                                                                               
CKEST5C  DS    0H                                                               
         XC    ESTU1,ESTU1       CLEAR EST USER FIELDS                          
         XC    ESTU2,ESTU2                                                      
         MVC   ESTFORMU,PESTBILP                                                
**                                                                              
**       SKIP  THE CODE BELOW     NOT NEEDED FOR THIS REPORT?                   
**                                                                              
******   B     CKEST82                                                          
*                                                                               
         CLI   MYUSER,C'E'         SEE IF USER FIELDS IN USE                    
         BNE   CKEST50B                                                         
         B     CKEST50C                                                         
*                                                                               
CKEST50B DS    0H                                                               
         CLI   MYUSER,C'Y'         SEE IF USER FIELDS IN USE                    
         BNE   CKEST50                                                          
*                                                                               
CKEST50C CLC   PESTKPRD,=C'AAA'     NOT FOR PRD AAA                             
         BE    CKEST50                                                          
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'E',PESTREC),ESTU1,ESTU2          
         CLI   DMCB,X'FF'                                                       
         BE    UESTERR                                                          
*                                                                               
         CLI   MYUSER,C'E'                                                      
         BE    *+12                                                             
*                                                                               
         CLI   ESTU1+21,C' '    MUST FIND DATA                                  
         BNH   UESTERR                                                          
         CLI   ESTU2+21,C' '    MUST FIND DATA                                  
         BNH   UESTERR                                                          
         B     CKEST50                                                          
*                                                                               
UESTERR  DS    0H                                                               
         LA    R1,BADESTS                                                       
         MVC   WORK(3),PPRDKPRD                                                 
         MVC   WORK+3(2),PESTKEST                                               
UESTERR2 CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    UESTERR3                                                         
         CLC   WORK(5),0(R1)                                                    
         BE    CKEST50                                                          
         LA    R1,6(R1)                                                         
         B     UESTERR2                                                         
*                                                                               
UESTERR3 MVC   0(5,R1),WORK                                                     
         MVI   5(R1),0                                                          
         MVI   6(R1),X'FF'        SET NEW END OF TABLE                          
*                                                                               
         MVC   P1(35),=C'*** MISSING ESTIMATE USER FIELD ***'                   
         MVC   P1+40(3),PPRDKPRD                                                
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+45(3),DUB                                                     
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'           SEE IF TEST RUN                             
         BE    CKEST50                                                          
         MVC   P2(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
*                                                                               
CKEST50  DS    0H                                                               
*                                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
         XC    PCONREC(200),PCONREC                                             
         CLC   PESTCOM,SPACES                                                   
         BNH   CKEST80                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   WORK2+6(6),PESTCOM                                               
         MVC   WORK2(6),SPACES                                                  
         LA    R2,WORK2+6                                                       
         CLI   5(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVC   KEY+4(6),0(R2)                                                   
         CLC   PCONREC(10),KEY       SEE IF ALREADY THERE                       
         BE    CKEST80                                                          
*                                                                               
*                                                                               
         MVI   CKESTSW,1                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MISSING STANDARD COMMENT                     
*                                                                               
*NOP*    LA    R0,PCONREC          READ INTO CONTRACT AREA                      
*NOP*    ST    R0,AREC                                                          
         MVC   AREC,ACONIO1        ACONIO1=A(PCONREC)                           
         GOTO1 GETPRT                                                           
*                                                                               
         DROP  RF                                                               
**                                                                              
**                                                                              
**                                                                              
CKEST80  DS    0H                                                               
**       CLI   MYUSER,C'E'             CHECK IF FOR H9                          
**       BNE   CKEST82                                                          
**       CLI   AAAPRD,C'Y'             CHECK IF WE HAVE AAA PRD                 
**       BNE   CKEST82                                                          
**       CLI   CKESTREC,C'B'           CHECK IF WE CAME FROM BUY                
**       BNE   CKEST82                                                          
**       XC    KEY,KEY                 BUILD A KEY TO READ                      
**       MVC   KEY(3),PBUYREC                                                   
**       MVI   KEY+3,X'07'             AAA EST                                  
**       MVC   KEY+4(3),PBUYREC+4                                               
**       MVC   KEY+7(3),=C'AAA'                                                 
**       MVC   KEY+10(2),PBUYREC+19                                             
**       XC    KEY+12(19),KEY+12                                                
**                                                                              
**       GOTO1 HIGH                                                             
**       CLC   KEY(25),KEYSAVE                                                  
**       BNE   CKEST82                                                          
**       GOTO1 GETPRT                                                           
**       MVC   AAAESTFR,PESTBILP       GET FORMULA                              
**                                                                              
CKEST81  DS    0H                                                               
**       XC    KEY,KEY                 RE READ EST RECORD                       
**       MVC   KEY(3),PBUYREC                                                   
**       MVI   KEY+3,X'07'                                                      
**       MVC   KEY+4(6),PBUYREC+4                                               
**       MVC   KEY+10(2),PBUYREC+19                                             
**       XC    KEY+12(19),KEY+12                                                
**       GOTO1 HIGH                                                             
**       CLC   KEY(25),KEYSAVE                                                  
**       BE    *+6                                                              
**       DC    H'0'                                                             
**       LA    R0,PESTREC                                                       
**       ST    R0,AREC                                                          
**       GOTO1 GETPRT                                                           
*                                                                               
CKEST82  CLI   QOPT1,C'I'       INVOICE REPORT?                                 
         BNE   CKEST99                                                          
*                                                                               
*        MUST DO UCOMM LOGIC NOW FOR INVOICE REPORT                             
*        SINCE PPFILCON DOESN'T READ THE ESTIMATE                               
*        BEFORE PASSING MODE FBILEST                                            
*                                                                               
*                                                                               
CEST     DS    0H                  ESTIMATE FIRST                               
         XC    UCOMDATA,UCOMDATA   CLEAR                                        
*                                                                               
         LA    R4,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R4                                                       
*                                                                               
         MVI   CKESTSW,1          SOMETHING READ                                
         MVC   UCPRD,PESTKPRD                                                   
CEST1    MVC   UCACOMF,VCOMFACS     COMFACS                                     
         MVI   UCSYS,C'P'        SYSTEM TO PRINT                                
         MVC   UCAGY,QAGENCY     AGENCY                                         
         MVC   UCMED,QMEDIA      MEDIA                                          
         MVC   UCCLT,PESTKCLT    CLIENT                                         
*                                DO UCOMM FOR PRD AAA                           
         OI    UCOPT,UCOEST     RETURN ESTIMATE UCOMMS                          
         MVC   UCEST,PESTKEST                                                   
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK    NEW UCOM CALL SINCE GOTO MACRO                
         CLI   UCERROR,0         TRASHED WRKING STORAGE USED BY DDUCOM          
         BNE   CEST3X       ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST    NO DATA?                                      
         BNO   CEST2                                                            
         CLC   UCPRD,=C'AAA'      DID I JUST TRY PRD AAA?                       
         BE    CEST2X                                                           
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         MVC   UCPRD,=C'AAA'      IF NOT FOUND TRY PRD AAA                      
         B     CEST1                                                            
*                                                                               
CEST2    XC    UCTTLS(UCALL),UCTTLS                                             
         L     RE,UCETTLS     EST TITLES                                        
         MVC   UCTTLS,0(RE)   SAVE INFO IN MY STORAGE                           
         L     RE,UCEDATA     EST DATA                                          
         MVC   UCOMDATA,0(RE)                                                   
*                                                                               
CEST2X   DS    0H                                                               
*                                                                               
         OC    UCOMDATA(4),UCOMDATA   1ST IS VALIDITY YEAR                      
         BNZ   CEST3X                                                           
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),UCPRD    DISPLAY PRD I LAST SEARCHED FOR                
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+25(3),DUB+6(2)                                                
         MVC   P1+31(24),=C'MISSING VALIDITY YEAR **'                           
         MVI   RCSUBPRG,10                                                      
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'      TEST RUN?                                        
         BE    CEST3X          KEEP GOING                                       
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
CEST3X   DS    0H                                                               
CKEST99  MVC   KEY,PPGKEY                                                       
         MVC   AREC,PPGAREC                                                     
         CLI   CKESTSW,1           SEE IF I READ SOMETHING                      
         BNE   CKESTX              NO - SKIP READ HIGH                          
         GOTO1 HIGH                                                             
CKESTX   XIT1                                                                   
*                                                                               
         EJECT                                                                  
ESTF     NTR1                                                                   
         CLI   PESTSTAT,C'1'                                                    
         BE    ESTFX                                                            
         LA    R1,250                                                           
         LA    R2,ESTLLST                                                       
ESTF5    CLC   0(2,R2),PESTKEST                                                 
         BE    ESTFX                                                            
         OC    0(2,R2),0(R2)                                                    
         BNZ   ESTF10                                                           
         MVC   0(2,R2),PESTKEST                                                 
         B     ESTFX                                                            
*                                                                               
ESTF10   LA    R2,2(R2)                                                         
         BCT   R1,ESTF5                                                         
         DC    H'0'                TOO MANY UNLOCKED ESTS                       
*                                                                               
ESTFX    XIT                                                                    
         LTORG                                                                  
CKESTSW  DC    X'00'                                                            
*****                                                                           
*                                                                               
TITLES   CSECT                                                                  
         DS    0C                                                               
         DC    CL30'PRINT- INV. FILE HEADER'                                    
         DC    CL30'            INV. HEADER'                                    
         DC    CL30'              INV. LINE'                                    
         DC    CL30'           INV. CONTROL'                                    
         DC    CL30'        EST FILE HEADER'                                    
         DC    CL30'             EST HEADER'                                    
         DC    CL30'               EST LINE'                                    
         DC    CL30'             SPEND LINE'                                    
         DC    CL30'            EST CONTROL'                                    
*                                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*             TABLE OF RECORD TYPES,LENGTHS AND COUNTS                          
* SPOT/NET                                                                      
LENTAB   CSECT                                                                  
         DC    CL2'I1',AL2(PTIFHX),PL4'0'                                       
         DC    CL2'I4',AL2(PTIHX),PL4'0'                                        
         DC    CL2'I5',AL2(PTILX),PL4'0'                                        
         DC    CL2'I9',AL2(PTIFCX),PL4'0'                                       
         DC    CL2'E1',AL2(PTEFHX),PL4'0'                                       
         DC    CL2'E4',AL2(PTEHX),PL4'0'                                        
         DC    CL2'E5',AL2(PTELX),PL4'0'                                        
         DC    CL2'E6',AL2(PTESLX),PL4'0'                                       
         DC    CL2'E9',AL2(PTEFCX),PL4'0'                                       
         DC    XL4'00',PL4'0'      EXTRA LINE                                   
         DC    X'FFFF'                                                          
         EJECT                                                                  
PPPTWRKD DSECT                                                                  
NETOPT   DS    CL1                                                              
SVQOPT1  DS    CL1          SO I'LL KNOW AT RUNLAST WHICH FILE TYPE             
SVQOPT6  DS    CL1          SO I'LL KNOW AT RUNLAST IF TEST RUN                 
SVQOPT7  DS    CL1          SO I'LL KNOW AT RUNLAST IF PDUMPING                 
*                                                                               
EHDRSW   DS    CL1        GETS SET TO Y WHEN EST FILE HEADER WRITTEN            
IHDRSW   DS    CL1        GETS SET TO Y WHEN INVOICE FILE HDR WRITTEN           
ERRSW    DS    CL1                                                              
RECTYPE  DS    CL2          USED BY MWRITE                                      
VENDOR   DS    CL6          SET FROM VENTAB                                     
PAYEE    DS    CL7          SET FROM VENTAB                                     
TOTCNT   DS    PL4'0'                                                           
MYDUB    DS    PL8                                                              
NETDUB   DS    PL8                                                              
CDDUB    DS    PL8                                                              
SDUB     DS    PL8          USED FOR SHARES IN PROCNET                          
SNETDUB  DS    PL8          USED FOR SHARES IN PROCNET                          
SVNUPRD2 DS    XL1          SAVED NUPRD2                                        
*                                                                               
MYDUMP   DS    XL2                                                              
*                                                                               
ALLOWSW  DS    XL1                                                              
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
         DS    0F          ALIGNMENT FOR WK                                     
*                                                                               
WK       DS    CL20                                                             
MYAMTD   DS    F           CALCULATED AMT DUE FOR INSERTION                     
WPRD     DS    XL1                                                              
*                                                                               
WRKDATE  DS    XL8                                                              
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
SVSTART  DS    CL6                                                              
SVEND    DS    CL6                                                              
SVQSTART DS    XL3                                                              
SVQEND   DS    XL3                                                              
*                                                                               
B1PROF   DS    CL16                                                             
B1XPROF  DS    CL16                                                             
*                                                                               
DINVFULL DS    CL10                                                             
*                                                                               
LINVFULL DS    CL10     SAVED                                                   
LBQDATE  DS    CL6      SAVED                                                   
*                                                                               
INVTOTD  DS    PL5      TOTAL $ FOR INV NUMBER                                  
LASTBILL DS    CL1      Y= LAST BILL                                            
FRSTBILL DS    CL1      Y= FIRST BILL                                           
INVRCNT  DS    PL5      COUNT OF INVOICE FILE RECORDS                           
ESTRCNT  DS    PL5      COUNT OF ESTIMATE FILE RECORDS                          
*                                                                               
WORK2    DS    CL64                                                             
ALENTAB  DS    A                                                                
ATITLES  DS    A                                                                
VGETUSER DS    A                                                                
VGETCOST DS    A                                                                
VDDUCOM  DS    A                                                                
AFMTINO  DS    A                                                                
ACONIO1  DS    A                                                                
ANXTINV  DS    A                                                                
BUFFIO   DS    A                                                                
BUFFBUFF DS    A                                                                
*                                                                               
MYUSER   DS    CL1       SET FROM AGYTAB AT FBUYREQ                             
*                                                                               
ESTU1    DS    CL54      USER FIELDS                                            
ESTU2    DS    CL38                                                             
PRDU1    DS    CL54                                                             
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
OUTREC   DS    CL500                                                            
*                                                                               
*                                                                               
ESTLLST  DS    CL500               LIST OF UNLOCKED ESTS (2 BYTES PER)          
*                                                                               
BADESTS  DS    CL240            ROOM FOR 40 BAD PRD/ESTS                        
*                                PRD(3)/EST(2)/+ ONE BYTE                       
*                                TO BE USED FOR ERRORS                          
*                                                                               
         DS    CL50             SPARE                                           
*                                                                               
         BUFF  LINES=4000,ROWS=1,COLUMNS=3,FLAVOR=PACKED,COMMENT=100,KEX        
               YLIST=(22,A)                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDUCOMD                                                        
         EJECT                                                                  
       ++INCLUDE PTIFILED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREPPT02 01/05/12'                                      
         END                                                                    
