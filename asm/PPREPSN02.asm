*          DATA SET PPREPSN02  AT LEVEL 007 AS OF 06/11/10                      
*PHASE PPSN02A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE DDUCOM                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE PPBVAL                                                                 
*INCLUDE PERVAL                                                                 
         TITLE 'PPSN02 - SONY INTERFACE'                                        
***************************************************************                 
***************************************************************                 
*        DO NOT RUN A MIXTURE OF ESTIMATE AND INVOICE FILE                      
*        TAPE REQUESTS IN THE SAME JOB STREAM - IT WON'T WORK                   
*     ** ACTUALLY THIS PROGRAM MIGHT WORK THAT WAY **                           
***************************************************************                 
***************************************************************                 
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*   BPLA  FEB/05 ZERO ADDED BEFORE INVOICE NUMBER                               
*         INVOICE RECORD EXPANDED TO 627                                        
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
PPSN02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPSN02,RR=R9                                                   
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
         USING PPSNWRKD,R8                                                      
         MVC   ACONIO1,ACONIO                                                   
         LA    R7,PPSN02+4095                                                   
         LA    R7,1(R7)                                                         
         USING PPSN02+4096,R7     **NOTE USE OF R7 AS BASE REG*                 
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
         MVI   ETAPESW,0                                                        
         MVI   ALLOWSW,0     DYNAMIC ALLOCATION INV. FILE NOT DONE              
         MVI   ELLOWSW,0     DYNAMIC ALLOCATION EST. FILE NOT DONE              
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
**REG**                                                                         
         L     R0,=A(REGTAB)                                                    
         A     R0,RELO                                                          
         ST    R0,AREGTAB                                                       
         ST    R0,ANXTREG                                                       
         L     R0,=A(REGTABX)                                                   
         A     R0,RELO                                                          
         ST    R0,AREGTABX         ADDRESS OF END OF TABLE                      
**REG**                                                                         
*                                                                               
         L     R2,ALENTAB          ZAP ACCUMS                                   
         LA    R3,2                                                             
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
         MVC   DYNDDN,=CL8'PSNTAPE'          INVOICE FILE                       
         MVC   DYNDSN,=CL20'PRTTAPE.PP0SNXXI'                                   
         MVC   DYNDSN+13(2),QAGENCY                                             
*                                                                               
         MVC   EDYNDDN,=CL8'PSNTAPE'         ESTIMATE FILE                      
         MVC   EDYNDSN,=CL20'PRTTAPE.PP0SNXXE'                                  
         MVC   EDYNDSN+13(2),QAGENCY                                            
*                                                                               
REQF10   DS    0H                                                               
         XC    LBILLKEY,LBILLKEY                                                
         XC    LESTOUT,LESTOUT                                                  
         MVI   FCRDBILL,C'Y'                                                    
         MVC   SVQOPT1,QOPT1      SAVE FILE TYPE                                
         MVC   SVQOPT6,QOPT6      SAVE DO TAPE OPTION                           
         MVC   SVQOPT7,QOPT7      SAVE PDUMPING OPTION                          
*                                                                               
         LA    RE,VENTAB           VENDOR TABLE                                 
REQF1C   CLI   0(RE),X'FF'         AGENCY NOT IN MY TABLE                       
         BNE   *+6                                                              
         DC    H'0'            DIE FOR NOW                                      
         CLC   QAGENCY,0(RE)                                                    
         BE    REQF1D                                                           
         LA    RE,12(RE)                                                        
         B     REQF1C                                                           
*                                                                               
REQF1D   DS    0H                                                               
         MVC   REQUSER,2(RE)     REQUESTING USER                                
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
         CLI   QOPT1,C'I'        INVOICE FILE?                                  
         BNE   REQF73                                                           
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
         OPEN  (OUTFILE,OUTPUT)                                                 
         PRINT NOGEN                                                            
         B     REQF75                                                           
*                                                                               
REQF73   CLI   QOPT1,C'E'        ESTIMATE FILE?                                 
         BE    *+6                                                              
         DC    H'0'              UNKNOWN FILE TYPE                              
*                                                                               
         CLI   ETAPESW,C'N'      SEE IF A PRIOR REQUEST WAS TEST                
         BE    MIXERR                                                           
         MVI   ETAPESW,C'Y'      SET TAPE BEING PRODUCED                        
*                                                                               
         TM    ELLOWSW,X'01'     DYNAMIC ALLOCATION DONE ALREADY?               
         BO    REQF75                                                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,EDYNDDN),(0,EDYNDSN)                            
         OI    ELLOWSW,X'01'    SO I WON'T DO AGAIN                             
         OPEN  (EOUTFIL,OUTPUT)                                                 
*                                                                               
REQF75   B     FIRSTB0X                                                         
*                                                                               
FIRSTB0  DS    0H                                                               
         CLI   QOPT1,C'I'       INVOICE FILE?                                   
         BNE   FIRSTB0E                                                         
         CLI   TAPESW,C'Y'      SEE IF A PRIOR REQUEST WAS LIVE                 
         BE    MIXERR                                                           
         MVI   TAPESW,C'N'                                                      
         B     FIRSTB0X                                                         
*                                                                               
FIRSTB0E DS    0H                                                               
         CLI   QOPT1,C'E'       ESTIMATE FILE?                                  
         BE    *+6                                                              
         DC    H'0'        UNKNOWN FILE TYPE                                    
*                                                                               
         CLI   ETAPESW,C'Y'     SEE IF A PRIOR REQUEST WAS LIVE                 
         BE    MIXERR                                                           
         MVI   ETAPESW,C'N'                                                     
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
*                                  SAVE DATES FOR ACTIVITY CHECKING             
         CLC   QEND,SPACES                                                      
         BNE   *+10                                                             
         MVC   QEND,QSTART     SET END TO START IF NOT GIVEN                    
*                                                                               
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
         MVI   LASTBILL,C'Y'        LAST BILL FOR CLIENT                        
         CLI   QOPT1,C'E'           ESTIMATE FILE?                              
         BNE   LCLI5                                                            
         OC    LBILLKEY,LBILLKEY     DID I PROCESS ANY?                         
         BZ    EXIT                                                             
         MVI   LBILLKEY+3,X'07'                                                 
         CLC   LESTOUT,LBILLKEY   HAVE I ALREADY DONE?                          
         BE    EXIT                                                             
*                            MUST NOW GO PROCESS LAST ESTIMATE                  
         MVI   CKESTREC,C'L'                                                    
         GOTO1 =A(CKEST)                                                        
         GOTO1 =A(ESTREC)                                                       
         B     EXIT                                                             
*                                                                               
LCLI5    DS    0H                                                               
*****    GOTO1 =A(INVOICE)       NO-OP CALL FOR LAST BILL                       
*****                            MAY NEED IF MULTIPLE MOS                       
*****                            ON ONE INVOICE AND I NEED TO TOTAL             
         B     EXIT                                                             
*                                                                               
*                                   FIRST FOR CLIENT                            
FCLI     DS    0H                                                               
*                                                                               
*                                                                               
         L     RF,AREGTAB    CLEAR REGION TABLE                                 
         LA    R4,39         CLEAR 7800 BYTES                                   
FCLI2    XC    0(200,RF),0(RF)                                                  
         LA    RF,200(RF)                                                       
         BCT   R4,FCLI2                                                         
         L     RF,AREGTAB                                                       
         ST    RF,ANXTREG                                                       
*                                                                               
         XC    PREGREC,PREGREC     CLEAR REGION RECORD                          
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    B4APROF,B4APROF      B4A PROFILE                                 
         XC    B5APROF,B5APROF      B5A PROFILE                                 
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
         MVC   WORK(4),=C'PB4A'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B4APROF,DATAMGR                                
         MVC   B4APROF+5(1),B4APROF+9   MOVE AC CONTROL AFTER OTHERS            
         MVC   WORK(4),=C'PB5A'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B5APROF,DATAMGR                                
         MVC   B5APROF+5(1),B5APROF+9   MOVE AC CONTROL AFTER OTHERS            
         MVC   WORK(4),=C'PB6A'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B6APROF,DATAMGR                                
         MVC   B6APROF+5(1),B6APROF+9   MOVE AC CONTROL AFTER OTHERS            
         MVC   WORK(4),=C'PB7A'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B7APROF,DATAMGR                                
         MVC   B7APROF+5(1),B7APROF+9   MOVE AC CONTROL AFTER OTHERS            
         EJECT                                                                  
         CLI   QOPT1,C'E'         ESTIMATE FILE ONLY?                           
         BE    EXIT               DONE                                          
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
FPRD     DS    0H                  FIRST BILL FOR PRODUCT                       
         XC    PREGREC,PREGREC         CLEAR REGION RECORD                      
*                                                                               
*                                      COMPANY AND OFFICE CODE                  
*                                      FROM PRODUCT INTERFACE                   
*                                                                               
         MVC   SVPRDIFC,SPACES                                                  
         LA    R2,PPRDREC+33                                                    
         USING PPRDICEL,R2                                                      
         MVI   ELCODE,X'30'      LOOK FOR INTERFACE CODE ELEMENT                
         BAS   RE,NEXTEL                                                        
         BNE   FPRD5                                                            
         MVC   SVPRDIFC,PPRDINFC    SAVE FIRST 4 CHARACTERS                     
*                                                                               
         DROP  R2                                                               
*                                                                               
FPRD5    DS    0H                                                               
         B     FPRDX               DON'T NEED TO DO ANYTHING ELSE?              
*                                                                               
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
         XC    PREGREC,PREGREC     CLEAR REGION RECORD                          
         CLI   QOPT1,C'E'          ESTIMATE FILE?                               
         BE    FESTX               NOTHINGTO DO                                 
*                                                                               
FEST0    DS    0H                                                               
         B     FESTX               DON'T NEED TO DO ANYTHING                    
*                                                                               
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
         UNPK  P1+25(3),DUB                                                     
         MVC   P1+31(21),=C'MISSING FIRST UCOM **'                              
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
FESTX    B     EXIT                                                             
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
**REG**                                                                         
         GOTO1 =A(CKREG)   PUTS THIS BILL'S DIV/REG/NAME IN REGTAB              
**REG**                                                                         
         CLI   QOPT1,C'E'         ESTIMATE FILE?                                
         BNE   BILLI                                                            
         OC    LBILLKEY,LBILLKEY        FIRST TIME?                             
         BZ    BILLB                                                            
         CLC   PBILLREC(12),LBILLKEY    CHANGE OF ESTIMATE?                     
         BE    EXIT                     NO - JUST EXIT                          
*                                                                               
         L     RF,ANXTREG             ONLY CLEAR IF NOT AT TOP                  
         L     RE,AREGTAB                                                       
         CR    RE,RF                                                            
         BE    BILLA                                                            
         SH    RF,=H'26'              CLEAR LAST ENTRY                          
         XC    0(26,RF),0(RF)         AS IT IS FOR THE NEXT EST                 
         XC    PREGREC,PREGREC     CLEAR REGION RECORD                          
*                                                                               
BILLA    MVI   CKESTREC,C'L'          FROM BILLREC                              
         GOTO1 =A(CKEST)              REREADS 'OLD' ESTIMATE                    
         GOTO1 =A(ESTREC)                                                       
*                          RE-ADD TO REGION TABLE                               
**REG**                                                                         
         GOTO1 =A(CKREG)   PUTS THIS BILL'S DIV/REG/NAME IN REGTAB              
**REG**                                                                         
*                                                                               
BILLB    MVC   LBILLKEY(12),PBILLREC     SAVE KEY DATA                          
         B     EXIT                                                             
*                                                                               
BILLI    GOTO1 AFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),                     X        
               (PBILKMED,B1PROF),B1XPROF                                        
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)      FULL INVOICE NUMBER                          
*                                                                               
         L     RF,DMCB+4        ADDRESS OF "SHORT" FORMAT                       
         MVC   WORK+2(4),3(RF)     DON'T MOVE '-'                               
         L     RF,DMCB+8        ADDRESS OF Y/M                                  
         MVC   WORK(2),0(RF)                                                    
         MVC   DINVNO(6),WORK                                                   
*                                                                               
*                                                                               
         GOTO1 APPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
*                                                                               
*        SET SOME VALUES INTO PBILLREC                                          
*                                                                               
         MVC   PBILLGRS,PPBVEBG                                                 
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
*                                                                               
         MVI   CKESTREC,C'L'       SET FROM BILL                                
         GOTO1 =A(CKEST)           READ EST AND COMMENT                         
*                                                                               
         GOTO1 =A(INVOICE)                                                      
         B     EXIT                                                             
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
         LA    R6,2                FOR BCT                                      
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
         CLI   TAPESW,C'Y'          SEE IF PRODUCING INV. TAPE                  
         BNE   TOT5                                                             
         CLOSE (OUTFILE,)                                                       
*                                                                               
TOT5     CLI   ETAPESW,C'Y'         SEE IF PRODUCING EST. TAPE                  
         BNE   EXIT                                                             
         CLOSE (EOUTFIL,)                                                       
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         DS    XL2000   FOR NOW - JUST SO ROUTINES BELOW CAN BE                 
*                       ACCESSED BY OTHER MODULES                               
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
**FB**   LH    R3,HALF                                                          
**FB**   LA    R3,4(R3)                                                         
**FB**   STH   R3,OUTREC-4                                                      
         CLI   SVQOPT7,C'P'                                                     
         BE    WRIT1                                                            
         CLI   QOPT7,C'P'                                                       
         BNE   WRIT2                                                            
WRIT1    MVC   P1+1(125),OUTREC                                                 
         MVC   P2+1(90),OUTREC+125                                              
         MVI   RCSUBPRG,20                                                      
         CLC   RECTYPE,=C'E1'      ESTIMATE RECORD?                             
         BE    WRIT1A                                                           
         MVI   RCSUBPRG,10                                                      
         MVC   P2+1(125),OUTREC+125                                             
         MVC   P3+1(125),OUTREC+250                                             
         MVC   P4+1(125),OUTREC+375                                             
         MVC   P5+1(127),OUTREC+500                                             
         MVI   P3,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P4,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P5,0       SO LINE WILL ALWAYS PRINT                             
*                                                                               
WRIT1A   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
         MVI   RCSUBPRG,20                                                      
         GOTO1 HEXOUT,DMCB,OUTREC-4,P1+10,54,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+50,P2+18,50,=C'N'                               
         GOTO1 (RF),(R1),OUTREC+100,P3+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+150,P4+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+200,P5+18,15,=C'N'                              
         CLC   RECTYPE,=C'E1'         ESTIMATE RECORD?                          
         BE    WRIT1C                                                           
         MVI   RCSUBPRG,10                                                      
         GOTO1 (RF),(R1),OUTREC+200,P5+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+250,P6+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+300,P7+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+350,P8+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+400,P9+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+450,P10+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+500,P11+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+550,P12+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+600,P13+18,27,=C'N'                             
WRIT1C   MVC   P1+1(7),=C'001-050'                                              
         MVC   P2+1(7),=C'051-100'                                              
         MVC   P3+1(7),=C'101-150'                                              
         MVC   P4+1(7),=C'151-200'                                              
         MVC   P5+1(7),=C'201-215'                                              
         CLC   RECTYPE,=C'E1'         ESTIMATE RECORD?                          
         BE    WRIT1E                                                           
         MVC   P5+1(7),=C'201-250'                                              
         MVC   P6+1(7),=C'251-300'                                              
         MVC   P7+1(7),=C'301-350'                                              
         MVC   P8+1(7),=C'351-400'                                              
         MVC   P9+1(7),=C'401-450'                                              
         MVC   P10+1(7),=C'451-500'                                             
         MVC   P11+1(7),=C'501-550'                                             
         MVC   P12+1(7),=C'551-600'                                             
         MVC   P13+1(7),=C'600-627'                                             
WRIT1E   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   SVQOPT6,C'Y'     SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LA    R1,OUTFILE                                                       
         CLI   QOPT1,C'I'       INVOICE FILE                                    
         BE    WRIT2B                                                           
         LA    R1,EOUTFIL       MUST BE ESTIMATE FILE                           
         CLI   QOPT1,C'E'                                                       
         BE    *+6                                                              
         DC    H'0'              SOMETHING VERY WRONG                           
*                                                                               
**FB**   LA    R0,OUTREC-4                                                      
WRIT2B   LA    R0,OUTREC                                                        
         PRINT GEN                                                              
         PUT   (1),(0)                                                          
         PRINT NOGEN                                                            
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
         LTORG                                                                  
*                                                                               
VENTAB   DS    0C                                                               
         DC    C'MC',CL10'SONYBILLR1'           MCCANN                          
         DC    C'SJ',CL10'REQ USER'             SJR TESTING                     
         DC    C'TC',CL10'REQ USER'             TRAINING CENTER                 
         DC    X'FFFF'         END OF TABLE                                     
*                                                                               
         EJECT                                                                  
*        AGENCY TABLE                                                           
*                                                                               
*        AGENCY CODE/VENDOR/COMPANY/MYUSER VALUE                                
*                                   E=ESTIMATE USER FIELDS                      
*                                                                               
AGYTAB   DC    C'MC',C'E'                                                       
         DC    C'TC',C'E'                                                       
         DC    C'SJ',C'E'                                                       
         DC    X'FFFF'                                                          
*                                                                               
AGYTABL  EQU   3                                                                
         EJECT                                                                  
*                                                                               
         PRINT GEN                                                              
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=FB,LRECL=00627,           X        
               BLKSIZE=00627,MACRF=PM                                           
EOUTFIL  DCB   DDNAME=EOUTFIL,DSORG=PS,RECFM=FB,LRECL=00215,           X        
               BLKSIZE=00215,MACRF=PM                                           
         PRINT NOGEN                                                            
         EJECT                                                                  
*                         CREATE ESTIMATE FILE RECORD                           
*                         MODE WILL BE FBILLEST                                 
ESTREC   CSECT                                                                  
         NMOD1 0,ESTREC                                                         
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8 AND R9                              
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
         MVC   LESTOUT,PESTREC   SAVE KEY OF LAST EST PROCESSED                 
*                                                                               
**REG**                                                                         
         L     R5,AREGTAB                                                       
EST5     DS    0H                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(132),SPACES                                           
*                                                                               
         USING PSEFILD,R6                                                       
         MVC   PSEFMED(1),PESTKMED                                              
         MVC   PSEFCLT(3),PESTKCLT                                              
         MVC   PSEFPRD(3),PESTKPRD                                              
         MVI   PSEFPGRP,C'N'                                                    
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSEFEST(3),DUB                                                   
         MVC   PSEFENM,PESTNAME                                                 
**REG**                                                                         
         CLC   0(3,R5),=C'RHV'     NO REGIONS FOR RHV                           
         BE    EST8                                                             
         MVC   PSEFEST+3(3),3(R5)  REGION CODE                                  
         MVC   PSEFENM3,6(R5)      REGION NAME                                  
**REG**                                                                         
EST8     DS    0H                                                               
         OC    PSEFENM,SPACES                                                   
         MVC   PSEFENM2,PESTNAM2                                                
         OC    PSEFENM2,SPACES                                                  
         GOTO1 DATCON,DMCB,(0,PESTST),(X'14',WORK)                              
         MVC   PSEFSTR(2),WORK+4   MONTH                                        
         MVI   PSEFSTR+2,C'/'                                                   
         MVC   PSEFSTR+3(2),WORK+6  DAY                                         
         MVI   PSEFSTR+5,C'/'                                                   
         MVC   PSEFSTR+6(4),WORK    YYYY                                        
         GOTO1 DATCON,DMCB,(0,PESTEND),(X'14',WORK)                             
         MVC   PSEFEND(2),WORK+4   MONTH                                        
         MVI   PSEFEND+2,C'/'                                                   
         MVC   PSEFEND+3(2),WORK+6  DAY                                         
         MVI   PSEFEND+5,C'/'                                                   
         MVC   PSEFEND+6(4),WORK    YYYY                                        
*                                                                               
         MVI   PSEFLOCK,C'N'                                                    
         MVC   PSEFREV,ZEROS       NO REVISION NUMBER                           
         MVI   PSEFCID,C'N'                                                     
         MVI   PSEFESTA,C'A'       ESTIMATE STATUS                              
         MVC   PSEFCPY,=C'02'      COMPANY CODE                                 
         MVC   PSEFOFF,=C'06'      OFFICE CODE                                  
         MVI   PSEFSTW,C'N'        STEWARDSHIP                                  
         MVI   PSEFBTR,C'N'        BARTER                                       
         MVI   PSEFCAD,C'N'        CLASSIFED                                    
         MVC   PSEFCPY2,=C'02'     COMPANY CODE  (BILLING)                      
         MVC   PSEFOFF2,=C'06'     OFFICE CODE   (BILLING)                      
         MVC   PSEFCPY3,=C'02'     COMPANY CODE  (PAYING)                       
         MVC   PSEFOFF3,=C'37'     OFFICE CODE   (PAYING)                       
         MVI   PSEFBPG,C'N'        BILL PRODUCT GROUP                           
         MVI   PSEFFLG,C'Y'        BILL FLAG FOR ESTIMATE                       
*                                                                               
*        PSEFRCD- REVISION CHANGE DATE - WHAT GOES HERE?                        
*                                                                               
         MVI   PSEFMGE,C'N'   MEMBER OF GROUP EST                               
*                                                                               
         MVC   PSEFBUD,ZEROS       BUDGET - ZEROS FOR NOW                       
         MVI   PSEFBUDE,C'G'       EST. BUDGET QUALIFER                         
         MVC   RECTYPE,=C'E1'                                                   
         BAS   RE,MWRITE                                                        
         AP    ESTRCNT,=P'1'                                                    
*                                                                               
         MVC   P1+1(3),PESTKPRD                                                 
         MVC   P1+6(3),PSEFEST                                                  
         MVI   P1+9,C'/'                                                        
         MVC   P1+10(3),PSEFEST+3   (REGION)                                    
         MVC   P1+15(10),PSEFSTR                                                
         MVC   P1+27(10),PSEFEND                                                
         MVC   P1+40(20),PESTNAME                                               
         MVC   P2+40(20),PESTNAM2                                               
         MVC   P3+40(20),6(R5)         REGION NAME                              
         MVI   RCSUBPRG,20                                                      
         BAS   RE,MYRPT                                                         
         LA    R5,REGTABL(R5)          BUMP TO NEXT REGION                      
         CLI   0(R5),0                                                          
         BNE   EST5                    ZERO MEANS END OF TABLE                  
*                                                                               
         L     RF,AREGTAB    CLEAR REGION TABLE FOR EACH ESTIMATE               
         LA    R4,39         CLEAR 7800 BYTES                                   
EST20    XC    0(200,RF),0(RF)                                                  
         LA    RF,200(RF)                                                       
         BCT   R4,EST20                                                         
         L     RF,AREGTAB                                                       
         ST    RF,ANXTREG                                                       
*                                                                               
         XC    PREGREC,PREGREC     CLEAR REGION RECORD                          
*                                  FOR NEXT ESTIMATE                            
*                                                                               
ESTX     XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
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
*                                                                               
BILL5    DS    0H                                                               
*                                                                               
BILL8    MVC   LINVFULL,DINVFULL                                                
         MVC   LBQDATE,PBILLDAT                                                 
*                                                                               
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         MVC   OUTREC+200(100),SPACES                                           
         MVC   OUTREC+300(100),SPACES                                           
         MVC   OUTREC+400(100),SPACES                                           
         MVC   OUTREC+500(100),SPACES                                           
         MVC   OUTREC+600(50),SPACES                                            
*                                                                               
*        OUTPUT INVOICE RECORD                                                  
*                                                                               
         LA    R6,OUTREC                                                        
         USING PSIFREC,R6                                                       
         MVC   PSIFUSER,REQUSER                                                 
         MVC   PSIFBCO,SVPRDIFC    FIRST 2                                      
         MVC   PSIFOFC,SVPRDIFC+2  NEXT  2                                      
         GOTO1 DATCON,DMCB,(3,PBILINVD),(X'14',WRKBDATE)                        
         MVC   PSIFBMN,WRKBDATE    YYYYMM                                       
         MVI   PSIFSCD,C'U'    SYSTEM CODE                                      
         MVC   PSIFMED(1),PBILKMED                                              
         MVI   PSIFINV,C'0'           LEADING ZERO                              
         MVC   PSIFINV+1(6),DINVNO    (SHORT FORMAT)                            
         GOTO1 DATCON,DMCB,(3,PBILINVD),(X'14',WORK)                            
*                                                                               
*        OLD DATE FORMAT WAS YYYY-MM-DD                                         
*                                                                               
*****    MVC   PSIFBDT(4),WORK         YYYY                                     
*****    MVI   PSIFBDT+4,C'-'                                                   
*****    MVC   PSIFBDT+5(2),WORK+4     MM                                       
*****    MVI   PSIFBDT+7,C'-'                                                   
*****    MVC   PSIFBDT+8(2),WORK+6     DD                                       
*                                                                               
*        NEW DATE FORMAT IS MM/DD/YYYY                                          
*                                                                               
         MVC   PSIFBDT(2),WORK+4       MM                                       
         MVI   PSIFBDT+2,C'/'                                                   
         MVC   PSIFBDT+3(2),WORK+6     DD                                       
         MVI   PSIFBDT+5,C'/'                                                   
         MVC   PSIFBDT+6(4),WORK       YYYY                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILDUED),(X'14',WORK)                            
*                                                                               
*        OLD DATE FORMAT WAS YYYY-MM-DD                                         
*                                                                               
*****    MVC   PSIFDUE(4),WORK         YYYY                                     
*****    MVI   PSIFDUE+4,C'-'                                                   
*****    MVC   PSIFDUE+5(2),WORK+4     MM                                       
*****    MVI   PSIFDUE+7,C'-'                                                   
*****    MVC   PSIFDUE+8(2),WORK+6     DD                                       
*                                                                               
*        NEW DATE FORMAT IS MM/DD/YYYY                                          
*                                                                               
         MVC   PSIFDUE(2),WORK+4       MM                                       
         MVI   PSIFDUE+2,C'/'                                                   
         MVC   PSIFDUE+3(2),WORK+6     DD                                       
         MVI   PSIFDUE+5,C'/'                                                   
         MVC   PSIFDUE+6(4),WORK       YYYY                                     
*                                                                               
         XC    WORK(20),WORK                                                    
         XC    WK,WK                                                            
         GOTO1 DATCON,DMCB,(3,PBILINVD),(8,WORK)                                
         MVI   WORK+8,C','                                                      
         GOTO1 DATCON,DMCB,(3,PBILDUED),(8,WORK+9)                              
         GOTO1 APERVAL,DMCB,(17,WORK),WK                                        
         LA    R2,WK                                                            
         USING PERVALD,R2                                                       
         LH    R0,PVALNDYS       NUMBER OF DAYS                                 
*                                                                               
         DROP  R2                                                               
*                                                                               
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'      SEE IF ZERO                                       
         BE    *+10                                                             
         SP    DUB,=P'1'      ADJUST BY ONE                                     
*                                                                               
         UNPK  PSIFDDY,DUB                                                      
         OI    PSIFDDY+2,X'F0'                                                  
         MVC   PSIFCLT(3),PBILKCLT                                              
         MVC   PSIFPRD(3),PBILKPRD                                              
         MVC   PSIFPNM(L'PPRDNAME),PPRDNAME                                     
         OC    PSIFPNM,SPACES                                                   
         MVI   PSIFPGI,C'N'      PRODUCT GROUP INDICATOR                        
         MVC   HALF,PBILKEST                 EST                                
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSIFEST(3),DUB                                                   
         MVC   PSIFENM,PESTNAME                                                 
**REG**                                                                         
         CLI   PNBMGR1,C'0'            SEE IF I HAVE ONE                        
         BL    BILL10                                                           
         MVC   PSIFEST+3(3),PNBMGR1    REGION CODE                              
         MVC   PSIFENM,PREGNAME    NAME OF REGION JUST READ IN CKREG            
         CLC   PNBMGR1,=C'999'       UNASSIGNED                                 
         BNE   BILL10                                                           
         MVC   PSIFENM,SPACES       BLANK OUT NAME                              
**REG**                                                                         
BILL10   OC    PSIFENM,SPACES                                                   
         MVC   PSIFCNM,PCLTNAME                                                 
         OC    PSIFCNM,SPACES                                                   
*                                                                               
         LA    R1,6              FOR BCT                                        
         LA    R5,3                                                             
         LA    R4,PSIFC12                                                       
         LA    R3,B4APROF                                                       
         CLI   PBILLMOD,C'4'                                                    
         BE    BILL17                                                           
         LA    R3,B5APROF                                                       
         CLI   PBILLMOD,C'5'                                                    
         BE    BILL17                                                           
         LA    R3,B6APROF                                                       
         CLI   PBILLMOD,C'6'                                                    
         BE    BILL17                                                           
         LA    R3,B7APROF         MUST BE B7                                    
*                                                                               
BILL17   LA    R2,DOLTAB                                                        
BILL18   CLI   0(R3),C'Y'        SHOWING?                                       
         BNE   BILL18F                                                          
*                                                                               
BILL18C  MVC   0(10,R4),0(R2)                                                   
*                                                                               
         LA    R4,16(R4)         TO NEXT FIELD                                  
         BCT   R5,BILL18F                                                       
         B     BILL18X                                                          
*                                                                               
BILL18F  LA    R3,1(R3)          NEXT PROFILE TYPE                              
         LA    R2,10(R2)         NEXT DOLLAR TYPE                               
         BCT   R1,BILL18                                                        
*                                                                               
BILL18X  MVC   PSIFOPCT,=C'10000' TOTAL ORDERED PCT                             
*                        FIELD IS LENGHT 5 WITH 4 DECIMALS                      
*                        AND I WAS TOLD TO SET TO 1                             
*                        SHOULD IT BE 1.0000? OR 0.00001                        
*                        OR DID THEY MEANT TO SET IT TO                         
*                        10000 (100 % WITH 2 DECIMALS)                          
*                                                                               
*        SET BILL'S VALUES INTO BASTAB                                          
*                                                                               
         ZAP   MYCD,PBILLGRS                                                    
         SP    MYCD,PBILLGRS  GROSS - (GROSS-CD) = CD                           
*                                                                               
         ZAP   BTGRS,PBILLGRS                                                   
         ZAP   BTGLCD,PBILLBIL                                                  
         ZAP   BTNET,PBILLNET                                                   
         AP    BTNET,MYCD                                                       
         ZAP   BTNLCD,PBILLNET                                                  
         ZAP   BTAC,PBILLGRS                                                    
         SP    BTAC,BTNET        AC= GROSS- NET                                 
                                                                                
*                                                                               
BILL20   LA    R2,BASTAB                                                        
BILL20A  CLI   0(R2),X'FF'      END OF TABLE?                                   
         BNE   *+6                                                              
         DC    H'0'              INVALID BILLING FORMULA                        
BILL20B  CLC   0(1,R2),PBILBASA                                                 
         BE    BILL20D                                                          
         LA    R2,15(R2)                                                        
         B     BILL20A                                                          
*                                                                               
BILL20D  MVC   PSIFBDSC,1(R2)                                                   
         ZAP   MYDUB2,9(6,R2)     SAVE BASE AMOUNT                              
*                                                                               
         EDIT  (P6,9(R2)),(13,PSIFBAMT),0,FILL=0,ZERO=NOBLANK                   
         CP    9(6,R2),=P'0'       IF NEGATIVE                                  
         BNL   *+8                                                              
         MVI   PSIFBAMT,C'-'                                                    
*                                                                               
*                                                                               
**       MVC   PSIFBPAC,ZEROS                                                   
**       MVC   PSIFAAMT,ZEROS                                                   
**                                                                              
**       OC    PBILADJ,PBILADJ     ANY IN FORMULA?                              
**       BZ    BILL30                                                           
**       EDIT  (B3,PBILADJ),(7,PSIFBPAC),0,FILL=0,ZERO=NOBLANK                  
**       TM    PBILADJ,X'F0'     IF NEGATIVE                                    
**       BZ    *+8                                                              
**       MVI   PSIFBPAC,C'-'                                                    
**                                                                              
**                                                                              
**LL25   LA    R2,BASTAB                                                        
**LL25A  CLI   0(R2),X'FF'      END OF TABLE?                                   
**       BNE   *+6                                                              
**       DC    H'0'              INVALID BILLING FORMULA                        
**LL25B  CLC   0(1,R2),PBILBASB                                                 
**       BE    BILL25D                                                          
**       LA    R2,15(R2)                                                        
**       B     BILL25A                                                          
**                                                                              
**LL25D  MVC   PSIFBAJD,1(R2)                                                   
**                                                                              
BILL30   ZAP   MYDUB,PBILLRCV     TOTAL DUE - BILL AMOUNT                       
         SP    MYDUB,MYDUB2       EQUALS ADJUSTMENT AMT                         
         EDIT  (P8,MYDUB),(13,PSIFAAMT),0,FILL=0,ZERO=NOBLANK                   
         CP    MYDUB,=P'0'       IF NEGATIVE                                    
         BNL   *+8                                                              
         MVI   PSIFAAMT,C'-'                                                    
*                                                                               
         EDIT  (P6,PBILLRCV),(13,PSIFTDUE),0,FILL=0,ZERO=NOBLANK                
         CP    PBILLRCV,=P'0'    IF NEGATIVE                                    
         BNL   *+8                                                              
         MVI   PSIFTDUE,C'-'                                                    
*                                                                               
**REMIT* MVC   PSIFRAL1,=CL30'REMIT ADDRESS'                                    
**REMIT* MVC   PSIFRAL2,=CL30'REMIT ADDRESS'                                    
**REMIT* MVC   PSIFRAL3,=CL30'REMIT ADDRESS'                                    
**REMIT* MVC   PSIFRAL4,=CL30'REMIT ADDRESS'                                    
**REMIT* MVC   PSIFRAL5,=CL30'REMIT ADDRESS'                                    
**REMIT* MVC   PSIFRAL6,=CL30'REMIT ADDRESS'                                    
**REMIT* MVC   PSIFRAL7,=CL30'REMIT ADDRESS'                                    
**REMIT* MVC   PSIFRAL8,=CL30'REMIT ADDRESS'                                    
**REMIT* MVC   PSIFRAL9,=CL30'REMIT ADDRESS'                                    
*                                                                               
         MVC   PSIFRAL1,SPACES                                                  
         MVC   PSIFRAL2,SPACES                                                  
         MVC   PSIFRAL3,SPACES                                                  
         MVC   PSIFRAL4,SPACES                                                  
         MVC   PSIFRAL5,SPACES                                                  
         MVC   PSIFRAL6,SPACES                                                  
         MVC   PSIFRAL7,SPACES                                                  
         MVC   PSIFRAL8,SPACES                                                  
         MVC   PSIFRAL9,SPACES                                                  
*                                                                               
         MVC   PSIFRAMT,PSIFAAMT    REVENUE - SAME AS ADJUSTMENT AMT.           
*                                                                               
         MVC   PSIFAAMT,SPACES      NOW CLEAR ADJUSTMENT AMT.                   
*                                                                               
         MVC   PSIFARCV,PSIFTDUE    A/R - SAME AS TOTAL DUE                     
         EDIT  (P6,PBILLNET),(13,PSIFPAY),0,FILL=0,ZERO=NOBLANK                 
         CP    PBILLNET,=P'0'    IF NEGATIVE                                    
         BNL   *+8                                                              
         MVI   PSIFPAY,C'-'                                                     
*                                                                               
         EDIT  (P6,BTNET),(11,PSIFNET),0,FILL=0,ZERO=NOBLANK                    
*        PSIFNET IS COST OF SALES (NET)                                         
         CP    BTNET,=P'0'    IF NEGATIVE                                       
         BNL   *+8                                                              
         MVI   PSIFNET,C'-'                                                     
*                                                                               
         EDIT  (P6,MYCD),(13,PSIFCD),0,FILL=0,ZERO=NOBLANK                      
         CP    MYCD,=P'0'    IF NEGATIVE                                        
         BNL   *+8                                                              
         MVI   PSIFCD,C'-'                                                      
*                                                                               
         MVI   PSIFCDID,C'P'        CD INDICATOR - ALWAYS P                     
         EDIT  (B4,PPBVETAX),(11,PSIFTAX),0,FILL=0,ZERO=NOBLANK                 
         TM    PPBVETAX,X'F0'   SEE IF NEGATIVE                                 
         BZ    *+8                                                              
         MVI   PSIFTAX,C'-'                                                     
*                                                                               
         MVI   PSIFTYPE,C'B'        BILL TYPE - ALWAYS B                        
         MVI   PSIFPPAY,C'2'                                                    
         CLI   PBILBASA,X'06'       BASE NET-CD?                                
         BE    *+8                                                              
         MVI   PSIFPPAY,C'1'        ELSE SET TO 1 (NET)                         
*                                                                               
         MVI   PSIFREV,C'N'         REVERSED - ALWAYS SET TO N                  
         MVI   PSIFDWN,C'N'         DOWNLOADED - ALWAYS SET TO N                
******   MVC   PSIFDWDT,=C'0001-01-01' DOWNLOADED DATE - DEFAULT                
         MVC   PSIFDWDT,=C'01/01/0001' DOWNLOADED DATE - DEFAULT                
*                                                                               
         MVC   RECTYPE,=C'I1'                                                   
         BAS   RE,MWRITE                                                        
         AP    INVRCNT,=P'1'      BUMP FILE REC COUNT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         CLI   QOPT5,C'Y'          SEE IF LISTING INVOICES                      
         BNE   POSTB8                                                           
         MVC   P1+0(3),PBILKPRD                                                 
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+4(3),DUB                                                      
         MVI   P1+7,C'/'                                                        
         MVC   P1+8(3),PNBMGR1     REGION                                       
*                                                                               
*                                                                               
INVR15   GOTO1 DATCON,DMCB,(3,PBILKMOS),(6,P1+14)                               
*                                                                               
INVR17   DS    0H                                                               
         MVC   P1+23(2),PBILLTYP                                                
         B     INVR20                                                           
INVR20   DS    0H                                                               
         MVC   P1+28(10),DINVFULL                                               
*                                                                               
         ZAP   MYBILLCD,PBILLGRS                                                
         SP    MYBILLCD,PBILLBIL                                                
*                                                                               
         EDIT  (P6,PBILLGRS),(14,P1+37),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P6,PBILLNET),(14,P1+53),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P8,MYBILLCD),(14,P1+69),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P6,PBILLRCV),(14,P1+85),2,COMMAS=YES,FLOAT=-                    
*                                                                               
INVR25   GOTO1 DATCON,DMCB,(3,PBILINVD),(5,P1+101)                              
         GOTO1 DATCON,DMCB,(3,PBILDUED),(5,P1+111)                              
*                                                                               
INVR28   MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
*                                                                               
*                                  ROLL TO TOTALS                               
INVR30   AP    CINVGRS,PBILLGRS    GROSS                                        
         AP    CINVBIL,PBILLNET    NET-CD                                       
         AP    CINVCD,MYBILLCD                                                  
         AP    CINVRCV,PBILLRCV    RECEIVABLE                                   
         OI    CINVSW,1                                                         
*                                                                               
POSTB8   DS    0H                                                               
*                                                                               
         B     BILLX                                                            
*                                                                               
BILLX    XIT1                                                                   
         LTORG                                                                  
*                                                                               
BASTAB   DS    0H                                                               
         DC    X'01',CL8'GROSS   '                                              
BTGRS    DC    PL6'0'                                                           
         DC    X'02',CL8'NET     '                                              
BTNET    DC    PL6'0'                                                           
         DC    X'05',CL8'GROSS-CD'                                              
BTGLCD   DC    PL6'0'                                                           
         DC    X'06',CL8'NET - CD'                                              
BTNLCD   DC    PL6'0'                                                           
         DC    X'08',CL8'AGY COMM'                                              
BTAC     DC    PL6'0'                                                           
         DC    X'FF'               END OF TABLE                                 
*                                                                               
DOLTAB   DC    CL10'GROSS'                                                      
         DC    CL10'NET'                                                        
         DC    CL10'CASH DISC'                                                  
         DC    CL10'GROSS - CD'                                                 
         DC    CL10'NET - CD'                                                   
         DC    CL10'AGY COMM'                                                   
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
CKEST    CSECT                                                                  
         NMOD1 0,CKEST                                                          
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8, R9, RA                             
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
         CLI   QOPT1,C'E'           DOING ESTIMATE FILE?                        
         BNE   *+10                                                             
         MVC   KEY(12),LBILLKEY     USE SAVED KEY                               
         MVI   KEY+3,X'07'                                                      
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
         B     CKEST82                                                          
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
CKEST82  B     CKEST99          SKIP FOR NOW                                    
*                                                                               
***ST82  CLI   QOPT1,C'E'       ESTIMATE REPORT?                                
         BNE   CKEST99                                                          
*                                                                               
*        MUST DO UCOMM LOGIC NOW FOR ESTIMATE REPORT                            
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
         UNPK  P1+25(3),DUB                                                     
         MVC   P1+31(24),=C'MISSING CLIENT BUDGET **'                           
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
         EJECT                                                                  
CKREG    CSECT                                                                  
         NMOD1 0,CKREG                                                          
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8, R9, RA                             
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
         DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
         MVC   PPGAREC,AREC                                                     
         MVI   CKREGSW,0    WILL BE SET TO X'01' IF I READ SOMETHING            
         CLC   PBILKCLT,=C'RHV'   FOR CLIENT RHV SKIP REGION                    
         BE    CKREG4                                                           
*                                                                               
         CLC   PNBMGR1(3),=C'999'    REGION UNASSGINED                          
         BE    CKREG4              SKIP REGION READ                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBILLREC                                                  
         MVI   KEY+3,X'04'                                                      
         MVC   KEY+4(3),PBILLREC+4  CLT   (MIGHT NEED DRD CLIENT)               
         MVC   KEY+7(3),PNBPGR1     DIVISION                                    
         OC    PNBPGR1,PNBPGR1     IS DIVISION PRESENT?                         
         BNZ   *+10                                                             
         MVC   KEY+7(3),=C'000'    IF NOT USE 000                               
         MVC   KEY+10(3),PNBMGR1   REGION                                       
*                                                                               
CKREG3   CLC   PREGREC(13),KEY      SEE IF I ALREADY HAVE REGION                
         BE    CKREG5                                                           
         MVI   CKREGSW,1                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                REG MUST BE ON FILE                          
         LA    R0,PREGREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
CKREG4   BAS   RE,REST            ADD TO REGION TABLE                           
*                                                                               
CKREG5   DS    0H                                                               
CKREG99  MVC   KEY,PPGKEY                                                       
         MVC   AREC,PPGAREC                                                     
         CLI   CKREGSW,1           SEE IF I READ SOMETHING                      
         BNE   CKREGX              NO - SKIP READ HIGH                          
         GOTO1 HIGH                                                             
CKREGX   XIT1                                                                   
*                                                                               
         EJECT                                                                  
REST     NTR1                                                                   
         CLC   PBILKCLT,=C'RHV'     NO REGIONS FOR CLIENT RHV                   
         BNE   REST5                                                            
         L     RF,AREGTAB                                                       
         MVC   0(3,RF),PBILKCLT                                                 
         MVC   6(20,RF),SPACES                                                  
         B     RESTX                                                            
*                                                                               
REST5    CLC   PNBMGR1(3),=C'999'   UNASSGINED REGION                           
         BNE   REST10               JUST PUT CODE                               
*                                                                               
         L     RF,ANXTREG                                                       
         MVC   0(6,RF),=C'000999'   REGION CODE 999                             
         MVC   6(20,RF),SPACES      NO NAME                                     
         B     RESTX                                                            
*                                                                               
REST10   L     RF,ANXTREG                                                       
         MVC   0(6,RF),PREGKDIV     PUT DIVISION/REGION +NAME                   
         MVC   6(20,RF),PREGNAME                                                
         OC    6(20,RF),SPACES                                                  
         LA    RF,REGTABL(RF)                                                   
         L     RE,AREGTABX                                                      
         CR    RF,RE                                                            
         BL    *+6                                                              
         DC    H'0'               TOO MANY REGIONS                              
         ST    RF,ANXTREG                                                       
*                                                                               
RESTX    XIT                                                                    
         LTORG                                                                  
CKREGSW  DC    X'00'                                                            
*****                                                                           
*                                                                               
TITLES   CSECT                                                                  
         DS    0C                                                               
         DC    CL30'        INVOICE RECORDS'                                    
         DC    CL30'       ESTIMATE RECORDS'                                    
*                                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*             TABLE OF RECORD TYPES,LENGTHS AND COUNTS                          
LENTAB   CSECT                                                                  
         DC    CL2'I1',AL2(PSIFRECX),PL4'0'                                     
         DC    CL2'E1',AL2(PSEFRECX),PL4'0'                                     
         DC    XL4'00',PL4'0'      EXTRA LINE                                   
         DC    X'FFFF'                                                          
         EJECT                                                                  
PPSNWRKD DSECT                                                                  
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
ELLOWSW  DS    XL1                                                              
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
EDYNDDN  DS    CL8                                                              
EDYNDSN  DS    CL20                                                             
         DS    0F          ALIGNMENT FOR WK                                     
*                                                                               
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
B4APROF  DS    CL16                                                             
B5APROF  DS    CL16                                                             
B6APROF  DS    CL16                                                             
B7APROF  DS    CL16                                                             
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
ESTRCNT  DS    PL5      COUNT OF ESTIMATE FILE RECORDS                          
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
OUTREC   DS    CL650                                                            
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
**REG**                                                                         
REGTAB   CSECT                                                                  
         DS    CL7800   ROOM FOR 300 REGIONS                                    
REGTABX  EQU   *                                                                
REGTABL  EQU   26       DIV/REG/NAME   3/3/20                                   
**REG**                                                                         
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
         EJECT                                                                  
       ++INCLUDE PSIFILED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPREPSN02 06/11/10'                                      
         END                                                                    
