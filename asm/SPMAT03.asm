*          DATA SET SPMAT03    AT LEVEL 080 AS OF 01/09/07                      
*PHASE T21503A,+0                                                               
*INCLUDE TIMUNPK                                                                
         TITLE 'T21503 - ICS MATCHING OVLY 3 - ICSOLR'                          
T21503   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21503                                                         
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T215FFD,RA                                                       
*                                                                               
         RELOC RELO03                                                           
*                                                                               
         L     RF,=V(TIMUNPK)                                                   
         A     RF,RELO03                                                        
         ST    RF,VTIMUNPK                                                      
         SPACE 2                                                                
         EJECT                                                                  
*                   READ TEMPSTR                                                
         CLI   BUFFSW,0                                                         
         BNE   CHKPRD              ALREADY HAVE BUFFERS                         
         LA    R3,1                                                             
         L     R4,ABUFFER                                                       
         ICM   R5,15,NPAGES           NO. OF PAGES                              
         MVC   HALF,TWPAGSIZ          PAGE SIZE                                 
         BP    RDBUFF2                                                          
         B     EXIT              NO TWA PAGES BUG                               
*                                JUST EXIT (SOME ERRMSG IS ALWAYS SET)          
RDBUFF2  DS    0H                                                               
         BAS   RE,RDTEMP                                                        
         LA    R3,1(R3)                                                         
         AH    R4,HALF            R4 IS POS IN BUFFER FOR NEXT PAGE             
         L     RF,ABUFFX                                                        
         SR    RF,R4                                                            
         CH    RF,TWPAGSIZ                                                      
         BNL   RDBUFF4                                                          
         STH   RF,HALF                                                          
*                                                                               
RDBUFF4  DS    0H                                                               
         BCT   R5,RDBUFF2                                                       
*                                                                               
         SPACE 3                                                                
*                                SET CLIST                                      
CHKPRD   DS    0H                                                               
         LA    RF,IOAREA                                                        
         USING CLTHDR,RF                                                        
         CLI   BPRD,0                                                           
         BE    RDCLT                                                            
         CLI   BPRD2,X'FF'                                                      
         BE    RDCLT                                                            
         SPACE 2                                                                
         MVC   CLIST(3),QPRD       FOR SINGLE BRAND FAKE CLIST                  
         MVC   CLIST+3(1),BPRD                                                  
         MVC   CLIST+4(3),QPRD2                                                 
         MVC   CLIST+7(1),BPRD2                                                 
         MVI   CLIST+8,0                                                        
         B     GETOLR                                                           
         DROP  RF                                                               
         SPACE 2                                                                
*                                  READ CLTHDR FOR CLIST                        
RDCLT    MVC   KEY+14(4),CLTDSK                                                 
         BAS   RE,GETREC                                                        
         SPACE 2                                                                
*                             BRANCH TO ICSOLR                                  
         SPACE 2                                                                
GETOLR   GOTO1 =A(ICSOLR),DMCB,(RC),RR=RELO03                                   
         SPACE 3                                                                
         B     EXIT                                                             
         EJECT                                                                  
*                   DATAMGR INTERFACE FOR TEMPSTR READ                          
         SPACE 2                                                                
RDTEMP   ST    RE,FULL                                                          
         XC    DMCB+8(2),DMCB+8                                                 
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERMINAL NUMBER                       
         STC   R3,DMCB+8           PAGE                                         
         MVI   DMCB+9,X'FF'        FULL READ OF PAGE                            
         MVC   DMCB+20(2),=C'L='   SET NEW LENGTH IN PARM6                      
         MVC   DMCB+22(2),HALF                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R4),(TERMNAL,0)           
         SPACE 2                                                                
         L     RE,FULL                                                          
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'           TEMPSTR READ ERROR                                
*                                                                               
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+14                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R3,TERMNAL                                                       
         IC    R4,DMINBTS                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'SPTFILE',               X        
               (R2),IOAREA,((R3),DMWORK)                                        
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT1                                                                   
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(2,DMCB)                            
         SPACE 2                                                                
EXIT     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         TITLE 'ICSOLR - ICS ON-LINE RECONCILIATION REPORT MODULE'              
ICSOLR   CSECT                                                                  
         NMOD1 0,ICSOLR                                                         
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING ICSOLR+4096,R9                                                   
         USING GENOLD,RC                                                        
         USING T215FFD,RA                                                       
         USING BYELEMD,R7                                                       
         USING IELEMD,R8                                                        
         XC    HOLDS,HOLDS                                                      
         ZAP   THISPAG,=P'0'                                                    
         MVI   BYTE2,0                                                          
         SPACE 2                                                                
         LA    R2,ICSLIN1H                                                      
         EJECT                                                                  
*                   BUCKET TOTALS AND DETERMINE STATUS OF                       
*                   RECONCILIATION                                              
*                                  BUYS                                         
         L     R7,AFBY                                                          
IR2      C     R7,ALBY                                                          
         BNL   IR4                                                              
         SR    R5,R5                                                            
         IC    R5,BYNOWK                                                        
         SR    R6,R6                                                            
         IC    R6,BYUNACH          R6=UNACHIEVED                                
         SR    R5,R6               R5=ACHIEVED                                  
         LTR   R6,R6               IF ANY UNACHIEVED                            
         BZ    IR2F                                                             
         BAS   RE,DEFTEST          AND IF NOT 'DEFERRED'                        
         BE    IR2F                                                             
         MVI   BYTE2,1             SET MISMATCH SW                              
*                                                                               
IR2F     DS    0H                                                               
         MVC   DUB(4),BYCOST                                                    
         L     R1,DUB                                                           
         TM    BYSTAT,X'01'        TEST NEG                                     
         BZ    *+6                                                              
         LCR   R1,R1                                                            
         CLI   CENTS,C'Y'          TEST HAVE PENNIES                            
         BE    *+8                                                              
         M     R0,=F'100'                                                       
         ST    R1,DUB                                                           
         MR    R0,R5               MATCHED                                      
         A     R1,MATGRS                                                        
         ST    R1,MATGRS                                                        
         L     R1,DUB                                                           
         MR    R0,R6               ONR                                          
         A     R1,ONRGRS                                                        
         ST    R1,ONRGRS                                                        
         LR    R0,R5                                                            
         A     R0,MATSPTS                                                       
         ST    R0,MATSPTS                                                       
         LR    R0,R6                                                            
         A     R0,ONRSPTS                                                       
         ST    R0,ONRSPTS                                                       
*                                                                               
         BAS   RE,DEFTEST          IS IT 'DEFERRED'                             
         BNE   IR2K                                                             
         L     R1,DEFSPTS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,DEFSPTS                                                       
         L     R1,DUB                                                           
         A     R1,DEFGRS                                                        
         ST    R1,DEFGRS                                                        
*                                                                               
IR2K     DS    0H                                                               
         LA    R7,L'BYELEM(R7)                                                  
         B     IR2                                                              
*                                  INVOICES                                     
IR4      L     R8,AFINV                                                         
IR6      C     R8,ALINV                                                         
         BNL   IR9                                                              
         OC    IBY,IBY             MATCHED -                                    
         BNZ   IR8                 YES - DONT TOTAL                             
         CLI   BEST,0              IF REQUEST FOR ONE EST                       
         BE    IR7                                                              
         CLC   IEST,BEST           COUNT ONLY UNORDEREDS FOR THAT EST           
         BNE   IR8B                                                             
*                                                                               
IR7      DS    0H                                                               
         MVI   BYTE2,1             SET MISMATCH SW                              
         MVC   DUB(4),ICOST                                                     
         L     R1,DUB                                                           
         TM    ISTAT,X'01'         TEST NEG                                     
         BZ    *+6                                                              
         LCR   R1,R1                                                            
         CLI   CENTS,C'Y'          TEST HAVE PENNIES                            
         BE    *+8                                                              
         M     R0,=F'100'                                                       
         A     R1,RNOGRS                                                        
         ST    R1,RNOGRS                                                        
         L     R1,RNOSPTS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,RNOSPTS                                                       
IR8      DS    0H                                                               
         MVC   DUB(4),ICOST                                                     
         L     R1,DUB                                                           
         TM    ISTAT,X'01'                                                      
         BZ    *+6                                                              
         LCR   R1,R1                                                            
         CLI   CENTS,C'Y'          TEST HAVE PENNIES                            
         BE    *+8                                                              
         M     R0,=F'100'                                                       
         A     R1,IGRS                                                          
         ST    R1,IGRS                                                          
         L     R1,ISPTS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,ISPTS                                                         
IR8B     DS    0H                                                               
         LA    R8,IEL$MLEN(R8)                                                  
         B     IR6                                                              
*                                                                               
IR9      DS    0H                                                               
         CLI   SPADSW,C'Y'         TEST DOING ADDS INTERFACE                    
         BNE   IR9A8                                                            
         CLI   WEEKSW,C'Y'         IF DOING ONLY 1 WEEK                         
         BNE   IR9A2                                                            
         CLI   BYTE2,0             AND GOOD MATCH, NO ADDS BECAUSE              
         BE    IR9A8               WHOLE MONTH MAY NOT BE GOOD                  
*                                                                               
IR9A2    DS    0H                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         MVC   DMCB+4(4),=X'D9000A5E'                                           
         GOTO1 (RF),DMCB,0                                                      
         MVC   FULL,0(R1)          A(SPADINT)                                   
*                                                                               
         LA    R4,X             NOTE- X IS BIG ENOUGH BECAUSE                   
         USING SPADINTD,R4      SPADINT DOESNT USE IT 'STATION TABLE'           
         XC    X,X                                                              
         MVI   ADACTN,ADAMATCH     MATCH                                        
         CLI   BYTE2,0                                                          
         BE    *+8                                                              
         MVI   ADACTN,ADAUNMAT     OR UNMATCH                                   
         MVC   ADACOMFC,VCOMFACS                                                
         MVC   ADQAGYMD,HKEY+1     AGYALPHA/MEDIA                               
         MVC   ADQAGY,AGYALPHA                                                  
         MVC   ADQCLT,BCLT                                                      
         MVC   ADQPRD,BPRD                                                      
         MVC   ADQEST,BEST                                                      
         MVC   ADQMKT,BMS                                                       
         MVC   ADQSTA,BMS+2                                                     
         MVC   ADQYM,BMOS                                                       
*                                                                               
         GOTO1 FULL,DMCB,SPADINTD                                               
         CLI   ADERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
IR9A8    DS    0H                                                               
         XC    ICSMSG,ICSMSG                                                    
         FOUT  ICSMSGH                                                          
         GOTO1 VDATCON,DMCB,(5,0),(5,ICSMSG+52)  DATE IN HEADLINE               
         LA    R2,ICSMEDH          CURSOR TO MEDIA                              
         L     R0,ISPTS                                                         
         A     R0,MATSPTS                                                       
         A     R0,ONRSPTS                                                       
         BNZ   IR9B                                                             
         MVC   ICSMSG(L'NOMSG),NOMSG                                            
         B     IR9T                                                             
IR9B     DS    0H                                                               
         CLI   BYTE2,0             TEST MISMATCH SW                             
         BNE   IR9R                                                             
         MVC   ICSMSG(L'GOODMSG),GOODMSG                                        
         CLI   AUTOPAY,C'Y'        IF MATCH OK- TEST AUTOPAY                    
         BNE   IR9B4                                                            
*                                                                               
*        XC    ICSSRV,ICSSRV       SET AUTOPAY                                  
*        MVC   ICSSRV(7),=C'+PAY,SV'                                            
*                                                                               
         L     R6,VCOMFACS                                                      
         USING COMFACSD,R6                                                      
         OC    CGLOBBER,CGLOBBER   TEST GLOBBER PROVIDED                        
         BZ    IR9H7                                                            
*                                                                               
         XC    WORK(14),WORK                                                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'    FROM THE SPOT SYSTEM                         
         MVC   GLVXFRPR,=C'MAT'    MATCH PROGRAM                                
         MVC   GLVXTOSY,=C'SPO'    TO THE SPOT SYSTEM                           
         MVC   GLVXTOPR,=C'PAY'    PAY PROGRAM                                  
         OI    GLVXFLG1,GLV1SEPS+GLV1RETG                                       
         DROP  R1                                                               
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BE    IR9H7                                                            
         DC    H'0'                                                             
*                                                                               
IR9B4    DS    0H                                                               
         CLI   AUTOPAY,C'S'        TEST SEMI-AUTOMATIC                          
         BNE   IR9T                                                             
         TM    ICSPAGH+4,X'20'     TEST PAGE CHANGED                            
         BNZ   IR9B6               NO, SET 'PAY'                                
         OI    ICSPAGH+4,X'20'                                                  
         CLI   ICSPAGH+5,0         ANY INPUT?                                   
         BH    IR9T                YES, DON'T CLOBBER                           
IR9B6    DS    0H                                                               
         MVC   PAGIN,=C'TO'         SHOW TOTAL PAGE                             
         MVC   ICSPAG,=C'PAY'      SET TO SWITCH NEXT TIME                      
         FOUT  ICSPAGH                                                          
*                                                                               
IR9H7    DS    0H                                                               
         CLI   AUTOPAY,C'Y'        FOR COMPLETE AUTOPAY                         
         BE    IREXT               EXIT WITHOUT BUILDING SCREENS                
         B     IR9T                                                             
*                                                                               
IR9R     DS    0H                                                               
         MVC   ICSMSG(L'BADMSG),BADMSG                                          
*                                                                               
IR9T     DS    0H                                                               
         EJECT                                                                  
*                            ORDERED - INVOICE LINES                            
IR10     BAS   RE,HDLA             INITIAL HEADLINE                             
         L     R7,AFBY                                                          
         ST    R2,LINPOINT                                                      
         SPACE 2                                                                
IR11     C     R7,ALBY                                                          
         BNL   IR20                                                             
*                                                                               
         CLC   BYCBLNET,LASTNET    ON CABLE NETWORK BREAK                       
         BE    IR12                                                             
         MVC   LASTNET,BYCBLNET                                                 
         CLI   BYCBLNET,0                                                       
         BE    IR12                                                             
*                                                                               
         XC    8(L'ICSLIN1,R2),8(R2)                                            
         MVC   8(8,R2),=C'NETWORK='                                             
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),BMS+2                                                   
         NI    DUB+4,X'80'                                                      
         OC    DUB+4(1),BYCBLNET                                                
*                                  UNPACK CABLE NETWORK                         
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPMED,ICSMED                                                   
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPSTA,DUB+2                                                    
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   16(3,R2),STAPQNET                                                
         FOUT  (R2)                                                             
         LA    R2,L'ICSLIN1+8(R2)                                               
         LA    R0,ICSLAST                                                       
         CR    R2,R0                                                            
         BL    IR12                                                             
         BAS   RE,NEXTWA                                                        
         BNZ   GIEXTX                                                           
         BAS   RE,HDLA                                                          
*                                                                               
IR12     DS    0H                                                               
         BAS   RE,FMTORD           ORDERED INFO                                 
         BAS   RE,GETINV           INVOICES                                     
         CLI   LINPOINT,X'FF'      TEST EXIT                                    
         BNE   *+12                                                             
         MVI   LASTNET,0           MAKE SURE THIS IS RESET!                     
         B     IREXT                                                            
         L     R2,LINPOINT                                                      
         LA    R7,L'BYELEM(R7)                                                  
         B     IR11                                                             
         EJECT                                                                  
*                   UNORDERED INVOICE ITEMS                                     
         SPACE 3                                                                
IR20     L     R8,AFINV                                                         
         MVI   BYTE2,0                                                          
IR21     C     R8,ALINV                                                         
         BNL   IR30                                                             
         OC    IBY,IBY             TEST ORDERED                                 
         BNZ   IR22                IS ORDERED                                   
*                                                                               
         TM    QBYID,QBYIDYQ       SKIP UNORDERED IF RUN BY ID                  
         BZ    IR21H                                                            
*                                                                               
IR21D    DS    0H                                                               
         CLC   QREP,=C'000'        OR FOR 1 REP                                 
         BNH   IR21H                                                            
*                                                                               
IR21F    DS    0H                                                               
         CLI   IID,0               TEST INPUT BY ID                             
         BE    IR22                NO                                           
*                                  YES- ACCEPT (MUST BE RIGHT ONE)              
IR21H    DS    0H                                                               
         CLI   BEST,0              IF REQ BY EST                                
         BE    IR211                                                            
         CLC   IEST,BEST           USE ONLY IF FOR THIS EST                     
         BNE   IR22                                                             
*                                                                               
IR211    DS    0H                                                               
         SPACE 2                                                                
         CLI   BYTE2,0             FIRST TIME SW                                
         BNE   IR21A                                                            
         MVI   BYTE2,1                                                          
         BAS   RE,HDLB             FIRST TIME HEADLINE                          
         SPACE 2                                                                
IR21A    BAS   RE,FMTUNORD         UNMATCHED SPOT                               
         LA    R2,L'ICSLIN1+8(R2)                                               
         LA    R0,ICSLAST                                                       
         CR    R2,R0                                                            
         BL    IR22                                                             
         BAS   RE,NEXTWA                                                        
         BNZ   IREXT                                                            
         BAS   RE,HDLB                                                          
IR22     LA    R8,IEL$MLEN(R8)                                                  
         B     IR21                                                             
         EJECT                                                                  
*                  TOTALS                                                       
         SPACE 3                                                                
IR30     DS    0H                                                               
         MVI   LASTNET,0           FOR CABLE NETWORK CHANGE TEST                
         LA    R0,ICSLINHH         NEED AT LEAST 3 LINES                        
         L     RF,AFREP                                                         
         OC    0(6,RF),0(RF)       TEST ANY SPEC REPS                           
         BZ    *+8                                                              
         SH    R0,=Y(2*(8+L'ICSLIN1))  YES- NEED 2 MORE LINES                   
         CLI   INOLIST,C' '        TEST HAVE LIST OF INVOICES                   
         BE    *+8                                                              
         SH    R0,=Y(2*(8+L'ICSLIN1))  YES- NEED 2 MORE LINES                   
         CR    R2,R0                                                            
         BNH   IR31                                                             
         BAS   RE,NEXTWA                                                        
         BNZ   IREXT                                                            
IR31     MVC   8(L'ICSLIN1,R2),HDC1+1                                           
         FOUT  (R2)                                                             
         MVI   PAGSW,C'T'          SET PAGE SW TO TOTAL                         
         LA    R2,L'ICSLIN1+8(R2)                                               
         XC    8(L'ICSLIN1,R2),8(R2)                                            
         FOUT  (R2)                                                             
*                                                                               
         LA    R3,MATSPTS          MATCHED                                      
         LA    R4,8(R2)                                                         
         MVI   RESPTOT,C'Y'        WILL SHOW RESPONSES                          
         MVI   BYTE,1              ROOM FOR $AMOUNT 12 LONG                     
         BAS   RE,IR90                                                          
*                                                                               
         LA    R3,ONRSPTS          ONR                                          
         LA    R4,16(R4)                                                        
         MVI   RESPTOT,C'N'        NO RESPONSES                                 
         BAS   RE,IR90                                                          
*                                                                               
         LA    R3,RNOSPTS          RNO                                          
         LA    R4,16(R4)                                                        
         MVI   RESPTOT,C'Y'        WILL SHOW RESPONSES                          
         BAS   RE,IR90                                                          
*                                                                               
*                                                                               
         LM    R5,R6,ONRSPTS                                                    
         A     R5,MATSPTS                                                       
         A     R6,MATGRS                                                        
         MVI   RESPTOT,C'N'        NO RESPONSES                                 
         STM   R5,R6,ONRSPTS                                                    
*                                                                               
         LA    R3,ONRSPTS          ORDERED TOTAL                                
         LA    R4,16(R4)                                                        
*                                  ADD TAX INTO ORDERED $                       
         L     R0,ONRGRS                                                        
         A     R0,TAXTOT                                                        
         ST    R0,ONRGRS                                                        
         MVI   RESPTOT,C'N'        NO RESPONSES                                 
         BAS   RE,IR90                                                          
*                                                                               
         OC    TAXTOT,TAXTOT                                                    
         BZ    IR31B                                                            
         LA    RF,4(R4)                                                         
         AR    RF,R0                                                            
         MVI   0(RF),C'T'          TO SHOW TAX                                  
*                                                                               
IR31B    DS    0H                                                               
         LA    R3,ISPTS          INVOICE TOTAL                                  
         LA    R4,16(R4)                                                        
         MVI   RESPTOT,C'Y'        WILL SHOW RESPONSES                          
         MVI   BYTE,0              NO ROOM FOR $AMOUNT 12 LONG                  
         BAS   RE,IR90             (ONLY 10, LAST AMOUNT ONLY)                  
*                                                                               
         LA    R2,L'ICSLIN1+8(R2)                                               
         XC    8(L'ICSLIN1,R2),8(R2)                                            
*                                  CLEARED/NOT-CLEARED                          
         FOUT  (R2)                                                             
         CLI   PSEUDOPT,C'R'       SKIP FOR RESPONSE                            
         BE    IR31D                                                            
         L     R5,PDGRS                                                         
         MVC   8(15,R2),=C'CLEARED (GROSS)'                                     
         CLI   NETINV,C'Y'                                                      
         BNE   *+14                                                             
         MVC   8+9(6,R2),=C'NET)  '                                             
         L     R5,PDNET                                                         
         EDIT  (R5),(11,24(R2)),2,FLOAT=$,ALIGN=LEFT,MINUS=YES                  
         MVC   8+28(11,R2),=C'NOT-CLEARED'                                      
         L     R0,ONRGRS                                                        
         SR    R0,R5                                                            
         EDIT  (R0),(11,48(R2)),2,FLOAT=$,ALIGN=LEFT,MINUS=YES                  
*                                                                               
IR31D    DS    0H                                                               
         OC    DEFSPTS,DEFSPTS     ANY DEFERRED?                                
         BZ    IR31H                                                            
         MVC   60(8,R2),=C'DEFERRED'                                            
         LA    R3,DEFSPTS                                                       
         LA    R4,69(R2)                                                        
         MVI   BYTE,1                                                           
         BAS   RE,IR90                                                          
*                                                                               
IR31H    DS    0H                                                               
*                                       SPECIAL REPS                            
         L     R4,AFREP                                                         
         OC    0(6,R4),0(R4)                                                    
         BZ    IR33                                                             
*                                                                               
         LA    R2,L'ICSLIN1+8(R2)                                               
         XC    8(L'ICSLIN1,R2),8(R2)                                            
         MVC   8(11,R2),=C'SPEC. REPS'                                          
         LA    R3,21(R2)                                                        
*                                                                               
IR32     DS    0H                                                               
         OC    0(2,R4),0(R4)            END                                     
         BZ    IR33                                                             
*                                  NNN/NNNNNN.NN IN WORK                        
         GOTO1 VRCPACK,DMCB,(C'U',0(R4)),ORDLINW                                
         MVI   ORDLINW+3,C'/'                                                   
         LA    R5,ORDLINW+4                                                     
         EDIT  (B4,2(R4)),(10,(R5)),2,ALIGN=LEFT,FLOAT=-                        
         LR    RF,R0                                                            
         LA    RF,4(RF)            RF HAS LENGTH                                
         LA    R6,0(RF,R3)                                                      
         LA    RE,L'ICSLIN1+8(R2)                                               
         CR    R6,RE               TEST WILL FIT                                
         BH    IR32D               NO                                           
IR32B    DS    0H                                                               
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ORDLINW                                                  
         LA    R3,1(R3,RF)                                                      
         LA    R4,6(R4)                                                         
         B     IR32                                                             
*                                                                               
IR32D    DS    0H                  NEXT LINE                                    
         FOUT  (R2)                                                             
         LA    R2,L'ICSLIN1+8(R2)                                               
         XC    8(L'ICSLIN1,R2),8(R2)                                            
         LA    R3,21(R2)                                                        
         B     IR32B                                                            
*                                                                               
IR33     DS    0H                                                               
         FOUT  (R2)                                                             
         LA    R2,L'ICSLIN1+8(R2)                                               
         SPACE 2                                                                
*                                       INVOICE NUMBERS                         
IR34     DS    0H                                                               
         CLI   INOLIST,C' '        TEST ANY                                     
         BNH   IR38                                                             
*                                                                               
         XC    8(L'ICSLIN1,R2),8(R2)                                            
         MVC   8(9,R2),=C'INVOICES='                                            
         LA    R3,18(R2)                                                        
         LA    R4,INOLIST                                                       
         USING INOLSTD,R4                                                       
*                                                                               
IR35     DS    0H                                                               
         CLI   0(R4),C' '               END                                     
         BNH   IR36                                                             
*                                                                               
         XC    ORDLINW,ORDLINW                                                  
         MVC   ORDLINW(10),INOLINV                                              
         CLC   INOLINV,=10C'*'     LUMPED EXTRAS                                
         BNE   *+10                                                             
         MVC   ORDLINW(10),=CL10'+MORE'                                         
         LA    R5,ORDLINW+10                                                    
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         CLI   INOLDAT,0            TEST HAVE DATE                              
         BE    IR35A                                                            
         MVI   1(R5),C','                                                       
         GOTO1 VDATCON,DMCB,(3,INOLDAT),(4,2(R5))                               
         LA    R5,6(R5)                                                         
IR35A    DS    0H                                                               
         CLI   INOLSTL(R4),C' '         TEST AT END                             
         BNH   *+8                 YES, NO SEPARATOR                            
         MVI   1(R5),C'/'                                                       
         LA    R0,ORDLINW                                                       
         SR    R5,R0               R5 HAS LENGTH -1                             
*                                                                               
         LA    R6,1(R5,R3)                                                      
         LA    RE,L'ICSLIN1+8(R2)                                               
         CR    R6,RE               TEST WILL FIT                                
         BH    IR35D               NO                                           
         BE    *+8                 NO ROOM FOR SEPARATOR                        
*                                                                               
IR35B    DS    0H                  YES- WILL FIT                                
         LA    R5,1(R5)            INCLUDE SEPARATOR IF ROOM                    
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ORDLINW                                                  
         LA    R3,1(R3,R5)                                                      
         LA    R4,INOLSTL(R4)                                                   
         B     IR35                                                             
         DROP  R4                                                               
*                                                                               
IR35D    DS    0H                  NEXT LINE                                    
         FOUT  (R2)                                                             
         CH    R7,=H'2'            ONLY 2 LINES                                 
         BNL   IR36                                                             
         LA    R7,1(R7)            BUMP LINE COUNTER                            
         LA    R2,L'ICSLIN1+8(R2)                                               
         XC    8(L'ICSLIN1,R2),8(R2)                                            
         LA    R3,17(R2)                                                        
         B     IR35B                                                            
*                                                                               
IR36     DS    0H                                                               
         FOUT  (R2)                                                             
         LA    R2,L'ICSLIN1+8(R2)                                               
IR38     DS    0H                                                               
         BAS   RE,NEXTWA                                                        
         SPACE 2                                                                
IREXT    XIT1                                                                   
*                                                                               
IR90     L     R0,0(R3)                                                         
         EDIT  (R0),(3,(R4))                                                    
         BNZ   *+12                                                             
         MVC   5(4,R4),=C'NONE'                                                 
         BR    RE                                                               
*                                                                               
         CLI   PSEUDOPT,C'R'       IF DOING RESPONSES                           
         BNE   IR90B                                                            
         CLI   RESPTOT,C'Y'        AND NEED FOR THIS TOTAL                      
         BNER  RE                                                               
         MVI   3(R4),C'/'                                                       
         L     R0,4(R3)                                                         
         EDIT  (R0),(10,4(R4)),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         BR    RE                                                               
*                                                                               
IR90B    DS    0H                                                               
         MVI   3(R4),C'/'                                                       
         L     R0,4(R3)                                                         
         EDIT  (R0),(12,X),2,FLOAT=$,ALIGN=LEFT                                 
*                                                                               
         CLI   BYTE,1              TEST IF HAVE ROOM FOR ALL 12                 
         BNE   *+12                                                             
         MVC   4(12,R4),X          YES                                          
         BR    RE                                                               
*                                                                               
         MVC   4(10,R4),X          NO, JUST SHOW 10                             
         BR    RE                  (LAST AMOUNT ONLY)                           
         EJECT                                                                  
*                   FORMAT ORDERED INFO                                         
         SPACE 3                                                                
FMTORD   NTR                                                                    
         SPACE 2                                                                
         LA    R6,ORDLINW-8                                                     
         XC    ORDLINW,ORDLINW                                                  
         USING LNAD,R6                                                          
*                                                                               
         BAS   RE,DEFTEST          IS IT 'DEFERRED'                             
         BNE   *+10                                                             
         MVC   LNAIDTE(12),=C'**DEFERRED**'  DEFERRED TILL NEXT MONTH           
*                                                                               
         LA    R3,BYSDT                                                         
         LA    R4,LNADTE                                                        
         BAS   RE,FDAT                                                          
         SPACE 2                                                                
         LA    R3,BYDAY                                                         
         LA    R4,LNADAYS                                                       
         BAS   RE,FDAY                                                          
         SPACE 2                                                                
         LA    R3,BYSTIM                                                        
         LA    R4,LNATIM                                                        
         BAS   RE,FTIM                                                          
         SPACE 1                                                                
         CLC   BYSTIM,BYETIM                                                    
         BE    FMTORD2                                                          
         LA    R3,BYETIM                                                        
         LA    R4,LNATIM+6                                                      
         BAS   RE,FTIM                                                          
         MVI   LNATIM+5,C'-'                                                    
         SPACE 2                                                                
FMTORD2  EQU   *                                                                
         CLI   LNATIM,C'0'                                                      
         BNE   *+8                                                              
         MVI   LNATIM,C' '                                                      
         CLI   LNATIM+6,C'0'                                                    
         BNE   *+8                                                              
         MVI   LNATIM+6,C' '                                                    
         SPACE 2                                                                
         EDIT  BYLEN,(3,LNALEN)                                                 
         SPACE 2                                                                
         LA    R3,BYPRD                                                         
         LA    R4,LNAPRD                                                        
         BAS   RE,FPRD                                                          
         SPACE 2                                                                
         CLI   PSEUDOPT,C'R'       FOR RESPONSES SKIP COST                      
         BE    FMTORD3                                                          
         MVC   DUB(4),BYCOST                                                    
         L     R0,DUB                                                           
         TM    BYSTAT,X'01'                                                     
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         CLI   CENTS,C'Y'                                                       
         BE    *+8                                                              
         MH    R0,=H'100'                                                       
         EDIT  (R0),(9,LNACOST),2,FLOAT=-                                       
*                                                                               
         TM    BYSTAT,X'08'        IF CONTRACT TRADE SPOT                       
         BZ    FMTORD2B                                                         
         LA    RF,LNACOST+6                                                     
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   0(RF),C'T'          FLOAT A T                                    
*                                                                               
FMTORD2B DS    0H                                                               
         LA    RE,LNAESTL          NORMAL SPOT FOR EST-LIN                      
         B     FMTORD3D                                                         
*                                                                               
FMTORD3  DS    0H                                                               
         LA    RE,LNACOST+1        FOR RESPONSES EST-LIN AT COST+1              
*                                                                               
FMTORD3D DS    0H                                                               
         EDIT  BYEST,(3,0(RE)),FILL=0                                           
         SPACE 1                                                                
         LA    R1,4(RE)                                                         
         EDIT  BYLIN,(3,(R1)),FILL=0                                            
         SPACE 1                                                                
         MVI   3(RE),C'-'                                                       
*                                                                               
*                                                                               
FMTORD8  DS    0H                                                               
         CLI   TRAFMBF,C'Y'        IF MATCHING BY FILM                          
         BNE   FO20                                                             
         CLI   FILMRSW,C'Y'        AND SHOWING FILMS                            
         BNE   FO20                                                             
         CLI   BYUNACH,0           FOR ONR'S                                    
         BE    FO20                                                             
         OC    BYFILM,BYFILM       SHOW FILM CODE                               
         BZ    FO20                                                             
         LA    R3,BYFILM                                                        
         BAS   RE,FOFILM                                                        
         OC    BYFILM2,BYFILM2     ANY 2ND FILEM?                               
         BNZ   FO18                                                             
         MVC   LNAIFILM,DUB        NO, SHOW FIRST IN NORMAL PLACE               
         B     FO20                                                             
*                                                                               
FO18     DS    0H                  IF HAVE 2ND FILM                             
         MVC   LNAIFILM-9(8),DUB   SHOW FIRST TO THE LEFT                       
         MVI   LNAIFILM-1,C'-'                                                  
         LA    R3,BYFILM2                                                       
         BAS   RE,FOFILM                                                        
         MVC   LNAIFILM,DUB                                                     
*                                                                               
FO20     DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                   FORMAT UNORDERED INVOICE ITEMS                              
         SPACE 3                                                                
FMTUNORD NTR                                                                    
         SPACE 2                                                                
         FOUT  (R2)                                                             
         USING LNBD,R2                                                          
         XC    LNB,LNB                                                          
         XC    PRDBH,PRDBH                                                      
         SPACE 2                                                                
         LA    R3,IDAT                                                          
         LA    R4,LNBDTE                                                        
         BAS   RE,FDAT                                                          
         MVC   DUB(1),IDAY         DAY OF WEEK                                  
         MVI   DUB+1,0             DONT NEED START/EDN DAY                      
         LA    R3,DUB                                                           
         LA    R4,LNBDAY                                                        
         BAS   RE,FDAY                                                          
         SPACE 2                                                                
         SPACE 2                                                                
         LA    R3,ITIM                                                          
         LA    R4,LNBTIM                                                        
         BAS   RE,FTIM                                                          
         CLI   LNBTIM,X'F0'                                                     
         BNE   *+8                                                              
         MVI   LNBTIM,X'40'                                                     
*                                                                               
         TM    ISTAT2,X'20'        SECONDARY INTERVAL ERROR                     
         BZ    *+8                                                              
         MVI   LNBTIM-1,C'+'                                                    
         TM    ISTAT2,X'40'        PRIMARY                                      
         BZ    *+8                                                              
         MVI   LNBTIM-1,C'*'                                                    
         TM    ISTAT,X'20'         TEST IGNORE INTERVAL CHECK                   
         BZ    *+8                                                              
         MVI   LNBTIM-1,C'='                                                    
         TM    ISTAT,X'02'         TEST IGNORE TIME FOR MATCH                   
         BZ    *+8                                                              
         MVI   LNBTIM+5,C'T'       'T' AFTER TIME                               
*                                                                               
         SPACE 2                                                                
         EDIT  ILEN,(3,LNBLEN)                                                  
         SPACE 2                                                                
         LA    R3,IPRD                                                          
         LA    R4,LNBPRD                                                        
         BAS   RE,FPRD                                                          
*                                                                               
         CLI   IEST,0              IF HAVE ESTIMATE                             
         BE    FMTUN2                                                           
         ZIC   R0,IEST             SHOW IT NOW                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    RF,LNBPRD+7         IF HAVE 2ND PROD                             
         CLI   LNBPRD+3,C' '                                                    
         BH    *+8                                                              
         LA    RF,LNBPRD+3         IF NO 2ND PROD                               
         UNPK  1(3,RF),DUB                                                      
         MVI   0(RF),C'-'                                                       
*                                                                               
FMTUN2   DS    0H                                                               
         MVC   DUB(4),ICOST                                                     
         L     R0,DUB                                                           
         CLI   PSEUDOPT,C'R'       FOR RESPONSES                                
         BNE   FMTUN3              SHOW 'COST' DIFFERENTLY                      
         EDIT  (R0),(9,LNBCOST),COMMAS=YES                                      
         B     FMTUN3B                                                          
*                                                                               
FMTUN3   DS    0H                                                               
         TM    ISTAT,X'01'                                                      
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         CLI   CENTS,C'Y'                                                       
         BE    *+8                                                              
         MH    R0,=H'100'                                                       
         EDIT  (R0),(9,LNBCOST),2,FLOAT=-                                       
*                                                                               
         TM    ISTAT2,ISTCTRQ      IF CONTRACT TRADE ITEM                       
         BZ    FMTUN3B                                                          
         LA    RF,LNBCOST+7        FLOAT A T                                    
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   0(RF),C'T'                                                       
*                                                                               
FMTUN3B  DS    0H                                                               
         TM    CBLHOPT,CBLHNHAQ    IF CBH REQ AND NEW MODE AND ALL NETS         
         BNO   FMTUN3D                                                          
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),BMS+2                                                   
         NI    DUB+4,X'80'                                                      
         OC    DUB+4(1),ICBLNET                                                 
*                                  UNPACK CABLE NETWORK                         
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPMED,ICSMED                                                   
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPSTA,DUB+2                                                    
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LNBNTWK,STAPQNET                                                 
*                                                                               
FMTUN3D  DS    0H                                                               
         CLI   FILMRSW,C'Y'                                                     
         BNE   FMTUN5                                                           
*                                                                               
         CLI   IFILM,0             FILMS                                        
         BNE   FMTUN3G                                                          
         MVC   LNBFILM(4),=C'NONE'                                              
*                                                                               
         CLI   TRAFIDSC,C'Y'       IF INVALID/MISSING FILMS CAUSE               
         BNE   FMTUN4              DISCREPANCY                                  
         MVI   LNBFILM+4,C'*'      INDICATE WITH A *                            
         B     FMTUN4                                                           
*                                                                               
FMTUN3G  DS    0H                                                               
         LA    R3,IFILM                                                         
         LA    R4,LNBFILM                                                       
         BAS   RE,FFILM                                                         
*                                                                               
         CLI   IFILM2,0                                                         
         BE    FMTUN4                                                           
         MVI   LNBFILM+8,C'-'                                                   
         LA    R3,IFILM2                                                        
         LA    R4,LNBFILM+9                                                     
         BAS   RE,FFILM                                                         
*                                                                               
FMTUN4   DS    0H                                                               
         TM    ISTAT2,X'04'        TEST PAST RECALL DATE                        
         BZ    FMTUN5                                                           
         LA    R4,LNBFILM+9                                                     
         CLI   IFILM2,0            DO WE HAVE 2ND FILM                          
         BE    *+8                                                              
         LA    R4,9(R4)                                                         
         MVC   0(7,R4),=C'(DATES)'                                              
*                                                                               
FMTUN5   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                   GET MATCHING INVOICES                                       
         SPACE 3                                                                
GETINV   NTR                                                                    
         SPACE 2                                                                
         L     R8,AFINV                                                         
         SR    R5,R5                                                            
         IC    R5,BYNOWK                                                        
         SR    R6,R6                                                            
         IC    R6,BYUNACH          R6 = UNACHIEVED                              
         SR    R5,R6               R5 = ACHIEVED                                
         SPACE 2                                                                
         LTR   R5,R5                                                            
         BZ    GI4                 NO MATCHES                                   
         LR    R1,R7                                                            
         LA    R1,1(R1)                                                         
         S     R1,AFBY                                                          
         STH   R1,HALF             HALF = RELATIVE BUY ADDRESS                  
GI2      CLC   HALF,IBY                                                         
         BE    GI7                                                              
GI3      LA    R8,IEL$MLEN(R8)                                                  
         C     R8,ALINV                                                         
         BNH   GI2                                                              
         SPACE 2                                                                
*                             HAVE EXHAUSTED INVOICES                           
*                             REPEAT ORDERED INFO FOR UNACHIEVED SPOTS          
GI4      LTR   R6,R6                                                            
         BZ    GIEXT                    NO UNMATCHED BUYS LEFT                  
*                                                                               
GI5      XC    8(L'ICSLIN1,R2),8(R2)                                            
         MVC   8(L'ORDLINW,R2),ORDLINW                                          
         FOUT  (R2)                                                             
         LA    R2,L'ICSLIN1+8(R2)                                               
         LA    R0,ICSLAST                                                       
         CR    R2,R0                                                            
         BL    GI6                                                              
         BAS   RE,NEXTWA                                                        
         BNZ   GIEXTX                                                           
         BAS   RE,HDLA                                                          
GI6      BCT   R6,GI5                                                           
         B     GIEXT                    NO UNMATCHED BUYS LEFT                  
         SPACE 3                                                                
GI7      DS    0H                                                               
         CLI   FILMRSW,C'Y'        TEST DOING FILM OPTION                       
         BNE   GI7B                                                             
         CLI   IFILM2,0            IF HAVE 2ND FILM                             
         BE    GI7B                                                             
         LA    R0,ICSLINIH         WILL NEED 2 LINES                            
         CR    R2,R0                                                            
         BNH   GI7B                                                             
         BAS   RE,NEXTWA                                                        
         BNZ   GIEXTX                                                           
         BAS   RE,HDLA                                                          
*                                                                               
GI7B     DS    0H                                                               
         USING LNAD,R2                                                          
         FOUT  (R2)                                                             
*                                            FORMAT INVOICE INFO                
         MVI   LNA,C' '                                                         
         MVC   LNA+1(L'LNA-1),LNA                                               
         MVC   LNA(L'ORDLINW),ORDLINW                                           
*                                                                               
         CLI   PSEUDOPT,C'R'       IF RESPONSES                                 
         BNE   G17B6                                                            
         SR    R0,R0                                                            
         ICM   R0,15,ICOST          SHOW INV "COST"                             
         LA    RE,LNACOST+11                                                    
         EDIT  (R0),(8,(RE)),COMMAS=YES                                         
*                                                                               
G17B6    DS    0H                                                               
         LA    R3,IDAT                                                          
         LA    R4,LNAIDTE                                                       
         BAS   RE,FDAT                                                          
         CLI   FILMRSW,C'Y'        FOR FILM REPORT                              
         BE    GI7C                NO DAY                                       
         MVC   DUB(1),IDAY         DAY OF WEEK                                  
         MVI   DUB+1,0             DONT NEED START/EDN DAY                      
         LA    R3,DUB                                                           
         LA    R4,LNAIDAY                                                       
         BAS   RE,FDAY                                                          
*        SPACE 1                                                                
GI7C     DS    0H                                                               
         LA    R3,ITIM                                                          
         LA    R4,LNAITIM                                                       
         CLI   FILMRSW,C'Y'        FOR FILM REPORT                              
         BNE   *+8                                                              
         LA    R4,LNAITIMF         TIME IN DIFFERENT PLACE                      
         BAS   RE,FTIM                                                          
         CLI   0(R4),C'0'                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         BCTR  R4,R0                                                            
*                                                                               
         TM    ISTAT2,X'20'        SECONDARY INTERVAL ERROR                     
         BZ    *+8                                                              
         MVI   0(R4),C'+'                                                       
         TM    ISTAT2,X'40'        PRIMARY                                      
         BZ    *+8                                                              
         MVI   0(R4),C'*'                                                       
         TM    ISTAT,X'20'         TEST IGNORE INTERVAL CHECK                   
         BZ    *+8                                                              
         MVI   0(R4),C'='                                                       
*                                                                               
         TM    ISTAT,X'02'         TEST IGNORE TIME FOR MATCH                   
         BZ    GI7H                                                             
         CLI   FILMRSW,C'Y'        DONT FLAG ON FILM RPT                        
         BE    GI7H                                                             
         MVI   LNAITIM+5,C'T'      'T' AFTER TIME                               
*                                                                               
GI7H     DS    0H                                                               
         CLI   FILMRSW,C'Y'        TEST DOING FILM OPTION                       
         BNE   GI7J                                                             
         MVC   LNAIFILM+2(4),=C'NONE'                                           
         CLI   IFILM,0                                                          
         BE    GI7J                                                             
         LA    R3,IFILM                                                         
         LA    R4,LNAIFILM                                                      
         BAS   RE,FFILM                                                         
*                                                                               
GI7J     DS    0H                                                               
         LA    R2,L'ICSLIN1+8(R2)                                               
         LA    R0,ICSLAST                                                       
         CR    R2,R0                                                            
         BL    GI8                                                              
         BAS   RE,NEXTWA                                                        
         BNZ   GIEXTX                                                           
         BAS   RE,HDLA                                                          
*                                                                               
GI8      DS    0H                                                               
         CLI   FILMRSW,C'Y'                                                     
         BNE   GI8B                                                             
         CLI   IFILM2,0            SEE IF HAVE TO DO 2ND FILM                   
         BE    GI8B                                                             
         MVI   LNA,C' '                                                         
         MVC   LNA+1(L'LNA-1),LNA                                               
         FOUT  (R2)                                                             
         LA    R3,IFILM2                                                        
         LA    R4,LNAIFILM                                                      
         BAS   RE,FFILM                                                         
         LA    R2,L'ICSLIN1+8(R2)                                               
         LA    R0,ICSLAST                                                       
         CR    R2,R0                                                            
         BL    GI8B                                                             
         BAS   RE,NEXTWA                                                        
         BNZ   GIEXTX                                                           
         BAS   RE,HDLA                                                          
*                                                                               
GI8B     BCT   R5,GI3                                                           
         B     GI4                 NO MATCHING INVOICES LEFT                    
         SPACE 2                                                                
GIEXT    ST    R2,LINPOINT                                                      
GIXIT    XIT1                                                                   
         SPACE 1                                                                
GIEXTX   MVI   LINPOINT,X'FF'      SET EXIT                                     
         B     GIXIT                                                            
         EJECT                                                                  
*                   DAY OF WEEK                                                 
         SPACE 3                                                                
FDAY     CLC   DAYINBH,0(R3)                                                    
         BE    FDAY2                                                            
         ST    RE,FULL                                                          
         GOTO1 VCODAY,DMCB,(1(R3),0(R3)),DAYOTH                                 
         MVC   DAYINBH,0(R3)                                                    
         L     RE,FULL                                                          
FDAY2    MVC   0(8,R4),DAYOTH                                                   
         BR    RE                                                               
         SPACE 2                                                                
FFILM    DS    0H                  FORMAT FILMS                                 
         ICM   R0,15,FLMTPARS+8                                                 
         BZR   RE                  NO FILMS                                     
         L     RF,AFLMTAB                                                       
         USING FLMTABD,RF                                                       
*                                                                               
FF4      DS    0H                                                               
         CLC   FLMIDNO,IINVID      TST FROM RIGHT INVOICE                       
         BNE   FF10                                                             
         CLC   FLMICOD,0(R3)       INTERNAL FILM NO.                            
         BNE   FF10                                                             
*                                                                               
         MVC   0(8,R4),FLMCOD      SET FILM CODE                                
         BR    RE                                                               
*                                                                               
FF10     DS    0H                                                               
         LA    RF,FLMTABEL(RF)                                                  
         BCT   R0,FF4                                                           
         BR    RE                                                               
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
*        FOFILM - FIND ORDERED FILM                                             
***********************************************************************         
         SPACE 1                                                                
FOFILM   NTR1                                                                   
         LA    RF,WORK                                                          
         XC    WORK,WORK                                                        
         XC    DUB,DUB                                                          
         USING FLMTABD,RF                                                       
*                                                                               
         MVC   FLMSEQ,0(R3)                                                     
         GOTO1 VBINSRCH,FLMTPARS,(2,FLMTABD) READ HIGH                          
         CLI   0(R1),1                                                          
         BE    FOF06                                                            
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         BZ    FOF06                                                            
         CLC   FLMSEQ,WORK+FLMSEQ-FLMTABD   DID WE FIND IT?                     
         BNE   FOF06                                                            
         MVC   DUB,FLMCOD                                                       
         B     FOFILMX                                                          
*                                                                               
FOF06    DS    0H                  FILM NOT IN TABLE                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLPID,=X'0AA1'                                                  
         MVC   CMLPAM,HKEY+1      AGENCY/MEDIA                                  
         OC    CMLPCLT,SVTRACLT    USE TRAFFIC OVERRIDE CLIENT                  
         BNZ   *+10                IF ANY                                       
         MVC   CMLPCLT,BCLT                                                     
         MVC   CMLPSEQ+1(2),0(R3)                                               
         MVC   KEYSAVE,KEY                                                      
         IC    R2,TERMNAL                                                       
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'TRFDIR',KEY,KEY,((R2),0)             
*****    GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,((R2),0)             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FOF10                                                            
         EDIT  (B2,0(R3)),(4,DUB+2),FILL=0                                      
         MVC   DUB(2),=C'**'                                                    
         MVC   DUB+6(2),=C'**'                                                  
         B     FOFILMX                                                          
*                                                                               
FOF10    DS    0H                                                               
         IC    R2,TERMNAL                                                       
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'TRFFILE',KEY+14,IOAREA,     X        
               ((R2),DMWORK)                                                    
***      GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,IOAREA,    X         
***            ((R2),DMWORK)                                                    
         LA    R4,IOAREA                                                        
         MVC   DUB,CMLKCML         SET COMMERCIAL CODE                          
*                                                                               
         LA    RF,WORK             ADD TO BINSRCH TABLE                         
         XC    WORK,WORK                                                        
         MVC   FLMSEQ,0(R3)                                                     
         MVC   FLMCOD,CMLKCML                                                   
         GOTO1 VBINSRCH,FLMTPARS,(1,FLMTABD)  (OK IF TABLE FILLS UP)            
*                                                                               
FOFILMX  XIT1                                                                   
         DROP  RF                                                               
         SPACE 2                                                                
*                   DATE FORMAT                                                 
         SPACE 2                                                                
FDAT     CLC   DATINBH,0(R3)                                                    
         BE    FDAT2                                                            
         ST    RE,FULL                                                          
         MVC   DATINBH,0(R3)                                                    
         GOTO1 VDATCON,DMCB,(2,0(R3)),(5,DATCHAH)                               
         SPACE 1                                                                
         L     RE,FULL                                                          
FDAT2    MVC   0(5,R4),DATCHAH                                                  
         BR    RE                                                               
         SPACE 3                                                                
*                   FORMAT PRODUCTS                                             
         SPACE 2                                                                
FPRD     ST    RE,FULL                                                          
         MVC   0(3,R4),PRDQH                                                    
         CLC   PRDBH,0(R3)                                                      
         BE    FPRD2                                                            
         MVC   PRDBH,0(R3)         SAVE PRD                                     
         BAS   RE,GETPRD                                                        
         MVC   PRDQH,0(R4)         SAVE MNEMONIC                                
FPRD2    LA    R3,1(R3)                                                         
         CLI   0(R3),0                                                          
         BE    FPRD3               NO SECOND PRD                                
         MVI   3(R4),C'-'                                                       
         MVC   4(3,R4),PRDQ2H                                                   
         CLC   PRDB2H,0(R3)                                                     
         BE    FPRD3                                                            
         MVC   PRDB2H,0(R3)                                                     
         LA    R4,4(R4)                                                         
         BAS   RE,GETPRD                                                        
         MVC   PRDQ2H,0(R4)                                                     
FPRD3    L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 3                                                                
*                                  FORMAT TIMES                                 
         SPACE 2                                                                
FTIM     ST    RE,FULL                                                          
         MVC   DUB(2),0(R3)                                                     
         LH    R1,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         MH    R1,=H'100'                                                       
         AR    R0,R1                                                            
         CH    R0,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R0,=H'2400'                                                      
         STH   R0,DUB                                                           
         GOTO1 VTIMUNPK,DMCB,DUB,(R4)                                           
         SPACE 1                                                                
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
*        DEFTEST - DETERMINE IF SPOT 'DEFERRED'                                 
         SPACE 1                                                                
DEFTEST  DS    0H                                                               
         CLI   PROGPROF+9,C'C'     NOT IF CALENDAR MONTHS                       
         BE    DEFTN                                                            
         CLC   BYEDT,BEND          IF SPOT END DATE IS OUTSIDE MONTH            
         BNH   DEFTN                                                            
         TM    BYSTAT,X'02'        AND ITS A POL SPOT                           
         BZ    DEFTN                                                            
         CLI   BYUNACH,0           AND ITS NOT MATCHED                          
         BE    DEFTN                                                            
DEFTY    CR    RE,RE               YES, CC=                                     
         BR    RE                                                               
DEFTN    LTR   RE,RE               NO, CC NOT =                                 
         BR    RE                                                               
         EJECT                                                                  
*                   ORDERED - INVOICE HEADLINE                                  
         SPACE 3                                                                
HDLA     MVC   ICSLIN1,HDA1+1                                                   
         MVC   ICSLIN2,HDA2+1                                                   
*                                                                               
         CLI   NETINV,C'Y'                                                      
         BNE   *+10                                                             
         MVC   ICSLIN2+39(08),=C'NET COST'                                      
*                                                                               
         CLI   PSEUDOPT,C'R'                                                    
         BNE   HDLA2                                                            
         MVC   ICSLIN2+39(08),=C'EST-LIN '                                      
         MVC   ICSLIN2+47(10),=C'  RESPONSE'                                    
         MVC   ICSLIN1+46(23),=C'  * * * * * INVOICE * *'                       
*                                                                               
HDLA2    DS    0H                                                               
         CLI   FILMRSW,C'Y'                                                     
         BNE   *+10                                                             
         MVC   ICSLIN2+64(12),=C' TIME   FILM'                                  
*                                                                               
         LA    R2,ICSLIN3H                                                      
         FOUT  ICSLIN1H                                                         
         FOUT  ICSLIN2H                                                         
         BR    RE                                                               
         SPACE 3                                                                
*                   UNORDERED HEADLINE                                          
         SPACE 3                                                                
HDLB     LA    R0,ICSLINHH                                                      
         CR    R2,R0                                                            
         BL    HDLB2                                                            
         ST    RE,FULL                                                          
         BAS   RE,NEXTWA           NEED 2 LINES FOR HEADLINE                    
*                                  AND 1 FOR DETAIL                             
         BZ    *+12                                                             
         MVI   LASTNET,0           MAKE SURE THIS IS RESET!                     
         B     IREXT                                                            
         L     RE,FULL                                                          
HDLB2    MVC   8(L'ICSLIN1,R2),HDB1+1                                           
         FOUT  (R2)                                                             
         LA    R2,L'ICSLIN1+8(R2)                                               
         MVC   8(L'ICSLIN1,R2),HDB2+1                                           
*                                                                               
         CLI   NETINV,C'Y'                                                      
         BNE   *+10                                                             
         MVC   8+41(08,R2),=C'NET COST'                                         
*                                                                               
         CLI   PSEUDOPT,C'R'                                                    
         BNE   *+10                                                             
         MVC   8+41(08,R2),=C'RESPONSE'                                         
*                                                                               
         TM    CBLHOPT,CBLHNHAQ  IF CBH REQ AND NEW MODE AND ALL NETS           
         BNO   HDLB3                                                            
         MVC   8+49(4,R2),=C'NTWK'                                              
*                                                                               
HDLB3    DS    0H                                                               
         CLI   FILMRSW,C'Y'                                                     
         BNE   *+10                                                             
         MVC   8+54(4,R2),=C'FILM'                                              
*                                                                               
         FOUT  (R2)                                                             
         LA    R2,L'ICSLIN1+8(R2)                                               
         MVI   PAGSW,C'U'          SET PAGE SW TO UNORDERED                     
         BR    RE                                                               
         SPACE 3                                                                
*                   CLEAR SCREEN                                                
         SPACE 3                                                                
SCRNCLR  LA    R4,L'ICSLIN1+8                                                   
         LA    R4,L'ICSLIN1+8                                                   
         LA    R5,ICSLAST-1                                                     
SC1      OC    8(L'ICSLIN1,R2),8(R2)                                            
         BZ    SC2                                                              
         XC    8(L'ICSLIN1,R2),8(R2)                                            
         FOUT  (R2)                                                             
SC2      BXLE  R2,R4,SC1                                                        
         BR    RE                                                               
         EJECT                                                                  
*                            GET PRODUCT MNEMONIC                               
         SPACE 3                                                                
GETPRD   LA    R1,IOAREA                                                        
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         XC    0(3,R4),0(R4)       CLEAR OUTPUT                                 
         CLI   0(R3),0                                                          
         BCR   8,RE                NO INPUT                                     
GP2      CLI   3(R1),0                                                          
         BCR   R8,RE                                                            
         CLC   0(1,R3),3(R1)                                                    
         BE    GP3                                                              
         LA    R1,4(R1)                                                         
         B     GP2                                                              
GP3      MVC   0(3,R4),0(R1)                                                    
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
*                  PAGING                                                       
         SPACE 3                                                                
NEXTWA   LA    R0,ICSLAST                                                       
         CR    R2,R0                                                            
         BNL   *+16                                                             
         ST    RE,DUB                                                           
         BAS   RE,SCRNCLR          CLEAR REST OF SCREEN                         
         L     RE,DUB                                                           
         AP    THISPAG,=P'1'                                                    
         CLI   PAGSW,C'T'                                                       
         BE    NT4                 TOTAL                                        
         CLC   PAGIN,=C'UN'        UNORDED                                      
         BNE   NT2                                                              
         CLI   PAGSW,C'U'                                                       
         BE    NT4                                                              
         B     NT5                                                              
NT2      CLC   PAGIN,=C'TO'         TOTAL                                       
         BE    NT5                                                              
         CP    THISPAG,PAGIN                                                    
         BL    NT5                                                              
*                                       PUT PAGE NO. ON SCREEN                  
NT4      DS    0H                                                               
         CLC   ICSPAG,=C'PAY'      DONT DISTURB AUTOPAY                         
         BE    NT4D                                                             
         XC    ICSPAG,ICSPAG                                                    
         EDIT  (P2,THISPAG),(3,ICSPAG),ALIGN=LEFT                               
         STC   R0,ICSPAGH+7       OUTPUT LENGTH                                 
NT4D     DS    0H                                                               
         OI    ICSPAGH+6,X'01'     FORCE INPUT NEXT TIME                        
         FOUT  ICSPAGH                                                          
*                                  CC NON-ZERO - NO MORE                        
         BR    RE                                                               
NT5      LA    R2,ICSLIN1H         FIRST LINE                                   
         SR    R0,R0               CC ZERO - KEEP GOING                         
         BR    RE                  RETURN                                       
         EJECT                                                                  
*                                                                               
*        COLUMN HEADINGS FOR MATCHING REPORT                                    
*                                                                               
*        NOTE- THESE ARE ADJUSTED AFTER BEING MOVED TO                          
*              OUTPUT LINES BASED ON CERTAIN REPORT OPTIONS                     
*                                                                               
HDA1     DS    0CL80                                                            
         DC    CL40' * * * * * * * * * * O R D E R E D * * *'                   
         DC    CL40' * * * * * * * *   * * INVOICE * *      '                   
         SPACE 1                                                                
HDA2     DS    0CL80                                                            
         DC    CL40'  DATE DAYS         TIME    LEN PRODUCT '                   
         DC    CL40'   COST  EST-LIN    DATE DAY  TIME      '                   
         SPACE 1                                                                
HDB1     DS    0CL80                                                            
         DC    CL40' * * * * * * * UNORDERED SPOTS * * * * *'                   
         DC    CL40' * *                                    '                   
         SPACE 1                                                                
HDB2     DS    0CL80                                                            
         DC    CL40'  DATE  DAY   TIME  LENGTH  PRODUCT     '                   
         DC    CL40'    COST                                '                   
         SPACE 1                                                                
HDC1     DS    0CL80                                                            
         DC    CL40'    *MATCHED*  *ORDERED NOT RUN**RUN NOT'                   
         DC    CL40' ORDERED**ORDERED TOTAL**INVOICE TOTAL* '                   
         SPACE 1                                                                
GOODMSG  DC    C'**MATCHING SUCCESSFUL**'                                       
BADMSG   DC    C'**MATCHING NOT SUCCESSFUL**'                                   
NOMSG    DC    C'**NO DATA FOUND**'                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
         SPACE 3                                                                
*                   ORDERED - INVOICE DETAIL LINE DSECT                         
*                   LNA                                                         
         SPACE 3                                                                
LNAD     DSECT                                                                  
         DS    CL8                                                              
LNA      DS    0CL78                                                            
LNADTE   DS    CL5                                                              
         DS    CL1                                                              
LNADAYS  DS    CL8                                                              
         DS    CL1                                                              
LNATIM   DS    CL11                                                             
         DS    CL1                                                              
LNALEN   DS    CL3                                                              
         DS    CL1                                                              
LNAPRD   DS    CL7                                                              
LNACOST  DS    CL9                                                              
         DS    CL1                                                              
LNAESTL  DS    CL7                                                              
         DS    CL1                                                              
         DS    CL2                                                              
LNAIDTE  DS    CL5                                                              
         DS    CL1                                                              
LNAIDAY  DS    CL3                                                              
         DS    CL1                                                              
LNAITIM  DS    CL5                                                              
         DS    CL5                                                              
         ORG   LNAIDAY             REDEFINE FOR FILM FORMAT                     
LNAITIMF DS    CL5                                                              
         DS    CL1                                                              
LNAIFILM DS    CL8                                                              
*                                                                               
         SPACE 3                                                                
*                   UNORDERED SPOT  DETAIL LINE DSECT                           
*                   LNB                                                         
         SPACE 3                                                                
LNBD     DSECT                                                                  
         DS    CL8                                                              
LNB      DS    0CL78                                                            
LNBDTE   DS    CL5                                                              
         DS    CL2                                                              
LNBDAY   DS    CL3                                                              
         DS    CL2                                                              
LNBTIM   DS    CL5                                                              
         DS    CL5                                                              
LNBLEN   DS    CL3                                                              
         DS    CL2                                                              
LNBPRD   DS    CL11                                                             
         DS    CL1                                                              
LNBCOST  DS    CL9                                                              
         DS    CL1                                                              
LNBNTWK  DS    CL3                                                              
         DS    CL2                                                              
LNBFILM  DS    CL17                                                             
         DS    CL16                                                             
         SPACE 3                                                                
         EJECT                                                                  
*INCLUDE DDOMFACS/SPTRCMML/SPSTAPACKD                                           
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPADINTD                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
         SPACE 2                                                                
       ++INCLUDE SPMATWKN                                                       
