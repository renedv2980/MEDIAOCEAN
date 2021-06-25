*          DATA SET NEPAY02    AT LEVEL 076 AS OF 11/20/17                      
*PHASE T31302A                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'NETPAK PAY PROGRAM - CLEARANCE OVERLAY - T31302'                
******************************************************************              
*************************  HISTORY  ******************************              
*LEVEL 70  MIDAS SPECIAL CHARGE / ALT ACC AGENCY / $0 CLEARANCES *              
*LEVEL 71  FIXES BUGS IN $) CLEARANCE WHEN AGY NOT SET TO CLEAR 0$              
*LEVEL 71 AND 0$ SI FIRST OR LAST ON CLRST SCREEN                *              
*LEVE  72 FIXES BUG IN CLEARING TO OLDER CLRST REC WITH F1 ELEM  *              
*LEVEL 73 CR/CK REVERSAL CODE                                    *              
******************************************************************              
T31302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CLPY**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING PAYWRKD,R9                                                       
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
*                                                                               
         MVI   AUTOAPP,C'N'                                                     
         TM    AGYFLG3,X'80'         AUTO-APPROVE?                              
         JZ    CL                                                               
         MVI   AUTOAPP,C'Y'                                                     
         BRAS  RE,CHAMTTYP                                                      
*                                                                               
CL       MVI   PASS,1              FIRST PASS THROUGH FILE                      
         LA    R1,CL4              SET HOOK ADDRESS                             
         ST    R1,NBHOOK                                                        
         OI    NBSBKEND,NBNODPT2                                                
         BAS   RE,SAVBLOCK                                                      
         XC    TOTG(8),TOTG        CLEAR RECORD BUCKETS                         
         XC    STATSEQ,STATSEQ     CLEAR SEQUENCE NUMBER                        
         MVI   DATADISP+1,27       SET FOR GETEL                                
         BAS   RE,TESTSTW          TEST FOR STW ESTIMATE                        
*                                                                               
         BAS   RE,TSARINIT                                                      
T        USING TSARD,TSARBLK                                                    
*                                                                               
         XC    DMCB,DMCB                                                        
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
*                                                                               
         XC    SVPASSWD,SVPASSWD                                                
         GOTO1 (RF),DMCB,(2,0)           REGULAR GETFACT CALL                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'                                                    
         BZ    CL3                                                              
         MVC   SVPASSWD,FAPASSWD                                                
         DROP  R1                                                               
*                                                                               
* REVERSE OPTION BYPASS UNIT LOGIC CALCULATE                                    
* THE CLEARENCE AMOUNT USING CLRST RECORDS                                      
*                                                                               
CL2      DS    0H                                                               
         OC    CRCKDAT,CRCKDAT     REVERSE OPTION  DONT UPDATE UNITS            
         BZ    CL3                 GO TO REGULAR MAINT                          
         CLI   PASS,2                                                           
         BE    CL20                                                             
         MVI   FDCRCKSW,C'Y'                                                    
         BAS   RE,BLDST             DUMMY CALL TO GET SEQUENCE NUMBER           
         MVI   FDCRCKSW,C'N'                                                    
         BRAS  RE,FINDCRCK         FIND REVERSE AMOUNT                          
         MVI   STATSEQ,0           RESET SEQUENCE NUMBER                        
         MVI   USERPAY,YES         SET PAYABLE UNIT SWITCH                      
         OC    CRCKDAT,CRCKDAT     REVERSE OPTION  DONT UPDATE UNITS            
         BNZ   CL13                GO TO CLRST MAINT                            
                                                                                
*                                                                               
* READ UNIT RECORDS                                                             
*                                                                               
CL3      DS    0H                                                               
         GOTO1 VNETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST     TEST FOR END-OF-FILE                         
         BNE   CL3                 NO-GET NEXT RECORD                           
         CLI   RECSW,YES           TEST FOR QUALIFIED RECORDS                   
         BE    CL10                                                             
         MVC   PAYMSG(L'NODATA),NODATA                                          
         B     CLX                                                              
         SPACE                                                                  
* FIND PAYABLE TOTALS - ADD PAY ELEMENTS                                        
*                                                                               
CL4      ST    RE,HOOKREG                                                       
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RECORD                         
         BNE   CL9                 NO                                           
         GOTO1 VFILREC                                                          
         BNE   CL9                 SKIP THIS RECORD                             
         L     RE,NBAIO            UNIT RECORD                                  
         USING NURECD,RE                                                        
*                                                                               
         BAS   RE,CHKESTW          CHECK IF UNIT IS UNDER STW ESTIMATE          
         BNE   CL9                 IF IT IS - SKIP RECORD                       
*                                                                               
         TM    SPECREP,X'80'       TEST FOR ANY SPEC REP FILTER                 
         BO    *+30                YES                                          
         CLC   SPECREP,NBSREP      TEST FOR MATCH ON SPECIAL REP                
         BE    CL410               YES                                          
*                                                                               
         BRAS  RE,GETSPRAT         CHECK SREP AGAINST S-RATE ELEMENTS           
         CLI   DMCB+12,0           GOOD RETURN CODE                             
         BNE   CL9                 NO-SKIP RECORD                               
         B     CL410               YES                                          
*                                                                               
         OC    NBSREP,NBSREP       TEST FOR ANY SPECIAL REP                     
         BNZ   CL410               YES GOOD PASS                                
*                                                                               
         BRAS  RE,GETSPRAT         CHECK SREP AGAINST S-RATE ELEMENTS           
         CLI   DMCB+12,0           GOOD RETURN CODE                             
         BNE   CL9                 NO-SKIP RECORD                               
         B     CL410               YES                                          
*                                                                               
CL410    CLI   PAYPROF2,YES                                                     
         BNE   CL500                                                            
         CLI   SVNAFF,YES                                                       
         BE    CL500                                                            
         L     RE,NBAIO                                                         
         TM    NUUNITST,X'02'      IS UNIT MISSED                               
         BO    CL500                                                            
         L     R6,NBAIO            UNIT RECORD                                  
         MVI   ELCODE,X'21'        COMMERCIAL SCHEDULED                         
         BAS   RE,GETEL                                                         
         BNE   CL420                                                            
         USING NUCMLEL,R6                                                       
         LA    R2,NUCML1                                                        
*                                                                               
         L     R6,NBAIO            COMMERCIAL THAT AIRED                        
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BE    CL440                                                            
CL420    MVI   FERN,PCMLMTHM                                                    
         B     CL50                                                             
*                                                                               
         USING NUPCELD,R6                                                       
CL440    CLI   NUPCLEN,15          NEW LEN WITH 3 CHAR PROD?                    
         BNE   CL441                                                            
         CLC   NUPCAPRD,NBPR1CL3   YET/CHECK ALPHA CODE                         
         BNE   CL442                                                            
CL441    CLC   NUPCPRD,NBPRD       COMPARE AGAINST 1ST OR 2ND PRD               
         BNE   CL442                                                            
CL442    CLC   0(8,R2),NUPCCML     1ST                                          
         BNE   CL420                                                            
         B     *+14                                                             
         CLC   8(8,R2),NUPCCML     2ND                                          
         BNE   CL420                                                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    *+18                                                             
         OC    8(8,R2),8(R2)                                                    
         BNZ   CL420                                                            
         B     CL500                                                            
*                                                                               
         CLI   NUPCLEN,15          NEW LEN WITH 3 CHAR PROD?                    
         BNE   CL450                                                            
         CLC   NUPCAPRD,NBPR1CL3                                                
         BNE   CL460                                                            
CL450    CLC   NUPCPRD,NBPRD       COMPARE AGAINST 1ST OR 2ND PRD               
         BNE   CL460                                                            
         CLC   0(8,R2),NUPCCML     1ST                                          
         BNE   CL420                                                            
         B     *+14                                                             
CL460    CLC   8(8,R2),NUPCCML     2ND                                          
         BNE   CL420                                                            
         DROP  R6                                                               
*                                                                               
CL500    MVI   RECSW,YES           SET SWITCH FOR QUALIFIED RECORD              
         LA    R5,BLOCK                                                         
         USING PAYBLKD,R5                                                       
         MVI   PAYFUNCT,PAYELS                                                  
         MVC   PAYAELS,AIOAREA3                                                 
*                                                                               
*                                                                               
CL500A   GOTO1 VGETPAY,DMCB,PAYWRKD,PAYBLKD                                     
*                                                                               
CL500B   MVC   FERN,PAYERROR                                                    
         CLI   FERN,0              TEST FOR ERROR                               
         BNE   CL50                LET ERROR ROUTINE HANDLE IT                  
         TM    PAYSTAT,PAYABLE     TEST IF PAYABLE                              
         BZ    CL9                 NO                                           
         MVI   USERPAY,YES         SET PAYABLE UNIT SWITCH                      
         L     R0,PAYGROSS                                                      
         A     R0,TOTG                                                          
         ST    R0,TOTG                                                          
         L     R0,PAYNT                                                         
         A     R0,TOTN                                                          
         ST    R0,TOTN                                                          
*                                                                               
         CLI   PASS,2                                                           
         BNE   *+8                                                              
         BAS   RE,CHKTSAR                                                       
*                                                                               
         ZIC   R2,PAYNELS          COUNTER                                      
         L     R3,PAYAELS          POINTER TO ELEMENTS                          
         USING NUPAYD,R3           POINTS TO PAY ELEMENTS                       
CL5      MVC   NUPAYSEQ,STATSEQ     MOVE SEQUENCE TO PAY ELEM                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),NBAIO,(R3),ADDCODE                    
         CLI   12(R1),0            TEST FOR SUCCESSFUL ADD                      
         BE    CL6                 YES                                          
         MVI   FERN,TOOLARGE                                                    
         CLI   12(R1),5            TEST FOR RECORD OVERFLOW                     
         BE    CL50                                                             
         DC    H'0'                                                             
*                                                                               
CL6      DS    0H                                                               
         CLI   NUPAYTYP,C'T'                                                    
         BE    CL7                                                              
         CLI   NUPAYTYP,C'I'                                                    
         BE    CL7                                                              
*                                                                               
         MVC   BYTE,NUPAYTYP                                                    
         BAS   RE,PAYSPCH          SET PAID BIT FOR SPECIAL CHARGE              
*                                                                               
CL7      ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         BCT   R2,CL5                                                           
         DROP  R3                                                               
         SPACE 1                                                                
CL8      CLI   ACTION,TEST                                                      
         BE    CL9                                                              
         CLI   PASS,2              TEST FOR SECOND PASS                         
         BNE   CL9                 NO                                           
*                                                                               
         MVI   NBUPUNIT,YES        SET TO UPDATE RECORD                         
         SPACE 1                                                                
CL9      L     RE,HOOKREG          GO BACK FOR NEXT                             
         BR    RE                  RETURN TO NETIO                              
         SPACE 1                                                                
* END-OF-FILE, TEST FOR MATCH BETWEEN INVOICE AND FILE TOTALS                   
*                                                                               
CL10     CLI   USERPAY,YES         TEST FOR ANY PAYABLE UNITS                   
         BE    CL12                YES-AT LEAST ONE                             
         MVI   FERN,NOPAYERR                                                    
         LA    R2,PAYACTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE 1                                                                
CL12     MVC   BYTE,PAYPROF        GROSS/NET OPTION                             
         CLI   ACTION,TEST                                                      
         BNE   *+18                                                             
         CLI   AMTOPT,0                                                         
         BE    *+10                                                             
         MVC   BYTE,AMTOPT                                                      
*                                                                               
         L     R0,TOTG                                                          
         CLI   BYTE,C'G'                                                        
         BE    *+8                                                              
         L     R0,TOTN                                                          
         C     R0,TOTAMTLO         TEST FOR WITHIN LOW RANGE                    
         BL    CL15                NO-EXIT WITH MESSAGE                         
         C     R0,TOTAMTHI                                                      
         BH    CL15                NO                                           
*                                                                               
CL13     CLI   ACTION,TEST         TOTALS MATCH-CHECK FOR ACTION TEST           
         BE    CL25                                                             
         CLI   PASS,2              TEST FOR SECOND PASS                         
         BE    CL20                YES-GENERATE CLEARANCE REQUESTS              
         MVI   PASS,2                                                           
*!!!     CLI   AUTOAPP,C'Y'                                                     
*!!!     BE    *+8                                                              
         BAS   RE,BLDST            BUILDS CLEARENCE STATUS RECORD               
         BAS   RE,RESTBLK          RESTORE START OF READ                        
         XC    TOTG(8),TOTG                                                     
         B     CL2                                                              
         SPACE 2                                                                
* DISCREPANCY - SHOW INVOICE TOTAL, FILE GROSS/NET                              
*                                                                               
CL15     CLI   ACTION,CLEAR        TEST FOR ACTION CLEAR                        
         BNE   *+14                NO                                           
         CLI   PASS,2              YES-IF ENTERING ON SECOND PASS               
         BNE   *+6                                                              
         DC    H'0'                BLOW UP BECAUSE ITS TROUBLE                  
*                                                                               
         MVI   OUTMSG,C' '         CLEAR OUTPUT MESSAGE AREA                    
         MVC   OUTMSG+1(L'OUTMSG-1),OUTMSG                                      
         MVC   OUTMSG(20),=C'TOTALS DO NOT AGREE.'                              
         MVI   OUTMSG+21,C'I'      INVOICE                                      
         MVC   OUTMSG+22(1),BYTE   GROSS/NET OPTION                             
         MVI   OUTMSG+23,EQUALS                                                 
         LA    R2,OUTMSG+24        SET OUTPUT POINTER                           
         L     R0,TOTAMT                                                        
         BAS   RE,CLEDT                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'G'          FILE GROSS                                   
         MVI   1(R2),EQUALS                                                     
         LA    R2,2(R2)                                                         
         L     R0,TOTG                                                          
         BAS   RE,CLEDT                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'N'          FILE NET                                     
         MVI   1(R2),EQUALS                                                     
         LA    R2,2(R2)                                                         
         L     R0,TOTN                                                          
         BAS   RE,CLEDT                                                         
         AR    R2,R0               UPDATE MESSAGE POINTER                       
*                                                                               
         LA    RE,OUTMSG           POINT RE AT START OF MESSAGE                 
         SR    R2,RE               FIND MESSAGE LENGTH                          
         LA    R1,L'PAYMSG         LENGTH OF PAY MESSAGE HEADER                 
         CR    R1,R2               FIELD LENGTH VS. MESSAGE LENGTH              
         BNH   *+6                                                              
         LR    R1,R2               USE MSG LEN IF LESS THAN FIELD LEN           
         BCTR  R1,0                EXECUTE LENGTH                               
         EX    R1,DISCMOVE                                                      
         B     CLX                                                              
         SPACE                                                                  
DISCMOVE MVC   PAYMSG(0),OUTMSG    SET DISCREPANCY MSG                          
         SPACE 2                                                                
* GENERATE CLEARANCES FOR ACTION CLEAR AND PUT OUT MESSAGE                      
*                                                                               
CL20     BAS   RE,CHK              GENERATE CLEARANCES                          
         SPACE                                                                  
CL25     CLI   ACTION,CLEAR                                                     
         BNE   CL30                                                             
*                                                                               
         TM    PAYFLAGS,X'06'      CR/CK REVERSE OPTION?                        
         BZ    CL30                                                             
         XC    PAYMSG,PAYMSG                                                    
         MVC   PAYMSG(16),=C'CR CHANGED TO CK'                                  
         TM    PAYFLAGS,REVRSCR                                                 
         BO    CL26                                                             
         MVC   PAYMSG(16),=C'CK CHANGED TO CR'                                  
         TM    PAYFLAGS,REVRSCK                                                 
         BO    CL26                                                             
         DC    H'0'                ???                                          
CL26     LA    R1,PAYOPT                                                        
         LA    R2,PAYLAST                                                       
CL27     CLC   =C'REVERSE',0(R1)                                                
         BE    CL28                                                             
         LA    R1,1(R1)                                                         
         CR    R1,R2                                                            
         BNE   CL27                                                             
         DC    H'0'                ???                                          
CL28     MVC   0(3,R1),=C'***'                                                  
         OI    PAYOPTH+6,X'80'     TRANSMIT                                     
         B     CL45                                                             
*                                                                               
CL30     XC    PAYMSG,PAYMSG                                                    
         MVC   PAYMSG(10),=C'** PAID **'                                        
         LA    R2,PAYMSG+11                                                     
         CLI   ACTION,CLEAR                                                     
         BE    *+14                                                             
         MVC   PAYMSG(18),=C'** TOTALS MATCH **'                                
         LA    R2,PAYMSG+19                                                     
         MVC   0(5,R2),=C'GROSS'                                                
         MVI   5(R2),EQUALS                                                     
         LA    R2,6(R2)                                                         
         L     R0,TOTG                                                          
         BAS   RE,CLEDT                                                         
         AR    R2,R0                                                            
         LA    R2,1(R2)                                                         
         MVC   0(3,R2),=C'NET'                                                  
         LA    R2,3(R2)                                                         
         MVI   0(R2),EQUALS                                                     
         LA    R2,1(R2)                                                         
         L     R0,TOTN                                                          
         BAS   RE,CLEDT                                                         
CL45     B     CLX                                                              
         SPACE 2                                                                
* ERROR EXIT ROUTINE - DISPLAY KEY OF UNIT IN ERROR                             
*                                                                               
CL50     L     RF,ASPOOLA                                                       
         USING SPOOLD,RF                                                        
         MVC   XTRA,SPACES                                                      
         DROP  RF                                                               
*                                                                               
         MVC   XTRA(2),=C'E='      ESTIMATE FIRST                               
         LA    R2,XTRA+2                                                        
         ZIC   R0,NBACTEST                                                      
         EDIT  (R0),(3,(R2)),ALIGN=LEFT                                         
         AR    R2,R0                                                            
         MVI   0(R2),SLASH                                                      
         LA    R2,1(R2)                                                         
         MVC   0(2,R2),=C'P='      PACKAGE                                      
         LA    R2,2(R2)                                                         
         ZIC   R0,NBPACK                                                        
         EDIT  (R0),(3,(R2)),ALIGN=LEFT                                         
         AR    R2,R0                                                            
         MVI   0(R2),SLASH                                                      
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(L'NBACTPRG,R2),NBACTPRG PROGRAM CODE                           
         LA    R0,L'NBACTPRG       COUNTER                                      
         LA    R3,5(R2)            POINT R3 AT LAST POSITION OF PROG            
         CLI   0(R3),C' '          TEST FOR BLANK CHARACTER                     
         BNE   *+10                                                             
         BCTR  R3,0                BACK UP POINTER                              
         BCT   R0,*-10                                                          
         LA    R2,1(R3)            POINT TO POSITION AFTER LAST CHAR            
*                                                                               
         MVI   0(R2),SLASH                                                      
         LA    R2,1(R2)                                                         
         GOTO1 VDATCON,DMCB,(2,NBACTDAT),(4,(R2))                               
         CLI   NBACTSUB,1          TEST FOR SUB-LINE OF ONE                     
         BE    CL55                YES-ALL DONE                                 
         LA    R2,5(R2)                                                         
         MVI   0(R2),DASH                                                       
         ZIC   R0,NBACTSUB                                                      
         EDIT  (R0),(3,1(R2)),ALIGN=LEFT                                        
*                                                                               
CL55     LA    R2,PAYACTH          SET CURSOR POSITION                          
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE 2                                                                
* MODULE EXIT                                                                   
*                                                                               
CLX      LA    R2,PAYACTH                                                       
         ST    R2,FADDR                                                         
*                                                                               
         TM    APYFLAG,APYYES       CAME FROM AUTOPAY?                          
         BZ    *+8                                                              
         BRAS  RE,UPDAPY            UPDATE AUTOPAY RECORD                       
*                                                                               
CLXX     B     EXXMOD                                                           
         SPACE 2                                                                
* COMMON EDIT                                                                   
*                                                                               
CLEDT    ST    RE,SAVEREG                                                       
         EDIT  (R0),(12,(R2)),2,ALIGN=LEFT,MINUS=YES,ZERO=NOBLANK               
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
*******************************************************************             
*    TSAR CALL FOR INVOICE/EST/PRD TABLE                                        
*******************************************************************             
TSARINIT NTR1                                                                   
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'5D'                                                     
         GOTO1 VCALOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         L     RF,DMCB                                                          
         ST    RF,VTSAR                                                         
*                                                                               
         MVI   T.TSACTN,TSAINI                                                  
         MVC   T.TSACOM,NBACOM       COMFACS                                    
*        MVI   T.TSINDS,TSINODSK     CORE ONLY !                                
         MVI   T.TSINDS,TSIXTTWA                                                
         MVI   T.TSKEYL,X'05'        KEY LENGTH (INX/EST/PRDCODE)               
         LHI   R0,L'TSARREC          RECORD LENGTH                              
         STH   R0,T.TSRECL                                                      
         MVI   T.TSPAGN,4            REQUEST 2 BLOCKS                           
         LA    R1,TSARREC                                                       
         ST    R1,T.TSAREC                                                      
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     EXXMOD                                                           
*                                                                               
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   T.TSERRS,0              SET CC ON EXIT                           
         LR    RE,R0                                                            
         BER   RE                                                               
*                                                                               
* AN ERROR HAS OCCURRED                                                         
*                                                                               
         CLI   T.TSACTN,TSAADD     TEST ACTION = ADD                            
         JNE   CALLTSX                                                          
         TM    T.TSERRS,TSEEOF     TEST EOF                                     
         JZ    CALLTSX                                                          
         DC    H'0'                                                             
*                                                                               
CALLTSX  LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
         DROP  RE                                                               
*                                                                               
*  UPDATE/ADD (INV/EST/PRD/GROSS/NET) RECORD VIA TSAR                           
*                                                                               
CHKTSAR  NTR1                                                                   
         L     R6,NBAIO            BUILD TSAR REC KEY (INV/EST/PRD)             
         USING NURECD,R6           AND CHECK IF IT EXISTS, IF NOT, ADD          
*                                  IT, ELSE ACCUMULATE TOTALS                   
         XC    TSARREC,TSARREC                                                  
         LA    R3,TSARREC                                                       
         USING TSARRD,R3                                                        
*                                                                               
         NI    MYFLAG,X'FF'-CPYSPLIT                                            
*                                                                               
         BAS   RE,GINVIND          GET INVOICE # INDEX ON SCREEN                
*                                                                               
         MVC   TSINDEX,BYTE        INDEX # TO INVOICE IN SCREEN                 
         MVC   TSEST,NUKEST        ESTIMATE                                     
         MVC   TSPRD3,NBPR1CL3     DEFAULT TO FIRST PRODUCT                     
         MVC   TSFLAG,TSARFLAG     FLAGS                                        
         ZAP   TSGROSS,=PL6'0'                                                  
         ZAP   TSNET,=PL6'0'                                                    
*                                                                               
         CLI   AUTOAPP,C'Y'        AUTOAPPROVE                                  
         BE    CHKTS08                                                          
         MVI   TSEST,0             DON'T SPLIT OUT ESTIMATES/PRODUCTS           
         XC    TSPRD3,TSPRD3                                                    
*                                                                               
CHKTS08  CLI   EST,0               ESTIMATE FILTER?                             
         BE    CHKTS10                                                          
         CLI   EST+1,0             IF RANGE, THEN DON'T PASS IT                 
         BNE   CHKTS10                                                          
         MVC   TSEST,NUKEST                                                     
*                                                                               
CHKTS10  MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         OI    MYFLAG,CPYSPLIT                                                  
         MVC   TSPRD3,=C'POL'      IT'S POL                                     
*                                                                               
         L     R6,NBAIO            NORE THAN 2 PRODS?                           
         MVI   ELCODE,X'19'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHKTS12                                                          
         USING NUPDED,R6                                                        
         CLI   NUPDELEN,17         (3+2PRODS X 7)                               
         BNH   *+14                                                             
         OI    MYFLAG,CPYSPLIT                                                  
         MVC   TSPRD3,=C'POL'       IT'S POL                                    
*                                                                               
CHKTS12  MVI   T.TSACTN,TSARDH     CHECK IF RECORD IS ALREADY EXISTS            
         BAS   RE,CALLTSAR                                                      
         TM    T.TSERRS,TSERNF     TEST NOT FOUND                               
         BZ    CHKTS30                                                          
*                                                                               
         L     R6,NBAIO            NOT FOUND SO REBUILD IT                      
         USING NURECD,R6                                                        
*                                                                               
         MVC   TSINDEX,BYTE        INDEX # TO INVOICE IN SCREEN                 
         MVC   TSEST,NUKEST        ESTIMATE                                     
         MVC   TSPRD3,=C'POL'                                                   
         TM    MYFLAG,CPYSPLIT                                                  
         BO    *+10                                                             
         MVC   TSPRD3,NBPR1CL3     DEFAULT TO FIRST PRODUCT                     
         MVC   TSFLAG,TSARFLAG     FLAGS                                        
         ZAP   TSGROSS,=PL6'0'                                                  
         ZAP   TSNET,=PL6'0'                                                    
*                                                                               
         CLI   AUTOAPP,C'Y'        AUTOAPPROVE                                  
         BE    CHKTS20                                                          
         MVI   TSEST,0             DON'T SPLIT OUT ESTIMATES/PRODUCTS           
         XC    TSPRD3,TSPRD3                                                    
*                                                                               
CHKTS20  CLI   EST,0               ESTIMATE FILTER?                             
         BE    CHKTS30                                                          
         CLI   EST+1,0             IF RANGE, THEN DON'T PASS IT                 
         BNE   CHKTS30                                                          
         MVC   TSEST,NUKEST                                                     
*                                                                               
CHKTS30  DS    0H                                                               
         L     R1,PAYGROSS                                                      
         CVD   R1,DUB                                                           
*                                                                               
         TM    TSFLAG,TSFLCK             IF CK, MAKE IT POSITIVE                
         BZ    *+10                      B/C ACC NEGATES IT                     
         MP    DUB,=P'-1'                                                       
         AP    TSGROSS(6),DUB(8)         GROSS AMOUNT                           
*                                                                               
         L     R1,PAYNT                                                         
         CVD   R1,DUB                                                           
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         OI    DUB+7,X'0F'                                                      
*                                                                               
         TM    TSFLAG,TSFLCK             IF CK, MAKE IT POSITIVE                
         BZ    *+10                      B/C ACC NEGATES IT                     
         MP    DUB,=P'-1'                                                       
         AP    TSNET(6),DUB(8)           GROSS AMOUNT                           
*                                                                               
         MVI   T.TSACTN,TSAPUT                                                  
         TM    T.TSERRS,TSERNF     TEST NOT FOUND                               
         BZ    *+8                                                              
         MVI   T.TSACTN,TSAADD     IF NOT FOUND, ADD IT                         
*                                                                               
         BAS   RE,CALLTSAR                                                      
         TM    T.TSERRS,TSEEOF     IF BUFFER FULL                               
         BNZ   TSARFULL                                                         
*                                                                               
CHKTSARX DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* BYTE IS RETURNED WITH INDEX VALUE OF INVOICE # ON SCREEN                      
*                                                                               
GINVIND  NTR1                                                                   
         LA    R3,CLRINV1H                                                      
         MVI   BYTE,X'01'                                                       
         MVI   TSARFLAG,0                                                       
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'18'         UNIT MUST HAVE INVOICE #                    
         BAS   RE,GETEL                                                         
         BNE   GINV30               NO INVOICE - DEFAULT TO FIRST               
         USING NUDTAD,R6                                                        
*                                                                               
         LA    RE,NUDTINVN         TIME INVOICE                                 
         MVC   TINVUNIT,=C'          '                                          
         LA    RF,TINVUNIT                                                      
         LA    R0,10                                                            
*                                                                               
GINV01   CLI   0(RE),C'-'                                                       
         BE    GINV02                                                           
         MVC   0(1,RF),0(RE)                                                    
         AHI   RF,1                                                             
GINV02   AHI   RE,1                                                             
         BCT   R0,GINV01                                                        
*                                                                               
         LA    RE,NUDTIIVN         INT INVOICE                                  
         MVC   IINVUNIT,=C'          '                                          
         LA    RF,IINVUNIT                                                      
         LA    R0,10                                                            
*                                                                               
GINV03   CLI   0(RE),C'-'                                                       
         BE    GINV04                                                           
         MVC   0(1,RF),0(RE)                                                    
         AHI   RF,1                                                             
GINV04   AHI   RE,1                                                             
         BCT   R0,GINV03                                                        
*                                                                               
GINV10   DS    0H                                                               
         LA    RF,CLRCOMNH                                                      
         CR    R3,RF                                                            
         BNH   GINV15               SHOULD'VE QUALIFIED ALREADY SO IT           
         LA    R3,CLRINV1H                                                      
*                                                                               
GINV12   MVI   BYTE,X'01'           DEFAULT TO FIRST INVOICE #                  
         B     GINV30                                                           
*                                                                               
GINV15   DS    0H                                                               
         XC    TMPINV,TMPINV                                                    
         MVC   TMPINV,8(R3)                                                     
         OC    TMPINV,=C'          '                                            
*                                                                               
         LA    RE,TMPINV           SCREEN INVOICE                               
         MVC   INVSCRN,=C'          '                                           
         LA    RF,INVSCRN                                                       
         LA    R0,10                                                            
*                                                                               
GINV16   CLI   0(RE),C'-'                                                       
         BE    GINV17                                                           
         MVC   0(1,RF),0(RE)                                                    
         AHI   RF,1                                                             
GINV17   AHI   RE,1                                                             
         BCT   R0,GINV16                                                        
*                                                                               
         CLI   PAYTYPE,C'Z'        TINT?                                        
         BNE   GINV18                                                           
         CLC   TINVUNIT,INVSCRN    MATCHED ON TIME?                             
         BE    GINV30                                                           
         CLC   IINVUNIT,INVSCRN    MATCHED ON INTEGRATION?                      
         BE    GINV30                                                           
         B     GINV25                                                           
*                                                                               
GINV18   CLI   PAYTYPE,C'I'         INTEGRATION ONLY?                           
         BNE   GINV20                                                           
         CLC   IINVUNIT,INVSCRN     MATCHED ON INTEGRATION?                     
         BNE   GINV25                                                           
         B     GINV30                                                           
*                                                                               
GINV20   CLC   TINVUNIT,INVSCRN     MATCHED ON TIME?                            
         BE    GINV30                                                           
*                                                                               
GINV25   ZIC   RF,0(R3)             NOPE, CHECK NEXT INVOICE INPUT              
         AR    R3,RF                ON SCREEN                                   
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
*                                                                               
         ZIC   RF,BYTE              BUMP INDEX #                                
         AHI   RF,1                                                             
         STC   RF,BYTE                                                          
         B     GINV10                                                           
*                                                                               
GINV30   DS    0H                                                               
         ZIC   RF,0(R3)             BUMP TO AMOUNT FIELD                        
         AR    R3,RF                                                            
         CLI   5(R3),2              DID THEY ENTER CR?                          
         BNH   GINVX                                                            
*                                                                               
         ZIC   RF,5(R3)                                                         
         SHI   RF,2                                                             
         LA    RE,8(R3)                                                         
         AR    RE,RF                                                            
*                                                                               
         CLC   =C'CK',0(RE)                                                     
         BNE   *+8                                                              
         OI    TSARFLAG,TSFLCK                                                  
*                                                                               
         CLC   =C'CR',0(RE)                                                     
         BNE   *+8                                                              
         OI    TSARFLAG,TSFLCR                                                  
*                                                                               
GINVX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
*                                                                               
TINVUNIT DS    CL10                                                             
IINVUNIT DS    CL10                                                             
INVSCRN  DS    CL10                                                             
*                                                                               
* SUB-ROUTINE TO RESTORE NETBLOCK                                               
*                                                                               
RESTBLK  ST    RE,SAVEREG                                                       
         LA    RE,NETBLOCK                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,SVNBLOCK                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE NETBLOCK                                                  
*                                                                               
SAVBLOCK ST    RE,SAVEREG                                                       
         LA    RE,SVNBLOCK                                                      
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,NETBLOCK                                                      
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* CHECK IF SINGLE ESTIMATE IS REQUIRED                                          
* IF SET TO TYPE=STW DONT ALLOW CLEARENCE                                       
*                                                                               
TESTSTW  NTR1                                                                   
         CLI   PAYPROFA+2,C'Y'      IS SINGLE ESTIMATE REQUIRED                 
         BNE   TESTSTWX                                                         
         LA    R2,PAYOPTH                                                       
         ST    R2,FADDR                                                         
*                                                                               
         MVI   FERN,ESTREQER                                                    
         CLI   ESTTYP,ISINGLE                                                   
         BNE   ERROR                                                            
* READ ESTIMATE CHECK IF STW ESTIMATE                                           
         MVI   FERN,ESTERR                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),AGYMED                                                  
         MVC   KEY+2(2),CLIPK                                                   
         MVC   KEY+4(3),=CL3'POL'                                               
         MVC   KEY+7(1),EST                                                     
         GOTO1 AIO,DMCB,SPT+HIGH+DIR                                            
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 AIO,DMCB,SPT+FILE+GET,NBAIO                                      
         L     RE,NBAIO                                                         
         USING ESTHDR,RE                                                        
*                                                                               
         MVI   FERN,ESTTYPER                                                    
         CLI   ETYPE,C'S'           IS ESTIMATE SET UP AS STEWARD               
         BE    ERROR                                                            
*                                                                               
TESTSTWX DS    0H                                                               
         B     EXXMOD                                                           
         DROP  RE                                                               
*                                                                               
* MARK SPECIAL CHARGE ELEMENTS AS PAID                                          
*                                                                               
PAYSPCH  NTR1                                                                   
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   PAYSPCHX                                                         
         B     PSPCH20                                                          
         USING NUSPRD,R6                                                        
*                                                                               
PSPCH10  BAS   RE,NEXTEL                                                        
         BNE   PAYSPCHX                                                         
*                                                                               
PSPCH20  DS    0H                                                               
         CLC   NUSPRTYP,BYTE       SAME PAY TYPE?                               
         BNE   PSPCH10                                                          
         CLI   NUSPRTYP,C'M'       MIDAS                                        
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT GET EHRE                          
*                                                                               
         TM    SPECREP,X'80'       TEST FOR ANY SPEC REP FILTER                 
         BO    *+14                YES                                          
         CLC   SRATREP,NUSPRREP    TEST FOR MATCH ON SPECIAL REP                
         BNE   PSPCH10                                                          
*                                                                               
         CLI   NUSPRLEN,NUSPRLN1   OLD STYLE RATE?                              
         BE    PSPCH50             YES - BUILD NEW RATE ELEMENT                 
*                                                                               
         NI    NUSPRSTA,X'FF'-NUSPRPRE   UNIT MISSED/PREEMPT?                   
         TM    NBUNITST,X'42'      MISSED/PREEMPT?                              
         BZ    *+8                                                              
         OI    NUSPRSTA,NUSPRPRE                                                
*                                                                               
         OI    NUSPRSTA,NUSPRCPD   CHARGE HAS BEEN PAID                         
         B     PSPCH10                                                          
*                                                                               
PSPCH50  DS    0H                  BUILD NEW RATE ELEMENT                       
         XC    OUTMSG,OUTMSG                                                    
         MVC   OUTMSG(NUSPRLN1),0(R6)                                           
*                                                                               
         MVI   OUTMSG+1,NUSPRLN2                                                
         CLI   BYTE,C'U'           CUT-IN?                                      
         BNE   *+8                                                              
         MVI   OUTMSG+1,NUSPRLN3                                                
*                                                                               
         NI    OUTMSG+12,X'FF'-NUSPRPRE   UNIT MISSED/PREEMPT?                  
         TM    NBUNITST,X'42'      MISSED/PREEMPT?                              
         BZ    *+8                                                              
         OI    OUTMSG+12,NUSPRPRE                                               
*                                                                               
         OI    OUTMSG+12,NUSPRCPD   CHARGE HAS BEEN PAID                        
*                                                                               
         MVI   0(R6),X'FF'                                                      
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'FF',NBAIO),0                       
*                                                                               
         LA    R3,OUTMSG                                                        
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),NBAIO,(R3),ADDCODE                    
         CLI   12(R1),0            TEST FOR SUCCESSFUL ADD                      
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
PAYSPCHX DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
*******************************************************************             
* CHECK IF UNIT IS UNDER STW ESTIMATE                                           
*******************************************************************             
CHKESTW  NTR1                                                                   
         L     R2,NBAIO            UNIT RECORD                                  
         L     R5,AESTTAB                                                       
*                                                                               
CHKES10  DS    0H                                                               
         CLI   0(R5),X'FF'         UNIT ESTIMATE MUST BE IN TABLE               
         BNE   CHKES20                                                          
         CLI   2(R5),X'FF'         MAKE SURE IT'S NOT ESTIMATE 255              
         BNE   CHKESKIP                                                         
*                                                                               
CHKES20  CLC   17(1,R2),0(R5)      FOUND ESTIMATE?                              
         BE    *+12                                                             
         LA    R5,2(R5)            NO - BUMP TO NEXT ENTRY                      
         B     CHKES10                                                          
*                                                                               
         TM    1(R5),X'01'         IS THIS ESTIMATE A STW ESTIMATE?             
         BO    CHKESKIP            YES - SKIP THIS UNIT                         
*                                                                               
         SR    R1,R1                                                            
         B     *+8                                                              
CHKESKIP LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
*=========================================================*                     
* MAINTAIN CLEARANCE STATUS RECORD                        *                     
* AND RETURN SEQUENCE NUMBER IN STATSEQ                   *                     
*=========================================================*                     
         SPACE 1                                                                
BLDST    NTR1                                                                   
         TM    TOTAMT,X'F0'           IS IT A CREDIT (-) $?                     
         BNZ   BLDST1                                                           
         CLC   TOTAMT,=XL4'7D2B7500'  TOTAL INVOICES < 21,000,000.00?           
         BL    BLDST1                                                           
*                                                                               
BLDSTERR MVI   FERN,USERERR                                                     
         MVC   PAYMSG(L'CLRERR),CLRERR                                          
         B     ERROR                                                            
*                                                                               
*----->                                                                         
BLDST1   DS    0H                                                               
         CLI   ACTION,TEST         IS THIS A TEST                               
         BE    BLDSTX                                                           
*                                                                               
         CLI   FDCRCKSW,C'Y'       CALLED FROM FINDCRCK JUST GET SEQ#           
         BE    *+12                                                             
*                                                                               
         CLI   PASS,2              CHECK 2ND PASS                               
         BNE   BLDSTX                                                           
*                                                                               
*        CLI   PAYPROF2+2,C'N'     TEST TO GENERATE                             
*        BE    BLDSTX                                                           
*                                                                               
         CLI   STATSEQ,0           CHECK IF SEQUENCE ALREADY SET                
         BNE   BLDSTX                                                           
*----->                                                                         
*                                                                               
         MVI   STATSEQ,0           RESET CURRENT SEQUENCE NUMBER                
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING CLSTEL01,R5                                                      
*                                                                               
         MVI   CLSTEL01,X'01'                                                   
         MVI   CLSTEL01+1,CL01ELN3                                              
         MVC   CLSTCLRD,TODAYC                                                  
         MVC   CLSTPYEE,REP        REP NUMBER                                   
         MVC   CLSTREPT,REPTYPE    SET REP TYPE                                 
         OI    CLSTSTAT,X'02'      NEW STYLE WITH X'03' FOLLOWING               
         MVC   CLSTPID,SVPASSWD    PID                                          
*                                                                               
         MVC   CLSTACC,NBSELAGY      DEFAULT AGENCY                             
         CLC   PAYPROF+14(2),=C'*A'  TEST ALT ACC AGENCY                        
         BNH   BLDST2                                                           
         CLC   PAYPROF+14(2),=C'00'  00 DOESN'T COUNT                           
         BE    *+10                                                             
         MVC   CLSTACC,PAYPROF+14    ACC AGENCY                                 
*                                                                               
BLDST2   MVC   CLSTPRD,PRDN        PRODUCT NUMBERS                              
         MVC   CLSTPRD2,PRDN2                                                   
         MVC   CLST3PR,PROD                                                     
         MVC   CLST3PR2,PROD2                                                   
         GOTO1 VDATCON,DMCB,(0,START),(2,CLSTSTDT)                              
         GOTO1 VDATCON,DMCB,(0,END),(2,CLSTNDDT)                                
         CLI   PAYPROF+0,C'G'      TEST PAYING GROSS                            
         BNE   BLDST4                                                           
         MVC   CLSTGRS,TOTG                                                     
         B     BLDST10                                                          
*                                                                               
BLDST4   MVC   CLSTNET,TOTN                                                     
         DROP  R5                                                               
*                                                                               
BLDST10  LA    R5,KEY                                                           
         USING CLRSTATD,R5                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   CLSKTYPE,=X'0D76'                                                
         MVC   CLSKAGMD,AGYMED     A-M                                          
         MVC   CLSKCLT,CLIPK       CLT                                          
         MVC   DUB(4),NETWORK                                                   
         MVI   DUB+4,C'N'                                                       
         PRINT GEN                                                              
         GOTO1 VMSPACK,DMCB,MARKET,DUB,CLSKMKT                                  
         PRINT NOGEN                                                            
         DROP  R5                                                               
*                                                                               
         GOTO1 AIO,DMCB,SPT+HIGH+DIR                                            
         CLC   KEY(10),KEYSAVE     SAME TYPE/CLT/MKT/STAT                       
         BE    BLDST20             YES - UPDATE LAST RECORD                     
*                                                                               
BLDST12  L     R5,AIOAREA2                                                      
         XC    0(256,R5),0(R5)                                                  
         USING CLRSTATD,R5                                                      
*                                                                               
         MVC   CLSKEY,KEYSAVE      USE THE KEY FROM READ HIGH                   
         XC    KEYSAVE,KEYSAVE     THEN CLEAR SO WILL ADD LATER                 
         MVC   13(2,R5),=H'24'     SET REC LEN WITH NO ELEMENTS                 
         MVC   CLSAGYA,AGENCY                                                   
         LA    R5,24(R5)           POINT TO FIRST ELEMENT POSITION              
         ST    R5,FULL             SET FOR NEXT INST                            
         B     BLDST24                                                          
         DROP  R5                                                               
*                                                                               
BLDST20  MVC   KEYSAVE,KEY         SAVE PREVIOUS KEY                            
         GOTO1 AIO,DMCB,SPT+SEQ+DIR                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    BLDST20                                                          
* UPDATE LAST RECORD FOR THIS STATION                                           
         MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
         MVC   NDXDA,KEY+14        RESTORE LAST DISK ADDRESS                    
         L     R5,AIOAREA2                                                      
         GOTO1 AIO,DMCB,SPT+GET+FILE+UPDATE,AIOAREA2                            
*                                                                               
         LA    R5,CLSELEMS-CLRSTATD(R5)  POINT TO FIRST ELEMENT                 
         USING CLSTEL01,R5                                                      
         ST    R5,FULL             STORE JUST IN CASE                           
         B     BLDST23                                                          
*                                                                               
BLDST22  SR    R0,R0                                                            
         ICM   R0,1,1(R5)          FIND LAST 01 ELEMENT IN RECORD               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R5,R0                                                            
BLDST23  DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    BLDST24                                                          
*                                                                               
         CLI   0(R5),X'03'         X'03'/X'05' PAIRINGS?                        
         BNE   *+12                                                             
         ST    R5,FULL             SAVE ELEMENT ADDRESS                         
         B     BLDST22                                                          
*                                                                               
         CLI   0(R5),X'05'                                                      
         BNE   *+12                                                             
         ST    R5,FULL             SAVE ELEMENT ADDRESS                         
         B     BLDST22                                                          
*                                                                               
         CLI   0(R5),X'01'                                                      
         BNE   BLDST22                                                          
         ST    R5,FULL             SAVE ELEMENT ADDRESS                         
         ST    R5,FULL2            A (LAST X'01' ELEM)                          
         B     BLDST22                                                          
*                                                                               
BLDST24  L     R5,FULL2            LAST X'01' ELEMENT                           
         SR    RE,RE                                                            
         CLC   CLSTCLRD,TODAYC     TEST TODAY                                   
         BNE   BLDST25             NOT TODAY'S DATE SO RE-START SEQ #           
*                                                                               
         IC    RE,CLSTCLSQ         GET SEQUENCE NUMBER                          
         CLI   CLSTCLSQ,X'FF'      TEST REACHED LIMIT                           
         BE    MAXERR                                                           
*                                                                               
BLDST25  L     R5,FULL             LAST ELEM SLOT                               
         LA    RE,1(RE)            BUMP SEQUENCE                                
         STC   RE,STATSEQ          SET FOR USER                                 
         STC   RE,WORK+CLSTCLSQ-CLSTEL01   AND SET IN NEW ELEMENT               
         ZIC   R0,1(R5)                                                         
         AR    R5,R0               BUMP PAST LAST ELEMENT                       
*                                                                               
         CLI   AUTOAPP,C'Y'        DON'T UPDATE CLRST, JUST GET SEQ#            
         BE    BLDSTX              WE ADD CLRST LATER IN BLD01                  
*                                                                               
         CLI   FDCRCKSW,C'Y'       CALLED FROM FINDCRCK JUST GET SEQ#           
         BE    BLDSTX                                                           
*                                                                               
BLDST26  DS    0H                                                               
         L     RF,AIOAREA2                                                      
         CLC   13(2,RF),=X'1388'   IS RECORD ALREADY 5000 BYTES?                
         BNL   BLDST27             WILL NOT FIT ON RECORD - ADD NEW             
*                                                                               
         GOTO1 =V(RECUP),DMCB,AIOAREA2,WORK,(C'R',(R5)),RR=MYRELO               
         CLI   8(R1),C'R'                                                       
         BE    BLDST30                                                          
*                                                                               
BLDST27  L     R5,AIOAREA2                                                      
         XC    13(256,R5),13(R5)   CLEAR PART OF IT (LEAVE KEY)                 
         MVC   10(2,R5),TODAYC     SET KEY FOR NEW RECORD                       
         MVC   12(1,R5),STATSEQ                                                 
         MVC   13(2,R5),=H'24'     SET LENGTH WITH NO ELEMENTS                  
         LA    R5,24(R5)           SET TO ADD FIRST ELEMENT                     
         B     BLDST26             ADD THE ELEMENT                              
*                                                                               
BLDST30  ZIC   RF,1(R5)            BUMP TO NEXT AVAILABLE ENTRY                 
         AR    R5,RF               IN CLRST REC FOR RECUP                       
         LA    R2,CLRINV1H                                                      
         LA    R3,AMTS                                                          
*                                                                               
BLDST40  LA    RF,CLRINVNH                                                      
         CR    R2,RF                                                            
         BH    BLDST90                                                          
*                                                                               
         CLI   8(R2),0             ANY INVOICE?                                 
         BE    BLDST50                                                          
*                                                                               
         XC    TMPINV,TMPINV                                                    
         MVC   TMPINV,8(R2)                                                     
         OC    TMPINV,=C'          '                                            
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING CLSTEL03,R6                                                      
*                                                                               
         MVI   CLSTEL03,X'03'                                                   
         MVI   CLSTLN03,CL03ELLN                                                
         MVC   CLS3INV(10),TMPINV      MOVE IN INVOICE                          
         DROP  R6                                                               
*                                                                               
         GOTO1 =V(RECUP),DMCB,AIOAREA2,WORK,(C'R',(R5)),RR=MYRELO               
*                                                                               
         ZIC   RF,1(R5)            BUMP TO NEXT AVAILABLE ENTRY                 
         AR    R5,RF               IN CLRST RECORD FOR RECUP                    
*                                                                               
         XC    WORK,WORK                                                        
BLDST40B LA    R6,WORK                                                          
         USING CLSTEL05,R6                                                      
         MVI   CLSTEL05,X'05'                                                   
         MVI   CLSTLN05,CL05ELN2                                                
         MVC   CLS5PRD1,PROD       ALPHA PRODUCT 1 (IF IN OPTIONS)              
         MVC   CLS5PRD2,PROD2      ALPHA PRODUCT 2 (IF IN OPTIONS)              
         MVC   CLS5EST,EST         BIN ESTIMATE (IF IN OPTIONS)                 
*                                                                               
         XC    CLS5GRS(8),CLS5GRS   CLEARS CLS5GRS + CLS5NET                    
*                                                                               
         CLI   4(R3),C'2'          CR?                                          
         BNE   *+8                                                              
         OI    CLS5STAT,CLS5STAT_CR                                             
         CLI   4(R3),C'3'          CK?                                          
         BNE   *+8                                                              
         OI    CLS5STAT,CLS5STAT_CK                                             
*                                                                               
*                                                                               
BLDST40I CLI   PAYPROF+0,C'G'      PAY GROSS?                                   
         BNE   BLDST42                                                          
         MVC   CLS5GRS,0(R3)       MOVE INTO GROSS AMT                          
         OC    CRCKDAT,CRCKDAT     REVERSE OPTION?                              
         BNZ   BLDST40K                                                         
*                                                                               
BLDST40J CLI   4(R3),C'1'          CK OR CR?                                    
         BE    BLDST45                                                          
         ICM   RF,15,CLS5GRS       ONLY MAKE SURE IT'S NEGATIVE FOR CR          
         LNR   RF,RF                                                            
         STCM  RF,15,CLS5GRS                                                    
*                                                                               
BLDST40K ICM   RF,15,CLS5GRS                                                    
         M     RE,TOTN                                                          
         D     RE,TOTG                                                          
         STCM  RF,15,CLS5NET                                                    
         B     BLDST45                                                          
*                                                                               
BLDST42  DS    0H                                                               
         MVC   CLS5NET,0(R3)       MOVE INTO NET AMT                            
         OC    CRCKDAT,CRCKDAT         REVERSEY OPTION ?                        
         BNZ   BLDST45                                                          
*                                                                               
BLDST43  CLI   4(R3),C'1'          CK OR CR?                                    
         BE    BLDST45                                                          
         ICM   RF,15,CLS5NET       ONLY MAKE SURE IT'S NEGATIVE FOR CR          
         LNR   RF,RF                                                            
         STCM  RF,15,CLS5NET                                                    
*                                                                               
BLDST45  OC    CRCKDAT,CRCKDAT       IF REVERSE                                 
         BZ    *+8                   NO                                         
         OI    CLS5STAT,CLS5STAT_RV    SET REVERSE BIT                          
         GOTO1 =V(RECUP),DMCB,AIOAREA2,WORK,(C'R',(R5)),RR=MYRELO               
         ZIC   RF,1(R5)            BUMP TO NEXT AVAILABLE ENTRY                 
         AR    R5,RF               IN CLRST RECORD FOR RECUP                    
*                                                                               
         OC    CRCKDAT,CRCKDAT       IF REVERSE                                 
         BZ    BLDST50               NO                                         
         NI    CLS5STAT,X'FF'-CLS5STAT_RV   RESET REVERSE BIT                   
*                                                                               
         ICM   RF,15,CLS5NET       MAKE CR/CK NEGATIVE                          
         LNR   RF,RF                                                            
         STCM  RF,15,CLS5NET                                                    
         ICM   RF,15,CLS5GRS                                                    
         LNR   RF,RF                                                            
         STCM  RF,15,CLS5GRS                                                    
         B     BLDST45A                                                         
*                                                                               
BLDST45A TM    CLS5STAT,CLS5STAT_CR        IF CR                                
         BNO   *+16                                                             
         NI    CLS5STAT,X'FF'-CLS5STAT_CR                                       
         OI    CLS5STAT,CLS5STAT_CK         MAKE IT CK                          
         B     BLDST45E                                                         
         TM    CLS5STAT,CLS5STAT_CK      ELSE MUST BE CK                        
         BO    *+6                                                              
         DC    H'0'                      ???                                    
         NI    CLS5STAT,X'FF'-CLS5STAT_CK                                       
         OI    CLS5STAT,CLS5STAT_CR         MAKE IT CR                          
BLDST45E GOTO1 =V(RECUP),DMCB,AIOAREA2,WORK,(C'R',(R5)),RR=MYRELO               
         ZIC   RF,1(R5)            BUMP TO NEXT AVAILABLE ENTRY                 
         AR    R5,RF               IN CLRST RECORD FOR RECUP                    
         DROP  R6                                                               
*                                                                               
BLDST50  ZIC   RF,0(R2)             NOPE, CHECK NEXT INVOICE INPUT              
         AR    R2,RF                ON SCREEN                                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         AHI   R3,8                 BUMP TO NEXT ENTRY IN AMT TABLE             
         B     BLDST40                                                          
*                                                                               
BLDST90  L     R5,AIOAREA2                                                      
         BRAS  RE,VALCLRST         MAKE SURE 01/03/05 SEQ IS CORRECT            
         CLC   0(13,R5),KEYSAVE    WRITING SAME RECORD READ                     
         BE    BLDST100            YES                                          
         GOTO1 AIO,DMCB,SPT+ADDREC+FILE,AIOAREA2                                
         B     BLDSTX                                                           
*                                                                               
BLDST100 DS    0H                                                               
         GOTO1 AIO,DMCB,SPT+PUT+FILE,AIOAREA2                                   
*                                                                               
BLDSTX   MVC   SVSEQNUM,STATSEQ    SEQUENCE NUMBER                              
         B     EXXMOD                                                           
         EJECT                                                                  
*=========================================================*                     
* BUILD X'01' ELEMENT FOR CLRST RECORD (IN AIOAREA4)      *                     
* AND RETURN SEQUENCE NUMBER IN STATSEQ                   *                     
*=========================================================*                     
BLD01    NTR1                                                                   
         NI    MYFLAG,X'FF'-ADDCLR                                              
*                                                                               
         TM    TOTAMT,X'F0'           IS IT A CREDIT (-) $?                     
         BNZ   BLD0110                                                          
         CLC   TOTAMT,=XL4'7D2B7500'  TOTAL INVOICES < 21,000,000.00?           
         BL    BLD0110                                                          
*                                                                               
         MVI   FERN,USERERR                                                     
         MVC   PAYMSG(L'CLRERR),CLRERR                                          
         B     ERROR                                                            
*                                                                               
BLD0110  DS    0H                                                               
         CLI   ACTION,TEST         IS THIS A TEST                               
         BE    BLD01X                                                           
*                                                                               
         CLI   PASS,2              CHECK 2ND PASS                               
         BNE   BLD01X                                                           
*                                                                               
         L     RE,AIOAREA4                                                      
         L     RF,=F'2000'                                                      
         XCEF                                                                   
*                                                                               
         MVI   STATSEQ,0           RESET CURRENT SEQUENCE NUMBER                
         L     R5,AIOAREA4                                                      
         MVC   13(2,R5),=H'64'     DUMMY KEY WITH ONE X'01' ELEM                
*                                                                               
         LA    R5,CLSELEMS-CLRSTATD(R5)  POINT TO FIRST ELEMENT                 
         USING CLSTEL01,R5                                                      
*                                                                               
         MVI   CLSTEL01,X'01'                                                   
         MVI   CLSTEL01+1,CL01ELN3                                              
         MVC   CLSTCLRD,TODAYC                                                  
         MVC   CLSTPYEE,REP        REP NUMBER                                   
         MVC   CLSTREPT,REPTYPE    SET REP TYPE                                 
         OI    CLSTSTAT,X'02'      NEW CLRST (03 ELEMS FOLLOW)                  
         MVC   CLSTPID,SVPASSWD    PID                                          
*                                                                               
         MVC   CLSTACC,NBSELAGY      DEFAULT AGENCY                             
         CLC   PAYPROF+14(2),=C'*A'  TEST ALT ACC AGENCY                        
         BNH   BLD0115                                                          
         CLC   PAYPROF+14(2),=C'00'  00 DOESN'T COUNT                           
         BE    *+10                                                             
         MVC   CLSTACC,PAYPROF+14  ACC AGENCY                                   
*                                                                               
BLD0115  MVC   CLSTPRD,PRDN        PRODUCT NUMBERS                              
         MVC   CLSTPRD2,PRDN2                                                   
         MVC   CLST3PR,PROD                                                     
         MVC   CLST3PR2,PROD2                                                   
         GOTO1 VDATCON,DMCB,(0,START),(2,CLSTSTDT)                              
         GOTO1 VDATCON,DMCB,(0,END),(2,CLSTNDDT)                                
*                                                                               
         CLI   PAYPROF+0,C'G'      TEST PAYING GROSS                            
         BNE   *+14                                                             
         MVC   CLSTGRS,TOTG                                                     
         B     *+10                                                             
         MVC   CLSTNET,TOTN                                                     
         DROP  R5                                                               
*                                                                               
         LA    R5,KEY                                                           
         USING CLRSTATD,R5                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   CLSKTYPE,=X'0D76'                                                
         MVC   CLSKAGMD,AGYMED     A-M                                          
         MVC   CLSKCLT,CLIPK       CLT                                          
         MVC   DUB(4),NETWORK                                                   
         MVI   DUB+4,C'N'                                                       
         PRINT GEN                                                              
         GOTO1 VMSPACK,DMCB,MARKET,DUB,CLSKMKT                                  
         PRINT NOGEN                                                            
         DROP  R5                                                               
*                                                                               
         GOTO1 AIO,DMCB,SPT+HIGH+DIR                                            
         CLC   KEY(10),KEYSAVE     SAME TYPE/CLT/MKT/STAT                       
         BE    BLD0120             YES - UPDATE LAST RECORD                     
*                                                                               
         L     R5,AIOAREA2                                                      
         XC    0(256,R5),0(R5)                                                  
         USING CLRSTATD,R5                                                      
         OI    MYFLAG,ADDCLR                                                    
*                                                                               
         MVC   CLSKEY,KEYSAVE      USE THE KEY FROM READ HIGH                   
         XC    KEYSAVE,KEYSAVE     THEN CLEAR SO WILL ADD LATER                 
         MVC   13(2,R5),=H'24'     SET REC LEN WITH NO ELEMENTS                 
         MVC   CLSAGYA,AGENCY                                                   
         AHI   R5,24               POINT TO FIRST ELEMENT POSITION              
         ST    R5,FULL             SET FOR NEXT INST                            
         B     BLD0124                                                          
         DROP  R5                                                               
*                                                                               
BLD0120  MVC   KEYSAVE,KEY         SAVE PREVIOUS KEY                            
         GOTO1 AIO,DMCB,SPT+SEQ+DIR                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    BLD0120                                                          
* UPDATE LAST RECORD FOR THIS STATION                                           
         MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
         MVC   NDXDA,KEY+14        RESTORE LAST DISK ADDRESS                    
         L     R5,AIOAREA2                                                      
         GOTO1 AIO,DMCB,SPT+GET+FILE+UPDATE,AIOAREA2                            
*                                                                               
         LA    R5,CLSELEMS-CLRSTATD(R5)  POINT TO FIRST ELEMENT                 
         USING CLSTEL01,R5                                                      
         ST    R5,FULL             STORE JUST IN CASE                           
         B     BLD0123                                                          
*                                                                               
BLD0122  SR    R0,R0                                                            
         ICM   R0,1,1(R5)          FIND LAST 01 ELEMENT IN RECORD               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R5,R0                                                            
BLD0123  DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    BLD0124                                                          
*                                                                               
         CLI   0(R5),X'03'         X'03'/X'05' PAIRINGS?                        
         BNE   *+12                                                             
         ST    R5,FULL             SAVE ELEMENT ADDRESS                         
         B     BLD0122                                                          
*                                                                               
         CLI   0(R5),X'05'                                                      
         BNE   *+12                                                             
         ST    R5,FULL             SAVE ELEMENT ADDRESS                         
         B     BLD0122                                                          
*                                                                               
         CLI   0(R5),X'01'                                                      
         BNE   BLD0122                                                          
         ST    R5,FULL             SAVE ELEMENT ADDRESS                         
         ST    R5,FULL2            A (LAST X'01' ELEM)                          
         B     BLD0122                                                          
*                                                                               
BLD0124  L     R5,FULL2                                                         
         SR    RE,RE                                                            
         CLC   CLSTCLRD,TODAYC     TEST TODAY                                   
         BNE   BLD0125             NOT TODAY'S DATE SO RE-START SEQ #           
*                                                                               
         IC    RE,CLSTCLSQ         GET SEQUENCE NUMBER                          
         CLI   CLSTCLSQ,X'FF'      TEST REACHED LIMIT                           
         BE    MAXERR                                                           
*                                                                               
BLD0125  LA    RE,1(RE)            BUMP SEQUENCE                                
         STC   RE,STATSEQ          SET FOR USER                                 
*                                                                               
         L     R5,AIOAREA4                                                      
         LA    R5,24(R5)  POINT TO FIRST ELEMENT                                
         USING CLSTEL01,R5                                                      
         MVC   CLSTCLSQ,STATSEQ   AND SET IN NEW ELEMENT                        
         DROP  R5                                                               
*                                                                               
BLD01X   DS    0H                                                               
         MVC   SVSEQNUM,STATSEQ    SEQUENCE NUMBER                              
*                                                                               
         CLI   ACTION,TEST         IS THIS A TEST                               
         BE    EXXMOD                                                           
         CLI   PASS,2              CHECK 2ND PASS                               
         BNE   EXXMOD                                                           
         CLI   STATSEQ,0                                                        
         BNE   EXXMOD                                                           
         DC    H'00'                                                            
*=========================================================*                     
* UPDATE/ADD CLRST RECORDS WITH NEW X'01-03-05' PAIRINGS  *                     
* AIOAREA2 HAS THE CLRST RECORD                           *                     
* AIOAREA4 HAS THE ELEMENT PAIRINGS                       *                     
*=========================================================*                     
ADDCLRST NTR1                                                                   
         L     R6,AIOAREA2          CLRST RECORD                                
         L     R5,AIOAREA4          ELEMENT PAIRINGS                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,13(R5)          GET NEW ELEMENTS TOTAL LENGTH               
         SH    RE,=H'24'            MINUS THE KEY                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,13(R6)          GET RECORD LENGTH                           
         AR    RF,RE                GET NEW LENGTH                              
         C     RF,=F'5000'                                                      
         BNL   ACLR10               WILL NOT FIT ON RECORD - ADD NEW            
*                                                                               
         L     R6,AIOAREA2                                                      
         AHI   R6,24                                                            
ACLR5    CLI   0(R6),0             BUMP TO END OF RECORD                        
         BE    ACLR20                                                           
         CLI   0(R6),X'F1'         BUMP TO END OF RECORD                        
         BE    ACLR20                                                           
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     ACLR5                                                            
*                                                                               
ACLR10   L     R6,AIOAREA2                                                      
         XC    13(256,R6),13(R6)   CLEAR PART OF IT (LEAVE KEY)                 
         MVC   10(2,R6),TODAYC     SET KEY FOR NEW RECORD                       
         MVC   12(1,R6),SVSEQNUM                                                
         MVC   13(2,R6),=H'24'     SET LENGTH WITH NO ELEMENTS                  
         LA    R6,24(R6)           BUMP TO FIRST ELEMENT SPOT                   
         OI    MYFLAG,ADDCLR                                                    
*                                                                               
ACLR20   DS    0H                                                               
         L     R5,AIOAREA4                                                      
         LA    R5,24(R5)                                                        
*                                                                               
ACLR30   DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    ACLR40                                                           
*                                                                               
         GOTO1 =V(RECUP),DMCB,AIOAREA2,0(R5),(C'R',(R6)),RR=MYRELO              
         CLI   8(R1),C'R'                                                       
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     ACLR30                                                           
*                                                                               
ACLR40   DS    0H                                                               
         L     R6,AIOAREA2                                                      
         SR    RF,RF                                                            
         ICM   RF,3,13(R6)          GET RECORD LENGTH                           
         AHI   RF,1                 GET NEW LENGTH                              
         STCM  RF,3,13(R6)          ADD ONE BYTE FOR EOR X'00'                  
*                                                                               
         BRAS  RE,VALCLRST         MAKE SURE 01/03/05 SEQ IS CORRECT            
         TM    MYFLAG,ADDCLR                                                    
         BZ    ACLR45                                                           
         GOTO1 AIO,DMCB,SPT+ADDREC+FILE,AIOAREA2                                
         B     ACLRX                                                            
*                                                                               
ACLR45   DS    0H                                                               
         GOTO1 AIO,DMCB,SPT+PUT+FILE,AIOAREA2                                   
*                                                                               
ACLRX    B     EXXMOD                                                           
*=========================================================*                     
MAXERR   DS    0H                                                               
         ST    R2,FADDR                                                         
         MVI   FERN,USERERR                                                     
         MVC   PAYMSG(L'BSTERR),BSTERR                                          
         B     ERROR                                                            
*                                                                               
TSARFULL DS    0H                                                               
         ST    R2,FADDR                                                         
         MVI   FERN,USERERR                                                     
         MVC   PAYMSG(L'TSARERR),TSARERR                                        
         B     ERROR                                                            
*                                                                               
CLRERR   DC    C'** ERROR - TOTAL INV AMOUNTS EXCEED 21,000,000'                
BSTERR   DC    C'MAX # OF CLEARANCES FOR CLT/NET REACHED TODAY '                
TSARERR  DC    C'MAX # REQUESTS GENERATED-SPECIFY ADDTL FILTERS'                
*NOINVQ   DC    C'INVOICE MISSING IN REQUEST -                  '               
*                                                                               
* SUB-ROUTINE TO GENERATE CLEARANCE REQUESTS ACCORDING TO SCREEN                
* INPUT                                                                         
*                                                                               
CHK      NTR1                                                                   
         L     R3,ASPOOLA                                                       
         USING SPOOLD,R3                                                        
*                                                                               
*  IF REVERSE GET TOTAL VALUES FROM SUM OF CLRST RECORDS                        
*  THESE VALUES ARE SET IN ROUTINE FINDCRCK                                     
         OC    CRCKDAT,CRCKDAT     REVERSE OPTION                               
         BZ    CHK2                GO TO REGULAR MAINT                          
         MVC   TOTG,REVGROSS                                                    
         MVC   TOTN,REVNET                                                      
         SPACE 1                                                                
* NET DOWN GROSS AMOUNTS SINCE PAY NET TO STATION                               
*                                                                               
CHK2     CLI   PAYPROF+0,C'G'      TEST FOR GROSS OPTION                        
         BNE   CHK10               NO                                           
         OC    TOTG,TOTG           TEST FOR ZERO GROSS                          
         BZ    CHK10               YES-NOTHING TO NET DOWN                      
*                                                                               
         LA    R0,CLRLINES                                                      
         LA    R6,AMTS                                                          
CHK5     L     R5,0(R6)                                                         
         M     R4,TOTN                                                          
         D     R4,TOTG                                                          
         ST    R5,0(R6)                                                         
         AHI   R6,8                                                             
         BCT   R0,CHK5                                                          
         SPACE 1                                                                
* SUM NET INVOICES                                                              
*                                                                               
CHK6     LA    R0,CLRLINES                                                      
         LA    R6,AMTS                                                          
         SR    RE,RE                                                            
CHK7     L     RF,0(R6)                                                         
         CLI   4(R6),C'1'                                                       
         BE    *+6                                                              
         LCR   RF,RF                                                            
         AR    RE,RF                                                            
         AHI   R6,8                                                             
         BCT   R0,CHK7                                                          
         ST    RE,TOTAMT                                                        
         SPACE 2                                                                
* TEST FOR DIFFERENCE BETWEEN TOTAMT AND TOTN AND ADJUST FIRST NON-CK           
* AMOUNT BY DIFFERENCE                                                          
*                                                                               
CHK10    L     RE,TOTN                                                          
         S     RE,TOTAMT                                                        
         BZ    CHK20               NO DIFFERENCE                                
*                                                                               
         LA    R0,CLRLINES                                                      
         LA    R6,AMTS                                                          
CHK12    CLI   4(R6),C'1'          TEST FOR REGULAR AMOUNT                      
         BE    CHK14               YES                                          
         CLI   4(R6),C'2'          TEST FOR CR                                  
         BE    CHK16                                                            
CHK13    AHI   R6,8                                                             
         BCT   R0,CHK12                                                         
         B     CHK20                                                            
*                                                                               
CHK14    A     RE,0(R6)                                                         
         BZ    CHK13               DO NOT SET AMOUNT = 0                        
         ST    RE,0(R6)                                                         
         B     CHK20                                                            
*                                                                               
CHK16    S     RE,0(R6)            ADJUST CR AMT BY SUBTRACTION                 
         BZ    CHK13               DO NOT SET AMOUNT = 0                        
         LCR   RE,RE               RESTORE PROPER SIGN                          
         ST    RE,0(R6)                                                         
         SPACE 2                                                                
* GENERATE THE CLEARANCE REQUESTS FOR THE INVOICES                              
*                                                                               
CHK20    LA    R2,CLRINV1H         R2 = INVOICE                                 
         LA    R6,AMTS                                                          
*!!!     OC    0(4,R6),0(R6)       FIRST AMOUNT FIELD = 0                       
*!!!     BZ    CHKX                YES - EXIT                                   
         SPACE 1                                                                
CHK22    DS    0H                                                               
         XC    SVAMTG,SVAMTG                                                    
         XC    SVAMTN,SVAMTN                                                    
         MVC   SVAMTG,0(R6)        DEFAULT AMOUNT                               
         MVC   SVAMTN,0(R6)        INVOICE NET AMOUNT                           
*                                                                               
         NI    MYFLAG,X'FF'-GRSCALC                                             
*                                                                               
*!!!     CLI   PRPAY1,C'N'                                                      
*!!!     BNE   *+8                                                              
         BAS   RE,CALCGRS          CALCULATE GROSS AMOUNT FROM NET              
*                                                                               
         CLI   4(R6),C'1'                                                       
         BE    CHK22B                                                           
*                                                                               
         L     RF,SVAMTG           THIS IS A CK, SO IT MUST BE -                
         LNR   RF,RF                                                            
         ST    RF,SVAMTG                                                        
*                                                                               
         L     RF,SVAMTN                                                        
         LNR   RF,RF                                                            
         ST    RF,SVAMTN                                                        
*                                                                               
CHK22B   DS    0H                                                               
         CLI   AUTOAPP,C'Y'                                                     
         BE    *+8                                                              
         BAS   RE,BLDREQ                                                        
*                                                                               
         XC    SVAGYCOD,SVAGYCOD                                                
         CLC   PAYPROF+14(2),=C'*A'  TEST ALT ACC AGENCY                        
         BNH   CHK23                                                            
         CLC   PAYPROF+14(2),=C'00'  00 DOESN'T COUNT                           
         BE    CHK23                                                            
         MVC   ZAGYCODE,PAYPROF+14                                              
         MVC   SVAGYCOD,ZAGYCODE                                                
*                                                                               
CHK23    L     R1,ACLIREC                                                       
         USING CLTHDRD,R1                                                       
         MVC   ZOFFC,COFFICE       EXTRACT OFFICE CODE                          
         MVI   ZOFFC+1,C' '        BLANK OUT SECOND BYTE OF OFFICE              
         CLI   SPOFFSW,YES         TEST SPECIAL OFFC ACTIVE                     
         BNE   *+10                                                             
         MVC   ZOFFC,PAYPROF+4     YES-SET SPECIAL OFFICE CODE                  
         MVI   ZOFFC+1,C' '        BLANK OUT SECOND BYTE OF OFFICE              
         CLI   CACCOFC,X'40'       TEST FOR ACC OFFICE                          
         BNH   *+10                                                             
         MVC   ZOFFC,CACCOFC       MOVE ACC OFFICE OUT                          
         DROP  R1                                                               
*                                                                               
         MVC   SVOFFC,ZOFFC                                                     
*                                                                               
         L     R0,0(R6)                                                         
         CLI   4(R6),C'2'          TEST CR                                      
         BNE   *+6                                                              
         LCR   R0,R0                                                            
         CVD   R0,DUB                                                           
         LTR   R0,R0                                                            
         BNP   *+8                                                              
         OI    DUB+7,X'0F'                                                      
*                                                                               
         ZAP   Z2AMT,DUB           NEW FIELD FOR > $9,999,999                   
*                                                                               
         MVC   ZAMTTYPE,4(R6)                                                   
         GOTO1 VGETFLD,(R2)                                                     
         MVC   ZINV(10),FLD        EXTRACT INVOICE                              
         OC    ZINV(10),=C'          '                                          
*                                                                               
         MVC   ZSEQNUM,STATSEQ     SEQUENCE NUMBER                              
         SPACE 2                                                                
* COMMENT HANDLING LOGIC                                                        
*                                                                               
CHK25    TM    7(R6),X'20'         TEST COMMENT THIS LINE                       
         BO    CHK26               YES                                          
*                                                                               
         MVI   ZCONT,0             INIT ZCONT                                   
*                                                                               
         AHI   R6,8                POINT TO NEXT LINE ENTRY                     
         CLI   7(R6),0             TEST FOR END OF SCREEN                       
         BE    CHKX                YES-ALL DONE                                 
         LA    R2,CLRLEN(R2)       NEXT LINE, INVOICE FIELD                     
         B     CHK22                                                            
         SPACE 1                                                                
* COMMENT ON THIS LINE                                                          
*                                                                               
CHK26    SR    R4,R4                                                            
         LA    R5,ZCOMMENT                                                      
CHK27    LA    R2,CLRCOMH(R2)      POINT TO COMMENT FIELD                       
         GOTO1 VGETFLD,(R2)                                                     
         MVC   0(40,R5),FLD        EXTRACT COMMENT                              
         LA    R4,1(R4)            BUMP COUNT                                   
         STCM  R4,1,ZCONT          SAVE COUNT                                   
         LA    R5,40(R5)           BUMP POSITION                                
*                                                                               
         LA    R6,8(R6)            NEXT AMOUNT                                  
         CLI   7(R6),0             TEST LAST INPUT FIELD                        
         BNE   CHK28               NO                                           
         B     CHKX                AND EXIT                                     
         SPACE 1                                                                
CHK28    ZIC   R0,0(R2)                                                         
         AR    R2,R0               NEXT INV                                     
         CLI   7(R6),X'20'         NEXT LINE CMT ONLY                           
         BE    CHK27               YES                                          
         B     CHK22               NO-GO BACK AND PROCESS NEXT AMOUNT           
*                                                                               
CHKX     DS    0H                                                               
         CLI   AUTOAPP,C'Y'        AUTOAPPROVE?                                 
         BE    CHKX05                                                           
         BAS   RE,ADDREQ                                                        
         B     CHKEXIT                                                          
*                                                                               
CHKX05   BAS   RE,BLD01            BUILD X'01' ELEMENT FOR CLRST                
*                                                                               
         XC    TSARREC,TSARREC                                                  
         MVI   T.TSACTN,TSARDH     GET RECORDS AND BLD REQ FOR ACC              
         B     CHKX20                                                           
*                                                                               
CHKX10   MVI   T.TSACTN,TSANXT     READ NEXT                                    
*                                                                               
CHKX20   BAS   RE,CALLTSAR                                                      
         TM    T.TSERRS,TSEEOF     END OF FILE?                                 
         BO    CHKX30                                                           
*                                                                               
         BAS   RE,BLDTSREQ                                                      
         BAS   RE,ADDREQ                                                        
*                                                                               
CHKX25   BRAS  RE,BLDCLELS         ADD X'03' AND X'05' ELEMS (CLRST)            
         B     CHKX10                                                           
*                                                                               
CHKX30   BAS   RE,ADDCLRST         ADD THE CLRST RECORD                         
*                                                                               
CHKEXIT  B     EXXMOD                                                           
         EJECT                                                                  
         DROP  R3                                                               
*******************************************************************             
*  CALCULATE GROSS AMOUNT FROM NET                                              
*******************************************************************             
CALCGRS  NTR1                                                                   
         L     R1,SVAMTN                                                        
         CVD   R1,DUB                                                           
         ZAP   SVGROSSP,DUB                                                     
         MP    SVGROSSP,=P'100'     GROSS = NET / 0.85                          
         DP    SVGROSSP,=P'85'                                                  
         ZAP   DUB,SVGROSSP(6)                                                  
         CVB   R1,DUB                                                           
         ST    R1,SVAMTG                                                        
         OI    MYFLAG,GRSCALC                                                   
CALCGX   B     EXXMOD                                                           
*                                                                               
* SUB-ROUTINE TO INITIALIZE AND BUILD ALL CLEARANCE REQUESTS                    
*                                                                               
BLDTSREQ NTR1                                                                   
         XC    ZCTL,ZCTL                                                        
*                                                                               
         L     RF,ASPOOLA                                                       
         USING SPOOLD,RF                                                        
         MVC   ZAREA,SPACES                                                     
         DROP  RF                                                               
*                                                                               
         MVC   ZTYPE,=C'30'                                                     
         MVC   ZDATE,TODAY                                                      
         MVC   ZAGY,AGYALPH                                                     
         MVI   ZMED,C'N'                                                        
         MVC   ZSMED,SMEDTYPE                                                   
         MVC   ZCLT,CLI                                                         
         XC    ZMKT,ZMKT                                                        
*!!!     MVC   ZAGYCODE,SVAGYCOD                                                
*                                                                               
         CLC   PAYPROF+14(2),=C'*A'  TEST ALT ACC AGENCY                        
         BNH   BLDTS20                                                          
         CLC   PAYPROF+14(2),=C'00'  00 DOESN'T COUNT                           
         BE    BLDTS20                                                          
BLDTS10  MVC   ZAGYCODE,PAYPROF+14                                              
*                                                                               
BLDTS20  DS    0H                                                               
         MVC   ZOFFC,SVOFFC                                                     
         MVC   ZSEQNUM,SVSEQNUM    SEQUENCE NUMBER                              
*                                                                               
         CLI   ZSEQNUM,0           IF SEQUENCE NUMBER IS 0, DUMP!!!             
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         TM    APYFLAG,APYYES       CAME FROM AUTOPAY?                          
         BZ    *+8                                                              
         OI    ZSTATUS,ZSTATUS_AUTPAY                                           
*                                                                               
         LA    R1,TSARREC                                                       
         USING TSARRD,R1                                                        
*                                                                               
         MVC   ZPRD,=C'POL'        DEFAULT PRODUCT IS POL                       
         CLC   TSPRD3,=C'POL'                                                   
         BE    BLDTS23                                                          
         OC    PROD,PROD           ANY PRODUCT FILTER?                          
         BZ    *+10                                                             
         MVC   ZPRD,PROD                                                        
*                                                                               
BLDTS23  CLI   TSEST,0             IF NO ESTIMATE, DON'T PASS IT                
         BE    BLDTS25                                                          
         MVC   ESTB,TSEST                                                       
         EDIT  (1,TSEST),(3,ZACCEST),FILL=0                                     
*                                                                               
BLDTS25  CLI   AUTOAPP,C'Y'        AUTOAPPROVE?                                 
         BNE   BLDTS30             DO NOT BREAK OUT PRODUCT                     
*                                                                               
         CLC   TSPRD3,=C'POL'       POL?                                        
         BE    BLDTS30                                                          
*                                                                               
         MVC   ZPRD,TSPRD3                                                      
*                                                                               
BLDTS30  DS    0H                                                               
         MVC   BYTE,TSINDEX        INVOICE # INDEX AND COMMENT                  
**       BAS   RE,GETINVA                                                       
         BRAS  RE,GETINVA                                                       
         MVC   ZINV(10),INVA                                                    
         OC    ZINV(10),=C'          '                                          
*                                                                               
         XC    ZCOMMENT,ZCOMMENT                                                
         MVI   ZCONT,0                                                          
         OC    CMNT,CMNT           IS THERE A COMMENT?                          
         BZ    *+14                                                             
         MVC   ZCOM1,CMNT                                                       
         MVI   ZCONT,X'01'                                                      
*                                                                               
         ZAP   Z2GRSAMT,TSGROSS    GROSS AMOUNT                                 
         ZAP   Z2AMT,TSNET         NET AMOUNT                                   
*                                                                               
         MVI   ZAMTTYPE,C'1'                                                    
*                                                                               
         MVC   CKCRFLAG,TSFLAG     SAVE AWAY CR/CK FLAG                         
*                                                                               
         TM    TSFLAG,TSFLCK       CK?                                          
         BZ    *+8                                                              
         MVI   ZAMTTYPE,C'3'                                                    
*                                                                               
         TM    TSFLAG,TSFLCR       CR?                                          
         BZ    *+8                                                              
         MVI   ZAMTTYPE,C'2'                                                    
*                                                                               
         DROP  R1                                                               
*                                                                               
*        MVC   ZMKT,MARKET                                                      
         MVC   ZSTA(4),NETWORK                                                  
         MVI   ZSTA+4,C'N'         BAND IS NETWORK                              
         MVC   ZREP,REP                                                         
         OC    ZREP,=C'000'        DICK TURNER WANTS NUMBERS                    
         CLI   REPTYPE,C'S'        TEST FOR SPECIAL REP                         
         BNE   *+8                                                              
         MVI   ZREPTYPE,C'S'                                                    
         MVC   ZSTART(12),START                                                 
         OC    PROD2,PROD2                                                      
         BZ    *+10                                                             
         MVC   ZPRD2,PROD2                                                      
*                                                                               
         TM    MYFLAG,GRSCALC                                                   
         BZ    *+8                                                              
         OI    Z2FLAG,Z2GCALC      GROSS WAS CALC. FROM NET                     
*                                                                               
         MVC   Z2PID,SVPASSWD      SEND PID NUMBER                              
*                                                                               
         MVC   PRD1A,ZPRD                                                       
         MVC   PRD2A,ZPRD2                                                      
         ZAP   GRSAMT,Z2GRSAMT     GROSS AMOUNT                                 
         ZAP   NETAMT,Z2AMT        NET AMOUNT                                   
*                                                                               
BLDTSRX  B     EXXMOD                                                           
*                                                                               
* SUB-ROUTINE TO INITIALIZE AND BUILD A CLEARANCE REQUEST                       
*                                                                               
BLDREQ   DS    0H                                                               
         XC    ZCTL,ZCTL                                                        
*                                                                               
         L     RF,ASPOOLA                                                       
         USING SPOOLD,RF                                                        
         MVC   ZAREA,SPACES                                                     
         DROP  RF                                                               
*                                                                               
         MVC   ZTYPE,=C'30'                                                     
         MVC   ZDATE,TODAY                                                      
         MVC   ZAGY,AGYALPH                                                     
         MVI   ZMED,C'N'                                                        
         MVC   ZSMED,SMEDTYPE                                                   
         MVC   ZCLT,CLI                                                         
         EDIT  (1,EST),(3,ZACCEST),FILL=0                                       
         MVC   ZPRD,PROD                                                        
         OC    PROD,PROD                                                        
         BNZ   *+10                                                             
         MVC   ZPRD,=C'POL'        DEFAULT PRODUCT IS POL                       
*                                                                               
         CLI   ZSEQNUM,0           IF SEQUENCE NUMBER IS 0, DUMP!!!             
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    ZMKT,ZMKT                                                        
         TM    APYFLAG,APYYES       CAME FROM AUTOPAY?                          
         BZ    *+8                                                              
         OI    ZSTATUS,ZSTATUS_AUTPAY                                           
*                                                                               
BLDREQ2  MVC   ZSTA(4),NETWORK                                                  
         MVI   ZSTA+4,C'N'         BAND IS NETWORK                              
         MVC   ZREP,REP                                                         
         OC    ZREP,=C'000'        DICK TURNER WANTS NUMBERS                    
         CLI   REPTYPE,C'S'        TEST FOR SPECIAL REP                         
         BNE   *+8                                                              
         MVI   ZREPTYPE,C'S'                                                    
         MVC   ZSTART(12),START                                                 
         OC    PROD2,PROD2                                                      
         BZ    *+10                                                             
         MVC   ZPRD2,PROD2                                                      
*                                                                               
         L     R0,SVAMTG           ALWAYS SEND GROSS AMOUNT                     
         CVD   R0,DUB              AMOUNT IS PACK-ED                            
         ZAP   Z2GRSAMT,DUB                                                     
*                                                                               
         L     R0,SVAMTN           ALWAYS SEND NET AMOUNT                       
         CVD   R0,DUB              AMOUNT IS PACK-ED                            
         ZAP   Z2AMT,DUB                                                        
*                                                                               
         TM    MYFLAG,GRSCALC                                                   
         BZ    *+8                                                              
         OI    Z2FLAG,Z2GCALC      GROSS WAS CALC. FROM NET                     
*                                                                               
         MVC   Z2PID,SVPASSWD      SEND PID NUMBER                              
*                                                                               
BLDREQX  BR    RE                                                               
*                                                                               
* SUB-ROUTINE TO ADD A REQUEST RECORD                                           
*                                                                               
ADDREQ   LR    R0,RE                                                            
         ST    R0,FULL                                                          
*                                                                               
         CLI   AUTOAPP,C'Y'        AUTOAPPROVE?                                 
         BE    ADDR60                                                           
*                                                                               
         LA    R5,CLRINV1H                                                      
         LA    R4,AMTS                                                          
*                                                                               
ADDR10   LA    RF,CLRCOMNH                                                      
         CR    R5,RF                                                            
         BH    ADDRX                SHOULD'VE QUALIFIED ALREADY SO IT           
*                                                                               
         CLI   8(R5),0             ANY INVOICE?                                 
         BE    ADDRX                                                            
*                                                                               
         MVC   TMPINV,8(R5)                                                     
         OC    TMPINV,=C'          '                                            
*                                                                               
         MVC   ZINV(10),TMPINV                                                  
*                                                                               
         MVC   ZAMTTYPE,4(R4)                                                   
*                                                                               
         MVC   SVAMTG,0(R4)                                                     
         MVC   SVAMTN,0(R4)                                                     
*                                                                               
         CLI   4(R4),C'1'          NORMAL CLEARANCE?                            
         BE    ADDR20                                                           
         CLI   4(R4),C'3'          CK?                                          
         BE    ADDR20                                                           
*                                                                               
         L     RF,SVAMTG           ONLY MAKE SURE IT'S NEGATIVE FOR CR          
         LNR   RF,RF                                                            
         ST    RF,SVAMTG                                                        
*                                                                               
         L     RF,SVAMTN                                                        
         LNR   RF,RF                                                            
         ST    RF,SVAMTN                                                        
*                                                                               
* COMMENT ON THIS LINE                                                          
*                                                                               
ADDR20   XC    ZCOMMENT,ZCOMMENT                                                
         MVI   ZCONT,0                                                          
         TM    7(R4),X'20'         TEST COMMENT THIS LINE                       
         BO    *+12                YES                                          
         LA    R5,CLRCOMH(R5)      POINT TO COMMENT FIELD                       
         B     ADDR50                                                           
*                                                                               
         SR    R3,R3                                                            
         LA    R2,ZCOMMENT                                                      
ADDR30   LA    R5,CLRCOMH(R5)      POINT TO COMMENT FIELD                       
         GOTO1 VGETFLD,(R5)                                                     
         MVC   0(40,R2),FLD        EXTRACT COMMENT                              
         LA    R3,1(R3)            BUMP COUNT                                   
         STCM  R3,1,ZCONT          SAVE COUNT                                   
         LA    R2,40(R2)           BUMP POSITION                                
*                                                                               
         LA    R4,8(R4)            NEXT AMOUNT                                  
         CLI   7(R4),0             TEST LAST INPUT FIELD                        
         BNE   ADDR40              NO                                           
         B     ADDR50              AND EXIT                                     
         SPACE 1                                                                
ADDR40   CLI   7(R4),X'20'         NEXT LINE CMT ONLY                           
         BNE   ADDR45              YES                                          
         ZIC   R0,0(R5)                                                         
         AR    R5,R0               NEXT INV                                     
         B     ADDR30                                                           
ADDR45   AHI   R4,-8               GET POINTER BACK TO CURRENT LINE             
*************                                                                   
*                                                                               
ADDR50   NI    MYFLAG,X'FF'-GRSCALC                                             
*                                                                               
         L     R1,SVAMTN                                                        
         CVD   R1,DUB                                                           
         ZAP   SVGROSSP,DUB                                                     
         MP    SVGROSSP,=P'100'     GROSS = NET / 0.85                          
         DP    SVGROSSP,=P'85'                                                  
         ZAP   DUB,SVGROSSP(6)                                                  
         CVB   R1,DUB                                                           
         ST    R1,SVAMTG                                                        
         OI    MYFLAG,GRSCALC                                                   
*                                                                               
         DS    0H                                                               
         NI    Z2FLAG,X'FF'-Z2GCALC                                             
*                                                                               
         L     R0,SVAMTG           ALWAYS SEND GROSS AMOUNT                     
         CVD   R0,DUB              AMOUNT IS PACK-ED                            
         ZAP   Z2GRSAMT,DUB                                                     
*                                                                               
         L     R0,SVAMTN                                                        
         CVD   R0,DUB              AMOUNT IS PACK-ED                            
         ZAP   Z2AMT,DUB                                                        
*                                                                               
         TM    MYFLAG,GRSCALC                                                   
         BZ    *+8                                                              
         OI    Z2FLAG,Z2GCALC      GROSS WAS CALC. FROM NET                     
*                                                                               
ADDR60   MVI   ZCTL+15,X'40'                                                    
         CLI   ZCONT,5                                                          
         BNH   *+8                                                              
         MVI   ZCONT,5             MAX OF 5 COMMENTS ALLOWED                    
*                                                                               
         CLI   PAYPROF,C'G'                                                     
         BNE   ADDR70                                                           
         CLC   Z2GRSAMT,=PL6'0'     GROSS AMT                                   
         BE    ADDR75                                                           
         B     ADDR80                                                           
*                                                                               
ADDR70   CLC   Z2AMT,=PL6'0'        NET AMOUNT                                  
         BNE   ADDR80                                                           
ADDR75   CLI   PAYPROFB+3,C'Y'     ALLOW ZERO $ CLEARANCES?                     
         BNE   ADDR140                                                          
*                                                                               
ADDR80   OC    CRCKDAT,CRCKDAT     REVERSE=OPTION ?                             
         BZ    ADDR110                                                          
***************************************************                             
* REVERSE= OPTION SENDS 2 REQ RECS TO ACC                                       
* FIRST REVERSES SIGN ON $$ AMTS ON REQ                                         
* SECOND SEND SAME REQ WITH BITS FLIPPED ON CR/CK                               
* AND MAKES SURE THE $$AMTS ARE IN THE NORMAL SIGN                              
*                                                                               
*  FOR AUTOAPPRVE                                                               
*  CK COMES IN WITH A NEGATIVE VALUE                                            
*  CR COMES IN WITH A NEGATIVE VALUE                                            
*                                                                               
*  FOR  NON AUTOAPPRVE                                                          
*  CK COMES IN WITH A POSITIVE VALUE                                            
*  CR COMES IN WITH A NEGATIVE VALUE                                            
*                                                                               
*  IN ALL CASES TO DO A REVERSAL REQUEST THE                                    
*  CK MUST HAVE A NEGATIVE VALUE                                                
*  CR MUST HAVE A POSITIVE VALUE                                                
***************************************************                             
         CLI   AUTOAPP,C'Y'        AUTOAPPROVE?                                 
         BE    ADDR90              YES DO AUTOAPPROVE REVERSAL                  
*                                                                               
*  NON AUTOAPPROVE LOGIC                                                        
         CLI   ZAMTTYPE,C'1'          NORMAL                                    
         BE    ADDR110                                                          
         CLI   ZAMTTYPE,C'3'          CK?                                       
         BNE   ADDR85                                                           
         MP    Z2GRSAMT,=P'-1'     MAKE IT NEGATIVE                             
         MP    Z2AMT,=P'-1'        MAKE IT NEGATIVE                             
         B     ADDR110                                                          
ADDR85   CLI   ZAMTTYPE,C'2'          CR?                                       
         BE    *+6                                                              
         DC    H'0'                   ???                                       
         MP    Z2GRSAMT,=P'-1'     MAKE IT POSITIVE                             
         MP    Z2AMT,=P'-1'        MAKE IT POSITIVE                             
         B     ADDR110                                                          
*                                                                               
*  AUTOAPPROVE LOGIC                                                            
ADDR90   CLI   ZAMTTYPE,C'1'          NORMAL                                    
         BE    ADDR110                                                          
         CLI   ZAMTTYPE,C'3'          CK?                                       
         BE    ADDR110                ALREADY NEGATIVE                          
ADDR95   CLI   ZAMTTYPE,C'2'          CR?                                       
         BE    *+6                                                              
         DC    H'0'                   ???                                       
         MP    Z2GRSAMT,=P'-1'     MAKE IT POSITIVE                             
         MP    Z2AMT,=P'-1'        MAKE IT POSITIVE                             
         B     ADDR110                                                          
*                                                                               
ADDR110  GOTO1 VDATMGR,DMCB,=C'DMADD',=C'REQUEST',ZCTL,ZCTL                     
         CLI   8(R1),0             TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                YES-TAKE A HIT                               
         OC    CRCKDAT,CRCKDAT     REVERSE=OPTION?                              
         BZ    ADDR140                                                          
***********************************************************                     
* SEND 2ND REQ TO ACC WITH CR/CK BITS FLIPPED                                   
* LEAVE $$AMT SIGN AS IS SINCE IT WAS 'REVERSED'ABOVE                           
* TO CANCEL OUT PREVIOUS ACC REQ AND WE ARE USING SAME                          
* WORK                                                                          
***********************************************************                     
         CLI   ZAMTTYPE,C'1'          NORMAL                                    
         BE    ADDR140                                                          
         CLI   ZAMTTYPE,C'2'          CR?                                       
         BNE   ADDR120                                                          
         MVI   ZAMTTYPE,C'3'          YES -> CK                                 
         B     ADDR130                                                          
ADDR120  CLI   ZAMTTYPE,C'3'          CK?                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ZAMTTYPE,C'2'          YES -> CR                                 
         B     ADDR130                                                          
*                                                                               
ADDR130  GOTO1 VDATMGR,DMCB,=C'DMADD',=C'REQUEST',ZCTL,ZCTL                     
*                                                                               
ADDR140  CLI   AUTOAPP,C'Y'                                                     
         BE    ADDRX                                                            
*                                                                               
         ZIC   RF,0(R5)            BUMP TO NEXT LINE ON SCREEN                  
         AR    R5,RF                                                            
         AHI   R4,8                                                             
         B     ADDR10                                                           
*                                                                               
ADDRX    L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*        GETEL                                                                  
         PRINT GEN                                                              
         PRINT NOGEN                                                            
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
*ATCH    DC    XL8'00'                                                          
ADDCODE  DC    CL8'ADD=CODE'                                                    
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
NODATA   DC    C'** NO UNITS FOUND - CHECK KEY FIELDS/OPTIONS **'               
*                                                                               
*=========================================================*                     
* BUILD X'03' AND X'05' ELEMS FOR CLRST (IN AIOAREA4)     *                     
*=========================================================*                     
BLDCLELS NTR1  BASE=*,LABEL=*                                                   
         L     R5,AIOAREA4                                                      
         USING CLRSTATD,R5                                                      
*                                                                               
* !!!! USE GOAL KEY IN THIS DUMMY AREA TO FOOL RECUP INTO DEFAULTING            
* !!!! THE LENGH TO JUST UNDER 4000 BYTES                                       
*                                                                               
         MVI   0(R5),X'02'                                                      
*                                                                               
         LA    R5,CLSELEMS-CLRSTATD(R5)  POINT TO FIRST ELEMENT                 
         DROP  R5                                                               
*                                                                               
         CLI   0(R5),X'01'         MAKE SURE IT'S THE X'01' ELEMENT             
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         ZIC   RF,1(R5)                                                         
         AR    R5,RF               BUMP TO END OF X'01' ELEM                    
*                                                                               
BLDCL10  CLI   0(R5),0             DID WE BUILD AN X'03' YET?                   
         BNE   BLDCL20             YES, CHECK IF IT'S THE SAME INV              
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING CLSTEL03,R6                                                      
*                                                                               
         MVI   CLSTEL03,X'03'                                                   
         MVI   CLSTLN03,CL03ELLN                                                
         MVC   CLS3INV(10),INVA        MOVE IN INVOICE                          
         DROP  R6                                                               
*                                                                               
         GOTO1 =V(RECUP),DMCB,AIOAREA4,WORK,(R5),RR=MYRELO                      
*                                                                               
         ZIC   RF,1(R5)                                                         
         AR    R5,RF               BUMP PAST THE X'03' TO ADD X'05'             
         B     BLDCL30             ADD X'05'                                    
*                                                                               
BLDCL20  DS    0H                                                               
         CLI   0(R5),X'03'                                                      
         BNE   BLDCL25                                                          
         USING CLSTEL03,R5                                                      
         CLC   CLS3INV(10),INVA    SAME INVOICE?                                
         BNE   BLDCL25             NO - GET NEXT X'03'                          
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     BLDCL30             YES - ADD NEW X'05'                          
         DROP  R5                                                               
*                                                                               
BLDCL25  ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     BLDCL10                                                          
*                                                                               
BLDCL30  DS    0H                  ADD X'05'                                    
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING CLSTEL05,R6                                                      
         MVI   CLSTEL05,X'05'                                                   
         MVI   CLSTLN05,CL05ELN2                                                
         MVC   CLS5PRD1,PRD1A      ALPHA PRODUCT 1                              
         MVC   CLS5PRD2,PRD2A      ALPHA PRODUCT 2                              
         MVC   CLS5EST,ESTB        BINARY ESTIMATE                              
*                                                                               
         TM    CKCRFLAG,CKFLON     CHECK                                        
         BZ    *+8                                                              
         OI    CLS5STAT,CLS5STAT_CK                                             
*                                                                               
         TM    CKCRFLAG,CRFLON     CREDIT                                       
         BZ    *+8                                                              
         OI    CLS5STAT,CLS5STAT_CR                                             
*                                                                               
*                                                                               
BLDCL40  XC    CLS5GRS,CLS5GRS                                                  
         XC    CLS5NET,CLS5NET                                                  
*                                                                               
* ON CLST REC $ AMTS ARE NEGATIVE FOR CK AND CR                                 
* FOR CK, $ AMT COMES IN POSITIVE BUT IS MADE NEGATIVE FOR CLST REC             
* FOR CR, $ AMT COMES IN NEGATIVE AND STAYS NEGATIVE FOR CLST REC               
* FOR REVERSE= OPTION CK AND CR AMTS MUST BE POSITIVE FOR CLST REC              
*                                                                               
         ZAP   TMPAMT,GRSAMT                                                    
         OC    CRCKDAT,CRCKDAT     REVERSE=OPTION?                              
         BNZ   BLDCL42             YES REVERSE AMOUNT                           
*                                                                               
BLDCL41  TM    CKCRFLAG,CKFLON           IF CK, MAKE IT POSITIVE                
         BZ    *+10                      B/C ACC NEGATES IT                     
BLDCL42  MP    TMPAMT,=P'-1'                                                    
         CVB   RF,TMPAMT                                                        
         STCM  RF,15,CLS5GRS                                                    
*                                                                               
         ZAP   TMPAMT,NETAMT                                                    
         OC    CRCKDAT,CRCKDAT     REVERSE=OPTION?                              
         BNZ   BLDCL45             NO                                           
*                                                                               
BLDCL44  TM    CKCRFLAG,CKFLON           IF CK, MAKE IT POSITIVE                
         BZ    *+10                      B/C ACC NEGATES IT                     
BLDCL45  MP    TMPAMT,=P'-1'                                                    
         CVB   RF,TMPAMT                                                        
         STCM  RF,15,CLS5NET                                                    
*                                                                               
*                                                                               
BLDCL60  OC    CRCKDAT,CRCKDAT     REVERSE= OPTION ?                            
         BZ    *+8                                                              
         OI    CLS5STAT,CLS5STAT_RV        SET REVERSE FLAG                     
         GOTO1 =V(RECUP),DMCB,AIOAREA4,WORK,(R5),RR=MYRELO                      
*                                                                               
         OC    CRCKDAT,CRCKDAT     REVERSE= OPTION ?                            
         BZ    BLDCLX                                                           
         ZIC   RF,1(R5)            BUMP TO NEXT AVAIL SPACE                     
         AR    R5,RF                                                            
*                                                                               
         NI    CLS5STAT,X'FF'-CLS5STAT_RV        UNSET REVERSE FLAG             
         TM    CLS5STAT,CLS5STAT_CR        IF CR                                
         BNO   *+16                                                             
         NI    CLS5STAT,X'FF'-CLS5STAT_CR                                       
         OI    CLS5STAT,CLS5STAT_CK         MAKE IT CK                          
         B     BLDCL65                                                          
         TM    CLS5STAT,CLS5STAT_CK      ELSE MUST BE CK                        
         BO    *+6                                                              
         DC    H'0'                      ???                                    
         NI    CLS5STAT,X'FF'-CLS5STAT_CK                                       
         OI    CLS5STAT,CLS5STAT_CR         MAKE IT CR                          
*                                                                               
BLDCL65  ICM   RF,15,CLS5GRS       MAKE SURE THEY'RE NEGATIVE                   
         LNR   RF,RF                                                            
         STCM  RF,15,CLS5GRS                                                    
         ICM   RF,15,CLS5NET                                                    
         LNR   RF,RF                                                            
         STCM  RF,15,CLS5NET                                                    
*                                                                               
         GOTO1 =V(RECUP),DMCB,AIOAREA4,WORK,(R5),RR=MYRELO                      
         B     BLDCLX                                                           
*                                                                               
                                                                                
*                                                                               
BLDCLX   DS    0H                                                               
         J     EXXMOD                                                           
         DROP  R6                                                               
***********************************************************************         
* GETSPRAT - CHECKS SPECIAL REP CODES ON SPECIAL RATE ELEMENTS                  
***********************************************************************         
GETSPRAT NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   GTSREX              YES                                          
         L     R5,12(R1)                                                        
         B     GTSR100                                                          
         USING NUSPRD,R5                                                        
GTSR050  ZIC   RE,NUSPRLEN                                                      
         AR    R5,RE                                                            
         CLI   NUSPREL,X'03'                                                    
         BE    GTSR100                                                          
         MVI   12(R1),X'FF'                                                     
         B     GTSREX                                                           
*                                                                               
GTSR100  TM    SPECREP,X'80'       TEST FOR ANY SPEC REP FILTER                 
         BO    GTSR120             YES                                          
         CLC   SRATREP,NUSPRREP    TEST FOR MATCH ON SPECIAL REP                
         BE    GTSREX              YES                                          
         B     GTSR050             YES                                          
*                                                                               
GTSR120  OC    NUSPRREP,NUSPRREP   TEST FOR ANY SPECIAL REP                     
         BNZ   GTSREX                                                           
         B     GTSR050             NO-SKIP RECORD                               
*                                                                               
GTSREX   J     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET INVOICE # FROM SCREEN                                                     
***********************************************************************         
GETINVA  NTR1  BASE=*,LABEL=*                                                   
         XC    INVA,INVA                                                        
         XC    CMNT,CMNT                                                        
*                                                                               
         OC    CRCKDAT,CRCKDAT     REVERSE OPTION                               
         BNZ   GINVA100            GET INVOICE NUMBER FROM TABLE                
*                                                                               
         LA    R3,CLRINV1H                                                      
         CLI   BYTE,X'01'                                                       
         BE    GINVA40                                                          
         ZIC   R5,BYTE              INVOICE INDEX # ON SCREEN                   
         SHI   R5,1                                                             
*                                                                               
GINVA10  DS    0H                                                               
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         BCT   R5,GINVA10                                                       
*                                                                               
GINVA40  DS     0H                                                              
         MVC   INVA,8(R3)           SAVE ALPHA INVOICE #                        
         OC    INVA,=C'          '                                              
*                                                                               
         ZIC   RF,0(R3)             NOW CHECK IF THERE'S A COMMENT              
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
*                                                                               
         CLI   5(R3),0              IS THERE A COMMENT?                         
         BE    *+10                                                             
         MVC   CMNT,8(R3)           YES - SAVE IT                               
         B     GINVAEX                                                          
*                                                                               
* GET INVOICE NUMBER FROM INVOICE TABLE                                         
* THIS IS USED FOR REVERSE OPTION ONLY                                          
*                                                                               
GINVA100 ZIC   RE,BYTE              TABLE INDEX                                 
         LA    RF,INVTABLE                                                      
         LTR   RE,RE                                                            
         BZ    GINVA160                                                         
*                                                                               
GINVA120 LA    RF,12(RF)                                                        
         BCT   RE,GINVA120                                                      
                                                                                
*                                                                               
GINVA160 MVC   INVA,0(RF)           SAVE ALPHA INVOICE #                        
*                                                                               
         CLI   CLRCOM1H+5,0                                                     
         BE    GINVAEX                                                          
         MVC   CMNT,CLRCOM1         SAVE COMMENT                                
*                                                                               
GINVAEX  J     EXXMOD                                                           
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*=========================================================*                     
* READ CLRST RECORDS GET SUM OF QUALIFYING REVERSALS      *                     
*=========================================================*                     
FINDCRCK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* GET CORRECT CLRST RECORD                                                      
*                                                                               
         LA    R2,BLOCK                                                         
         USING PAYBLKD,R2                                                       
         XC    PAYNT,PAYNT                                                      
         XC    PAYGROSS,PAYGROSS                                                
         LA    R5,KEY                                                           
         USING CLRSTATD,R5                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   CLSKTYPE,=X'0D76'                                                
         MVC   CLSKAGMD,AGYMED     A-M                                          
         MVC   CLSKCLT,CLIPK       CLT                                          
         MVC   DUB(4),NETWORK                                                   
         MVI   DUB+4,C'N'                                                       
         PRINT GEN                                                              
         GOTO1 VMSPACK,DMCB,MARKET,DUB,CLSKMKT                                  
         PRINT NOGEN                                                            
         DROP  R5                                                               
*                                                                               
         GOTO1 AIO,DMCB,SPT+HIGH+DIR                                            
         CLC   KEY(10),KEYSAVE     SAME TYPE/CLT/MKT/STAT                       
         BNE   FCRNODAT            YES - UPDATE LAST RECORD                     
*                                                                               
FCR20    MVC   KEYSAVE,KEY         SAVE PREVIOUS KEY                            
         GOTO1 AIO,DMCB,SPT+SEQ+DIR                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   FCR40                                                            
         CLC   CRCKDAT,KEY+10                                                   
         BH    FCR20                                                            
         BE    FCR50                                                            
* UPDATE LAST RECORD FOR THIS STATION                                           
FCR40    MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
         MVC   NDXDA,KEY+14        RESTORE LAST DISK ADDRESS                    
         L     R3,AIOAREA2                                                      
         GOTO1 AIO,DMCB,SPT+READ+DIR   REPOSITION THE POINTER                   
FCR50    GOTO1 AIO,DMCB,SPT+GET+FILE+UPDATE,AIOAREA2                            
*                                 IT'S A MATCH                                  
FCR60    SR    R4,R4               INIT 1ST ELEM 5 POINTER                      
         GOTO1 VHELLO,DMCB,(C'G',=C'SPTFILE'),(X'01',AIOAREA2),0                
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BE    *+6                                                              
         DC    H'0'                ???                                          
         L     R5,12(R1)                                                        
         B     FCR100                                                           
*                                                                               
*  GET NEXT 01 ELEMENT                                                          
FCR80    ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),0              END OF RECORD ERROR                         
         BE    FCR600               GET NEXT CLRST RECORD                       
         CLI   0(R5),X'01'                                                      
         BNE   FCR80                                                            
* CHECK IF THE ELEMENT CLEARENCE DATE MATCHES THE REQUEST DATE                  
FCR100   CLC   CRCKDAT,CLSTCLRD-CLSTEL01(R5)                                    
         BH    FCR80                                                            
         BL    FCRNODAT                                                         
         LTR   R4,R4                                                            
         BNZ   FCR120                                                           
         LR    R4,R5                POINT R4 TO X'01' ELEMENT                   
*                                                                               
*  GET NEXT 05 ELEMENT                                                          
FCR120   ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),0              END OF RECORD ERROR                         
         BE    FCR280               GET NEXT CLRST RECORD                       
         CLI   0(R5),X'01'          IF ELEMENT 1 CHECK TOTAL                    
         BE    FCR280                                                           
         CLI   0(R5),X'05'          IF ELEMENT 5 ADD TO TOTAL                   
         BNE   FCR120                                                           
*                                                                               
* IF QUALIFIES ACCUMULATE THE GROSS/NET DOLLAR AMOUNT                           
* R5 = ELEMENT 5                                                                
FCR200   DS    0H                                                               
         USING CLSTEL05,R5                                                      
         CLI   AUTOAPP,C'Y'                                                     
         BE    FCR230                                                           
* REGULAR REVERSALS OPTION FILTER                                               
         CLC   CLS5PRD1,PROD       ALPHA PRODUCT 1                              
         BNE   FCR120                                                           
         CLC   CLS5PRD2,PROD2      ALPHA PRODUCT 2                              
         BNE   FCR120                                                           
         CLC   CLS5EST,EST         BIN ESTIMATE                                 
         BNE   FCR120                                                           
         B     FCR240                                                           
* AUTOAPPROVE REVERSALS OPTION FILTER                                           
FCR230   OC    PROD(8),PROD        WAS ANY PROD FILTER REQUESTED                
         BZ    FCR235                                                           
         CLC   CLS5PRD1,PROD       ALPHA PRODUCT 1                              
         BNE   FCR120                                                           
*                                                                               
         OC    PROD2,PROD2         WAS PIGGYBACK PROD FILTER REQUESTED          
         BNZ   *+18                                                             
         CLC   CLS5PRD2,=XL3'404040'  CHECK FOR NO PIGGYBACK                    
         BNE   FCR120                                                           
         B     FCR235                                                           
         CLC   CLS5PRD2,PROD2      ALPHA PRODUCT 2                              
         BNE   FCR120                                                           
*                                                                               
FCR235   OC    EST,EST             WAS ESTIMATE FILTER REQUESTED                
         BZ    FCR240                                                           
         CLC   CLS5EST,EST         BIN ESTIMATE                                 
         BNE   FCR120                                                           
FCR240   TM    CLS5STAT,X'60'      CRCK ?                                       
         BZ    FCR120                                                           
         TM    CLS5STAT,CLS5STAT_RV  ELEM ALREADY REVERSED                      
         BO    FCR120                                                           
******************************************                                      
*AT 'TEST' FUNCITON IN NPAY PAYFLAGS IS NOT YET SET                             
*IT SHOULD BE SET BY THE 'CLEAR' FUNCTION                                       
*THIS ATTEMPTS TO HELP USER MATCH THE ORIGINAL CR/CK REQUEST                    
******************************************                                      
         TM    PAYFLAGS,REVRSCR+REVRSCK                                         
         BZ    FCR260                                                           
         TM    CLS5STAT,CLS5STAT_CK    CK?                                      
         BNO   FCR250                                                           
         TM    PAYFLAGS,REVRSCK                                                 
         BNO   FCR120               GET NEXT 05 ELEMENT                         
         B     FCR260                                                           
*                                                                               
FCR250   TM    CLS5STAT,CLS5STAT_CR    CR?                                      
         BNZ   *+6                                                              
         DC    H'0'                ?                                            
         TM    PAYFLAGS,REVRSCR                                                 
         BNO   FCR120               GET NEXT 05 ELEMENT                         
*                                                                               
* ADD GROSS AND NET DOLLARS FROM 05 ELEMENT                                     
FCR260   ICM   R1,15,CLS5GRS                                                    
         A     R1,PAYGROSS                                                      
         ST    R1,PAYGROSS                                                      
         ICM   R1,15,CLS5NET                                                    
         A     R1,PAYNT                                                         
         ST    R1,PAYNT                                                         
         OI    PAYSTAT,PAYABLE                                                  
         B     FCR120               GET NEXT 05 ELEMENT                         
         DROP  R5                                                               
*                                                                               
* CHECK CLRST TOTALS MATCH SCREEN TOTALS                                        
*                                                                               
FCR280   OC    PAYNT,PAYNT                                                      
         BNZ   *+14                                                             
         OC    PAYGROSS,PAYGROSS    IF DOLLARS ZERO                             
         BZ    FCR300               NOTHING TO CHECK                            
         MVC   BYTE,PAYPROF        GROSS/NET OPTION                             
         CLI   ACTION,TEST                                                      
         BNE   *+18                                                             
         CLI   AMTOPT,0                                                         
         BE    *+10                                                             
         MVC   BYTE,AMTOPT                                                      
*                                                                               
         L     R0,PAYGROSS                                                      
         CLI   BYTE,C'G'                                                        
         BE    *+8                                                              
         L     R0,PAYNT                                                         
         C     R0,TOTAMT           TEST IF SCREEN MATCHES CLRST                 
         BE    FCR400                                                           
*                                                                               
* CLEAR ACCUMULATORS                                                            
FCR300   XC    PAYNT,PAYNT                                                      
         XC    PAYGROSS,PAYGROSS                                                
         SR    R4,R4               CLEAR FIRST ELEMENT 5 POINTER                
* CHECK WHAT ELEMENT R5 IS POINTING TO                                          
         CLI   0(R5),0              END OF RECORD READ NEXT CLEARST             
         BE    FCR600                                                           
         CLI   0(R5),X'01'          ANOTHER 01 ELEMENT TO CHECK                 
         BE    FCR100                                                           
         DC    H'0'                 R5 IS LOST                                  
*                                                                               
*  IS ACTION 'CLEAR' CHANGE STATUS ON ELEMENT 5                                 
*  TO STATUS REVERSED THIS WAY THEY CANNOT REVERSE                              
*  THE SAME CLRST INFORMATION MULTIPLE TIMES                                    
*                                                                               
FCR400   MVC   REVNET,PAYNT         STORE TOTAL NET COST                        
         MVC   REVGROSS,PAYGROSS                                                
         MVC   TOTG,REVGROSS                                                    
         MVC   TOTN,REVNET                                                      
         XC    PAYNT,PAYNT                                                      
         XC    PAYGROSS,PAYGROSS                                                
         CLI   0(R4),X'01'         R4 POINTS TO 01 ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                 IF NOT AT ELEMENT 5 DUMP                    
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'03'          GET 03 ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,TABLEINV          STORE INV NUMBER AND OFFSET                 
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'05'          GET 05 ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* DO ESTIMATE AND PRODUCT FILTERING                                             
*                                                                               
FCR420   DS    0H                                                               
         USING CLSTEL05,R4                                                      
         CLI   AUTOAPP,C'Y'                                                     
         BE    FCR430                                                           
* REGULAR REVERSALS OPTION FILTER                                               
         CLC   CLS5PRD1,PROD       ALPHA PRODUCT 1                              
         BNE   FCR520                                                           
         CLC   CLS5PRD2,PROD2      ALPHA PRODUCT 2                              
         BNE   FCR520                                                           
         CLC   CLS5EST,EST         BIN ESTIMATE                                 
         BNE   FCR520                                                           
         B     FCR480                                                           
* AUTOAPPROVE REVERSALS OPTION FILTER                                           
FCR430   OC    PROD(8),PROD        WAS ANY PROD FILTER REQUESTED                
         BZ    FCR435                                                           
         CLC   CLS5PRD1,PROD       ALPHA PRODUCT 1                              
         BNE   FCR520                                                           
*                                                                               
         OC    PROD2,PROD2         WAS PIGGYBACK PROD FILTER REQUESTED          
         BNZ   *+18                                                             
         CLC   CLS5PRD2,=XL3'404040'  CHECK FOR NO PIGGYBACK                    
         BNE   FCR520                                                           
         B     FCR435                                                           
         CLC   CLS5PRD2,PROD2      ALPHA PRODUCT 2                              
         BNE   FCR520                                                           
*                                                                               
FCR435   OC    EST,EST             WAS ESTIMATE FILTER REQUESTED                
         BZ    FCR480                                                           
         CLC   CLS5EST,EST         BIN ESTIMATE                                 
         BNE   FCR520                                                           
*                                                                               
*  CHECK PAYTYPE                                                                
*                                                                               
FCR480   TM    CLS5STAT,CLS5STAT_CK    CK?                                      
         BNO   FCR500                                                           
         TM    PAYFLAGS,REVRSCK                                                 
         BNO   FCR520               GET NEXT 05 ELEMENT                         
         B     FCR540               SET FLAG ADD TO ACCUMS                      
*                                                                               
FCR500   TM    CLS5STAT,CLS5STAT_CR    CR?                                      
         BNO   FCR520                                                           
         TM    PAYFLAGS,REVRSCR                                                 
         BO    FCR540               SET FLAG ADD TO ACCUMS                      
*  GET NEXT 05 ELEMENT                                                          
FCR520   ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),0              END OF RECORD                               
         BE    FCR560               PUT THE CLRST RECORD                        
         CLI   0(R4),X'01'                                                      
         BE    FCR560               PUT THE CLRST RECORD                        
         CLI   0(R4),X'05'          IF ELEMENT 5 CHECK FOR REVERSE              
         BE    FCR420                                                           
         CLI   0(R4),X'03'          IF ELEMENT 3 ADD INV TO TABLE               
         BNE   FCR520               GET NEXT ELEMENT                            
         BAS   RE,TABLEINV          STORE INV NUMBER AND OFFSET                 
         B     FCR520               GET NEXT ELEMENT                            
*                                                                               
* ADD GROSS AND NET DOLLARS FROM 05 ELEMENT                                     
FCR540   CLI   CLSTLN05,CL05ELN2       TO REVERSE MUST BE EXP 05 ELEM           
         BL    FCROLDCL                                                         
         MVC   CLS5CLSQ,STATSEQ     SEQUENCE NUMBER OF CLEARENCE                
         MVC   CLS5CLDT,TODAYC      TODAYS DATE                                 
         OI    CLS5STAT,CLS5STAT_RV    SET AS REVERSED                          
         ICM   R1,15,CLS5GRS                                                    
         A     R1,PAYGROSS                                                      
         ST    R1,PAYGROSS                                                      
         ICM   R1,15,CLS5NET                                                    
         A     R1,PAYNT                                                         
         ST    R1,PAYNT                                                         
         DROP  R4                                                               
*                                                                               
*  IF AUTOPAY BUILD TSAR RECORDS TO ADD REQUESTS AND CLRST ELEMS                
*                                                                               
         CLI   AUTOAPP,C'N'                                                     
         BE    FCR520                                                           
         BAS   RE,BLDREVTS                                                      
         B     FCR520               GET NEXT 05 ELEMENT                         
*                                                                               
*  BEFORE WRITING CLRST RECORD BACK                                             
*  MAKE SURE THE ELEMENTS MARKED AS                                             
*  REVERSED ADD UP TO AMOUNT ON THE SCREEN                                      
*                                                                               
FCR560   MVC   BYTE,PAYPROF        GROSS/NET OPTION                             
         CLI   AMTOPT,0                                                         
         BE    *+10                                                             
         MVC   BYTE,AMTOPT                                                      
*                                                                               
         L     R0,PAYGROSS                                                      
         CLI   BYTE,C'G'                                                        
         BE    *+8                                                              
         L     R0,PAYNT                                                         
         C     R0,TOTAMT           SEE IF SCREEN MATCHES REVERSED ELEMS         
         BE    *+6                                                              
         DC    H'0'                ERROR IN TOTAL                               
*                                                                               
         CLI   ACTION,CLEAR        TEST FOR ACTION CLEAR                        
         BNE   FCREX               ONLY CHANGE ELEMNTS ON CLEAR ACTION          
         GOTO1 AIO,DMCB,SPT+PUT+FILE,AIOAREA2                                   
         B     FCREX                                                            
*                                                                               
* GET NEXT CLRST RECORD                                                         
*                                                                               
FCR600   B     FCRNODAT            ERROR NO DATA FOUND                          
         GOTO1 AIO,DMCB,SPT+SEQ+DIR                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   FCRNODAT                                                         
         CLC   CRCKDAT,KEY+10                                                   
         BH    FCRNODAT                                                         
* UPDATE LAST RECORD FOR THIS STATION                                           
FCR620   L     R3,AIOAREA2                                                      
         GOTO1 AIO,DMCB,SPT+GET+FILE+UPDATE,AIOAREA2                            
         B     FCR60                                                            
*                                                                               
* ERROR MESSAGE COULD NOT FIND MATCHING INFORMATION                             
FCRNODAT MVI   FERN,USERERR                                                     
         MVC   PAYMSG(L'NOREV),NOREV                                            
         B     FCERR                                                            
*                                                                               
* ERROR MESSAGE OLD STYLE CLEARENCE CANNOT BE REVERSED                          
FCROLDCL MVI   FERN,USERERR                                                     
         MVC   PAYMSG(L'OLDCLEAR),OLDCLEAR                                      
         B     FCERR                                                            
*                                                                               
FCERR    GOTO1 VERROR                                                           
*                                                                               
FCREX    J     EXXMOD                                                           
         DROP  R2                                                               
*                                                                               
* FINDCRCK ERROR MESSAGES                                                       
*                                                                               
NOREV    DC    C'** AMOUNT/DATE COMBO DOES NOT MATCH ANY CLEARENCES **'         
OLDCLEAR DC    C'** OLD STYLE CLEARENCE CANNOT BE REVERSED **'                  
         SPACE 2                                                                
*                                                                               
*  BUILT INVOICE TABLE AND STORE OFFSET                                         
*  TO CURRENT INVOICE NUMBER IN INVTBOFF                                        
*                                                                               
TABLEINV NTR1                                                                   
*                                                                               
         CLI   AUTOAPP,C'Y'        AUTOAPPROVE                                  
         BNE   TABINVEX                                                         
*                                                                               
         USING CLSTEL03,R4                                                      
         SPACE 2                                                                
*                                                                               
         LA    RE,INVTABLE                                                      
         LA    RF,14                                                            
         SR    R2,R2                                                            
*                                                                               
TABINV20 OC    0(12,RE),0(RE)                                                   
         BZ    TABINV60                                                         
         CLC   CLS3INV,0(RE)                                                    
         BE    TABINV60                                                         
         LA    RE,12(RE)                                                        
         LA    R2,1(R2)                                                         
         BCT   RF,TABINV20                                                      
         DC    H'0'                                                             
*                                                                               
TABINV60 MVC   0(12,RE),CLS3INV                                                 
         STCM  R2,1,INVTBOFF                                                    
*                                                                               
TABINVEX J     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
*  ADD TSAR RECORDS TO REVERSE AUTOAPPROVE CLEARENCES                           
*  TSAR RECORDS ARE BUILT FROM 05 CLRST ELEMENTS                                
*                                                                               
BLDREVTS NTR1                                                                   
*                                                                               
         CLI   AUTOAPP,C'Y'        AUTOAPPROVE                                  
         BNE   BLDREVEX                                                         
*                                                                               
         USING CLSTEL05,R4                                                      
*                                  IT, ELSE ACCUMULATE TOTALS                   
         XC    TSARREC,TSARREC                                                  
         LA    R3,TSARREC                                                       
         USING TSARRD,R3                                                        
*                                                                               
         MVC   TSINDEX,INVTBOFF    INDEX # TO INVOICE IN INVTABLE               
         MVC   TSEST,CLS5EST       ESTIMATE                                     
         MVC   TSPRD3,CLS5PRD1     DEFAULT TO FIRST PRODUCT                     
         MVI   TSFLAG,X'01'        FLAGS                                        
         TM    CLS5STAT,CLS5STAT_CK    CK?                                      
         BO    BLDREV20                                                         
         MVI   TSFLAG,X'02'        FLAGS                                        
         TM    CLS5STAT,CLS5STAT_CR    CR?                                      
         BO    BLDREV20                                                         
         DC    H'0'                                                             
*                                                                               
BLDREV20 ZAP   TSGROSS,=PL6'0'                                                  
         ICM   R1,15,CLS5GRS                                                    
         CVD   R1,DUB                                                           
         AP    TSGROSS(6),DUB(8)         GROSS AMOUNT                           
         ZAP   TSNET,=PL6'0'                                                    
         ICM   R1,15,CLS5NET                                                    
         CVD   R1,DUB                                                           
         AP    TSNET(6),DUB(8)           NET AMOUNT                             
*                                                                               
         MVI   T.TSACTN,TSAADD     ADD RECORD TO TSAR                           
         BRAS  RE,CALLTSAR                                                      
         TM    T.TSERRS,TSEEOF     IF BUFFER FULL                               
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDREVEX J     EXXMOD                                                           
         DROP  R4                                                               
         LTORG                                                                  
****=========================================================*                  
**** FIND X'12' ELEMS FOR REVERSE OPTION                     *                  
****=========================================================*                  
***FINDCRCK NTR1  BASE=*,LABEL=*                                                
****                                                                            
***         MVI   FERN,0                                                        
****                                                                            
***         LA    R2,BLOCK                                                      
***         USING PAYBLKD,R2                                                    
***         XC    PAYGROSS(8),PAYGROSS                                          
***         MVI   PAYFUNCT,PAYELS                                               
***         MVI   PAYNELS,0                                                     
***         MVC   PAYAELS,AIOAREA3                                              
***         MVI   PAYSTAT,0                                                     
****                                                                            
***         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',NBAIO),0                    
***         CLI   12(R1),0            TEST IF ELEMENT FOUND                     
***         BE    *+6                                                           
***         DC    H'0'                ???                                       
***         L     R5,12(R1)                                                     
***         USING NUPAYD,R5                                                     
***FCR10    TM    NUPAYCR,X'30'       CRCK ?                                    
***         BZ    FCR50                                                         
*********************************************                                   
****AT 'TEST' FUNCITON IN NPAY PAYFLAGS IS NOT YET SET                          
****IT SHOULD BE SET BY THE 'CLEAR' FUNCTION                                    
****THIS ATTEMPTS TO HELP USER MATCH THE ORIGINAL CR/CK REQUEST                 
*********************************************                                   
*********************************************                                   
***         TM    PAYFLAGS,REVRSCR+REVRSCK                                      
***         BZ    FCR12                                                         
***         TM    NUPAYCR,X'10'       CK                                        
***         BNO   FCR11                                                         
***         TM    PAYFLAGS,REVRSCK                                              
***         BNO   FCR50                                                         
***         B     FCR12                                                         
****                                                                            
***FCR11    TM    NUPAYCR,X'20'       CR                                        
***         BNZ   *+6                                                           
***         DC    H'0'                ?                                         
***         TM    PAYFLAGS,REVRSCR                                              
***         BNO   FCR50                                                         
*********************************************                                   
***FCR12    CLC   CRCKDAT,NUPAYDAT    PAID DATE                                 
***         BNE   FCR50                                                         
****                                 IT'S A MATCH                               
***         ICM   R1,15,NUPAYGRS                                                
***         A     R1,PAYGROSS                                                   
***         ST    R1,PAYGROSS                                                   
***         ICM   R1,15,NUPAYNET                                                
***         A     R1,PAYNT                                                      
***         ST    R1,PAYNT                                                      
***         OI    PAYSTAT,PAYABLE                                               
****                                                                            
****        CREATE X'12' ELEMENT                                                
****                                                                            
***         ICM   RE,15,PAYAELS       GET POINTER TO ELEMENT AREA               
***         BNZ   *+6                                                           
***         DC    H'0'                BLOW UP - USER DID NOT SUPPLY ONE         
***         ZIC   R1,PAYNELS          INCREMENT ELEMENT COUNT                   
***         LA    R1,1(R1)                                                      
***         STC   R1,PAYNELS                                                    
***         BCTR  R1,0                                                          
***         MH    R1,=Y(NUPAYELN)     INDEX INTO ELEMENT AREA                   
***         LA    RE,0(R1,RE)         POINT TO NEXT ELEMENT POSITION            
****                                                                            
***         DROP  R5                                                            
***         USING NUPAYD,RE                                                     
***         XC    NUPAYD(NUPAYELN),NUPAYD                                       
***                                                                             
**** COPY X'12' AND REVERSE PREVIOUS PAID                                       
***         MVC   0(NUPAYELN,RE),0(R5)     COPY X'12'                           
***         MVC   NUPAYDAT,TODAYC          SET TODAY'S DATE                     
***         ICM   R1,15,NUPAYGRS              REVERSE PAID DOLLARS              
***         LPR   R1,R1                                                         
***         STCM  R1,15,NUPAYGRS                                                
***         ICM   R1,15,NUPAYNET                                                
***         LPR   R1,R1                                                         
***         STCM  R1,15,NUPAYNET                                                
****                                                                            
**** COPY X'12' AND SWITCH CR/CK                                                
***         ZIC   R1,PAYNELS          INCREMENT ELEMENT COUNT                   
***         LA    R1,1(R1)                                                      
***         STC   R1,PAYNELS                                                    
***         LA    RE,NUPAYELN(RE)     BUMP TO NEXT AVAILABLE SLOT               
***         MVC   0(NUPAYELN,RE),0(R5)     COPY  X'12'                          
***         MVC   NUPAYDAT,TODAYC          SET TODAY'S DATE                     
****                                                                            
***         TM    NUPAYCR,X'20'       CREDIT ?                                  
***         BNO   FCR30                                                         
***         NI    NUPAYCR,X'FF'-X'20'                                           
***         OI    NUPAYCR,X'10'                                                 
***         OI    NBPPDOPT,NBPPDCK                                              
***         OI    TSARFLG,TSFLCK                                                
***         B     FCR50                                                         
***FCR30    TM    NUPAYCR,X'10'       CK?                                       
***         BO    *+6                                                           
***         DC    H'0'                ???                                       
***         NI    NUPAYCR,X'FF'-X'10'                                           
***         OI    NUPAYCR,X'20'                                                 
***         OI    NBPPDOPT,NBPPDCR                                              
*********** OI    TSARFLG,TSFLCR                                                
***         B     FCR50                                                         
*                                                                               
***                                                                             
***FCR50    DS    0H                                                            
****                                                                            
***         ZIC   R1,1(R5)                                                      
***         AR    R5,R1                                                         
***         CLI   0(R5),X'12'                                                   
***         BE    FCR10                                                         
***         B     FCRX                                                          
****                                                                            
***FCRX     J     EXXMOD                                                        
***         DROP  RE,R2                                                         
*=========================================================*                     
                                                                                
*                                                                               
*=========================================================*                     
* VALIDATE 01/03/05 SEQUENCE IN CLRST                     *                     
*=========================================================*                     
VALCLRST NTR1  BASE=*,LABEL=*                                                   
         L     R5,AIOAREA2                                                      
         USING CLRSTATD,R5                                                      
         LA    R5,CLSELEMS-CLRSTATD(R5)  POINT TO FIRST ELEMENT                 
         DROP  R5                                                               
*                                                                               
         CLI   0(R5),X'01'         FIRST ELEM SHOULD BE 01                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
VCLR10   CLI   0(R5),0             END OF RECORD?                               
         BE    VCLRX                                                            
         CLI   0(R5),X'01'                                                      
         BE    VCLR20                                                           
         CLI   0(R5),X'03'                                                      
         BE    VCLR120                                                          
*                                                                               
VCLRNXT  ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     VCLR10                                                           
*                                                                               
         USING CLSTEL01,R5                                                      
VCLR20   TM    CLSTSTAT,X'02'      NEW CLRST 01/03/05 PAIRINGS?                 
         BO    VCLR30                                                           
         DROP  R5                                                               
*                                                                               
         LR    R4,R5                                                            
         ZIC   RF,1(R5)                                                         
         AR    R4,RF                                                            
*                                                                               
         CLI   0(R4),0                                                          
         BE    VCLRX                                                            
         CLI   0(R4),X'F1'                                                      
         BE    VCLRX                                                            
         CLI   0(R4),X'01'         NEXT ELEM SHOULD BE 01                       
         BE    VCLRNXT                                                          
         DC    H'00'                                                            
*                                                                               
VCLR30   DS    0H                  NEW 01 ELEMS                                 
         LR    R4,R5                                                            
         ZIC   RF,1(R5)                                                         
         AR    R4,RF                                                            
*                                                                               
         CLI   0(R4),X'03'         NEXT ELEM SHOULD BE 03                       
         BE    VCLRNXT                                                          
         DC    H'00'                                                            
*                                                                               
VCLR120  DS    0H                  FOUND AN 03                                  
         LR    R4,R5                                                            
         ZIC   RF,1(R5)                                                         
         AR    R4,RF                                                            
*                                                                               
         CLI   0(R4),X'05'         NEXT ELEM SHOULD BE 05                       
         BE    VCLRNXT                                                          
         DC    H'00'                                                            
*                                                                               
VCLRX    DS    0H                                                               
         J     EXXMOD                                                           
*******************************************************************             
*    UPDATE AUTOPAY RECORD WITH SUCCESS/ERROR                                   
*******************************************************************             
UPDAPY   NTR1  BASE=*,LABEL=*                                                   
         CLC   PAYMSG(10),=C'** PAID **'                                        
         BNE   UPDAPYX                                                          
*                                                                               
         MVC   TMPAKEY(32),APYKEY                                               
         GOTO1 VDATMGR,DMCB,(X'80',=C'DMRDHI'),=C'XSPDIR',APYKEY,      +        
               TMPAKEY,DMWORK                                                   
         CLC   TMPAKEY(32),APYKEY                                               
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R4,TMPAKEY                                                       
         USING NAPRECD,R4                                                       
         OI    NAPKCNTL,NAPKCPRC    MARK AUTOPAY RECORD PROCESSED               
         GOTO1 VDATMGR,DMCB,=C'DMWRT',=C'XSPDIR',TMPAKEY,TMPAKEY,      +        
               DMWORK                                                           
*                                                                               
         GOTO1 VDATMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL',TMPAKEY+36,  +        
               AIOAREA2,DMWORK                                                  
*                                                                               
         L     R4,AIOAREA2                                                      
         OI    NAPRCNTL,NAPRCPRC    MARK AUTOPAY RECORD PROCESSED               
         GOTO1 VDATCON,DMCB,(5,0),(2,NAPPAID)                                   
*                                                                               
UPDAP10  GOTO1 VDATMGR,DMCB,=C'PUTREC',=C'XSPFILE ',TMPAKEY+36,        +        
               AIOAREA2,DMWORK                                                  
*                                                                               
UPDAPYX  J     EXXMOD                                                           
         EJECT                                                                  
         DROP  R4                                                               
         LTORG                                                                  
*******************************************************************             
*  FOR AUTOAPPROVE ALL AMOUNT TYPES MUST MATCH                                  
*******************************************************************             
CHAMTTYP NTR1  BASE=*,LABEL=*                                                   
         LA    R0,CLRLINES                                                      
         LA    R6,AMTS                                                          
CHAMT020 TM    7(R6),X'40'          IS AMOUNT PRESENT                           
         BZ    CHAMT040                                                         
         MVC   BYTE,4(R6)           GET FIRST AMOUNT TYPE                       
         B     CHAMT100                                                         
CHAMT040 AHI   R6,8                                                             
         BCT   R0,CHAMT020                                                      
         B     CHAMTEX              NO AMOUNTS FOR THIS CLEARENCE               
*                                                                               
CHAMT100 LA    R0,CLRLINES                                                      
         LA    R6,AMTS                                                          
CHAMT120 TM    7(R6),X'40'          IS AMOUNT PRESENT                           
         BZ    CHAMT140                                                         
         CLC   BYTE,4(R6)           COMPARE TO FIRST AMOUNT TYPE                
         BNE   CHAMTERR             NOT EQUAL ERROR                             
CHAMT140 AHI   R6,8                                                             
         BCT   R0,CHAMT120                                                      
         B     CHAMTEX              NO AMOUNTS FOR THIS CLEARENCE               
*                                                                               
CHAMTEX  J     EXXMOD                                                           
*                                                                               
* ERROR MESSAGE AMOUNT TYPES NOT THE SAME                                       
CHAMTERR MVI   FERN,USERERR                                                     
         LA    R2,PAYACTH                                                       
         ST    R2,FADDR                                                         
         MVC   PAYMSG(L'AMTTYPER),AMTTYPER                                      
         GOTO1 VERROR                                                           
*                                                                               
AMTTYPER DC    C'MUST HAVE THE SAME AMOUNT TYPE FOR AUTO APPROVE'               
         EJECT                                                                  
*=========================================================*                     
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPAYWRK                                                       
         SPACE 2                                                                
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
HOOKREG  DS    A                   RETURN POINT TO NETIO                        
USERPAY  DS    C                                                                
INTPAY   DS    C                                                                
RECSW    DS    C                                                                
PASS     DS    X                                                                
STATSEQ  DS    X                                                                
OUTMSG   DS    CL80                                                             
         DS    0D                                                               
BLOCK    DS    CL256                                                            
*                                                                               
SVPASSWD DS    XL2                 PID                                          
SVAMTN   DS    F                   NET AMOUNT                                   
SVAMTG   DS    F                   GROSS AMOUNT                                 
REVGROSS DS    F                   REVERSE GROSS AMOUNT                         
REVNET   DS    F                   REVERSE NET AMOUNT                           
SVGROSSP DS    PL8                                                              
*                                                                               
TMPAKEY  DS    XL50                                                             
*                                                                               
MYFLAG   DS    XL1                                                              
GRSCALC  EQU   X'01'                                                            
CPYSPLIT EQU   X'02'                                                            
ADDCLR   EQU   X'04'                                                            
*                                                                               
VTSAR    DS    A                                                                
TSARREC  DS    XL18                                                             
*                                                                               
TMPINV   DS    CL10                                                             
SVAGYCOD DS    CL2                 OVERRIDE AGENCY CODE                         
SVOFFC   DS    CL2                 OFFICE CODE (OR OVERRIDE)                    
SVSEQNUM DS    XL1                 SEQUENCE NUMBER                              
*                                                                               
AUTOAPP  DS    CL1                 (Y)YES - USES AUTOAPPROVE                    
FDCRCKSW DS    CL1                 (Y)YES - USES CALLED FROM FINDCRCK           
TSARFLAG DS    XL1                 CK OR CR FLAG                                
*                                                                               
TMPGROSS DS    PL6                                                              
*                                                                               
INVA     DS    CL10                                                             
PRD1A    DS    CL3                                                              
PRD2A    DS    CL3                                                              
TMPAMT   DS    PL8                                                              
GRSAMT   DS    PL6                                                              
NETAMT   DS    PL6                                                              
ESTB     DS    XL1                                                              
CKCRFLAG DS    XL1                                                              
CKFLON   EQU   X'01'               CHECK                                        
CRFLON   EQU   X'02'               CREDIT                                       
*                                                                               
CMNT     DS    CL40                                                             
*                                                                               
INVTABLE DS    CL169               INVOICE TABLE FOR AUTO APP REVERSAL          
INVTBOFF DS    XL1                 CURRENT OFFSET TO INVOICE NUMBER             
**********************************************************************          
* REQUEST RECORD AREA FOR CLEARANCE REQUESTS                                    
**********************************************************************          
ZCTL     DS    CL26                                                             
ZAREA    DS    0CL160                                                           
ZTYPE    DS    CL2                                                              
ZAGY     DS    CL2                                                              
ZMED     DS    CL1                                                              
ZCLT     DS    CL3                                                              
ZPGR     DS    CL1                                                              
ZMGR     DS    CL1                                                              
         DS    CL1                                                              
         ORG   ZPGR                                                             
ZACCEST  DS    CL3                 ESTIMATE NUMBER FOR ACC CLEARANCE            
ZPRD     DS    CL3                                                              
ZMKT     DS    CL4                                                              
         ORG   ZMKT                FOR CHECK REQUESTS ONLY                      
ZSTATUS  DS    CL1                                                              
ZSTATUS_ONLINE EQU X'80'           ON-LINE POSTING TO ACC                       
ZSTATUS_AUTPAY EQU X'40'           GENERATED BY AUTOPAY (BEALS)                 
         ORG   ZMKT+L'ZMKT                                                      
ZSTA     DS    CL5                                                              
ZDATE    DS    CL6                                                              
ZEST     EQU   ZDATE                                                            
ZREP     DS    CL3                                                              
ZREPTYPE DS    CL1                                                              
ZAGYCODE DS    CL2                 OVERRIDE AGENCY CODE FOR ACC                 
ZCONT    DS    CL1                                                              
ZSEQNUM  DS    XL1                 CLEARANCE SEQUENCE NUMBER                    
ZSTART   DS    CL6                                                              
ZEND     DS    CL6                                                              
ZPRD2    DS    CL3                                                              
ZPRD3    DS    CL3                                                              
ZMODE    EQU   ZPRD3                                                            
*                                                                               
ZAMTTYPE DS    CL1                                                              
ZAMT     DS    PL5                                                              
*                                                                               
ZGSTCD   DS    CL1                                                              
ZGST     DS    XL4                                                              
*                                                                               
         ORG   ZAMT                                                             
ZICLAMT  DS    CL10                ICL STILL GETS EBCDIC                        
*                                                                               
ZOFFC    DS    CL2                 SPECIAL OFFICE CODE                          
ZUESTOR  DS    CL12                                                             
ZINV     EQU   ZUESTOR                                                          
*                                                                               
Z2AREA   DS    0CL80               REQUEST CARD EXTENSION                       
ZPST     DS    CL42                6 7-BYTE PST FIELDS                          
         ORG   ZPST                                                             
ZPSTPROV DS    CL2                 PST PROVINCE CODE                            
ZPSTCD   DS    CL1                 PST CODE                                     
ZPSTAMT  DS    PL4                 PST AMOUNT                                   
*                                                                               
         ORG   Z2AREA                                                           
ZCTADATA DS    5XL10               5 10-BYTE CTA FIELDS                         
         ORG   ZCTADATA                                                         
ZCTACON  DS    PL4                                                              
ZCTAGRS  DS    PL6                                                              
         ORG   Z2AREA+50                                                        
Z2GRSAMT DS    PL6                 GROSS AMOUNT                                 
Z2PID    DS    XL2                 PID NUMBER                                   
Z2AMT    DS    PL6                                                              
Z2FLAG   DS    XL1                                                              
Z2GCALC  EQU   X'80'               GROSS IS CALC FROM NET                       
ZSMED    DS    CL1                 SUB MEDIA                                    
Z2SPARE  DS    CL12                                                             
Z2CTA#   DS    CL1                                                              
Z2CTA    DS    CL1                 C'T' = CTA TRADE BUY                         
*                                                                               
*                                                                               
ZCOMMENT DS    0CL200                                                           
ZCOM1    DS    CL40                                                             
ZCOM2    DS    CL40                                                             
ZCOM3    DS    CL40                                                             
ZCOM4    DS    CL40                                                             
ZCOM5    DS    CL40                                                             
ZSPARE   DS    CL40                                                             
**********************************************************************          
*                                                                               
* SAVE AREA                                                                     
*                                                                               
SVDATAA  DS    0C                                                               
SVNBLOCK DS    XL(NEBLOCKL)                                                     
SVDATAAL EQU   *-SVDATA                                                         
         EJECT                                                                  
* EQUATES                                                                       
*                                                                               
KEYLEN   EQU   L'NUKEY+L'NUKSTAT+L'NUDA LENGTH OF DIRECTORY ENTRY               
*                                                                               
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDTSARD                                                        
*                                                                               
TSARRD   DSECT                                                                  
TSINDEX  DS    XL1                  INDEX TO INVOICE ON SCREEN                  
TSEST    DS    XL1                  ESTIMATE                                    
TSPRD3   DS    CL3                  ALPHA PRODUCT                               
TSGROSS  DS    PL6                  SUBTOTAL GROSS AMOUNT                       
TSNET    DS    PL6                  SUBTOTAL NET AMOUNT                         
TSFLAG   DS    XL1                                                              
TSFLCK   EQU   X'01'                 - CK                                       
TSFLCR   EQU   X'02'                 - CR                                       
*                                                                               
*                                                                               
       ++INCLUDE NEGENAPY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076NEPAY02   11/20/17'                                      
         END                                                                    
