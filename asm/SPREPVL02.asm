*          DATA SET SPREPVL02  AT LEVEL 116 AS OF 01/28/21                      
*PHASE SPVL02A                                                                  
*INCLUDE SPFMTINO                                                               
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE DDUCOM                                                                 
*INCLUDE NETBLRDR                                                               
*INCLUDE WIDE                                                                   
*INCLUDE USSIO                                                                  
*                                                                               
         TITLE 'SPREPVL02 - VENDOR LEVEL EDI BILLING'                           
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT SPEC-39025   01/28/21 NEW EDI PHD/Coupa for Canadian Tire      *         
* AKAT SPSUG-1667   09/10/20 GENERIC EDI                              *         
* AKAT SPEC-46256   05/20/20 CARRY TAXES FOR MULTI MONTH INVOICES     *         
* AKAT SPEC-43119   02/24/20 INCREASE OUTREC                          *         
* AKAT SPEC-41453   12/11/19 CHANGES FOR SCOTIABANK                   *         
* AKAT SPEC-24094   08/12/19 NEW EDI BILLING FOR PHD CANADA           *         
* AKAT SPEC-34644   04/18/19 CARRY OVER TAXES FOR BILLS W/MULTI MOS   *         
* AKAT SPEC-22855   04/11/18 ADD AGENCY O1 TO AGYTAB FOR QA REGRESSION*         
* AKAT SPEC-14682   09/07/17 NEW EDI BILLING MEDIA EXPERTS/BELL       *         
***********************************************************************         
* QOPT  VAL                        COMMENTS                           *         
* ----- --- --------------------------------------------------------- *         
* QOPT1  Y  WHATEVER IS IN THE PRODUCE FILE OPTION (BLANK=Y)          *         
* QOPT5  N  ONLY SET VIA JCL FOR TESTING - NO MQ MESSAGE              *         
* QOPT6  Y  ONLY SET VIA JCL FOR TESTING - TEST RUN (SKIP TAPE)       *         
* QOPT7  P  ONLY SET VIA JCL FOR TESTING - PRINT OUTPUT RECORDS       *         
***********************************************************************         
SPVL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPVL02,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DS    F                                                                
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUNFRST?                                     
         BE    RUNF                YES                                          
         CLI   MODE,REQFRST        REQFRST?                                     
         BE    REQF                YES                                          
         CLI   MODE,PROCBILL       PROCBILL?                                    
         BE    PROCBL              YES                                          
         CLI   MODE,CLTFRST        CLTFRST?                                     
         BE    CLTF                YES                                          
         CLI   MODE,CLTLAST        CLTLAST?                                     
         BE    CLTL                YES                                          
         CLI   MODE,RUNLAST        RUNLAST?                                     
         BE    RUNL                YES                                          
*                                                                               
EXIT     XIT1                      EXIT                                         
*                                                                               
RUNF     DS    0H                  RUNFRST                                      
*                                                                               
         LA    R0,(ACONSX-ACONS)/4 NUMBER OF ADDRESSES                          
         LA    R2,ACONS            RELOCATE ADDRESSES IN ACONS                  
         LA    RE,RCONS            NEW ADDRESSES IN RCONS                       
*                                                                               
RUNF5    L     RF,0(R2)            A(IN ACONS)                                  
         A     RF,RELO             ADD RELO                                     
         ST    RF,0(RE)            SAVE A(IN RCONS)                             
         LA    R2,4(R2)            NEXT ADDRESS IN ACONS                        
         LA    RE,4(RE)            NEXT ADDRESS IN RCONS                        
         BCT   R0,RUNF5            PROCESS NEXT ADDRESS                         
*                                                                               
         L     RF,VMASTC           A(MASTC)                                     
         USING MASTD,RF            MASTD DSECT                                  
         MVC   AMQRPT,MCVMQRPT     A(MQRPT)                                     
         DROP  RF                  DROP MASTD USING                             
*                                                                               
         MVI   OPENSW,C'N'         INIT TO TAPE NOT OPEN                        
         MVI   NETOPT,0            INIT TO SPOT                                 
*                                                                               
         L     R1,VMASTC           A(MASTC)                                     
         USING MASTD,R1            MASTD DSECT                                  
         CLI   MCNETPAK,C'Y'       NETPAK?                                      
         BNE   RUNF10              NO                                           
         MVI   NETOPT,C'N'         SET FOR NEW NETPAK                           
         DROP  R1                                                               
*                                                                               
         L     R4,ADBUY            A(I/O AREA)                                  
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',NUFLIST,(R4)                      
*                                                                               
RUNF10   MVI   FRSTSW,C' '         CLEAR FIRST TIME SWITCH                      
*                                                                               
         LA    R2,INVCNT           ZAP ACCUMULATORS                             
         LA    R3,4                4 ACCUMULATORS                               
*                                                                               
RUNF20   ZAP   0(4,R2),=P'0'       ZAP ACCUMULATOR                              
         LA    R2,4(R2)            BUMP TO NEXT ACCUMULATOR                     
         BCT   R3,RUNF20           ZAP NEXT ACCUMULATOR                         
*                                                                               
         ZAP   BILTOT,=P'0'        INIT BILLING $ ACCUMULATOR                   
         ZAP   DTLTOT,=P'0'        INIT DETAIL $ ACCUMULATOR                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,CTODAY) TODAY - YYYYMMDD                   
         TIME  DEC                 R0=HHMMSSHH                                  
*                                                                               
         ST    R0,FULL             R0 NOW HAS TIME HHMMSSHH (PWOS)              
         SRL   R0,4                GET JUST HHMMSS                              
         ST    R0,WORD             SAVE OFF HHMMSS                              
         XC    DUB,DUB             CLEAR DUB                                    
         MVC   DUB+5(3),WORD       HHMMSS                                       
         OI    DUB+7,X'0F'         PREPARE FOR CVB                              
         CVB   R5,DUB              HHMMSS                                       
         EDIT  (R5),(5,TIMEOFD),2,FILL=0                                        
         MVI   TIMEOFD+5,C'.'      HH.MM                                        
         UNPK  WORK(3),FULL+2(2)   GET SECONDS                                  
         MVC   TIMEOFD+6(2),WORK   HH.MM.SS                                     
*                                                                               
         TIME  DEC,ZONE=UTC        R0 = HHMMSSHH                                
         ST    R0,FULL             R0 NOW HAS TIME HHMMSSHH (PWOS)              
         SRL   R0,4                GET JUST HHMMSS                              
         ST    R0,WORD             SAVE OFF HHMMSS                              
         XC    DUB,DUB             CLEAR DUB                                    
         MVC   DUB+5(3),WORD       HHMMSS                                       
         OI    DUB+7,X'0F'         PREPARE FOR CVB                              
         CVB   R5,DUB              HHMMSS                                       
         EDIT  (R5),(5,TIMEOFDU),2,FILL=0                                       
         MVI   TIMEOFDU+5,C'.'     HH.MM                                        
         UNPK  WORK(3),FULL+2(2)   GET SECONDS                                  
         MVC   TIMEOFDU+6(2),WORK  HH.MM.SS                                     
*                                                                               
         MVI   NOFILE,C'N'         INIT NOFILE                                  
         MVI   SVQOPT1,0           INIT SVQOPT1                                 
*                                                                               
         B     EXIT                EXIT                                         
*                                                                               
REQF     DS    0H                  REQFRST                                      
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE HEADLINES TO PRINT                     
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         MVI   DMOUTBTS,X'FD'      PASS DELETED RECORDS                         
*                                                                               
         LAY   R6,AGYTAB           GET TAPE SPEC FROM AGYTAB                    
         USING AGYTABD,R6          AGENCY SPEC DSECT                            
*                                                                               
         CLI   QOPT1,C'G'          PRODUCE FILE = G?                            
         BNE   REQF10              NO                                           
         LAY   R6,GENTABLE         POINT TO GENERIC SPOT TABLE                  
         B     REQF20              GENERIC ROUTINES FOR ALL AGENCIES            
*                                                                               
REQF10   CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    REQFERR1            YES - GIVE ERROR MESSAGE AND EXIT            
         CLC   QAGY,AGYTAGY        MATCH ON AGENCY?                             
         BNE   REQF15              NO - BUMP TO NEXT TABLE ENTRY                
         CLC   QOPT1,AGYTAPE       MATCH ON FILE TYPE? BLANK IS NORMAL          
         BE    REQF20              YES                                          
*                                                                               
REQF15   LA    R6,AGYTABL(R6)      BUMP TO NEXT ENTRY                           
         B     REQF10              CHECK NEXT ENTRY                             
*                                                                               
REQF20   OC    AGYBLKS,AGYBLKS     NULL BLKSIZE?                                
         BNZ   *+8                 NO                                           
         MVI   QOPT6,C'Y'          YES - SKIP DYNALLOC & OPEN                   
         CLI   SVQOPT1,0           FIRST TIME IN?                               
         BE    *+14                YES                                          
         CLC   QOPT1,SVQOPT1       "PRODUCE FILE" OPT MATCHES?                  
         BNE   REQFERR2            NO - GIVE ERROR MESSAGE AND EXIT             
         MVC   SVQOPT1,QOPT1       SAVE "PRODUCE FILE" OPTION                   
         MVC   SVQOPT6,QOPT6       SAVE QOPT6 IT'S OUT OF SCOPE AT RUNL         
         MVC   SVQOPT7,QOPT7       SAVE QOPT7 IT'S OUT OF SCOPE AT RUNL         
*                                                                               
         MVI   MQMSG,C'Y'          DEFAULT IS TO SEND MQ NOTIFICATION           
         CLI   QOPT5,C'N'          SUPPRESS MQ NOTIFICATION?                    
         BNE   *+8                 NO                                           
         MVI   MQMSG,C'N'          YES - SUPPRESS MQ NOTIFICATION               
*                                                                               
         CLI   OPENSW,C'N'         DID WE OPEN THE TAPE ALREADY?                
         BNE   REQF25              YES - ALL FIELDS ALREADY SET                 
*                                                                               
         XR    R0,R0               CLEAR R0                                     
         ICM   R0,3,AGYSKEYL       SORTCARD KEY LENGTH                          
         LAY   RE,SRTCDLEN         SORTCARD KEY LENGTH                          
         EDIT  (R0),(2,0(RE)),ALIGN=LEFT                                        
*                                                                               
         XR    R0,R0               CLEAR R0                                     
         ICM   R0,3,AGYSRECL       RECCARD RECORD LENGTH                        
         LAY   RE,RECCDLEN         RECCARD RECORD LENGTH                        
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT                                        
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD  INITIALIZE SORT                   
*                                                                               
         LAY   R0,OUTRECLN         200k AOUTREC BUFFER                          
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'GET',(R0),(R0)                                 
*                                                                               
         ICM   RE,15,4(R1)         DID WE GET STORAGE?                          
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
*                                                                               
         MVC   0(12,RE),=C'** OUTREC **'                                        
         LA    RE,16(RE)           LEAVE 4 BYTES FOR LENGTH                     
         ST    RE,AOUTREC          A(OUTREC)                                    
*                                                                               
         L     RF,AGYBRECL         BINSRCH RECORD LENGTH                        
         M     RE,AGYBMAXR         BINSRCH NUMBER OF RECORDS                    
         AHI   RF,8                RF = (REC LEN * NUM RECS) + 8                
         LR    R0,RF               R0 = (REC LEN * NUM RECS) + 8                
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'GET',(R0),(R0)                                 
*                                                                               
         ICM   RE,15,4(R1)         DID WE GET STORAGE?                          
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
*                                                                               
         XR    R0,R0               P1 = BINSRCH KEY                             
         LR    R1,RE               P2 = A(BINSRCH TABLE)                        
         XR    R2,R2               P3 = NUMBER OF RECORDS IN TABLE              
         L     R3,AGYBRECL         P4 = RECORD LENGTH (L'KEY+L'REC)             
         L     R4,AGYBKEYL         P5 = KEY LENGTH                              
         L     R5,AGYBMAXR         P6 = MAX NUMBER OF RECS IN TABLE             
         STM   R0,R5,ABINKEY       INIT BINSRCH PARMS                           
*                                                                               
         LA    R0,4                VBILLR, VBILHR, VUNITR & VOUTPR              
         LA    R2,ABILLR           ADDRESSES IN AGYTABD                         
         LA    RE,VBILLR           RELOCATED ADDRESSES IN RCONS                 
*                                                                               
REQF21   L     RF,0(R2)            A(IN ACONS)                                  
         A     RF,RELO             ADD RELO                                     
         ST    RF,0(RE)            SAVE A(IN RCONS)                             
         LA    R2,4(R2)            NEXT ADDRESS IN ACONS                        
         LA    RE,4(RE)            NEXT ADDRESS IN RCONS                        
         BCT   R0,REQF21           PROCESS NEXT ADDRESS                         
*                                                                               
         LA    RF,SVLTAPE          SVLTAPE                                      
         A     RF,RELO             ADD RELO                                     
         ST    RF,ASVLTAPE         SAVE A(SVLTAPE)                              
*                                                                               
         MVC   RCSUBPRG,AGYSSPEC   SET FOR SPOT FOR SSPEC IN SPREPVL01          
         CLI   NETOPT,C'N'         NETWORK REQUEST?                             
         BNE   *+10                NO                                           
         MVC   RCSUBPRG,AGYNSPEC   SET FOR NET FOR SSPEC IN SPREPVL01           
*                                                                               
         MVC   SAVEAGY,QAGY        SAVE OFF AGENCY ALPHA                        
*                                                                               
         MVI   OPENSW,C'Y'         SET TAPE OPEN                                
*                                                                               
         CLI   QOPT6,C'Y'          SKIP TAPE?                                   
         BE    REQF30              YES - SKIP DYNALLOC & OPEN                   
*                                                                               
         LAY   R5,SVLTAPE          A(DCB FOR TAPE)                              
         USING IHADCB,R5           DCB DSECT                                    
         OI    DCBRECFM,DCBRECBR   BLOCKED RECORDS                              
         CLI   AGYRECFM,C'F'       FIXED RECORD LENGTH?                         
         BNE   *+12                NO                                           
         OI    DCBRECFM,DCBRECF    RECFM=FB (FIXED BLOCK)                       
         B     *+8                 SET TO FIXED BLOCK                           
         OI    DCBRECFM,DCBRECV    RECFM=VB (VARIABLE BLOCK)                    
         MVC   DCBBLKSI,AGYBLKS    BLKSIZE= (BLOCK SIZE)                        
         MVC   DCBLRECL,AGYLRCL    LRECL= (RECORD SIZE)                         
*                                                                               
         LA    R3,NEDYNDSN         NETPAK TAPE DEFINITION                       
         CLI   NETOPT,C'N'         NETWORK REQUEST?                             
         BE    *+8                 YES                                          
         LA    R3,SPDYNDSN         NO - SET TO SPOTPAK TAPE DEF                 
         MVC   13(2,R3),QAGY       AGENCY                                       
         MVC   15(1,R3),AGYSUFX    SUFFIX (DEFAULT IS 1)                        
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,=C'SVLTAPE '),(0,0(R3))                         
*                                                                               
         OPEN  ((R5),OUTPUT)       OPEN THE TAPE                                
         B     REQF30              SAVE OFF AGENCY TAPE SPECS                   
         DROP  R5                  DROP DCB USING                               
*                                                                               
REQF25   CLI   QOPT6,C'Y'          SKIP TAPE?                                   
         BE    REQF30              YES - SKIP DYNALLOC & OPEN                   
         LAY   R5,SVLTAPE          A(DCB FOR TAPE)                              
         USING IHADCB,R5           DCB DSECT                                    
         CLC   DCBBLKSI,AGYBLKS    SAME BLOCK SIZE?                             
         BNE   REQFERR2            NO - GIVE ERROR MESSAGE AND EXIT             
         CLC   DCBLRECL,AGYLRCL    RECORD SIZE?                                 
         BNE   REQFERR2            NO - GIVE ERROR MESSAGE AND EXIT             
         CLC   QOPT1,SVQOPT1       "PRODUCE FILE" OPT MATCHES?                  
         BNE   REQFERR2            NO - GIVE ERROR MESSAGE AND EXIT             
         DROP  R5                  DROP DCB USING                               
*                                                                               
REQF30   CLC   =C'ALL',QEST        ALL ESTIMATE REQUEST?                        
         BE    *+14                YES                                          
         CLC   QEST(3),SPACES      ESTIMATE FIELD BLANKS?                       
         BNE   *+10                NO                                           
         MVC   QEST(6),=C'001255'  YES - CHANGE TO 1-255                        
*                                                                               
         MVC   MYSTART,QSTART      SAVE QSTART                                  
         MVC   MYEND,QEND          SAVE QEND                                    
         GOTO1 DATCON,DMCB,QSTART,(3,MYSTRTB)                                   
         GOTO1 DATCON,DMCB,QEND,(3,MYENDB)                                      
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QEND,(2,BQENDP)                                      
*                                                                               
         MVC   QSTART,SPACES       CHANGE QSTART TO SPACES                      
         MVC   QEND,SPACES         CHANGE QEND TO SPACES                        
*                                                                               
         B     EXIT                EXIT                                         
*                                                                               
REQFERR1 LAY   RE,NOTAPSPC         NO TAPE SPECS ERROR MESSAGE                  
         MVC   P(L'NOTAPSPC),0(RE) MOVE THE ERROR MESSAGE TO P1                 
         B     REQFERRX            PRINT THE ERROR MESSAGE & END JOB            
*                                                                               
REQFERR2 LAY   RE,MULTMSG          MULTIPLE TAPE SPECS ERROR MESSAGE            
         MVC   P(L'MULTMSG),0(RE)  MOVE THE ERROR MESSAGE TO P1                 
         B     REQFERRX            PRINT THE ERROR MESSAGE & END JOB            
*                                                                               
REQFERRX GOTO1 REPORT              PRINT THE ERROR MESSAGE LINE 1               
         LAY   RE,JOBEND           MAKE IT CLEAR THAT JOB IS TERMINATED         
         MVC   P(L'JOBEND),0(RE)   AND EDIT FILE IS NOT GENERATED               
         GOTO1 REPORT              PRINT THE ERROR MESSAGE LINE 2               
         MVI   NOFILE,C'Y'         DO NOT GENERATE FILE!                        
         GOTO1 AENDREQ             END THE REPORT                               
*                                                                               
PROCBL   GOTO1 APRBILL,DMCB,(RC)   PROCESS BILL RECORD                          
         B     EXIT                EXIT                                         
*                                                                               
CLTF     DS    0H                  CLTFRST                                      
*                                                                               
         XC    B1PROF,B1PROF       CLEAR B1 PROFILE AREA                        
         XC    B1XPROF,B1XPROF     CLEAR B1X PROFILE AREA                       
         XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK(4),=C'SOB1'    READ B1 PROFILE                              
         MVC   WORK+4(2),QAGY      AGENCY                                       
         MVC   WORK+6(1),MED       MEDIA                                        
         MVC   WORK+7(3),CLT       CLIENT                                       
*                                                                               
         L     RF,ADCLT            A(CLIENT RECORD)                             
         USING CLTHDR,RF           CLIENT RECORD DSECT                          
         CLI   COFFICE,C' '        HAVE A CLIENT OFFICE?                        
         BNH   *+14                NO                                           
         MVI   WORK+10,C'*'        OFFICE CODE                                  
         MVC   WORK+11(1),COFFICE  CLIENT OFFICE CODE                           
         DROP  RF                  DROP CLIENT RECORD USING                     
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
*                                                                               
         MVC   WORK(4),=C'SB1X'    SB1X PROFILE                                 
         NI    WORK,X'FF'-X'40'    MAKE SYSTEM LOWER CASE                       
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         B     EXIT                EXIT                                         
*                                                                               
CLTL     DS    0H                  CLTLAST                                      
*                                                                               
         CLI   NETOPT,C'N'         NETPAK?                                      
         BE    CLTL5               YES - GO READ UNITS                          
*                                                                               
         CLC   QPRD,=C'ALL'        ALL PRODUCT REQUEST?                         
         BE    *+14                YES - CLEAR BPRD                             
         CLC   QPRD,=C'   '        ALL PRODUCT REQUEST?                         
         BNE   *+8                 NO                                           
         MVI   BPRD,0              CLEAR BPRD                                   
*                                                                               
         GOTO1 ARNBILL,DMCB,(RC)   PROCESS X'0E01' STATION BUCKETS              
         B     CLTL10              DONE                                         
*                                                                               
CLTL5    GOTO1 APROCNET,DMCB,(RC)  PROCESS UNITS                                
*                                                                               
CLTL10   B     EXIT                EXIT                                         
*                                                                               
RUNL     DS    0H                  RUNLAST - FILE IS PROCESSED HERE             
*                                                                               
         CLI   NOFILE,C'Y'         SUPPRESS FILE?                               
         BNE   RUNL05              NO                                           
         CLI   OPENSW,C'Y'         DID WE OPEN THE TAPE?                        
         BNE   RUNLXIT             NO - DON'T PROCESS ANYTHING!                 
         GOTO1 VSORTER,DMCB,=C'END'                                             
         B     RUNL50              CLOSE TAPE BEFORE EXITING                    
*                                                                               
RUNL05   L     RF,VOUTPR           A(AGY SPECIFIC OUTPUT ROUTINE)               
         GOTO1 (RF),DMCB,(RC)      PROCESS AGY SPECIFIC OUTPUT ROUTINE          
*                                                                               
         GOTO1 VSORTER,DMCB,=C'END'                                             
*                                                                               
         GOTO1 REPORT              FORCE LINE BREAK                             
*                                                                               
         LA    R4,TITLES           REPORT TOTALS                                
         LA    R5,INVCNT           REPORT TOTAL ACCUMULATORS                    
         LA    R3,4                FOR BCT                                      
*                                                                               
RUNL10   MVC   P1+7(19),0(R4)      LITERAL TO PRINT                             
         EDIT  (P4,0(R5)),(9,P1+26),0,COMMAS=YES                                
*                                                                               
         GOTO1 REPORT              PRINT TOTAL LINE                             
*                                                                               
         LA    R4,19(R4)           NEXT LITERAL TO PRINT                        
         LA    R5,4(R5)            NEXT ACCUMULATOR                             
         BCT   R3,RUNL10           PROCESS NEXT LITERAL/ACCUMULATOR             
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P1+7(08),=C'INVOICES'                                            
*                                                                               
         LA    R4,P1+18                                                         
         CP    BILTOT,=P'0'                                                     
         BL    RUNL20                                                           
         MVI   0(R4),C'$'                                                       
         LA    R4,1(R4)                                                         
         EDIT  (P8,BILTOT),(14,0(R4)),2,COMMAS=YES,ALIGN=LEFT                   
         B     RUNL25                                                           
*                                                                               
RUNL20   MVC   0(2,R4),=C'($'                                                   
         LA    R4,2(R4)                                                         
         EDIT  (P8,BILTOT),(14,0(R4)),2,COMMAS=YES,ALIGN=LEFT                   
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVI   0(R4),C')'                                                       
*                                                                               
RUNL25   GOTO1 REPORT                                                           
*                                                                               
         MVC   P1+7(07),=C'DETAILS'                                             
*                                                                               
         LA    R4,P1+18                                                         
         CP    DTLTOT,=P'0'                                                     
         BL    RUNL30                                                           
         MVI   0(R4),C'$'                                                       
         LA    R4,1(R4)                                                         
         EDIT  (P8,DTLTOT),(14,0(R4)),2,COMMAS=YES,ALIGN=LEFT                   
         B     RUNL35                                                           
*                                                                               
RUNL30   MVC   0(2,R4),=C'($'                                                   
         LA    R4,2(R4)                                                         
         EDIT  (P8,DTLTOT),(14,0(R4)),2,COMMAS=YES,ALIGN=LEFT                   
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVI   0(R4),C')'                                                       
*                                                                               
RUNL35   GOTO1 REPORT                                                           
*                                                                               
         CP    BILTOT,DTLTOT                                                    
         BE    RUNL40                                                           
         GOTO1 REPORT                                                           
         MVC   P1+1(41),=C'*****************************************'           
         MVC   P2+1(41),=C'* WARNING-INVOICE AND DETAIL $ MISMATCH *'           
         MVC   P3+1(41),=C'*****************************************'           
*                                                                               
RUNL40   GOTO1 REPORT                                                           
*                                                                               
RUNL50   CLI   SVQOPT6,C'Y'        TEST RUN ?                                   
         BE    RUNLXIT             YES - NO CLOSE                               
*                                                                               
         CLOSE SVLTAPE             CLOSE THE TAPE                               
*                                                                               
RUNLXIT  B     EXIT                DONE                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
NUFLIST  DC    CL8'NUNTFIL'        UNITFIL DEFINITION                           
         DC    CL8'NUNTDIR'        UNITDIR DEFINITION                           
         DC    CL10'X'             "X" PADDED                                   
*                                                                               
ACONS    DS    0F                  A(RELOCATED ADDRESSES)                       
         DC    A(PRBILL)           A(PRBILL)                                    
         DC    A(STABUCKC)         A(STABUCKC)                                  
         DC    V(SPFMTINO)         V(SPFMTINO)                                  
         DC    A(PROCNET)          A(PROCNET)                                   
         DC    A(NETBLK)           A(NETBLK)                                    
         DC    A(RNBILL)           A(RNBILL)                                    
         DC    V(SORTER)           V(SORTER)                                    
         DC    V(BINSRCH)          V(BINSRCH)                                   
         DC    A(TAPEWRT)          A(TAPEWRT)                                   
         DC    V(DDUCOM)           V(DDUCOM)                                    
         DC    V(WIDE)             V(WIDE)                                      
         DC    A(MQMESSGE)         V(MQMESSGE)                                  
ACONSX   EQU   *                   END OF ACONS                                 
*                                                                               
SPDYNDSN DC    CL20'SPTTAPE.SP0VLAG1'                                           
NEDYNDSN DC    CL20'NETTAPE.NE0VLAG1'                                           
*                                                                               
NOTAPSPC DC    C'**NO TAPE SPECS FOUND - REQ BYPASSED**'                        
MULTMSG  DC    C'**MULTIPLE TAPE DESCRIPTIONS IN ONE JOB - BYPASSED**'          
JOBEND   DC    C'**JOB TERMINATED - EDI FILE NOT GENERATED**'                   
*                                                                               
TITLES   DS    0C                                                               
         DC    CL19'INVOICES'                                                   
         DC    CL19'LINE ITEMS'                                                 
         DC    CL19'FILE HEADERS'                                               
         DC    CL19'FILE RECORDS OUTPUT'                                        
*                                                                               
AGYTAB   DS    0D                                                               
*                                                                               
*        MSNYA - UNILEVER - SPEC-11845                                          
*                                                                               
*&&DO                                                                           
         DC    C'H7'               AGENCY                                       
         DC    C'Y'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'01'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'02'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(6050)           DCB/TAPE - BLKSIZE=                          
         DC    AL2(6050)           DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SORTKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SORTRLEN)       SORTER - RECCDLEN                            
         DC    A(ULBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(ULBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(ULBILLR)          BILL ROUTINE                                 
         DC    A(ULBILHR)          BILL HEADER ROUTINE                          
         DC    A(ULUNITR)          UNIT ROUTINE                                 
         DC    A(ULOUTPR)          OUTPUT ROUTINE                               
*&&                                                                             
*                                                                               
*        M2TOA - BELL - SPEC-14682                                              
*                                                                               
         DC    C'U#'               AGENCY                                       
         DC    C'Y'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'00'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'00'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(449)            DCB/TAPE - BLKSIZE=                          
         DC    AL2(449)            DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTBKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTBRLEN)       SORTER - RECCDLEN                            
         DC    A(BLBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(BLBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(BLBILLR)          BILL ROUTINE                                 
         DC    A(BLBILHR)          BILL HEADER ROUTINE                          
         DC    A(0)                NO UNIT ROUTINE                              
         DC    A(BLOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        TCH1 - BELL - SPEC-14682                                               
*                                                                               
         DC    C'T1'               AGENCY                                       
         DC    C'Y'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'00'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'00'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(449)            DCB/TAPE - BLKSIZE=                          
         DC    AL2(449)            DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTBKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTBRLEN)       SORTER - RECCDLEN                            
         DC    A(BLBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(BLBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(BLBILLR)          BILL ROUTINE                                 
         DC    A(BLBILHR)          BILL HEADER ROUTINE                          
         DC    A(0)                NO UNIT ROUTINE                              
         DC    A(BLOUTPR)          OUTPUT ROUTINE                               
*&&DO                                                                           
*                                                                               
*        TCH1O- BELL - SPEC-14682                                               
*                                                                               
         DC    C'O1'               AGENCY                                       
         DC    C'Y'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'00'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'00'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(449)            DCB/TAPE - BLKSIZE=                          
         DC    AL2(449)            DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTBKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTBRLEN)       SORTER - RECCDLEN                            
         DC    A(BLBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(BLBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(BLBILLR)          BILL ROUTINE                                 
         DC    A(BLBILHR)          BILL HEADER ROUTINE                          
         DC    A(0)                NO UNIT ROUTINE                              
         DC    A(BLOUTPR)          OUTPUT ROUTINE                               
*&&                                                                             
*                                                                               
*        PHDTO - SPEC-24094                                                     
*                                                                               
         DC    C'OU'               AGENCY                                       
         DC    C'Y'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'01'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'00'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBILHR)          BILL HEADER ROUTINE                          
         DC    A(0)                NO UNIT ROUTINE                              
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        LETO - SPEC-24094                                                      
*                                                                               
         DC    C'FG'               AGENCY                                       
         DC    C'Y'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'01'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'00'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBILHR)          BILL HEADER ROUTINE                          
         DC    A(0)                NO UNIT ROUTINE                              
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        TCH1O  - SPEC-24094                                                    
*                                                                               
         DC    C'O1'               AGENCY                                       
         DC    C'Y'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'01'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'00'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBILHR)          BILL HEADER ROUTINE                          
         DC    A(0)                NO UNIT ROUTINE                              
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        PHDTO - SPEC-39025                                                     
*                                                                               
         DC    C'OU'               AGENCY                                       
         DC    C'C'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'04'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'00'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBILHR)          BILL HEADER ROUTINE                          
         DC    A(0)                NO UNIT ROUTINE                              
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        LETO - SPEC-39025                                                      
*                                                                               
         DC    C'FG'               AGENCY                                       
         DC    C'C'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'04'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'00'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBILHR)          BILL HEADER ROUTINE                          
         DC    A(0)                NO UNIT ROUTINE                              
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        TCH1O  - SPEC-39025                                                    
*                                                                               
         DC    C'O1'               AGENCY                                       
         DC    C'C'                TAPE CODE (BLANK IF NO TAPE CODE)            
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'04'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'00'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBILHR)          BILL HEADER ROUTINE                          
         DC    A(0)                NO UNIT ROUTINE                              
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        ALL AGENCIES - GENERIC EDI - SPSUG-1667                                
*                                                                               
GENTABLE DC    C'  '               ALL AGENCIES                                 
         DC    C'G'                TAPE CODE G FOR GENERIC EDI                  
         DC    C'1'                SUFFIX = 1                                   
         DC    X'01'               SPEC/RCSUBPRG FOR SPOT                       
         DC    X'02'               SPEC/RCSUBPRG FOR NET                        
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTGKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTGRLEN)       SORTER - RECCDLEN                            
         DC    A(GEBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(GEBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(GNBILLR)          BILL ROUTINE                                 
         DC    A(GNSTABR)          STATION BUCKET ROUTINE                       
         DC    A(GNUNITR)          UNIT ROUTINE                                 
         DC    A(GNOUTPR)          OUTPUT ROUTINE                               
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
SVLTAPE  DCB   DDNAME=SVLTAPE,DSORG=PS,MACRF=PM                                 
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,XX,A),FORMAT=BI'                             
         ORG   SORTCARD                                                         
         DS    CL15                                                             
SRTCDLEN DS    CL2                                                              
         ORG                                                                    
*                                                                               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XX '                                   
         ORG   RECCARD                                                          
         DS    CL21                                                             
RECCDLEN DS    CL2                                                              
         ORG                                                                    
*                                                                               
         TITLE 'RNBILL - READ SPOT BILLING BUCKET RECORDS'                      
*                                                                               
RNBILL   CSECT                                                                  
         NMOD1 0,RNBILL                                                         
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         MVC   KEY1,KEY            SAVE OFF THE CLIENT KEY                      
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R6,KEY              R6 = KEY                                     
         USING STABUCKD,R6         STATION BUCKET KEY DSECT                     
         MVC   STABKCOD,=X'0E01'   RECORD TYPE = X'0E01'                        
         MVC   STABKAM,BAGYMD      AGENCY/MEDIA                                 
         MVC   STABKCLT,BCLT       CLIENT                                       
         CLI   BPRD,0              MULTI-PRODUCT REQUEST?                       
         BE    RNB10               YES - SKIP REST OF KEY                       
         MVC   STABKPRD,BPRD       PRODUCT                                      
*                                                                               
RNB05    MVC   STABKEST,BEST       ESTIMATE                                     
         XC    STABKMKT(6),STABKMKT MAKE SURE KEY IS CLEAR AFTER EST            
*                                                                               
RNB10    GOTO1 HIGH                READ HIGH                                    
         B     RNB20               GO TEST KEY                                  
*                                                                               
RNB15    GOTO1 SEQ                 READ SEQ                                     
*                                                                               
RNB20    CLC   STABKCOD(5),KEYSAVE SAME AGENCY/MEDIA & CLIENT?                  
         BNE   RNBX                NO - DONE                                    
         CLI   BPRD,0              MULTI-PRODUCT REQUEST?                       
         BE    RNB25               YES - WE WANT ALL PRODUCTS                   
         CLC   STABKPRD,BPRD       PRODUCTS MATCH?                              
         BNE   RNBX                NO - DONE                                    
*                                                                               
RNB25    CLI   BEST,0              MULTI-ESTIMATE REQUEST?                      
         BE    RNB30               YES - WE WANT ALL ESTIMATES                  
         CLC   STABKEST,BEST       ESTIMATES MATCH?                             
         BE    RNB30               YES - PROCESS THIS RECORD                    
         BL    RNB05               IF LESS THEN SET ESTIMATE AND READHI         
         CLI   BESTEND,0           HAVE AN ESTIMATE RANGE?                      
         BE    *+14                NO                                           
         CLC   KEY+6(1),BESTEND    ESTIMATE IN KEY <= ESTIMATE END?             
         BNH   RNB30               YES - PROCESS THIS RECORD                    
         CLI   BPRD,0              PRODUCT SPECIFIC REQUEST?                    
         BNE   RNBX                YES - PRD & EST DOESN'T MATCH - DONE         
         LLC   RF,KEY+5            MULTI-PRD REQUEST - GET CURRENT PRD          
         LA    RF,1(RF)            BUMP TO NEXT PRODUCT                         
         STC   RF,KEY+5            TRY FOR THIS PRODUCT                         
         B     RNB05               SET THE ESTIMATE AND READ HIGH               
*                                                                               
RNB30    L     R7,ADSTABUC         A(STATION BUCKET RECORD)                     
         ST    R7,AREC             STORE ADDRESS IN AREC                        
*                                                                               
         GOTO1 GET                 GET THE STATION BUCKET RECORD                
*                                                                               
         L     RF,VCLIST           COMBINED CLIST AND CLIST2                    
*                                                                               
RNB35    CLC   STABKPRD,3(RF)      MATCH ON BINARY PRODUCT?                     
         BE    RNB40               YES                                          
         LA    RF,4(RF)            NO - BUMP TO NEXT PRODUCT IN VLIST           
         CLI   0(RF),0             END OF LIST?                                 
         BNE   RNB35               NO - PROCESS NEXT PRODUCT                    
         DC    H'0'                MUST FIND THE PRODUCT                        
*                                                                               
RNB40    MVC   SAVEPRD,0(RF)       SAVE 3-CHARACTER PRODUCT CODE                
*                                                                               
         MVI   ELCODE,X'0E'        PROCESS ALL BILLING ELEMENTS                 
         BAS   RE,GETEL            HAVE A BILLING ELEMENT?                      
         B     *+8                 GO TEST CONDITION CODE                       
*                                                                               
RNB45    BAS   RE,NEXTEL           HAVE ANOTHER BILLING ELEMENT?                
         BNE   RNB15               NO - READ NEXT STATION BUCKET RECORD         
*                                                                               
         USING STABELEM,R7         BILLING ELEMENT DSECT                        
         CLC   STABBDT,BQSTARTP    BILLING BEFORE REQ START DATE?               
         BL    RNB45               YES - PROCESS NEXT BILLING ELEMENT           
         CLC   STABBDT,BQENDP      BILLING AFTER REQ END DATE?                  
         BH    RNB45               YES - PROCESS NEXT BILLING ELEMENT           
*                                                                               
         L     RF,VBILHR           A(AGY SPECIFIC BILL HEADER ROUTINE)          
         GOTO1 (RF),DMCB,(RC)      PROCESS AGY SPECIFIC BILLHDR ROUTINE         
         B     RNB45               PROCESS NEXT BILLING ELEMENT                 
*                                                                               
RNBX     MVC   KEY,KEY1            SPONSOR'S KEY                                
*                                                                               
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
*                                                                               
         XIT1                      EXIT                                         
         DROP  R6,R7               DROP STATION BUCKET KEY/REC USINGS           
*                                                                               
         GETEL R7,24,ELCODE        GETEL MACRO                                  
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'ULBILHR - PROCESS STA BUCKET ELEMENT FOR UNILEVER'              
ULBILHR  CSECT                                                                  
         NMOD1 0,ULBILHR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
         USING STABUCKD,R6         STATION BUCKET KEY DSECT                     
         USING STABELEM,R7         BILLING ELEMENT DSECT                        
*                                                                               
         LA    R5,BINKEY           R5 = BINSRCH KEY                             
         USING ULBKEYD,R5          BILL BINSRCH KEY DSECT                       
         XC    BINKEY,BINKEY       CLEAR BINSRCH KEY                            
         MVC   ULBAM,STABKAM       AGENCY/MEDIA                                 
         MVC   ULBCLT,STABKCLT     PACKED CLIENT                                
         MVC   ULBPRD,SAVEPRD      3-CHAR PRODUCT                               
         MVC   ULBEST,STABKEST     BINARY ESTIMATE                              
         MVC   ULBINVYM,STABBDT    DATE OF BILLING COMPRESSED                   
         MVC   ULBINVN,STABINV     PACKED INVOICE NUMBER                        
         NI    ULBINVN,X'FF'-X'C0' STRIP REVERSED & REVERSAL FLAGS              
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         ST    R5,ABINKEY          A(BINSRCH KEY)                               
         MVI   ABINKEY,0           SET TO SEARCH ONLY - DON'T ADD!              
*                                                                               
         GOTO1 VBINSRCH,ABINKEY    BILL BINSRCH ENTRY FOR THIS ELEM             
*                                                                               
         CLI   ABINKEY,X'01'       RECORD NOT FOUND FLAG SET?                   
         BNE   *+6                 NO                                           
         DC    H'0'                REC NOT FOUND - SOMETHING IS WRONG!          
*                                                                               
         L     R5,ABINKEY          ADDRESS OF BINSRCH RECORD                    
         LA    R5,ULBKEYL(R5)      BUMP PAST KEY TO RECORD                      
         USING ULBRECD,R5          BINSRCH RECORD DSECT                         
*                                                                               
         LA    R4,SORTREC          R4 = SORTREC                                 
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         USING SORTRECD,R4         SORT RECORD DSECT                            
         MVC   SORTAM,STABKAM      AGENCY/MEDIA (KEY)                           
         MVC   SORTNMED,QMED       MEDIA                                        
         MVC   SORTCLT,CLT         3-CHARACTER CLIENT (KEY)                     
         MVC   SORTPRD,SAVEPRD     3-CHARACTER PRODUCT (KEY)                    
         MVC   SORTEST,STABKEST    BINARY ESTIMATE (KEY)                        
         MVC   SORTINV,STABINV     PACKED INVOICE NUMBER (KEY)                  
         NI    SORTINV,X'FF'-X'C0' STRIP REVERSED & REVERSAL FLAGS              
         MVC   SORTBDAT,STABBDT    BILLED DATE COMPRESSED (KEY)                 
*                                                                               
         GOTO1 MSUNPK,DMCB,STABKMKT,WORK,WORK+8                                 
         MVC   SORTSTA(4),WORK+8   STATION                                      
         MVI   SORTSTA+4,C'-'      DASH AFTER STATION                           
         MVC   SORTSTA+5(1),WORK+12 BAND FOR RADIO                              
         MVI   SORTSTA+6,C'M'      FOLLOWED BY AN "M'                           
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   ULB50               NO - MUST BE MEDIA R OR X                    
         MVC   SORTSTA+5(2),=C'TV' YES - ADD "TV" AFTER STATION                 
         CLI   WORK+12,C'D'        IS THIS BAND "DV"?                           
         BNE   *+8                 NO                                           
         MVI   SORTSTA+5,C'D'      YES - ADD "DV" AFTER STATION                 
*                                                                               
ULB50    MVC   SORTIGRS,ULBGROSS   GROSS AMOUNT DUE                             
         MVC   SORTINET,ULBNET     NET AMOUNT DUE                               
         MVC   SORTIINV,ULBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SORTIDAT,ULBINVD    INVOICE DATE AS ON BILL                      
         MVC   SORTIDDT,ULBINVDD   INVOICE DUE DATE AS ON BILL                  
         MVC   SORTUDF2,ULBEUDF2   ESTIMATE UDEF 2                              
         MVC   SORTPNAM,ULBPNAME   PRODUCT NAME                                 
         MVC   SORTENAM,ULBENAME   ESTIMATE NAME                                
*                                                                               
         LA    R1,SORTMEDN         R1 = MEDIA NAME                              
         MVC   0(7,R1),=C'SPOT TV' INIT TO SPOT TV                              
         CLI   QMED,C'T'           MEDIA T?                                     
         BE    ULB55               YES                                          
         MVC   5(5,R1),=C'RADIO'   SET TO SPOT RADIO                            
         CLI   QMED,C'R'           MEDIA R?                                     
         BE    ULB55               YES                                          
         MVC   0(13,R1),=C'NETWORK RADIO' SET TO NETWORK RADIO                  
         CLI   QMED,C'X'           MEDIA X?                                     
         BE    ULB55               YES                                          
         DC    H'0'                UNKNOWN MEDIA                                
*                                                                               
ULB55    L     RF,ADCLT            A(CLIENT RECORD)                             
         USING CLTHDR,RF           CLIENT RECORD DSECT                          
         MVC   SORTCNAM,CNAME      CLIENT NAME                                  
         DROP  RF                  DROP CLIENT RECORD USING                     
*                                                                               
         MVC   WORK(2),STABPER     YEAR/MONTH                                   
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         GOTO1 DATCON,DMCB,(3,WORK),(6,SORTMOS)  MMM/YY                         
*                                                                               
         MVC   THISSTA,SORTSTA     SET STATION FOR GETSNAME                     
         GOTO1 =A(GETSNAME),DMCB,(RC) GET STATION NAME                          
         MVC   SORTSNAM,VENDORNM   VENDOR NAME                                  
*                                                                               
         MVC   SORTBNET,STABNET    VENDOR NET AMOUNT                            
*                                                                               
         L     R0,STABNET          VENDOR NET AMOUNT                            
         CVD   R0,DUB              CONVERT TO PACKED                            
         AP    DTLTOT,DUB          SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
         XIT1                      EXIT                                         
         DROP  R6,R7               DROP STATION BUCKET KEY/REC USINGS           
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'PROCNET - PROCESS NETPAK UNIT RECORDS'                          
*                                                                               
PROCNET  CSECT                                                                  
         NMOD1 0,PROCNET                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         L     R6,ANETBLK          A(NETBLOCK)                                  
         USING NETBLOCK,R6         NETBLOCK DSECT                               
*                                                                               
         LR    RE,R6               A(NETBLOCK)                                  
         LH    RF,=Y(NETBLKLN)     L'NETBLOCK                                   
         XCEF                                                                   
*                                                                               
         MVI   NBUSER+13,C'N'      ALWAYS PASS PRE-EMPTED UNITS                 
         MVC   NBSELAGY,QAGY       AGENCY                                       
         MVC   NBSELMED,QMED       MEDIA                                        
         MVC   NBSELCLI,CLT        CLT                                          
         MVC   NBSELPRD,=C'POL'    ALWAYS SET PRODUCTS TO POL                   
         MVC   NBSELEST,BEST       ESTIMATE START                               
         MVC   NBSELESE,BESTEND    ESTIMATE END                                 
         CLC   =C'NO',QEST         REQUESTED ESTIMATE = NO?                     
         BNE   *+10                NO                                           
         MVC   NBSELEFL,QESTEND    YES - SET ESTIMATE FILTER                    
         MVC   NBSELSTR,=C'750101' START DATE (YYMMDD) 1975                     
***      MVC   NBSELEND,=C'991231' END DATE (YYMMDD) 2019                       
***      MVI   NBSELSTR+6,X'FB'    X'FC' MAKES NETIO GIVE END<START ERR         
         XC    NBSELEND,NBSELEND   NO END DATE NEEDED!                          
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBSEQ,C'D'          DATE SEQ                                     
         MVC   NBTRCOPT,RCTRACE    TRACE FROM TRACE CARD IN JCL                 
         MVI   NBFUNCT,NBFNORM     NORMAL FUNCTION                              
         MVI   NBSELPST,C'B'       PASS LOCKED PACKAGES                         
*                                                                               
         MVC   NBAIO,=A(NBIOAREA)  USE NBIOAREA AS I/O AREA                     
         MVC   NBPRINT,PRINT       V(PRINT) NEEDED FOR TRACE                    
         MVC   NBLOADER,LOADER     A(LOADER) REQUIRED IF NO CALLOV              
         MVC   NBACOM,ACOMFACS     A(COMFACS) REQUIRED UNLESS MODULE            
*                                                                               
NTU10    GOTO1 NETIO,DMCB,NETBLOCK CALL NETIO TO PASS BACK UNIT RECORD          
*                                                                               
         CLI   NBERROR,NBINVEST    INVALID ESTIMATE ERROR?                      
         BE    NTUEXIT             YES - DONE                                   
         CLI   NBERROR,NBINVPRD    INVALID PRODUCT ERROR?                       
         BE    NTUEXIT             YES - DONE                                   
         CLI   NBERROR,0           ANY OTHER ERRORS?                            
         BE    *+6                 NO                                           
         DC    H'0'                YES - NO ERRORS TOLERATED                    
*                                                                               
         CLI   NBMODE,NBPROCUN     PROCESS UNIT?                                
         BE    NTU15               YES                                          
         CLI   NBMODE,NBREQLST     LAST FOR THIS REQUEST?                       
         BE    NTUEXIT             YES - DONE                                   
         CLI   NBMODE,NBVALCLI     JUST VALIDATED A CLIENT?                     
         BNE   NTU10               NO - CALL NETIO FOR NEXT UNIT                
         MVI   NBUSER+13,C'N'      ALWAYS PASS PRE-EMPTED UNITS                 
         B     NTU10               CALL NETIO FOR NEXT UNIT                     
*                                                                               
NTU15    L     R3,NBAIO            A(UNIT RECORD)                               
         USING NURECD,R3           UNIT RECORD DSECT                            
         CLI   NUPRD,0             UNALLOCATED?                                 
         BE    NTU10               YES - SKIP THIS UNIT                         
         MVC   SAVEEST,NUKEST      SAVE THE BINARY ESTIMATE                     
*                                                                               
         LAY   RF,NETBILLB         AREA FOR NBLBLOCK                            
         XC    0(NBLLENQ,RF),0(RF) CLEAR NBLBLOCK                               
         USING NBLBILLD,RF         NEGENUBILL READER DSECT                      
         MVC   NBLUNAIO,NBAIO      A(UNIT RECORD)                               
         LA    R1,ELEM             A(ELEM)                                      
         ST    R1,NBL10AIO         A(TO RETURN X'10' ELEM)                      
         LAY   R1,NTU10EL          A(ROUTINE TO PROCESS X'10' ELEM)             
         ST    R1,NBLAHOOK         A(ROUTINE TO PROCESS X'10' ELEM)             
         OI    NBLFUNC,NBLHRAW     RETURN NEW X'10' ELEMS                       
         ST    RF,NBABILRD         A(NETBILLB)                                  
***                                                                             
* THERE IS A BUG IN NEUBILLRDR                                                  
* IF I SET NBLRSTRT & NBLREND, IT GETS MESSED UP WHEN IT TESTS THEM             
* THE FOLLOWING INSTRUCTION CAN CHANGE WHAT'S IN THE FIELD!                     
* OC    NBLRSTRT,NBLREND     BILL RUN DATE FILTER?                              
* THIS WOULD BE THE FIX                                                         
* OC    NBLRSTRT(4),NBLRSTRT BILL RUN DATE FILTER?                              
* BUT SINCE I'M THE ONLY ONE SETTING THESE FIELDS, I WILL FILTER ON             
* THE BILLING DATES MYSELF IN NBL10AIO                                          
***                                                                             
***      MVC   NBLRSTRT,BQSTARTP   BILL RUN DATE START FILTER                   
***      MVC   NBLREND,BQENDP      BILL RUN DATE END FILTER                     
         DROP  RF                  DROP NEGENUBILL READER USING                 
*                                                                               
         GOTO1 =V(NETBLRDR),DMCB,NETBLOCK CALL READER TO GET NUBILL REC         
*                                                                               
         BNE   NTU10               CALL NETIO FOR NEXT UNIT                     
*                                                                               
NTUEXIT  XIT1                      EXIT                                         
*                                                                               
         DROP  R3,R5,R6            DROP ALL USINGS                              
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
NTU10EL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING NETBLOCK,R6         NETBLOCK DSECT                               
         LA    R2,ELEM             A(RETURNED X'10' ELEM)                       
         USING NBILD,R2            X'10' BILL ELEM DSECT                        
         LAY   R3,NETBILLB         AREA FOR NBLBLOCK                            
         USING NBLBILLD,R3         NEGENUBILL READER DSECT                      
*                                                                               
         TM    NBILST,NBILUBQ      UNBILLED?                                    
         BO    NTU10XIT            YES - SKIP THIS ELEMENT                      
         CLC   NBLRUNDT,BQSTARTP   BILLED BEFORE START DATE?                    
         BL    NTU10XIT            YES - SKIP THIS ELEMENT                      
         CLC   NBLRUNDT,BQENDP     BILLED AFTER END DATE?                       
         BH    NTU10XIT            YES - SKIP THIS ELEMENT                      
         CLC   QPRD,=C'ALL'        ALL PRODUCT REQUEST?                         
         BE    NTU10B              YES - ACCEPT ANY PRODUCT                     
         CLC   QPRD,=C'   '        ALL PRODUCT REQUEST?                         
         BE    NTU10B              YES - ACCEPT ANY PRODUCT                     
         CLC   QPRD,=C'POL'        POL PRODUCT REQUEST?                         
         BE    NTU10B              YES - ACCEPT ANY PRODUCT                     
         CLI   NBILPRD,0           HAVE BINARY PRODUCT?                         
         BNE   NTU10A              YES                                          
         CLC   NBILPRDC,PRD        3-CHARACTER PRODUCTS MATCH?                  
         B     *+10                GO TEST CONDITION CODE                       
NTU10A   CLC   NBILPRD,BPRD        BINARY PRODUCTS MATCH?                       
         BNE   NTU10XIT            NO - SKIP THIS ELEMENT                       
*                                                                               
NTU10B   L     RF,VUNITR           A(AGENCY SPECIFIC UNIT ROUTINE)              
         GOTO1 (RF),DMCB,(RC)      PROCESS AGENCY SPECIFIC UNIT ROUTINE         
*                                                                               
NTU10XIT XIT1                      EXIT                                         
*                                                                               
         DROP  R2,R3,R6            DROP USINGS                                  
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'ULUNITR - PROCESS UNIT FOR UNILEVER'                            
ULUNITR  CSECT                                                                  
         NMOD1 0,ULUNITR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
         USING NETBLOCK,R6         NETBLOCK DSECT                               
         USING NURECD,R3           UNIT RECORD DSECT                            
         USING NUBILD,R7           BILL ELEMENT DSECT                           
                                                                                
         LA    R5,BINKEY           R5 = BINSRCH KEY                             
         USING ULBKEYD,R5          BILL BINSRCH KEY DSECT                       
         XC    BINKEY,BINKEY       CLEAR BINSRCH KEY                            
         MVC   ULBAM,BAGYMD        AGENCY/MEDIA                                 
         MVC   ULBCLT,BCLT         PACKED CLIENT                                
         MVC   ULBPRD,NUBILPRC     3-CHAR PRODUCT                               
         CLI   NUBILPRD,0          HAVE BINARY PRODUCT?                         
         BE    ULU45               NO                                           
                                                                                
         L     RF,VCLIST           COMBINED CLIST AND CLIST2                    
*                                                                               
ULU35    CLC   NUBILPRD,3(RF)      MATCH ON BINARY PRODUCT?                     
         BE    ULU40               YES                                          
         LA    RF,4(RF)            NO - BUMP TO NEXT PRODUCT IN VLIST           
         CLI   0(RF),0             END OF LIST?                                 
         BNE   ULU35               NO - PROCESS NEXT PRODUCT                    
         DC    H'0'                MUST FIND THE PRODUCT                        
*                                                                               
ULU40    MVC   ULBPRD,0(RF)        3-CHAR PRODUCT                               
*                                                                               
ULU45    MVC   SAVEPRD,ULBPRD      SAVE 3-CHARACTER PRODUCT CODE                
                                                                                
         MVC   ULBEST,SAVEEST      BINARY ESTIMATE                              
*                                                                               
         GOTO1 AFMTINO,DMCB,0,(C'P',NUBILNUM) PACK INVOICE NUMBER               
*                                                                               
         L     RE,DMCB+4           A(P2) = BINARY INVOICE NUMBER                
         MVC   ULBINVN,0(RE)       BINARY INVOICE NUMBER                        
         MVC   SVINV,0(RE)         SAVE BINARY INVOICE NUMBER                   
         MVC   ULBINVYM,NUBILDAT   DATE OF BILLING COMPRESSED                   
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         ST    R5,ABINKEY          A(BINSRCH KEY)                               
         MVI   ABINKEY,0           SET TO SEARCH ONLY - DON'T ADD!              
*                                                                               
         GOTO1 VBINSRCH,ABINKEY    BILL BINSRCH ENTRY FOR THIS ELEM             
*                                                                               
         CLI   ABINKEY,X'01'       RECORD NOT FOUND FLAG SET?                   
         BNE   *+6                 NO                                           
         DC    H'0'                REC NOT FOUND - SOMETHING IS WRONG!          
*                                                                               
         L     R5,ABINKEY          ADDRESS OF BINSRCH RECORD                    
         LA    R5,ULBKEYL(R5)      BUMP PAST KEY TO RECORD                      
         USING ULBRECD,R5          BINSRCH RECORD DSECT                         
*                                                                               
         LA    R4,SORTREC          R4 = SORTREC                                 
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         USING SORTRECD,R4         SORT RECORD DSECT                            
         MVC   SORTAM,QMED         MEDIA                                        
         MVC   SORTCLT,CLT         CLIENT                                       
         MVC   SORTPRD,SAVEPRD     3-CHARACTER PRODUCT (KEY)                    
         MVC   SORTEST,SAVEEST     BINARY ESTIMATE (KEY)                        
         MVC   SORTINV,SVINV       PACKED INVOICE NUMBER (KEY)                  
         MVC   SORTBDAT,NUBILDAT   BILLED DATE COMPRESSED (KEY)                 
         MVC   SORTSTA(4),NUKNET   NETWORK (KEY)                                
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(3,WORK+20)                              
         CLI   SORTNMED,C'N'       MEDIA N?                                     
         BE    ULU50               YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(0,WORK)                                 
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,WORK+20)                              
*                                                                               
ULU50    MVI   WORK+22,X'01'       SET DAY TO 1                                 
         MVC   SORTKMOS,WORK+20    MOS (KEY)                                    
*                                                                               
         MVC   SORTIGRS,ULBGROSS   GROSS AMOUNT DUE                             
         MVC   SORTINET,ULBNET     NET AMOUNT DUE                               
         MVC   SORTIINV,ULBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SORTIDAT,ULBINVD    INVOICE DATE AS ON BILL                      
         MVC   SORTIDDT,ULBINVDD   INVOICE DUE DATE AS ON BILL                  
         MVC   SORTUDF2,ULBEUDF2   ESTIMATE UDEF 2                              
         MVC   SORTPNAM,ULBPNAME   PRODUCT NAME                                 
         MVC   SORTENAM,ULBENAME   ESTIMATE NAME                                
         MVC   SORTNMED,ULBNMED    MEDIA CODE                                   
*                                                                               
         LA    R1,SORTMEDN         R1 = MEDIA NAME                              
         MVC   0(7,R1),=C'NETWORK' INIT TO NETWORK                              
         CLI   SORTNMED,C'N'       MEDIA N?                                     
         BE    ULU55               YES                                          
         MVC   0(7,R1),=C'CABLE  ' INIT TO CABLE                                
         CLI   SORTNMED,C'C'       MEDIA C?                                     
         BE    ULU55               YES                                          
         MVC   0(7,R1),=C'OTHER  ' INIT TO OTHER                                
         CLI   SORTNMED,C'S'       MEDIA S?                                     
         BE    *+8                 YES                                          
         CLI   SORTNMED,C' '       MEDIA BLANK?                                 
         BE    *+8                 YES                                          
         CLI   SORTNMED,X'00'      MEDIA NULLS?                                 
         BNE   ULU55               NO                                           
         MVC   0(11,R1),=C'SYNDICATION' SET TO SYNDICATION                      
*                                                                               
ULU55    L     RF,ADCLT            A(CLIENT RECORD)                             
         USING CLTHDR,RF           CLIENT RECORD DSECT                          
         MVC   SORTCNAM,CNAME      CLIENT NAME                                  
         DROP  RF                  DROP CLIENT RECORD USING                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,SORTKMOS),(6,SORTMOS)  MMM/YY                     
*                                                                               
         GOTO1 =A(GETSNAME),DMCB,(RC) GET STATION NAME                          
         MVC   SORTSNAM,VENDORNM   VENDOR NAME                                  
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'U',NUBILEL),SPBVALD,0                             
*                                                                               
         MVC   NUBILNET,SPBVENET   SET EFFECTIVE NET INTO ELEM                  
*                                                                               
         MVC   SORTBNET,NUBILNET   VENDOR NET AMOUNT                            
*                                                                               
         L     R0,NUBILNET         VENDOR NET AMOUNT                            
         CVD   R0,DUB              CONVERT TO PACKED                            
         AP    DTLTOT,DUB          SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
         XIT1                      EXIT                                         
*                                                                               
         DROP  R3,R4,R5,R6,R7      DROP ALL USINGS                              
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'PRBILL - PROCESS BILL RECORDS'                                  
PRBILL   CSECT                                                                  
         NMOD1 0,PRBILL                                                         
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         TM    KEY+13,X'80'        DELETED BILL RECORD?                         
         BNZ   PRBXIT              YES - SKIP                                   
*                                                                               
         L     R6,ADBILL           A(BILL RECORD)                               
         USING BILLREC,R6          BILL RECORD USING                            
*                                                                               
         CLC   BDATE,MYSTART       BILL DATE BEFORE START DATE?                 
         BL    PRBXIT              YES - SKIP                                   
         CLC   BDATE,MYEND         BILL DATE AFTER END DATE?                    
         BH    PRBXIT              YES - SKIP                                   
         CLI   SVQOPT1,C'G'        GENERIC EDI?                                 
         BE    *+12                YES - INCLUDE AOR BILLS                      
         TM    BILSTAT,X'20'       AOR BILL?                                    
         BNZ   PRBXIT              YES - SKIP                                   
*                                                                               
         L     RF,VBILLR           A(AGENCY SPECIFIC BILL ROUTINE)              
         GOTO1 (RF),DMCB,(RC)      PROCESS AGENCY SPECIFIC BILL ROUTINE         
*                                                                               
PRBXIT   XIT1                      EXIT                                         
         DROP  R6                  DROP BILL RECORD USING                       
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'ULBILLR - PROCESS BILL RECORD FOR UNILEVER'                     
ULBILLR  CSECT                                                                  
         NMOD1 0,ULBILLR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LA    R0,BINKEY           BINSRCH KEY                                  
         ST    R0,ABINKEY          A(BINSRCH KEY)                               
*                                                                               
         LA    R5,BINKEY           R5 = BINSRCH KEY                             
         USING ULBKEYD,R5          BILLING SORT KEY DSECT                       
         USING BILLREC,R6          BILL RECORD USING                            
         MVC   ULBAM,BKEYAM        AGY/MED                                      
         MVC   ULBCLT,BKEYCLT      PACKED CLIENT CODE                           
         MVC   ULBPRD,BKEYPRD      PRODUCT CODE                                 
         MVC   ULBEST,BKEYEST      ESTIMATE                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(2,ULBINVYM) INV YYMMDD                    
         MVC   ULBINVN,BKEYINV     INVOICE NUMBER PACKED                        
         DROP  R5                  DROP BILLING SORT KEY USING                  
*                                                                               
         LA    R5,BINREC           R5 = BINSRCH RECORD                          
         USING ULBRECD,R5          BILL RECORD DATA DSECT                       
         MVC   ULBREC,SPACES       SPACE-FILL OUTPUT RECORD                     
***      MVC   ULBNET,BNETP        NET AMOUNT                                   
         MVC   ULBNET,BACTP        NET AMOUNT                                   
***      MVC   ULBGROSS,BGRSP      GROSS AMOUNT                                 
         MVC   ULBGROSS,BACTP      GROSS AMOUNT                                 
*                                                                               
         LA    R1,B1PROF           B1 PROFILE                                   
         ST    R1,DMCB+8           PARM 3                                       
         MVC   DMCB+8(1),MED       MEDIA IS HOB OF PARM 4                       
         CLI   NETOPT,C'N'         NETPAK?                                      
         BNE   ULB10               NO                                           
         CLI   BLMED,C' '          MEDIA GIVEN?                                 
         BNH   ULB10               NO - USE MEDIA FROM MED                      
         MVC   DMCB+8(1),BLMED     YES - MEDIA IS HOB OF PARM 4                 
*                                                                               
ULB10    LA    R1,B1XPROF          B1X PROFILE                                  
         ST    R1,DMCB+12          PARM 4                                       
         ST    R6,DMCB+16          A(BILL HEADER RECORD)                        
*                                                                               
         GOTO1 AFMTINO,DMCB,(C'B',BDATE),(2,BKEYINV)                            
*                                                                               
         L     RF,DMCB             RF = DMCB                                    
         MVC   ULBINV,0(RF)        FULL INVOICE NUMBER                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,BQDATE),(20,WORK) YYYYMMDD                        
*                                                                               
         LA    R4,ULBINVD          INVOICE DATE                                 
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(20,WORK) YYYYMMDD                      
*                                                                               
         LA    R4,ULBINVDD         INVOICE DUE DATE                             
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         MVC   ULBNMED,BLMED       NETWORK SUBMEDIA                             
         CLI   BLMED,C' '          HAVE NETWORK SUB-MEDIA ON BILL REC?          
         BH    *+10                YES                                          
         MVC   ULBNMED,MED         NO - USE MEDIA FROM MED                      
*                                                                               
         GOTO1 =A(CKPRD),DMCB,(RC) MAY NEED TO RE-READ PRODUCT                  
*                                                                               
         L     RF,ADPRD            A(PRODUCT RECORD)                            
         USING PRDHDR,RF           PRODUCT RECORD DSECT                         
         MVC   ULBPNAME,PNAME      PRODUCT NAME                                 
         DROP  RF                  DROP PRODUCT RECORD USING                    
*                                                                               
         MVI   CKESTREC,C'L'       FLAG TO READ EST FROM BILL KEY               
         GOTO1 =A(CKEST),DMCB,(RC) MAY NEED TO RE-READ ESTIMATE                 
*                                                                               
         L     RF,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDR,RF           ESTIMATE RECORD DSECT                        
         MVC   ULBEUDF2,EUSER2     ESTIMATE UDEF2                               
         MVC   ULBENAME,EDESC      ESTIMATE NAME                                
         DROP  R5,RF               DROP EST REC & ULBRECD USING                 
*                                                                               
         ZAP   DUB,BACTP           GET BACTP INTO DUB                           
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) TOTAL               
*                                                                               
         MVI   ABINKEY,1           RESET TO INSERT RECORD IF NOT FOUND          
         GOTO1 VBINSRCH,ABINKEY    CALL BINSRCH                                 
*                                                                               
         CLI   ABINKEY,1           RECORD INSERTED?                             
         BE    ULBXIT              YES - DONE                                   
*                                                                               
         OC    1(3,R1),1(R1)       HAVE A(BINSRCH REC)?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - TABLE IS FULL - EXPAND TABLE!           
*                                                                               
         L     RF,ABINKEY          ADDRESS OF FOUND RECORD                      
         LA    RF,L'ULBKEY(RF)     PAST KEY                                     
         USING ULBRECD,RF          BILL SORT RECORD DSECT                       
***      AP    ULBNET,BNETP        NET AMT (DUP KEY MUST ADD FIELD)             
         AP    ULBNET,BACTP        NET AMT (DUP KEY MUST ADD FIELD)             
***      AP    ULBGROSS,BGRSP      GROSS AMT (DUP KEY MUST ADD FIELD)           
         AP    ULBGROSS,BACTP      GROSS AMT (DUP KEY MUST ADD FIELD)           
         DROP  RF                  DROP BILL SORT USING                         
*                                                                               
ULBXIT   AP    INVCNT,=P'1'        UPDATE INVOICE COUNT                         
         XIT1                      EXIT                                         
         DROP  R6                  DROP BILL RECORD USING                       
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GNBILLR - PROCESS BILL RECORD FOR GENERIC EDI'                  
GNBILLR  CSECT                                                                  
         NMOD1 0,GNBILLR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LA    R0,BINKEYGN         BINSRCH KEY                                  
         ST    R0,ABINKEY          A(BINSRCH KEY)                               
*                                                                               
         LA    R5,BINKEYGN         R5 = BINSRCH KEY                             
         USING GEBKEYD,R5          BILLING SORT KEY DSECT                       
         USING BILLREC,R6          BILL RECORD USING                            
         MVC   GEBAM,BKEYAM        AGY/MED                                      
         MVC   GEBCLT,BKEYCLT      PACKED CLIENT CODE                           
*                                                                               
         MVC   GEBPRD,BKEYPRD      PRODUCT CODE                                 
         MVC   GEBEST,BKEYEST      ESTIMATE                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(2,GEBINVYM) INV YYMMDD                    
         MVC   GEBIMOS,BKEYYSRV    MOS                                          
         MVC   GEBINVN,BKEYINV     INVOICE NUMBER PACKED                        
         DROP  R5                  DROP BILLING SORT KEY USING                  
*                                                                               
         LA    R5,BINRECGN         R5 = BINSRCH RECORD                          
         USING GEBRECD,R5          BILL RECORD DATA DSECT                       
         XC    GEBREC,GEBREC       CLEAR OUTPUT RECORD                          
*                                                                               
         L     RF,ADCLT            A(CLIENT RECORD)                             
         USING CLTHDR,RF           CLIENT RECORD DSECT                          
         IC    R0,CPROF+6          AAN PROFILE                                  
         DROP  RF                  DROP CLIENT RECORD USING                     
*                                                                               
         GOTO1 CLUNPK,DMCB,((R0),BKEYCLT),GEBCLT3                               
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
*                                                                               
         MVC   GEBGROSS,SPBVGRSP   BILL GROSS AMOUNT                            
         MVC   GEBNET,SPBVNETP     BILL NET AMOUNT                              
         MVC   GEBACT,SPBVACTP     BILL ACTUAL AMOUNT                           
*                                                                               
***      MVC   GEBCOM,BILBFP       COMMISSION ADJUSTMENT                        
         MVC   GEBTAX,SPBVETAX     BILL TAX AMOUNT                              
         MVC   GEBGST,SPBVGST      BILL GST                                     
         MVC   GEBPST,SPBVPST      BILL PST                                     
         MVC   GEBHST,SPBVHST      BILL HST                                     
*                                                                               
         MVI   GEBPROV,0           INIT PROVINCE                                
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,1,BILNPVTS       HAVE ANY VAT "ELEMENTS"?                     
         BZ    GEB05               NO                                           
*                                                                               
         LA    RF,BILPVPRV         START OF PROVINCE "ELEMENTS"                 
         USING BILPVELD,RF         PROVINCE "ELEMENT" DSECT                     
*                                                                               
GEB00    OC    BILPVAMT,BILPVAMT   HAVE A VAT AMOUNT?                           
         BZ    *+14                NO, CHECK NEXT "ELEMENT"                     
         MVC   GEBPROV,BILPVPRV    SET BILL PROVINCE                            
         B     GEB05               DONE WITH PROVINCE                           
         LA    RF,BILPVLEN(RF)     BUMP TO NEXT "ELEMENT"                       
         BCT   RE,GEB00            CHECK NEXT "ELEMENT"                         
         DROP  RF                  DROP PROVINCE "ELEMENT" DSECT                
*                                                                               
GEB05    LA    R1,B1PROF           B1 PROFILE                                   
         ST    R1,DMCB+8           PARM 3                                       
         MVC   DMCB+8(1),MED       MEDIA IS HOB OF PARM 4                       
         CLI   NETOPT,C'N'         NETPAK?                                      
         BNE   GEB10               NO                                           
         CLI   BLMED,C' '          MEDIA GIVEN?                                 
         BNH   GEB10               NO - USE MEDIA FROM MED                      
         MVC   DMCB+8(1),BLMED     YES - MEDIA IS HOB OF PARM 4                 
*                                                                               
GEB10    LA    R1,B1XPROF          B1X PROFILE                                  
         ST    R1,DMCB+12          PARM 4                                       
         ST    R6,DMCB+16          A(BILL HEADER RECORD)                        
*                                                                               
         GOTO1 AFMTINO,DMCB,(C'B',BDATE),(2,BKEYINV)                            
*                                                                               
         L     RF,DMCB             RF = DMCB                                    
         MVC   GEBINV,0(RF)        FULL INVOICE NUMBER                          
*                                                                               
***      GOTO1 DATCON,DMCB,(0,BQDATE),(20,WORK) YYYYMMDD                        
         GOTO1 DATCON,DMCB,(0,BDATE),(20,WORK) YYYYMMDD                         
*                                                                               
         LA    R4,GEBINVD          INVOICE RUN DATE                             
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(20,WORK) YYYYMMDD                      
*                                                                               
         LA    R4,GEBINVDD         INVOICE DUE DATE                             
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         MVC   GEBQMED,QMED        MEDIA CODE                                   
         MVC   GEBNMED,BLMED       NETWORK SUBMEDIA                             
         CLI   BLMED,C' '          HAVE NETWORK SUB-MEDIA ON BILL REC?          
         BH    *+10                YES                                          
         MVC   GEBNMED,MED         NO - USE MEDIA FROM MED                      
*                                                                               
         MVC   GEBTYPE,SPACES      INIT GEBTYPE                                 
         MVC   GEBTYPE(2),BTYPE    BILL TYPE (B4-B7)                            
         TM    BILSTAT,X'40'       MANUAL BILL?                                 
         BZ    *+14                NO                                           
         MVC   GEBTYPE(3),=C'MAN'  YES - REPORT "MAN"                           
         B     GEB15               BILL TYPE AT THE END                         
         TM    BILSTAT,BSTSCOMQ    SEPARATE COMMISSION BILL?                    
         BZ    *+14                NO                                           
         MVC   GEBTYPE(3),=C'COM'  YES - REPORT "COM"                           
         B     GEB15               BILL TYPE AT THE END                         
         TM    BILSTAT,BSTTAORQ    AOR BILL?                                    
         BZ    GEB20               NO                                           
         MVC   GEBTYPE(3),=C'AOR'  YES - REPORT "AOR"                           
*                                                                               
GEB15    MVC   GEBTYPE+3(1),BTYPE+1 REPORT NUMERIC VALUE AT THE END             
*                                                                               
GEB20    MVC   GEBSTAT,BILSTAT     BILL STATUS                                  
         MVC   GEBSTAT2,BILSTAT2   BILL STATUS 2                                
         MVC   GEBSTAT3,BILSTAT3   BILL STATUS 3                                
         MVC   GEBRETFL,BRETAIL    RETAIL STATUS BITS                           
         MVC   GEBRETAC,BRETACCT   RETAIL ACCOUNT CODE                          
         MVC   GEBWBFLT,BLFLT      WB FLIGHT CODE                               
         MVC   GEBCNET,BCLDNET     CALCULATED NET                               
         MVC   GEBCTYPE,BILCTYP    COST TYPE (NET ONLY)                         
***      MVC   GEBPKGNM,BLPKGNM    PACKAGE NAME (NET ONLY)                      
         MVC   GEBDPTFL,BLDPT      DAYPART FILTER (NET ONLY)                    
         MVC   GEBPKGNO,BLPKG      PACKAGE NUMBER (NET ONLY)                    
*                                                                               
         DROP  R5                  DROP GEBRECD USING                           
*                                                                               
         ZAP   DUB,SPBVNETP        GET SPBVNETP INTO DUB                        
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) TOTAL               
*                                                                               
         MVI   ABINKEY,1           RESET TO INSERT RECORD IF NOT FOUND          
         GOTO1 VBINSRCH,ABINKEY    CALL BINSRCH                                 
*                                                                               
         CLI   ABINKEY,1           RECORD INSERTED?                             
         BE    GEBXIT              YES - DONE                                   
*                                                                               
         OC    1(3,R1),1(R1)       HAVE A(BINSRCH REC)?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - TABLE IS FULL - EXPAND TABLE!           
*                                                                               
         L     RF,ABINKEY          ADDRESS OF FOUND RECORD                      
         LA    RF,L'GEBKEY(RF)     PAST KEY                                     
         USING GEBRECD,RF          BILL SORT RECORD DSECT                       
         OI    GEBIFLAG,GEBIFDUP   MARK THIS AS A DUPLICATE!                    
         ZAP   DUB,SPBVNETP        GET SPBVNETP INTO DUB                        
         SP    BILTOT,DUB          DON'T ADD TWICE                              
*                                                                               
*&&DO                                                                           
         AP    GEBGROSS,SPBVGRSP   BILL GROSS AMOUNT                            
         AP    GEBNET,SPBVNETP     BILL NET AMOUNT                              
         AP    GEBACT,SPBVACTP     BILL ACTUAL AMOUNT                           
*                                                                               
         ICM   R1,15,BILBFP        COMMISSION ADJUSTMENT                        
         ICM   R2,15,GEBCOM        COMMISSION ADJUSTMENT AMT IN BUFFER          
         AR    R1,R2               ADD THEM                                     
         STCM  R1,15,GEBCOM        NEW COMMISSION ADJUSTMENT AMOUNT             
*                                                                               
         ICM   R1,15,SPBVETAX      BILL TAX AMOUNT                              
         ICM   R2,15,GEBTAX        BILLED TAX AMOUNT IN BUFFER                  
         AR    R1,R2               ADD THEM                                     
         STCM  R1,15,GEBTAX        NEW BILLED TAX AMOUNT                        
*                                                                               
         ICM   R1,15,SPBVGST       BILL GST AMOUNT                              
         ICM   R2,15,GEBGST        BILLED GST AMOUNT IN BUFFER                  
         AR    R1,R2               ADD THEM                                     
         STCM  R1,15,GEBGST        NEW BILLED GST AMOUNT                        
*                                                                               
         ICM   R1,15,SPBVPST       BILL PST AMOUNT                              
         ICM   R2,15,GEBPST        BILLED PST AMOUNT IN BUFFER                  
         AR    R1,R2               ADD THEM                                     
         STCM  R1,15,GEBPST        NEW BILLED PST AMOUNT                        
*                                                                               
         ICM   R1,15,SPBVHST       BILL HST AMOUNT                              
         ICM   R2,15,GEBHST        BILLED HST AMOUNT IN BUFFER                  
         AR    R1,R2               ADD THEM                                     
         STCM  R1,15,GEBHST        NEW BILLED HST AMOUNT                        
*&&                                                                             
         DROP  RF                  DROP BILL SORT USING                         
*                                                                               
GEBXIT   AP    INVCNT,=P'1'        UPDATE INVOICE COUNT                         
         XIT1                      EXIT                                         
         DROP  R6                  DROP BILL RECORD USING                       
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GNSTABR - PROCESS STA BUCKET ELEM FOR GENERIC EDI'              
GNSTABR  CSECT                                                                  
         NMOD1 0,GNSTABR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
         USING STABUCKD,R6         STATION BUCKET KEY DSECT                     
         USING STABELEM,R7         BILLING ELEMENT DSECT                        
*                                                                               
         LA    R5,BINKEYGN         R5 = BINSRCH KEY                             
         USING GEBKEYD,R5          BILL BINSRCH KEY DSECT                       
         XC    BINKEYGN,BINKEYGN   CLEAR BINSRCH KEY                            
         MVC   GEBAM,STABKAM       AGENCY/MEDIA                                 
         MVC   GEBCLT,STABKCLT     PACKED CLIENT                                
         MVC   GEBPRD,SAVEPRD      3-CHAR PRODUCT                               
         MVC   GEBEST,STABKEST     BINARY ESTIMATE                              
         MVC   GEBINVYM,STABBDT    DATE OF BILLING COMPRESSED                   
         MVC   GEBIMOS,STABPER     MOS                                          
         MVC   GEBINVN,STABINV     PACKED INVOICE NUMBER                        
         NI    GEBINVN,X'FF'-X'C0' STRIP REVERSED & REVERSAL FLAGS              
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         ST    R5,ABINKEY          A(BINSRCH KEY)                               
         MVI   ABINKEY,0           SET TO SEARCH ONLY - DON'T ADD!              
*                                                                               
         GOTO1 VBINSRCH,ABINKEY    BILL BINSRCH ENTRY FOR THIS ELEM             
*                                                                               
         CLI   ABINKEY,X'01'       RECORD NOT FOUND FLAG SET?                   
         BNE   *+6                 NO                                           
         DC    H'0'                REC NOT FOUND - SOMETHING IS WRONG!          
*                                                                               
         L     R5,ABINKEY          ADDRESS OF BINSRCH RECORD                    
         LA    R5,GEBKEYL(R5)      BUMP PAST KEY TO RECORD                      
         USING GEBRECD,R5          BINSRCH RECORD DSECT                         
*                                                                               
         TM    GEBIFLAG,GEBIFDUP   IS THIS A DUPLICATE BILL HEADER?             
         BNZ   GNSXIT              YES - DO NOT PROCESS!                        
         TM    GEBSTAT,BSTMANQ     MANUAL BILL?                                 
         BNZ   GNSXIT              YES, UNDER DUMMY STA, DON'T PROCESS!         
         OI    GEBIFLAG,GEBIFVEN   FLAG THIS AS HAVING VENDOR INFO              
*                                                                               
         LA    R4,SORTREC          R4 = SORTREC                                 
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         USING SRTGRECD,R4         STATION BUCKET SORT RECORD DSECT             
         MVC   SRTGAM,STABKAM      AGENCY/MEDIA (KEY)                           
         MVC   SRTGCLT,GEBCLT3     3-CHARACTER CLIENT (KEY)                     
         MVC   SRTGPRD,SAVEPRD     3-CHARACTER PRODUCT (KEY)                    
         MVC   SRTGEST,STABKEST    BINARY ESTIMATE (KEY)                        
         MVC   SRTGINV,STABINV     PACKED INVOICE NUMBER (KEY)                  
         NI    SRTGINV,X'FF'-X'C0' STRIP REVERSED & REVERSAL FLAGS              
         MVC   SRTGBDAT,STABBDT    BILLED DATE COMPRESSED (KEY)                 
         MVC   SRTGMOS,STABPER     MOS                                          
         MVC   SRTGMKT,STABKMKT    MARKET                                       
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',STABKMKT),WORK,WORK+8                         
*                                                                               
         MVC   SRTGSTA(4),WORK+8   STATION                                      
         MVI   SRTGSTA+4,C'-'      DASH AFTER STATION                           
         MVI   SRTGSTA+5,C'N'      "N"                                          
         CLI   QMED,C'N'           MEDIA N?                                     
         BNE   GNS40               NO                                           
*                                                                               
         L     RE,ADAGY            A(AGENCY RECORD)                             
         USING AGYHDR,RE           AGENCY RECORD DSECT                          
         CLI   AGYPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   GNS50               NO                                           
         DROP  RE                  DROP AGENCY RECORD USING                     
*                                                                               
         CLI   WORK+12,C'/'        CANADIAN CABLE? (FOOD/SJ)                    
         BNE   GNS50               NO                                           
         MVC   SRTGSTA(7),WORK+8   MOVE CABLE STATION/SUFFIX                    
         B     GNS50               DONE                                         
*                                                                               
GNS40    MVC   SRTGSTA+5(1),WORK+12 BAND FOR RADIO                              
         MVI   SRTGSTA+6,C'M'      FOLLOWED BY AN "M'                           
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   GNS50               NO - MUST BE MEDIA R OR X                    
         MVC   SRTGSTA+5(2),=C'TV' YES - ADD "TV" AFTER STATION                 
         CLI   WORK+12,C'D'        IS THIS BAND "DV"?                           
         BNE   *+8                 NO                                           
         MVI   SRTGSTA+5,C'D'      YES - ADD "DV" AFTER STATION                 
*                                                                               
GNS50    MVC   SRTGBGRS,GEBGROSS   GROSS                                        
         MVC   SRTGBNET,GEBNET     NET                                          
         MVC   SRTGBACT,GEBACT     ACTUAL                                       
***      MVC   SRTGBCOM,GEBCOM     COMMISSION ADJUSTMENT                        
         MVC   SRTGBTAX,GEBTAX     TAX                                          
         MVC   SRTGBGST,GEBGST     GST                                          
         MVC   SRTGBPST,GEBPST     PST                                          
         MVC   SRTGBHST,GEBHST     HST                                          
         MVC   SRTGBPRV,GEBPROV    PROVINCE                                     
         MVC   SRTGBTYP,GEBTYPE    BILL TYPE (B1,B2,B3,B4,B5,ETC)               
         MVC   SRTGBINV,GEBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SRTGIDAT,GEBINVD    INVOICE RUN DATE                             
         MVC   SRTGIDDT,GEBINVDD   INVOICE DUE DATE                             
         MVC   SRTGSTAT,GEBSTAT    STATUS BYTE                                  
         MVC   SRTGSTA2,GEBSTAT2   STATUS BYTE2                                 
         MVC   SRTGSTA3,GEBSTAT3   BILL STATUS 3                                
         MVC   SRTGRTLF,GEBRETFL   RETAIL STATUS BITS                           
         MVC   SRTGRTLA,GEBRETAC   RETAIL ACCOUNT CODE                          
         MVC   SRTGWBFL,GEBWBFLT   WB FLIGHT CODE                               
         MVC   SRTGBCNT,GEBCNET    CALCULATED NET                               
         MVC   SRTGBMED,QMED       MEDIA                                        
*                                                                               
         MVC   WORK(2),STABPER     YEAR/MONTH                                   
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTGBMOS)  MMM/YY                        
         CLI   WORK+1,12           SPECIAL PERIOD?                              
         BNH   GNS60               NO - LEAVE AS MMM/YY                         
         LLC   R1,WORK+1           SPECIAL PERIOD                               
         EDIT  (R1),(2,SRTGBMOS+1) MONTH (>12)                                  
         MVI   SRTGBMOS,C' '       WILL BE 13/YY FOR EXAMPLE                    
*                                                                               
GNS60    GOTO1 SPBVAL,DMCB,(C'E',(R7)),SPBVALD,0                                
*                                                                               
         MVC   SRTGVGRS,SPBVEGRS   VENDOR GROSS AMOUNT                          
         MVC   SRTGVNET,SPBVENET   VENDOR NET AMOUNT                            
         MVC   SRTGVTAX,SPBVETAX   VENDOR TAX AMOUNT                            
*                                                                               
         CLI   STABKCUR,X'01'      COST2 RECORD?                                
         BNE   *+8                 NO                                           
         MVI   SRTGCOS2,C'Y'       YES                                          
*                                                                               
         L     R0,SPBVENET         VENDOR NET AMOUNT                            
         CVD   R0,DUB              CONVERT TO PACKED                            
         AP    DTLTOT,DUB          SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
GNSXIT   XIT1                      EXIT                                         
         DROP  R6,R7               DROP STATION BUCKET KEY/REC USINGS           
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GNUNITR - PROCESS UNIT FOR GENERIC EDI'                         
GNUNITR  CSECT                                                                  
         NMOD1 0,GNUNITR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
         USING NETBLOCK,R6         NETBLOCK DSECT                               
*                                                                               
         L     R3,NBAIO            A(UNIT RECORD)                               
         USING NURECD,R3           UNIT RECORD DSECT                            
*                                                                               
         LA    R7,ELEM             A(RETURNED X'10' ELEM)                       
         USING NBILD,R7            X'10' BILL ELEM DSECT                        
*                                                                               
         LA    R5,BINKEYGN         R5 = BINSRCH KEY                             
         USING GEBKEYD,R5          BILL BINSRCH KEY DSECT                       
         XC    BINKEYGN,BINKEYGN   CLEAR BINSRCH KEY                            
         MVC   GEBAM,BAGYMD        AGENCY/MEDIA                                 
         MVC   GEBCLT,BCLT         PACKED CLIENT                                
         MVC   GEBPRD,NBILPRDC     3-CHAR PRODUCT                               
         CLI   NBILPRD,0           HAVE BINARY PRODUCT?                         
         BE    GNU45               NO                                           
                                                                                
         L     RF,VCLIST           COMBINED CLIST AND CLIST2                    
*                                                                               
GNU35    CLC   NBILPRD,3(RF)       MATCH ON BINARY PRODUCT?                     
         BE    GNU40               YES                                          
         LA    RF,4(RF)            NO - BUMP TO NEXT PRODUCT IN VLIST           
         CLI   0(RF),0             END OF LIST?                                 
         BNE   GNU35               NO - PROCESS NEXT PRODUCT                    
         DC    H'0'                MUST FIND THE PRODUCT                        
*                                                                               
GNU40    MVC   GEBPRD,0(RF)        3-CHAR PRODUCT                               
*                                                                               
GNU45    MVC   GEBEST,SAVEEST      BINARY ESTIMATE                              
                                                                                
         LAY   RF,NETBILLB         AREA FOR NBLBLOCK                            
         USING NBLBILLD,RF         NEGENUBILL READER DSECT                      
         MVC   GEBINVYM,NBLRUNDT   DATE OF BILLING COMPRESSED                   
         DROP  RF                  DROP NEGENUBILL READER USING                 
*                                                                               
***      GOTO1 DATCON,DMCB,(2,NBILMOS),(3,WORK+20)                              
*                                                                               
***      MVC   GEBIMOS,WORK+20     MOS                                          
         MVC   GEBIMOS,NBILMOS     MOS                                          
*                                                                               
***      GOTO1 AFMTINO,DMCB,0,(C'P',NUBILNUM) PACK INVOICE NUMBER               
*                                                                               
***      L     RE,DMCB+4           A(P2) = BINARY INVOICE NUMBER                
***      MVC   GEBINVN,0(RE)       BINARY INVOICE NUMBER                        
         MVC   GEBINVN,NBILNUM     BINARY INVOICE NUMBER                        
*                                                                               
         ST    R5,ABINKEY          A(BINSRCH KEY)                               
         MVI   ABINKEY,0           SET TO SEARCH ONLY - DON'T ADD!              
*                                                                               
         GOTO1 VBINSRCH,ABINKEY    BILL BINSRCH ENTRY FOR THIS ELEM             
*                                                                               
         CLI   ABINKEY,X'01'       RECORD NOT FOUND FLAG SET?                   
         BNE   *+6                 NO                                           
         DC    H'0'                REC NOT FOUND - SOMETHING IS WRONG!          
*                                                                               
         L     R5,ABINKEY          ADDRESS OF BINSRCH RECORD                    
*                                                                               
         LA    R4,SORTREC          R4 = SORTREC                                 
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         USING SRTGRECD,R4         SORT RECORD DSECT                            
         MVC   SRTGAM,GEBAM        MEDIA                                        
         MVC   SRTGPRD,GEBPRD      3-CHARACTER PRODUCT                          
         MVC   SRTGEST,GEBEST      BINARY ESTIMATE                              
         MVC   SRTGINV,GEBINVN     PACKED INVOICE NUMBER                        
         MVC   SRTGBDAT,GEBINVYM   BILLED DATE COMPRESSED                       
         MVC   SRTGMOS,GEBIMOS     MOS                                          
*                                                                               
         MVC   WORK(2),GEBIMOS     YEAR/MONTH                                   
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTGBMOS)  MMM/YY                        
         CLI   WORK+1,12           SPECIAL PERIOD?                              
         BNH   GNU60               NO - LEAVE AS MMM/YY                         
         LLC   R1,WORK+1           SPECIAL PERIOD                               
         EDIT  (R1),(2,SRTGBMOS+1) MONTH (>12)                                  
         MVI   SRTGBMOS,C' '       WILL BE 13/YY FOR EXAMPLE                    
*                                                                               
GNU60    MVC   SRTGMKT,NBMARKET    MARKET                                       
         MVC   SRTGSTA(4),NUKNET   NETWORK                                      
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         LA    R5,GEBKEYL(R5)      BUMP PAST KEY TO RECORD                      
         USING GEBRECD,R5          BINSRCH RECORD DSECT                         
*                                                                               
         TM    GEBIFLAG,GEBIFDUP   IS THIS A DUPLICATE BILL HEADER?             
         BNZ   GNUXIT              YES - DO NOT PROCESS!                        
         TM    GEBSTAT,BSTMANQ     MANUAL BILL?                                 
         BNZ   GNUXIT              YES, UNDER DUMMY STA, DON'T PROCESS!         
         OI    GEBIFLAG,GEBIFVEN   FLAG THIS AS HAVING VENDOR INFO              
*                                                                               
         MVC   SRTGCLT,GEBCLT3     CLIENT                                       
         MVC   SRTGBGRS,GEBGROSS   GROSS AMOUNT DUE                             
         MVC   SRTGBNET,GEBNET     NET AMOUNT DUE                               
         MVC   SRTGBACT,GEBACT     ACTUAL AMOUNT DUE                            
***      MVC   SRTGBCOM,GEBCOM     COMMISSION ADJUSTMENT                        
         MVC   SRTGBTAX,GEBTAX     TAX AMOUNT DUE                               
         MVC   SRTGBTYP,GEBTYPE    BILL TYPE (B1,B2,B3,B4,B5,ETC)               
         MVC   SRTGBINV,GEBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SRTGIDAT,GEBINVD    INVOICE RUN DATE                             
         MVC   SRTGIDDT,GEBINVDD   INVOICE DUE DATE                             
         MVC   SRTGSTAT,GEBSTAT    STATUS BYTE                                  
         MVC   SRTGSTA2,GEBSTAT2   STATUS BYTE 2                                
         MVC   SRTGSTA3,GEBSTAT3   BILL STATUS 3                                
         MVC   SRTGRTLF,GEBRETFL   RETAIL STATUS BITS                           
         MVC   SRTGRTLA,GEBRETAC   RETAIL ACCOUNT CODE                          
         MVC   SRTGWBFL,GEBWBFLT   WB FLIGHT CODE                               
         MVC   SRTGBCNT,GEBCNET    CALCULATED NET                               
         MVC   SRTGCTYP,GEBCTYPE   COST TYPE (NET ONLY)                         
***      MVC   SRTGPKNM,GEBPKGNM   PACKAGE NAME (NET ONLY)                      
         MVC   SRTGDPTF,GEBDPTFL   DAYPART FILTER (NET ONLY)                    
         MVC   SRTGPKNO,GEBPKGNO   PACKAGE NUMBER (NET ONLY)                    
         MVC   SRTGBMED,QMED       MEDIA CODE                                   
         MVC   SRTGBSMD,GEBNMED    NETWORK SUB-MEDIA                            
         DROP  R5                  DROP BINSRCH REC USING                       
*                                                                               
         CLI   NBILCHGT,C'X'       SPECIAL CHARGE TYPE = X?                     
         BNE   GNU65               NO                                           
         MVC   SRTGVGTX,NBILGRS    VENDOR TAX AMOUNT GROSS                      
         MVC   SRTGVNTX,NBILNET    VENDOR TAX AMOUNT NET                        
         B     GNU70               THIS IS A TAX ONLY ELEMENT                   
*                                                                               
GNU65    XC    ELEM2,ELEM2         CLEAR ELEM2                                  
         LA    RF,ELEM2            BUILD FAKE UNIT BILLING ELEMENT              
         USING NUBILD,RF           NUBILD DSECT                                 
         MVC   NUBILGRS,NBILGRS    GROSS                                        
         MVC   NUBILNET,NBILNET    NET                                          
         MVC   NUBILST,NBILST      STATUS                                       
         DROP  RF                  DROP NUBILD USING                            
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'U',ELEM2),SPBVALD,0                               
*                                                                               
         MVC   SRTGVGRS,SPBVEGRS   VENDOR GROSS AMOUNT                          
         MVC   SRTGVNET,SPBVENET   VENDOR NET AMOUNT                            
         MVC   SRTGVCS2,NBILGR2    VENDOR COST2                                 
*                                                                               
GNU70    L     R0,SPBVENET         VENDOR NET AMOUNT                            
         CVD   R0,DUB              CONVERT TO PACKED                            
         AP    DTLTOT,DUB          SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
GNUXIT   XIT1                      EXIT                                         
*                                                                               
         DROP  R3,R4,R6,R7         DROP ALL USINGS                              
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GNOUTPR - OUTPUT ROUTINE FOR GENERIC EDI'                       
GNOUTPR  CSECT                                                                  
         NMOD1 0,GNOUTPR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         BRAS  RE,NOVENDOR         ADD SORT RECS FOR INV W/NO VENDORS           
*                                                                               
         XC    SRTKSVGE,SRTKSVGE   CLEAR SAVED KEY                              
         XC    SVCLTKEY,SVCLTKEY   CLEAR SAVED KEY                              
         XC    SVPRDKEY,SVPRDKEY   CLEAR SAVED KEY                              
         XC    SVESTKEY,SVESTKEY   CLEAR SAVED KEY                              
         XC    SVMKTKEY,SVMKTKEY   CLEAR SAVED KEY                              
         XC    SVMGPKEY,SVMGPKEY   CLEAR SAVED KEY                              
         XC    PMGRKEY,PMGRKEY     CLEAR SAVED KEY                              
         XC    SVADDKEY,SVADDKEY   CLEAR SAVED KEY                              
         MVI   NETFLAGS,0          INIT NETWORK FLAGS                           
         ZAP   VENCOUNT,=P'0'      ZAP ACCUMULATOR                              
*                                                                               
         BRAS  RE,OUTRECCL         CLEAR OUTREC                                 
*                                                                               
         MVI   CANADIAN,C'N'       INIT TO USA AGENCY                           
         L     R2,ADAGY            A(AGENCY RECORD)                             
         USING AGYHDR,R2           AGENCY RECORD DSECT                          
         CLI   AGYPROF+7,C'C'      IS THIS CANADIAN                             
         BNE   *+8                 NO                                           
         MVI   CANADIAN,C'Y'       CANADIAN AGENCY                              
         DROP  R2                  DROP AGENCY RECORD USING                     
*                                                                               
         L     R2,BUYLIST          GENERIC EDI SOFT INV FIELDS BLOCK            
         USING GENSFLDH,R2         GENERIC EDI SOFT INV FIELDS DSECT            
*                                                                               
         L     RE,VMASTC           A(MASTC)                                     
         USING MASTD,RE            MASTD DSECT                                  
         MVC   GENUSRID,MCUSERID   USER-ID                                      
         DROP  RE                  DROP MASTD USING                             
*                                                                               
         MVC   GENSYSTM,=C'NET '   SYSTEM (SPOT/NET)                            
         CLI   NETOPT,C'N'         NETWORK REQUEST?                             
         BE    *+10                YES                                          
         MVC   GENSYSTM,=C'SPOT'   SYSTEM (SPOT/NET)                            
         MVC   GENAGNCY,SAVEAGY    AGENCY ALPHA                                 
         MVC   GENFRMAT,SVQOPT1    FORMAT/FLAVOR                                
         MVC   GENCRDAT,CTODAY     CREATION DATE (20191008)                     
         MVC   GENCRTHH,TIMEOFD    CREATION TIME HH (HH:24:55)                  
         MVC   GENCRTMM,TIMEOFD+3  CREATION TIME MM (21:MM:55)                  
         MVC   GENCRTSS,TIMEOFD+6  CREATION TIME SS (21:24:SS)                  
*                                                                               
GNOUT10  GOTO1 VSORTER,DMCB,=C'GET' GET SORT RECORD                             
*                                                                               
         ICM   R5,15,4(R1)         ANY MORE RECORDS FROM SORTER?                
         BZ    GNOUT50             NO - DONE                                    
         USING SRTGRECD,R5         SRTGRECD DSECT                               
*                                                                               
         MVC   SORTREC(SRTGRLEN),0(R5)  MOVE SORT RECORD TO SORTREC             
         LA    R5,SORTREC          R5 = SORTREC                                 
*                                                                               
         CLI   NETOPT,C'N'         NETWORK REQUEST?                             
         BE    *+10                YES - WE COUNT RECORDS LATER                 
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   GNOUT14             NO - REPORT SPOT AS IS                       
         TM    NETFLAGS,FIRSTNET   PROCESSING FIRST SORT RECORD?                
         BNZ   GNOUT11             NO                                           
         MVC   SRTKSVGN,SRTGKEY    YES - SET SRTKSVGN FIRST TIME IN             
         MVC   SRTRCSGN,SORTREC    SAVE THE SORT RECORD                         
         OI    NETFLAGS,FIRSTNET   PROCESSED FIRST NET SORT REC FLAG            
*                                                                               
GNOUT11  CLC   SRTKSVGN,SRTGKEY    SAME SORT KEY AS LAST?                       
         BNE   GNOUT12             NO - REPORT                                  
*                                                                               
         ICM   RE,15,SRTGVGRS      BILLED GROSS FROM THIS SORT REC              
         ICM   RF,15,SAVEBGRS      RUNNING TOTAL BILLED GROSS FOR KEY           
         AR    RF,RE               ADD THIS ENTRY                               
         STCM  RF,15,SAVEBGRS      NEW RUNNING TOTAL                            
*                                                                               
         ICM   RE,15,SRTGVNET      BILLED NET FROM THIS SORT REC                
         ICM   RF,15,SAVEBNET      RUNNING TOTAL BILLED NET FOR KEY             
         AR    RF,RE               ADD THIS ENTRY                               
         STCM  RF,15,SAVEBNET      NEW RUNNING TOTAL                            
*                                                                               
         ICM   RE,15,SRTGVGTX      BILLED TAX GROSS FROM THIS SORT REC          
         ICM   RF,15,SAVEBGTX      RUNNING TOTAL BILLED TAX FOR KEY             
         AR    RF,RE               ADD THIS ENTRY                               
         STCM  RF,15,SAVEBGTX      NEW RUNNING TOTAL                            
*                                                                               
         ICM   RE,15,SRTGVNTX      BILLED TAX NET FROM THIS SORT REC            
         ICM   RF,15,SAVEBNTX      RUNNING TOTAL BILLED TAX FOR KEY             
         AR    RF,RE               ADD THIS ENTRY                               
         STCM  RF,15,SAVEBNTX      NEW RUNNING TOTAL                            
*                                                                               
         ICM   RE,15,SRTGVCS2      BILLED COST2 FROM THIS SORT REC              
         ICM   RF,15,SAVEBCS2      RUNNING TOTAL BILLED COST2 FOR KEY           
         AR    RF,RE               ADD THIS ENTRY                               
         STCM  RF,15,SAVEBCS2      NEW RUNNING TOTAL                            
*                                                                               
         NI    NETFLAGS,X'FF'-THISNET  NOT PROCEESED YET                        
         B     GNOUT10             GET NEXT SORT REC                            
*                                                                               
GNOUT12  LA    R5,SRTRCSGN         REPORT THE SAVED SORT RECORD                 
         MVC   SRTGVGRS,SAVEBGRS   TOTAL GROSS                                  
         MVC   SRTGVNET,SAVEBNET   TOTAL NET                                    
         MVC   SRTGVGTX,SAVEBGTX   TOTAL TAX GROSS                              
         MVC   SRTGVNTX,SAVEBNTX   TOTAL TAX NET                                
         MVC   SRTGVCS2,SAVEBCS2   TOTAL COST2                                  
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
         OI    NETFLAGS,THISNET    PROCESSED THIS TIME                          
*                                                                               
GNOUT14  CLC   SRTKSVGE,SRTGKEY    SAME BILL HDR INFO?                          
         BE    GNOUT46             YES - KEEP BULDING VENDOR JSON DATA          
*                                                                               
         MVC   SRTKSVGE,SRTGKEY    SAVE SORT KEY (JUST BILL HDR INFO)           
         MVI   FIRSTINV,C'Y'       FIRST TIME FOR THIS INVOICE                  
*                                                                               
         CP    SORTCNT,=P'1'       FIRST TIME IN?                               
         BE    GNOUT15             YES                                          
*                                                                               
         LAY   RE,GENDJSON         END JSON TABLE                               
         CLI   ONLYINVC,C'Y'       ONLY HAVE INVOICE INFO?                      
         BNE   *+10                NO                                           
         LAY   RE,GENDJSN2         YES - DON'T SEND CLOSING VENDOR "]"          
         ST    RE,DMCB+4           PARM 2                                       
         LA    RE,=X'FF'           NO SPECIAL TABLE                             
         ST    RE,DMCB+8           PARM 3                                       
         ST    RE,DMCB+12          PARM 4                                       
         ST    RE,DMCB+16          PARM 5                                       
*                                                                               
         GOTO1 =A(GNTABLE),DMCB,(RC)                                            
*                                                                               
         BRAS  RE,SENDMQ           SEND JSON PAYLOAD VIA MQ MSG                 
*                                                                               
         BRAS  RE,PRNTJSON         PRINT JSON DATA                              
*                                                                               
         BRAS  RE,OUTRECCL         CLEAR OUTREC                                 
*                                                                               
GNOUT15  L     R0,BUYLIST          INVOICE BLOCK                                
         AHI   R0,GENCLLEN         BUMP PAST DATA WE DON'T CLEAR                
         LHI   R1,GENSFDHQ         LENGTH OF INVOICE BLOCK                      
         SHI   R1,GENCLLEN         MINUS DATA WE AREN'T CLEARING                
         SR    RE,RE               CLEAR RE                                     
         SR    RF,RF               CLEAR RF                                     
         MVCL  R0,RE               CLEAR THE INVOICE BLOCK                      
*                                                                               
         AP    VENCOUNT,=P'1'      UPDATE VENDOR COUNT                          
*                                                                               
         MVC   GENMEDCD,SRTGBMED   MEDIA CODE                                   
*                                                                               
         GOTO1 =A(GETMEDNM),DMCB,(RC),SRTGBMED,GENMEDNM                         
*                                                                               
         GOTO1 =A(RDCLT),DMCB,(RC),SRTGAM,SRTGCLT                               
*                                                                               
         MVC   GENCLTCD,SRTGCLT    CLIENT CODE                                  
*                                                                               
         L     RF,ADCLT            A(CLIENT RECORD)                             
         USING CLTHDR,RF           CLIENT RECORD DSECT                          
         MVC   GENCLTNM,CNAME      CLIENT NAME                                  
         MVC   GENCLTAO,CACCOFC    ACC OFFICE CODE                              
         DROP  RF                  DROP CLIENT RECORD USING                     
*                                                                               
         GOTO1 =A(RDPRD),DMCB,(RC),SRTGAM,SRTGCLT,SRTGPRD                       
*                                                                               
         MVC   GENPRDCD,SRTGPRD    PRODUCT CODE                                 
*                                                                               
         L     RF,ADPRD            A(PRODUCT RECORD)                            
         USING PRDHDR,RF           PRODUCT RECORD DSECT                         
         MVC   GENPRDNM,PNAME      PRODUCT NAME                                 
         MVC   GENPRUS1,PUSER1     PRODUCT UDEF 1                               
         MVC   GENPRUS2,PUSER2     PRODUCT UDEF2                                
         MVC   GENPRDA1,PADDR1     PRODUCT ADDRESS 1                            
         MVC   GENPRDA2,PADDR2     PRODUCT ADDRESS 2                            
         MVC   GENPRDA3,PADDR3     PRODUCT ADDRESS 3                            
         MVC   GENPRDA4,PADDR4     PRODUCT ADDRESS 4                            
         MVC   GENPRDIC,PACCT      PRODUCT INTERFACE CODE                       
         MVC   GENPRDDV,PDIV       PRODUCT DIVISION                             
         DROP  RF                  DROP PRODUCT RECORD USING                    
*                                                                               
         GOTO1 =A(PRDUCOMM),DMCB,(RC) READ PRODUCT UCOMM DATA                   
*                                                                               
         MVC   GENPRDU1,SVPUCOM1   PRODUCT UCOMM 1                              
         MVC   GENPRDU2,SVPUCOM2   PRODUCT UCOMM 2                              
         MVC   GENPRDU3,SVPUCOM3   PRODUCT UCOMM 3                              
         MVC   GENPRDU4,SVPUCOM4   PRODUCT UCOMM 4                              
*                                                                               
         GOTO1 =A(READPGRP),DMCB,(RC) READ PRODUCT GROUP RECORD                 
*                                                                               
         MVC   GENPRGRP,SVPGRP     PRODUCT GROUP CODE                           
         MVC   GENPGRPN,SVPGRPN    PRODUCT GROUP NAME                           
         MVC   GENPGRN2,SVPGRPN2   PRODUCT GROUP NAME 2                         
*                                                                               
         GOTO1 =A(RDEST),DMCB,(RC),SRTGAM,SRTGCLT,SRTGPRD,SRTGEST               
*                                                                               
         EDIT  SRTGEST,(3,GENESTCD),FILL=0                                      
*                                                                               
         L     RF,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDR,RF           ESTIMATE RECORD DSECT                        
         MVC   GENESTNM,EDESC      ESTIMATE NAME                                
         MVC   GENESUS1,EUSER1     ESTIMATE UDEF 1                              
         MVC   GENESUS2,EUSER2     ESTIMATE UDEF2                               
         DROP  RF                  DROP ESTIMATE RECORD USING                   
*                                                                               
         GOTO1 =A(ESTUCOMM),DMCB,(RC) READ UCOMM DATA                           
*                                                                               
         LA    RE,GENESUC1         ESTIMATE UCOMM 1 TO REPORT                   
         LA    RF,SVEUCOM1         ESTIMATE UCOMM 1 FROM RECORD                 
         LA    R0,8                8 ESTIMATE UCOMMS                            
*                                                                               
GNOUT16  MVC   0(32,RE),0(RF)      MOVE ESTIMATE UCOMM                          
         LA    RE,32(RE)           NEXT ESTIMATE UCOMM                          
         LA    RF,32(RF)           NEXT ESTIMATE UCOMM                          
         BCT   R0,GNOUT16          MOVE NEXT ESTIMATE UCOMM                     
*                                                                               
         MVC   GENINMOS,SRTGBMOS   INVOICE MOS (APR/20)                         
         MVC   GENINVDT,SRTGIDAT   INVOICE RUN DATE (2019-10-08)                
         MVC   GENINVDD,SRTGIDDT   INVOICE DUE DATE (2019-10-08)                
         MVC   GENINVNO,SRTGBINV   INVOICE NUMBER WITH DASHES                   
*                                                                               
         EDIT  SRTGBGRS,(13,GENINGRS),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENINGRS    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENINGRS(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGBNET,(13,GENINNET),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENINNET    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENINNET(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGBACT,(13,GENINACT),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENINACT    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENINACT(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGBCNT,(13,GENINNTC),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENINNTC    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENINNTC(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         ZAP   PAK6,SRTGBACT       ACTUAL                                       
         SP    PAK6,SRTGBNET       MINUS NET                                    
*                                                                               
         EDIT  PAK6,(13,GENINCOM),2,ALIGN=LEFT,FLOAT=-                          
*                                                                               
         CLC   =C'.00',GENINCOM    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENINCOM(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGBTAX,(13,GENINTAX),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENINTAX    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENINTAX(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGBGST,(13,GENINGST),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENINGST    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
***      MVC   GENINGST(3),=C'0  ' YES - MAKE IT ZERO                           
         XC    GENINGST,GENINGST   YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGBPST,(13,GENINPST),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENINPST    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         XC    GENINPST,GENINPST   YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGBHST,(13,GENINHST),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENINHST    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         XC    GENINHST,GENINHST   YES - MAKE IT ZERO                           
*                                                                               
         XC    GENINGSR,GENINGSR   CLEAR GST RATE                               
         XC    GENINPSR,GENINPSR   CLEAR PST RATE                               
         XC    GENINHSR,GENINHSR   CLEAR HST RATE                               
         XC    GENINPRV,GENINPRV   CLEAR PROVINCE NAME                          
         XC    GENINTXT,GENINTXT   CLEAR TAX TYPE                               
*                                                                               
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,1,SRTGBPRV       HAVE A PROVINCE?                             
         BZ    GNOUT20             NO                                           
         BCTR  RE,0                YES, -1 TO INDEX INTO TABLE                  
         MHI   RE,L'PRVTABG        INDEX INTO PROVINCE TABLE                    
         LAY   R6,PRVTABG          PROVINCE TABLE                               
         AR    R6,RE               INDEX INTO PROVINCE TABLE                    
*                                                                               
         MVC   GENINPRV,0(R6)      PROVINCE (PROVTAB IN SPGETRATE)              
         MVC   GENINTXT,20(R6)     INVOICE TAX TYPE FOR PROVINCE                
         CLC   =C'PST',20(R6)      PROVINCE HAS PST?                            
         BE    *+14                YES                                          
         CLC   =C'QST',20(R6)      PROVINCE HAS QST?                            
         BNE   *+10                NO                                           
         MVC   GENINPSR,23(R6)     PST TAX RATE FOR PROVINCE                    
         CLC   =C'HST',20(R6)      PROVINCE HAS HST?                            
         BNE   *+16                NO                                           
         MVC   GENINHSR,23(R6)     HST TAX RATE FOR PROVINCE                    
         XC    GENINPST,GENINPST   MAKE PST AMOUNT ZERO                         
*                                                                               
         OC    SRTGBGST,SRTGBGST   HAVE GST?                                    
         BZ    *+10                NO                                           
         MVC   GENINGSR,=C'5    '  GST RATE (5.000)                             
*                                                                               
GNOUT20  MVC   GENINTYP,SRTGBTYP   BILL TYPE (BTYPE FROM SPGENBILL)             
         MVC   GENRETAC,SRTGRTLA   RETAIL ACCOUNT CODE                          
         MVC   GENWBFLT,SRTGWBFL   WB FLIGHT                                    
         MVC   GENINSMD,SRTGBSMD   NETWORK SUB-MEDIA                            
         MVC   GENCTYPE,SRTGCTYP   COST TYPE (NET ONLY)                         
***      MVC   GENPKGNM,SRTGPKNM   PACKAGE NAME (NET ONLY)                      
         MVC   GENDPTFL,SRTGDPTF   DAYPART FILTER (NET ONLY)                    
*                                                                               
         EDIT  SRTGPKNO,(3,GENPKGNO),0,ALIGN=LEFT                               
*                                                                               
         LA    R3,GENSADJB         SEP ADJUSTMENT BILL FLAG                     
         LA    R4,SRTGSTAT         BILL STATUS (BILSTAT)                        
         BRAS  RE,TRFLAG           TRANSLATE THE FLAG INTO FIELDS               
*                                                                               
         LA    R3,GENRETCB         BRETAIL FLAG FROM SORT                       
         LA    R4,SRTGRTLF         BRETAIL FLAG SOFT FIELDS                     
         BRAS  RE,TRFLAG           TRANSLATE THE FLAG INTO FIELDS               
*                                                                               
         LA    R3,GENSTAT2         BILSTAT2 FLAG FROM SORT                      
         LA    R4,SRTGSTA2         BILSTAT2 FLAG SOFT FIELDS                    
         BRAS  RE,TRFLAG           TRANSLATE THE FLAG INTO FIELDS               
*                                                                               
         LA    R3,GENSTAT3         BILSTAT3 FLAG FROM SORT                      
         LA    R4,SRTGSTA3         BILSTAT3 FLAG SOFT FIELDS                    
         BRAS  RE,TRFLAG           TRANSLATE THE FLAG INTO FIELDS               
*                                                                               
         EDIT  VENCOUNT,GENINCNT,0,COMMAS=YES,ALIGN=LEFT                        
*                                                                               
         L     R4,AOUTREC          R4 = A(OUTREC)                               
*                                                                               
         LAY   RE,GNHTAB           INVOICE HEADER JSON TABLE                    
         ST    RE,DMCB+4           PARM 2                                       
         LAY   RE,GNHSOFT          INVOICE HEADER SOFT FIELDS TABLE             
         ST    RE,DMCB+8           PARM 3                                       
         L     RE,BUYLIST          A(SOFT FIELDS)                               
         ST    RE,DMCB+12          PARM 4                                       
         LAY   RE,GNHEXL           INVOICE HEADER EXCLUSION TABLE               
         ST    RE,DMCB+16          PARM 5                                       
*                                                                               
         GOTO1 =A(GNTABLE),DMCB,(RC)                                            
***                                                                             
* LOOP HERE FOR EACH VENDOR                                                     
***                                                                             
GNOUT46  BRAS  RE,PRTRPT           PRINT REPORT LINE                            
*                                                                               
         OC    SRTGSTA,SRTGSTA     DO WE HAVE A VENDOR?                         
         BNZ   GNOUT46A            YES                                          
         MVI   ONLYINVC,C'Y'       INDICATE INVOICE ONLY INFO                   
         BCTR  R4,0                EXTRA COMMA BEFORE VENDORS ARRAY             
         B     GNOUT49B            SKIP VENDOR SECTION                          
*                                                                               
GNOUT46A L     R3,BUYLIST          GENERIC EDI SOFT INV FIELDS BLOCK            
         AHI   R3,GENSFDHQ         BUMP TO EDI SOFT VENDOR FIELDS BLOCK         
         USING GENSFLDV,R3         GENERIC EDI SOFT INV FIELDS DSECT            
*                                                                               
         LR    R0,R3               VENDOR BLOCK                                 
         LHI   R1,GENSFDVQ         LENGTH OF VENDOR BLOCK                       
         SR    RE,RE               CLEAR RE                                     
         SR    RF,RF               CLEAR RF                                     
         MVCL  R0,RE               CLEAR THE VENDOR BLOCK                       
*                                                                               
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,3,SRTGMKT        BINARY MARKET                                
         CVD   RE,DUB              CVD                                          
         OI    DUB+7,X'0F'         MAKE SURE IT'S POSITIVE                      
         UNPK  GENVMKCD,DUB        UNPACKED MARKET                              
*                                                                               
         MVC   GENVMKNM,=CL24'MARKET NOT ON FILE'                               
*                                                                               
         GOTO1 =A(RDMKT),DMCB,(RC),SRTGBMED,GENVMKCD,SAVEAGY                    
         BNE   GNOUT47             NOT FOUND                                    
*                                                                               
         L     RF,ADMARKET         A(MARKET RECORD)                             
         USING MKTRECD,RF          MARKET RECORD DSECT                          
         MVC   GENVMKNM,MKTNAME    MARKET NAME                                  
         OC    GENVMKNM,SPACES     SPACE PAD                                    
         DROP  RF                  DROP MARKET RECORD USING                     
*                                                                               
GNOUT47  CLI   NETOPT,C'N'         NETWORK REQUEST?                             
         BE    GNOUT48             YES - NET DOES NOT HAVE MKT GROUPS           
*                                                                               
         GOTO1 =A(RDMKTGRP),DMCB,(RC),SRTGAM,SRTGCLT,SVPGRPP,SRTGMKT            
*                                                                               
         MVC   GENVMGRP,SVMGROUP   UP TO 6 CHARACTER MARKET GROUP               
         MVC   GENVMGN1,SVMGRPN    MARKET GROUP NAME 1                          
         MVC   GENVMGN2,SVMGRPN2   MARKET GROUP NAME 2                          
         MVC   GENVMGN3,SVMGRPN3   MARKET GROUP NAME 3                          
*                                                                               
         GOTO1 =A(MKTUCOMM),DMCB,(RC),SRTGMKT READ MARKET UCOMM DATA            
*                                                                               
         MVC   GENVMUC1,SVMUCOM1   MARKET UCOMM 1                               
         MVC   GENVMUC2,SVMUCOM2   MARKET UCOMM 2                               
         MVC   GENVMUC3,SVMUCOM3   MARKET UCOMM 3                               
         MVC   GENVMUC4,SVMUCOM4   MARKET UCOMM 4                               
*                                                                               
         MVC   GENVSTAN,SRTGSTA    STATION                                      
         MVC   DUB(4),SRTGSTA      STATION                                      
         MVC   DUB+4(1),SRTGSTA+5  NEED 5 CHARS FOR SPOT                        
         MVC   GENVNAME,=CL20'ADDRESS NOT ON FILE'                              
         B     GNOUT48A            READ STATION ADDRESS RECORD                  
*                                                                               
GNOUT48  MVC   GENVNTWK,SRTGSTA    NETWORK                                      
         MVC   DUB(4),SRTGSTA      NETWORK                                      
         MVC   GENVNTWN,=CL20'ADDRESS NOT ON FILE'                              
*                                                                               
GNOUT48A GOTO1 =A(RDSTADD),DMCB,(RC),SRTGBMED,DUB,SAVEAGY                       
         BNE   GNOUT49             NOT FOUND                                    
*                                                                               
         L     RF,ADSTATAD         A(STATION ADDRESS RECORD)                    
         USING ADDRECD,RF          STATION ADDRESS RECORD DSECT                 
         LA    R1,GENVNTWN         NET STATION NAME                             
         CLI   NETOPT,C'N'         NETWORK REQUEST?                             
         BE    *+8                 YES                                          
         LA    R1,GENVNAME         SPOT STATION NAME                            
         MVC   0(20,R1),ANAME      STATION/NETWORK NAME                         
         OC    0(20,R1),SPACES     SPACE PAD                                    
         DROP  RF                  DROP STATION ADDRESS RECORD USING            
*                                                                               
GNOUT49  EDIT  SRTGVGRS,(13,GENVGRSD),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENVGRSD    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENVGRSD(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGVNET,(13,GENVNETD),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENVNETD    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENVNETD(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGVTAX,(13,GENVTAXD),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENVTAXD    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENVTAXD(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGVGTX,(13,GENVTAXG),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENVTAXG    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENVTAXG(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGVNTX,(13,GENVTAXN),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENVTAXN    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENVTAXN(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGVCS2,(13,GENVCS2D),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENVCS2D    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         XC    GENVCS2D,GENVCS2D   YES - MAKE IT NULL (DON'T SEND)              
*                                                                               
         EDIT  SRTGVNET,(13,GENVNETD),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         MVC   GENVCOS2,SRTGCOS2   COST2 FLAG                                   
         CLI   NETOPT,C'N'         NETWORK REQUEST?                             
         BNE   GNOUT49A            NO                                           
         TM    SRTGSTA2,X'10'      COST2 BILL?                                  
         BZ    GNOUT49A            NO                                           
         MVI   GENVCOS2,C'Y'       YES - SET VENDOR LEVEL AS COST2              
*                                                                               
GNOUT49A LAY   RE,GVENTAB          FIRST VENDOR JSON TABLE                      
         CLI   FIRSTINV,C'Y'       FIRST TIME FOR THIS INVOICE?                 
         BE    *+10                YES                                          
         LAY   RE,GVENTAB2         NO - ADDITIONAL VENDOR JSON TABLE            
         ST    RE,DMCB+4           PARM 2                                       
         LAY   RE,GNVSOFT          VENDOR SOFT FIELDS TABLE                     
         ST    RE,DMCB+8           PARM 3                                       
***      LAY   RE,GENSFDVB         A(SOFT FIELDS)                               
         L     RE,BUYLIST          GENERIC EDI SOFT INV FIELDS BLOCK            
         AHI   RE,GENSFDHQ         BUMP TO EDI SOFT VENDOR FIELDS BLOCK         
         ST    RE,DMCB+12          PARM 4                                       
         LAY   RE,GVNEXL           VENDOR EXCLUSION TABLE                       
         ST    RE,DMCB+16          PARM 5                                       
*                                                                               
         GOTO1 =A(GNTABLE),DMCB,(RC)                                            
*                                                                               
         MVI   ONLYINVC,C'N'       INDICATE HAVE VENDOR INFO                    
*                                                                               
GNOUT49B MVI   FIRSTINV,C'N'       NO LONGER FIRST TIME FOR THIS INV            
*                                                                               
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   GNOUT10             NO - JUST GET NEXT SORT RECORD               
         TM    NETFLAGS,LASTNET    JUST PROCESSED LAST NET REC?                 
         BNZ   GNOUT60             YES - SEND END OF THIS JSON DATA NOW         
         LA    R5,SORTREC          POINT TO SORT RECORD                         
         MVC   SRTKSVGN,SRTGKEY    YES - SET SRTKSVGN                           
         MVC   SRTRCSGN,SORTREC    SAVE THE SORT RECORD                         
         MVC   SAVEBGRS,SRTGVGRS   INIT GROSS                                   
         MVC   SAVEBNET,SRTGVNET   INIT NET                                     
         MVC   SAVEBGTX,SRTGVGTX   INIT TAX GROSS                               
         MVC   SAVEBNTX,SRTGVNTX   INIT TAX NET                                 
         MVC   SAVEBCS2,SRTGVCS2   INIT COST2                                   
         NI    NETFLAGS,X'FF'-THISNET  NOT PROCEESED YET                        
         B     GNOUT10             GET NEXT SORT RECORD                         
*                                                                               
GNOUT50  CP    SORTCNT,=P'0'       DID WE PROCESS ANY RECORDS?                  
         BNE   GNOUT51             YES                                          
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   GNOUTXIT            NO - NOTHING IN THE PIPELINE TO SEND         
         TM    NETFLAGS,FIRSTNET   SOMETHING IN THE PIPELINE TO SEND?           
         BZ    GNOUTXIT            NO - NOTHING IN THE PIPELINE TO SEND         
*                                                                               
GNOUT51  CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   GNOUT60             NO - DONE                                    
         TM    NETFLAGS,THISNET    WAS LAST SORT REC PROCESSED?                 
         BNZ   GNOUT60             YES - DONE                                   
         OI    NETFLAGS,LASTNET    NO - SET LAST NET REC FLAG                   
         B     GNOUT12             REPORT LAST INVOICE                          
*                                                                               
GNOUT60  LAY   RE,GENDJSON         END JSON TABLE                               
         CLI   ONLYINVC,C'Y'       ONLY HAVE INVOICE INFO?                      
         BNE   *+10                NO                                           
         LAY   RE,GENDJSN2         YES - DON'T SEND CLOSING VENDOR "]"          
         ST    RE,DMCB+4           PARM 2                                       
         LA    RE,=X'FF'           NO SPECIAL TABLE                             
         ST    RE,DMCB+8           PARM 3                                       
         ST    RE,DMCB+12          PARM 4                                       
         ST    RE,DMCB+16          PARM 5                                       
*                                                                               
         GOTO1 =A(GNTABLE),DMCB,(RC)                                            
*                                                                               
         BRAS  RE,SENDMQ           SEND JSON PAYLOAD VIA MQ MSG                 
*                                                                               
         BRAS  RE,PRNTJSON         PRINT JSON DATA                              
*                                                                               
         BRAS  RE,OUTRECCL         CLEAR OUTREC                                 
*                                                                               
         EDIT  VENCOUNT,GENINCNT,0,COMMAS=YES,ALIGN=LEFT                        
*                                                                               
         L     R4,AOUTREC          R4 = A(OUTREC)                               
*                                                                               
         LAY   RE,GFINJSON         FINAL JSON TABLE                             
         ST    RE,DMCB+4           PARM 2                                       
         LAY   RE,GNHSOFT          INVOICE HEADER SOFT FIELDS TABLE             
         ST    RE,DMCB+8           PARM 3                                       
***      LAY   RE,GENSFDHB         A(SOFT FIELDS)                               
         L     RE,BUYLIST          A(SOFT FIELDS)                               
         ST    RE,DMCB+12          PARM 4                                       
         LA    RE,=X'FF'           NO SPECIAL TABLE                             
         ST    RE,DMCB+16          PARM 5                                       
*                                                                               
         GOTO1 =A(GNTABLE),DMCB,(RC)                                            
*                                                                               
         BRAS  RE,SENDMQ           SEND JSON PAYLOAD VIA MQ MSG                 
*                                                                               
         BRAS  RE,PRNTJSON         PRINT JSON DATA                              
*                                                                               
GNOUTXIT XIT1                      DONE                                         
*                                                                               
         DROP  R2,R3,R5            DROP USINGS                                  
*                                                                               
PRVTABG  DS    0CL28                                                            
         DC    C'BRITISH COLUMBIA    ',C'PST',C'7    '                          
         DC    C'ALBERTA             ',C'   ',C'     '                          
         DC    C'SASKATCHEWAN        ',C'PST',C'6    '                          
         DC    C'MANITOBA            ',C'PST',C'7    '                          
         DC    C'ONTARIO             ',C'HST',C'13   '                          
         DC    C'QUEBEC              ',C'QST',C'9.975'                          
         DC    C'NEW BRUNSWICK       ',C'HST',C'15   '                          
         DC    C'NOVA SCOTIA         ',C'HST',C'15   '                          
         DC    C'PRINCE EDWARD ISLAND',C'HST',C'15   '                          
         DC    C'NEWFOUNDLAND        ',C'HST',C'15   '                          
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
OUTRECCL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         XIT1                      EXIT                                         
*                                                                               
NOVENDOR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ABINREC          A(BINSRCH RECORDS)                           
         ICM   R2,15,ABINRECN      NUMBER OF RECORDS IN ABINREC                 
         BZ    NOVENDX             IF NO RECORDS, EXIT                          
         L     R3,ABINRECL         RECORD LENGTH (INCLUDING KEY)                
         LA    R4,SORTREC          R4 = SORTREC                                 
*                                                                               
NOVEND00 LA    R6,GEBKEYL(R5)      BUMP PAST KEY TO RECORD                      
         USING GEBRECD,R6          BINSRCH RECORD DSECT                         
         TM    GEBIFLAG,GEBIFVEN   DID THIS INVOICE HAVE VENDOR INFO?           
         BNZ   NOVEND05            YES                                          
*                                                                               
         USING SRTGRECD,R4         STATION BUCKET SORT RECORD DSECT             
         USING GEBKEYD,R5          BINSRCH KEY DSECT                            
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         MVC   SRTGAM,GEBAM        AGENCY/MEDIA                                 
         MVC   SRTGCLT,GEBCLT3     3-CHARACTER CLIENT                           
         MVC   SRTGPRD,GEBPRD      3-CHARACTER PRODUCT                          
         MVC   SRTGEST,GEBEST      BINARY ESTIMATE                              
         MVC   SRTGINV,GEBINVN     PACKED INVOICE NUMBER                        
         MVC   SRTGBDAT,GEBINVYM   BILLED DATE COMPRESSED                       
         MVC   SRTGMOS,GEBIMOS     MOS                                          
         MVC   SRTGBGRS,GEBGROSS   GROSS                                        
         MVC   SRTGBNET,GEBNET     NET                                          
         MVC   SRTGBACT,GEBACT     ACTUAL                                       
***      MVC   SRTGBCOM,GEBCOM     COMMISSION ADJUSTMENT                        
         MVC   SRTGBTAX,GEBTAX     TAX                                          
         MVC   SRTGBGST,GEBGST     GST                                          
         MVC   SRTGBPST,GEBPST     PST                                          
         MVC   SRTGBHST,GEBHST     HST                                          
         MVC   SRTGBPRV,GEBPROV    PROVINCE                                     
         MVC   SRTGBTYP,GEBTYPE    BILL TYPE (B1,B2,B3,B4,B5,ETC)               
         MVC   SRTGBINV,GEBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SRTGIDAT,GEBINVD    INVOICE RUN DATE                             
         MVC   SRTGIDDT,GEBINVDD   INVOICE DUE DATE                             
         MVC   SRTGSTAT,GEBSTAT    STATUS BYTE                                  
         MVC   SRTGSTA2,GEBSTAT2   STATUS BYTE2                                 
         MVC   SRTGSTA3,GEBSTAT3   BILL STATUS 3                                
         MVC   SRTGRTLF,GEBRETFL   RETAIL STATUS BITS                           
         MVC   SRTGRTLA,GEBRETAC   RETAIL ACCOUNT CODE                          
         MVC   SRTGWBFL,GEBWBFLT   WB FLIGHT CODE                               
         MVC   SRTGBCNT,GEBCNET    CALCULATED NET                               
         MVC   SRTGBMED,GEBQMED    MEDIA CODE                                   
         MVC   SRTGBSMD,GEBNMED    NETWORK SUB-MEDIA                            
*                                                                               
         MVC   WORK(2),GEBIMOS     YEAR/MONTH                                   
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTGBMOS)  MMM/YY                        
         CLI   WORK+1,12           SPECIAL PERIOD?                              
         BNH   NOVEND01            NO - LEAVE AS MMM/YY                         
         LLC   R1,WORK+1           SPECIAL PERIOD                               
         EDIT  (R1),(2,SRTGBMOS+1) MONTH (>12)                                  
         MVI   SRTGBMOS,C' '       WILL BE 13/YY FOR EXAMPLE                    
*                                                                               
NOVEND01 GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
NOVEND05 AR    R5,R3               BUMP TO NEXT INVOICE RECORD                  
         BCT   R2,NOVEND00         PROCESS NEXT INVOICE                         
*                                                                               
NOVENDX  XIT1                      EXIT                                         
*                                                                               
         DROP  R4,R5,R6            DROP USINGS                                  
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
TRFLAG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,X'80'            START WITH X'80'                             
         LR    RF,R4               FLAG FROM RECORD                             
         LA    R4,8                HAVE 8 FLAGS                                 
*                                                                               
TRFLG30  MVI   0(R3),C'N'          INIT STATUS TO AN 'N'                        
         EX    RE,*+8              EXECUTE THE TM                               
         B     *+8                 FOR IDF                                      
         TM    0(RF),0             STATUS MATCHES?                              
         BZ    *+8                 NO                                           
         MVI   0(R3),C'Y'          YES - THIS FLAG IS ON                        
         SRL   RE,1                SHIFT THE FLAG BIT OVER BY 1                 
         LA    R3,1(R3)            BUMP TO NEXT FIELD                           
         BCT   R4,TRFLG30          PROCESS THE NEXT STATUS BYTE                 
*                                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
PRNTJSON NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   RCWHATPR,2          SET TO SECOND SYSPRINT                       
         MVC   SVLINE,LINE         SAVE PRINT LINE FROM REPORT 1                
         MVC   SVFORCEH,FORCEHED   SAVE FORCEHED                                
         MVI   LINE,0              RESET LINE FOR SECOND SYSPRINT               
         MVI   FORCEHED,C'N'       NO HEADLINES FOR SECOND SYSPRINT             
*                                                                               
         L     R6,AOUTREC          RE = A(OUTREC)                               
         LAY   R7,OUTRECLN-17      LENGTH OF OUTREC-1                           
         AR    R7,R6               LAST BYTE OF OUTREC                          
*                                                                               
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         LA    R4,1(R7)            1 PAST LAST BYTE                             
*                                                                               
         L     R7,AOUTREC          RE = A(OUTREC)                               
         LR    R1,R7               SAVE A(OPENING TAG)                          
         MVI   INDENT,0            INIT INDENT                                  
*                                                                               
PJSON00  CR    R7,R4               EOF?                                         
         BE    PJSON100            YES - DONE                                   
         CLC   =C'",',0(R7)        PRINT JSON LINE WITH CURRENT INDENT?         
         BE    PJSON20             YES                                          
         CLC   =C'"}',0(R7)        PRINT JSON LINE WITH CURRENT INDENT?         
         BE    PJSON30             YES                                          
         CLI   0(R7),C'}'          CLOSING TAG?                                 
         BE    PJSON40             YES                                          
         CLI   0(R7),X'BB'         CLOSING ARRAY TAG (])?                       
         BE    PJSON40             YES                                          
         CLI   0(R7),C'{'          OPENING TAG?                                 
         BE    PJSON10             YES                                          
         CLI   0(R7),X'BA'         OPENING ARRAY TAG ([)?                       
         BNE   PJSON50             NO                                           
*                                                                               
PJSON10  BAS   RE,PJSNDATA         PRINT 1 LINE OF JSON DATA                    
         LA    R1,1(R7)            SAVE A(WHERE TO START PRINTING NEXT)         
*                                                                               
         LLC   RF,INDENT           CURRENT INDENTATION                          
         AHI   RF,2                ADD 2                                        
         STC   RF,INDENT           NEW CURRENT INDENTATION                      
         B     PJSON50             CONTINUE PROCESSING                          
*                                                                               
PJSON20  LA    R7,1(R7)            BUMP R7 PAST FIELD ENDING IN (")             
PJSON30  BAS   RE,PJSNDATA         PRINT 1 LINE OF JSON DATA                    
         LA    R1,1(R7)            SAVE A(WHERE TO START PRINTING NEXT)         
         B     PJSON50             CONTINUE PROCESSING                          
*                                                                               
PJSON40  CLI   1(R7),C','          COMMA?                                       
         BNE   *+8                 NO                                           
         LA    R7,1(R7)            YES - BUMP R7 PAST COMMA                     
*                                                                               
         LLC   RF,INDENT           CURRENT INDENTATION                          
         SHI   RF,2                SUBTRACT 2                                   
         STC   RF,INDENT           NEW CURRENT INDENTATION                      
*                                                                               
         BAS   RE,PJSNDATA         PRINT 1 LINE OF JSON DATA                    
         LA    R1,1(R7)            SAVE A(WHERE TO START PRINTING NEXT)         
*                                                                               
PJSON50  LA    R7,1(R7)            BUMP TO NEXT BYTE                            
         B     PJSON00             TEST NEXT BYTE                               
*                                                                               
PJSON100 GOTO1 REPORT              PRINT BLANK LINE                             
         MVI   RCWHATPR,1          RESET TO FIRST SYSPRINT                      
         MVC   LINE,SVLINE         RESTORE LINE                                 
         MVC   FORCEHED,SVFORCEH   RESTORE FORCEHED                             
*                                                                               
PJSNXIT  XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
PJSNDATA LR    R0,RE               SAVE OFF RE                                  
         LLC   RF,INDENT           CURRENT INDENTATION                          
         LA    RE,P1               PRINT LINE                                   
         AR    RE,RF               ADD INDENTATION                              
         LR    R2,R7               PRINT UP TO THIS ADDRESS                     
         SR    R2,R1               GET LENGTH OF DATA TO PRINT                  
         EX    R2,*+8              EXECUTE MVC                                  
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   0(0,RE),0(R1)       MOVE JSON DATA TO PRINT LINE                 
         CLI   0(RE),X'BB'         CLOSING ARRAY TAG (])?                       
         BNE   *+8                                                              
         MVI   0(RE),C']'          CLOSING ARRAY TAG (])?                       
***      CLI   0(R7),X'BA'         OPENING ARRAY TAG ([)?                       
***      BNE   *+8                                                              
***      MVI   0(RE),C'['          CLOSING ARRAY TAG (])?                       
         MVI   FORCEHED,C'N'       NO HEADLINES FOR SECOND SYSPRINT             
*                                                                               
         GOTO1 REPORT              PRINT JSON DATA TO SECOND SYSPRINT           
*                                                                               
         CLI   LINE,5              IS LINE > 5?                                 
         BNH   *+8                 NO                                           
         MVI   LINE,5              YES - SUPPRESS LINE BREAKS                   
*                                                                               
         LR    RE,R0               RESTORE RE                                   
         BR    RE                  RETURN TO CALLER                             
*                                                                               
PRTRPT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R7,P                POINT TO PRINT LINE                          
         USING BLINED,R7           BLINED DSECT TO COVER PRINT LINE             
         USING GENSFLDH,R2         GENERIC EDI SOFT INV FIELDS DSECT            
         USING SRTGRECD,R5         SRTGRECD DSECT                               
         MVC   BLMEDIA,SRTGBMED    MEDIA                                        
         MVC   BLCLT,SRTGCLT       CLIENT                                       
         MVC   BLCLNAME,GENCLTNM   CLIENT NAME                                  
         MVC   BLPRD,SRTGPRD       PRODUCT                                      
         EDIT  SRTGEST,(3,BLEST),FILL=0                                         
         MVC   BLINVNO,SRTGBINV    FULL INVOICE NUMBER                          
         MVC   BLINVMOS,SRTGBMOS   MOS MMM/YY                                   
         MVC   BLINVD,SRTGIDAT     INVOICE RUN DATE                             
         EDIT  SRTGBGRS,(13,BLGROSS),2,MINUS=YES                                
         MVC   BLSTA,SRTGSTA       STATION/VENDOR CODE                          
         OC    BLSTA,SPACES        SPACE PAD                                    
         EDIT  SRTGVGRS,(13,BLVAMT),2,MINUS=YES                                 
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R2,R5,R7            DROP USINGS                                  
*                                                                               
         XIT1                      DONE                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
SENDMQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   MQMSG,C'N'          SUPPRESS MQ NOTIFICATION?                    
         BE    SENDMQX             YES                                          
*                                                                               
         MVI   DMCB+8,X'E0'        SUPPRESS LEN, ADD CRLF & MSG HDR             
         LA    R1,DMCB             R1 = DMCB                                    
*                                                                               
         L     RF,VMASTC           A(MASTC)                                     
         USING MASTD,RF            MASTD DSECT                                  
         ICM   RE,15,MCSSB         HAVE SSB?                                    
         JZ    *+2                 NO - DEATH                                   
         USING SSBD,RE             SSBD DSECT                                   
         DROP  RF                  DROP MASTD USING                             
*                                                                               
         LA    RF,=C'GENERICEDI**** *'  MQ MESSAGE LABEL                        
*                                                                               
         CLI   SSODSPAC,C'A'       RUNNING IN PRODUCTION?                       
         BNE   SENDMQ10            NO                                           
         CLC   SAVEAGY,=C'SJ'      AGENCY SJ?                                   
         BNE   SENDMQ10            NO                                           
         MVC   10(4,RF),=C'DEMO'   CHG GENERICEDI**** TO GENERICEDIDEMO         
*                                                                               
SENDMQ10 MVC   14(1,RF),SSODSPAC   T(TST)/C(CSC)/Q(FQA)/A(PRODUCTION)           
         ST    RF,4(R1)            A(MQ MESSAGE LABEL) IN DMCB+4                
         MVI   4(R1),0             0 IN HOB OF P2                               
         DROP  RE                  DROP SSBD USING                              
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),,,0                                     
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         L     R6,AOUTREC          R4 = MQ MESSAGE BUILD AREA                   
         SR    R4,R6               JSON DATA LENGTH                             
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),(0,0(R6)),(R4),0                         
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
SENDMQX  XIT1                      DONE                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
GNHTAB   DC    X'00',X'01',C'{'                                                 
         DC    X'00',X'19',C'"messageType": "INVOICE",'                         
         DC    X'05',X'13',C'"exportDateTime": "'                               
         DC    X'00',X'02',C'",'                                                
         DC    X'02',X'0B',C'"system": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'01',X'0B',C'"userId": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'03',X'10',C'"agencyAlpha": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'04',X'0B',C'"flavor": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'26',X'0F',C'"indexCount": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'00',X'0A',C'"media": {'                                        
         DC    X'07',X'09',C'"code": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'08',X'09',C'"name": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'54',X'0D',C'"submedia": "'                                     
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'02',C'},'                                                
         DC    X'00',X'0B',C'"client": {'                                       
         DC    X'09',X'09',C'"code": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'0A',X'09',C'"name": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'32',X'16',C'"accountOfficeCode": "'                            
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'02',C'},'                                                
         DC    X'00',X'0C',C'"product": {'                                      
         DC    X'0B',X'09',C'"code": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'0C',X'09',C'"name": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'0D',X'0A',C'"udef1": "'                                        
         DC    X'00',X'02',C'",'                                                
         DC    X'0E',X'0A',C'"udef2": "'                                        
         DC    X'00',X'02',C'",'                                                
         DC    X'33',X'0F',C'"billToName": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'34',X'11',C'"addressLine1": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'35',X'11',C'"addressLine2": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'36',X'11',C'"addressLine3": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'37',X'15',C'"productInterface": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'38',X'0D',C'"division": "'                                     
         DC    X'00',X'02',C'",'                                                
         DC    X'39',X'0B',C'"ucomm1": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'3A',X'0B',C'"ucomm2": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'3B',X'0B',C'"ucomm3": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'3C',X'0B',C'"ucomm4": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'28',X'0A',C'"group": "'                                        
         DC    X'00',X'02',C'",'                                                
         DC    X'29',X'0F',C'"groupName1": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'30',X'0F',C'"groupName2": "'                                   
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'02',C'},'                                                
         DC    X'00',X'0D',C'"estimate": {'                                     
         DC    X'0F',X'09',C'"code": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'10',X'09',C'"name": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'11',X'0A',C'"udef1": "'                                        
         DC    X'00',X'02',C'",'                                                
         DC    X'12',X'0A',C'"udef2": "'                                        
         DC    X'00',X'02',C'",'                                                
         DC    X'13',X'0B',C'"ucomm1": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'14',X'0B',C'"ucomm2": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'15',X'0B',C'"ucomm3": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'16',X'0B',C'"ucomm4": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'3D',X'0B',C'"ucomm5": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'3E',X'0B',C'"ucomm6": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'3F',X'0B',C'"ucomm7": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'40',X'0B',C'"ucomm8": "'                                       
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'02',C'},'                                                
         DC    X'00',X'0C',C'"invoice": {'                                      
         DC    X'17',X'13',C'"monthOfService": "'                               
         DC    X'00',X'02',C'",'                                                
         DC    X'18',X'13',C'"invoiceRunDate": "'                               
         DC    X'00',X'02',C'",'                                                
         DC    X'31',X'13',C'"invoiceDueDate": "'                               
         DC    X'00',X'02',C'",'                                                
         DC    X'19',X'0F',C'"invoiceNum": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'1A',X'0D',C'"grossAmt": "'                                     
         DC    X'00',X'02',C'",'                                                
         DC    X'1B',X'0B',C'"netAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'1C',X'0E',C'"actualAmt": "'                                    
         DC    X'00',X'02',C'",'                                                
         DC    X'1D',X'12',C'"commissionAmt": "'                                
         DC    X'00',X'02',C'",'                                                
         DC    X'1E',X'0B',C'"taxAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'25',X'0D',C'"billType": "'                                     
         DC    X'00',X'02',C'",'                                                
         DC    X'41',X'13',C'"sepAdjBillFlag": "'                               
         DC    X'00',X'02',C'",'                                                
         DC    X'42',X'13',C'"manualBillFlag": "'                               
         DC    X'00',X'02',C'",'                                                
         DC    X'43',X'10',C'"aorBillFlag": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'44',X'16',C'"aorClientBillFlag": "'                            
         DC    X'00',X'02',C'",'                                                
***      DC    X'45',X'17',C'"noTaxInAorCalcFlag": "'                           
***      DC    X'00',X'02',C'",'                                                
         DC    X'46',X'17',C'"commissionOnlyFlag": "'                           
         DC    X'00',X'02',C'",'                                                
         DC    X'47',X'1A',C'"sepCommissionBillFlag": "'                        
         DC    X'00',X'02',C'",'                                                
         DC    X'48',X'10',C'"netBillFlag": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'49',X'19',C'"retailCorpOutletFlag": "'                         
         DC    X'00',X'02',C'",'                                                
         DC    X'4A',X'18',C'"retailRegOutletFlag": "'                          
         DC    X'00',X'02',C'",'                                                
         DC    X'4B',X'1A',C'"retailControlBillFlag": "'                        
         DC    X'00',X'02',C'",'                                                
         DC    X'4C',X'1A',C'"retailSummaryBillFlag": "'                        
         DC    X'00',X'02',C'",'                                                
         DC    X'4D',X'16',C'"retailAccountCode": "'                            
         DC    X'00',X'02',C'",'                                                
         DC    X'4E',X'14',C'"groupMTradeFlag": "'                              
         DC    X'00',X'02',C'",'                                                
         DC    X'4F',X'1B',C'"groupMTradeCalcNetFlag": "'                       
         DC    X'00',X'02',C'",'                                                
         DC    X'50',X'19',C'"groupMTradeMidasFlag": "'                         
         DC    X'00',X'02',C'",'                                                
         DC    X'51',X'11',C'"wbFlightCode": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'52',X'15',C'"calculatedNetAmt": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'53',X'0E',C'"cost2Flag": "'                                    
         DC    X'00',X'02',C'",'                                                
         DC    X'55',X'0D',C'"costType": "'                                     
         DC    X'00',X'02',C'",'                                                
         DC    X'56',X'10',C'"packageName": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'57',X'12',C'"daypartFilter": "'                                
         DC    X'00',X'02',C'",'                                                
         DC    X'58',X'0F',C'"packageNum": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'1F',X'0B',C'"gstAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'20',X'0C',C'"gstRate": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'21',X'0B',C'"pstAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'22',X'0C',C'"pstRate": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'23',X'0B',C'"hstAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'59',X'0C',C'"hstRate": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'27',X'0C',C'"taxType": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'24',X'0D',C'"province": "'                                     
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'02',C'},'                                                
         DC    X'FF'                                                            
***                                                                             
* THE TABLE BELIW IS AN EXCLUSION TABLE                                         
* THIS INDICATES THAT IF THE FIRST BYTE OF THE GNHTAB TABLE                     
* ENTRY MATCHES THE FIRST BYTE OF THIS TABLE ENTRY, WE NEED                     
* TO EXCLUDE THAT FIELD IF THE BITS IN THE SECOND BYTE DON'T MATCH              
* THE FOLLOWING                                                                 
* X'01' = CANADIAN FIELD                                                        
* X'02' = SPOT FIELD                                                            
* X'04' = NET FIELD                                                             
***                                                                             
***********************************************************************         
* THE TABLE BELOW IS AN EXCLUSION TABLE - HERE IS HOW THIS WORKS      *         
*                                                                     *         
* IF( 1ST BYTE OF GNHTAB ENTRY = 1ST BYTE OF GNHEXL ENTRY ){          *         
*   SEE CODE IN EXCLD10                                               *         
* }                                                                   *         
*                                                                     *         
* IF 2ND BYTE OF TABLE HAS THE X'01' BIT SET = SPOT CANADIAN FIELD    *         
* IF 2ND BYTE OF TABLE HAS THE X'02' BIT SET = SPOT US FIELD          *         
* IF 2ND BYTE OF TABLE HAS THE X'04' BIT SET = NET US FIELD           *         
***********************************************************************         
GNHEXL   DC    X'38',X'03'   division               = SPOT or CANADIAN          
         DC    X'49',X'03'   retailCorpOutletFlag   = SPOT or CANADIAN          
         DC    X'4A',X'03'   retailRegOutletFlag    = SPOT or CANADIAN          
         DC    X'4B',X'03'   retailControlBillFlag  = SPOT or CANADIAN          
         DC    X'4C',X'03'   retailSummaryBillFlag  = SPOT or CANADIAN          
         DC    X'4D',X'03'   retailAccountCode      = SPOT or CANADIAN          
         DC    X'4E',X'03'   groupMTradeFlag        = SPOT or CANADIAN          
         DC    X'4F',X'03'   groupMTradeCalcNetFlag = SPOT or CANADIAN          
         DC    X'50',X'03'   groupMTradeMidasFlag   = SPOT or CANADIAN          
         DC    X'52',X'03'   calculatedNetAmt       = SPOT or CANADIAN          
         DC    X'54',X'04'   submedia               = NET                       
         DC    X'55',X'04'   costType               = NET                       
         DC    X'56',X'04'   packageName            = NET                       
         DC    X'57',X'04'   daypartFilter          = NET                       
         DC    X'58',X'04'   packageNum             = NET                       
         DC    X'1F',X'01'   gstAmt                 = CANADIAN                  
         DC    X'20',X'01'   gstRate                = CANADIAN                  
         DC    X'21',X'01'   pstAmt                 = CANADIAN                  
         DC    X'22',X'01'   pstRate                = CANADIAN                  
         DC    X'23',X'01'   hstAmt                 = CANADIAN                  
         DC    X'59',X'01'   hstRate                = CANADIAN                  
         DC    X'27',X'01'   taxType                = CANADIAN                  
         DC    X'24',X'01'   province               = CANADIAN                  
         DC    X'1E',X'02'   taxAmt                 = SPOT US ONLY              
         DC    X'FF'                                                            
*                                                                               
GVENTAB  DC    X'00',X'0B',C'"vendors": '                                       
         DC    X'00',X'01',X'BA'                THIS IS A "["                   
         DC    X'00',X'01',C'{'                                                 
         DC    X'02',X'0F',C'"marketCode": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'03',X'0F',C'"marketName": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'08',X'10',C'"marketGroup": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'09',X'15',C'"marketGroupName1": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'0A',X'15',C'"marketGroupName2": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'0B',X'15',C'"marketGroupName3": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'0C',X'11',C'"marketUcomm1": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'0D',X'11',C'"marketUcomm2": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'0E',X'11',C'"marketUcomm3": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'0F',X'11',C'"marketUcomm4": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'04',X'10',C'"stationCode": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'01',X'10',C'"stationName": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'10',X'10',C'"networkCode": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'11',X'10',C'"networkName": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'05',X'0D',C'"grossAmt": "'                                     
         DC    X'00',X'02',C'",'                                                
         DC    X'06',X'0B',C'"netAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'07',X'0B',C'"taxAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'14',X'10',C'"taxGrossAmt": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'15',X'0E',C'"taxNetAmt": "'                                    
         DC    X'00',X'02',C'",'                                                
         DC    X'12',X'0E',C'"cost2Flag": "'                                    
         DC    X'00',X'02',C'",'                                                
         DC    X'13',X'0D',C'"cost2Amt": "'                                     
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'01',C'}'                                                 
         DC    X'FF'                                                            
*                                                                               
GVENTAB2 DC    X'00',X'01',C','                                                 
         DC    X'00',X'01',C'{'                                                 
         DC    X'02',X'0F',C'"marketCode": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'03',X'0F',C'"marketName": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'08',X'10',C'"marketGroup": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'09',X'15',C'"marketGroupName1": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'0A',X'15',C'"marketGroupName2": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'0B',X'15',C'"marketGroupName3": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'0C',X'11',C'"marketUcomm1": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'0D',X'11',C'"marketUcomm2": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'0E',X'11',C'"marketUcomm3": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'0F',X'11',C'"marketUcomm4": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'04',X'10',C'"stationCode": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'01',X'10',C'"stationName": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'10',X'10',C'"networkCode": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'11',X'10',C'"networkName": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'05',X'0D',C'"grossAmt": "'                                     
         DC    X'00',X'02',C'",'                                                
         DC    X'06',X'0B',C'"netAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'07',X'0B',C'"taxAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'14',X'10',C'"taxGrossAmt": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'15',X'0E',C'"taxNetAmt": "'                                    
         DC    X'00',X'02',C'",'                                                
         DC    X'12',X'0E',C'"cost2Flag": "'                                    
         DC    X'00',X'02',C'",'                                                
         DC    X'13',X'0D',C'"cost2Amt": "'                                     
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'01',C'}'                                                 
         DC    X'FF'                                                            
***********************************************************************         
* THE TABLE BELOW IS AN EXCLUSION TABLE - HERE IS HOW THIS WORKS      *         
*                                                                     *         
* IF( 1ST BYTE OF GVENTAB/2 ENTRY = 1ST BYTE OF GVNEXL ENTRY ){       *         
*   SEE CODE IN EXCLD10                                               *         
* }                                                                   *         
*                                                                     *         
* IF 2ND BYTE OF TABLE HAS THE X'01' BIT SET = SPOT CANADIAN FIELD    *         
* IF 2ND BYTE OF TABLE HAS THE X'02' BIT SET = SPOT US FIELD          *         
* IF 2ND BYTE OF TABLE HAS THE X'04' BIT SET = NET US FIELD           *         
***********************************************************************         
GVNEXL   DC    X'08',X'03'   marketGroup            = SPOT USA or CAN           
         DC    X'09',X'03'   marketGroupName1       = SPOT USA or CAN           
         DC    X'0A',X'03'   marketGroupName2       = SPOT USA or CAN           
         DC    X'0B',X'03'   marketGroupName3       = SPOT USA or CAN           
         DC    X'0C',X'03'   marketUCOMM1           = SPOT USA or CAN           
         DC    X'0D',X'03'   marketUCOMM2           = SPOT USA or CAN           
         DC    X'0E',X'03'   marketUCOMM3           = SPOT USA or CAN           
         DC    X'0F',X'03'   marketUCOMM4           = SPOT USA or CAN           
         DC    X'04',X'03'   stationCode            = SPOT USA or CAN           
         DC    X'01',X'03'   stationName            = SPOT USA or CAN           
         DC    X'10',X'04'   networkCode            = NET ONLY                  
         DC    X'11',X'04'   networkName            = NET ONLY                  
         DC    X'13',X'04'   cost2Amt               = NET ONLY                  
         DC    X'14',X'04'   taxAmtGross            = NET ONLY                  
         DC    X'15',X'04'   taxAmtNet              = NET ONLY                  
         DC    X'07',X'02'   taxAmt                 = SPOT USA ONLY             
         DC    X'FF'                                                            
*                                                                               
GENDJSON DC    X'00',X'01',X'BB'                THIS IS A "]"                   
GENDJSN2 DC    X'00',X'01',C'}'                                                 
         DC    X'FF'                                                            
*                                                                               
GFINJSON DC    X'00',X'01',C'{'                                                 
         DC    X'00',X'17',C'"messageType": "GROUP",'                           
         DC    X'05',X'13',C'"exportDateTime": "'                               
         DC    X'00',X'02',C'",'                                                
         DC    X'02',X'0B',C'"system": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'01',X'0B',C'"userId": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'03',X'10',C'"agencyAlpha": "'                                  
         DC    X'00',X'02',C'",'                                                
         DC    X'04',X'0B',C'"flavor": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'26',X'0F',C'"indexCount": "'                                   
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'01',C'}'                                                 
         DC    X'FF'                                                            
***                                                                             
* GENERIC EDI INVOICE HEADER SOFT FIELDS TABLE                                  
* THIS MAPS AGAINST THE FIRST BYTE OF GNHTAB AND IF IT MATCHES                  
* WE INDEX INTO GENSFLDH (BUILT IN GENSFDHB) USING THE DISPLACEMENT             
* IN THE SECOND FIELD FOR A PRE-DEFINED LENGTH (3RD FIELD)                      
* WE DO THIS IN ORDER TO HANDLE DATA THAT IS SOFT & VARIABLE IN LENGTH          
*                                                                               
* FORMAT IS XL1 - FIELD NUMBER (MATCHED AGAINST GNHTAB)                         
*           AL1 - DISPLACEMENT INTO GENSFLDH IN GENSFDHB                        
*           AL1 - MAX LENGTH OF SOFT DATA                                       
***                                                                             
GNHSOFT  DC    X'01',AL2(GENUSRID-GENSFLDH),AL1(L'GENUSRID)                     
         DC    X'02',AL2(GENSYSTM-GENSFLDH),AL1(L'GENSYSTM)                     
         DC    X'03',AL2(GENAGNCY-GENSFLDH),AL1(L'GENAGNCY)                     
         DC    X'04',AL2(GENFRMAT-GENSFLDH),AL1(L'GENFRMAT)                     
         DC    X'05',AL2(GENCRDTM-GENSFLDH),AL1(L'GENCRDTM)                     
GNHCLEAR DC    X'07',AL2(GENMEDCD-GENSFLDH),AL1(L'GENMEDCD)                     
         DC    X'08',AL2(GENMEDNM-GENSFLDH),AL1(L'GENMEDNM)                     
         DC    X'09',AL2(GENCLTCD-GENSFLDH),AL1(L'GENCLTCD)                     
         DC    X'0A',AL2(GENCLTNM-GENSFLDH),AL1(L'GENCLTNM)                     
         DC    X'0B',AL2(GENPRDCD-GENSFLDH),AL1(L'GENPRDCD)                     
         DC    X'0C',AL2(GENPRDNM-GENSFLDH),AL1(L'GENPRDNM)                     
         DC    X'0D',AL2(GENPRUS1-GENSFLDH),AL1(L'GENPRUS1)                     
         DC    X'0E',AL2(GENPRUS2-GENSFLDH),AL1(L'GENPRUS2)                     
         DC    X'0F',AL2(GENESTCD-GENSFLDH),AL1(L'GENESTCD)                     
         DC    X'10',AL2(GENESTNM-GENSFLDH),AL1(L'GENESTNM)                     
         DC    X'11',AL2(GENESUS1-GENSFLDH),AL1(L'GENESUS1)                     
         DC    X'12',AL2(GENESUS2-GENSFLDH),AL1(L'GENESUS2)                     
         DC    X'13',AL2(GENESUC1-GENSFLDH),AL1(L'GENESUC1)                     
         DC    X'14',AL2(GENESUC2-GENSFLDH),AL1(L'GENESUC2)                     
         DC    X'15',AL2(GENESUC3-GENSFLDH),AL1(L'GENESUC3)                     
         DC    X'16',AL2(GENESUC4-GENSFLDH),AL1(L'GENESUC4)                     
         DC    X'17',AL2(GENINMOS-GENSFLDH),AL1(L'GENINMOS)                     
         DC    X'18',AL2(GENINVDT-GENSFLDH),AL1(L'GENINVDT)                     
         DC    X'19',AL2(GENINVNO-GENSFLDH),AL1(L'GENINVNO)                     
         DC    X'1A',AL2(GENINGRS-GENSFLDH),AL1(L'GENINGRS)                     
         DC    X'1B',AL2(GENINNET-GENSFLDH),AL1(L'GENINNET)                     
         DC    X'1C',AL2(GENINACT-GENSFLDH),AL1(L'GENINACT)                     
         DC    X'1D',AL2(GENINCOM-GENSFLDH),AL1(L'GENINCOM)                     
         DC    X'1E',AL2(GENINTAX-GENSFLDH),AL1(L'GENINTAX)                     
         DC    X'1F',AL2(GENINGST-GENSFLDH),AL1(L'GENINGST)                     
         DC    X'20',AL2(GENINGSR-GENSFLDH),AL1(L'GENINGSR)                     
         DC    X'21',AL2(GENINPST-GENSFLDH),AL1(L'GENINPST)                     
         DC    X'22',AL2(GENINPSR-GENSFLDH),AL1(L'GENINPSR)                     
         DC    X'23',AL2(GENINHST-GENSFLDH),AL1(L'GENINHST)                     
         DC    X'59',AL2(GENINHSR-GENSFLDH),AL1(L'GENINHSR)                     
         DC    X'24',AL2(GENINPRV-GENSFLDH),AL1(L'GENINPRV)                     
         DC    X'25',AL2(GENINTYP-GENSFLDH),AL1(L'GENINTYP)                     
         DC    X'26',AL2(GENINCNT-GENSFLDH),AL1(L'GENINCNT)                     
         DC    X'27',AL2(GENINTXT-GENSFLDH),AL1(L'GENINTXT)                     
         DC    X'28',AL2(GENPRGRP-GENSFLDH),AL1(L'GENPRGRP)                     
         DC    X'29',AL2(GENPGRPN-GENSFLDH),AL1(L'GENPGRPN)                     
         DC    X'30',AL2(GENPGRN2-GENSFLDH),AL1(L'GENPGRN2)                     
         DC    X'31',AL2(GENINVDD-GENSFLDH),AL1(L'GENINVDD)                     
         DC    X'32',AL2(GENCLTAO-GENSFLDH),AL1(L'GENCLTAO)                     
         DC    X'33',AL2(GENPRDA1-GENSFLDH),AL1(L'GENPRDA1)                     
         DC    X'34',AL2(GENPRDA2-GENSFLDH),AL1(L'GENPRDA2)                     
         DC    X'35',AL2(GENPRDA3-GENSFLDH),AL1(L'GENPRDA3)                     
         DC    X'36',AL2(GENPRDA4-GENSFLDH),AL1(L'GENPRDA4)                     
         DC    X'37',AL2(GENPRDIC-GENSFLDH),AL1(L'GENPRDIC)                     
         DC    X'38',AL2(GENPRDDV-GENSFLDH),AL1(L'GENPRDDV)                     
         DC    X'39',AL2(GENPRDU1-GENSFLDH),AL1(L'GENPRDU1)                     
         DC    X'3A',AL2(GENPRDU2-GENSFLDH),AL1(L'GENPRDU2)                     
         DC    X'3B',AL2(GENPRDU3-GENSFLDH),AL1(L'GENPRDU3)                     
         DC    X'3C',AL2(GENPRDU4-GENSFLDH),AL1(L'GENPRDU4)                     
         DC    X'3D',AL2(GENESUC5-GENSFLDH),AL1(L'GENESUC5)                     
         DC    X'3E',AL2(GENESUC6-GENSFLDH),AL1(L'GENESUC6)                     
         DC    X'3F',AL2(GENESUC7-GENSFLDH),AL1(L'GENESUC7)                     
         DC    X'40',AL2(GENESUC8-GENSFLDH),AL1(L'GENESUC8)                     
         DC    X'41',AL2(GENSADJB-GENSFLDH),AL1(L'GENSADJB)                     
         DC    X'42',AL2(GENMANBL-GENSFLDH),AL1(L'GENMANBL)                     
         DC    X'43',AL2(GENTAORB-GENSFLDH),AL1(L'GENTAORB)                     
         DC    X'44',AL2(GENCAORB-GENSFLDH),AL1(L'GENCAORB)                     
***      DC    X'45',AL2(GENNTAOR-GENSFLDH),AL1(L'GENNTAOR)                     
         DC    X'46',AL2(GENCOMBL-GENSFLDH),AL1(L'GENCOMBL)                     
         DC    X'47',AL2(GENCOMSB-GENSFLDH),AL1(L'GENCOMSB)                     
         DC    X'48',AL2(GENNETBL-GENSFLDH),AL1(L'GENNETBL)                     
         DC    X'49',AL2(GENRETCO-GENSFLDH),AL1(L'GENRETCO)                     
         DC    X'4A',AL2(GENRETRO-GENSFLDH),AL1(L'GENRETRO)                     
         DC    X'4B',AL2(GENRETCB-GENSFLDH),AL1(L'GENRETCB)                     
         DC    X'4C',AL2(GENRETSB-GENSFLDH),AL1(L'GENRETSB)                     
         DC    X'4D',AL2(GENRETAC-GENSFLDH),AL1(L'GENRETAC)                     
         DC    X'4E',AL2(GENGMTRD-GENSFLDH),AL1(L'GENGMTRD)                     
         DC    X'4F',AL2(GENGMTCN-GENSFLDH),AL1(L'GENGMTCN)                     
         DC    X'50',AL2(GENGMTDM-GENSFLDH),AL1(L'GENGMTDM)                     
         DC    X'51',AL2(GENWBFLT-GENSFLDH),AL1(L'GENWBFLT)                     
         DC    X'52',AL2(GENINNTC-GENSFLDH),AL1(L'GENINNTC)                     
         DC    X'53',AL2(GENCOST2-GENSFLDH),AL1(L'GENCOST2)                     
         DC    X'54',AL2(GENINSMD-GENSFLDH),AL1(L'GENINSMD)                     
         DC    X'55',AL2(GENCTYPE-GENSFLDH),AL1(L'GENCTYPE)                     
         DC    X'56',AL2(GENPKGNM-GENSFLDH),AL1(L'GENPKGNM)                     
         DC    X'57',AL2(GENDPTFL-GENSFLDH),AL1(L'GENDPTFL)                     
         DC    X'58',AL2(GENPKGNO-GENSFLDH),AL1(L'GENPKGNO)                     
         DC    X'FF'                                                            
***                                                                             
* GENERIC EDI VENDOR LEVEL SOFT FIELDS TABLE                                    
* THIS MAPS AGAINST THE FIRST BYTE OF GVENTAB AND IF IT MATCHES                 
* WE INDEX INTO GENSFLDV (BUILT IN GENSFDVB) USING THE DISPLACEMENT             
* IN THE SECOND FIELD FOR A PRE-DEFINED LENGTH (3RD FIELD)                      
* WE DO THIS IN ORDER TO HANDLE DATA THAT IS SOFT & VARIABLE IN LENGTH          
*                                                                               
* FORMAT IS XL1 - FIELD NUMBER (MATCHED AGAINST GVENTAB)                        
*           AL1 - DISPLACEMENT INTO GENSFLDV IN GENSFDVB                        
*           AL1 - MAX LENGTH OF SOFT DATA                                       
***                                                                             
GNVSOFT  DC    X'01',AL2(GENVNAME-GENSFLDV),AL1(L'GENVNAME)                     
         DC    X'02',AL2(GENVMKCD-GENSFLDV),AL1(L'GENVMKCD)                     
         DC    X'03',AL2(GENVMKNM-GENSFLDV),AL1(L'GENVMKNM)                     
         DC    X'08',AL2(GENVMGRP-GENSFLDV),AL1(L'GENVMGRP)                     
         DC    X'09',AL2(GENVMGN1-GENSFLDV),AL1(L'GENVMGN1)                     
         DC    X'0A',AL2(GENVMGN2-GENSFLDV),AL1(L'GENVMGN2)                     
         DC    X'0B',AL2(GENVMGN3-GENSFLDV),AL1(L'GENVMGN3)                     
         DC    X'0C',AL2(GENVMUC1-GENSFLDV),AL1(L'GENVMUC1)                     
         DC    X'0D',AL2(GENVMUC2-GENSFLDV),AL1(L'GENVMUC2)                     
         DC    X'0E',AL2(GENVMUC3-GENSFLDV),AL1(L'GENVMUC3)                     
         DC    X'0F',AL2(GENVMUC4-GENSFLDV),AL1(L'GENVMUC4)                     
         DC    X'04',AL2(GENVSTAN-GENSFLDV),AL1(L'GENVSTAN)                     
         DC    X'05',AL2(GENVGRSD-GENSFLDV),AL1(L'GENVGRSD)                     
         DC    X'06',AL2(GENVNETD-GENSFLDV),AL1(L'GENVNETD)                     
         DC    X'07',AL2(GENVTAXD-GENSFLDV),AL1(L'GENVTAXD)                     
         DC    X'10',AL2(GENVNTWK-GENSFLDV),AL1(L'GENVNTWK)                     
         DC    X'11',AL2(GENVNTWN-GENSFLDV),AL1(L'GENVNTWN)                     
         DC    X'12',AL2(GENVCOS2-GENSFLDV),AL1(L'GENVCOS2)                     
         DC    X'13',AL2(GENVCS2D-GENSFLDV),AL1(L'GENVCS2D)                     
         DC    X'14',AL2(GENVTAXG-GENSFLDV),AL1(L'GENVTAXG)                     
         DC    X'15',AL2(GENVTAXN-GENSFLDV),AL1(L'GENVTAXN)                     
         DC    X'FF'                                                            
*                                                                               
         TITLE 'BLBILLR - PROCESS BILL RECORD FOR BELL'                         
BLBILLR  CSECT                                                                  
         NMOD1 0,BLBILLR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LA    R0,BINKEYBL         BINSRCH KEY                                  
         ST    R0,ABINKEY          A(BINSRCH KEY)                               
*                                                                               
         LA    R5,BINKEYBL         R5 = BINSRCH KEY                             
         USING BLBKEYD,R5          BILLING SORT KEY DSECT                       
         USING BILLREC,R6          BILL RECORD USING                            
         MVC   BLBAM,BKEYAM        AGY/MED                                      
         MVC   BLBCLT,BKEYCLT      PACKED CLIENT CODE                           
         MVC   BLBPRD,BKEYPRD      PRODUCT CODE                                 
         MVC   BLBEST,BKEYEST      ESTIMATE                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(2,BLBINVYM) INV YYMMDD                    
         MVC   BLBINVN,BKEYINV     INVOICE NUMBER PACKED                        
         DROP  R5                  DROP BILLING SORT KEY USING                  
*                                                                               
         LA    R5,BINRECBL         R5 = BINSRCH RECORD                          
         USING BLBRECD,R5          BILL RECORD DATA DSECT                       
         MVC   BLBREC,SPACES       SPACE-FILL OUTPUT RECORD                     
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
*                                                                               
         MVC   BLBNET,SPBVNETP     NET AMOUNT                                   
*                                                                               
         L     R0,SPBVGST          GST FROM SPBVAL                              
         CVD   R0,DUB              CVD                                          
         ZAP   BLBGST,DUB          GST - PACKED                                 
*                                                                               
         L     R0,SPBVPST          PST FROM SPBVAL                              
         CVD   R0,DUB              CVD                                          
         ZAP   BLBPST,DUB          PST - PACKED                                 
*                                                                               
         LA    R1,B1PROF           B1 PROFILE                                   
         ST    R1,DMCB+8           PARM 3                                       
         MVC   DMCB+8(1),MED       MEDIA IS HOB OF PARM 4                       
*                                                                               
         LA    R1,B1XPROF          B1X PROFILE                                  
         ST    R1,DMCB+12          PARM 4                                       
         ST    R6,DMCB+16          A(BILL HEADER RECORD)                        
*                                                                               
         GOTO1 AFMTINO,DMCB,(C'B',BDATE),(2,BKEYINV)                            
*                                                                               
         L     RF,DMCB             RF = DMCB                                    
         MVC   BLBINV,0(RF)        FULL INVOICE NUMBER                          
*                                                                               
         MVC   BLBTYPE,BTYPE       BILL TYPE (B1,B2,B3,B4,B5,ETC)               
*                                                                               
         GOTO1 DATCON,DMCB,(0,BQDATE),(20,WORK) YYYYMMDD                        
*                                                                               
         LA    R4,BLBINVD          INVOICE DATE                                 
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         GOTO1 =A(CKPRD),DMCB,(RC) MAY NEED TO RE-READ PRODUCT                  
*                                                                               
         L     RF,ADPRD            A(PRODUCT RECORD)                            
         USING PRDHDR,RF           PRODUCT RECORD DSECT                         
         MVC   BLBPNAME,PNAME      PRODUCT NAME                                 
         OC    BLBPNAME,SPACES     SPACE PAD                                    
         DROP  RF                  DROP PRODUCT RECORD USING                    
*                                                                               
         GOTO1 =A(READPGRP),DMCB,(RC) READ PRODUCT GROUP RECORD                 
*                                                                               
         MVC   BLBPGRP,SVPGRP      PRODUCT GROUP CODE                           
         MVC   BLBPGRPN,SVPGRPN    PRODUCT GROUP NAME                           
*                                                                               
         MVI   CKESTREC,C'L'       FLAG TO READ EST FROM BILL KEY               
         GOTO1 =A(CKEST),DMCB,(RC) MAY NEED TO RE-READ ESTIMATE                 
*                                                                               
         L     RF,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDR,RF           ESTIMATE RECORD DSECT                        
         MVC   BLBENAME,EDESC      ESTIMATE NAME                                
         DROP  RF                  DROP EST REC & ULBRECD USING                 
*                                                                               
         GOTO1 =A(READUCOM),DMCB,(RC) READ UCOMM DATA                           
*                                                                               
         MVC   BLBEUCOM,SVEUCOM1   ESTIMATE UCOMM 1                             
         MVC   BLBEUCM3,SVEUCOM3   ESTIMATE UCOMM 3                             
         MVC   BLBEUCM4,SVEUCOM4   ESTIMATE UCOMM 4                             
*                                                                               
         DROP  R5                  DROP BILL SORT USING                         
*                                                                               
         ZAP   DUB,BACTP           GET BACTP INTO DUB                           
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) TOTAL               
*                                                                               
         MVI   ABINKEY,1           RESET TO INSERT RECORD IF NOT FOUND          
         GOTO1 VBINSRCH,ABINKEY    CALL BINSRCH                                 
*                                                                               
         CLI   ABINKEY,1           RECORD INSERTED?                             
         BE    BLBXIT              YES - DONE                                   
*                                                                               
         OC    1(3,R1),1(R1)       HAVE A(BINSRCH REC)?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - TABLE IS FULL - EXPAND TABLE!           
*                                                                               
         L     RF,ABINKEY          ADDRESS OF FOUND RECORD                      
         LA    RF,L'BLBKEY(RF)     PAST KEY                                     
         USING BLBRECD,RF          BILL SORT RECORD DSECT                       
         AP    BLBNET,SPBVNETP     NET AMOUNT (DUP KEY MUST ADD FIELD)          
*                                                                               
         L     R0,SPBVGST          GST FROM SPBVAL                              
         CVD   R0,DUB              CVD                                          
         AP    BLBGST,DUB          GST - PACKED                                 
*                                                                               
         L     R0,SPBVPST          PST FROM SPBVAL                              
         CVD   R0,DUB              CVD                                          
         AP    BLBPST,DUB          PST - PACKED                                 
         DROP  RF                  DROP BILL SORT USING                         
*                                                                               
BLBXIT   AP    INVCNT,=P'1'        UPDATE INVOICE COUNT                         
         XIT1                      EXIT                                         
         DROP  R6                  DROP BILL RECORD USING                       
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'BLBILHR - PROCESS STA BUCKET ELEMENT FOR BELL'                  
BLBILHR  CSECT                                                                  
         NMOD1 0,BLBILHR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
         USING STABUCKD,R6         STATION BUCKET KEY DSECT                     
         USING STABELEM,R7         BILLING ELEMENT DSECT                        
*                                                                               
         LA    R5,BINKEYBL         R5 = BINSRCH KEY                             
         USING BLBKEYD,R5          BILL BINSRCH KEY DSECT                       
         XC    BINKEYBL,BINKEYBL   CLEAR BINSRCH KEY                            
         MVC   BLBAM,STABKAM       AGENCY/MEDIA                                 
         MVC   BLBCLT,STABKCLT     PACKED CLIENT                                
         MVC   BLBPRD,SAVEPRD      3-CHAR PRODUCT                               
         MVC   BLBEST,STABKEST     BINARY ESTIMATE                              
         MVC   BLBINVYM,STABBDT    DATE OF BILLING COMPRESSED                   
         MVC   BLBINVN,STABINV     PACKED INVOICE NUMBER                        
         NI    BLBINVN,X'FF'-X'C0' STRIP REVERSED & REVERSAL FLAGS              
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         ST    R5,ABINKEY          A(BINSRCH KEY)                               
         MVI   ABINKEY,0           SET TO SEARCH ONLY - DON'T ADD!              
*                                                                               
         GOTO1 VBINSRCH,ABINKEY    BILL BINSRCH ENTRY FOR THIS ELEM             
*                                                                               
         CLI   ABINKEY,X'01'       RECORD NOT FOUND FLAG SET?                   
         BNE   *+6                 NO                                           
         DC    H'0'                REC NOT FOUND - SOMETHING IS WRONG!          
*                                                                               
         L     R5,ABINKEY          ADDRESS OF BINSRCH RECORD                    
         LA    R5,BLBKEYL(R5)      BUMP PAST KEY TO RECORD                      
         USING BLBRECD,R5          BINSRCH RECORD DSECT                         
*                                                                               
         LA    R4,SORTREC          R4 = SORTREC                                 
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         USING SRTBRECD,R4         SORT RECORD DSECT                            
         MVC   SRTBAM,STABKAM      AGENCY/MEDIA (KEY)                           
         MVC   SRTBCLT,CLT         3-CHARACTER CLIENT (KEY)                     
         MVC   SRTBPRD,SAVEPRD     3-CHARACTER PRODUCT (KEY)                    
         MVC   SRTBEST,STABKEST    BINARY ESTIMATE (KEY)                        
         MVC   SRTBINV,STABINV     PACKED INVOICE NUMBER (KEY)                  
         NI    SRTBINV,X'FF'-X'C0' STRIP REVERSED & REVERSAL FLAGS              
         MVC   SRTBBDAT,STABBDT    BILLED DATE COMPRESSED (KEY)                 
         MVC   SRTBMSTA,STABKMKT   PACKED MKT/STA                               
*                                                                               
         GOTO1 MSUNPK,DMCB,STABKMKT,WORK,WORK+8                                 
*                                                                               
         MVC   SRTBSTA(4),WORK+8   STATION                                      
         MVI   SRTBSTA+4,C'-'      DASH AFTER STATION                           
         MVI   SRTBSTA+5,C'N'      "N"                                          
         CLI   QMED,C'N'           MEDIA N?                                     
         BE    BLB50               YES                                          
         MVC   SRTBSTA+5(1),WORK+12 BAND FOR RADIO                              
         MVI   SRTBSTA+6,C'M'      FOLLOWED BY AN "M'                           
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   BLB50               NO - MUST BE MEDIA R OR X                    
         MVC   SRTBSTA+5(2),=C'TV' YES - ADD "TV" AFTER STATION                 
         CLI   WORK+12,C'D'        IS THIS BAND "DV"?                           
         BNE   *+8                 NO                                           
         MVI   SRTBSTA+5,C'D'      YES - ADD "DV" AFTER STATION                 
*                                                                               
BLB50    MVC   SRTBINET,BLBNET     NET AMOUNT DUE                               
         MVC   SRTBIGST,BLBGST     GST AMOUNT DUE                               
         MVC   SRTBIPST,BLBPST     PST AMOUNT DUE                               
         MVC   SRTBTYPE,BLBTYPE    BILL TYPE (B1,B2,B3,B4,B5,ETC)               
         MVC   SRTBIINV,BLBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SRTBIDAT,BLBINVD    INVOICE DATE AS ON BILL                      
         MVC   SRTBUCM4,BLBEUCM4   ESTIMATE UCOMM 4                             
         MVC   SRTBUCM3,BLBEUCM3   ESTIMATE UCOMM 3                             
         MVC   SRTBUCOM,BLBEUCOM   ESTIMATE UCOMM 1                             
         MVC   SRTBPNAM,BLBPNAME   PRODUCT NAME                                 
         MVC   SRTBPGRP,BLBPGRP    PRODUCT GROUP                                
         MVC   SRTBPGNM,BLBPGRPN   PRODUCT GROUP NAME                           
         MVC   SRTBENAM,BLBENAME   ESTIMATE NAME                                
*                                                                               
         L     R2,ADAGY            A(AGENCY RECORD)                             
         LA    R2,24(R2)           A(FIRST ELEMENT)                             
*                                                                               
BLB60    CLI   0(R2),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SOMETHING IS WRONG                     
*                                                                               
         CLI   0(R2),AGYMEDEQ      X'02' MEDIA CODE ELEMENT?                    
         BNE   BLB65               NO - BUMP TO NEXT ELEMENT                    
*                                                                               
         USING AGYMEDEL,R2         MEDIA CODE ELEMENT DSECT                     
         CLC   AGYMEDBT,SRTBAM     MATCH ON AGENCY MEDIA?                       
         BE    BLB70               YES                                          
*                                                                               
BLB65    LLC   RF,1(R2)            ELEMENT LENGTH                               
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         B     BLB60               NO - GET NEXT ELEMENT                        
*                                                                               
BLB70    MVC   SRTBMED,AGYMEDCD    MEDIA CODE                                   
         MVC   SRTBMEDN,AGYMEDEX   MEDIA NAME                                   
         OC    SRTBMEDN,SPACES     SPACE PAD                                    
         DROP  R2                  DROP MEDIA CODE ELEMENT USING                
*                                                                               
         L     RF,ADCLT            A(CLIENT RECORD)                             
         USING CLTHDR,RF           CLIENT RECORD DSECT                          
         MVC   SRTBCNAM,CNAME      CLIENT NAME                                  
         DROP  RF                  DROP CLIENT RECORD USING                     
*                                                                               
         MVC   WORK(2),STABPER     YEAR/MONTH                                   
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         MVC   SRTBMOSP,WORK       SAVE PACKED MOS                              
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTBMOS)  MMM/YY                         
*                                                                               
         GOTO1 =A(GETMKNAM),DMCB,(RC) GET MARKET NAME                           
*                                                                               
         MVC   SRTBMNAM,SVMKTNM    MARKET NAME                                  
*                                                                               
         GOTO1 =A(GETMGNAM),DMCB,(RC) GET MARKET GROUP NAME                     
*                                                                               
         MVC   SRTBMGNM,SVMGRPN    MARKET GROUP NAME                            
*                                                                               
         MVC   THISSTA,SRTBSTA     SET STATION FOR GETSNAME                     
*                                                                               
         GOTO1 =A(GETSNAME),DMCB,(RC) GET STATION NAME                          
*                                                                               
         MVC   SRTBSNAM,VENDORNM   VENDOR NAME                                  
         OC    SRTBSNAM,SPACES     SPACE PAD                                    
*                                                                               
         MVC   SRTBBNET,STABNET    VENDOR NET AMOUNT                            
*                                                                               
         L     R0,STABNET          VENDOR NET AMOUNT                            
         CVD   R0,DUB              CONVERT TO PACKED                            
         AP    DTLTOT,DUB          SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
         XIT1                      EXIT                                         
         DROP  R4,R6,R7            DROP USINGS                                  
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'BLOUTPR - OUTPUT ROUTINE BELL'                                  
BLOUTPR  CSECT                                                                  
         NMOD1 0,BLOUTPR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
         L     R6,AWIDEC                                                        
         USING WIDED,R6                                                         
*                                                                               
         L     R1,ABOX             A(BOX DEFINITIONS)                           
         USING BOXD,R1             BOX DEFINITIONS DSECT                        
         MVC   BOXWIDTH,=F'198'    SET FOR WIDE PRINTING                        
         MVC   BOXAWIDE,AWIDEC     ADDRESS OF WIDE PRINT LINES                  
         MVI   BOXYORN,C'N'        NO BOXES                                     
         DROP  R1                  DROP BOX DEFINITIONS USING                   
*                                                                               
         MVI   FORCEHED,C'N'       DO NOT PRINT HEADLINES                       
         XC    SRTKSVBL,SRTKSVBL   CLEAR SAVED SORT KEY                         
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         L     R4,AOUTREC          R4 = A(OUTREC)                               
*                                                                               
         MVI   0(R4),C'Y'          Y RECORD                                     
         LA    R4,1(R4)            BUMP R4                                      
*                                                                               
         LA    R1,6                LENGTH OF YYMMDD                             
         LA    R2,CTODAY+2         YYMMDD                                       
         BAS   RE,ADDINFBL         ADD DATA & BUMP R4                           
*                                                                               
         LA    R1,2                LENGTH OF HH                                 
         LA    R2,TIMEOFD          HH                                           
         BAS   RE,ADDINFBL         ADD DATA & BUMP R4                           
*                                                                               
         LA    R1,2                LENGTH OF MM                                 
         LA    R2,TIMEOFD+3        MM                                           
         BAS   RE,ADDINFBL         ADD DATA & BUMP R4                           
*                                                                               
         LA    R1,2                LENGTH OF SS                                 
         LA    R2,TIMEOFD+6        MM                                           
         BAS   RE,ADDINFBL         ADD DATA & BUMP R4                           
*                                                                               
         EDIT  ABINRECN,(4,0(R4)),0,FILL=0                                      
         LA    R4,4(R4)            BUMP R4                                      
*                                                                               
         LA    R1,8                LENGTH OF YYYYMMDD                           
         LA    R2,CTODAY           YYYYMMDD                                     
         BAS   RE,ADDINFBL         ADD DATA & BUMP R4                           
*                                                                               
         GOTO1 VTAPEWRT,DMCB,(RC)  WRITE RECORD TO TAPE                         
*                                                                               
         AP    HDRCNT,=P'1'        HEADERS                                      
*                                                                               
         LA    R7,XHEAD1           POINT TO HEADLINES                           
         USING BBLINED,R7          BBLINED DSECT TO COVER PRINT LINE            
*                                                                               
         MVC   BBLINVNO,=C' INV NUM  '                                          
         MVC   BBLINVD,=C' INV DATE '                                           
         MVC   BBLCLT(11),=C'CLT/PRD/EST'                                       
         MVC   BBLCNAME,=C'    CLIENT NAME     '                                
         MVC   BBLPGRP,=C'PGRP'                                                 
         MVC   BBLPGRNM,=C' PRODUCT GROUP NAME '                                
         MVC   BBLIVMOS,=C' MOS  '                                              
         MVC   BBLMED,=C'  MEDIA   '                                            
         MVC   BBLUCOMM,=C'PROJECT CODE'                                        
         MVC   BBLUCOM3,=C'    CONTACT PIN     '                                
         MVC   BBLUCOM4,=C'          CONTACT NAME          '                    
         MVC   BBLGROSS,=C'   AMOUNT     '                                      
*                                                                               
         LA    R7,XHEAD2           POINT TO HEADLINES 2                         
         MVC   BBLINVNO,=C'----------'                                          
         MVC   BBLINVD,=C'----------'                                           
         MVC   BBLCLT(11),=C'-----------'                                       
         MVC   BBLCNAME,=C'--------------------'                                
         MVC   BBLPGRP,=C'----'                                                 
         MVC   BBLPGRNM,=C'--------------------'                                
         MVC   BBLIVMOS,=C'------'                                              
         MVC   BBLMED,=C'----------'                                            
         MVC   BBLUCOMM,=C'------------'                                        
         MVC   BBLUCOM3,=C'--------------------'                                
         MVC   BBLUCOM4,=C'--------------------------------'                    
         MVC   BBLGROSS,=C'--------------'                                      
*                                                                               
         DROP  R7                  DROP PRINT LINE USING                        
*                                                                               
BLOUT10  GOTO1 VSORTER,DMCB,=C'GET' GET SORT RECORD                             
*                                                                               
         ICM   R5,15,4(R1)         ANY MORE RECORDS FROM SORTER?                
         BZ    BLOUT50             NO - DONE                                    
*                                                                               
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         USING SRTBRECD,R5         SRTBRECD DSECT                               
         CLC   SRTKSVBL,SRTBKEY    SAME AS PREVIOUS KEY?                        
         BE    BLOUT20             YES - VENDOR LEVEL LINE                      
*                                                                               
         LA    R7,XP               POINT TO PRINT LINE                          
         USING BBLINED,R7          BBLINED DSECT TO COVER PRINT LINE            
         MVC   BBLINVNO,SRTBIINV   FULL INVOICE NUMBER                          
         MVC   BBLINVD,SRTBIDAT    INVOICE DATE                                 
         MVC   BBLCLT,SRTBCLT      CLIENT CODE                                  
         MVI   BBLCLT+3,C'/'       /                                            
         MVC   BBLPRD,SRTBPRD      PRODUCT                                      
         MVI   BBLPRD+3,C'/'       /                                            
         EDIT  SRTBEST,(3,BBLEST),FILL=0                                        
         MVC   BBLCNAME,SRTBCNAM   CLIENT NAME                                  
         MVC   BBLPGRP(3),SRTBPGRP PRODUCT GROUP                                
         MVC   BBLPGRNM,SRTBPGNM   PRODUCT GROUP NAME                           
         MVC   BBLIVMOS,SRTBMOS    MOS                                          
         MVC   BBLMED,SRTBMEDN     MEDIA NAME                                   
         MVC   BBLUCOMM(4),SRTBUCOM  EST UCOMM 1 (PROJECT CODE)                 
         MVC   BBLUCOM3,SRTBUCM3   EST UCOMM 3 (CONTACT PIN)                    
         MVC   BBLUCOM4,SRTBUCM4   EST UCOMM 4 (CONTACT NAME)                   
*                                                                               
         ZAP   PAK6,SRTBINET       GROSS                                        
         AP    PAK6,SRTBIGST       +GST                                         
         AP    PAK6,SRTBIPST       +QST                                         
*                                                                               
         EDIT  PAK6,(13,BBLGROSS),2,FLOAT=-                                     
*                                                                               
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R7                  DROP PRINT LINE USING                        
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         L     R7,AOUTREC          R4 = A(OUTREC)                               
         USING BLFLINED,R7         FILE LINE DSECT                              
         MVC   SRTKSVBL,SRTBKEY    SAVE SORT KEY MINUS VENDOR DETAILS           
         MVI   BTYPEA,C'A'         A FOR INVOICE LEVEL LINE                     
         MVC   BINVNUM,SRTBIINV    INVOICE NUMBER                               
         MVI   BINVTYPE,C'F'       INIT TYPE =  REGULAR INVOICE                 
         CLC   SRTBTYPE,=C'B5'     B5 TYPE BILL?                                
         BNE   *+8                 NO - REGULAR INVOICE                         
         MVI   BINVTYPE,C'A'       TYPE =  ADJUSTMENT BILL                      
*                                                                               
         MVC   BCLIENT,SRTBCLT     CLIENT CODE                                  
         MVC   BCLTNAME,SRTBCNAM   CLIENT NAME                                  
*                                                                               
         LA    R4,BINVDATE         INVOICE DATE                                 
         MVC   0(4,R4),SRTBIDAT    YYYY                                         
         MVC   4(2,R4),SRTBIDAT+5  MM                                           
         MVC   6(2,R4),SRTBIDAT+8  DD                                           
*                                                                               
         MVC   BINVCURR,=C'CAN'    CANADA                                       
         MVC   BMEDCODE,SRTBMED    MEDIA CODE                                   
         EDIT  SRTBEST,(3,BESTNUM),FILL=0                                       
         MVC   BESTNAME,SRTBENAM   ESTIMATE NAME                                
         MVC   BEUCOMM,SRTBUCOM    EST UCOMM 1 (4 DIGIT PROJECT CODE)           
         MVC   BEUCOM3,SRTBUCM3    ESTIMATE UCOMM 3 (EMPLOYEE NUMBER)           
*                                                                               
         LA    R4,BEUCOM4          ESTIMATE UCOMM4(EMPLOYEE NAME + NUM)         
         MVC   0(32,R4),SRTBUCM4   ESTIMATE UCOMM4(EMPLOYEE NAME)               
         LA    R4,32(R4)           1 BYTE PAST ESTIMATE UCOMM4                  
         CLI   0(R4),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R4,*-8              NO, CHECK PREVIOUS BYTE                      
         MVC   2(16,R4),SRTBUCM3   ESTIMATE UCOMM 3 (EMPLOYEE NUMBER)           
*                                                                               
         GOTO1 DATCON,DMCB,(3,SRTBMOSP),(20,BMEDMON)  YYYYMMDD                  
*                                                                               
         EDIT  PAK6,(12,BINVAMT),2,FILL=0                                       
*                                                                               
         CP    PAK6,=P'0'          NEGATIVE VALUE?                              
         BNL   *+8                 NO                                           
         MVI   BINVAMT,C'-'        YES - FLOAT NEGATIVE BEFORE VALUE            
*                                                                               
         EDIT  SRTBIGST,(12,BINVGST),2,FILL=0                                   
*                                                                               
         CP    SRTBIGST,=P'0'      NEGATIVE VALUE?                              
         BNL   *+8                 NO                                           
         MVI   BINVGST,C'-'        YES - FLOAT NEGATIVE BEFORE VALUE            
*                                                                               
         MVC   BGSTPCNT,=C'000.0500' GST PERCENT                                
*                                                                               
         CP    SRTBIGST,=P'0'      DO WE HAVE ZERO AMOUNT?                      
         BNE   *+10                NO                                           
         MVC   BGSTPCNT,=C'000.0000' GST PERCENT                                
*                                                                               
         EDIT  SRTBIPST,(12,BINVQST),2,FILL=0                                   
*                                                                               
         CP    SRTBIPST,=P'0'      NEGATIVE VALUE?                              
         BNL   *+8                 NO                                           
         MVI   BINVQST,C'-'        YES - FLOAT NEGATIVE BEFORE VALUE            
*                                                                               
         MVC   BQSTPCNT,=C'000.0998' QST PERCENT                                
*                                                                               
         CP    SRTBIPST,=P'0'      DO WE HAVE ZERO AMOUNT?                      
         BNE   *+10                NO                                           
         MVC   BQSTPCNT,=C'000.0000' QST PERCENT                                
*                                                                               
         LA    R4,BPRDNAME         PRODUCT CODE + PRODUCT NAME                  
         MVC   0(3,R4),SRTBPRD     PRODUCT CODE                                 
         LA    R4,3(R4)            1 BYTE PAST ESTIMATE UDEF1                   
         CLI   0(R4),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R4,*-8              NO, CHECK PREVIOUS BYTE                      
         MVI   1(R4),C'/'          C'/'                                         
         MVC   2(20,R4),SRTBPNAM   PRODUCT NAME                                 
*                                                                               
         MVC   BPGRP,SRTBPGRP      PRODUCT GROUP                                
         MVC   BPGRPNAM,SRTBPGNM   PRODUCT GROUP NAME                           
*                                                                               
         GOTO1 VTAPEWRT,DMCB,(RC)  WRITE RECORD TO TAPE                         
*                                                                               
BLOUT20  LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         L     R7,AOUTREC          R4 = A(OUTREC)                               
         MVI   BTYPEB,C'B'         B FOR VENDOR LEVEL LINE                      
         MVC   BVINVNUM,SRTBIINV   INVOICE NUMBER                               
         MVC   BVCLIENT,SRTBCLT    CLIENT CODE                                  
         MVC   BVMGNAME,SRTBMGNM   MARKET GROUP NAME                            
         MVC   BVMKTNAM,SRTBMNAM   MARKET NAME                                  
         MVC   BVSTANAM,SRTBSNAM   VENDOR/STATION NAME                          
*                                                                               
         EDIT  SRTBBNET,(12,BVAMT),2,FILL=0                                     
*                                                                               
         ICM   R0,15,SRTBBNET      R0 = VALUE OF SRTBBNET                       
         C     R0,=F'0'            NEGATIVE VALUE?                              
         BNL   *+8                 NO                                           
         MVI   BVAMT,C'-'          YES - FLOAT NEGATIVE BEFORE VALUE            
*                                                                               
         MVC   BVPAMT,=C'0.0000000000'                                          
         XR    R4,R4               CLEAR R4                                     
         EDIT  (R4),(12,BVPAMT),2,FILL=0                                        
*                                                                               
         GOTO1 VTAPEWRT,DMCB,(RC)  WRITE RECORD TO TAPE                         
*                                                                               
         B     BLOUT10             GET NEXT SORT RECORD                         
*                                                                               
BLOUT50  L     R1,ABOX             A(BOX DEFINITIONS)                           
         USING BOXD,R1             BOX DEFINITIONS DSECT                        
         MVC   BOXWIDTH,=F'132'    RESTORE NON-WIDE PRINTING                    
         DROP  R1                  DROP BOX DEFINITIONS USING                   
*                                                                               
BLOUTXIT XIT1                      DONE                                         
*                                                                               
ADDINFBL BCTR  R1,0                SUBTRACT 1 FROM LENGTH FOR EX                
         EX    R1,*+8              EXECUTE THE MVC                              
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   0(0,R4),0(R2)       UPDATE THE ENTRY                             
         LA    R4,1(R1,R4)         ADDITIONAL DATA STARTS HERE                  
         BR    RE                  RETURN TO CALLER                             
*                                                                               
PRNTFILE NTR1                                                                   
*                                                                               
         MVI   SPACING,1           SINGLE SPACING                               
         MVC   SVLINE,LINE         SAVE LINE                                    
         MVI   LINE,0              FORCE LINE TO ZERO                           
         MVI   RCWHATPR,2          SET TO SYSPRINT 2 (FILE)                     
*                                                                               
         GOTO1 REPORT              PRINT FILE LINE                              
*                                                                               
         MVC   LINE,SVLINE         RESTORE LINE                                 
         MVI   RCWHATPR,1          RESTORE SYSPRINT 1 (REPORT ON PQ)            
*                                                                               
PFXIT    XIT1                      RETURN TO CALLER                             
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'OUBILLR - PROCESS BILL RECORD FOR PHDTO AGENCY OU'              
OUBILLR  CSECT                                                                  
         NMOD1 0,OUBILLR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LA    R0,BINKEYOU         BINSRCH KEY                                  
         ST    R0,ABINKEY          A(BINSRCH KEY)                               
*                                                                               
         LA    R5,BINKEYOU         R5 = BINSRCH KEY                             
         USING OUBKEYD,R5          BILLING SORT KEY DSECT                       
         USING BILLREC,R6          BILL RECORD USING                            
         MVC   OUBAM,BKEYAM        AGY/MED                                      
         MVC   OUBCLT,BKEYCLT      PACKED CLIENT CODE                           
         MVC   OUBPRD,BKEYPRD      PRODUCT CODE                                 
         MVC   OUBEST,BKEYEST      ESTIMATE                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(2,OUBINVYM) INV YYMMDD                    
         MVC   OUBINVN,BKEYINV     INVOICE NUMBER PACKED                        
         DROP  R5                  DROP BILLING SORT KEY USING                  
*                                                                               
         LA    R5,BINRECOU         R5 = BINSRCH RECORD                          
         USING OUBRECD,R5          BILL RECORD DATA DSECT                       
         MVC   OUBREC,SPACES       SPACE-FILL OUTPUT RECORD                     
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
*                                                                               
         MVC   OUBNET,SPBVNETP     NET AMOUNT                                   
*                                                                               
         L     R0,SPBVGST          GST FROM SPBVAL                              
         CVD   R0,DUB              CVD                                          
         ZAP   OUBGST,DUB          GST - PACKED                                 
*                                                                               
         L     R0,SPBVPST          PST FROM SPBVAL                              
         CVD   R0,DUB              CVD                                          
         ZAP   OUBPST,DUB          PST - PACKED                                 
*                                                                               
         XC    OUBCOM,OUBCOM       CLEAR AGENCY COMMISSION                      
         CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BNE   OUBIL05             NO                                           
         ZAP   DUB,SPBVACTP        ACTUAL                                       
         SP    DUB,SPBVNETP        MINUS NET                                    
         CVB   R1,DUB              CONVERT TO BINARY                            
         STCM  R1,15,OUBCOM        AGENCY COMMISSION                            
*                                                                               
***      MVC   FULL,SPBVEGRS       EFFECTIVE GROSS                              
***      ICM   RF,15,BILBFP        COMMISSION ADUSTMENT (3.2% = 32000)          
***      M     RE,FULL             X ADJ PCT                                    
*                                                                               
***      SLDA  RE,1                *2                                           
***      D     RE,=F'1000000'      /1000000                                     
***      LTR   RF,RF               HAVE REMAINDER?                              
***      BNP   *+8                 NO                                           
***      AHI   RF,1                YES - ADD 1                                  
***      SRA   RF,1                /2                                           
***      STCM  RF,15,OUBCOM        COMMISSION ADUSTMENT AMOUNT                  
*                                                                               
OUBIL05  MVI   OUBPROV,0           INIT PROVINCE CODE                           
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,1,BILNPVTS       HAVE ANY VAT "ELEMENTS"?                     
         BZ    OUBIL20             NO                                           
*                                                                               
         LA    RF,BILPVPRV         START OF PROVINCE "ELEMENTS"                 
         USING BILPVELD,RF         PROVINCE "ELEMENT" DSECT                     
*                                                                               
OUBIL10  OC    BILPVAMT,BILPVAMT   HAVE A VAT AMOUNT?                           
         BZ    OUBIL15             NO, CHECK NEXT "ELEMENT"                     
         MVC   OUBPROV,BILPVPRV    SET BILL PROVINCE                            
         B     OUBIL20             DONE WITH PROVINCE                           
OUBIL15  LA    RF,BILPVLEN(RF)     BUMP TO NEXT "ELEMENT"                       
         BCT   RE,OUBIL10          CHECK NEXT "ELEMENT"                         
         DROP  RF                  DROP PROVINCE "ELEMENT" DSECT                
*                                                                               
OUBIL20  LA    R1,B1PROF           B1 PROFILE                                   
         ST    R1,DMCB+8           PARM 3                                       
         MVC   DMCB+8(1),MED       MEDIA IS HOB OF PARM 4                       
*                                                                               
         LA    R1,B1XPROF          B1X PROFILE                                  
         ST    R1,DMCB+12          PARM 4                                       
         ST    R6,DMCB+16          A(BILL HEADER RECORD)                        
*                                                                               
         GOTO1 AFMTINO,DMCB,(C'B',BDATE),(2,BKEYINV)                            
*                                                                               
         L     RF,DMCB             RF = DMCB                                    
         MVC   OUBINV,0(RF)        FULL INVOICE NUMBER                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,BQDATE),(20,WORK) YYYYMMDD                        
*                                                                               
         LA    R4,OUBINVD          INVOICE DATE                                 
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         MVI   CKESTREC,C'L'       FLAG TO READ EST FROM BILL KEY               
         GOTO1 =A(CKEST),DMCB,(RC) MAY NEED TO RE-READ ESTIMATE                 
*                                                                               
         L     RF,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDR,RF           ESTIMATE RECORD DSECT                        
         MVC   OUEUDEF1,EUSER1     ESTIMATE UDEF 1                              
         OC    OUEUDEF1,SPACES     SPACE PAD                                    
         MVC   OUEUDEF2,EUSER2     ESTIMATE UDEF 2                              
         OC    OUEUDEF2,SPACES     SPACE PAD                                    
         DROP  RF                  DROP EST REC & ULBRECD USING                 
         DROP  R5                  DROP BILL SORT USING                         
*                                                                               
         ZAP   DUB,BACTP           GET BACTP INTO DUB                           
         CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BNE   *+10                NO                                           
         ZAP   DUB,BNETP           GET BNETP INTO DUB                           
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) TOTAL               
*                                                                               
         MVI   ABINKEY,1           RESET TO INSERT RECORD IF NOT FOUND          
         GOTO1 VBINSRCH,ABINKEY    CALL BINSRCH                                 
*                                                                               
         CLI   ABINKEY,1           RECORD INSERTED?                             
         BE    OUBXIT              YES - DONE                                   
*                                                                               
         OC    1(3,R1),1(R1)       HAVE A(BINSRCH REC)?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - TABLE IS FULL - EXPAND TABLE!           
*                                                                               
         L     RF,ABINKEY          ADDRESS OF FOUND RECORD                      
         LA    RF,L'OUBKEY(RF)     PAST KEY                                     
         USING OUBRECD,RF          BILL SORT RECORD DSECT                       
         AP    OUBNET,SPBVNETP     NET AMOUNT (DUP KEY MUST ADD FIELD)          
*                                                                               
         L     R0,SPBVGST          GST FROM SPBVAL                              
         CVD   R0,DUB              CVD                                          
         AP    OUBGST,DUB          ADD GST (DUP KEY MUST ADD FIELD)             
*                                                                               
         L     R0,SPBVPST          PST FROM SPBVAL                              
         CVD   R0,DUB              CVD                                          
         AP    OUBPST,DUB          ADD PST (DUP KEY MUST ADD FIELD)             
*                                                                               
         DROP  RF                  DROP BILL SORT USING                         
*                                                                               
OUBXIT   AP    INVCNT,=P'1'        UPDATE INVOICE COUNT                         
         XIT1                      EXIT                                         
         DROP  R6                  DROP BILL RECORD USING                       
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'OUBILHR - PROCESS STA BUCKET ELEMENT FOR PHD'                   
OUBILHR  CSECT                                                                  
         NMOD1 0,OUBILHR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
         USING STABUCKD,R6         STATION BUCKET KEY DSECT                     
         USING STABELEM,R7         BILLING ELEMENT DSECT                        
*                                                                               
         LA    R5,BINKEYOU         R5 = BINSRCH KEY                             
         USING OUBKEYD,R5          BILL BINSRCH KEY DSECT                       
         XC    BINKEYOU,BINKEYOU   CLEAR BINSRCH KEY                            
         MVC   OUBAM,STABKAM       AGENCY/MEDIA                                 
         MVC   OUBCLT,STABKCLT     PACKED CLIENT                                
         MVC   OUBPRD,SAVEPRD      3-CHAR PRODUCT                               
         MVC   OUBEST,STABKEST     BINARY ESTIMATE                              
         MVC   OUBINVYM,STABBDT    DATE OF BILLING COMPRESSED                   
         MVC   OUBINVN,STABINV     PACKED INVOICE NUMBER                        
         NI    OUBINVN,X'FF'-X'C0' STRIP REVERSED & REVERSAL FLAGS              
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         ST    R5,ABINKEY          A(BINSRCH KEY)                               
         MVI   ABINKEY,0           SET TO SEARCH ONLY - DON'T ADD!              
*                                                                               
         GOTO1 VBINSRCH,ABINKEY    BILL BINSRCH ENTRY FOR THIS ELEM             
*                                                                               
         CLI   ABINKEY,X'01'       RECORD NOT FOUND FLAG SET?                   
         BNE   *+6                 NO                                           
         DC    H'0'                REC NOT FOUND - SOMETHING IS WRONG!          
*                                                                               
         L     R5,ABINKEY          ADDRESS OF BINSRCH RECORD                    
         LA    R5,BLBKEYL(R5)      BUMP PAST KEY TO RECORD                      
         USING OUBRECD,R5          BINSRCH RECORD DSECT                         
*                                                                               
         LA    R4,SORTREC          R4 = SORTREC                                 
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         USING SRTORECD,R4         SORT RECORD DSECT                            
         MVC   SRTOAM,STABKAM      AGENCY/MEDIA (KEY)                           
         MVC   SRTOCLT,CLT         3-CHARACTER CLIENT (KEY)                     
         MVC   SRTOPRD,SAVEPRD     3-CHARACTER PRODUCT (KEY)                    
         MVC   SRTOEST,STABKEST    BINARY ESTIMATE (KEY)                        
         MVC   SRTOINV,STABINV     PACKED INVOICE NUMBER (KEY)                  
         NI    SRTOINV,X'FF'-X'C0' STRIP REVERSED & REVERSAL FLAGS              
         MVC   SRTOBDAT,STABBDT    BILLED DATE COMPRESSED (KEY)                 
         MVC   SRTOMSTA,STABKMKT   PACKED MKT/STA                               
*                                                                               
         GOTO1 MSUNPK,DMCB,STABKMKT,WORK,WORK+8                                 
*                                                                               
         MVC   SRTOSTA(4),WORK+8   STATION                                      
         MVI   SRTOSTA+4,C'-'      DASH AFTER STATION                           
         MVI   SRTOSTA+5,C'N'      "N"                                          
         CLI   QMED,C'N'           MEDIA N?                                     
         BE    OUB50               YES                                          
         MVC   SRTOSTA+5(1),WORK+12 BAND FOR RADIO                              
         MVI   SRTOSTA+6,C'M'      FOLLOWED BY AN "M'                           
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   OUB50               NO - MUST BE MEDIA R OR X                    
         MVC   SRTOSTA+5(2),=C'TV' YES - ADD "TV" AFTER STATION                 
         CLI   WORK+12,C'D'        IS THIS BAND "DV"?                           
         BNE   *+8                 NO                                           
         MVI   SRTOSTA+5,C'D'      YES - ADD "DV" AFTER STATION                 
*                                                                               
OUB50    MVC   SRTOINET,OUBNET     NET AMOUNT DUE                               
         MVC   SRTOIGST,OUBGST     GST AMOUNT DUE                               
         MVC   SRTOIPST,OUBPST     PST AMOUNT DUE                               
         MVC   SRTOACOM,OUBCOM     AGENCY COMMISSION AMOUNT                     
         MVC   SRTOPROV,OUBPROV    PROVINCE                                     
         MVC   SRTOIINV,OUBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SRTOIDAT,OUBINVD    INVOICE DATE AS ON BILL                      
         MVC   SRTOEUD1,OUEUDEF1   ESTIMATE UDEF 1                              
         MVC   SRTOEUD2,OUEUDEF2   ESTIMATE UDEF 2                              
*                                                                               
         L     RF,ADCLT            A(CLIENT RECORD)                             
         USING CLTHDR,RF           CLIENT RECORD DSECT                          
         MVC   SRTOCNAM,CNAME      CLIENT NAME                                  
         DROP  RF                  DROP CLIENT RECORD USING                     
*                                                                               
         L     R2,ADAGY            A(AGENCY RECORD)                             
         LA    R2,24(R2)           A(FIRST ELEMENT)                             
*                                                                               
OUB60    CLI   0(R2),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SOMETHING IS WRONG                     
*                                                                               
         CLI   0(R2),AGYMEDEQ      X'02' MEDIA CODE ELEMENT?                    
         BNE   OUB65               NO - BUMP TO NEXT ELEMENT                    
*                                                                               
         USING AGYMEDEL,R2         MEDIA CODE ELEMENT DSECT                     
         CLC   AGYMEDBT,SRTOAM     MATCH ON AGENCY MEDIA?                       
         BE    OUB70               YES                                          
*                                                                               
OUB65    LLC   RF,1(R2)            ELEMENT LENGTH                               
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         B     OUB60               NO - GET NEXT ELEMENT                        
*                                                                               
OUB70    MVC   SRTOMED,AGYMEDCD    MEDIA CODE                                   
         DROP  R2                  DROP MEDIA CODE ELEMENT USING                
*                                                                               
         MVC   WORK(2),STABPER     YEAR/MONTH                                   
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         MVC   SRTOMOSP,WORK       SAVE PACKED MOS                              
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTOMOS)  MMM/YY                         
*                                                                               
         MVC   THISSTA,SRTOSTA     SET STATION FOR GETSNAME                     
*                                                                               
         GOTO1 =A(GETSNAME),DMCB,(RC) GET STATION NAME                          
*                                                                               
         MVC   SRTOSNAM,VENDORNM   VENDOR NAME                                  
         OC    SRTOSNAM,SPACES     SPACE PAD                                    
*                                                                               
         MVC   SRTOBNET,STABNET    VENDOR NET AMOUNT                            
*                                                                               
         L     R0,STABNET          VENDOR NET AMOUNT                            
         CVD   R0,DUB              CONVERT TO PACKED                            
         AP    DTLTOT,DUB          SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
         XIT1                      EXIT                                         
         DROP  R4,R6,R7            DROP USINGS                                  
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'OUOUTPR - OUTPUT ROUTINE PHD'                                   
OUOUTPR  CSECT                                                                  
         NMOD1 0,OUOUTPR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LAY   R2,OUHDSTMP         UPDATE DATESTAMP YYYY-MM-DD                  
         MVC   0(4,R2),CTODAY      YYYY                                         
         MVC   5(2,R2),CTODAY+4    MM                                           
         MVC   8(2,R2),CTODAY+6    DD                                           
*                                                                               
         LAY   R3,OUHDSTM2         UPDATE DATESTAMP YYYY-MM-DD                  
         MVC   0(10,R3),0(R2)      COPY DATE STAMP                              
*                                                                               
         LAY   R3,OUFDSTMP         UPDATE DATESTAMP YYYY-MM-DD                  
         MVC   0(10,R3),0(R2)      COPY DATE STAMP                              
*                                                                               
         LAY   R3,OUFDSTM2         UPDATE DATESTAMP YYYY-MM-DD                  
         MVC   0(10,R3),0(R2)      COPY DATE STAMP                              
***                                                                             
* THESE NEXT 2 FIELDS ARE FOR EDI PHD/Coupa for Canadian Tire                   
***                                                                             
         LAY   R3,OUHDSTM3         UPDATE DATESTAMP YYYY-MM-DD                  
         MVC   0(10,R3),0(R2)      COPY DATE STAMP                              
*                                                                               
         LAY   R3,OUHDSTM4         UPDATE DATESTAMP YYYY-MM-DD                  
         MVC   0(10,R3),0(R2)      COPY DATE STAMP                              
*                                                                               
         LAY   R3,OUHDSTM5         UPDATE DATESTAMP YYYY-MM-DD                  
         MVC   0(10,R3),0(R2)      COPY DATE STAMP                              
*                                                                               
         LAY   R3,OUHDSTM6         UPDATE DATESTAMP YYYY-MM-DD                  
         MVC   0(10,R3),0(R2)      COPY DATE STAMP                              
*                                                                               
         LAY   R2,OUHTSTMP         UPDATE TIMSTAMP HH:MM:SS                     
         MVC   0(2,R2),TIMEOFD     HH                                           
         MVC   3(2,R2),TIMEOFD+3   MM                                           
         MVC   6(2,R2),TIMEOFD+6   SS                                           
*                                                                               
         LAY   R3,OUHTSTM2         UPDATE TIMSTAMP HH:MM:SS                     
         MVC   0(8,R3),0(R2)       COPY TIME STAMP                              
*                                                                               
         LAY   R3,OUFTSTMP         UPDATE TIMSTAMP HH:MM:SS                     
         MVC   0(8,R3),0(R2)       COPY TIME STAMP                              
*                                                                               
         LAY   R3,OUFTSTM2         UPDATE TIMSTAMP HH:MM:SS                     
         MVC   0(8,R3),0(R2)       COPY TIME STAMP                              
***                                                                             
* THESE NEXT 2 FIELDS ARE FOR EDI PHD/Coupa for Canadian Tire                   
***                                                                             
         LAY   R3,OUHTSTM3         UPDATE TIMSTAMP HH:MM:SS                     
         MVC   0(8,R3),0(R2)       COPY TIME STAMP                              
*                                                                               
         LAY   R3,OUHTSTM4         UPDATE TIMSTAMP HH:MM:SS                     
         MVC   0(8,R3),0(R2)       COPY TIME STAMP                              
*                                                                               
         LAY   R3,OUHTSTM5         UPDATE TIMSTAMP HH:MM:SS                     
         MVC   0(8,R3),0(R2)       COPY TIME STAMP                              
*                                                                               
         LAY   R3,OUHTSTM6         UPDATE TIMSTAMP HH:MM:SS                     
         MVC   0(8,R3),0(R2)       COPY TIME STAMP                              
*                                                                               
         XC    SRTKSVOU,SRTKSVOU   CLEAR SAVED SORT KEY                         
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         LA    R2,PHDSFLDB         PHD SPECIAL FIELDS BLOCK                     
         USING PHDSFLDD,R2         PHD SPECIAL FIELDS BLOCK DSECT               
*                                                                               
OUOUT10  GOTO1 VSORTER,DMCB,=C'GET' GET SORT RECORD                             
*                                                                               
         ICM   R5,15,4(R1)         ANY MORE RECORDS FROM SORTER?                
         BZ    OUOUT50             NO - DONE                                    
         USING SRTORECD,R5         SRTORECD DSECT                               
*                                                                               
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BE    OUOUT55             YES - COUPA FOR CANADIAN TIRE                
*                                                                               
         LA    R7,P                POINT TO PRINT LINE                          
         USING BLINED,R7           BLINED DSECT TO COVER PRINT LINE             
         MVC   BLMEDIA,SRTOMED     MEDIA                                        
         MVC   BLCLT,SRTOCLT       CLIENT                                       
         MVC   BLCLNAME,SRTOCNAM   CLIENT NAME                                  
         MVC   BLPRD,SRTOPRD       PRODUCT                                      
         EDIT  SRTOEST,(3,BLEST),FILL=0                                         
         MVC   BLINVNO,SRTOIINV    FULL INVOICE NUMBER                          
         MVC   BLINVMOS,SRTOMOS    MOS MMM/YY                                   
         MVC   BLINVD,SRTOIDAT     INVOICE DATE                                 
         EDIT  SRTOINET,(13,BLGROSS),2,MINUS=YES                                
         MVC   BLSTA,SRTOSTA       STATION/VENDOR CODE                          
         OC    BLSTA,SPACES        SPACE PAD                                    
         EDIT  SRTOBNET,(13,BLVAMT),2,MINUS=YES                                 
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R7                  DROP PRINT LINE USING                        
*                                                                               
         CLC   SRTKSVOU,SRTOKEY    SAME AS PREVIOUS KEY?                        
         BE    OUOUT20             YES - SAME INVOICE                           
*                                                                               
         BAS   RE,ENDINV           END PREVIOUS INV IF THERE WAS ONE            
*                                                                               
         L     R4,AOUTREC          R4 = A(OUTREC)                               
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         LA    RE,PHDINVNO         INVOICE NUMBER WITH NO DASHES                
         XC    PHDINVNO,PHDINVNO   CLEAR PHDINVNO                               
         LA    RF,SRTOIINV         10 CHARACTER INVOICE NUM WITH DASHES         
         LHI   R0,L'SRTOIINV       10 CHARACTERS                                
*                                                                               
OUOUT15  CLI   0(RF),C'-'          DASH?                                        
         BE    *+14                YES - EXCLUDE                                
         MVC   0(1,RE),0(RF)       NO - MOVE INVOICE NUMBER                     
         LA    RE,1(RE)            BUMP PHDINVNO                                
         LA    RF,1(RF)            BUMP SRTOIINV                                
         BCT   R0,OUOUT15          PROCESS NEXT INVOICE CHARACTER               
*                                                                               
         MVC   PHDPUPSE,=C'standard  ' INIT PURPOSE CODE TO "standard"          
         CP    SRTOINET,=P'0'      INVOICE AMOUNT NEGATIVE?                     
         BNL   *+10                NO                                           
         MVC   PHDPUPSE,=C'creditMemo' SET PURPOSE CODE TO "creditMemo"         
***      MVC   PHDEUDEF,SRTOEUD1   ESTIMATE UDEF 1                              
         MVC   PHDEUDEF,SRTOEUD2   ESTIMATE UDEF 2                              
*                                                                               
         LAY   R6,OUHTAB           PHD CXML HEADER TABLE                        
         LAY   R7,OUHSOFT          PHD HEADER SPECIAL FIELDS TABLE              
         BRAS  RE,OUTABLE          MOVE HEADER TABLE TO OUTREC                  
*                                                                               
         AP    HDRCNT,=P'1'        HEADERS                                      
         XC    PHDINVLB,PHDINVLB   CLEAR PHDINVLB                               
*                                                                               
         EDIT  SRTOINET,(13,PHDINVAM),2,FLOAT=-,ALIGN=LEFT                      
*                                                                               
         ZAP   PAK6,SRTOIGST       GST                                          
         EDIT  PAK6,(13,PHDIGST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
         ZAP   PAK6,SRTOIPST       PST                                          
         EDIT  PAK6,(13,PHDIPST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
         ZAP   PAK6,SRTOIGST       GST                                          
         AP    PAK6,SRTOIPST       ADD PST TO GET TOTAL TAX                     
*                                                                               
         EDIT  PAK6,(13,PHDINVTX),2,FLOAT=-,ALIGN=LEFT                          
*                                                                               
         MVC   PHDTAXCD,=C'ZERORATE' INIT INV TAX CODE TO "ZERORATE"            
         MVC   PHDTAXRT,=C'0.000 ' INIT INVOICE TAX RATE TO 0%                  
*                                                                               
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,1,SRTOPROV       HAVE A PROVINCE?                             
         BZ    OUOUT16             NO                                           
         BCTR  RE,0                YES - -1 TO INDEX INTO TABLE                 
         MHI   RE,L'PRVTAB         INDEX INTO PROVINCE TABLE                    
         LAY   R6,PRVTAB           PROVINCE TABLE                               
         AR    R6,RE               INDEX INTO PROVINCE TABLE                    
*                                                                               
         MVC   PHDTAXCD,0(R6)      INVOICE TAX CODE FOR PROVINCE                
         MVC   PHDTAXRT,8(R6)      INVOICE TAX RATE FOR PROVINCE                
*                                                                               
OUOUT16  ZAP   PAK6,SRTOINET       NET AMOUNT                                   
         AP    PAK6,SRTOIGST       +GST                                         
         AP    PAK6,SRTOIPST       +PST                                         
*                                                                               
         EDIT  PAK6,(13,PHDINTOT),2,FLOAT=-,ALIGN=LEFT                          
*                                                                               
         MVI   FILNMLEN,0          INIT FILE NAME LENGTH                        
         MVC   FILENAME,SPACES     INIT FILE NAME                               
*                                                                               
         LA    R7,FILENAME         R7 = FILENAME                                
         MVC   0(1,R7),SRTOMED     MEDIA                                        
         MVI   1(R7),C'-'          DASH                                         
         MVC   2(3,R7),SRTOCLT     CLIENT                                       
*                                                                               
         LA    R7,4(R7)            BUMP TO LAST POSSIBLE BYTE OF CLIENT         
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'-'          DASH                                         
         MVC   2(3,R7),SRTOPRD     PRODUCT                                      
*                                                                               
         LA    R7,4(R7)            BUMP TO LAST POSSIBLE BYTE OF PRD            
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'-'          DASH                                         
*                                                                               
         EDIT  SRTOEST,(3,2(R7)),FILL=0                                         
*                                                                               
         MVI   5(R7),C'-'          DASH                                         
         MVC   6(10,R7),PHDINVNO   INVOICE NUMBER WITH NO DASHES                
*                                                                               
         LA    R7,15(R7)           BUMP TO LAST POSSIBLE BYTE OF INV            
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'-'          DASH                                         
         MVC   2(16,R7),SRTOEUD2   PO NUMBER (EST UDEF 2)                       
*                                                                               
         AHI   R7,L'SRTOEUD2+1     BUMP TO LAST POSSIBLE BYTE OF PO NUM         
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'-'          DASH                                         
         MVC   2(4,R7),CTODAY      YYYY                                         
         MVI   6(R7),C'-'          DASH                                         
         MVC   7(2,R7),CTODAY+4    MM                                           
         MVI   9(R7),C'-'          DASH                                         
         MVC   10(2,R7),CTODAY+6   DD                                           
         MVI   12(R7),C'-'         DASH                                         
         MVC   13(2,R7),TIMEOFDU   HH                                           
         MVI   15(R7),C'-'         DASH                                         
         MVC   16(2,R7),TIMEOFDU+3 MM                                           
         MVC   18(4,R7),=C'.xml'   .xml EXTENSION                               
*                                                                               
         LA    RE,L'FILENAME       MAX FILENAME LENGTH                          
         LA    R7,FILENAME+L'FILENAME-1                                         
*                                                                               
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+10                YES                                          
         BCTR  RE,0                DECREMENT FILE NAME LENGTH                   
         BCT   R7,*-10             NO, CHECK PREVIOUS BYTE                      
*                                                                               
         STC   RE,FILNMLEN         FILE NAME LENGTH                             
         MVC   SRTKSVOU,SRTOKEY    SAVE INVOICE LEVEL SORT KEY                  
*                                                                               
OUOUT20  XR    R6,R6               CLEAR R6                                     
         ICM   R6,3,PHDINVLB       CURRENT INVOICE LINE COUNTER BINARY          
         AHI   R6,1                BUMP INVOICE LINE COUNTER                    
         STCM  R6,3,PHDINVLB       UPDATE INVOICE LINE COUNTER                  
*                                                                               
         EDIT  PHDINVLB,PHDINVLN,ALIGN=LEFT                                     
*                                                                               
         MVC   PHDINVQT,=C'1 '     INIT INVOICE QUANTITY                        
***      ICM   R0,15,SRTOBNET      R0 = VENDOR NET AMOUNT                       
***      C     R0,=F'0'            VENDOR NET AMOUNT NEGATIVE?                  
***      BNL   *+10                NO                                           
***      MVC   PHDINVQT,=C'-1'     INIT INVOICE QUANTITY                        
*                                                                               
         EDIT  SRTOBNET,(12,PHDVRAMT),2,ALIGN=LEFT                              
*                                                                               
         MVC   PHDVNAME,SRTOSNAM   VENDOR NAME                                  
*                                                                               
         LA    R7,PHDVDESC         M_CLT_PRD_EST_EUDEF1_EUDEF2                  
         MVC   0(1,R7),SRTOMED     MEDIA                                        
         MVI   1(R7),C'_'          UNDERSCORE                                   
         MVC   2(3,R7),SRTOCLT     CLIENT                                       
*                                                                               
         LA    R7,4(R7)            BUMP TO LAST POSSIBLE BYTE OF CLIENT         
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'_'          UNDERSCORE                                   
         MVC   2(3,R7),SRTOPRD     PRODUCT                                      
*                                                                               
         LA    R7,4(R7)            BUMP TO LAST POSSIBLE BYTE OF PRD            
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'_'          UNDERSCORE                                   
*                                                                               
         EDIT  SRTOEST,(3,2(R7)),FILL=0                                         
*                                                                               
         MVI   5(R7),C'_'          UNDERSCORE                                   
         MVC   6(32,R7),SRTOEUD1   PO NUMBER (EST UDEF 1)                       
*                                                                               
         LA    R7,39(R7)           BUMP TO LAST POSSIBLE BYTE OF PO NUM         
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'_'          UNDERSCORE                                   
         MVC   2(16,R7),SRTOEUD2   EST UDEF 2                                   
*                                                                               
         EDIT  SRTOBNET,(12,PHDVAMT2),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         LAY   R6,OUINVTAB         PHD CXML VENDOR INVOICE TABLE                
         LAY   R7,OUISOFT          PHD INVOICE SPECIAL FIELDS TABLE             
         BRAS  RE,OUTABLE          MOVE VENDOR TABLE TO OUTREC                  
*                                                                               
         B     OUOUT10             GET NEXT SORT RECORD                         
*                                                                               
OUOUT50  CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BE    OUOUTXIT            YES - DONE                                   
*                                                                               
         BAS   RE,ENDINV           END PREVIOUS INV IF THERE WAS ONE            
         B     OUOUTXIT            DONE                                         
*                                                                               
         USING SRTORECD,R5         SRTORECD DSECT                               
*                                                                               
OUOUT55  CLC   SRTKSVOU,SRTOKEY    SAME AS PREVIOUS KEY?                        
         BE    OUOUT10             YES - SAME INVOICE - NEXT SORT REC           
*                                                                               
         LA    R7,P                POINT TO PRINT LINE                          
         USING BLINED,R7           BLINED DSECT TO COVER PRINT LINE             
         MVC   BLMEDIA,SRTOMED     MEDIA                                        
         MVC   BLCLT,SRTOCLT       CLIENT                                       
         MVC   BLCLNAME,SRTOCNAM   CLIENT NAME                                  
         MVC   BLPRD,SRTOPRD       PRODUCT                                      
         EDIT  SRTOEST,(3,BLEST),FILL=0                                         
         MVC   BLINVNO,SRTOIINV    FULL INVOICE NUMBER                          
         MVC   BLINVMOS,SRTOMOS    MOS MMM/YY                                   
         MVC   BLINVD,SRTOIDAT     INVOICE DATE                                 
         EDIT  SRTOINET,(15,BLGROSS),2,MINUS=YES                                
         ZAP   PAK6,SRTOIGST       GST                                          
         AP    PAK6,SRTOIPST       ADD PST TO GET TOTAL TAX                     
         EDIT  PAK6,(8,BLTAX),2,FLOAT=-                                         
         EDIT  SRTOACOM,(11,BLACOM),2,MINUS=YES                                 
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R7                  DROP PRINT LINE USING                        
*                                                                               
         L     R4,AOUTREC          R4 = A(OUTREC)                               
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         GOTO1 =A(RDPRD),DMCB,(RC),SRTOAM,SRTOCLT,SRTOPRD                       
*                                                                               
         L     RF,ADPRD            A(PRODUCT RECORD)                            
         USING PRDHDR,RF           PRODUCT RECORD DSECT                         
         MVC   PHDPADD1,PADDR1     PRODUCT ADDRESS 1                            
         MVC   PHDPADD2,PADDR2     PRODUCT ADDRESS 2                            
         MVC   PHDPADD3,PADDR3     PRODUCT ADDRESS 3                            
         MVC   PHDPADD4,PADDR4     PRODUCT ADDRESS 4                            
         DROP  RF                  DROP PRODUCT RECORD USING                    
*                                                                               
         LA    RE,PHDINVNO         INVOICE NUMBER WITH NO DASHES                
         XC    PHDINVNO,PHDINVNO   CLEAR PHDINVNO                               
         LA    RF,SRTOIINV         10 CHARACTER INVOICE NUM WITH DASHES         
         LHI   R0,L'SRTOIINV       10 CHARACTERS                                
*                                                                               
OUOUT65  CLI   0(RF),C'-'          DASH?                                        
         BE    *+14                YES - EXCLUDE                                
         MVC   0(1,RE),0(RF)       NO - MOVE INVOICE NUMBER                     
         LA    RE,1(RE)            BUMP PHDINVNO                                
         LA    RF,1(RF)            BUMP SRTOIINV                                
         BCT   R0,OUOUT65          PROCESS NEXT INVOICE CHARACTER               
*                                                                               
         MVC   PHDPUPSE,=C'standard  ' INIT PURPOSE CODE TO "standard"          
         CP    SRTOINET,=P'0'      INVOICE AMOUNT NEGATIVE?                     
         BNL   *+10                NO                                           
         MVC   PHDPUPSE,=C'creditMemo' SET PURPOSE CODE TO "creditMemo"         
         MVC   PHDEUDF1,SRTOEUD1   ESTIMATE UDEF 1                              
*                                                                               
         LAY   R6,OUHTAB2          PHD CXML HEADER TABLE                        
         LAY   R7,OUHSOFT2         PHD HEADER SPECIAL FIELDS TABLE              
         BRAS  RE,OUTABLE          MOVE HEADER TABLE TO OUTREC                  
*                                                                               
         AP    HDRCNT,=P'1'        HEADERS                                      
         XC    PHDINVLB,PHDINVLB   CLEAR PHDINVLB                               
*                                                                               
         EDIT  SRTOINET,(13,PHDINVAM),2,FLOAT=-,ALIGN=LEFT                      
*                                                                               
         ZAP   PAK6,SRTOINET       INVOICE AMOUNT                               
         ICM   R0,15,SRTOACOM      AGENCY COMMISSION                            
         CVD   R0,DUB              PACKED                                       
         AP    PAK6,DUB            INVOICE PLUS AGENCY COMMISSION               
*                                                                               
         EDIT  PAK6,(13,PHDINVAC),2,FLOAT=-,ALIGN=LEFT                          
*                                                                               
         AP    PAK6,SRTOIGST       +GST                                         
         AP    PAK6,SRTOIPST       +PST                                         
*                                                                               
         EDIT  PAK6,(13,PHDINTOT),2,FLOAT=-,ALIGN=LEFT                          
*                                                                               
         MVC   PHDTAXCD,=C'ZERORATE' INIT INV TAX CODE TO "ZERORATE"            
         MVC   PHDTAXRT,=C'0.000 ' INIT INVOICE TAX RATE TO 0%                  
*                                                                               
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,1,SRTOPROV       HAVE A PROVINCE?                             
         BZ    OUOUT66             NO                                           
         BCTR  RE,0                YES - -1 TO INDEX INTO TABLE                 
         MHI   RE,L'PRVTAB         INDEX INTO PROVINCE TABLE                    
         LAY   R6,PRVTAB           PROVINCE TABLE                               
         AR    R6,RE               INDEX INTO PROVINCE TABLE                    
*                                                                               
         MVC   PHDTAXCD,0(R6)      INVOICE TAX CODE FOR PROVINCE                
         MVC   PHDTAXRT,8(R6)      INVOICE TAX RATE FOR PROVINCE                
         MVC   PHDTAXRB,14(R6)     INVOICE TAX RATE BINARY                      
*                                                                               
OUOUT66  ZAP   PAK6,SRTOIGST       GST                                          
         EDIT  PAK6,(13,PHDIGST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
         CLC   =C'.00 ',PHDIGST    HAVE GST?                                    
         BE    OUOUT67             NO                                           
         LAY   RF,50000            GST TAX RATE OF 5%                           
         ZAP   DUB,SRTOINET        NET AMOUNT                                   
         CVB   R1,DUB              CONVERT TO BINARY                            
         STCM  R1,15,FULL          NET AMOUNT                                   
         M     RE,FULL             X ADJ PCT                                    
*                                                                               
         SLDA  RE,1                *2                                           
         D     RE,=F'1000000'      /1000000                                     
         LTR   RF,RF               HAVE REMAINDER?                              
         BNP   *+8                 NO                                           
         AHI   RF,1                YES - ADD 1                                  
         SRA   RF,1                /2                                           
         CVD   RF,DUB              CVD                                          
         ZAP   PAK6,DUB            GST                                          
         EDIT  PAK6,(13,PHDIGST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
OUOUT67  ZAP   PAK6,SRTOIPST       PST                                          
         EDIT  PAK6,(13,PHDIPST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
         CLC   =C'.00 ',PHDIPST    HAVE PST?                                    
         BE    OUOUT68             NO                                           
         ICM   RF,15,PHDTAXRB      PST TAX RATE                                 
         ZAP   DUB,SRTOINET        NET AMOUNT                                   
         CVB   R1,DUB              CONVERT TO BINARY                            
         STCM  R1,15,FULL          NET AMOUNT                                   
         M     RE,FULL             X ADJ PCT                                    
*                                                                               
         SLDA  RE,1                *2                                           
         D     RE,=F'1000000'      /1000000                                     
         LTR   RF,RF               HAVE REMAINDER?                              
         BNP   *+8                 NO                                           
         AHI   RF,1                YES - ADD 1                                  
         SRA   RF,1                /2                                           
         CVD   RF,DUB              CVD                                          
         ZAP   PAK6,DUB            GST                                          
         EDIT  PAK6,(13,PHDIPST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
OUOUT68  ZAP   PAK6,SRTOIGST       GST                                          
         AP    PAK6,SRTOIPST       ADD PST TO GET TOTAL TAX                     
*                                                                               
         EDIT  PAK6,(13,PHDINVTX),2,FLOAT=-,ALIGN=LEFT                          
*                                                                               
         MVI   FILNMLEN,0          INIT FILE NAME LENGTH                        
         MVC   FILENAME,SPACES     INIT FILE NAME                               
*                                                                               
         LA    R7,FILENAME         R7 = FILENAME                                
         MVC   0(1,R7),SRTOMED     MEDIA                                        
         MVI   1(R7),C'-'          DASH                                         
         MVC   2(3,R7),SRTOCLT     CLIENT                                       
*                                                                               
         LA    R7,4(R7)            BUMP TO LAST POSSIBLE BYTE OF CLIENT         
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'-'          DASH                                         
         MVC   2(3,R7),SRTOPRD     PRODUCT                                      
*                                                                               
         LA    R7,4(R7)            BUMP TO LAST POSSIBLE BYTE OF PRD            
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'-'          DASH                                         
*                                                                               
         EDIT  SRTOEST,(3,2(R7)),FILL=0                                         
*                                                                               
         MVI   5(R7),C'-'          DASH                                         
         MVC   6(10,R7),PHDINVNO   INVOICE NUMBER WITH NO DASHES                
*                                                                               
         LA    R7,15(R7)           BUMP TO LAST POSSIBLE BYTE OF INV            
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'-'          DASH                                         
         MVC   2(32,R7),SRTOEUD1   PO NUMBER (EST UDEF 1)                       
*                                                                               
         AHI   R7,L'SRTOEUD1+1     BUMP TO LAST POSSIBLE BYTE OF PO NUM         
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVI   1(R7),C'-'          DASH                                         
         MVC   2(4,R7),CTODAY      YYYY                                         
         MVI   6(R7),C'-'          DASH                                         
         MVC   7(2,R7),CTODAY+4    MM                                           
         MVI   9(R7),C'-'          DASH                                         
         MVC   10(2,R7),CTODAY+6   DD                                           
         MVI   12(R7),C'-'         DASH                                         
         MVC   13(2,R7),TIMEOFDU   HH                                           
         MVI   15(R7),C'-'         DASH                                         
         MVC   16(2,R7),TIMEOFDU+3 MM                                           
         MVC   18(4,R7),=C'.xml'   .xml EXTENSION                               
*                                                                               
         LA    RE,L'FILENAME       MAX FILENAME LENGTH                          
         LA    R7,FILENAME+L'FILENAME-1                                         
*                                                                               
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+10                YES                                          
         BCTR  RE,0                DECREMENT FILE NAME LENGTH                   
         BCT   R7,*-10             NO, CHECK PREVIOUS BYTE                      
*                                                                               
         STC   RE,FILNMLEN         FILE NAME LENGTH                             
         MVC   SRTKSVOU,SRTOKEY    SAVE INVOICE LEVEL SORT KEY                  
*                                                                               
         XR    R6,R6               CLEAR R6                                     
         ICM   R6,3,PHDINVLB       CURRENT INVOICE LINE COUNTER BINARY          
         AHI   R6,1                BUMP INVOICE LINE COUNTER                    
         STCM  R6,3,PHDINVLB       UPDATE INVOICE LINE COUNTER                  
*                                                                               
         EDIT  PHDINVLB,PHDINVLN,ALIGN=LEFT                                     
*                                                                               
         MVC   PHDINVQT,=C'1 '     INIT INVOICE QUANTITY                        
*                                                                               
         BAS   RE,ENDINV           END INVOICE                                  
*                                                                               
         B     OUOUT10             GET NEXT SORT RECORD                         
*                                                                               
OUOUTXIT XIT1                      DONE                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
OUTABLE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,AOUTREC          R1 = A(OUTREC)                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         AR    R1,RF               END OF OUTREC                                
*                                                                               
         XR    R3,R3               CLEAR R3                                     
*                                                                               
OUTAB05  CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    OUTABX              YES - DONE                                   
         BAS   RE,OUSPFLD          PROCESSED SPECIAL FIELD?                     
         BE    OUTAB05             YES, FIELD PROCESSED & R2 BUMPED             
         IC    R3,1(R6)            LENGTH OF LITERAL                            
         BCTR  R3,0                DECREMENT FOR EX                             
         EX    R3,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),2(R6)       MOVE LITERAL TO OUTREC                       
         LA    R4,1(R4,R3)         BUMP TO NEXT POSITION IN OUTREC              
         CR    R4,R1               PAST OUTREC?                                 
         BNH   *+6                 NO                                           
         DC    H'0'                YES - EXPAND OUTREC                          
         LA    R6,3(R3,R6)         BUMP TO NEXT TABLE ENTRY                     
         B     OUTAB05             PROCESS NEXT TABLE ENTRY                     
*                                                                               
OUTABX   XIT1  REGS=(R4)           EXIT BUT KEEP R4 INTACT                      
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
OUSPFLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
SPFLD00  CLI   0(R7),X'FF'         END OF TABLE?                                
         BE    SPFLDNEQ            YES - SET CC NEQ                             
         CLC   0(1,R6),0(R7)       IS THIS A SPECIAL FIELD?                     
         BE    SPFLD10             YES                                          
         LA    R7,4(R7)            BUMP TO NEXT TABLE ENTRY                     
         B     SPFLD00             PROCESS NEXT TABLE ENTRY                     
*                                                                               
SPFLD10  LLC   RF,1(R6)            LENGTH OF LITERAL                            
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),2(R6)       MOVE LITERAL TO TPREC                        
         LA    R4,1(RF,R4)         MOVE SOFT DATA HERE                          
         LA    R6,3(RF,R6)         BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         LA    R2,PHDSFLDB         SPECIAL DATA BUILT HERE                      
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,3,1(R7)          DISPLACEMENT INTO DATA                       
         LLC   RF,3(R7)            LENGTH OF DATA                               
         BCTR  RF,0                DECREMENT FOR EX                             
         AR    R2,RE               INDEX INTO DATA                              
         EX    RF,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),0(R2)       MOVE SOFT DATA                               
         LA    R4,1(RF,R4)         LAST BYTE OF SOFT DATA                       
*                                                                               
         CLI   0(R4),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R4,*-8              NO, CHECK PREVIOUS BYTE                      
         LA    R4,1(R4)            BUMP PAST LAST BYTE OF DATA                  
*                                                                               
         CR    RE,RE               SET CC EQU                                   
         B     SPFLDXIT            AND EXIT                                     
*                                                                               
SPFLDNEQ LTR   RE,RE               SET CC NEQ                                   
*                                                                               
SPFLDXIT XIT1  REGS=(R4,R6)        EXIT BUT KEEP R4 & R6 INTACT                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
ENDINV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,AOUTREC          RE = A(OUTREC)                               
         CLC   0(100,RE),SPACES    DO WE CXML DATA TO SEND?                     
         BE    ENDINVX             NO                                           
*                                                                               
         LAY   R6,OUFTAB           PHD CXML FOOTER TABLE                        
         LAY   R7,OUFSOFT          PHD FOOTER SPECIAL FIELDS TABLE              
         CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BNE   ENDINV07            NO                                           
         MVI   PASS,1              PASS 1                                       
         LAY   R6,OUINTB2A         INVOICE LINE NUMBER                          
         MVI   0(R6),C'1'          LINE NUMBER 1                                
         LAY   R6,OUINTB2B         INOICE LINE NUMBER                           
         MVI   0(R6),C'1'          LINE NUMBER 1                                
*                                                                               
ENDINV05 LAY   R6,OUINVTB2         PHD CXML VENDOR INVOICE TABLE                
ENDINV06 LAY   R7,OUISOFTC         PHD INVOICE SPECIAL FIELDS TABLE             
*                                                                               
ENDINV07 BRAS  RE,OUTABLE          MOVE FOOTER TABLE TO OUTREC                  
*                                                                               
         CLC   =C'.00 ',PHDIGST    HAVE GST?                                    
         BE    ENDINV10            NO                                           
         LAY   R6,OUFTABG          PHD CXML FOOTER TABLE GST TAX                
         LAY   R7,OUFSOFTG         PHD FOOTER SPECIAL FIELDS TABLE              
         CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BNE   ENDINV08            NO                                           
         LAY   R6,OUFTABGC         PHD CXML FOOTER TABLE GST TAX COUPA          
         LAY   R7,OUFSFTG          PHD FOOTER SPECIAL FIELDS COUPA              
         CLI   PASS,1              PASS 1?                                      
         BE    ENDINV08            YES                                          
         OC    SRTOACOM,SRTOACOM   HAVE AGENCY COMMISSION?                      
         BNZ   ENDINV08            YES                                          
         MVC   PHDIGST,SPACES      NO - INIT TO SPACES                          
         MVC   PHDIGST(3),=C'.00'  NO - SET TO .00                              
ENDINV08 BRAS  RE,OUTABLE          MOVE FOOTER TABLE TO OUTREC                  
*                                                                               
ENDINV10 CLC   =C'.00 ',PHDIPST    HAVE PST?                                    
         BE    ENDINV20            NO                                           
         LAY   R6,OUFTABP          PHD CXML FOOTER TABLE PST TAX                
         LAY   R7,OUFSOFTP         PHD FOOTER SPECIAL FIELDS TABLE              
         CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BNE   ENDINV11            NO                                           
         LAY   R6,OUFTABPC         PHD CXML FOOTER TABLE PST TAX COUPA          
         LAY   R7,OUFSFTP          PHD FOOTER SPECIAL FIELDS TABLE              
         CLI   PASS,1              PASS 1?                                      
         BE    ENDINV11            YES                                          
         OC    SRTOACOM,SRTOACOM   HAVE AGENCY COMMISSION?                      
         BNZ   ENDINV11            YES                                          
         MVC   PHDIPST,SPACES      NO - INIT TO SPACES                          
         MVC   PHDIPST(3),=C'.00'  NO - SET TO .00                              
ENDINV11 BRAS  RE,OUTABLE          MOVE FOOTER TABLE TO OUTREC                  
*                                                                               
ENDINV20 CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BNE   ENDINV25            NO                                           
         LAY   R6,OUINVTB3         END InvoiceDetailItem TABLE                  
         LA    R7,=X'FF'           NO SPECIAL FIELDS FOR THIS                   
         BRAS  RE,OUTABLE          MOVE TABLE TO OUTREC                         
*                                                                               
         CLI   PASS,1              PASS 1?                                      
         BNE   ENDINV25            NO, ALREADY PROCESSED AGY COMMISSION         
*                                                                               
         MVI   PASS,2              SET PASS 2                                   
         LAY   R6,OUINTB2A         INVOICE LINE NUMBER                          
         MVI   0(R6),C'2'          LINE NUMBER 2                                
         LAY   R6,OUINTB2B         INOICE LINE NUMBER                           
         MVI   0(R6),C'2'          LINE NUMBER 2                                
*                                                                               
         EDIT  SRTOACOM,(12,PHDINVAM),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         OC    SRTOACOM,SRTOACOM   HAVE AGENCY COMMISSION?                      
         BZ    ENDINV22            NO - LEAVE GST & PST AS-IS                   
                                                                                
         CLC   =C'.00 ',PHDIGST    HAVE GST?                                    
         BE    ENDINV21            NO                                           
         LAY   RF,50000            GST TAX RATE OF 5%                           
         MVC   FULL,SRTOACOM       AGENCY COMMISSION                            
         M     RE,FULL             X ADJ PCT                                    
*                                                                               
         SLDA  RE,1                *2                                           
         D     RE,=F'1000000'      /1000000                                     
         LTR   RF,RF               HAVE REMAINDER?                              
         BNP   *+8                 NO                                           
         AHI   RF,1                YES - ADD 1                                  
         SRA   RF,1                /2                                           
         CVD   RF,DUB              CVD                                          
         ZAP   PAK6,DUB            GST                                          
         EDIT  PAK6,(13,PHDIGST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
ENDINV21 CLC   =C'.00 ',PHDIPST    HAVE PST?                                    
         BE    ENDINV22            NO                                           
         ICM   RF,15,PHDTAXRB      PST TAX RATE                                 
         MVC   FULL,SRTOACOM       AGENCY COMMISSION                            
         M     RE,FULL             X ADJ PCT                                    
*                                                                               
         SLDA  RE,1                *2                                           
         D     RE,=F'1000000'      /1000000                                     
         LTR   RF,RF               HAVE REMAINDER?                              
         BNP   *+8                 NO                                           
         AHI   RF,1                YES - ADD 1                                  
         SRA   RF,1                /2                                           
         CVD   RF,DUB              CVD                                          
         ZAP   PAK6,DUB            GST                                          
         EDIT  PAK6,(13,PHDIPST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
ENDINV22 LAY   R6,OUINVTBL         invoiceLineNumber="2"                        
         B     ENDINV06                                                         
*                                                                               
ENDINV25 LAY   R6,OUFTAB2          PHD CXML FOOTER TABLE 2                      
         LAY   R7,OUFSOFT2         PHD FOOTER SPECIAL FIELDS TABLE              
*                                                                               
         CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BNE   ENDINV28            NO                                           
*                                                                               
         LAY   R6,OUFTABC          PHD CXML FOOTER TABLE 2 COUPA                
         LAY   R7,OUFSFTC2         PHD FOOTER SPECIAL FIELDS COUPA              
*                                                                               
         BRAS  RE,OUTABLE          MOVE FOOTER TABLE TO OUTREC                  
*                                                                               
         ZAP   PAK6,SRTOIGST       RESTORE ORIGINAL GST                         
         EDIT  PAK6,(13,PHDIGST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
         CLC   =C'.00 ',PHDIGST    HAVE GST?                                    
         BE    ENDINV26            NO                                           
         LAY   R6,OUFTABGF         PHD CXML FOOTER TABLE GST TAX COUPA          
         LAY   R7,OUFSFTGT         PHD FOOTER SPECIAL FIELDS COUPA              
         BRAS  RE,OUTABLE          MOVE FOOTER TABLE TO OUTREC                  
*                                                                               
ENDINV26 ZAP   PAK6,SRTOIPST       RESTORE ORIGINAL PST                         
         EDIT  PAK6,(13,PHDIPST),2,FLOAT=-,ALIGN=LEFT                           
*                                                                               
         CLC   =C'.00 ',PHDIPST    HAVE PST?                                    
         BE    ENDINV27            NO                                           
         LAY   R6,OUFTABPF         PHD CXML FOOTER TABLE PST TAX COUPA          
         LAY   R7,OUFSFTPT         PHD FOOTER SPECIAL FIELDS TABLE              
         BRAS  RE,OUTABLE          MOVE FOOTER TABLE TO OUTREC                  
*                                                                               
ENDINV27 LAY   R6,OUFTABCF         PHD CXML FOOTER TABLE 2 COUPA                
         LAY   R7,OUFSFTC3         PHD FOOTER SPECIAL FIELDS COUPA              
*                                                                               
ENDINV28 BRAS  RE,OUTABLE          MOVE FOOTER TABLE TO OUTREC                  
*                                                                               
         GOTO1 =A(UNIXFCRT),DMCB,(RC)   CREATE UNIX FILE                        
         GOTO1 =A(UNIXFPUT),DMCB,(RC)   ADD DATA TO UNIX FILE                   
         GOTO1 =A(UNIXFCLO),DMCB,(RC)   CLOSE UNIX FILE                         
*                                                                               
         MVC   QUALIFY,BILLING     QUALIFIER IS BILLING                         
*                                                                               
         L     RF,VMASTC           A(MASTC)                                     
         USING MASTD,RF            MASTD DSECT                                  
         L     RE,MCAEXTRA         EXTRA DATA AREA                              
         USING MCEXTRA,RE          EXTRA DATA AREA DSECT                        
         MVC   AGENCYID,MCAGYCOD   AGENCY LABEL ON ACCESS RECORD                
         DROP  RE,RF               DROP USINGS                                  
*                                                                               
         CLC   AGENCYID,=C'OUTO'   AGENCY OU?                                   
         BNE   ENDINV30            NO                                           
         MVC   AGENCYID,=C'OUUX'   YES - OUTO ALREADY SET UP ON HUB             
         CLI   SVQOPT1,C'C'        TAPE OPTION C?                               
         BNE   ENDINV30            NO                                           
         MVC   AGENCYID,=C'OUU2'   YES - OUTO ALREADY SET UP ON HUB             
*                                                                               
ENDINV30 GOTO1 AMQMSG,DMCB,(RC)    SEND MQ MESSAGE WITH FILENAME                
*                                                                               
         BRAS  RE,PRINTXML         PRINT XML DATA                               
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
ENDINVX  XIT1                      EXIT                                         
*                                                                               
BILLING  DC    C'BILLING         '                                              
*                                                                               
PRINTXML NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   RCWHATPR,2          SET TO SECOND SYSPRINT                       
         MVC   SVLINE,LINE         SAVE PRINT LINE FROM REPORT 1                
         MVC   SVFORCEH,FORCEHED   SAVE FORCEHED                                
         MVI   LINE,0              RESET LINE FOR SECOND SYSPRINT               
         MVI   FORCEHED,C'N'       NO HEADLINES FOR SECOND SYSPRINT             
*                                                                               
         MVC   P(L'FILENAME),FILENAME                                           
         GOTO1 REPORT                                                           
*                                                                               
         L     R6,AOUTREC          RE = A(OUTREC)                               
         LAY   R7,OUTRECLN-17      LENGTH OF OUTREC-1                           
         AR    R7,R6               LAST BYTE OF OUTREC                          
*                                                                               
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         LA    R4,1(R7)            1 PAST LAST BYTE                             
*                                                                               
         L     R7,AOUTREC          RE = A(OUTREC)                               
         MVI   INDENT,0            INIT INDENT                                  
         MVI   CLOSETAG,C'N'       INIT CLOSETAG                                
*                                                                               
PXML00   CR    R7,R4               EOF?                                         
         BE    PXML100             YES - DONE                                   
*                                                                               
         CLC   =C'<!--',0(R7)      XML COMMENT?                                 
         BNE   PXML00C             NO                                           
*                                                                               
PXML00A  CLC   =C'-->',0(R7)       END OF XML COMMENT?                          
         BE    PXML00B             YES                                          
         LA    R7,1(R7)            BUMP TO NEXT BYTE                            
         CR    R7,R4               EOF?                                         
         BE    PXML100             YES - DONE                                   
         B     PXML00A             KEEP CHECKING FOR END OF XML COMMENT         
*                                                                               
PXML00B  AHI   R7,3                BUMP BY 3                                    
         B     PXML00              COMMENTS ARE SKIPPED                         
*                                                                               
PXML00C  CLI   0(R7),C'<'          OPENING TAG?                                 
         BNE   PXML50              NO                                           
*                                                                               
         LR    R1,R7               SAVE A(OPENING TAG)                          
         CLI   1(R7),C'/'          CLOSING TAG?                                 
         BNE   PXML02              NO                                           
*                                                                               
PXML01   CLI   0(R7),C'>'          CLOSING TAG?                                 
         BE    *+12                YES                                          
         LA    R7,1(R7)            BUMP R7                                      
         B     PXML01              CHECK NEXT BYTE FOR CLOSING TAG              
*                                                                               
         LLC   RF,INDENT           CURRENT INDENTATION                          
         SHI   RF,2                BACK UP 2                                    
         STC   RF,INDENT           NEW CURRENT INDENTATION                      
         MVI   CLOSETAG,C'Y'       INDICATE THAT WE JUST CLOSED A TAG           
*                                                                               
         BAS   RE,PXMLDATA         PRINT 1 LINE OF XML DATA                     
         B     PXML50              BUMP TO NEXT BYTE                            
*                                                                               
PXML02   CLI   CLOSETAG,C'Y'       DID WE PREVIOUSLY CLOSE A TAG?               
         BNE   *+12                NO                                           
         MVI   CLOSETAG,C'N'       RESET CLOSETAG                               
         B     PXML02A             LEAVE INDENT AS IS                           
         CLI   LINE,3              PAST 3RD LINE?                               
         BNH   PXML02A             NO, DON'T INDENT YET                         
         LLC   RF,INDENT           CURRENT INDENTATION                          
         AHI   RF,2                ADD 2                                        
         STC   RF,INDENT           NEW CURRENT INDENTATION                      
*                                                                               
PXML02A  CLI   0(R7),C'>'          CLOSING TAG?                                 
         BE    *+12                YES                                          
         LA    R7,1(R7)            BUMP R7                                      
         B     PXML02A             CHECK NEXT BYTE FOR CLOSING TAG              
*                                                                               
         LR    R3,R7               SAVE A(CLOSING TAG)                          
         BCTR  R3,0                PREVIOUS BYTE                                
         CLI   0(R3),C'/'          IS THIS SELF CLOSING?                        
         BNE   PXML03              NO                                           
         MVI   CLOSETAG,C'Y'       INDICATE THAT WE JUST CLOSED A TAG           
         BAS   RE,PXMLDATA         YES - PRINT 1 LINE OF XML DATA               
         B     PXML50              BUMP TO NEXT BYTE                            
*                                                                               
PXML03   LR    R3,R7               SAVE A(CLOSING TAG)                          
*                                                                               
PXML04   CLI   0(R7),C'<'          NEXT OPENING TAG?                            
         BE    *+12                YES                                          
         LA    R7,1(R7)            BUMP R7                                      
         B     PXML04              CHECK NEXT BYTE FOR OPENING TAG              
*                                                                               
         CLI   1(R7),C'/'          SELF CLOSING TAG?                            
         BE    PXML05              YES                                          
         LR    R7,R3               NO - RESTORE TO PREV CLOSING TAG             
         BAS   RE,PXMLDATA         YES - PRINT 1 LINE OF XML DATA               
         B     PXML50              BUMP TO NEXT BYTE                            
*                                                                               
PXML05   CLI   0(R7),C'>'          CLOSING TAG?                                 
         BE    *+12                YES                                          
         LA    R7,1(R7)            BUMP R7                                      
         B     PXML05              CHECK NEXT BYTE FOR CLOSING TAG              
*                                                                               
         MVI   CLOSETAG,C'Y'       INDICATE THAT WE JUST CLOSED A TAG           
         BAS   RE,PXMLDATA         PRINT 1 LINE OF XML DATA                     
*                                                                               
PXML50   LA    R7,1(R7)            BUMP TO NEXT BYTE                            
         B     PXML00              TEST NEXT BYTE                               
*                                                                               
PXML100  GOTO1 REPORT              PRINT BLANK LINE                             
         MVI   RCWHATPR,1          RESET TO FIRST SYSPRINT                      
         MVC   LINE,SVLINE         RESTORE LINE                                 
         MVC   FORCEHED,SVFORCEH   RESTORE FORCEHED                             
*                                                                               
PXMLXIT  XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
PXMLDATA LR    R0,RE               SAVE OFF RE                                  
         LLC   RF,INDENT           CURRENT INDENTATION                          
         LA    RE,P                PRINT LINE                                   
         AR    RE,RF               ADD INDENTATION                              
         LR    R2,R7               PRINT UP TO THIS ADDRESS                     
         SR    R2,R1               GET LENGTH OF DATA TO PRINT                  
         EX    R2,*+8              EXECUTE MVC                                  
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   0(0,RE),0(R1)       MOVE XML DATA TO PRINT LINE                  
         MVI   FORCEHED,C'N'       NO HEADLINES FOR SECOND SYSPRINT             
*                                                                               
         GOTO1 REPORT              PRINT XML DATA TO SECOND SYSPRINT            
*                                                                               
         CLI   LINE,5              IS LINE > 5?                                 
         BNH   *+8                 NO                                           
         MVI   LINE,5              YES - SUPPRESS LINE BREAKS                   
*                                                                               
         LR    RE,R0               RESTORE RE                                   
         BR    RE                  RETURN TO CALLER                             
*                                                                               
OUHTAB   DC    X'01',X'26',C'<?xml version="1.0" encoding="UTF-8"?>'            
         DC    X'02',X'53',C'<!DOCTYPE cXML SYSTEM "http://xml.cXML.or'         
         DC                C'g/schemas/cXML/1.2.020/InvoiceDetail.dtd"'         
         DC                C'>'                                                 
         DC    X'03',X'41',C'<cXML version="1.0" payloadID="'                   
         DC                C'1240598937@SUBDOMAIN.coupahost.com'                
         DC    X'04',X'28',C'" timestamp="'                                     
OUHDSTMP DC                C'2009-05-04T'                                       
OUHTSTMP DC                C'01:24:51-07:00">'                                  
         DC    X'04',X'08',C'<Header>'                                          
         DC    X'05',X'06',C'<From>'                                            
         DC    X'06',X'1A',C'<Credential domain="DUNS">'                        
         DC    X'07',X'20',C'<Identity>PHD_OMNICOM</Identity>'                  
         DC    X'08',X'0D',C'</Credential>'                                     
         DC    X'09',X'07',C'</From>'                                           
         DC    X'0A',X'04',C'<To>'                                              
         DC    X'0B',X'1A',C'<Credential domain="DUNS">'                        
         DC    X'0C',X'1B',C'<Identity>SCOTIA</Identity>'                       
         DC    X'0D',X'0D',C'</Credential>'                                     
         DC    X'0E',X'05',C'</To>'                                             
         DC    X'0F',X'08',C'<Sender>'                                          
         DC    X'10',X'1A',C'<Credential domain="DUNS">'                        
         DC    X'11',X'20',C'<Identity>PHD_OMNICOM</Identity>'                  
         DC    X'12',X'1B',C'<SharedSecret>ScotiaPHD2019'                       
         DC    X'12',X'0F',C'</SharedSecret>'                                   
         DC    X'13',X'0D',C'</Credential>'                                     
         DC    X'14',X'22',C'<UserAgent>Procurement</UserAgent>'                
         DC    X'15',X'09',C'</Sender>'                                         
         DC    X'16',X'09',C'</Header>'                                         
         DC    X'17',X'25',C'<Request deploymentMode="production">'             
         DC    X'18',X'16',C'<InvoiceDetailRequest>'                            
         DC    X'19',X'27',C'<InvoiceDetailRequestHeader invoiceID="'           
         DC    X'1A',X'0B',C'" purpose="'                                       
         DC    X'1B',X'3A',C'" operation="new" invoiceDate="'                   
OUHDSTM2 DC                C'2017-06-11T'                                       
OUHTSTM2 DC                C'11:45:51-07:00">'                                  
         DC    X'1C',X'20',C'<InvoiceDetailHeaderIndicator />'                  
         DC    X'1C',X'36',C'<InvoiceDetailLineIndicator isAccounting'          
         DC                C'InLine="yes"/>'                                    
         DC    X'1C',X'10',C'<InvoicePartner>'                                  
         DC    X'1C',X'18',C'<Contact role="remitTo">'                          
         DC    X'1D',X'1B',C'<Name xml:lang="en"></Name>'                       
         DC    X'1E',X'0F',C'<PostalAddress>'                                   
         DC    X'1F',X'22',C'<Street>96 Spadina Avenue</Street>'                
         DC    X'20',X'1A',C'<Street>Suite 600</Street>'                        
         DC    X'21',X'14',C'<City>Toronto</City>'                              
         DC    X'22',X'16',C'<State>Ontario</State>'                            
         DC    X'23',X'20',C'<PostalCode>M5V 2J6</PostalCode>'                  
         DC    X'24',X'2D',C'<Country isoCountryCode="CA">Canada</Coun'         
         DC                C'try>'                                              
         DC    X'25',X'10',C'</PostalAddress>'                                  
         DC    X'26',X'0A',C'</Contact>'                                        
         DC    X'27',X'11',C'</InvoicePartner>'                                 
         DC    X'28',X'1D',C'</InvoiceDetailRequestHeader>'                     
         DC    X'FF'                                                            
*                                                                               
OUINVTAB DC    X'01',X'14',C'<InvoiceDetailOrder>'                              
         DC    X'02',X'18',C'<InvoiceDetailOrderInfo>'                          
         DC    X'03',X'10',C'<OrderReference>'                                  
         DC    X'04',X'1E',C'<DocumentReference payloadID="'                    
         DC    X'05',X'04',C'" />'                                              
         DC    X'06',X'11',C'</OrderReference>'                                 
         DC    X'07',X'19',C'</InvoiceDetailOrderInfo>'                         
         DC    X'08',X'2D',C'<InvoiceDetailServiceItem invoiceLineNumb'         
         DC                C'er="'                                              
         DC    X'09',X'02',C'">'                                                
         DC    X'0A',X'2F',C'<InvoiceDetailServiceItemReference lineNu'         
         DC                C'mber="'                                            
         DC    X'0B',X'02',C'">'                                                
         DC    X'0C',X'1B',C'<Description xml:lang="en">'                       
         DC    X'0D',X'0E',C'</Description>'                                    
         DC    X'0E',X'24',C'</InvoiceDetailServiceItemReference>'              
         DC    X'0F',X'10',C'<SubtotalAmount>'                                  
         DC    X'10',X'16',C'<Money currency="CAD">'                            
         DC    X'11',X'08',C'</Money>'                                          
         DC    X'12',X'11',C'</SubtotalAmount>'                                 
         DC    X'13',X'1B',C'</InvoiceDetailServiceItem>'                       
         DC    X'14',X'15',C'</InvoiceDetailOrder>'                             
         DC    X'FF'                                                            
*                                                                               
OUFTAB   DC    X'01',X'16',C'<InvoiceDetailSummary>'                            
         DC    X'01',X'10',C'<SubtotalAmount>'                                  
         DC    X'02',X'16',C'<Money currency="CAD">'                            
         DC    X'03',X'08',C'</Money>'                                          
         DC    X'04',X'11',C'</SubtotalAmount>'                                 
         DC    X'05',X'05',C'<Tax>'                                             
         DC    X'06',X'16',C'<Money currency="CAD">'                            
         DC    X'07',X'08',C'</Money>'                                          
         DC    X'08',X'1D',C'<Description xml:lang="CA" />'                     
         DC    X'FF'                                                            
*                                                                               
OUFTABG  DC    X'09',X'57',C'<TaxDetail purpose="TAX" taxPointDate="'           
OUFDSTMP DC                C'2017-05-01T'                                       
OUFTSTMP DC                C'11:45:51-07:00" percentageRate="5.000'             
         DC    X'0A',X'0F',C'" category="GST'                                   
         DC    X'0C',X'02',C'">'                                                
         DC    X'0D',X'0F',C'<TaxableAmount>'                                   
         DC    X'0E',X'16',C'<Money currency="CAD">'                            
         DC    X'0F',X'08',C'</Money>'                                          
         DC    X'10',X'10',C'</TaxableAmount>'                                  
         DC    X'11',X'0B',C'<TaxAmount>'                                       
         DC    X'12',X'16',C'<Money currency="CAD">'                            
         DC    X'13',X'08',C'</Money>'                                          
         DC    X'14',X'0C',C'</TaxAmount>'                                      
         DC    X'15',X'2B',C'<TaxLocation xml:lang="en">CA</TaxLocatio'         
         DC                C'n>'                                                
         DC    X'16',X'0C',C'</TaxDetail>'                                      
         DC    X'FF'                                                            
*                                                                               
OUFTABP  DC    X'09',X'52',C'<TaxDetail purpose="TAX" taxPointDate="'           
OUFDSTM2 DC                C'2017-05-01T'                                       
OUFTSTM2 DC                C'11:45:51-07:00" percentageRate="'                  
         DC    X'0A',X'0C',C'" category="'                                      
         DC    X'0C',X'02',C'">'                                                
         DC    X'0D',X'0F',C'<TaxableAmount>'                                   
         DC    X'0E',X'16',C'<Money currency="CAD">'                            
         DC    X'0F',X'08',C'</Money>'                                          
         DC    X'10',X'10',C'</TaxableAmount>'                                  
         DC    X'11',X'0B',C'<TaxAmount>'                                       
         DC    X'12',X'16',C'<Money currency="CAD">'                            
         DC    X'13',X'08',C'</Money>'                                          
         DC    X'14',X'0C',C'</TaxAmount>'                                      
         DC    X'15',X'2B',C'<TaxLocation xml:lang="en">CA</TaxLocatio'         
         DC                C'n>'                                                
         DC    X'16',X'0C',C'</TaxDetail>'                                      
         DC    X'FF'                                                            
*                                                                               
OUFTAB2  DC    X'17',X'06',C'</Tax>'                                            
         DC    X'18',X'17',C'<SpecialHandlingAmount>'                           
         DC    X'19',X'1A',C'<Money currency="CAD">0.00'                        
         DC    X'1A',X'08',C'</Money>'                                          
         DC    X'1B',X'18',C'</SpecialHandlingAmount>'                          
         DC    X'18',X'10',C'<ShippingAmount>'                                  
         DC    X'19',X'1A',C'<Money currency="CAD">0.00'                        
         DC    X'1A',X'08',C'</Money>'                                          
         DC    X'1B',X'11',C'</ShippingAmount>'                                 
         DC    X'1C',X'0B',C'<NetAmount>'                                       
         DC    X'1D',X'16',C'<Money currency="CAD">'                            
         DC    X'1E',X'08',C'</Money>'                                          
         DC    X'1F',X'0C',C'</NetAmount>'                                      
         DC    X'20',X'17',C'</InvoiceDetailSummary>'                           
         DC    X'21',X'17',C'</InvoiceDetailRequest>'                           
         DC    X'22',X'0A',C'</Request>'                                        
         DC    X'23',X'07',C'</cXML>'                                           
         DC    X'FF'                                                            
***                                                                             
* PHD CXML INVOICE HEADER SPECIAL FIELDS TABLE                                  
* THIS MAPS AGAINST THE FIRST FIELD OF OUHTAB AND IF IT MATCHES                 
* WE INDEX INTO PHDSFLDD (BUILT IN PHDSFLDB) USING THE DISPLACEMENT             
* IN THE SECOND FIELD FOR A PRE-DEFINED LENGTH (3RD FIELD)                      
* WE DO THIS IN ORDER TO HANDLE DATA THAT IS SOFT & VARIABLE IN LENGTH          
*                                                                               
* FORMAT IS XL1 - FIELD NUMBER (MATCHED AGAINST OUHTAB)                         
*           AL1 - DISPLACEMENT INTO PHDSFLDD IN PHDSFLDB                        
*           AL1 - MAX LENGTH OF SOFT DATA                                       
***                                                                             
OUHSOFT  DC    X'03',AL2(PHDINVNO-PHDSFLDD),AL1(L'PHDINVNO)                     
         DC    X'19',AL2(PHDINVNO-PHDSFLDD),AL1(L'PHDINVNO)                     
         DC    X'1A',AL2(PHDPUPSE-PHDSFLDD),AL1(L'PHDPUPSE)                     
         DC    X'FF'                                                            
***                                                                             
* PHD CXML INVOICE DETAIL (VENDOR LEVEL) SPECIAL FIELDS TABLE                   
* THIS MAPS AGAINST THE FIRST FIELD OF OUINVTAB AND IF IT MATCHES               
* WE INDEX INTO PHDSFLDD (BUILT IN PHDSFLDB) USING THE DISPLACEMENT             
* IN THE SECOND FIELD FOR A PRE-DEFINED LENGTH (3RD FIELD)                      
* WE DO THIS IN ORDER TO HANDLE DATA THAT IS SOFT & VARIABLE IN LENGTH          
*                                                                               
* FORMAT IS XL1 - FIELD NUMBER (MATCHED AGAINST OUINVTAB)                       
*           AL1 - DISPLACEMENT INTO PHDSFLDD IN PHDSFLDB                        
*           AL1 - MAX LENGTH OF SOFT DATA                                       
***                                                                             
OUISOFT  DC    X'04',AL2(PHDEUDEF-PHDSFLDD),AL1(L'PHDEUDEF)                     
         DC    X'08',AL2(PHDINVLN-PHDSFLDD),AL1(L'PHDINVLN)                     
         DC    X'0A',AL2(PHDINVQT-PHDSFLDD),AL1(L'PHDINVQT)                     
         DC    X'0C',AL2(PHDVDESC-PHDSFLDD),AL1(L'PHDVDESC)                     
         DC    X'10',AL2(PHDVAMT2-PHDSFLDD),AL1(L'PHDVAMT2)                     
         DC    X'FF'                                                            
***                                                                             
* PHD CXML INVOICE FOOTER SPECIAL FIELDS TABLE                                  
* THIS MAPS AGAINST THE FIRST FIELD OF OUFTAB AND IF IT MATCHES                 
* WE INDEX INTO PHDSFLDD (BUILT IN PHDSFLDB) USING THE DISPLACEMENT             
* IN THE SECOND FIELD FOR A PRE-DEFINED LENGTH (3RD FIELD)                      
* WE DO THIS IN ORDER TO HANDLE DATA THAT IS SOFT & VARIABLE IN LENGTH          
*                                                                               
* FORMAT IS XL1 - FIELD NUMBER (MATCHED AGAINST OUFTAB)                         
*           AL1 - DISPLACEMENT INTO PHDSFLDD IN PHDSFLDB                        
*           AL1 - MAX LENGTH OF SOFT DATA                                       
***                                                                             
OUFSOFT  DC    X'02',AL2(PHDINVAM-PHDSFLDD),AL1(L'PHDINVAM)                     
         DC    X'06',AL2(PHDINVTX-PHDSFLDD),AL1(L'PHDINVTX)                     
         DC    X'FF'                                                            
*                                                                               
OUFSOFTG DC    X'0E',AL2(PHDINVAM-PHDSFLDD),AL1(L'PHDINVAM)                     
         DC    X'12',AL2(PHDIGST-PHDSFLDD),AL1(L'PHDIGST)                       
         DC    X'FF'                                                            
*                                                                               
OUFSOFTP DC    X'09',AL2(PHDTAXRT-PHDSFLDD),AL1(L'PHDTAXRT)                     
         DC    X'0A',AL2(PHDTAXCD-PHDSFLDD),AL1(L'PHDTAXCD)                     
         DC    X'0E',AL2(PHDINVAM-PHDSFLDD),AL1(L'PHDINVAM)                     
         DC    X'12',AL2(PHDIPST-PHDSFLDD),AL1(L'PHDIPST)                       
         DC    X'FF'                                                            
*                                                                               
OUFSOFT2 DC    X'1D',AL2(PHDINTOT-PHDSFLDD),AL1(L'PHDINTOT)                     
         DC    X'FF'                                                            
*                                                                               
PRVTAB   DS    0CL18                                                            
         DC    C'PST BC  ',C'7.000 ',X'00011170'  BRITISH COLUMBIA              
         DC    C'GST     ',C'5.000 ',X'0000C350'  ALBERTA                       
         DC    C'PST SK  ',C'6.000 ',X'0000EA60'  SASKATCHEWAN                  
         DC    C'PST MB  ',C'8.000 ',X'00013880'  MANITOBA                      
         DC    C'HST ON  ',C'13.000',X'0001FBD0'  ONTARIO                       
         DC    C'QST QC  ',C'9.975 ',X'000185A6'  QUEBEC                        
         DC    C'HST NB  ',C'15.000',X'000249F0'  NEW BRUNSWICK                 
         DC    C'HST NS  ',C'15.000',X'000249F0'  NOVA SCOTIA                   
         DC    C'HST PEI ',C'14.000',X'000222E0'  PRINCE EDWARD ISLAND          
         DC    C'HST NFL ',C'15.000',X'000249F0'  NEWFOUNDLAND                  
         DC    X'FF'                                                            
***                                                                             
* EDI PHD/Coupa for Canadian Tire                                               
***                                                                             
OUHTAB2  DC    X'FE',X'26',C'<?xml version="1.0" encoding="UTF-8"?>'            
         DC    X'FE',X'53',C'<!DOCTYPE cXML SYSTEM "http://xml.cXML.or'         
         DC                C'g/schemas/cXML/1.2.020/InvoiceDetail.dtd"'         
         DC                C'>'                                                 
         DC    X'FE',X'28',C'<cXML version="1.2.020" xml:lang="en-US"'          
         DC    X'FE',X'50',C' timestamp="'                                      
OUHDSTM3 DC                C'2009-05-04T'                                       
OUHTSTM3 DC                C'01:24:51-07:00" '                                  
         DC                C'payloadID="xxxx.xxxx.xxx@demo.coupahost">'         
*                                                                               
         DC    X'FE',X'08',C'<Header>'                                          
         DC    X'FE',X'06',C'<From>'                                            
         DC    X'FE',X'1A',C'<Credential domain="DUNS">'                        
         DC    X'FE',X'19',C'<Identity>XLR8</Identity>'                         
         DC    X'FE',X'0D',C'</Credential>'                                     
         DC    X'FE',X'07',C'</From>'                                           
         DC    X'FE',X'04',C'<To>'                                              
         DC    X'FE',X'1A',C'<Credential domain="DUNS">'                        
         DC    X'FE',X'21',C'<Identity>CANADIANTIRE</Identity>'                 
         DC    X'FE',X'0D',C'</Credential>'                                     
         DC    X'FE',X'05',C'</To>'                                             
         DC    X'FE',X'08',C'<Sender>'                                          
         DC    X'FE',X'1A',C'<Credential domain="DUNS">'                        
         DC    X'FE',X'19',C'<Identity>XLR8</Identity>'                         
         DC    X'FE',X'1E',C'<SharedSecret>CANADIANTIRE2019'                    
         DC    X'FE',X'0F',C'</SharedSecret>'                                   
         DC    X'FE',X'0D',C'</Credential>'                                     
         DC    X'FE',X'22',C'<UserAgent>Procurement</UserAgent>'                
         DC    X'FE',X'09',C'</Sender>'                                         
         DC    X'FE',X'09',C'</Header>'                                         
*                                                                               
         DC    X'FE',X'25',C'<Request deploymentMode="production">'             
         DC    X'FE',X'16',C'<InvoiceDetailRequest>'                            
         DC    X'FE',X'1B',C'<InvoiceDetailRequestHeader'                       
         DC    X'FE',X'27',C' invoiceDate="'                                    
OUHDSTM4 DC                C'2017-06-11T'                                       
OUHTSTM4 DC                C'11:45:51-07:00'                                    
         DC    X'FE',X'11',C'" operation="new"'                                 
         DC    X'01',X'0A',C' purpose="'                                        
         DC    X'FE',X'1A',C'" invoiceOrigin="supplier"'                        
         DC    X'02',X'0C',C' invoiceID="'                                      
         DC    X'FE',X'02',C'">'                                                
         DC    X'FE',X'20',C'<InvoiceDetailHeaderIndicator />'                  
         DC    X'FE',X'49',C'<InvoiceDetailLineIndicator isAccounting'          
         DC                C'InLine="yes" isTaxInLine="yes" />'                 
*                                                                               
         DC    X'FE',X'10',C'<InvoicePartner>'                                  
         DC    X'FE',X'17',C'<Contact role="soldTo">'                           
         DC    X'FE',X'27',C'<Name xml:lang="en-US">Customer Contact'           
         DC    X'FE',X'07',C'</Name>'                                           
         DC    X'FE',X'0F',C'<PostalAddress>'                                   
         DC    X'03',X'0B',C'<DeliverTo>'                                       
         DC    X'FE',X'0C',C'</DeliverTo>'                                      
         DC    X'FE',X'1E',C'<Street>P.O. Box 5008</Street>'                    
         DC    X'FE',X'17',C'<City>Burlington</City>'                           
         DC    X'FE',X'20',C'<PostalCode>L7R 0C1</PostalCode>'                  
         DC    X'FE',X'1F',C'<Country isoCountryCode="CA" />'                   
         DC    X'FE',X'10',C'</PostalAddress>'                                  
         DC    X'FE',X'0A',C'</Contact>'                                        
         DC    X'FE',X'11',C'</InvoicePartner>'                                 
*                                                                               
         DC    X'FE',X'10',C'<InvoicePartner>'                                  
         DC    X'FE',X'28',C'<Contact role="remitTo" addressID="CAN">'          
         DC    X'FE',X'25',C'<Name xml:lang="en">Remit Name</Name>'             
         DC    X'FE',X'26',C'<PostalAddress name="XLR8 MEDIA INC.">'            
         DC    X'FE',X'24',C'<Street>400-3575 boul. Saint-Laurent'              
         DC    X'FE',X'09',C'</Street>'                                         
         DC    X'FE',X'15',C'<City>Montreal</City>'                             
         DC    X'FE',X'20',C'<PostalCode>H2X 2T7</PostalCode>'                  
         DC    X'FE',X'1F',C'<Country isoCountryCode="CA" />'                   
         DC    X'FE',X'10',C'</PostalAddress>'                                  
         DC    X'FE',X'0A',C'</Contact>'                                        
         DC    X'FE',X'1F',C'<IdReference domain="taxPrefix"'                   
         DC    X'FE',X'11',C' identifier="" />'                                 
         DC    X'FE',X'1F',C'<IdReference domain="taxNumber"'                   
         DC    X'FE',X'11',C' identifier="" />'                                 
         DC    X'FE',X'11',C'</InvoicePartner>'                                 
*                                                                               
         DC    X'FE',X'10',C'<InvoicePartner>'                                  
         DC    X'FE',X'1B',C'<Contact role="invoiceFrom"'                       
         DC    X'FE',X'11',C' addressID="CAN">'                                 
         DC    X'FE',X'1B',C'<Name xml:lang="en"></Name>'                       
         DC    X'FE',X'26',C'<PostalAddress name="XLR8 MEDIA INC.">'            
         DC    X'FE',X'24',C'<Street>400-3575 boul. Saint-Laurent'              
         DC    X'FE',X'09',C'</Street>'                                         
         DC    X'FE',X'15',C'<City>Montreal</City>'                             
         DC    X'FE',X'20',C'<PostalCode>H2X 2T7</PostalCode>'                  
         DC    X'FE',X'1F',C'<Country isoCountryCode="CA" />'                   
         DC    X'FE',X'10',C'</PostalAddress>'                                  
         DC    X'FE',X'0A',C'</Contact>'                                        
         DC    X'FE',X'1F',C'<IdReference domain="taxPrefix"'                   
         DC    X'FE',X'11',C' identifier="" />'                                 
         DC    X'FE',X'1F',C'<IdReference domain="taxNumber"'                   
         DC    X'FE',X'11',C' identifier="" />'                                 
         DC    X'FE',X'11',C'</InvoicePartner>'                                 
*                                                                               
         DC    X'FE',X'17',C'<InvoiceDetailShipping>'                           
         DC    X'FE',X'29',C'<Contact role="shipFrom" addressID="CAN">'         
         DC    X'FE',X'28',C'<Name xml:lang="en-US">Me, Myself, and I'          
         DC    X'FE',X'07',C'</Name>'                                           
         DC    X'FE',X'0F',C'<PostalAddress>'                                   
         DC    X'FE',X'26',C'<DeliverTo>XLR8 MEDIA INC.</DeliverTo>'            
         DC    X'FE',X'24',C'<Street>400-3575 boul. Saint-Laurent'              
         DC    X'FE',X'09',C'</Street>'                                         
         DC    X'FE',X'15',C'<City>Montreal</City>'                             
         DC    X'FE',X'20',C'<PostalCode>H2X 2T7</PostalCode>'                  
         DC    X'FE',X'1F',C'<Country isoCountryCode="CA" />'                   
         DC    X'FE',X'10',C'</PostalAddress>'                                  
         DC    X'FE',X'0A',C'</Contact>'                                        
*                                                                               
         DC    X'FE',X'27',C'<Contact role="shipTo" addressID="CAN">'           
         DC    X'FE',X'2A',C'<Name xml:lang="en-US">Contact Name'               
         DC                C'</Name>'                                           
         DC    X'FE',X'0F',C'<PostalAddress>'                                   
         DC    X'03',X'0B',C'<DeliverTo>'                                       
         DC    X'FE',X'0C',C'</DeliverTo>'                                      
         DC    X'04',X'08',C'<Street>'                                          
         DC    X'FE',X'09',C'</Street>'                                         
         DC    X'05',X'06',C'<City>'                                            
         DC    X'FE',X'07',C'</City>'                                           
         DC    X'06',X'0C',C'<PostalCode>'                                      
         DC    X'FE',X'0D',C'</PostalCode>'                                     
         DC    X'FE',X'1F',C'<Country isoCountryCode="CA" />'                   
         DC    X'FE',X'10',C'</PostalAddress>'                                  
         DC    X'FE',X'0A',C'</Contact>'                                        
         DC    X'FE',X'18',C'</InvoiceDetailShipping>'                          
         DC    X'FE',X'26',C'<PaymentTerm payInNumberOfDays="45" />'            
         DC    X'FE',X'1D',C'<Comments xml:lang="en-US" />'                     
***      DC    X'FE',X'21',C'<!-- Extrinsic fields go here -->'                 
***      DC    X'FE',X'21',C'<Extrinsic name="CustomFields" />'                 
         DC    X'FE',X'1F',C'<Extrinsic name="ExchangeRate">'                   
         DC    X'FE',X'10',C'1.00</Extrinsic>'                                  
         DC    X'FE',X'1D',C'</InvoiceDetailRequestHeader>'                     
         DC    X'FF'                                                            
*                                                                               
OUINVTB2 DC    X'01',X'14',C'<InvoiceDetailOrder>'                              
         DC    X'02',X'18',C'<InvoiceDetailOrderInfo>'                          
         DC    X'03',X'10',C'<OrderReference>'                                  
         DC    X'04',X'1E',C'<DocumentReference payloadID="'                    
         DC    X'05',X'04',C'" />'                                              
         DC    X'06',X'11',C'</OrderReference>'                                 
         DC    X'07',X'19',C'</InvoiceDetailOrderInfo>'                         
OUINVTBL DC    X'08',X'3C',C'<InvoiceDetailServiceItem invoiceLineNumb'         
         DC                C'er="'                                              
OUINTB2A DC                C'1'                                                 
         DC                C'" quantity="">'                                    
         DC    X'FE',X'34',C'<InvoiceDetailServiceItemReference '               
         DC                C'lineNumber="'                                      
OUINTB2B DC                C'1'                                                 
         DC                C'" />'                                              
         DC    X'FE',X'10',C'<SubtotalAmount>'                                  
         DC    X'10',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'11',C'</SubtotalAmount>'                                 
         DC    X'FE',X'05',C'<Tax>'                                             
         DC    X'FF'                                                            
***                                                                             
* GST TAXES                                                                     
***                                                                             
OUFTABGC DC    X'01',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'2F',C'<Description xml:lang="en-GB">GST'                 
         DC                C'</Description>'                                    
         DC    X'FE',X'3F',C'<TaxDetail purpose="TAX" category="GST"'           
         DC                C' percentageRate="5.000">'                          
         DC    X'FE',X'0F',C'<TaxableAmount>'                                   
         DC    X'02',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'10',C'</TaxableAmount>'                                  
         DC    X'FE',X'0B',C'<TaxAmount>'                                       
         DC    X'01',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'0C',C'</TaxAmount>'                                      
         DC    X'FE',X'0C',C'</TaxDetail>'                                      
         DC    X'FF'                                                            
***                                                                             
* PST TAXES                                                                     
***                                                                             
OUFTABPC DC    X'01',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'02',X'1E',C'<Description xml:lang="en-GB">'                    
         DC    X'FE',X'0E',C'</Description>'                                    
         DC    X'02',X'23',C'<TaxDetail purpose="TAX" category="'               
         DC    X'03',X'12',C'" percentageRate="'                                
         DC    X'FE',X'02',C'">'                                                
         DC    X'FE',X'0F',C'<TaxableAmount>'                                   
         DC    X'04',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'10',C'</TaxableAmount>'                                  
         DC    X'FE',X'0B',C'<TaxAmount>'                                       
         DC    X'01',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'0C',C'</TaxAmount>'                                      
         DC    X'FE',X'0C',C'</TaxDetail>'                                      
         DC    X'FF'                                                            
*                                                                               
OUINVTB3 DC    X'FE',X'06',C'</Tax>'                                            
         DC    X'FE',X'1B',C'</InvoiceDetailServiceItem>'                       
         DC    X'FF'                                                            
*                                                                               
OUFTABC  DC    X'FE',X'15',C'</InvoiceDetailOrder>'                             
         DC    X'FE',X'16',C'<InvoiceDetailSummary>'                            
         DC    X'FE',X'10',C'<SubtotalAmount>'                                  
         DC    X'01',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'11',C'</SubtotalAmount>'                                 
         DC    X'FE',X'05',C'<Tax>'                                             
         DC    X'02',X'18',C'<Money alternateAmount="'                          
         DC    X'02',X'29',C'" alternateCurrency="CAD" currency="CAD">'         
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'20',C'<Description xml:lang="en-US" />'                  
         DC    X'FF'                                                            
***                                                                             
* TOTAL GST TAXES                                                               
***                                                                             
OUFTABGF DC    X'FE',X'68',C'<TaxDetail purpose="TAX" category="GST"'           
         DC                C' percentageRate="5.000" taxPointDate="'            
OUHDSTM5 DC                C'2017-06-11T'                                       
OUHTSTM5 DC                C'11:45:51-07:00'                                    
         DC                C'">'                                                
         DC    X'FE',X'0F',C'<TaxableAmount>'                                   
         DC    X'02',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'10',C'</TaxableAmount>'                                  
         DC    X'FE',X'0B',C'<TaxAmount>'                                       
         DC    X'01',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'0C',C'</TaxAmount>'                                      
         DC    X'FE',X'2C',C'<TaxLocation xml:lang="en">CAD'                    
         DC                C'</TaxLocation>'                                    
         DC    X'FE',X'0C',C'</TaxDetail>'                                      
         DC    X'FF'                                                            
***                                                                             
* TOTAL PST TAXES                                                               
***                                                                             
OUFTABPF DC    X'01',X'23',C'<TaxDetail purpose="TAX" category="'               
         DC    X'02',X'12',C'" percentageRate="'                                
         DC    X'FE',X'2B',C'" taxPointDate="'                                  
OUHDSTM6 DC                C'2017-06-11T'                                       
OUHTSTM6 DC                C'11:45:51-07:00'                                    
         DC                C'">'                                                
         DC    X'FE',X'0F',C'<TaxableAmount>'                                   
         DC    X'04',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'10',C'</TaxableAmount>'                                  
         DC    X'FE',X'0B',C'<TaxAmount>'                                       
         DC    X'03',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'0C',C'</TaxAmount>'                                      
         DC    X'FE',X'2C',C'<TaxLocation xml:lang="en">CAD'                    
         DC                C'</TaxLocation>'                                    
         DC    X'FE',X'0C',C'</TaxDetail>'                                      
         DC    X'FF'                                                            
*                                                                               
OUFTABCF DC    X'FE',X'06',C'</Tax>'                                            
         DC    X'FE',X'17',C'<SpecialHandlingAmount>'                           
         DC    X'FE',X'1F',C'<Money currency="CAD">0</Money>'                   
         DC    X'FE',X'18',C'</SpecialHandlingAmount>'                          
         DC    X'FE',X'10',C'<ShippingAmount>'                                  
         DC    X'FE',X'1F',C'<Money currency="CAD">0</Money>'                   
         DC    X'FE',X'11',C'</ShippingAmount>'                                 
         DC    X'FE',X'17',C'<InvoiceDetailDiscount>'                           
         DC    X'FE',X'1F',C'<Money currency="CAD">0</Money>'                   
         DC    X'FE',X'18',C'</InvoiceDetailDiscount>'                          
         DC    X'FE',X'0B',C'<NetAmount>'                                       
         DC    X'01',X'16',C'<Money currency="CAD">'                            
         DC    X'FE',X'08',C'</Money>'                                          
         DC    X'FE',X'0C',C'</NetAmount>'                                      
*                                                                               
         DC    X'20',X'17',C'</InvoiceDetailSummary>'                           
         DC    X'21',X'17',C'</InvoiceDetailRequest>'                           
         DC    X'22',X'0A',C'</Request>'                                        
         DC    X'23',X'07',C'</cXML>'                                           
         DC    X'FF'                                                            
***                                                                             
* PHD CXML INVOICE HEADER SPECIAL FIELDS TABLE                                  
* THIS MAPS AGAINST THE FIRST FIELD OF OUHTAB AND IF IT MATCHES                 
* WE INDEX INTO PHDSFLDD (BUILT IN PHDSFLDB) USING THE DISPLACEMENT             
* IN THE SECOND FIELD FOR A PRE-DEFINED LENGTH (3RD FIELD)                      
* WE DO THIS IN ORDER TO HANDLE DATA THAT IS SOFT & VARIABLE IN LENGTH          
*                                                                               
* FORMAT IS XL1 - FIELD NUMBER (MATCHED AGAINST OUHTAB)                         
*           AL1 - DISPLACEMENT INTO PHDSFLDD IN PHDSFLDB                        
*           AL1 - MAX LENGTH OF SOFT DATA                                       
***                                                                             
OUHSOFT2 DC    X'01',AL2(PHDPUPSE-PHDSFLDD),AL1(L'PHDPUPSE)                     
         DC    X'02',AL2(PHDINVNO-PHDSFLDD),AL1(L'PHDINVNO)                     
         DC    X'03',AL2(PHDPADD1-PHDSFLDD),AL1(L'PHDPADD1)                     
         DC    X'04',AL2(PHDPADD2-PHDSFLDD),AL1(L'PHDPADD2)                     
         DC    X'05',AL2(PHDPADD3-PHDSFLDD),AL1(L'PHDPADD3)                     
         DC    X'06',AL2(PHDPADD4-PHDSFLDD),AL1(L'PHDPADD4)                     
         DC    X'FF'                                                            
***                                                                             
* PHD CXML INVOICE DETAIL (VENDOR LEVEL) SPECIAL FIELDS TABLE                   
* THIS MAPS AGAINST THE FIRST FIELD OF OUINVTB2 AND IF IT MATCHES               
* WE INDEX INTO PHDSFLDD (BUILT IN PHDSFLDB) USING THE DISPLACEMENT             
* IN THE SECOND FIELD FOR A PRE-DEFINED LENGTH (3RD FIELD)                      
* WE DO THIS IN ORDER TO HANDLE DATA THAT IS SOFT & VARIABLE IN LENGTH          
*                                                                               
* FORMAT IS XL1 - FIELD NUMBER (MATCHED AGAINST OUINVTB2)                       
*           AL1 - DISPLACEMENT INTO PHDSFLDD IN PHDSFLDB                        
*           AL1 - MAX LENGTH OF SOFT DATA                                       
***                                                                             
OUISOFTC DC    X'04',AL2(PHDEUDF1-PHDSFLDD),AL1(L'PHDEUDF1)                     
         DC    X'10',AL2(PHDINVAM-PHDSFLDD),AL1(L'PHDINVAM)                     
         DC    X'FF'                                                            
***                                                                             
* PHD CXML INVOICE FOOTER SPECIAL FIELDS TABLE                                  
* THIS MAPS AGAINST THE FIRST FIELD OF OUFTABC AND IF IT MATCHES                
* WE INDEX INTO PHDSFLDD (BUILT IN PHDSFLDB) USING THE DISPLACEMENT             
* IN THE SECOND FIELD FOR A PRE-DEFINED LENGTH (3RD FIELD)                      
* WE DO THIS IN ORDER TO HANDLE DATA THAT IS SOFT & VARIABLE IN LENGTH          
*                                                                               
* FORMAT IS XL1 - FIELD NUMBER (MATCHED AGAINST OUFTABC)                        
*           AL1 - DISPLACEMENT INTO PHDSFLDD IN PHDSFLDB                        
*           AL1 - MAX LENGTH OF SOFT DATA                                       
***                                                                             
OUFSFT   DC    X'02',AL2(PHDINVAM-PHDSFLDD),AL1(L'PHDINVAM)                     
         DC    X'06',AL2(PHDINVTX-PHDSFLDD),AL1(L'PHDINVTX)                     
         DC    X'FF'                                                            
*                                                                               
OUFSFTG  DC    X'01',AL2(PHDIGST-PHDSFLDD),AL1(L'PHDIGST)                       
         DC    X'02',AL2(PHDINVAM-PHDSFLDD),AL1(L'PHDINVAM)                     
         DC    X'FF'                                                            
*                                                                               
OUFSFTP  DC    X'01',AL2(PHDIPST-PHDSFLDD),AL1(L'PHDIPST)                       
         DC    X'02',AL2(PHDTAXCD-PHDSFLDD),AL1(L'PHDTAXCD)                     
         DC    X'03',AL2(PHDTAXRT-PHDSFLDD),AL1(L'PHDTAXRT)                     
         DC    X'04',AL2(PHDINVAM-PHDSFLDD),AL1(L'PHDINVAM)                     
         DC    X'FF'                                                            
*                                                                               
OUFSFTC2 DC    X'01',AL2(PHDINVAC-PHDSFLDD),AL1(L'PHDINVAC)                     
         DC    X'02',AL2(PHDINVTX-PHDSFLDD),AL1(L'PHDINVTX)                     
         DC    X'FF'                                                            
*                                                                               
OUFSFTGT DC    X'01',AL2(PHDIGST-PHDSFLDD),AL1(L'PHDIGST)                       
         DC    X'02',AL2(PHDINVAC-PHDSFLDD),AL1(L'PHDINVAC)                     
         DC    X'FF'                                                            
*                                                                               
OUFSFTPT DC    X'01',AL2(PHDTAXCD-PHDSFLDD),AL1(L'PHDTAXCD)                     
         DC    X'02',AL2(PHDTAXRT-PHDSFLDD),AL1(L'PHDTAXRT)                     
         DC    X'03',AL2(PHDIPST-PHDSFLDD),AL1(L'PHDIPST)                       
         DC    X'04',AL2(PHDINVAC-PHDSFLDD),AL1(L'PHDINVAC)                     
         DC    X'FF'                                                            
*                                                                               
OUFSFTC3 DC    X'01',AL2(PHDINTOT-PHDSFLDD),AL1(L'PHDINTOT)                     
         DC    X'FF'                                                            
*                                                                               
         TITLE 'READUCOM - READ THE UCOMM RECORD'                               
READUCOM CSECT                                                                  
         NMOD1 0,READUCOM                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
         LA    R3,UCOMBLK          UCOMM CONTROL BLOCK                          
         USING DDUCOMD,R3          UCOMM DSECT                                  
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM CONTROL BLOCK                    
         MVC   UCACOMF,ACOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'S'          SYSTEM                                       
*                                                                               
         L     RF,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDR,RF           ESTIMATE RECORD DSECT                        
         MVC   UCSAM,EKEYAM        AGENCY/MEDIA                                 
         MVC   UCSCLT,EKEYCLT      PACKED CLIENT                                
         MVC   UCPRD,EKEYPRD       PRD CODE                                     
         MVC   UCSEST,EKEYEST      ESTIMATE                                     
         DROP  RF                  DROP ESTIMATE RECORD USING                   
*                                                                               
         CLC   SVUCKEY,UCSAM       DID I JUST CHECK THIS?                       
         BE    RDU10               YES                                          
         MVC   SVUCKEY,UCSAM       SAVE KEY FOR OPTIMIZATION                    
         MVC   SVEUCOM1,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM2,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM3,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM4,SPACES     INIT IN CASE OF ERROR                        
*                                                                               
         OI    UCOPT,UCOEST        RETURN ESTIMATE UCOMMS                       
         GOTO1 =V(DDUCOM),UCOMBLK  GET ESTIMATE UCOMMS                          
*                                                                               
         CLI   UCERROR,0           ANY ERROR?                                   
         BNE   RDU10               YES                                          
         TM    UCDATA,UCDNOEST     FOUND EST LEVEL UCOMM REC?                   
         BO    RDU10               NO                                           
*                                                                               
         L     R1,UCEDATA          ESTIMATE UCOMM DATA                          
         LA    RF,UCELENS          ESTIMATE UCOMM LENGTHS                       
         LA    RE,SVEUCOM1         START OF FIRST SAVED ESTIMATE UCOMM          
         LHI   R0,4                4 ESTIMATE UCOMMS                            
*                                                                               
RDU05    CLI   0(RF),0             IS ESTIMATE UCOMM LENGTH ZERO?               
         BE    *+10                YES - DON'T HAVE THIS ESTIMATE UCOMM         
         MVC   0(32,RE),0(R1)      MOVE IN ESTIMATE UCOMM                       
         OC    0(32,RE),SPACES     SPACE PAD                                    
         LA    RE,32(RE)           BUMP TO NEXT SAVED ESTIMATE UCOMM            
         LA    R1,32(R1)           BUMP TO NEXT ESTIMATE UCOMM                  
         LA    RF,1(RF)            BUMP TO NEXT ESTIMATE UCOMM LENGTH           
         BCT   R0,RDU05            PROCESS NEXT ESTIMATE UCOMM                  
*                                                                               
RDU10    MVC   KEY,SAVEKEY         RESTORE KEY                                  
         GOTO1 HIGH                YES - RESTORE THE READ SEQUENCE              
*                                                                               
RDUXIT   XIT1                      EXIT                                         
         DROP  R3                  DROP UCOMM CONTROL BLOCK USING               
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'PRDUCOMM - READ THE PRODUCT UCOMMS'                             
PRDUCOMM CSECT                                                                  
         NMOD1 0,PRDUCOMM                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LA    R3,UCOMBLK          UCOMM CONTROL BLOCK                          
         USING DDUCOMD,R3          UCOMM DSECT                                  
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM CONTROL BLOCK                    
         MVC   UCACOMF,ACOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'S'          SPOT SYSTEM                                  
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   *+8                 NO                                           
         MVI   UCSYS,C'N'          NET SYSTEM                                   
*                                                                               
         L     RF,ADPRD            A(PRODUCT RECORD)                            
         USING PRDHDR,RF           PRODUCT RECORD DSECT                         
         MVC   UCSAM,PKEYAM        AGENCY/MEDIA                                 
         MVC   UCSCLT,PKEYCLT      PACKED CLIENT                                
         MVC   UCPRD,PKEYPRD       PRD CODE                                     
         DROP  RF                  DROP PRODUCT RECORD USING                    
*                                                                               
         CLC   SVUCPKEY,UCSAM      DID I JUST CHECK THIS?                       
         BE    PUXIT               YES                                          
         MVC   SVUCPKEY,UCSAM      SAVE KEY FOR OPTIMIZATION                    
         MVC   SVPUCOM1,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVPUCOM2,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVPUCOM3,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVPUCOM4,SPACES     INIT IN CASE OF ERROR                        
*                                                                               
         OI    UCOPT,UCOPRD        RETURN PRODUCT UCOMMS                        
         GOTO1 =V(DDUCOM),UCOMBLK  GET PRODUCT UCOMMS                           
*                                                                               
         CLI   UCERROR,0           ANY ERROR?                                   
         BNE   PUXIT               YES                                          
         TM    UCDATA,UCDNOPRD     FOUND PRD LEVEL UCOMM REC?                   
         BO    PUXIT               NO                                           
*                                                                               
         L     R1,UCPDATA          PRODUCT UCOMM DATA                           
         LA    RF,UCPLENS          PRODUCT UCOMM LENGTHS                        
         LA    RE,SVPUCOM1         START OF FIRST SAVED PRODUCT UCOMM           
         LHI   R0,4                4 PRODUCT UCOMMS                             
*                                                                               
PU05     CLI   0(RF),0             IS PRODUCT UCOMM LENGTH ZERO?                
         BE    *+10                YES - DON'T HAVE THIS PRODUCT UCOMM          
         MVC   0(32,RE),0(R1)      MOVE IN PRODUCT UCOMM                        
***                                                                             
* DO NOT SPACE PAD THIS DATA                                                    
* AS PER SPEC-48613, IF SPECIAL CHARACTERS SUCH AS A TILDA ARE USED             
* A X'A1' BECOMES A X'E1'                                                       
***                                                                             
***      OC    0(32,RE),SPACES     SPACE PAD                                    
         LA    RE,32(RE)           BUMP TO NEXT SAVED PRODUCT UCOMM             
         LA    R1,32(R1)           BUMP TO NEXT PRODUCT UCOMM                   
         LA    RF,1(RF)            BUMP TO NEXT PRODUCT UCOMM LENGTH            
         BCT   R0,PU05             PROCESS NEXT PRODUCT UCOMM                   
*                                                                               
PUXIT    XIT1                      EXIT                                         
         DROP  R3                  DROP UCOMM CONTROL BLOCK USING               
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'ESTUCOMM - READ THE ESTIMATE UCOMMS'                            
ESTUCOMM CSECT                                                                  
         NMOD1 0,ESTUCOMM                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LA    R3,UCOMBLK          UCOMM CONTROL BLOCK                          
         USING DDUCOMD,R3          UCOMM DSECT                                  
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM CONTROL BLOCK                    
         MVC   UCACOMF,ACOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'S'          SYSTEM                                       
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   *+8                 NO                                           
         MVI   UCSYS,C'N'          NET SYSTEM                                   
*                                                                               
         L     RF,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDR,RF           ESTIMATE RECORD DSECT                        
         MVC   UCSAM,EKEYAM        AGENCY/MEDIA                                 
         MVC   UCSCLT,EKEYCLT      PACKED CLIENT                                
         MVC   UCPRD,EKEYPRD       PRD CODE                                     
         MVC   UCSEST,EKEYEST      ESTIMATE                                     
         DROP  RF                  DROP ESTIMATE RECORD USING                   
*                                                                               
         CLC   SVUCKEY,UCSAM       DID I JUST CHECK THIS?                       
         BE    EUXIT               YES                                          
         MVC   SVUCKEY,UCSAM       SAVE KEY FOR OPTIMIZATION                    
         MVC   SVEUCOM1,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM2,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM3,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM4,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM5,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM6,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM7,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM8,SPACES     INIT IN CASE OF ERROR                        
*                                                                               
         OI    UCOPT,UCO8EST       RETURN 8 ESTIMATE UCOMMS                     
         GOTO1 =V(DDUCOM),UCOMBLK  GET ESTIMATE UCOMMS                          
*                                                                               
         CLI   UCERROR,0           ANY ERROR?                                   
         BNE   EUXIT               YES                                          
         TM    UCDATA,UCDNOEST     FOUND EST LEVEL UCOMM REC?                   
         BO    EUXIT               NO                                           
*                                                                               
         L     R1,UCEDATA          ESTIMATE UCOMM DATA                          
         LA    RF,UCELENS          ESTIMATE UCOMM LENGTHS                       
         LA    RE,SVEUCOM1         START OF FIRST SAVED ESTIMATE UCOMM          
         LHI   R0,4                4 ESTIMATE UCOMMS                            
*                                                                               
EU05     CLI   0(RF),0             IS ESTIMATE UCOMM LENGTH ZERO?               
         BE    *+10                YES - DON'T HAVE THIS ESTIMATE UCOMM         
         MVC   0(32,RE),0(R1)      MOVE IN ESTIMATE UCOMM                       
***                                                                             
* DO NOT SPACE PAD THIS DATA                                                    
* AS PER SPEC-48613, IF SPECIAL CHARACTERS SUCH AS A TILDA ARE USED             
* A X'A1' BECOMES A X'E1'                                                       
***                                                                             
***      OC    0(32,RE),SPACES     SPACE PAD                                    
         LA    RE,32(RE)           BUMP TO NEXT SAVED ESTIMATE UCOMM            
         LA    R1,32(R1)           BUMP TO NEXT ESTIMATE UCOMM                  
         LA    RF,1(RF)            BUMP TO NEXT ESTIMATE UCOMM LENGTH           
         BCT   R0,EU05             PROCESS NEXT ESTIMATE UCOMM                  
***                                                                             
* ESTIMATE UCOMMS 5-8 ARE IN UCPDATA                                            
***                                                                             
         L     R1,UCPDATA          ESTIMATE UCOMM DATA                          
         LA    RF,UCPLENS          ESTIMATE UCOMM LENGTHS                       
         LA    RE,SVEUCOM5         START OF FIRST SAVED ESTIMATE UCOMM          
         LHI   R0,4                4 ESTIMATE UCOMMS (5-8)                      
*                                                                               
EU10     CLI   0(RF),0             IS ESTIMATE UCOMM LENGTH ZERO?               
         BE    *+10                YES - DON'T HAVE THIS ESTIMATE UCOMM         
         MVC   0(32,RE),0(R1)      MOVE IN ESTIMATE UCOMM                       
***                                                                             
* DO NOT SPACE PAD THIS DATA                                                    
* AS PER SPEC-48613, IF SPECIAL CHARACTERS SUCH AS A TILDA ARE USED             
* A X'A1' BECOMES A X'E1'                                                       
***                                                                             
***      OC    0(32,RE),SPACES     SPACE PAD                                    
         LA    RE,32(RE)           BUMP TO NEXT SAVED ESTIMATE UCOMM            
         LA    R1,32(R1)           BUMP TO NEXT ESTIMATE UCOMM                  
         LA    RF,1(RF)            BUMP TO NEXT ESTIMATE UCOMM LENGTH           
         BCT   R0,EU10             PROCESS NEXT ESTIMATE UCOMM                  
*                                                                               
EUXIT    XIT1                      EXIT                                         
         DROP  R3                  DROP UCOMM CONTROL BLOCK USING               
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'MKTUCOMM - READ THE MARKET UCOMMS'                              
MKTUCOMM CSECT                                                                  
         NMOD1 0,MKTUCOMM                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LA    R3,UCOMBLK          UCOMM CONTROL BLOCK                          
         USING DDUCOMD,R3          UCOMM DSECT                                  
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM CONTROL BLOCK                    
         MVC   UCACOMF,ACOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'S'          SYSTEM                                       
*                                                                               
         L     RF,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDR,RF           ESTIMATE RECORD DSECT                        
         MVC   UCSAM,EKEYAM        AGENCY/MEDIA                                 
         MVC   UCSCLT,EKEYCLT      PACKED CLIENT                                
         MVC   UCPRD,EKEYPRD       PRD CODE                                     
         MVC   UCSEST,EKEYEST      ESTIMATE                                     
         DROP  RF                  DROP ESTIMATE RECORD USING                   
*                                                                               
         ICM   RF,15,4(R1)         A(BINARY MARKET)                             
         MVC   UCMKT,0(RF)         MARKET                                       
*                                                                               
         CLC   SVUCMKEY,UCSAM      DID I JUST CHECK THIS?                       
         BNE   *+14                NO                                           
         CLC   SVUCMKT,UCMKT       DID I JUST CHECK THIS?                       
         BE    MUXIT               YES                                          
         MVC   SVUCMKEY,UCSAM      SAVE KEY FOR OPTIMIZATION                    
         MVC   SVUCMKT,UCMKT       SAVE MARKET OPTIMIZATION                     
         MVC   SVMUCOM1,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVMUCOM2,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVMUCOM3,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVMUCOM4,SPACES     INIT IN CASE OF ERROR                        
*                                                                               
         OI    UCOPT,UCOMKT        RETURN MARKET UCOMMS                         
         GOTO1 =V(DDUCOM),UCOMBLK  GET MARKET UCOMMS                            
*                                                                               
         CLI   UCERROR,0           ANY ERROR?                                   
         BNE   MUXIT               YES                                          
         TM    UCDATA,UCDNOMKT     FOUND MKT LEVEL UCOMM REC?                   
         BO    MUXIT               NO                                           
*                                                                               
         L     R1,UCMDATA          MARKET UCOMM DATA                            
         LA    RF,UCMLENS          MARKET UCOMM LENGTHS                         
         LA    RE,SVMUCOM1         START OF FIRST SAVED MARKET UCOMM            
         LHI   R0,4                4 MARKET UCOMMS                              
*                                                                               
MU05     CLI   0(RF),0             IS ESTIMATE UCOMM LENGTH ZERO?               
         BE    *+10                YES - DON'T HAVE THIS ESTIMATE UCOMM         
         MVC   0(32,RE),0(R1)      MOVE IN ESTIMATE UCOMM                       
***                                                                             
* DO NOT SPACE PAD THIS DATA                                                    
* AS PER SPEC-48613, IF SPECIAL CHARACTERS SUCH AS A TILDA ARE USED             
* A X'A1' BECOMES A X'E1'                                                       
***                                                                             
***      OC    0(32,RE),SPACES     SPACE PAD                                    
         LA    RE,32(RE)           BUMP TO NEXT SAVED ESTIMATE UCOMM            
         LA    R1,32(R1)           BUMP TO NEXT ESTIMATE UCOMM                  
         LA    RF,1(RF)            BUMP TO NEXT ESTIMATE UCOMM LENGTH           
         BCT   R0,MU05             PROCESS NEXT ESTIMATE UCOMM                  
*                                                                               
MUXIT    XIT1                      EXIT                                         
         DROP  R3                  DROP UCOMM CONTROL BLOCK USING               
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'READPGRP - READ THE PRODUCT GROUP RECORD'                       
READPGRP CSECT                                                                  
         NMOD1 0,READPGRP                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         L     RF,ADPRD            A(PRODUCT RECORD)                            
         USING PRDHDR,RF           PRODUCT RECORD DSECT                         
         CLC   SVPGRAM,PKEYAM      SAME AGENCY/MEDIA?                           
         BNE   *+10                NO                                           
         CLC   SVPGRCLT,PKEYCLT    SAME CLIENT CODE?                            
         BNE   *+10                NO                                           
         CLC   SVPGRPRD,PKEYPRD    SAME PRODUCT?                                
         BE    RDPXIT              YES                                          
*                                                                               
         XC    SVPGRP,SVPGRP       CLEAR SAVED PRODUCT GROUP                    
         XC    SVPGRPP,SVPGRPP     CLEAR SAVED PRODUCT GROUP (PACKED)           
         XC    SVPGRPN,SVPGRPN     CLEAR SAVED PRODUCT GROUP NAME               
         XC    SVPGRPN2,SVPGRPN2   CLEAR SAVED PRODUCT GROUP NAME 2             
         MVC   SVPGRAM,PKEYAM      SAVE AGENCY/MEDIA                            
         MVC   SVPGRCLT,PKEYCLT    SAVE CLIENT CODE                             
         MVC   SVPGRPRD,PKEYPRD    SAVE PRODUCT                                 
         DROP  RF                  DROP PRODUCT RECORD USING                    
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R6,KEY              R6 = KEY                                     
         USING PRGKEY,R6           PRODUCT GROUP DSECT                          
         MVC   PRGPTYP,=X'0D81'    PRODUCT GROUP PASSIVE                        
         MVC   PRGPAGMD,SVPGRAM    AGENCY/MEDIA                                 
         MVC   PRGPCLT,SVPGRCLT    CLIENT                                       
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     RDP20               GO TEST KEY                                  
*                                                                               
RDP10    GOTO1 SEQ                 READ SEQ                                     
*                                                                               
RDP20    CLC   PRGPTYP(5),KEYSAVE  SAME TYPE/AGENCY/MEDIA/CLIENT?               
         BNE   RDP50               NO - DONE                                    
         CLC   PRGPPRD,SVPGRPRD    MATCH ON PRODUCT?                            
         BNE   RDP10               NO - READ SEQ                                
*                                                                               
         MVC   PRGKTYP,=X'0D01'    NOW READ THE ACTIVE KEY                      
         XC    PRGPPRD,PRGPPRD     NO PRD IN THE ACTIVE KEY                     
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   PRGKEY,KEYSAVE      DID WE FIND EXACT KEY?                       
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING IS WRONG                      
*                                                                               
         MVC   SVPGRP(1),PRGKID    PRODUCT GROUP ID                             
         MVC   SVPGRP+1(3),PRGKGRP PRODUCT GROUP NUMBER (PWOS)                  
         MVC   SVPGRPP,PRGKID      PRODUCT GROUP ID & PACKED NUMBER             
*                                                                               
         GOTO1 GETPRDGR            GET THE PRODUCT GROUP RECORD                 
*                                                                               
         XC    PRGKGRP,PRGKGRP     PGRDEF KEY                                   
*                                                                               
         L     R6,ADPRDGRP         A(PRODUCT GROUP RECORD)                      
         MVI   ELCODE2,X'10'       GET THE PRDGRP BREAK NAMES                   
         BAS   RE,GETEL2           HAVE PRDGRP BREAK NAMES ELEMENT?             
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING IS WRONG                      
*                                                                               
         USING PRGEL10,R6          PRDGRP BREAK NAMES DSECT                     
         MVC   SVPGRPN,PRGNAM1     CLEAR SAVED PRODUCT GROUP NAME               
         OC    SVPGRPN,SPACES      SPACE PAD                                    
         MVC   SVPGRPN2,PRGNAM2    CLEAR SAVED PRODUCT GROUP NAME 2             
         OC    SVPGRPN2,SPACES     SPACE PAD                                    
         DROP  R6                  DROP PRDGRP BREAK NAMES USING                
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   KEY(13),KEYSAVE     FOUND PGRDEF KEY?                            
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING IS WRONG                      
*                                                                               
         GOTO1 GETPRDGR            GET THE PRODUCT GROUP DEF RECORD             
*                                                                               
         L     R6,ADPRDGRP         A(PRODUCT GROUP DEF RECORD)                  
         MVI   ELCODE2,X'01'       PRDGRP BREAK DESCRIPTION                     
         BAS   RE,GETEL2           HAVE PRDGRP BREAK DESCRIPTION ELEM?          
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING IS WRONG                      
*                                                                               
         USING PRGEL01,R6          PRDGRP BREAK DESCRIPTION DSECT               
*                                                                               
         LLC   R0,PRGBK1LN         BREAK LENGTH 1                               
         LLC   RE,PRGBK2LN         BREAK LENGTH 2                               
         AR    RE,R0               RE = TOTAL BREAK LENGTH                      
         BCTR  RE,0                - 1 FOR EX                                   
         DROP  R6                  PRDGRP BREAK DESCRIPTION USING               
*                                                                               
         UNPK  DUB,SVPGRP+1(3)     UNPACK GROUP NUMBER                          
         EX    RE,*+8              EXECUTE THE MVC                              
         B     *+10                FOR IDF                                      
         MVC   SVPGRP+1(0),DUB+3   ** EXECUTED **                               
         OC    SVPGRP,SPACES       SPACE PAD                                    
*                                                                               
RDP50    MVC   KEY,SAVEKEY         RESTORE KEY                                  
         GOTO1 HIGH                YES - RESTORE THE READ SEQUENCE              
*                                                                               
RDPXIT   XIT1                      EXIT                                         
*                                                                               
         GETELN R6,24,ELCODE2,2    GETEL2 MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GETMKNAM - READ THE MARKET RECORD'                              
GETMKNAM CSECT                                                                  
         NMOD1 0,GETMKNAM                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         L     R6,ADSTABUC         A(STATION BUCKET RECORD)                     
         USING STABUCKD,R6         STATION BUCKET RECORD DSECT                  
         CLC   SVMKTAM,STABKAM     SAME AGENCY/MEDIA?                           
         BNE   *+10                NO                                           
         CLC   SVMKTMKT,STABKMKT   SAME MARKET?                                 
         BE    RDMKXIT             YES - ALREADY HAVE MKT REC IN CORE           
*                                                                               
         MVC   SVMKTNM,SPACES      INIT SAVED MARKET NAME                       
         MVC   SVMKTAM,STABKAM     SAVE AGENCY/MEDIA                            
         MVC   SVMKTMKT,STABKMKT   SAVE MARKET                                  
         DROP  R6                  DROP STATION BUCKET RECORD USING             
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R6,KEY              R5 = KEY                                     
         USING MKTRECD,R6          MARKET RECORD DSECT                          
         MVI   MKTKEY,C'0'         INIT TO ZEROES                               
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,MKTKTYPQ   C'M'                                         
*                                                                               
         L     R2,ADAGY            A(AGENCY RECORD)                             
         LA    R2,24(R2)           A(FIRST ELEMENT)                             
*                                                                               
RDMK10   CLI   0(R2),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SOMETHING IS WRONG                     
*                                                                               
         CLI   0(R2),AGYMEDEQ      X'02' MEDIA CODE ELEMENT?                    
         BNE   RDMK20              NO - BUMP TO NEXT ELEMENT                    
*                                                                               
         USING AGYMEDEL,R2         MEDIA CODE ELEMENT DSECT                     
         CLC   AGYMEDBT,SVMKTAM    MATCH ON AGENCY MEDIA?                       
         BE    RDMK30              YES                                          
*                                                                               
RDMK20   LLC   RF,1(R2)            ELEMENT LENGTH                               
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         B     RDMK10              NO - GET NEXT ELEMENT                        
*                                                                               
RDMK30   MVC   MKTKMED,AGYMEDCD    MEDIA CODE                                   
         DROP  R2                  DROP MEDIA CODE ELEMENT USING                
*                                                                               
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,3,SVMKTMKT       BINARY MARKET                                
         CVD   RE,DUB              CVD                                          
         OI    DUB+7,X'0F'         MAKE SURE IT'S POSITIVE                      
         UNPK  MKTKMKT,DUB         UNPACKED MARKET                              
*                                                                               
         MVC   MKTKAGY,QAGY        AGENCY CODE                                  
*                                                                               
         GOTO1 HIGHMKT             READ HIGH FOR MARKET                         
*                                                                               
         CLC   MKTKEY,KEYSAVE      FOUND OUR KEY?                               
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING IS WRONG                      
*                                                                               
         L     R6,ADMARKET         A(MARKET RECORD)                             
         MVC   SVMKTNM,MKTNAME     MARKET NAME                                  
         OC    SVMKTNM,SPACES      SPACE PAD                                    
*                                                                               
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         GOTO1 HIGH                YES - RESTORE THE READ SEQUENCE              
*                                                                               
RDMKXIT  XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GETMGNAM - READ THE MARKET GROUP RECORD'                        
GETMGNAM CSECT                                                                  
         NMOD1 0,GETMGNAM                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         L     R6,ADSTABUC         A(STATION BUCKET RECORD)                     
         USING STABUCKD,R6         STATION BUCKET RECORD DSECT                  
         CLC   SVMGRAM,STABKAM     SAME AGENCY/MEDIA?                           
         BNE   *+10                NO                                           
         CLC   SVMGRCLT,STABKCLT   SAME CLIENT CODE?                            
         BNE   *+10                NO                                           
         CLC   SVMGRMKT,STABKMKT   SAME MARKET?                                 
         BE    RDMGXIT             YES                                          
*                                                                               
         MVC   SVMGRPN,SPACES      CLEAR SAVED MARKET GROUP NAME                
         MVC   SVMGRAM,STABKAM     SAVE AGENCY/MEDIA                            
         MVC   SVMGRCLT,STABKCLT   SAVE CLIENT CODE                             
         MVC   SVMGRMKT,STABKMKT   SAVE MARKET                                  
         DROP  R6                  DROP STATION BUCKET RECORD USING             
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R6,KEY              R6 = KEY                                     
         USING MKGRECD,R6          MARKET GROUP DSECT                           
         MVC   MKGPTYP,=X'0D82'    MARKET GROUP PASSIVE                         
         MVC   MKGPAGMD,SVMGRAM    AGENCY/MEDIA                                 
         MVC   MKGPCLT,SVMGRCLT    CLIENT                                       
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     RDMG20              GO TEST KEY                                  
*                                                                               
RDMG10   GOTO1 SEQ                 READ SEQ                                     
*                                                                               
RDMG20   CLC   MKGPTYP(5),KEYSAVE  SAME TYPE/AGENCY/MEDIA/CLIENT?               
         BNE   RDMG50              NO - DONE                                    
         OC    MKGPPID(3),MKGPPID  HAVE A PRODUCT GROUP?                        
         BNZ   RDMG10              YES - SKIP                                   
         CLC   MKGPMKT,SVMGRMKT    MATCH ON MARKET?                             
         BNE   RDMG10              NO - READ SEQ                                
*                                                                               
         MVC   MKGKTYP,=X'0D02'    NOW READ THE ACTIVE KEY                      
         XC    MKGPMKT,MKGPMKT     NO MKT IN THE ACTIVE KEY                     
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   MKGKEY,KEYSAVE      DID WE FIND EXACT KEY?                       
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING IS WRONG                      
*                                                                               
         GOTO1 GETMKTGR            GET THE MARKET GROUP RECORD                  
*                                                                               
         L     R6,ADMKTGRP         A(MARKET GROUP RECORD)                       
         LA    R6,24(R6)           A(FIRST ELEMENT)                             
*                                                                               
RDMG30   CLI   0(R6),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SOMETHING IS WRONG                     
*                                                                               
         CLI   0(R6),X'10'         X'10' MKTGRP BREAK NAMES ELEMENT?            
         BE    RDMG40              YES                                          
         LLC   RF,1(R6)            ELEMENT LENGTH                               
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     RDMG30              NO - GET NEXT ELEMENT                        
*                                                                               
         USING MKGEL10,R6          MKTGRP BREAK NAMES DSECT                     
RDMG40   MVC   SVMGRPN,MKGNAM1     CLEAR SAVED MARKET GROUP NAME                
         OC    SVMGRPN,SPACES      SPACE PAD                                    
         DROP  R6                  DROP MKTGRP BREAK NAMES USING                
*                                                                               
RDMG50   MVC   KEY,SAVEKEY         RESTORE KEY                                  
         GOTO1 HIGH                YES - RESTORE THE READ SEQUENCE              
*                                                                               
RDMGXIT  XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GETSNAME - GET STATION NAME'                                    
GETSNAME CSECT                                                                  
         NMOD1 0,GETSNAME                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
         USING SORTRECD,R4         SORT RECORD DSECT                            
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
         MVI   READSW,0            INIT READ SWITCH                             
         XC    VENDORNM,VENDORNM   INIT VENDOR NAME                             
*                                                                               
         CLI   NETOPT,C'N'         NETPAK?                                      
         BE    GETSN20             YES - STATION NAME IN MKT REC                
*                                                                               
         LA    R5,KEY              R5 = KEY                                     
         USING ADDRECD,R5          STATION ADDRESS RECORD DSECT                 
         MVI   ADDRKEY,C'0'        INIT TO ZEROES                               
         MVC   ADDRKEY+1(L'ADDRKEY-1),ADDRKEY                                   
         MVI   ADDKTYPE,ADDKTYPQ   C'A'                                         
         MVC   ADDKMED,QMED        MEDIA                                        
*                                                                               
         MVC   ADDKCALL(4),THISSTA STATION                                      
         MVC   ADDKCALL+4(1),THISSTA+5 BAND                                     
*                                                                               
         MVC   ADDKAGY,QAGY        AGENCY ALPHA                                 
*                                                                               
         L     R5,ADSTATAD         A(STATION ADDRESS RECORD)                    
         CLC   ADDRKEY,KEY         HAVE RECORD IN CORE?                         
         BE    GETSN10             YES                                          
*                                                                               
         GOTO1 HIGHSTAD            READHI FOR THE STATION ADDRESS REC           
*                                                                               
         MVI   READSW,1            FLAG THAT WE READ THE ADDRESS REC            
         CLC   ADDRKEY,KEYSAVE     FOUND OUR KEY?                               
         BE    GETSN10             YES                                          
*                                                                               
         MVC   P+1(26),=C'STATION ADDRESS RECORD FOR'                           
         MVC   P+28(5),KEYSAVE+2   STATION                                      
         MVC   P+34(11),=C'NOT ON FILE'                                         
         GOTO1 REPORT              PRINT THE ERROR                              
         B     GETSN50             RESTORE THE READ SEQUENCE                    
*                                                                               
GETSN10  MVC   VENDORNM(20),ANAME  STATION NAME                                 
         B     GETSN50             RESTORE THE READ SEQUENCE                    
         DROP  R5                  DROP STATION ADDRESS RECORD USING            
*                                                                               
GETSN20  LA    R5,KEY              R5 = KEY                                     
         USING MKTRECD,R5          MARKET RECORD DSECT                          
         MVI   MKTKEY,C'0'         INIT TO ZEROES                               
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,MKTKTYPQ   C'M'                                         
         MVI   MKTKMED,C'N'        ALWAYS MEDIA N FOR NET                       
*                                                                               
         L     R6,ANETBLK          A(NETBLOCK)                                  
         USING NETBLOCK,R6         NETBLOCK DSECT                               
         EDIT  (2,NBMARKET),(4,MKTKMKT),FILL=0,ZERO=NOBLANK                     
         MVC   MKTKAGY,QAGY        AGENCY                                       
*                                                                               
         L     R5,ADMARKET         A(MARKET RECORD)                             
         CLC   MKTKEY,KEY          HAVE RECORD IN CORE?                         
         BE    GETSN30             YES                                          
*                                                                               
         GOTO1 HIGHMKT             READ HIGH FOR MARKET                         
*                                                                               
         MVI   READSW,1            FLAG THAT WE READ THE MARKET REC             
         CLC   MKTKEY,KEYSAVE      FOUND OUR KEY?                               
         BE    GETSN30             YES                                          
*                                                                               
         MVC   P+1(13),=C'MARKET RECORD'                                        
         MVC   P+15(4),MKTKMKT                                                  
         MVC   P+21(11),=C'NOT ON FILE'                                         
         GOTO1 REPORT              PRINT THE ERROR                              
         B     GETSN50             RESTORE THE READ SEQUENCE                    
*                                                                               
GETSN30  MVC   VENDORNM,MKTNAME    MARKET NAME = STATION NAME FOR NET           
         DROP  R5,R6               DROP MKT REC & NETBLOCK USINGS               
*                                                                               
GETSN50  MVC   KEY,SAVEKEY         RESTORE KEY                                  
         CLI   READSW,1            DID I READ A RECORD?                         
         BNE   GETSNXIT            NO - DONE                                    
*                                                                               
         GOTO1 HIGH                YES - RESTORE THE READ SEQUENCE              
*                                                                               
GETSNXIT XIT1                      EXIT                                         
         DROP  R4                  DROP SORT RECORD USING                       
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'CKEST - READ ESTIMATE RECORD IF NOT ALREADY IN ADEST'           
CKEST    CSECT                                                                  
         NMOD1 0,CKEST                                                          
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
         MVC   SAVEREC,AREC        SAVE CURRENT RECORD ADDRESS                  
         MVI   CKESTSW,0           INIT TO NO ESTIMATE READ                     
         XC    KEY,KEY             CLEAR THE KEY                                
*                                                                               
         CLI   CKESTREC,C'K'       READ EST FROM A STATION BUCKET REC?          
         BE    CKEST05             YES                                          
         CLI   CKESTREC,C'L'       READ EST FROM A BILL RECORD?                 
         BE    CKEST10             YES                                          
         CLI   CKESTREC,C'U'       READ EST FROM A UNIT RECORD?                 
         BE    CKEST15             YES                                          
         DC    H'0'                ERROR - UNKNOWN RECORD TYPE                  
*                                                                               
CKEST05  L     RF,ADSTABUC         A(STATION BUCKET RECORD)                     
         USING STABUCK,RF          STATION BUCKET DSECT                         
         MVC   KEY+1(1),STABKAM    AGENCY/MEDIA                                 
         MVC   KEY+2(2),STABKCLT   CLIENT                                       
         MVC   KEY+4(3),SAVEPRD    PRODUCT                                      
         MVC   KEY+7(1),STABKEST   ESTIMATE                                     
         B     CKEST20             GO READ ESTIMATE RECORD                      
         DROP  RF                  DROP STATION BUCKET USING                    
*                                                                               
CKEST10  L     RF,ADBILL           A(BILL RECORD)                               
         USING BILLREC,RF          BILL RECORD DSECT                            
         MVC   KEY(8),BKEY         00/A/M/CLT/PRD/EST                           
         B     CKEST20             GO READ ESTIMATE RECORD                      
         DROP  RF                  DROP BILL RECORD USING                       
*                                                                               
CKEST15  L     R4,ANETBLK          A(NETBLOCK)                                  
         USING NETBLOCK,R4         NETBLOCK DSECT                               
         L     RF,NBAIO            RF = A(NBAIO)                                
         USING NURECD,RF           UNIT RECORD DSECT                            
         MVC   KEY+1(1),NUKAM      AGENCY/MEDIA                                 
         MVC   KEY+2(2),NUKCLT     CLIENT                                       
         MVC   KEY+4(3),=C'POL'    PRODUCT POL                                  
         MVC   KEY+7(1),NUKEST     ESTIMATE                                     
         DROP  R4,RF               DROP UNIT RECORD & NETBLOCK USINGS           
*                                                                               
CKEST20  L     RF,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDR,RF           ESTIMATE RECORD DSECT                        
         CLC   EKEY,KEY            IS THE EST RECORD ALREADY IN ADEST?          
         BE    CKEST25             YES - DO NOT RE-READ                         
*                                                                               
         GOTO1 HIGH                READ HIGH FOR THE ESTIMATE                   
*                                                                               
         CLC   KEY(13),KEYSAVE     DID WE FIND THE ESTIMATE?                    
         BE    *+6                 YES                                          
         DC    H'0'                NO - ESTIMATE MUST BE ON FILE                
*                                                                               
         GOTO1 GETEST              READ THE ESTIMATE RECORD INTO ADEST          
*                                                                               
         MVI   CKESTSW,1           FLAG THAT ESTIMATE WAS READ                  
***                                                                             
* WE MAY HAVE CLOBBERED THE BILL REC SINCE THE ESTIMATE BUFF (SPESTBUF          
* IN SPREPFILEC) IS ONLY DEFINED AS XL1000 WHILE THE ESTIMATE RECORD            
* IS MORE THAN LIKELY 1,274 BYTES (AS OF 9/26/16 ANYWAY) AND WILL BLOW          
* AWAY THE START OF THE BILL RECORD (SPBILBUF) WHICH COMES RIGHT AFTER          
***                                                                             
         CLI   CKESTREC,C'L'       PROCESSING BILLS?                            
         BNE   CKEST25             NO                                           
         MVC   KEY(13),SAVEKEY     SAVED BILL KEY                               
*                                                                               
         GOTO1 HIGH                READ HIGH FOR THE BILL KEY                   
*                                                                               
         CLC   KEY(13),KEYSAVE     IS THE BILL THERE?                           
         BE    *+6                 YES                                          
         DC    H'0'                BILL MUST BE ON FILE                         
*                                                                               
         GOTO1 GETBILL             ADBILL IS NOW RESTORED AFTER GETEST          
*                                                                               
         B     CKEST30             DON'T DO READ HIGH AGAIN                     
*                                                                               
CKEST25  MVC   KEY,SAVEKEY         RESTORE KEY                                  
         CLI   CKESTSW,1           DID I READ THE ESTIMATE RECORD?              
         BNE   CKEST30             NO                                           
*                                                                               
         GOTO1 HIGH                YES - RESTORE THE READ SEQUENCE              
*                                                                               
CKEST30  MVC   AREC,SAVEREC        RESTORE AREC                                 
*                                                                               
CKESTXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'CKPRD - READ PRODUCT RECORD IF NOT ALREADY IN ADPRD'            
CKPRD    CSECT                                                                  
         NMOD1 0,CKPRD                                                          
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
         MVC   SAVEREC,AREC        SAVE CURRENT RECORD ADDRESS                  
         MVI   CKPRDSW,0           INIT TO NO ESTIMATE READ                     
         XC    KEY,KEY             CLEAR THE KEY                                
*                                                                               
         L     RF,ADBILL           A(BILL RECORD)                               
         USING BILLREC,RF          BILL RECORD DSECT                            
         MVC   KEY(7),BKEY         00/A/M/CLT/PRD/EST                           
         DROP  RF                  DROP BILL RECORD USING                       
*                                                                               
         L     RF,ADPRD            A(PRODUCT RECORD)                            
         USING PRDHDR,RF           PRODUCT RECORD DSECT                         
         CLC   PKEY,KEY            IS THE PRD RECORD ALREADY IN ADPRD?          
         BE    CKPRD25             YES - DO NOT RE-READ                         
         DROP  RF                  DROP PRD RECORD USING                        
*                                                                               
         GOTO1 HIGH                READ HIGH FOR THE PRODUCT                    
*                                                                               
         CLC   KEY(13),KEYSAVE     DID WE FIND THE PRODUCT?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO - ESTIMATE MUST BE ON FILE                
*                                                                               
         GOTO1 GETPRD              READ THE PRODUCT RECORD INTO ADPRD           
*                                                                               
         MVI   CKPRDSW,1           FLAG THAT PRODUCT WAS READ                   
*                                                                               
CKPRD25  MVC   KEY,SAVEKEY         RESTORE KEY                                  
         CLI   CKPRDSW,1           DID I READ THE PRODUCT RECORD?               
         BNE   CKPRD30             NO                                           
*                                                                               
         GOTO1 HIGH                YES - RESTORE THE READ SEQUENCE              
*                                                                               
CKPRD30  MVC   AREC,SAVEREC        RESTORE AREC                                 
*                                                                               
CKPRDXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDCLT - READ CLIENT RECORD IF NOT ALREADY IN ADCLT'             
RDCLT    CSECT                                                                  
         NMOD1 0,RDCLT                                                          
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         ICM   R3,15,4(R1)         A(AGENCY/MEDIA CODE)                         
         MVC   SVCLTAM,0(R3)       AGENCY/MEDIA                                 
*                                                                               
         ICM   R3,15,8(R1)         A(UNPACKED CLIENT CODE)                      
         GOTO1 CLPACK,DMCB,0(R3),SVCLTCLT                                       
*                                                                               
         L     R6,ADCLT            A(CLIENT RECORD)                             
         USING CLTHDR,R6           CLIENT RECORD DSECT                          
         CLC   SVCLTKEY,CKEYAM     IS THE CLT RECORD ALREADY IN ADCLT?          
         BE    RDCLTXIT            YES - DO NOT RE-READ                         
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   CKEYAM,SVCLTAM      AGENCY/MEDIA                                 
         MVC   CKEYCLT,SVCLTCLT    CLIENT                                       
         DROP  R6                  DROP CLIENT RECORD USING                     
*                                                                               
         GOTO1 HIGH                READ HIGH FOR THE CLIENT                     
*                                                                               
         CLC   KEY(13),KEYSAVE     DID WE FIND THE CLIENT RECORD?               
         BE    *+6                 YES                                          
         DC    H'0'                NO                                           
*                                                                               
         GOTO1 GETCLT              READ THE CLIENT RECORD INTO ADCLT            
*                                                                               
RDCLTXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDPRD - READ PRODUCT RECORD IF NOT ALREADY IN ADPRD'            
RDPRD    CSECT                                                                  
         NMOD1 0,RDPRD                                                          
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         ICM   R3,15,4(R1)         A(AGENCY/MEDIA CODE)                         
         MVC   SVPRDAM,0(R3)       AGENCY/MEDIA                                 
*                                                                               
         ICM   R3,15,12(R1)        A(PRODUCT CODE)                              
         MVC   SVPRDPRD,0(R3)      PRODUCT CODE                                 
*                                                                               
         ICM   R3,15,8(R1)         A(UNPACKED CLIENT CODE)                      
         GOTO1 CLPACK,DMCB,0(R3),SVPRDCLT                                       
*                                                                               
         L     R6,ADPRD            A(PRODUCT RECORD)                            
         USING PRDHDR,R6           PRODUCT RECORD DSECT                         
         CLC   SVPRDKEY,PKEYAM     IS THE PRD RECORD ALREADY IN ADPRD?          
         BE    RDPRDXIT            YES - DO NOT RE-READ                         
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   PKEYAM,SVPRDAM      AGENCY/MEDIA                                 
         MVC   PKEYCLT,SVPRDCLT    CLIENT                                       
         MVC   PKEYPRD,SVPRDPRD    PRODUCT                                      
         DROP  R6                  DROP PRODUCT RECORD USING                    
*                                                                               
         GOTO1 HIGH                READ HIGH FOR THE PRODUCT                    
*                                                                               
         CLC   KEY(13),KEYSAVE     DID WE FIND THE PRODUCT?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO                                           
*                                                                               
         GOTO1 GETPRD              READ THE PRODUCT RECORD INTO ADPRD           
*                                                                               
RDPRDXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDEST - READ ESTIMATE RECORD IF NOT ALREADY IN ADEST'           
RDEST    CSECT                                                                  
         NMOD1 0,RDEST                                                          
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         ICM   R3,15,4(R1)         A(AGENCY/MEDIA CODE)                         
         MVC   SVESTAM,0(R3)       AGENCY/MEDIA                                 
*                                                                               
         ICM   R3,15,12(R1)        A(PRODUCT CODE)                              
         MVC   SVESTPRD,0(R3)      PRODUCT CODE                                 
*                                                                               
         ICM   R3,15,16(R1)        A(ESTIMATE CODE)                             
         MVC   SVESTEST,0(R3)      ESTIMATE CODE                                
*                                                                               
         ICM   R3,15,8(R1)         A(UNPACKED CLIENT CODE)                      
         GOTO1 CLPACK,DMCB,0(R3),SVESTCLT                                       
*                                                                               
         L     R6,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDR,R6           ESTIMATE RECORD DSECT                        
         CLC   SVESTKEY,EKEYAM     IS THE PRD RECORD ALREADY IN ADPRD?          
         BE    RDESTXIT            YES - DO NOT RE-READ                         
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   EKEYAM,SVESTAM      AGENCY/MEDIA                                 
         MVC   EKEYCLT,SVESTCLT    CLIENT                                       
         MVC   EKEYPRD,SVESTPRD    PRODUCT                                      
         MVC   EKEYEST,SVESTEST    ESTIMATE                                     
         DROP  R6                  DROP ESTIMATE RECORD USING                   
*                                                                               
         GOTO1 HIGH                READ HIGH FOR THE ESTIMATE                   
*                                                                               
         CLC   KEY(13),KEYSAVE     DID WE FIND THE ESTIMATE?                    
         BE    *+6                 YES                                          
         DC    H'0'                NO                                           
*                                                                               
         GOTO1 GETEST              READ THE ESTIMATE RECORD INTO ADEST          
*                                                                               
RDESTXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDMKT - READ MARKET RECORD IF NOT ALREADY IN ADMARKET'          
RDMKT    CSECT                                                                  
         NMOD1 0,RDMKT                                                          
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         ICM   R3,15,4(R1)         A(MEDIA CODE)                                
         MVC   SVMKTMED,0(R3)      MEDIA                                        
*                                                                               
         ICM   R3,15,8(R1)         A(MARKET CODE)                               
         MVC   SVMKTMRK,0(R3)      MARKET CODE                                  
*                                                                               
         ICM   R3,15,12(R1)        A(AGENCY CODE)                               
         MVC   SVMKTAGY,0(R3)      AGENCY CODE                                  
*                                                                               
         L     R6,ADMARKET         A(MARKET RECORD)                             
         USING MKTRECD,R6          MARKET RECORD DSECT                          
         CLC   SVMKTKEY,MKTKMED    IS THE MKT REC ALREADY IN ADMARKET?          
         BE    RDMKTEQU            YES - DO NOT RE-READ                         
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   MKTKEY,C'0'         INIT TO ZEROES                               
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,MKTKTYPQ   C'M'                                         
         MVC   MKTKMED,SVMKTMED    MEDIA                                        
         MVC   MKTKMKT,SVMKTMRK    MARKET                                       
         MVC   MKTKAGY,SVMKTAGY    AGENCY                                       
*                                                                               
         GOTO1 HIGHMKT             READ HIGH FOR MARKET                         
*                                                                               
         CLC   MKTKEY,KEYSAVE      FOUND OUR KEY?                               
         BE    RDMKTEQU            YES                                          
         DROP  R6                  DROP MARKET RECORD USING                     
*                                                                               
RDMKTNEQ LTR   RE,RE               SET CC NEQ                                   
         B     RDMKTXIT            EXIT                                         
*                                                                               
RDMKTEQU CR    RE,RE               SET CC EQU                                   
*                                                                               
RDMKTXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDMKTGRP - READ MARKET GROUP RECORD'                            
RDMKTGRP CSECT                                                                  
         NMOD1 0,RDMKTGRP                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         ICM   R3,15,4(R1)         A(AGENCY/MEDIA CODE)                         
         MVC   PMGRAM,0(R3)        AGENCY/MEDIA                                 
*                                                                               
         ICM   R3,15,12(R1)        A(PRODUCT GROUP)                             
         MVC   PMGRPGR,0(R3)       PRODUCT GROUP                                
*                                                                               
         ICM   R3,15,16(R1)        A(MARKET CODE)                               
         MVC   PMGRMKT,0(R3)       MARKET CODE                                  
*                                                                               
         ICM   R3,15,8(R1)         A(UNPACKED CLIENT CODE)                      
         GOTO1 CLPACK,DMCB,0(R3),PMGRCLT                                        
*                                                                               
         CLC   PMGRKEY,SVMGPKEY    TRYING TO READ THE SAME MGRP REC?            
         BE    RDMGRXIT            YES - DO NOT RE-READ                         
*                                                                               
         MVC   SVMGPKEY,PMGRKEY    SAVE OFF THE KEY                             
         XC    SVMGROUP,SVMGROUP   CLEAR MARKET GROUP CODE                      
         XC    SVMGRPN,SVMGRPN     CLEAR MARKET GROUP NAME 1                    
         XC    SVMGRPN2,SVMGRPN2   CLEAR MARKET GROUP NAME 2                    
         XC    SVMGRPN3,SVMGRPN3   CLEAR MARKET GROUP NAME 3                    
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         USING MKGRECD,R6          MARKET GROUP DSECT                           
         CLI   SVMGPPID,0          SHOULD WE EVEN TRY FOR PRD GROUP?            
         BE    RDMGR15             NO - TRY FOR CLIENT LEVEL                    
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   MKGPTYP,=X'0D82'    MARKET GROUP PASSIVE                         
         MVC   MKGPAGMD,SVMGPAM    AGENCY/MEDIA                                 
         MVC   MKGPCLT,SVMGPCLT    CLIENT                                       
         MVC   MKGPPID,SVMGPPID    PRDGRP ID                                    
         MVC   MKGPPGRP,SVMGPPNM   PRDGRP NUMBER                                
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     RDMGR11             GO TEST KEY                                  
*                                                                               
RDMGR10  GOTO1 SEQ                 READ SEQ                                     
*                                                                               
RDMGR11  CLC   MKGPTYP(8),KEYSAVE  SAME TYPE/AGENCY/MEDIA/CLT/PGRP?             
         BNE   RDMGR15             NO - TRY FOR CLIENT LEVEL                    
         CLC   MKGPMKT,SVMGPMRK    MATCH ON MARKET?                             
         BNE   RDMGR10             NO - READ SEQ                                
         B     RDMGR60             YES - GO READ MARKET GROUP RECORD            
*                                                                               
RDMGR15  XC    KEY,KEY             CLEAR THE KEY                                
         MVC   MKGPTYP,=X'0D82'    MARKET GROUP PASSIVE                         
         MVC   MKGPAGMD,SVMGPAM    AGENCY/MEDIA                                 
         MVC   MKGPCLT,SVMGPCLT    CLIENT                                       
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     RDMGR21             GO TEST KEY                                  
*                                                                               
RDMGR20  GOTO1 SEQ                 READ SEQ                                     
*                                                                               
RDMGR21  CLC   MKGPTYP(5),KEYSAVE  SAME TYPE/AGENCY/MEDIA/CLIENT?               
         BNE   RDMGR25             NO - CHECK FOR ALL CLIENT LEVEL              
         OC    MKGPPID(3),MKGPPID  HAVE A PRODUCT GROUP?                        
         BNZ   RDMGR20             YES - SKIP                                   
         CLC   MKGPMKT,SVMGPMRK    MATCH ON MARKET?                             
         BNE   RDMGR20             NO - READ SEQ                                
         B     RDMGR60             YES - GO READ MARKET GROUP RECORD            
*                                                                               
RDMGR25  XC    KEY,KEY             CLEAR THE KEY                                
         MVC   MKGPTYP,=X'0D82'    MARKET GROUP PASSIVE (ALL CLT/PGRP)          
         MVC   MKGPAGMD,SVMGPAM    AGENCY/MEDIA                                 
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     RDMGR31             GO TEST KEY                                  
*                                                                               
RDMGR30  GOTO1 SEQ                 READ SEQ                                     
*                                                                               
RDMGR31  CLC   MKGPTYP(3),KEYSAVE  SAME TYPE/AGENCY/MEDIA?                      
         BNE   RDMGRXIT            NO - DONE                                    
         OC    MKGKCLT,MKGKCLT     HAVE A CLIENT CODE?                          
         BNZ   RDMGR30             YES - SKIP                                   
         OC    MKGPPID(3),MKGPPID  HAVE A PRODUCT GROUP?                        
         BNZ   RDMGR30             YES - SKIP                                   
         CLC   MKGPMKT,SVMGPMRK    MATCH ON MARKET?                             
         BNE   RDMGR30             NO - READ SEQ                                
*                                                                               
RDMGR60  MVC   MKGKTYP,=X'0D02'    NOW READ THE ACTIVE KEY                      
         XC    MKGPMKT,MKGPMKT     NO MKT IN THE ACTIVE KEY                     
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   MKGKEY,KEYSAVE      DID WE FIND EXACT KEY?                       
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING IS WRONG                      
*                                                                               
         GOTO1 GETMKTGR            GET THE MARKET GROUP RECORD                  
*                                                                               
         L     R6,ADMKTGRP         A(MARKET GROUP RECORD)                       
         LA    R6,24(R6)           A(FIRST ELEMENT)                             
*                                                                               
RDMG70   CLI   0(R6),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SOMETHING IS WRONG                     
*                                                                               
         CLI   0(R6),X'10'         X'10' MKTGRP BREAK NAMES ELEMENT?            
         BE    RDMG80              YES                                          
         LLC   RF,1(R6)            ELEMENT LENGTH                               
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     RDMG70              NO - GET NEXT ELEMENT                        
*                                                                               
         USING MKGEL10,R6          MKTGRP BREAK NAMES DSECT                     
RDMG80   MVC   SVMGRPN,MKGNAM1     MARKET GROUP NAME 1                          
         OC    SVMGRPN,SPACES      SPACE PAD                                    
         MVC   SVMGRPN2,MKGNAM2    MARKET GROUP NAME 2                          
         OC    SVMGRPN2,SPACES     SPACE PAD                                    
         MVC   SVMGRPN3,MKGNAM3    MARKET GROUP NAME 2                          
         OC    SVMGRPN3,SPACES     SPACE PAD                                    
         DROP  R6                  DROP MKTGRP BREAK NAMES USING                
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         USING MKGRECD,R6          MARKET GROUP DSECT                           
*                                                                               
         XC    MKGPPID,MKGPPID     NO PRDGRP ID IN MGRDEF RECORD                
         XC    MKGKPGRP,MKGKPGRP   NO PRDGRP NUMBER IN MGRDEF RECORD            
         MVC   SVMGRBIN,MKGKMGRP   SAVE OFF THE BINARY MGROUP                   
         XC    MKGKMGRP,MKGKMGRP   NO MKTGRP NUMBER IN MGRDEF RECORD            
*                                                                               
         GOTO1 HIGH                READ THE MGRDEF RECORD                       
*                                                                               
         CLC   MKGKEY,KEYSAVE      DID WE FIND EXACT KEY?                       
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING IS WRONG                      
*                                                                               
         MVC   AREC,ADBUY          READ RECORD INTO ADBUY                       
*                                                                               
         GOTO1 GET                 GET THE RECORD                               
*                                                                               
         L     R6,AREC             A(MARKET GROUP DEFINITION RECORD)            
*                                                                               
         L     RE,=A(SPMGRTAB)     RE = A(SPMGRTAB)                             
         LHI   RF,NUMMGRS          NUMBER OF MARKET GROUP CODES                 
*                                                                               
RDMG85   CLC   MKGKMID,2(RE)       MATCH ON 1 BYTE BINARY?                      
         BE    RDMG86              YES                                          
         LA    RE,3(RE)            BUMP TO NEXT ENTRY                           
         BCT   RF,RDMG85           PROCESS NEXT TABLE ENTRY                     
         DC    H'0'                THIS SHOULD NEVER HAPPEN                     
*                                                                               
RDMG86   MVC   SVMGROUP(2),0(RE)   1 OR 2 CHARACTER MGRP ID                     
*                                                                               
         LA    R6,24(R6)           A(FIRST ELEMENT)                             
*                                                                               
RDMG90   CLI   0(R6),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SOMETHING IS WRONG                     
*                                                                               
         CLI   0(R6),X'01'         MKTGRP BREAK DESCRIPTION ELEMENT?            
         BE    RDMG95              YES                                          
         LLC   RF,1(R6)            ELEMENT LENGTH                               
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     RDMG90              NO - GET NEXT ELEMENT                        
*                                                                               
         USING MKGEL01,R6          MKTGRP BREAK NAMES DSECT                     
RDMG95   LLC   RE,MKGBK1LN         BREAK LENGTH 1                               
         XR    RF,RF               CLEAR RE                                     
         ICM   RF,1,MKGBK2LN       HAVE BREAK LENGTH 2?                         
         BZ    RDMG100             NO                                           
         AR    RE,RF               ADD BREAK LENGTH 2                           
         ICM   RF,1,MKGBK3LN       HAVE BREAK LENGTH 3?                         
         BZ    RDMG100             NO                                           
         AR    RE,RF               ADD BREAK LENGTH 3                           
         DROP  R6                  DROP MKTGRP BREAK NAMES USING                
*                                                                               
RDMG100  LA    R6,KEY              R6 = KEY                                     
         USING MKGRECD,R6          MARKET GROUP DSECT                           
         LA    RF,SVMGROUP+1       POINT TO SECOND CHAR OF MGRPID               
         CLI   0(RF),C' '          ANYTHING THERE?                              
         BH    *+6                 YES                                          
         BCTR  RF,0                NO - BACK 1 FOR 1 CHAR MGRP                  
*                                                                               
         UNPK  DUB,SVMGRBIN(3)     UNPACK MARKET GROUP NUMBER                   
         BCTR  RE,0                -1 FOR EX                                    
         EX    RE,*+8              EXECUTE                                      
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   1(0,RF),DUB+3       MARKET GROUP CODE                            
*                                                                               
RDMGRXIT XIT1                      EXIT                                         
*                                                                               
NUMMGRS  EQU   (SPMGRTBX-SPMGRTAB)/3                                            
*                                                                               
       ++INCLUDE SPMGRTAB                                                       
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDSTADD - READ STA ADDRESS REC IF NOT IN ADSTATAD'              
RDSTADD  CSECT                                                                  
         NMOD1 0,RDSTADD                                                        
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         ICM   R3,15,4(R1)         A(MEDIA CODE)                                
         MVC   SVADDMED,0(R3)      MEDIA                                        
*                                                                               
         ICM   R3,15,8(R1)         A(STATION)                                   
         MVC   SVADDSTA,0(R3)      STATION CODE                                 
         CLI   NETOPT,C'N'         NETWORK REQUEST?                             
         BNE   *+8                 NO                                           
         MVI   SVADDSTA+4,C'N'     YES - END WITH MEDIA N                       
*                                                                               
         ICM   R3,15,12(R1)        A(AGENCY CODE)                               
         MVC   SVADDAGY,0(R3)      AGENCY CODE                                  
*                                                                               
         L     R6,ADSTATAD         A(STATION ADDRESS RECORD)                    
         USING ADDRECD,R6          STATION ADDRESS RECORD DSECT                 
         CLC   SVADDKEY,ADDKMED    IS THE ADDRESS ALREADY IN ADSTATAD?          
         BE    RDADDEQU            YES - DO NOT RE-READ                         
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   ADDRKEY,C'0'        INIT TO ZEROES                               
         MVC   ADDRKEY+1(L'ADDRKEY-1),ADDRKEY                                   
         MVI   ADDKTYPE,ADDKTYPQ   C'A'                                         
         MVC   ADDKMED,SVADDMED    MEDIA                                        
         MVC   ADDKCALL,SVADDSTA   STATION                                      
         MVC   ADDKAGY,SVADDAGY    AGENCY                                       
*                                                                               
         GOTO1 HIGHSTAD            READHI FOR THE STATION ADDRESS REC           
*                                                                               
         CLC   ADDRKEY,KEYSAVE     FOUND OUR KEY?                               
         BE    RDADDEQU            YES                                          
         DROP  R6                  DROP STATION ADDRESS RECORD USING            
*                                                                               
RDADDNEQ LTR   RE,RE               SET CC NEQ                                   
         B     RDADDXIT            EXIT                                         
*                                                                               
RDADDEQU CR    RE,RE               SET CC EQU                                   
*                                                                               
RDADDXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GNTABLE - MOVE TABLE DATA TO OUTPUT AREA'                       
GNTABLE  CSECT                                                                  
         NMOD1 0,GNTABLE                                                        
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         ICM   R6,15,4(R1)         A(TABLE)                                     
         ICM   R7,15,8(R1)         A(SOFT FIELD MAPPING TABLE)                  
         ICM   R2,15,12(R1)        A(SOFT FIELD DATA AREA)                      
         ICM   R5,15,16(R1)        A(EXCLUSIONS TABLE)                          
*                                                                               
         L     R1,AOUTREC          R1 = A(OUTREC)                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         AR    R1,RF               END OF OUTREC                                
*                                                                               
         XR    R3,R3               CLEAR R3                                     
*                                                                               
GNTAB05  CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    GNTABX              YES - DONE                                   
         CLI   0(R6),X'00'         HAVE A SOFT FIELD TO PROCESS?                
         BE    GNTAB10             NO                                           
         BAS   RE,EXCLUDE          EXCLUDE THIS FIELD?                          
         BE    GNTAB05             YES - R6 BUMPED                              
         BAS   RE,GNSPFLD          PROCESS SPECIAL FIELD                        
         BE    GNTAB05             CC EQU = FIELD PROCESSED & R6 BUMPED         
GNTAB10  IC    R3,1(R6)            LENGTH OF LITERAL                            
         BCTR  R3,0                DECREMENT FOR EX                             
         EX    R3,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),2(R6)       MOVE LITERAL TO OUTREC                       
         LA    R4,1(R4,R3)         BUMP TO NEXT POSITION IN OUTREC              
         CR    R4,R1               PAST OUTREC?                                 
         BNH   *+6                 NO                                           
         DC    H'0'                YES - EXPAND OUTREC                          
         LA    R6,3(R3,R6)         BUMP TO NEXT TABLE ENTRY                     
         B     GNTAB05             PROCESS NEXT TABLE ENTRY                     
*                                                                               
GNTABX   XIT1  REGS=(R4)           EXIT BUT KEEP R4 INTACT                      
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
EXCLUDE  NTR1                                                                   
*                                                                               
         LR    R0,R5               SAVE R5                                      
*                                                                               
EXCLD00  CLI   0(R5),X'FF'         END OF TABLE?                                
         BE    EXCLDNEQ            YES - SET CC NEQ (DON'T EXCLUDE)             
         CLC   0(1,R6),0(R5)       POSSIBLY EXCLUDE THIS ENTRY?                 
         BE    EXCLD10             YES                                          
         LA    R5,2(R5)            BUMP TO NEXT TABLE ENTRY                     
         B     EXCLD00             PROCESS NEXT TABLE ENTRY                     
*                                                                               
* IF 2ND BYTE OF TABLE HAS THE X'01' BIT SET = SPOT CANADIAN FIELD    *         
* IF 2ND BYTE OF TABLE HAS THE X'02' BIT SET = SPOT US FIELD          *         
* IF 2ND BYTE OF TABLE HAS THE X'04' BIT SET = NET US FIELD           *         
*                                                                               
EXCLD10  CLI   CANADIAN,C'Y'       CANADIAN AGENCY?                             
         BNE   *+12                NO                                           
         TM    1(R5),X'01'         IS THIS A CANADIAN FIELD?                    
         BNZ   EXCLDNEQ            YES - SET CC NEQ (DON'T EXCLUDE)             
*                                                                               
         CLI   NETOPT,0            SPOT REQUEST?                                
         BNE   EXCLD20             NO                                           
         CLI   CANADIAN,C'Y'       CANADIAN AGENCY?                             
         BE    EXCLD20             NO                                           
         TM    1(R5),X'02'         IS THIS A US SPOT FIELD?                     
         BNZ   EXCLDNEQ            YES - SET CC NEQ (DON'T EXCLUDE)             
*                                                                               
EXCLD20  CLI   NETOPT,C'N'         NET REQUEST?                                 
         BNE   *+12                NO                                           
         TM    1(R5),X'04'         IS THIS A NET FIELD?                         
         BNZ   EXCLDNEQ            YES - SET CC NEQ (DON'T EXCLUDE)             
*                                                                               
         LLC   RF,1(R6)            LENGTH OF LITERAL                            
         LA    R6,2(RF,R6)         BUMP TO NEXT TABLE ENTRY                     
         CLC   =C'",',2(R6)        END QUOTE + COMMA?                           
         BNE   EXCLD30             NO                                           
         LLC   RF,1(R6)            LENGTH OF LITERAL                            
         LA    R6,2(RF,R6)         BUMP TO NEXT TABLE ENTRY                     
         B     EXCLDEQU            SET CC EQU (WE EXCLUDED THIS ENTRY)          
*                                                                               
EXCLD30  CLI   2(R6),C'"'          END QUOTE?                                   
         BNE   EXCLDEQU            NO                                           
         LLC   RF,1(R6)            LENGTH OF LITERAL                            
         LA    R6,2(RF,R6)         BUMP TO NEXT TABLE ENTRY                     
         BCTR  R4,0                BACK UP R4 BY 1                              
         CLI   0(R4),C','          IS THERE A COMMA HERE?                       
         BE    *+6                 YES                                          
         DC    H'0'                NO - THERE SHOULD BE!                        
*                                                                               
EXCLDEQU LR    R5,R0               RESTORE R5                                   
         CR    RE,RE               SET CC EQU                                   
         B     EXCLDXIT            AND EXIT                                     
*                                                                               
EXCLDNEQ LR    R5,R0               RESTORE R5                                   
         LTR   RB,RB               SET CC NEQ                                   
*                                                                               
EXCLDXIT XIT1  REGS=(R4,R6)        EXIT BUT KEEP R4 & R6 INTACT                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
GNSPFLD  NTR1                                                                   
*                                                                               
GNFLD00  CLI   0(R7),X'FF'         END OF TABLE?                                
         BE    GNFLDNEQ            YES - SET CC NEQ                             
         CLC   0(1,R6),0(R7)       IS THIS A SPECIAL FIELD?                     
         BE    GNFLD10             YES                                          
         LA    R7,4(R7)            BUMP TO NEXT TABLE ENTRY                     
         B     GNFLD00             PROCESS NEXT TABLE ENTRY                     
*                                                                               
GNFLD10  BAS   RE,EMPTYFLD         IS THE SOFT DATA NULL?                       
         BE    GNFLDEQU            YES - EXIT WITH CC EQU                       
*                                                                               
         LLC   RF,1(R6)            LENGTH OF LITERAL                            
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),2(R6)       MOVE LITERAL TO TPREC                        
         LA    R4,1(RF,R4)         MOVE SOFT DATA HERE                          
         LA    R6,3(RF,R6)         BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,3,1(R7)          DISPLACEMENT INTO DATA                       
         AR    R2,RE               INDEX INTO DATA                              
         LLC   R3,3(R7)            LENGTH OF DATA                               
         BCTR  R3,0                DECREMENT FOR EX                             
         EX    R3,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),0(R2)       MOVE SOFT DATA                               
*                                                                               
         BAS   RE,ESCSPCL          ESCAPE SPECIAL CHARACTERS                    
*                                                                               
         LA    R4,1(R3,R4)         LAST BYTE OF SOFT DATA                       
*                                                                               
         CLI   0(R4),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R4,*-8              NO, CHECK PREVIOUS BYTE                      
         LA    R4,1(R4)            BUMP PAST LAST BYTE OF DATA                  
*                                                                               
GNFLDEQU CR    RE,RE               SET CC EQU                                   
         B     GNFLDXIT            AND EXIT                                     
*                                                                               
GNFLDNEQ LTR   RB,RB               SET CC NEQ                                   
*                                                                               
GNFLDXIT XIT1  REGS=(R4,R6)        EXIT BUT KEEP R4-R6 INTACT                   
*                                                                               
         LTORG                     LITERAL POOL                                 
***********************************************************************         
* THIS ROUTINE WILL ESCAPE SPECIAL CHARACTERS BY ADDING A BACKSLASH   *         
* BEFORE THE SPECIAL CHARACTER AND SHIFTING EVERYTHING DOWN BY 1      *         
* FOR EXAMPLE                                                         *         
* SAMPLE DA"TA                                                        *         
* WILL BECOME                                                         *         
* SAMPLE DA\"TA                                                       *         
***********************************************************************         
ESCSPCL  NTR1                                                                   
*                                                                               
         AHI   R3,1                INCREMENT R3 TO GET LENGTH OF FIELD          
         LR    R5,R4               POINT TO DATA IN OUTREC                      
*                                                                               
ESC00    CLI   0(R5),C'"'          IS THIS A DOUBLE QUOTE?                      
         BE    *+12                YES                                          
         CLI   0(R5),C'\'          IS THIS A BACKSLASH?                         
         BNE   ESC10               NO                                           
*                                                                               
         LR    RE,R3               LENGTH OF THE REST OF THE FIELD              
         LR    RF,R5               A(CURRENT OUTPUT POINTER)                    
         MVC   HALF(1),0(RF)       FIRST BYTE                                   
*                                                                               
ESC05    MVC   BYTE,1(RF)          SAVE NEXT BYTE OFF                           
         MVC   1(1,RF),HALF        MOVE BYTE OVER BY 1 TO THE RIGHT             
         LA    RF,1(RF)            BUMP TO NEXT BYTE                            
         MVC   HALF(1),BYTE        SAVE OFF BYTE                                
         BCT   RE,ESC05            PROCESS NEXT BYTE                            
*                                                                               
         MVI   0(R5),C'\'          ESCAPE THE DOUBLE QUOTE FOR JSON             
         AHI   R4,1                BUMP OUTPUT BY 1                             
         AHI   R5,1                ACCOUNT FOR ESCAPE CHARACTER                 
         AHI   R3,1                ACCOUNT FOR ESCAPE CHARACTER                 
*                                                                               
ESC10    LA    R5,1(R5)            BUMP POINTER                                 
         BCT   R3,ESC00            PROCESS NEXT BYTE                            
*                                                                               
ESCXIT   XIT1  REGS=(R4)           EXIT BUT KEEP R4 INTACT                      
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
EMPTYFLD NTR1                                                                   
*                                                                               
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,3,1(R7)          DISPLACEMENT INTO DATA                       
         LLC   RF,3(R7)            LENGTH OF DATA                               
         BCTR  RF,0                DECREMENT FOR EX                             
         AR    R2,RE               INDEX INTO DATA                              
         EX    RF,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         CLC   0(0,R2),SPACES      DO WE HAVE ANY ACTUAL DATA HERE?             
         BH    EMPTYNEQ            YES - SET CC NEQ                             
*                                                                               
         LLC   RF,1(R6)            LENGTH OF LITERAL                            
         LA    R6,2(RF,R6)         BUMP TO NEXT TABLE ENTRY                     
         CLC   =C'",',2(R6)        END QUOTE + COMMA?                           
         BNE   EMPTY10             NO                                           
         LLC   RF,1(R6)            LENGTH OF LITERAL                            
         LA    R6,2(RF,R6)         BUMP TO NEXT TABLE ENTRY                     
         B     EMPTYEQU            SET CC EQU (WE EXCLUDED THIS ENTRY)          
*                                                                               
EMPTY10  CLI   2(R6),C'"'          END QUOTE?                                   
         BNE   EMPTYEQU            NO                                           
         LLC   RF,1(R6)            LENGTH OF LITERAL                            
         LA    R6,2(RF,R6)         BUMP TO NEXT TABLE ENTRY                     
         BCTR  R4,0                BACK UP R4 BY 1                              
         CLI   0(R4),C','          IS THERE A COMMA HERE?                       
         BE    *+6                 YES                                          
         DC    H'0'                NO - THERE SHOULD BE!                        
*                                                                               
EMPTYEQU CR    RE,RE               SET CC EQU                                   
         B     EMPTYXIT            AND EXIT                                     
*                                                                               
EMPTYNEQ LTR   RB,RB               SET CC NEQ                                   
*                                                                               
EMPTYXIT XIT1  REGS=(R4,R6)        EXIT BUT KEEP R4-R6 INTACT                   
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
***********************************************************************         
* This routine will create a unix file                                *         
* To see this file, follow the steps outlined below                   *         
* 1) In the command line, put in TSO OMVS                             *         
* 2) Change the directory by using the command cd /u/EDIHUB           *         
* 3) Use the command ls -t to list files (most recent listed first)   *         
* 4) copy the file name you wish to view                              *         
* 5) Display your file by using the command head filename             *         
***********************************************************************         
         TITLE 'UNIXFCRT - CREATE UNIX FILE'                                    
UNIXFCRT CSECT                                                                  
         NMOD1 0,UNIXFCRT                                                       
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LA    R0,C'E'             SENT TO EDIHUB                               
         LLC   RF,FILNMLEN         FILE NAME LENGTH                             
         GOTO1 =V(USSIO),DMCB,CREATE,((R0),FILENAME),(RF),0,0,0                 
         TM    DMCB+12,X'80'       ERROR?                                       
         BZ    *+6                 NO                                           
         DC    H'0'                YES - NO ERRORS TOLERATED                    
*                                                                               
UNIXXIT  XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
CREATE   DC    C'CREATE'                                                        
*                                                                               
         TITLE 'UNIXFPUT - PUT LINE TO UNIX FILE'                               
UNIXFPUT CSECT                                                                  
         NMOD1 0,UNIXFPUT                                                       
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         L     R6,AOUTREC          RE = A(OUTREC)                               
         LAY   R7,OUTRECLN-17      LENGTH OF OUTREC-1                           
         AR    R7,R6               LAST BYTE OF OUTREC                          
*                                                                               
         CLI   0(R7),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R7,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         LA    R7,1(R7)            1 PAST LAST BYTE                             
         MVI   0(R7),X'15'         USE X'15' for Unix                           
         SR    R7,R6               LENGTH OF DATA                               
*                                                                               
         GOTO1 =V(USSIO),DMCB,USSPUT,0(R6),(R7),0,0,0                           
         TM    DMCB+12,X'80'       ERROR?                                       
         BZ    *+6                 NO                                           
         DC    H'0'                YES - NO ERRORS TOLERATED                    
*                                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
USSPUT   DC    C'PUT'                                                           
*                                                                               
         TITLE 'UNIXFCLO - CLOSE UNIX FILE'                                     
UNIXFCLO CSECT                                                                  
         NMOD1 0,UNIXFCLO                                                       
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         GOTO1 =V(USSIO),DMCB,CLOSE,0,0,0,0,0                                   
         TM    DMCB+12,X'80'       ERROR?                                       
         BZ    *+6                 NO                                           
         DC    H'0'                YES - NO ERRORS TOLERATED                    
*                                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
CLOSE    DC    C'CLOSE'                                                         
*                                                                               
         TITLE 'GETMEDNM - GET MEDIA NAME'                                      
GETMEDNM CSECT                                                                  
         NMOD1 0,GETMEDNM                                                       
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         ICM   R3,15,4(R1)         A(MEDIA CODE)                                
         ICM   R4,15,8(R1)         A(MEDIA NAME TO POPULATE)                    
         MVC   0(11,R4),SPACES     INIT MEDIA NAME TO SPACES                    
*                                                                               
         CLI   NETOPT,C'N'         NETWORK REQUEST?                             
         BE    GMN40               YES                                          
*                                                                               
         L     R2,ADAGY            A(AGENCY RECORD)                             
         LA    R2,24(R2)           A(FIRST ELEMENT)                             
*                                                                               
GMN10    CLI   0(R2),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SOMETHING IS WRONG                     
*                                                                               
         CLI   0(R2),AGYMEDEQ      X'02' MEDIA CODE ELEMENT?                    
         BNE   GMN20               NO - BUMP TO NEXT ELEMENT                    
*                                                                               
         USING AGYMEDEL,R2         MEDIA CODE ELEMENT DSECT                     
         CLC   AGYMEDCD,0(R3)      MATCH ON MEDIA CODE?                         
         BE    GMN30               YES                                          
*                                                                               
GMN20    LLC   RF,1(R2)            ELEMENT LENGTH                               
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         B     GMN10               NO - GET NEXT ELEMENT                        
*                                                                               
GMN30    MVC   0(10,R4),AGYMEDEX   MEDIA NAME                                   
         OC    0(10,R4),SPACES     SPACE PAD                                    
         B     GMNXIT              DONE                                         
         DROP  R2                  DROP MEDIA CODE ELEMENT USING                
*                                                                               
GMN40    MVC   0(7,R4),=C'NETWORK' INIT TO NETWORK                              
         CLI   0(R3),C'N'          MEDIA N?                                     
         BE    GMNXIT              YES                                          
         MVC   0(7,R4),=C'CABLE  ' INIT TO CABLE                                
         CLI   0(R3),C'C'          MEDIA C?                                     
         BE    GMNXIT              YES                                          
         MVC   0(7,R4),=C'OTHER  ' INIT TO OTHER                                
         CLI   0(R3),C'S'          MEDIA S?                                     
         BE    *+8                 YES                                          
         CLI   0(R3),C' '          MEDIA BLANK?                                 
         BE    *+8                 YES                                          
         CLI   0(R3),X'00'         MEDIA NULLS?                                 
         BNE   GMNXIT              NO                                           
         MVC   0(11,R4),=C'SYNDICATION' SET TO SYNDICATION                      
*                                                                               
GMNXIT   XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'ULOUTPR - OUTPUT ROUTINE UNILEVER'                              
ULOUTPR  CSECT                                                                  
         NMOD1 0,ULOUTPR                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         LAY   R4,COLHDSUL         COLUMN HEADINGS                              
         LAY   R5,COLHDLEN         LENGTH OF COLUMN HEADINGS                    
         L     RE,AOUTREC          MOVE COLUMN HEADINGS TO OUTREC               
         LR    RF,R5               MOVE FOR LENGTH OF COLUMN HEADINGS           
         MVCL  RE,R4               MOVE TO OUTREC                               
*                                                                               
         L     RE,AOUTREC          RE = A(OUTREC)                               
         SHI   RE,4                OUTREC RECORD LENGTH                         
         LAY   RF,COLHDLEN+4       COLUMN HEADINGS                              
         STCM  RF,15,0(RE)         RECORD LENGTH IN OUTREC-4                    
*                                                                               
         AP    HDRCNT,=P'1'        HEADERS                                      
*                                                                               
         GOTO1 VTAPEWRT,DMCB,(RC)  WRITE RECORD TO TAPE                         
*                                                                               
         MVI   NETFLAGS,0          INIT NETWORK FLAGS                           
*                                                                               
ULOUT10  GOTO1 VSORTER,DMCB,=C'GET' GET SORT RECORD                             
*                                                                               
         ICM   R5,15,4(R1)         ANY MORE RECORDS FROM SORTER?                
         BZ    ULOUT50             NO - DONE                                    
         CLI   NETOPT,C'N'         NETWORK?                                     
         BE    *+10                YES - WE COUNT RECORDS LATER                 
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         MVC   SORTREC,0(R5)       MOVE SORTED TO SORTREC                       
*                                                                               
         LA    R5,SORTREC          R5 = SORTREC                                 
         USING SORTRECD,R5         SORTREC DSECT                                
*                                                                               
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   ULOUT25             NO - REPORT SPOT AS IS                       
         TM    NETFLAGS,FIRSTNET   PROCESSING FIRST SORT RECORD?                
         BNZ   ULOUT15             NO                                           
         MVC   SORTKYSV,SORTKEY    YES - SET SORTKYSV FIRST TIME IN             
         MVC   SORTRCSV,SORTREC    SAVE THE SORT RECORD                         
         OI    NETFLAGS,FIRSTNET   PROCESSED FIRST NET SORT REC FLAG            
*                                                                               
ULOUT15  CLC   SORTKYSV,SORTKEY    SAME SORT KEY AS LAST?                       
         BNE   ULOUT20             NO - REPORT                                  
         ICM   R1,15,SORTBNET      BILLED NET FROM THIS SORT REC                
         ICM   R2,15,SAVEBNET      RUNNING TOTAL BILLED NET FOR KEY             
         AR    R2,R1               ADD THIS ENTRY                               
         STCM  R2,15,SAVEBNET      NEW RUNNING TOTAL                            
         NI    NETFLAGS,X'FF'-THISNET  NOT PROCEESED YET                        
         B     ULOUT10             GET NEXT SORT REC                            
*                                                                               
ULOUT20  LA    R5,SORTRCSV         REPORT THE SAVED SORT RECORD                 
         MVC   SORTBNET,SAVEBNET   REPORT ONCE WITH RUNNING TOTAL               
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
         OI    NETFLAGS,THISNET    PROCESSED THIS TIME                          
*                                                                               
ULOUT25  L     RE,AOUTREC          RE = A(OUTREC)                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         XCEF                                                                   
*                                                                               
         LA    R7,P                POINT TO PRINT LINE                          
         USING BLINED,R7           BLINED DSECT TO COVER PRINT LINE             
         MVC   BLMEDIA,SORTNMED    MEDIA                                        
         MVC   BLCLT,SORTCLT       CLIENT                                       
         MVC   BLCLNAME,SORTCNAM   CLIENT NAME                                  
         MVC   BLPRD,SORTPRD       PRODUCT                                      
         EDIT  SORTEST,(3,BLEST),FILL=0                                         
         MVC   BLINVNO,SORTIINV    FULL INVOICE NUMBER                          
         MVC   BLINVMOS,SORTMOS    MOS MMM/YY                                   
         MVC   BLINVD,SORTIDAT     INVOICE DATE                                 
         EDIT  SORTIGRS,(13,BLGROSS),2,MINUS=YES                                
         MVC   BLSTA,SORTSTA       STATION/VENDOR CODE                          
         OC    BLSTA,SPACES        SPACE PAD                                    
         EDIT  SORTBNET,(13,BLVAMT),2,MINUS=YES                                 
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R7                  DROP PRINT LINE USING                        
*                                                                               
         L     R4,AOUTREC          R4 = A(OUTREC)                               
*                                                                               
         LA    R1,L'SORTIINV       LENGTH OF INVOICE (FIELD 1)                  
         LA    R2,SORTIINV         FULL INVOICE NUMBER                          
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,L'SORTIDAT       LENGTH OF INVOICE DATE (FIELD 2)             
         LA    R2,SORTIDAT         INVOICE DATE                                 
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,1                NUM OF DELIMITERS (FIELD 3)                  
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         LA    R1,3                LENGTH OF "380"/"381" (FIELD 4)              
         LA    R2,=C'380'          IF INVOICE AMOUNT POSITIVE                   
         CP    SORTINET,=P'0'      INVOICE AMOUNT POSITIVE?                     
         BNL   *+8                 YES                                          
         LA    R2,=C'381'          INVOICE AMOUNT NEGATIVE                      
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,12               LENGTH OF BUYER ID (FIELD 5)                 
         LA    R2,=C'AAA241069178' HARDCODE BUYER ID FOR ULV                    
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,4                NUM OF DELIMITERS (FIELDS 6-9)               
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         OC    SORTUDF2,SPACES     HAVE UDEF DATA?                              
         BH    ULOUT30             YES                                          
         LA    R1,1                NUM OF DELIMITERS (FIELD 10)                 
         BAS   RE,ADDNULL          ADD DELIMITER FOR NULL DATA                  
         B     ULOUT35             PROCESS NEXT FIELDS                          
*                                                                               
ULOUT30  XC    MYWORK,MYWORK       CLEAR MYWORK                                 
         MVC   MYWORK(L'SORTUDF2),SORTUDF2                                      
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         LA    R2,MYWORK           R2 = ESTIMATE UDEF2 (FIELD 10)               
         SR    R1,R2               R1 = LENGTH OF EST UDEF - 1                  
         LA    R1,1(R1)            R1 = LENGTH OF EST UDEF                      
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
ULOUT35  LA    R1,12               NUM OF DELIMITERS (FIELDS 11-22)             
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         LA    R1,L'SORTIDDT       LENGTH OF INV DUE DATE (FIELD 23)            
         LA    R2,SORTIDDT         INVOICE DUE DATE                             
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,61               NUM OF DELIMITERS (FIELDS 24-84)             
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         XC    MYWORK,MYWORK       CLEAR MYWORK                                 
         EDIT  SORTINET,(13,MYWORK),2,MINUS=YES,ALIGN=LEFT                      
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         LA    R2,MYWORK           R2 = START OF NET AMOUNT (FIELD 85)          
         SR    R1,R2               R1 = LENGTH OF NET AMOUNT - 1                
         LA    R1,1(R1)            R1 = LENGTH OF NET AMOUNT                    
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,4                LENGTH OF "0.00" (FIELD 86)                  
         LA    R2,=C'0.00'         INVOICE TAX AMOUNT = "0.00"                  
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         XC    MYWORK,MYWORK       CLEAR MYWORK                                 
         EDIT  SORTIGRS,(13,MYWORK),2,MINUS=YES,ALIGN=LEFT                      
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         LA    R2,MYWORK           R2 = START OF GROSS AMT (FIELD 87)           
         SR    R1,R2               R1 = LENGTH OF GROSS AMOUNT - 1              
         LA    R1,1(R1)            R1 = LENGTH OF GROSS AMOUNT                  
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,3                LENGTH OF "USD" (FIELD 88)                   
         LA    R2,=C'USD'          CURRENCY = "USD"                             
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,13               NUM OF DELIMITERS (FIELDS 89-101)            
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         LA    R1,1                LENGTH OF "1" (FIELD 102)                    
         LA    R2,=C'1'            QUANTITY = "1"                               
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,2                LENGTH OF "EA" (FIELD 103)                   
         LA    R2,=C'EA'           UNIT OF MEASURE = "EA"                       
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,1                NUM OF DELIMITERS (FIELD 104)                
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         XC    MYWORK,MYWORK       CLEAR MYWORK                                 
         EDIT  (B4,SORTBNET),(12,MYWORK),2,MINUS=YES,ALIGN=LEFT                 
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         LA    R2,MYWORK           R2 = VENDOR NET AMOUNT (FIELD 105)           
         SR    R1,R2               R1 = LENGTH OF NET AMOUNT - 1                
         LA    R1,1(R1)            R1 = LENGTH OF NET AMOUNT                    
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         XC    MYWORK,MYWORK       CLEAR MYWORK                                 
         MVC   MYWORK(L'SORTMEDN),SORTMEDN                                      
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         LA    R2,MYWORK           R2 = MEDIA NAME (FIELD 106)                  
         SR    R1,R2               R1 = LENGTH OF MEDIA NAME - 1                
         LA    R1,1(R1)            R1 = LENGTH OF MEDIA NAME                    
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         XC    MYWORK,MYWORK       CLEAR MYWORK                                 
         LA    R1,MYWORK           VENDOR CODE + NAME + MOS (FIELD 107)         
*                                                                               
         MVC   0(L'SORTSTA,R1),SORTSTA  VENDOR CODE                             
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         MVC   2(L'SORTSNAM,R1),SORTSNAM  VENDOR NAME                           
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         MVC   2(L'SORTMOS,R1),SORTMOS  MOS                                     
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         OC    MYWORK,SPACES       SPACE PAD                                    
         LA    R2,MYWORK           VENDOR CODE + NAME + MOS (FIELD 107)         
         SR    R1,R2               R1 = LENGTH OF FIELD - 1                     
         LA    R1,1(R1)            R1 = LENGTH OF FIELD                         
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,39               NUM OF DELIMITERS (FIELDS 108-146)           
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         LA    R1,2                WITHHOLD TAX INFORMATION (FIELD 147)         
         LA    R2,=C'NO'           HARDCODE TO "NO"                             
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,7                NUM OF DELIMITERS (FIELDS 148-154)           
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         LA    R1,2                GOODS/SERVICE INDICATOR (FIELD 155)          
         LA    R2,=C'01'           HARDCODE TO "01"                             
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,160              NUM OF DELIMITERS (FIELDS 156-315)           
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         XC    MYWORK,MYWORK       CLEAR MYWORK                                 
         LA    R1,MYWORK           CLT/PRD/EST CODE + NAME (FIELD 316)          
*                                                                               
         MVC   0(L'SORTCLT,R1),SORTCLT  CLIENT CODE                             
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         MVC   2(L'SORTCNAM,R1),SORTCNAM  CLIENT NAME                           
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         MVC   2(L'SORTCLT,R1),SORTPRD  PRODUCT CODE                            
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         MVC   2(L'SORTPNAM,R1),SORTPNAM  PRODUCT NAME                          
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         EDIT  SORTEST,(3,2(R1)),FILL=0                                         
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         MVC   2(L'SORTENAM,R1),SORTENAM  ESTIMATE NAME                         
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         OC    MYWORK,SPACES       SPACE PAD                                    
         LA    R2,MYWORK           CLT/PRD/EST CODE + NAME (FIELD 316)          
         SR    R1,R2               R1 = LENGTH OF FIELD - 1                     
         LA    R1,1(R1)            R1 = LENGTH OF FIELD                         
         BAS   RE,ADDINFO          ADD DATA & DELIMITER & BUMP R4               
*                                                                               
         LA    R1,1                NUM OF DELIMITERS (FIELDS 317-318)           
         BAS   RE,ADDNULL          ADD DELIMITERS FOR NULL DATA                 
*                                                                               
         L     RE,AOUTREC          RE = A(OUTREC)                               
         SHI   RE,4                RECORD LENGTH IN OUTREC - 4                  
         LR    RF,R4               LAST CHAR OF DATA                            
         SR    RF,RE               DIFFERENCE IS RECORD LENGTH                  
         STCM  RF,15,0(RE)         RECORD LENGTH KEPT HERE                      
*                                                                               
         GOTO1 VTAPEWRT,DMCB,(RC)  WRITE RECORD TO TAPE                         
*                                                                               
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   ULOUT10             NO - JUST GET NEXT SORT RECORD               
         TM    NETFLAGS,LASTNET    JUST PROCESSED LAST NET REC?                 
         BNZ   ULOUTXIT            YES - DONE                                   
         LA    R5,SORTREC          POINT TO SORT RECORD                         
         MVC   SORTKYSV,SORTKEY    YES - SET SORTKYSV FIRST TIME IN             
         MVC   SORTRCSV,SORTREC    SAVE THE SORT RECORD                         
         MVC   SAVEBNET,SORTBNET   INIT RUNNING TOTALS                          
         B     ULOUT10             GET NEXT SORT RECORD                         
         DROP  R5                  DROP SORT RECORD USING                       
*                                                                               
ULOUT50  CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   ULOUTXIT            NO - DONE                                    
         TM    NETFLAGS,THISNET    WAS LAST SORT REC PROCESSED?                 
         BNZ   ULOUTXIT            YES - DONE                                   
         OI    NETFLAGS,LASTNET    NO - SET LAST NET REC FLAG                   
         B     ULOUT20             REPORT LAST INVOICE                          
*                                                                               
ULOUTXIT XIT1                      DONE                                         
*                                                                               
ADDNULL  MVI   0(R4),C'|'          ADD DELIMITER                                
         LA    R4,1(R4)            ADDITIONAL DATA STARTS HERE                  
         BCT   R1,ADDNULL          ADD DELIMITERS UNTIL R1=0                    
         BR    RE                  RETURN TO CALLER                             
*                                                                               
ADDINFO  BCTR  R1,0                SUBTRACT 1 FROM LENGTH FOR EX                
         EX    R1,*+8              EXECUTE THE MVC                              
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   0(0,R4),0(R2)       UPDATE THE ENTRY                             
         LA    R4,1(R1,R4)         ADD DELIMITER HERE                           
         MVI   0(R4),C'|'          PIPELINE DELIMITED                           
         LA    R4,1(R4)            ADDITIONAL DATA STARTS HERE                  
         BR    RE                  RETURN TO CALLER                             
*                                                                               
FINDEND  LA    R1,MYWORK+L'MYWORK-1 LAST BYTE OF MYWORK                         
         CLI   0(R1),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R1,*-8              NO, CHECK PREVIOUS BYTE                      
         BR    RE                  RETURN TO CALLER                             
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
*        HEADER COLUMN HEADINGS FOR UNILEVER                                    
*                                                                               
COLHDSUL DC    C'InvoiceNumber|'                                                
         DC    C'InvoiceDate|'                                                  
         DC    C'Tax Point Date|'                                               
         DC    C'InvoiceType|'                                                  
         DC    C'BuyerID|'                                                      
         DC    C'Original Invoice Number|'                                      
         DC    C'Original Invoice Date|'                                        
         DC    C'Original Delivery Date|'                                       
         DC    C'Credit Reason|'                                                
         DC    C'PO Number|'                                                    
         DC    C'Delivery Note Number|'                                         
         DC    C'PaymentReference|'                                             
         DC    C'PaymentMethod|'                                                
         DC    C'FormOfPayment|'                                                
         DC    C'Payment Terms|'                                                
         DC    C'NetPaymentDays|'                                               
         DC    C'Start Date|'                                                   
         DC    C'End Date|'                                                     
         DC    C'Order Date|'                                                   
         DC    C'Delivery Date|'                                                
         DC    C'ShipDate|'                                                     
         DC    C'Declaration Date|'                                             
         DC    C'Payment Due By Date|'                                          
         DC    C'Early Payment Date|'                                           
         DC    C'ManufactureDate|'                             line 25          
         DC    C'ExpiryDate|'                                                   
         DC    C'InvFromName|'                                                  
         DC    C'InvFromAdd1|'                                                  
         DC    C'InvFromAdd2|'                                                  
         DC    C'InvFromCity|'                                                  
         DC    C'InvFromPostalCode|'                                            
         DC    C'InvFromState|'                                                 
         DC    C'InvFromCountry|'                                               
         DC    C'Supplier QST Tax. Reg. Num.|'                                  
         DC    C'Supplier PST Tax. Reg. Num.|'                                  
         DC    C'Supplier GST Tax. Reg. Num.|'                                  
         DC    C'Supplier HST Tax. Reg. Num.|'                                  
         DC    C'Main Supplier Contact Name|'                                   
         DC    C'Main Supplier Contact Tel|'                                    
         DC    C'Main Supplier Contact Email|'                                  
         DC    C'ContactNameForInvQ|'                                           
         DC    C'ContactTelForInvQ|'                                            
         DC    C'ContactEmailForInvQ|'                                          
         DC    C'InvToName|'                                                    
         DC    C'InvToAdd1|'                                                    
         DC    C'InvToAdd2|'                                                    
         DC    C'InvToCity|'                                                    
         DC    C'InvToPostalCode|'                                              
         DC    C'InvToState|'                                                   
         DC    C'InvToCountry|'                                line 50          
         DC    C'BuyerContactName|'                                             
         DC    C'BuyerContactTel|'                                              
         DC    C'BuyerContactEmail|'                                            
         DC    C'SupplierFiscRepName|'                                          
         DC    C'SupplierFiscRepAdd1|'                                          
         DC    C'SupplierFiscRepAdd2|'                                          
         DC    C'SupplierFiscRepCity|'                                          
         DC    C'SupplierFiscRepPostalCode|'                                    
         DC    C'SupplierFiscRepState|'                                         
         DC    C'SupplierFiscRepCountry|'                                       
         DC    C'SupplierFiscRepTaxRegNum|'                                     
         DC    C'ShipToName|'                                                   
         DC    C'ShipToAdd1|'                                                   
         DC    C'ShipToAdd2|'                                                   
         DC    C'ShipToCity|'                                                   
         DC    C'ShipToPostalCode|'                                             
         DC    C'ShipToState|'                                                  
         DC    C'ShipToCountry|'                                                
         DC    C'ShipToTaxRegNum|'                                              
         DC    C'ShipFromName|'                                                 
         DC    C'ShipFromAdd1|'                                                 
         DC    C'ShipFromAdd2|'                                                 
         DC    C'ShipFromCity|'                                                 
         DC    C'ShipFromPostalCode|'                                           
         DC    C'ShipFromState|'                               line 75          
         DC    C'ShipFromCountry|'                                              
         DC    C'ShipFromTaxRegNum|'                                            
         DC    C'OrderedByName|'                                                
         DC    C'OrderedByAdd1|'                                                
         DC    C'OrderedByAdd2|'                                                
         DC    C'OrderedByCity|'                                                
         DC    C'OrderedByPostalCode|'                                          
         DC    C'OrderedByState|'                                               
         DC    C'OrderedByCountry|'                                             
         DC    C'InvoiceNetAmount|'                                             
         DC    C'InvoiceTaxAmount|'                                             
         DC    C'InvoiceGrossAmount|'                                           
         DC    C'Currency|'                                                     
         DC    C'LocalCurrency|'                                                
         DC    C'ExchangeRate|'                                                 
         DC    C'LocalCurrencyNet|'                                             
         DC    C'LocalCurrencyTax|'                                             
         DC    C'LocalCurrencyGross|'                                           
         DC    C'Bank Name|'                                                    
         DC    C'Bank Address|'                                                 
         DC    C'Sort Code|'                                                    
         DC    C'Bank Account Number|'                                          
         DC    C'Bank Account Name|'                                            
         DC    C'IBAN|'                                                         
         DC    C'SWIFT|'                                       line 100         
         DC    C'POLineNum|'                                                    
         DC    C'Quantity|'                                                     
         DC    C'UnitOfMeasure|'                                                
         DC    C'UnitPrice|'                                                    
         DC    C'LineNetAmount|'                                                
         DC    C'SupplierPartNum|'                                              
         DC    C'SupplierPartDescr|'                                            
         DC    C'BuyerPartNum|'                                                 
         DC    C'ManufacturerPartNum|'                                          
         DC    C'CommodityCode|'                                                
         DC    C'TaxDescriptor|'                                                
         DC    C'TaxCategoryCode1|'                                             
         DC    C'TaxRate1|'                                                     
         DC    C'TaxAmount1|'                                                   
         DC    C'TaxCategoryCode2|'                                             
         DC    C'TaxRate2|'                                                     
         DC    C'TaxAmount2|'                                                   
         DC    C'TaxCategoryCode3|'                                             
         DC    C'TaxRate3|'                                                     
         DC    C'TaxAmount3|'                                                   
         DC    C'TaxCategoryCode4|'                                             
         DC    C'TaxRate4|'                                                     
         DC    C'TaxAmount4|'                                                   
         DC    C'ShipmentNum|'                                                  
         DC    C'BuyerCostCenter|'                             line 125         
         DC    C'BillOfLading|'                                                 
         DC    C'ContractID|'                                                   
         DC    C'DUNSNumber|'                                                   
         DC    C'INCOTerms|'                                                    
         DC    C'WBS|'                                                          
         DC    C'NatureOfTax|'                                                  
         DC    C'PlaceOfIsuue|'                                                 
         DC    C'GLNumber|'                                                     
         DC    C'AccountNumber|'                                                
         DC    C'UtilityID|'                                                    
         DC    C'ESRRef|'                                                       
         DC    C'ESRNum|'                                                       
         DC    C'Weight|'                                                       
         DC    C'NetWeight|'                                                    
         DC    C'GrossWeight|'                                                  
         DC    C'NumberOfPacks|'                                                
         DC    C'ModeOfTransport|'                                              
         DC    C'ExpectedTimeOfArrival|'                                        
         DC    C'PortOfLoading|'                                                
         DC    C'PortOfDischarge|'                                              
         DC    C'ChargeCategory|'                                               
         DC    C'WithholdingTaxIndicator|'                                      
         DC    C'LicenseNum|'                                                   
         DC    C'CustomsDeclNum|'                                               
         DC    C'CustomsDeclOffice|'                           line 150         
         DC    C'CountryOfOrigin|'                                              
         DC    C'RailTruckNumber|'                                              
         DC    C'BatchNumber|'                                                  
         DC    C'BatchQuantity|'                                                
         DC    C'GoodsOrServiceIndicator|'                                      
         DC    C'Month|'                                                        
         DC    C'Week|'                                                         
         DC    C'Hours|'                                                        
         DC    C'Name|'                                                         
         DC    C'LocationCode|'                                                 
         DC    C'ApproverCode|'                                                 
         DC    C'Discount Description|'                                         
         DC    C'Discount Amount|'                                              
         DC    C'Discount Tax Category 1|'                                      
         DC    C'Discount Tax Rate 1|'                                          
         DC    C'Discount Tax Amount 1|'                                        
         DC    C'Discount Tax Category 2|'                                      
         DC    C'Discount Tax Rate 2|'                                          
         DC    C'Discount Tax Amount 2|'                                        
         DC    C'Discount Tax Category 3|'                                      
         DC    C'Discount Tax Rate 3|'                                          
         DC    C'Discount Tax Amount 3|'                                        
         DC    C'Discount Tax Category 4|'                                      
         DC    C'Discount Tax Rate 4|'                                          
         DC    C'Discount Tax Amount 4|'                       line 175         
         DC    C'Special Charge Description|'                                   
         DC    C'Special Charge Amount|'                                        
         DC    C'Special Charge Tax Category 1|'                                
         DC    C'Special Charge Tax Rate 1|'                                    
         DC    C'Special Charge Tax Amount 1|'                                  
         DC    C'Special Charge Tax Category 2|'                                
         DC    C'Special Charge Tax Rate 2|'                                    
         DC    C'Special Charge Tax Amount 2|'                                  
         DC    C'Special Charge Tax Category 3|'                                
         DC    C'Special Charge Tax Rate 3|'                                    
         DC    C'Special Charge Tax Amount 3|'                                  
         DC    C'Special Charge Tax Category 4|'                                
         DC    C'Special Charge Tax Rate 4|'                                    
         DC    C'Special Charge Tax Amount 4|'                                  
         DC    C'Carriage Description|'                                         
         DC    C'Carriage Amount|'                                              
         DC    C'Carriage Tax Category 1|'                                      
         DC    C'Carriage Tax Rate 1|'                                          
         DC    C'Carriage Tax Amount 1|'                                        
         DC    C'Carriage Tax Category 2|'                                      
         DC    C'Carriage Tax Rate 2|'                                          
         DC    C'Carriage Tax Amount 2|'                                        
         DC    C'Carriage Tax Category 3|'                                      
         DC    C'Carriage Tax Rate 3|'                                          
         DC    C'Carriage Tax Amount 3|'                       line 200         
         DC    C'Carriage Tax Category 4|'                                      
         DC    C'Carriage Tax Rate 4|'                                          
         DC    C'Carriage Tax Amount 4|'                                        
         DC    C'Freight Description|'                                          
         DC    C'Freight Amount|'                                               
         DC    C'Freight Tax Category 1|'                                       
         DC    C'Freight Tax Rate 1|'                                           
         DC    C'Freight Tax Amount 1|'                                         
         DC    C'Freight Tax Category 2|'                                       
         DC    C'Freight Tax Rate 2|'                                           
         DC    C'Freight Tax Amount 2|'                                         
         DC    C'Freight Tax Category 3|'                                       
         DC    C'Freight Tax Rate 3|'                                           
         DC    C'Freight Tax Amount 3|'                                         
         DC    C'Freight Tax Category 4|'                                       
         DC    C'Freight Tax Rate 4|'                                           
         DC    C'Freight Tax Amount 4|'                                         
         DC    C'Insurance Description|'                                        
         DC    C'Insurance Amount|'                                             
         DC    C'Insurance Tax Category 1|'                                     
         DC    C'Insurance Tax Rate 1|'                                         
         DC    C'Insurance Tax Amount 1|'                                       
         DC    C'Insurance Tax Category 2|'                                     
         DC    C'Insurance Tax Rate 2|'                                         
         DC    C'Insurance Tax Amount 2|'                      line 225         
         DC    C'Insurance Tax Category 3|'                                     
         DC    C'Insurance Tax Rate 3|'                                         
         DC    C'Insurance Tax Amount 3|'                                       
         DC    C'Insurance Tax Category 4|'                                     
         DC    C'Insurance Tax Rate 4|'                                         
         DC    C'Insurance Tax Amount 4|'                                       
         DC    C'Packing Description|'                                          
         DC    C'Packing Amount|'                                               
         DC    C'Packing Tax Category 1|'                                       
         DC    C'Packing Tax Rate 1|'                                           
         DC    C'Packing Tax Amount 1|'                                         
         DC    C'Packing Tax Category 2|'                                       
         DC    C'Packing Tax Rate 2|'                                           
         DC    C'Packing Tax Amount 2|'                                         
         DC    C'Packing Tax Category 3|'                                       
         DC    C'Packing Tax Rate 3|'                                           
         DC    C'Packing Tax Amount 3|'                                         
         DC    C'Packing Tax Category 4|'                                       
         DC    C'Packing Tax Rate 4|'                                           
         DC    C'Packing Tax Amount 4|'                                         
         DC    C'Admin Charge Description|'                                     
         DC    C'Admin Charge Amount|'                                          
         DC    C'Admin Charge Tax Category 1|'                                  
         DC    C'Admin Charge Tax Rate 1|'                                      
         DC    C'Admin Charge Tax Amount 1|'                   line 250         
         DC    C'Admin Charge Tax Category 2|'                                  
         DC    C'Admin Charge Tax Rate 2|'                                      
         DC    C'Admin Charge Tax Amount 2|'                                    
         DC    C'Admin Charge Tax Category 3|'                                  
         DC    C'Admin Charge Tax Rate 3|'                                      
         DC    C'Admin Charge Tax Amount 3|'                                    
         DC    C'Admin Charge Tax Category 4|'                                  
         DC    C'Admin Charge Tax Rate 4|'                                      
         DC    C'Admin Charge Tax Amount 4|'                                    
         DC    C'Fuel Surcharge Description|'                                   
         DC    C'Fuel Surcharge Amount|'                                        
         DC    C'Fuel Surcharge Tax Category 1|'                                
         DC    C'Fuel Surcharge Tax Rate 1|'                                    
         DC    C'Fuel Surcharge Tax Amount 1|'                                  
         DC    C'Fuel Surcharge Tax Category 2|'                                
         DC    C'Fuel Surcharge Tax Rate 2|'                                    
         DC    C'Fuel Surcharge Tax Amount 2|'                                  
         DC    C'Fuel Surcharge Tax Category 3|'                                
         DC    C'Fuel Surcharge Tax Rate 3|'                                    
         DC    C'Fuel Surcharge Tax Amount 3|'                                  
         DC    C'Fuel Surcharge Tax Category 4|'                                
         DC    C'Fuel Surcharge Tax Rate 4|'                                    
         DC    C'Fuel Surcharge Tax Amount 4|'                                  
         DC    C'Green Tax Description|'                                        
         DC    C'Green Tax Amount|'                            line 275         
         DC    C'Green Tax Tax Category 1|'                                     
         DC    C'Green Tax Tax Rate 1|'                                         
         DC    C'Green Tax Tax Amount 1|'                                       
         DC    C'Green Tax Tax Category 2|'                                     
         DC    C'Green Tax Tax Rate 2|'                                         
         DC    C'Green Tax Tax Amount 2|'                                       
         DC    C'Green Tax Tax Category 3|'                                     
         DC    C'Green Tax Tax Rate 3|'                                         
         DC    C'Green Tax Tax Amount 3|'                                       
         DC    C'Green Tax Tax Category 4|'                                     
         DC    C'Green Tax Tax Rate 4|'                                         
         DC    C'Green Tax Tax Amount 4|'                                       
         DC    C'Rounding Line Description|'                                    
         DC    C'Rounding Line Amount|'                                         
         DC    C'Rounding Line Tax Category 1|'                                 
         DC    C'Rounding Line Tax Rate 1|'                                     
         DC    C'Rounding Line Tax Amount 1|'                                   
         DC    C'Rounding Line Tax Category 2|'                                 
         DC    C'Rounding Line Tax Rate 2|'                                     
         DC    C'Rounding Line Tax Amount 2|'                                   
         DC    C'Rounding Line Tax Category 3|'                                 
         DC    C'Rounding Line Tax Rate 3|'                                     
         DC    C'Rounding Line Tax Amount 3|'                                   
         DC    C'Rounding Line Tax Category 4|'                                 
         DC    C'Rounding Line Tax Rate 4|'                    line 300         
         DC    C'Rounding Line Tax Amount 4|'                                   
         DC    C'Demurrage Description|'                                        
         DC    C'Demurrage Amount|'                                             
         DC    C'Demurrage Tax Category 1|'                                     
         DC    C'Demurrage Tax Rate 1|'                                         
         DC    C'Demurrage Tax Amount 1|'                                       
         DC    C'Demurrage Tax Category 2|'                                     
         DC    C'Demurrage Tax Rate 2|'                                         
         DC    C'Demurrage Tax Amount 2|'                                       
         DC    C'Demurrage Tax Category 3|'                                     
         DC    C'Demurrage Tax Rate 3|'                                         
         DC    C'Demurrage Tax Amount 3|'                                       
         DC    C'Demurrage Tax Category 4|'                                     
         DC    C'Demurrage Tax Rate 4|'                                         
         DC    C'Demurrage Tax Amount 4|'                                       
         DC    C'Invoice Detail1|'                                              
         DC    C'Invoice Detail2|'                                              
         DC    C'Invoice Detail3'                                               
*                                                                               
COLHDX   EQU   *                                                                
COLHDLEN EQU   *-COLHDSUL                                                       
*                                                                               
         TITLE 'TAPEWRT - WRITE RECORD TO TAPE'                                 
TAPEWRT  CSECT                                                                  
         NMOD1 0,TAPEWRT                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         CLI   SVQOPT7,C'P'        PRINT RECORD OUT?                            
         BNE   TW20                NO                                           
*                                                                               
         L     R6,AWIDEC                                                        
         USING WIDED,R6                                                         
*                                                                               
         L     R7,ABOX             A(BOX DEFINITIONS)                           
         USING BOXD,R7             BOX DEFINITIONS DSECT                        
*                                                                               
         L     R2,AOUTREC          R2 = A(OUTREC)                               
         LA    R3,OUTRECLN/L'P-1   MAX LINES TO PRINT                           
         CLC   BOXWIDTH,=F'198'    WIDE PRINTING?                               
         BNE   *+8                 NO                                           
         LA    R3,OUTRECLN/L'XP-1  MAX LINES TO PRINT                           
*                                                                               
TW10     MVC   P+1(L'P-1),0(R2)    MOVE TO PRINT LINE                           
         CLC   BOXWIDTH,=F'198'    WIDE PRINTING?                               
         BNE   *+10                NO                                           
         MVC   XP+1(L'XP-1),0(R2)  MOVE TO PRINT LINE                           
*                                                                               
         GOTO1 REPORT              PRINT THE RECORD                             
*                                                                               
         CLC   BOXWIDTH,=F'198'    WIDE PRINTING?                               
         BNE   TW15                NO                                           
         LA    R2,L'XP-1(R2)       BUMP TO NEXT LINE                            
         OC    0(L'XP-1,R2),0(R2)  ANY MORE DATA TO REPORT?                     
         B     TW16                                                             
*                                                                               
TW15     LA    R2,L'P-1(R2)        BUMP TO NEXT LINE                            
         OC    0(L'P-1,R2),0(R2)   ANY MORE DATA TO REPORT?                     
TW16     BZ    TW20                NO                                           
         BCT   R3,TW10             PROCESS NEXT PRINT LINE                      
         DROP  R6,R7               DROP USINGS                                  
*                                                                               
TW20     CLI   SVQOPT6,C'Y'        TEST RUN?                                    
         BE    TW30                YES - NO TAPE                                
*                                                                               
         L     R1,ASVLTAPE         R1 = TAPE                                    
         L     R0,AOUTREC          R0 = A(OUTREC)                               
         PUT   (1),(0)             ADD RECORD TO TAPE                           
*                                                                               
TW30     AP    TOTCNT,=P'1'        UPDATE RUNNING TOTAL OR TAPE RECS            
*                                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'MQMESSGE - SEND MQ MESSAGE WITH FILE'                           
MQMESSGE CSECT                                                                  
         NMOD1 0,MQMESSGE                                                       
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPVLWRKD,R8                                                      
*                                                                               
         CLI   MQMSG,C'N'          SUPPRESS MQ NOTIFICATION?                    
         BE    MQX                 YES                                          
*                                                                               
         LA    R5,ELEM             SEND MQ MESSAGE WITH FILE NAME               
         USING MQMSGD,R5           MQ MESSAGE DSECT                             
         MVI   ELEM,C' '           INIT ELEM TO SPACES                          
         MVC   ELEM+1(L'ELEM-1),ELEM  SPACE PAD                                 
         MVC   MQFILELG,FILENAME   FILENAME                                     
         MVC   MQDATE,CTODAY+2     YYMMDD                                       
         LA    RE,MQTIME           RE = MQTIME                                  
         MVC   0(2,RE),TIMEOFD     HH                                           
         MVC   2(2,RE),TIMEOFD+3   MM                                           
         MVC   4(2,RE),TIMEOFD+6   SS                                           
*                                                                               
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MSG & HDR                
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'EDIHUBSFTP******'),,0             
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         MVC   MQHID,=CL6'DANOT1'  HUB RECORD ID                                
         MVC   MQSYS,=C'SPT'       SPOT SYSTEM                                  
         CLI   NETOPT,C'N'         IS THIS NETPAK?                              
         BNE   *+10                NO                                           
         MVC   MQSYS,=C'NET'       SPOT SYSTEM                                  
*                                                                               
         MVC   MQQUAL,QUALIFY      QUALIFIER                                    
         MVC   MQAGYID,AGENCYID    AGENCY ID                                    
         DROP  R5                  DROP MQMSGD USING                            
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLQ2,0                          
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
MQX      XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
QOPT6    EQU   QGRP                DEFINE QOPT6                                 
QOPT7    EQU   QGRP+1              DEFINE QOPT7                                 
*                                                                               
NETBLKLN EQU   NBBLKEND-NETBLOCK   NETBLOCK LENGTH                              
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
BLINED   DSECT                     AUDITING REPORT DSECT FOR UNILEVER           
BLINE    DS    0CL132              132 CHAR PRINT LINE                          
         DS    CL4                                                              
BLMEDIA  DS    CL1                 MEDIA                                        
         DS    CL2                                                              
BLCLT    DS    CL3                 CLIENT                                       
         DS    CL1                                                              
BLCLNAME DS    CL20                CLIENT NAME                                  
         DS    CL1                                                              
BLPRD    DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
BLEST    DS    CL3                 ESTIMATE                                     
         DS    CL1                                                              
BLINVNO  DS    CL10                INVOICE NUMBER                               
         DS    CL1                                                              
BLINVMOS DS    CL6                 INVOICE MOS                                  
         DS    CL1                                                              
BLINVD   DS    CL10                INVOICE DATE                                 
         DS    CL1                                                              
BLGROSS  DS    CL14                GROSS                                        
         DS    CL1                                                              
BLSTA    DS    CL7                 STATION                                      
         DS    CL1                                                              
BLVAMT   DS    CL14                VENDOR AMOUNT                                
         ORG   BLSTA                                                            
BLTAX    DS    CL8                 INVOICE TAX                                  
         DS    CL1                                                              
BLACOM   DS    CL10                AGENCY COMMISSION                            
         ORG                                                                    
BKEYLENQ EQU   *-BLINE                                                          
*                                                                               
BBLINED  DSECT                     AUDITING REPORT DSECT FOR BELL               
BBLINE   DS    0CL132              132 CHAR PRINT LINE                          
         DS    CL4                                                              
BBLINVNO DS    CL10                INVOICE NUMBER                               
         DS    CL1                                                              
BBLINVD  DS    CL10                INVOICE DATE                                 
         DS    CL1                                                              
BBLCLT   DS    CL3                 CLIENT                                       
         DS    CL1                                                              
BBLPRD   DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
BBLEST   DS    CL3                 ESTIMATE                                     
         DS    CL1                                                              
BBLCNAME DS    CL20                CLIENT NAME                                  
         DS    CL1                                                              
BBLPGRP  DS    CL4                 PGRP                                         
         DS    CL1                                                              
BBLPGRNM DS    CL20                PRODUCT GROUP NAME                           
         DS    CL1                                                              
BBLIVMOS DS    CL6                 INVOICE MOS                                  
         DS    CL1                                                              
BBLMED   DS    CL10                MEDIA TYPE                                   
         DS    CL1                                                              
BBLUCOMM DS    CL12                EST UCOMM 1 (PROJECT CODE)                   
         DS    CL1                                                              
BBLUCOM3 DS    CL20                EST UCOMM 3 (CONTACT PIN)                    
         DS    CL1                                                              
BBLUCOM4 DS    CL32                EST UCOMM 4 (CONTACT NAME)                   
         DS    CL1                                                              
BBLGROSS DS    CL14                GROSS                                        
BBKYLENQ EQU   *-BBLINE                                                         
*                                                                               
BLFLINED DSECT                     FILE LINE DSECT FOR BELL                     
BTYPEA   DS    CL1                 RECORD TYPE A (INVOICE LINE)                 
BINVNUM  DS    CL10                INVOICE NUMBER                               
         DS    CL2                                                              
BINVTYPE DS    CL1                 INVOICE TYPE (F=REG, A=ADJ)                  
BCLIENT  DS    CL3                 CLIENT CODE                                  
         DS    CL3                                                              
BCLTNAME DS    CL20                CLIENT NAME                                  
         DS    CL20                                                             
BINVDATE DS    CL8                 INVOICE DATE YYYYMMDD                        
         DS    CL1                 INVOICE LANGUAGE (BLANK)                     
BINVCURR DS    CL3                 INVOICE CURRENCY (CAN)                       
         DS    CL40                REGION (BLANK)                               
BMEDCODE DS    CL1                 MEDIA CODE                                   
BESTNUM  DS    CL3                 ESTIMATE NUMBER                              
         DS    CL4                                                              
BESTNAME DS    CL20                ESTIMATE NAME                                
         DS    CL20                                                             
BEUCOMM  DS    CL4                 UCOMM (4 DIGIT PROJECT CODE)                 
         DS    CL21                                                             
BEUCOM3  DS    CL20                ESTIMATE UCOMM3 (EMPLOYEE NUMBER)            
BEUCOM4  DS    CL52                ESTIMATE UCOMM4 (EMPLOYEE NAME+NUM)          
         DS    CL8                                                              
BMEDMON  DS    CL8                 MEDIA MONTH (ALWAYS 1ST DAY OF MON)          
BINVAMT  DS    CL12                INVOICE AMOUNT (INCLUDING TAXES)             
BINVGST  DS    CL12                GST AMOUNT                                   
BGSTPCNT DS    CL8                 GST PERCENT                                  
         DS    CL20                FEDERAL TAX CODE (BLANK)                     
BINVQST  DS    CL12                QST AMOUNT                                   
BQSTPCNT DS    CL8                 QST PERCENT                                  
         DS    CL20                                                             
BPRDNAME DS    CL60                PRODUCT CODE + PRODUCT NAME                  
BPGRP    DS    CL3                 PRODUCT GROUP                                
BPGRPNAM DS    CL20                PRODUCT GROUP NAME                           
BLFLENQ  EQU   *-BTYPEA            LENGTH OF INVOICE FILE LINE                  
         ORG   BTYPEA                                                           
BTYPEB   DS    CL1                 RECORD TYPE B (VENDOR LINE)                  
BVINVNUM DS    CL10                INVOICE NUMBER                               
         DS    CL2                                                              
BVCLIENT DS    CL3                 CLIENT CODE                                  
         DS    CL3                                                              
BVMGNAME DS    CL24                MARKET GROUP NAME                            
         DS    CL6                                                              
BVMKTNAM DS    CL24                MARKET NAME                                  
         DS    CL6                                                              
BVSTANAM DS    CL20                VENDOR NAME                                  
         DS    CL10                                                             
BVAMT    DS    CL12                AMOUNT BEFORE TAXES                          
BVPAMT   DS    CL12                PREVIOUS AMOUNT (ALWAY 0)                    
         DS    CL12                                                             
BLVLENQ  EQU   *-BTYPEB            LENGTH OF VENDOR FILE LINE                   
*                                                                               
SPVLWRKD DSECT                     WORK AREA DSECT                              
*                                                                               
RCONS    DS    0F                  RELOCATED ADDRESSES                          
APRBILL  DS    A                   A(PRBILL)                                    
ADSTABUC DS    A                   A(STABUCKC)                                  
AFMTINO  DS    A                   A(SPFMTINO)                                  
APROCNET DS    A                   A(PROCNET)                                   
ANETBLK  DS    A                   A(NETBLK)                                    
ARNBILL  DS    A                   A(RNBILL)                                    
VSORTER  DS    A                   V(SORTER)                                    
VBINSRCH DS    A                   V(BINSRCH)                                   
VTAPEWRT DS    A                   V(TAPEWRT)                                   
VDDUCOM  DS    A                   V(DDUCOM)                                    
AWIDEC   DS    A                   V(WIDE)                                      
AMQMSG   DS    A                   A(MQMSG)                                     
*                                                                               
VBILLR   DS    A                   A(BILL ROUTINE)                              
VBILHR   DS    A                   A(BILL HEADER ROUTINE)                       
VUNITR   DS    A                   A(UNIT ROUTINE)                              
VOUTPR   DS    A                   A(OUTPUT ROUTINE)                            
*                                                                               
ASVLTAPE DS    A                   A(SVLTAPE)                                   
AMQRPT   DS    A                   A(MQRPT)                                     
*                                                                               
ELCODE   DS    CL1                 ELEMENT CODE FOR GETEL                       
ELCODE1  DS    CL1                 ELEMENT CODE FOR GETEL1                      
ELCODE2  DS    CL1                 ELEMENT CODE FOR GETEL2                      
NETOPT   DS    CL1                 "N" IF NETPAK                                
MYSTART  DS    CL6                 ORIGINAL QSTART FROM REQF                    
MYEND    DS    CL6                 ORIGINAL QEND FROM REQF                      
MYSTRTB  DS    CL3                 ORIGINAL QSTART FROM REQF IN BINARY          
MYENDB   DS    CL3                 ORIGINAL QEND FROM REQF IN BINARY            
MYWORK   DS    XL100               WORK AREA                                    
CKESTREC DS    CL1                 RECORD TYPE TO GET ESTIMATE FROM             
SAVEPRD  DS    CL3                 SAVED 3-CHAR PRODUCT                         
SAVEEST  DS    XL1                 SAVED BINARY ESTIMATE                        
SVINV    DS    XL2                 SAVED INVOICE NUMBER                         
SAVEKEY  DS    CL32                SAVED KEY                                    
SAVEREC  DS    A                   SAVED AREC                                   
BINKEY   DS    CL(ULBKEYL)         BINSRCH KEY                                  
BINREC   DS    CL(ULBRECL)         BINSRCH RECORD DATA                          
BINKEYBL DS    CL(BLBKEYL)         BINSRCH KEY                                  
BINRECBL DS    CL(BLBRECL)         BINSRCH RECORD DATA                          
BINKEYOU DS    CL(OUBKEYL)         BINSRCH KEY                                  
BINRECOU DS    CL(OUBRECL)         BINSRCH RECORD DATA                          
BINKEYGN DS    CL(GEBKEYL)         BINSRCH KEY                                  
BINRECGN DS    CL(GEBRECL)         BINSRCH RECORD DATA                          
*                                                                               
INVCNT   DS    PL4'0'              INVOICE HEADER COUNT                         
SORTCNT  DS    PL4'0'              RECORDS READ FROM SORTER                     
HDRCNT   DS    PL4'0'              HEADER RECORDS                               
TOTCNT   DS    PL4'0'              TOTAL FILE RECORDS                           
*                                                                               
VENCOUNT DS    PL4'0'              VENDOR LINE COUNT                            
*                                                                               
BILTOT   DS    D                   BILLING AMOUNT $ TOTAL                       
DTLTOT   DS    D                   SUM OF BILLING ON DETAIL RECS                
*                                                                               
PAK6     DS    PL6                 FOR PACKED CALCULATIONS                      
*                                                                               
B1PROF   DS    XL16                B1 PROFILE                                   
B1XPROF  DS    XL16                B1X PROFILE                                  
SVQOPT1  DS    CL1                 SAVED QOPT1                                  
SVQOPT6  DS    CL1                 SAVED QOPT6                                  
SVQOPT7  DS    CL1                 SAVED QOPT7                                  
CKESTSW  DS    XL1                 ESTIMATE READ FLAG                           
CKPRDSW  DS    XL1                 PRODUCT READ FLAG                            
READSW   DS    XL1                 STATION NAME READ FLAG                       
FRSTSW   DS    X                   FIRST TIME SWITCH                            
OPENSW   DS    X                   FIRST TIME SWITCH                            
SORTREC  DS    XL255               SORT REC STA BUCKET ELEM/UNIT LEVEL          
*                                                                               
SORTKYSV DS    XL(L'SORTKEY)       FOR NET WE'LL DO OUR OWN ROLL-UP             
SORTRCSV DS    XL(SORTRLEN)        SAVES THE LAST SORT RECORD FOR NET           
SAVEBGRS DS    XL4                 RUNNING TOTAL OF BILLED GROSS                
SAVEBNET DS    XL4                 RUNNING TOTAL OF BILLED NET                  
SAVEBGTX DS    XL4                 RUNNING TOTAL OF BILLED TAX GROSS            
SAVEBNTX DS    XL4                 RUNNING TOTAL OF BILLED TAX NET              
SAVEBCS2 DS    XL4                 RUNNING TOTAL OF BILLED COST2                
*                                                                               
SRTKSVBL DS    XL(SRTBKYL1)        SORT KEY SAVE FOR BELL                       
*                                                                               
SRTKSVOU DS    XL(SRTOKYL1)        SORT KEY SAVE FOR PHD                        
*                                                                               
PASS     DS    CL1                 PASS 1/PASS 2                                
*                                                                               
SRTKSVGE DS    XL(SRTGKYL1)        SORT KEY SAVE FOR GENERIC EDI                
SRTKSVGN DS    XL(SRTGKEYL)        FOR NET WE'LL DO OUR OWN ROLL-UP             
SRTRCSGN DS    XL(SRTGRLEN)        SAVES THE LAST SORT RECORD FOR NET           
*                                                                               
SVCLTKEY DS    0XL(SVCLTKYL)       CLIENT KEY LENGTH                            
SVCLTAM  DS    XL1                 A/M                                          
SVCLTCLT DS    XL2                 PACKED CLIENT                                
SVCLTKYL EQU   *-SVCLTAM           CLIENT KEY LENGTH                            
*                                                                               
SVPRDKEY DS    0XL(SVPRDKYL)       PRODUCT KEY LENGTH                           
SVPRDAM  DS    XL1                 A/M                                          
SVPRDCLT DS    XL2                 PACKED CLIENT                                
SVPRDPRD DS    CL3                 PRODUCT CODE                                 
SVPRDKYL EQU   *-SVPRDAM           PRODUCT KEY LENGTH                           
*                                                                               
SVESTKEY DS    0XL(SVESTKYL)       ESTIMATE KEY LENGTH                          
SVESTAM  DS    XL1                 A/M                                          
SVESTCLT DS    XL2                 PACKED CLIENT                                
SVESTPRD DS    CL3                 PRODUCT CODE                                 
SVESTEST DS    CL1                 ESTIMATE CODE                                
SVESTKYL EQU   *-SVESTAM           ESTIMATE KEY LENGTH                          
*                                                                               
SVMKTKEY DS    0XL(SVMKTKYL)       MARKET KEY LENGTH                            
SVMKTMED DS    XL1                 MEDIA                                        
SVMKTMRK DS    XL4                 MARKET CODE                                  
SVMKTAGY DS    CL2                 AGENCY CODE                                  
SVMKTKYL EQU   *-SVMKTMED          MARKET KEY LENGTH                            
*                                                                               
SVMGPKEY DS    0XL(SVMGPKYL)       MARKET GROUP KEY LENGTH                      
SVMGPAM  DS    XL1                 AGENCY/MEDIA                                 
SVMGPCLT DS    CL2                 CLIENT CODE                                  
SVMGPPID DS    CL1                 PRODUCT GROUP ID                             
SVMGPPNM DS    CL2                 PRODUCT GROUP NUMBER                         
SVMGPMRK DS    XL2                 MARKET CODE                                  
SVMGPKYL EQU   *-SVMGPAM           MARKET GROUP KEY LENGTH                      
*                                                                               
PMGRKEY DS    0XL(PMGRKYL)        MARKET GROUP KEY LENGTH                       
PMGRAM  DS    XL1                 AGENCY/MEDIA                                  
PMGRCLT DS    CL2                 CLIENT CODE                                   
PMGRPGR DS    XL3                 PRODUCT GROUP                                 
PMGRMKT DS    XL2                 MARKET CODE                                   
PMGRKYL EQU   *-PMGRAM            MARKET GROUP KEY LENGTH                       
*                                                                               
SVADDKEY DS   0XL(SVADDKYL)       STATION ADDRESS KEY LENGTH                    
SVADDMED DS    XL1                 MEDIA                                        
SVADDSTA DS    XL5                 STATION                                      
SVADDAGY DS    CL2                 AGENCY CODE                                  
SVADDKYL EQU   *-SVADDMED          STATION ADDRESS KEY LENGTH                   
*                                                                               
CANADIAN DS    CL1                 FLAG CANADIAN AGENCY                         
FIRSTINV DS    CL1                 FIRST TIME PROCESSING INVOICE FLAG           
ONLYINVC DS    CL1                 ONLY HAVE INVOICE INFO                       
*                                                                               
NETFLAGS DS    XL1                 NETWORK FLAGS                                
FIRSTNET EQU   X'80'               PROCESSED FIRST NET REC FROM SORTER          
THISNET  EQU   X'40'               JUST PROCESSED NET REC FROM SORTER           
LASTNET  EQU   X'20'               PROCESSED LAST NET REC FROM SORTER           
*                                                                               
SVUCPKEY DS    CL7                 UCOMM AGY/MED, CLT, PRD                      
SVPUCOM1 DS    CL32                SAVED 1ST PRODUCT UCOMM                      
SVPUCOM2 DS    CL32                SAVED 2ND PRODUCT UCOMM                      
SVPUCOM3 DS    CL32                SAVED 3RD PRODUCT UCOMM                      
SVPUCOM4 DS    CL32                SAVED 4TH PRODUCT UCOMM                      
*                                                                               
SVUCKEY  DS    CL8                 UCOMM AGY/MED, CLT, PRD, EST                 
SVEUCOM1 DS    CL32                SAVED 1ST ESTIMATE UCOMM                     
SVEUCOM2 DS    CL32                SAVED 2ND ESTIMATE UCOMM                     
SVEUCOM3 DS    CL32                SAVED 3RD ESTIMATE UCOMM                     
SVEUCOM4 DS    CL32                SAVED 4TH ESTIMATE UCOMM                     
SVEUCOM5 DS    CL32                SAVED 5TH ESTIMATE UCOMM                     
SVEUCOM6 DS    CL32                SAVED 6TH ESTIMATE UCOMM                     
SVEUCOM7 DS    CL32                SAVED 7TH ESTIMATE UCOMM                     
SVEUCOM8 DS    CL32                SAVED 8TH ESTIMATE UCOMM                     
*                                                                               
SVUCMKEY DS    CL8                 UCOMM AGY/MED, CLT, PRD, EST                 
SVUCMKT  DS    CL2                 MARKET                                       
SVMUCOM1 DS    CL32                SAVED 1ST MARKET UCOMM                       
SVMUCOM2 DS    CL32                SAVED 2ND MARKET UCOMM                       
SVMUCOM3 DS    CL32                SAVED 3RD MARKET UCOMM                       
SVMUCOM4 DS    CL32                SAVED 4TH MARKET UCOMM                       
*                                                                               
SVPGRAM  DS    XL1                 SAVED PRODUCT GROUP AGENCY/MEDIA             
SVPGRCLT DS    XL2                 SAVED PRODUCT GROUP CLIENT                   
SVPGRPRD DS    CL3                 SAVED PRODUCT GROUP PRODUCT                  
SVPGRP   DS    CL4                 SAVED PRODUCT GROUP                          
SVPGRPP  DS    CL3                 SAVED PRODUCT GROUP ID & PACKED NUM          
SVPGRPN  DS    CL24                SAVED PRODUCT GROUP NAME                     
SVPGRPN2 DS    CL24                SAVED PRODUCT GROUP NAME 2                   
*                                                                               
SVMKTNM  DS    CL24                SAVED MARKET NAME                            
SVMKTAM  DS    XL1                 SAVED MARKET A/M                             
SVMKTMKT DS    XL2                 SAVED MARKET                                 
*                                                                               
SVMGRBIN DS    CL2                 SAVED BINARY MARKET GROUP NUMBER             
SVMGROUP DS    CL6                 UP TO 6 CHARACTER MARKET GROUP               
SVMGRPN  DS    CL24                SAVED MARKET GROUP NAME                      
SVMGRPN2 DS    CL24                SAVED MARKET GROUP NAME 2                    
SVMGRPN3 DS    CL24                SAVED MARKET GROUP NAME 3                    
SVMGRAM  DS    XL1                 SAVED MARKET GROUP A/M                       
SVMGRCLT DS    XL2                 SAVED MARKET GROUP CLIENT                    
SVMGRMKT DS    XL2                 SAVED MARKET GROUP MARKET                    
*                                                                               
THISSTA  DS    CL7                 STATION TO LOOK UP IN GETSNAME               
VENDORNM DS    CL24                VENDOR (STATION OR NETWORK) NAME             
SVLINE   DS    X                   SAVED LINE NUMBER                            
SVFORCEH DS    X                   SAVED FORCEHED                               
INDENT   DS    X                   INDENTATION FOR XML PRINTING                 
CLOSETAG DS    C                   INDICATE IF WE JUST CLOSED XML TAG           
SAVEAGY  DS    CL2                 SAVED AGENCY CODE                            
*                                                                               
CTODAY   DS    CL8                 YYYYMMDD                                     
TIMEOFD  DS    CL8                 HH.MM.SS                                     
TIMEOFDU DS    CL8                 HH.MM.SS UTC                                 
*                                                                               
NOFILE   DS    CL1                 SET ON ERROR AND SUPPRESS FILE               
*                                                                               
*                                  BINSRCH PARMS FOR BUFFERING BILL REC         
*                                                                               
ABINKEY  DS    F                   A(BINSRCH KEY)                               
ABINREC  DS    F                   A(BINSRCH REC)                               
ABINRECN DS    F                   NUMBER OF RECORDS IN ABINREC                 
ABINRECL DS    F                   RECORD LENGTH (INCLUDING KEY)                
ABINKEYL DS    F                   KEY LENGTH                                   
ABINMAXR DS    F                   MAX NUMBER OF RECORDS                        
*                                                                               
UCOMBLK  DS    CL(UCOMDLNQ)        DDUCOM CONTROL BLOCK                         
*                                                                               
PHDSFLDB DS    CL(PHDSFLDQ)        PHD SPECIAL FIELDS BLOCK                     
*                                                                               
FILNMLEN DS    XL1                 FILE NAME LENGTH                             
FILENAME DS    CL128               FILE NAME                                    
MQMSG    DS    CL1                 SUPPRESS MQ MESSAGE IF C'Y'                  
ELEM     DS    XL(MQMSGLQ2)        BUILD MQ MESSAGE HERE                        
QUALIFY  DS    CL16                QUALIFIER FOR MQ MESSAGE                     
AGENCYID DS    CL4                 AGENCY ID FOR MQ MESSAGE                     
*                                                                               
NETBILLB DS    XL(NBLLENQ)         FOR NBLBLOCK                                 
*                                                                               
ELEM2    DS    XL(NUBILLN1)        BUILD FAKE NET BILLING ELEMENT               
*                                                                               
       ++INCLUDE SPBVALD                                                        
*                                                                               
AOUTREC  DS    A                   A(OUTREC)                                    
OUTRECLN EQU   200000              OUTREC LENGTH = 160K                         
*                                                                               
AGYTABD  DSECT                     AGENCY TABLE DSECT                           
AGYTAGY  DS    CL2                 AGENCY CODE                                  
AGYTAPE  DS    C                   TAPE CODE (C' ' = NONE)                      
AGYSUFX  DS    C                   SUFFIX IF TAPE CODE IS NOT BLANK             
AGYSSPEC DS    X                   RCSUBPRG FOR SPOT                            
AGYNSPEC DS    X                   RCSUBPRG FOR NET                             
AGYBLKS  DS    AL2                 DCB/TAPE - BLKSIZE=                          
AGYLRCL  DS    AL2                 DCB/TAPE - LRECL=                            
AGYRECFM DS    C                   DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
AGYSKEYL DS    AL2                 SORTER - SRTCDLEN                            
AGYSRECL DS    AL2                 SORTER - RECCDLEN                            
AGYBKEYL DS    F                   BINSRCH - KEY LENGTH                         
AGYBRECL DS    F                   BINSRCH - REC LENGTH                         
AGYBMAXR DS    F                   BINSRCH - MAX NUMBER OF RECORDS              
ABILLR   DS    A                   A(BILL ROUTINE)                              
ABILHR   DS    A                   A(BILL HEADER ROUTINE)                       
AUNITR   DS    A                   A(UNIT ROUTINE)                              
AOUTPR   DS    A                   A(OUTPUT ROUTINE)                            
AGYTABL  EQU   *-AGYTABD           LENGTH OF TABLE ENTRY                        
*                                                                               
ULBKEYD  DSECT                     BILLING SORT KEY DSECT                       
ULBKEY   DS    0XL(ULBKEYL)        KEY OF SORT RECORD                           
ULBAM    DS    CL1                 AGENCY/MEDIA                                 
ULBCLT   DS    CL2                 CLIENT CODE                                  
ULBPRD   DS    CL3                 PRODUCT CODE                                 
ULBEST   DS    XL1                 ESTIMATE CODE                                
ULBINVYM DS    XL2                 INVOICE YYMMDD COMPRESSED                    
ULBINVN  DS    XL2                 INVOICE NUMBER                               
ULBKEYL  EQU   *-ULBAM             KEY LENGTH                                   
*                                                                               
ULBRECD  DSECT                     BILL SORT RECORD DSECT                       
ULBREC   DS    0XL(ULBRECL)        BILL RECORD DATA                             
ULBGROSS DS    PL6                 GROSS AMOUNT DUE                             
ULBNET   DS    PL6                 NET AMOUNT DUE                               
ULBINV   DS    CL10                FULL INVOICE NUMBER AS ON BILL               
ULBINVD  DS    CL10                INVOICE DATE AS ON BILL                      
ULBINVDD DS    CL10                INVOICE DUE DATE AS ON BILL                  
ULBEUDF2 DS    CL16                ESTIMATE UDEF 2                              
ULBPNAME DS    CL20                PRODUCT NAME                                 
ULBENAME DS    CL20                ESTIMATE NAME                                
ULBNMED  DS    CL1                 NETWORK MEDIA (FROM BILL HEADER)             
ULBRECL  EQU   *-ULBGROSS          SORT RECORD LENGTH                           
ULBRECL2 EQU   ULBRECL+ULBKEYL     SORT KEY+RECORD LENGTH                       
*                                                                               
SORTRECD DSECT                     SECONDARY SORT FROM STA BUCKET/UNIT          
SORTKEY  DS    0XL(SORTKEYL)       SECONDARY SORT KEY                           
SORTRCRD DS    0XL(SORTRLEN)                                                    
SORTAM   DS    CL1                 AGENCY/MEDIA                                 
SORTCLT  DS    CL3                 3-CHARACTER CLIENT CODE                      
SORTPRD  DS    CL3                 3-CHARACTER PRODUCT CODE                     
SORTEST  DS    XL1                 ESTIMATE                                     
SORTINV  DS    XL2                 INVOICE NUMBER                               
SORTBDAT DS    XL3                 BILLED DATE                                  
SORTSTA  DS    CL7                 FULL STATION/VENDOR CODE                     
SORTKMOS DS    XL3                 MOS FOR MEDIA N REQUESTS                     
SORTKEYL EQU   *-SORTAM            KEY LENGTH                                   
*                                                                               
*  NEXT SEVERAL FIELDS FROM BINSRCH INVOICE DATA                                
*                                                                               
SORTIGRS DS    PL6                 GROSS AMOUNT DUE                             
SORTINET DS    PL6                 NET AMOUNT DUE                               
SORTIINV DS    CL10                FULL INVOICE NUMBER AS ON BILL               
SORTIDAT DS    CL10                INVOICE DATE AS ON BILL                      
SORTIDDT DS    CL10                INVOICE DUE DATE AS ON BILL                  
SORTUDF2 DS    CL16                ESTIMATE UDEF 2                              
SORTPNAM DS    CL20                PRODUCT NAME                                 
SORTENAM DS    CL20                ESTIMATE NAME                                
SORTNMED DS    CL1                 NETWORK MEDIA (FROM BILL HEADER)             
*                                                                               
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
*                                                                               
SORTMEDN DS    CL13                MEDIA NAME                                   
SORTCNAM DS    CL20                CLIENT NAME                                  
SORTMOS  DS    CL6                 MOS (MMM/YY)                                 
SORTSNAM DS    CL24                STATION OR NETWORK NAME                      
SORTBNET DS    XL4                 VENDOR NET AMOUNT                            
SORTRLEN EQU   *-SORTAM                                                         
***                                                                             
* DSECTS FOR MEDIA EXPERTS CLIENT BELL (AGENCY U# M2TOA) SPEC-14682             
***                                                                             
BLBKEYD  DSECT                     BILLING SORT KEY DSECT                       
BLBKEY   DS    0XL(BLBKEYL)        KEY OF SORT RECORD                           
BLBAM    DS    CL1                 AGENCY/MEDIA                                 
BLBCLT   DS    CL2                 CLIENT CODE                                  
BLBPRD   DS    CL3                 PRODUCT CODE                                 
BLBEST   DS    XL1                 ESTIMATE CODE                                
BLBINVYM DS    XL2                 INVOICE YYMMDD COMPRESSED                    
BLBINVN  DS    XL2                 INVOICE NUMBER                               
BLBKEYL  EQU   *-BLBAM             KEY LENGTH                                   
*                                                                               
BLBRECD  DSECT                     BILL SORT RECORD DSECT                       
BLBREC   DS    0XL(BLBRECL)        BILL RECORD DATA                             
BLBNET   DS    PL6                 NET AMOUNT DUE                               
BLBGST   DS    PL6                 GST AMOUNT DUE                               
BLBPST   DS    PL6                 PST AMOUNT DUE                               
BLBTYPE  DS    CL2                 BILL TYPE (B1,B2,B3,B4,B5,ETC)               
BLBINV   DS    CL10                FULL INVOICE NUMBER AS ON BILL               
BLBINVD  DS    CL10                INVOICE DATE AS ON BILL                      
BLBEUCM4 DS    CL32                ESTIMATE UCOMM 4                             
BLBEUCM3 DS    CL20                ESTIMATE UCOMM 3                             
BLBEUCOM DS    CL4                 ESTIMATE UCOMM                               
BLBPNAME DS    CL20                PRODUCT NAME                                 
BLBPGRP  DS    CL3                 PRODUCT GROUP                                
BLBPGRPN DS    CL20                PRODUCT GROUP NAME                           
BLBENAME DS    CL20                ESTIMATE NAME                                
BLBRECL  EQU   *-BLBNET            SORT RECORD LENGTH                           
BLBRECL2 EQU   BLBRECL+BLBKEYL     SORT KEY+RECORD LENGTH                       
*                                                                               
SRTBRECD DSECT                     SECONDARY SORT FROM STA BUCKET               
SRTBKEY  DS    0XL(SRTBKEYL)       SECONDARY SORT KEY                           
SRTBRCRD DS    0XL(SRTBRLEN)                                                    
SRTBAM   DS    CL1                 AGENCY/MEDIA                                 
SRTBCLT  DS    CL3                 3-CHARACTER CLIENT CODE                      
SRTBPRD  DS    CL3                 3-CHARACTER PRODUCT CODE                     
SRTBEST  DS    XL1                 ESTIMATE                                     
SRTBINV  DS    XL2                 INVOICE NUMBER                               
SRTBKMOS DS    XL3                 MOS                                          
SRTBBDAT DS    XL3                 BILLED DATE                                  
SRTBKYL1 EQU   *-SRTBAM            KEY LENGTH 1 FOR REPORTING                   
SRTBMSTA DS    CL5                 PACKED MARKET/STATION                        
SRTBSTA  DS    CL7                 VENDOR/STATION                               
SRTBKEYL EQU   *-SRTBAM            KEY LENGTH                                   
*                                                                               
*  NEXT SEVERAL FIELDS FROM BINSRCH INVOICE DATA                                
*                                                                               
SRTBINET DS    PL6                 NET AMOUNT DUE                               
SRTBIGST DS    PL6                 GST AMOUNT DUE                               
SRTBIPST DS    PL6                 PST AMOUNT DUE                               
SRTBTYPE DS    CL2                 BILL TYPE (B1,B2,B3,B4,B5,ETC)               
SRTBIINV DS    CL10                FULL INVOICE NUMBER AS ON BILL               
SRTBIDAT DS    CL10                INVOICE DATE AS ON BILL                      
SRTBUCM4 DS    CL32                ESTIMATE UCOMM 4                             
SRTBUCM3 DS    CL20                ESTIMATE UCOMM 3                             
SRTBUCOM DS    CL4                 ESTIMATE UCOMM 1                             
SRTBCNAM DS    CL20                CLIENT NAME                                  
SRTBPNAM DS    CL20                PRODUCT NAME                                 
SRTBPGRP DS    CL3                 PRODUCT GROUP                                
SRTBPGNM DS    CL20                PRODUCT GROUP NAME                           
SRTBENAM DS    CL20                ESTIMATE NAME                                
*                                                                               
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
*                                                                               
SRTBMED  DS    CL1                 MEDIA CODE                                   
SRTBMEDN DS    CL10                MEDIA NAME                                   
SRTBMOSP DS    XL3                 MOS YMD (BINARY)                             
SRTBMOS  DS    CL6                 MOS (MMM/YY)                                 
SRTBMGNM DS    CL24                MARKET GROUP NAME                            
SRTBMNAM DS    CL24                MARKET NAME                                  
SRTBSNAM DS    CL20                STATION NAME                                 
SRTBBNET DS    XL4                 VENDOR NET AMOUNT                            
SRTBRLEN EQU   *-SRTBAM                                                         
***                                                                             
* DSECTS FOR PHDTO (AGENCY OU) SPEC-24094                                       
***                                                                             
OUBKEYD  DSECT                     BILLING SORT KEY DSECT                       
OUBKEY   DS    0XL(OUBKEYL)        KEY OF SORT RECORD                           
OUBAM    DS    CL1                 AGENCY/MEDIA                                 
OUBCLT   DS    CL2                 CLIENT CODE                                  
OUBPRD   DS    CL3                 PRODUCT CODE                                 
OUBEST   DS    XL1                 ESTIMATE CODE                                
OUBINVYM DS    XL2                 INVOICE YYMMDD COMPRESSED                    
OUBINVN  DS    XL2                 INVOICE NUMBER                               
OUBKEYL  EQU   *-OUBAM             KEY LENGTH                                   
*                                                                               
OUBRECD  DSECT                     BILL SORT RECORD DSECT                       
OUBREC   DS    0XL(OUBRECL)        BILL RECORD DATA                             
OUBNET   DS    PL6                 NET AMOUNT DUE                               
OUBGST   DS    PL6                 GST AMOUNT DUE                               
OUBPST   DS    PL6                 PST AMOUNT DUE                               
OUBCOM   DS    XL4                 AGENCY COMMISSION AMOUNT                     
OUBPROV  DS    CL1                 PROVINCE                                     
OUBINV   DS    CL10                FULL INVOICE NUMBER AS ON BILL               
OUBINVD  DS    CL10                INVOICE DATE AS ON BILL                      
OUEUDEF1 DS    CL32                ESTIMATE UDEF 1                              
OUEUDEF2 DS    CL16                ESTIMATE UDEF 2                              
OUBRECL  EQU   *-OUBNET            SORT RECORD LENGTH                           
OUBRECL2 EQU   OUBRECL+OUBKEYL     SORT KEY+RECORD LENGTH                       
*                                                                               
SRTORECD DSECT                     SECONDARY SORT FROM STA BUCKET               
SRTOKEY  DS    0XL(SRTOKEYL)       SECONDARY SORT KEY                           
SRTORCRD DS    0XL(SRTORLEN)       SECONDARY SORT RECORD LENGTH                 
SRTOAM   DS    CL1                 AGENCY/MEDIA                                 
SRTOCLT  DS    CL3                 3-CHARACTER CLIENT CODE                      
SRTOPRD  DS    CL3                 3-CHARACTER PRODUCT CODE                     
SRTOEST  DS    XL1                 ESTIMATE                                     
SRTOINV  DS    XL2                 INVOICE NUMBER                               
SRTOKMOS DS    XL3                 MOS (NOT USED - COLLAPSE MULTI-MOS)          
SRTOBDAT DS    XL3                 BILLED DATE                                  
SRTOKYL1 EQU   *-SRTOAM            KEY LENGTH 1 FOR REPORTING                   
SRTOMSTA DS    CL5                 PACKED MARKET/STATION                        
SRTOSTA  DS    CL7                 VENDOR/STATION                               
SRTOKEYL EQU   *-SRTOAM            KEY LENGTH                                   
*                                                                               
*  NEXT SEVERAL FIELDS FROM BINSRCH INVOICE DATA                                
*                                                                               
SRTOINET DS    PL6                 NET AMOUNT DUE                               
SRTOIGST DS    PL6                 GST AMOUNT DUE                               
SRTOIPST DS    PL6                 PST AMOUNT DUE                               
SRTOPROV DS    CL1                 PROVINCE                                     
SRTOIINV DS    CL10                FULL INVOICE NUMBER AS ON BILL               
SRTOIDAT DS    CL10                INVOICE DATE AS ON BILL                      
SRTOEUD1 DS    CL32                ESTIMATE UDEF 1                              
SRTOEUD2 DS    CL16                ESTIMATE UDEF 2                              
SRTOCNAM DS    CL20                CLIENT NAME                                  
SRTOACOM DS    XL4                 AGENCY COMMISSION AMOUNT                     
*                                                                               
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
*                                                                               
SRTOMED  DS    CL1                 MEDIA CODE                                   
SRTOMOSP DS    XL3                 MOS YMD (BINARY)                             
SRTOMOS  DS    CL6                 MOS (MMM/YY)                                 
SRTOSNAM DS    CL20                STATION NAME                                 
SRTOBNET DS    XL4                 VENDOR NET AMOUNT                            
SRTORLEN EQU   *-SRTOAM                                                         
***                                                                             
* DSECTS FOR GENERIC EDI SPSUG-1667                                             
***                                                                             
GEBKEYD  DSECT                     BILLING SORT KEY DSECT                       
GEBKEY   DS    0XL(GEBKEYL)        KEY OF SORT RECORD                           
GEBAM    DS    CL1                 AGENCY/MEDIA                                 
GEBCLT   DS    CL2                 CLIENT CODE                                  
GEBPRD   DS    CL3                 PRODUCT CODE                                 
GEBEST   DS    XL1                 ESTIMATE CODE                                
GEBINVYM DS    XL2                 BILL RUN DATE (BDATE) YYMMDD                 
GEBIMOS  DS    XL2                 MOS                                          
GEBINVN  DS    XL2                 INVOICE NUMBER                               
GEBKEYL  EQU   *-GEBAM             KEY LENGTH                                   
*                                                                               
GEBRECD  DSECT                     BILL SORT RECORD DSECT                       
GEBREC   DS    0XL(GEBRECL)        BILL RECORD DATA                             
GEBGROSS DS    PL6                 GROSS                                        
GEBNET   DS    PL6                 NET                                          
GEBACT   DS    PL6                 ACTUAL                                       
GEBCNET  DS    XL4                 CALCULATED NET (BCLDNET)                     
GEBTAX   DS    XL4                 TAX                                          
GEBGST   DS    XL4                 GST                                          
GEBPST   DS    XL4                 PST                                          
GEBHST   DS    XL4                 HST                                          
GEBPROV  DS    XL1                 PROVINCE                                     
GEBTYPE  DS    CL4                 BILL TYPE (B1,B2,B3,B4,B5,ETC)               
GEBINV   DS    CL10                FULL INVOICE NUMBER AS ON BILL               
GEBINVD  DS    CL10                INVOICE RUN DATE                             
GEBINVDD DS    CL10                INVOICE DUE DATE                             
GEBQMED  DS    CL1                 MEDIA CODE                                   
GEBNMED  DS    CL1                 NETWORK SUB-MEDIA                            
GEBCLT3  DS    CL3                 CLIENT CODE UNPACKED (HONORS AAN)            
GEBSTAT  DS    XL1                 STATUS BYTE                                  
GEBSTAT2 DS    XL1                 STATUS BYTE 2                                
GEBSTAT3 DS    XL1                 STATUS BYTE 3                                
GEBRETFL DS    XL1                 BRETAIL STATUS BITS                          
GEBRETAC DS    CL12                BRETACCT ACCOUNT CODE                        
GEBWBFLT DS    CL10                WB FLIGHT CODE                               
GEBCTYPE DS    CL1                 COST TYPE (NET ONLY)                         
GEBPKGNM DS    CL5                 PACKAGE NAME (NET ONLY)                      
GEBDPTFL DS    CL1                 DAYPART FILTER (NET ONLY)                    
GEBPKGNO DS    CL1                 PACKAGE NUMBER (NET ONLY)                    
GEBIFLAG DS    XL1                 INTERNAL FLAG                                
GEBIFDUP EQU   X'80'               THIS IS A DUPLICATE                          
GEBIFVEN EQU   X'40'               THIS INVOICE HAS VENDOR DATA                 
GEBRECL  EQU   *-GEBGROSS          SORT RECORD LENGTH                           
GEBRECL2 EQU   GEBRECL+GEBKEYL     SORT KEY+RECORD LENGTH                       
*                                                                               
SRTGRECD DSECT                     SECONDARY SORT FROM STA BUCKET/UNIT          
SRTGKEY  DS    0XL(SRTGKEYL)       VENDOR KEY LENGTH                            
SRTGRCRD DS    0XL(SRTGRLEN)       VENDOR RECORD LENGTH (W/KEY)                 
SRTGAM   DS    CL1                 AGENCY/MEDIA                                 
SRTGCLT  DS    CL3                 3-CHARACTER CLIENT CODE                      
SRTGPRD  DS    CL3                 3-CHARACTER PRODUCT CODE                     
SRTGEST  DS    XL1                 ESTIMATE                                     
SRTGINV  DS    XL2                 INVOICE NUMBER                               
SRTGBDAT DS    XL2                 BILLED DATE                                  
SRTGMOS  DS    XL2                 MOS                                          
SRTGKYL1 EQU   *-SRTGAM            KEY LENGTH 1 (JUST BILL HEADER INFO)         
SRTGMKT  DS    XL2                 MARKET CODE                                  
SRTGSTA  DS    XL7                 STATION                                      
SRTGKEYL EQU   *-SRTGAM            KEY LENGTH 2 (BILL HEADER + VENDOR)          
***                                                                             
*  NEXT SEVERAL FIELDS FROM BILL HEADER                                         
***                                                                             
SRTGBMOS DS    CL6                 MOS (MMM/YY)                                 
SRTGBGRS DS    PL6                 GROSS AMOUNT DUE                             
SRTGBNET DS    PL6                 NET AMOUNT DUE                               
SRTGBACT DS    PL6                 ACTUAL AMOUNT DUE                            
SRTGBCNT DS    XL4                 CALCULATED NET                               
SRTGBTAX DS    XL4                 TAX AMOUNT DUE (USA)                         
SRTGBGST DS    XL4                 GST AMOUNT DUE (CAN)                         
SRTGBPST DS    XL4                 PST AMOUNT DUE (CAN)                         
SRTGBHST DS    XL4                 HST AMOUNT DUE (CAN)                         
SRTGBPRV DS    XL1                 PROVINCE       (CAN)                         
SRTGBTYP DS    CL4                 BILL TYPE (B1,B2,B3,B4,B5,ETC)               
SRTGBINV DS    CL10                FULL INVOICE NUMBER AS ON BILL               
SRTGIDAT DS    CL10                INVOICE RUN DATE                             
SRTGIDDT DS    CL10                INVOICE DUE DATE                             
SRTGSTAT DS    XL1                 STATUS BYTE                                  
SRTGSTA2 DS    XL1                 STATUS BYTE 2                                
SRTGSTA3 DS    XL1                 STATUS BYTE 3                                
SRTGRTLF DS    XL1                 RETAIL FLAG                                  
SRTGRTLA DS    CL12                RETAIL ACCOUNT CODE                          
SRTGWBFL DS    CL10                WB FLIGHT CODE                               
SRTGBMED DS    CL1                 MEDIA CODE                                   
SRTGBSMD DS    CL1                 NETWORK SUB-MEDIA                            
SRTGCTYP DS    CL1                 COST TYPE (NET ONLY)                         
SRTGPKNM DS    CL5                 PACKAGE NAME (NET ONLY)                      
SRTGDPTF DS    CL1                 DAYPART FILTER (NET ONLY)                    
SRTGPKNO DS    CL1                 PACKAGE NUMBER (NET ONLY)                    
***                                                                             
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
***                                                                             
SRTGVGRS DS    XL4                 SPBVEGRS - VENDOR GROSS AMOUNT               
SRTGVNET DS    XL4                 SPBVENET - VENDOR NET AMOUNT                 
SRTGVTAX DS    XL4                 SPBVETAX - VENDOR TAX AMT (SPOT USA)         
SRTGVGTX DS    XL4                 NBILGRS  - VENDOR GROSS TAX (NET)            
SRTGVNTX DS    XL4                 NBILNET  - VENDOR NET TAX (NET)              
SRTGCOS2 DS    CL1                 STABKCUR - VENDOR COST2                      
SRTGVCS2 DS    XL4                 SRTGVCS2 - VENDOR COST2 AMOUNT               
SRTGRLEN EQU   *-SRTGAM                                                         
*                                                                               
PHDSFLDD DSECT                     PHD SPECIAL FIELDS TABLE                     
PHDINVNO DS    CL10                HDR - INVOICE NUMBER WITH NO DASHES          
PHDPUPSE DS    CL10                HDR - "standard" OR "creditMemo"             
PHDEUDF1 DS    CL32                HDR - ESTIMATE UDEF 1                        
PHDEUDEF DS    CL16                HDR - ESTIMATE UDEF 2                        
PHDINVLN DS    CL3                 INV - INVOICE LINE NUMBER                    
PHDINVLB DS    XL2                 INV - INVOICE LINE NUMBER BINARY             
PHDINVQT DS    CL2                 INV - QUANTITY WILL BE 1                     
PHDVRAMT DS    CL12                INV - VENDOR AMT  (ALWAYS POSITIVE)          
PHDVNAME DS    CL20                INV - VENDOR NAME                            
PHDVDESC DS    CL64                INV - M_CLT_PRD_EST_EUDEF1_EUDEF2            
PHDVAMT2 DS    CL12                INV - VENDOR AMT  (CAN BE NEGATIVE)          
PHDINVAM DS    CL13                FTR - INVOICE AMT (CAN BE NEGATIVE)          
PHDINVAC DS    CL13                FTR - INVOICE AMT + COMMISSION               
PHDINVTX DS    CL13                FTR - INVOICE TAX (CAN BE NEGATIVE)          
PHDIGST  DS    CL13                FTR - INVOICE GST (CAN BE NEGATIVE)          
PHDIPST  DS    CL13                FTR - INVOICE PST (CAN BE NEGATIVE)          
PHDTAXRT DS    CL6                 FTR - INVOICE TAX RATE                       
PHDTAXRB DS    XL4                 FTR - INVOICE TAX RATE BINARY                
PHDTAXCD DS    CL8                 FTR - INVOICE TAX CODE                       
PHDINTOT DS    CL13                FTR - INV AMT+TAX (CAN BE NAGATIVE)          
PHDPADD1 DS    CL30                PRODUCT RECORD ADDRESS LINE 1                
PHDPADD2 DS    CL30                PRODUCT RECORD ADDRESS LINE 2                
PHDPADD3 DS    CL30                PRODUCT RECORD ADDRESS LINE 3                
PHDPADD4 DS    CL30                PRODUCT RECORD ADDRESS LINE 4                
PHDSFLDQ EQU   *-PHDSFLDD          SPECIAL FIELDS TABLE LENGTH                  
*                                                                               
GENSFLDH DSECT                     GENERIC EDI BILL HDR SOFT FIELDS             
GENUSRID DS    CL8                 USER-ID                                      
GENSYSTM DS    CL4                 SYSTEM (SPOT/NET)                            
GENAGNCY DS    CL2                 AGENCY ALPHA                                 
GENFRMAT DS    CL1                 FORMAT/FLAVOR                                
GENCRDTM DS    0CL14               DATE/TIME (yyyymmddhhmmss)                   
GENCRDAT DS    CL8                 DATE (yyyymmdd)                              
GENCRTHH DS    CL2                 CREATION TIME HH (21:24:55)                  
GENCRTMM DS    CL2                 CREATION TIME HH (21:24:55)                  
GENCRTSS DS    CL2                 CREATION TIME HH (21:24:55)                  
GENCLLEN EQU   *-GENSFLDH          SUBTRACT OVERHEAD BEFORE CLEARING            
GENMEDCD DS    CL1                 MEDIA CODE                                   
GENMEDNM DS    CL11                MEDIA NAME                                   
GENCLTCD DS    CL3                 CLIENT CODE                                  
GENCLTNM DS    CL20                CLIENT NAME                                  
GENCLTAO DS    CL2                 ACC OFFICE CODE                              
GENPRDCD DS    CL3                 PRODUCT CODE                                 
GENPRDNM DS    CL20                PRODUCT NAME                                 
GENPRUS1 DS    CL32                PRODUCT UDEF 1                               
GENPRUS2 DS    CL16                PRODUCT UDEF2                                
GENPRDA1 DS    CL30                PRODUCT ADDRESS 1                            
GENPRDA2 DS    CL30                PRODUCT ADDRESS 2                            
GENPRDA3 DS    CL30                PRODUCT ADDRESS 3                            
GENPRDA4 DS    CL30                PRODUCT ADDRESS 4                            
GENPRDIC DS    CL4                 PRODUCT INTERFACE CODE                       
GENPRDDV DS    CL3                 PRODUCT DIVISION                             
GENPRDU1 DS    CL32                PRODUCT UCOMM 1                              
GENPRDU2 DS    CL32                PRODUCT UCOMM 2                              
GENPRDU3 DS    CL32                PRODUCT UCOMM 3                              
GENPRDU4 DS    CL32                PRODUCT UCOMM 4                              
GENPRGRP DS    CL4                 PRODUCT GROUP                                
GENPGRPN DS    CL24                PRODUCT GROUP NAME                           
GENPGRN2 DS    CL24                PRODUCT GROUP NAME 2                         
GENESTCD DS    CL3                 ESTIMATE CODE                                
GENESTNM DS    CL20                ESTIMATE NAME                                
GENESUS1 DS    CL32                ESTIMATE UDEF 1                              
GENESUS2 DS    CL16                ESTIMATE UDEF2                               
GENESUC1 DS    CL32                ESTIMATE UCOMM 1                             
GENESUC2 DS    CL32                ESTIMATE UCOMM 2                             
GENESUC3 DS    CL32                ESTIMATE UCOMM 3                             
GENESUC4 DS    CL32                ESTIMATE UCOMM 4                             
GENESUC5 DS    CL32                ESTIMATE UCOMM 5                             
GENESUC6 DS    CL32                ESTIMATE UCOMM 6                             
GENESUC7 DS    CL32                ESTIMATE UCOMM 7                             
GENESUC8 DS    CL32                ESTIMATE UCOMM 8                             
GENSADJB DS    CL1                 BILSTAT - SEP ADJUSTMENT BILL                
GENMANBL DS    CL1                 BILSTAT - MANUAL BILL                        
GENTAORB DS    CL1                 BILSTAT - TRUE AOR BILL                      
GENCAORB DS    CL1                 BILSTAT - CLIENT AOR BILL                    
GENNETBL DS    CL1                 BILSTAT - NET BILL (IN SEP COMM )            
GENNTAOR DS    CL1                 BILSTAT - NO TAX IN AOR CALC                 
GENCOMBL DS    CL1                 BILSTAT - COMM ONLY BILL                     
GENCOMSB DS    CL1                 BILSTAT - SEPARATE COMMISSION BILL           
GENSTAT2 DS    CL1                 BILSTAT2 X'80' - DUMMY FLAG                  
         DS    CL1                 BILSTAT2 X'40' - DUMMY FLAG                  
         DS    CL1                 BILSTAT2 X'20' - DUMMY FLAG                  
GENCOST2 DS    CL1                 BILSTAT2 X'10' - COST2 BILL                  
         DS    CL1                 BILSTAT2 X'08' - DUMMY FLAG                  
         DS    CL1                 BILSTAT2 X'04' - DUMMY FLAG                  
         DS    CL1                 BILSTAT2 X'02' - DUMMY FLAG                  
         DS    CL1                 BILSTAT2 X'01' - DUMMY FLAG                  
GENSTAT3 DS    CL1                 BILSTAT3 X'80' - DUMMY FLAG                  
         DS    CL1                 BILSTAT3 X'40' - DUMMY FLAG                  
         DS    CL1                 BILSTAT3 X'20' - DUMMY FLAG                  
         DS    CL1                 BILSTAT3 X'10' - DUMMY FLAG                  
         DS    CL1                 BILSTAT3 X'08' - DUMMY FLAG                  
GENGMTRD DS    CL1                 BILSTAT3 X'04' - GROUPM TRADE                
GENGMTCN DS    CL1                 BILSTAT3 X'02' - GROUPM TRADE CNET           
GENGMTDM DS    CL1                 BILSTAT3 X'01' - GROUPM TRADE MIDAS          
GENRETCB DS    CL1                 BRETAIL X'80' - CONTROL BILL                 
GENRETSB DS    CL1                 BRETAIL X'40' - SUMMARY BILL                 
         DS    CL1                 BRETAIL X'20' - DUMMY FLAG                   
         DS    CL1                 BRETAIL X'10' - DUMMY FLAG                   
         DS    CL1                 BRETAIL X'08' - DUMMY FLAG                   
         DS    CL1                 BRETAIL X'04' - DUMMY FLAG                   
GENRETRO DS    CL1                 BRETAIL X'02' - REGULAR OUTLET               
GENRETCO DS    CL1                 BRETAIL X'01' - CORPORATE OUTLET             
GENRETAC DS    CL12                BRETACCT - RETAIL ACCOUNT CODE               
GENWBFLT DS    CL10                WB FLIGHT CODE                               
GENINMOS DS    CL6                 INVOICE MOS (APR/20)                         
GENINVDT DS    CL10                INVOICE RUN DATE (2019-10-08)                
GENINVDD DS    CL10                INVOICE DUE DATE (2019-10-08)                
GENINVNO DS    CL10                INVOICE NUMBER WITH DASHES                   
GENINGRS DS    CL13                INVOICE GROSS AMOUNT                         
GENINNET DS    CL13                INVOICE NET AMOUNT                           
GENINACT DS    CL13                INVOICE ACTUAL AMOUNT                        
GENINNTC DS    CL13                INVOICE CALCULATED NET                       
GENINCOM DS    CL13                INVOICE COMMISSION                           
GENINTAX DS    CL13                INVOICE TAX (SPBVETAX FROM SPBVAL)           
GENINGST DS    CL13                INVOICE GST (SPBVGST FROM SPBVAL)            
GENINGSR DS    CL5                 GST RATE (5.000)                             
GENINPST DS    CL13                INVOICE PST (SPBVPST FROM SPBVAL)            
GENINPSR DS    CL5                 PST RATE (5.000)                             
GENINHST DS    CL13                INVOICE HST (SPBVHST FROM SPBVAL)            
GENINHSR DS    CL5                 HST RATE (5.000)                             
GENINTXT DS    CL3                 TAX TYPE (PST/HST/QST)                       
GENINPRV DS    CL20                PROVINCE (PROVTAB IN SPGETRATE)              
GENINTYP DS    CL4                 BILL TYPE (BTYPE FROM SPGENBILL)             
GENINCNT DS    CL9                 TOTAL INVOICE COUNT                          
GENINSMD DS    CL1                 SUB-MEDIA (NET)                              
GENCTYPE DS    CL1                 COST TYPE (NET ONLY)                         
GENPKGNM DS    CL5                 PACKAGE NAME (NET ONLY)                      
GENDPTFL DS    CL1                 DAYPART FILTER (NET ONLY)                    
GENPKGNO DS    CL3                 PACKAGE NUMBER (NET ONLY)                    
GENSFDHQ EQU   *-GENSFLDH          SPECIAL FIELDS BILL HDR TABLE LENGTH         
*                                                                               
GENSFLDV DSECT                     GENERIC EDI VENDOR SOFT FIELDS               
GENVMKCD DS    CL4                 VENDOR MARKET CODE                           
GENVMKNM DS    CL24                VENDOR MARKET NAME                           
GENVMGRP DS    CL6                 VENDOR MARKET GROUP CODE                     
GENVMGN1 DS    CL24                VENDOR MARKET GROUP NAME 1                   
GENVMGN2 DS    CL24                VENDOR MARKET GROUP NAME 2                   
GENVMGN3 DS    CL24                VENDOR MARKET GROUP NAME 3                   
GENVMUC1 DS    CL32                MARKET UCOMM 1                               
GENVMUC2 DS    CL32                MARKET UCOMM 2                               
GENVMUC3 DS    CL32                MARKET UCOMM 3                               
GENVMUC4 DS    CL32                MARKET UCOMM 4                               
GENVSTAN DS    CL7                 VENDOR STATION                               
GENVNAME DS    CL20                VENDOR STATION ADDRESS NAME                  
GENVNTWK DS    CL4                 NETWORK CODE                                 
GENVNTWN DS    CL20                NETWORK STATION ADDRESS NAME                 
GENVCOS2 DS    CL1                 COST2 FLAG                                   
GENVGRSD DS    CL13                VENDOR GROSS                                 
GENVNETD DS    CL13                VENDOR NET                                   
GENVTAXD DS    CL13                VENDOR TAX (SPOT ONLY)                       
GENVTAXG DS    CL13                VENDOR TAX GROSS (NET ONLY)                  
GENVTAXN DS    CL13                VENDOR TAX NET (NET ONLY)                    
GENVCS2D DS    CL13                VENDOR COST2 AMOUNT (NET ONLY)               
GENSFDVQ EQU   *-GENSFLDV          SPECIAL FIELDS VENDOR TABLE LENGTH           
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM                                       
MQAGYID  DS    CL4                 AGENCY 1D 4-CHAR                             
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 YYMMDD OF DSN                                
MQTIME   DS    CL6                 HHMMSS OF DSN                                
MQDATA1  DS    CL32                NOT USED                                     
MQDATA2  DS    CL32                NOT USED                                     
MQFILE   DS    CL64                DSN  (MINUS SFTPDISK.PROD.)                  
MQMSGLNQ EQU   *-MQMSGD                                                         
         ORG   MQFILE                                                           
MQFILELG DS    CL128               DNS Long version                             
MQMSGLQ2 EQU   *-MQMSGD                                                         
*                                                                               
NBIOAREA CSECT                                                                  
         DS    20000C              FOR NETIO TO USE AS NBAIO                    
*                                                                               
NETBLK   CSECT                                                                  
         DS    1200C               NETBLOCK                                     
*                                                                               
STABUCKC CSECT                     STATION BUCKET RECORD I/O AREA               
         DS    2000C               2K I/O AREA FOR STATION BUCKET RECS          
*                                                                               
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NENETRATED                                                     
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE SPGENSTAB                                                      
ADDRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENMKG                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDUCOMD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE NEGENUBILL                                                     
       ++INCLUDE NETBILLRD                                                      
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'116SPREPVL02 01/28/21'                                      
         END                                                                    
