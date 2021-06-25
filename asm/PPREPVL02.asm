*          DATA SET PPREPVL02  AT LEVEL 139 AS OF 01/28/21                      
*PHASE PPVL02A                                                                  
*INCLUDE PPFMTINO                                                               
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE PUBFLOAT                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE PPBVAL                                                                 
*INCLUDE DDUCOM                                                                 
*INCLUDE WIDE                                                                   
*INCLUDE USSIO                                                                  
*                                                                               
         TITLE 'PPVL02 - VENDOR LEVEL EDI BILLING'                              
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT SPEC-39025   01/28/21 NEW EDI PHD/Coupa for Canadian Tire      *         
* AKAT SPSUG-1667   09/10/20 GENERIC EDI                              *         
* AKAT SPEC-46256   06/08/20 CARRY TAXES FOR MULTI MONTH INVOICES     *         
* AKAT SPEC-39485   01/09/20 PRINT ERR MSG FOR MISSING PLACEMENT ID   *         
* AKAT SPEC-41453   12/11/19 CHANGES FOR SCOTIABANK                   *         
* AKAT SPEC-24094   07/15/19 NEW EDI BILLING FOR PHD CANADA           *         
* AKAT SPEC-35666   08/12/19 PAD LAST VENDOR INVOICE FOR MEDIA EXPERTS*         
* AKAT SPEC-28717   06/17/19 ALLOW ANY AGENCY TO REQ WITH OPTOIN "P"  *         
* AKAT SPEC-34644   04/18/19 CARRY OVER TAXES FOR BILLS W/MULTI MOS   *         
* AKAT SPEC-23158   03/11/19 CALL PRISMA API FOR CARAT & HAVAS        *         
* AKAT SPEC-24006   02/21/19 MEDIA EXPERTS CHANGES FOR MEDIA O B6/B7  *         
* AKAT SPEC-22855   04/09/18 CD CHANGES FOR VL                        *         
* AKAT SPEC-22223   03/29/18 BUG FIX AND CD CHANGES FOR VL            *         
* AKAT SPEC-14682   09/07/17 NEW EDI BILLING MEDIA EXPERTS/BELL       *         
***********************************************************************         
* QOPT  VAL                        COMMENTS                           *         
* ----- --- --------------------------------------------------------- *         
* QOPT4  Y  WHATEVER IS IN THE PRODUCE FILE OPTION (BLANK=Y)          *         
* QOPT5  Y  ONLY SET VIA JCL FOR TESTING - SUPPRESS MQ MESSAGE        *         
* QOPT6  Y  ONLY SET VIA JCL FOR TESTING - TEST RUN (SKIP TAPE)       *         
* QOPT7  P  ONLY SET VIA JCL FOR TESTING - PRINT OUTPUT RECORDS       *         
***********************************************************************         
PPVL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPVL02,RR=R9                                                   
*                                                                               
         ST    R9,RELO             RELO VALUE                                   
         B     *+8                                                              
RELO     DC    F'0'                RELO                                         
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         MVI   RC2DSECT,C'Y'       HAVE GLOBAL WORKING STORAGE 2                
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         CLI   MODE,RUNFRST        RUNFRST?                                     
         BE    RUNF                YES                                          
         CLI   MODE,PROCREQ        PROCESS REQUEST?                             
         BE    REQF                YES                                          
         CLI   MODE,RUNLAST        RUNLAST?                                     
         BE    RUNL                YES                                          
*                                                                               
EXIT     XIT1                      EXIT                                         
*                                                                               
RUNF     DS    0H                  RUN FIRST                                    
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
*                                                                               
         LA    R2,INVCNT           ZAP ACCUMULATORS                             
         LA    R3,4                4 ACCUMULATORS                               
*                                                                               
RUNF20   ZAP   0(4,R2),=P'0'       ZAP ACCUMULATOR                              
         LA    R2,4(R2)            BUMP TO NEXT ACCUMULATOR                     
         BCT   R3,RUNF20           ZAP NEXT ACCUMULATOR                         
*                                                                               
         ZAP   BILTOT,=P'0'        ZAP BILLING $ ACCUMULATOR                    
         ZAP   DTLTOT,=P'0'        ZAP DETAIL $ ACCUMULATOR                     
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
         MVI   ZEROS,C'0'          INIT ZEROS                                   
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         MVI   NOFILE,C'N'         INIT NOFILE                                  
         MVI   PLCMSTAT,0          INIT PLCMSTAT                                
*                                                                               
         B     EXIT                EXIT                                         
*                                                                               
REQF     DS    0H                  PROCESS REQUEST                              
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE HEADLINES TO PRINT                     
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         MVI   DMOUTBTS,X'FD'      PASS DELETED RECORDS                         
*                                                                               
         LAY   R6,AGYTAB           GET TAPE SPEC FROM AGYTAB                    
         USING AGYTABD,R6          AGENCY SPEC DSECT                            
*                                                                               
         CLI   QOPT4,C'P'          PRODUCE FILE = P?                            
         BNE   REQF05              NO                                           
         LAY   R6,UBTABLE          POINT TO UBTABLE                             
         B     REQF20              ALL AGENCIES GO THROUGH UB ROUTINE           
*                                                                               
REQF05   CLI   QOPT4,C'G'          PRODUCE FILE = G?                            
         BNE   REQF10              NO                                           
         LAY   R6,GENTABLE         POINT TO GENERIC SPOT TABLE                  
         B     REQF20              GENERIC ROUTINES FOR ALL AGENCIES            
*                                                                               
REQF10   CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    REQFERR1            YES - GIVE ERROR MESSAGE AND EXIT            
         CLC   QAGENCY,AGYTAGY     MATCH ON AGENCY?                             
         BNE   REQF15              NO - BUMP TO NEXT TABLE ENTRY                
         CLC   QOPT4,AGYTAPE       MATCH ON FILE TYPE?                          
         BE    REQF20              YES                                          
*                                                                               
REQF15   LA    R6,AGYTABL(R6)      BUMP TO NEXT ENTRY                           
         B     REQF10              CHECK NEXT ENTRY                             
*                                                                               
REQF20   OC    AGYBLKS,AGYBLKS     NULL BLKSIZE?                                
         BNZ   *+8                 NO                                           
         MVI   QOPT6,C'Y'          YES - SKIP DYNALLOC & OPEN                   
         MVC   SVQOPT4,QOPT4       SAVE "PRODUCE FILE" OPTION                   
         MVC   SVQOPT6,QOPT6       SAVE QOPT6 IT'S OUT OF SCOPE AT RUNL         
         MVC   SVQOPT7,QOPT7       SAVE QOPT7 IT'S OUT OF SCOPE AT RUNL         
         MVC   SVQSTART,QSTART     SAVE QSTART                                  
         MVC   SVQEND,QEND         SAVE QEND                                    
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
         LAY   R0,OUTRECLN         80K AOUTREC BUFFER                           
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
         LA    R0,3                VBILLR, VBUYR & VOUTPR                       
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
         LA    RF,PVLTAPE          PVLTAPE                                      
         A     RF,RELO             ADD RELO                                     
         ST    RF,APVLTAPE         SAVE A(PVLTAPE)                              
*                                                                               
         MVC   RCSUBPRG,AGYPSPEC   SET PSPEC IN PPREPVL01                       
*                                                                               
         MVC   SAVEAGY,QAGENCY     SAVE OFF AGENCY ALPHA                        
*                                                                               
         MVI   OPENSW,C'Y'         SET TAPE OPEN                                
*                                                                               
         CLI   QOPT6,C'Y'          SKIP TAPE?                                   
         BE    REQF30              YES - SKIP DYNALLOC & OPEN                   
*                                                                               
         LAY   R5,PVLTAPE          A(DCB FOR TAPE)                              
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
         LA    R3,PPDYNDSN         TAPE DEFINITION                              
         MVC   13(2,R3),QAGENCY    AGENCY                                       
         MVC   15(1,R3),AGYSUFX    SUFFIX (DEFAULT IS 1)                        
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,=C'PVLTAPE '),(0,0(R3))                         
*                                                                               
         OPEN  ((R5),OUTPUT)       OPEN THE TAPE                                
         B     REQF30              SAVE OFF AGENCY TAPE SPECS                   
         DROP  R5                  DROP DCB USING                               
*                                                                               
REQF25   CLI   QOPT6,C'Y'          SKIP TAPE?                                   
         BE    REQF30              YES - SKIP DYNALLOC & OPEN                   
         LAY   R5,PVLTAPE          A(DCB FOR TAPE)                              
         USING IHADCB,R5           DCB DSECT                                    
         CLC   DCBBLKSI,AGYBLKS    SAME BLOCK SIZE?                             
         BNE   REQFERR2            NO - GIVE ERROR MESSAGE AND EXIT             
         CLC   DCBLRECL,AGYLRCL    RECORD SIZE?                                 
         BNE   REQFERR2            NO - GIVE ERROR MESSAGE AND EXIT             
         CLC   QOPT4,SVQOPT4       "PRODUCE FILE" OPT MATCHES?                  
         BNE   REQFERR2            NO - GIVE ERROR MESSAGE AND EXIT             
         DROP  R5                  DROP DCB USING                               
*                                                                               
REQF30   MVI   ONEPRD,C'N'         INIT ONEPRD TO C'N'                          
         CLC   QPRODUCT,=C'ALL'    ALL PRODUCTS?                                
         BE    REQF35              YES                                          
         CLC   QPRODUCT,SPACES     ALL PRODUCTS?                                
         BE    REQF35              YES                                          
         MVI   ONEPRD,C'Y'         NO - SET FOR ONE PRODUCT                     
*                                                                               
REQF35   MVC   SVQELOW,=X'0000'    ESTIMATE LOW                                 
         MVC   SVQEHI,=X'FFFF'     ESTIMATE HIGH                                
         CLC   QEST,SPACES         ALL ESTIMATES?                               
         BE    REQF40              YES                                          
         CLC   QEST,=C'ALL'        ALL ESTIMATES?                               
         BE    REQF40              YES                                          
         PACK  DUB,QEST            PACK EBCIDIC ESTIMATE                        
         CVB   R0,DUB              CONVERT TO BINARY                            
         STH   R0,SVQELOW          SET AS LOW ESTIMATE                          
         STH   R0,SVQEHI           SET AS HIGH ESTIMATE                         
         CLC   QESTEND,SPACES      HAVE ESTIMATE END?                           
         BE    REQF40              NO                                           
         PACK  DUB,QESTEND         PACK EBCIDIC ESTIMATE                        
         CVB   R0,DUB              CONVERT TO BINARY                            
         STH   R0,SVQEHI           SET AS HIGH ESTIMATE                         
*                                                                               
REQF40   GOTO1 DATCON,DMCB,QSTART,(3,BQS) SET BQS                               
         GOTO1 (RF),(R1),QEND,(3,BQE)     SET BQE                               
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R5,KEY              R5 = KEY                                     
         USING PCLTKEY,R5          CLIENT RECORD DSECT                          
         MVC   PCLTKAGY,QAGENCY    AGENCY                                       
         MVC   PCLTKMED,QMEDIA     MEDIA                                        
         MVI   PCLTKRCD,X'02'      CLIENT RECORD CODE                           
         MVC   PCLTKCLT,QCLIENT    CLIENT CODE                                  
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   PCLTKEY,KEYSAVE     FOUND CLIENT RECORD?                         
         BNE   EXIT                NO - SKIP THIS REQUEST                       
         DROP  R5                  DROP CLIENT RECORD USING                     
*                                                                               
         LA    R0,PCLTREC          A(CLIENT RECORD)                             
         ST    R0,AREC             AREC = A(PCLTREC)                            
*                                                                               
         GOTO1 GETPRT              READ CLIENT RECORD INTO PCLTREC              
*                                                                               
         XC    B1PROF,B1PROF       CLEAR B1PROF                                 
         XC    B1XPROF,B1XPROF     CLEAR B1XPROF                                
         XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK(4),=C'POB1'    START BUILDING B1 PROFILE KEY                
         MVC   WORK+4(2),QAGENCY   AGENCY                                       
         MVC   WORK+6(1),QMEDIA    MEDIA                                        
         MVC   WORK+7(3),PCLTKCLT  CLIENT                                       
         CLI   PCLTOFF,C' '        HAVE CLIENT OFFICE?                          
         BNH   REQF45              NO                                           
         MVI   WORK+10,C'*'        YES - INDICATE CLIENT OFFICE                 
         MVC   WORK+11(1),PCLTOFF  SET CLIENT OFFICE                            
*                                                                               
REQF45   GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
*                                                                               
         MVC   WORK(4),=C'PB1X'    B1X PROFILE                                  
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         GOTO1 APRBILL,DMCB,(RA)   PROCESS BILL RECORDS                         
*                                                                               
         GOTO1 APRBUY,DMCB,(RA)    PROCESS BUY RECORDS                          
*                                                                               
         B     EXIT                EXIT                                         
*                                                                               
REQFERR1 LAY   RE,NOTAPSPC         NO TAPE SPECS ERROR MESSAGE                  
         MVC   P1(L'NOTAPSPC),0(RE) MOVE THE ERROR MESSAGE TO P1                
         B     REQFERRX            PRINT THE ERROR MESSAGE & END JOB            
*                                                                               
REQFERR2 LAY   RE,MULTMSG          MULTIPLE TAPE SPECS ERROR MESSAGE            
         MVC   P1(L'MULTMSG),0(RE)  MOVE THE ERROR MESSAGE TO P1                
         B     REQFERRX            PRINT THE ERROR MESSAGE & END JOB            
*                                                                               
REQFERRX LAY   RE,JOBEND           MAKE IT CLEAR THAT JOB IS TERMINATED         
         MVC   P1(L'JOBEND),0(RE)  AND EDI FILE IS NOT GENERATED                
         GOTO1 REPORT              PRINT THE ERROR MESSAGE                      
         MVI   NOFILE,C'Y'         DO NOT GENERATE FILE!                        
         GOTO1 AENDREQ             THIS WON'T END THE REQUESTS!                 
*                                                                               
RUNL     DS    0H                  RUNLAST - FILE IS PROCESSED HERE             
*                                                                               
*                                                                               
         CLI   NOFILE,C'Y'         SUPPRESS FILE?                               
         BE    RUNLXIT             YES                                          
*                                                                               
         L     RF,VOUTPR           A(AGY SPECIFIC OUTPUT ROUTINE)               
         GOTO1 (RF),DMCB,(RA)      PROCESS AGY SPECIFIC OUTPUT ROUTINE          
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
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P1+7(08),=C'INVOICES'                                            
*                                                                               
         LA    R4,P1+18                                                         
         CP    BILTOT,=P'0'                                                     
         BL    RUNL20                                                           
         MVI   0(R4),C'$'                                                       
         LA    R4,1(R4)                                                         
         EDIT  (P8,BILTOT),(14,0(R4)),2,COMMAS=YES,ALIGN=LEFT                   
         B     RUNL25                                                           
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
         CLI   SVQOPT6,C'Y'        TEST RUN ?                                   
         BE    RUNLXIT             YES - NO CLOSE                               
         CLOSE PVLTAPE             CLOSE THE TAPE                               
*                                                                               
RUNLXIT  GOTO1 VSORTER,DMCB,=C'END'                                             
         B     EXIT                DONE                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
ACONS    DS    0F                  A(RELOCATED ADDRESSES)                       
         DC    A(PRBILL)           A(PRBILL)                                    
         DC    A(PRBUY)            A(PRBUY)                                     
         DC    V(PPFMTINO)         V(PPFMTINO)                                  
         DC    V(PUBFLOAT)         V(PUBFLOAT)                                  
         DC    V(SORTER)           V(SORTER)                                    
         DC    V(BINSRCH)          V(BINSRCH)                                   
         DC    A(TAPEWRT)          A(TAPEWRT)                                   
         DC    V(DDUCOM)           V(DDUCOM)                                    
         DC    V(WIDE)             V(WIDE)                                      
         DC    A(MQMESSGE)         V(MQMESSGE)                                  
ACONSX   EQU   *                   END OF ACONS                                 
*                                                                               
PPDYNDSN DC    CL20'PRTTAPE.PP0VLAG1'                                           
*                                                                               
NOTAPSPC DC    C'**NO TAPE SPECS FOUND - REQ BYPASSED**'                        
MULTMSG  DC    C'**MULTIPLE TAPE DESCRIPTIONS IN ONE JOB - BYPASSED**'          
JOBEND   DC    C'**THIS AGENCY/TAPE COMBINATION IS NOT SET UP FOR THE V+        
               L - JOB TERMINATED - EDI FILE NOT GENERATED**'                   
*                                                                               
TITLES   DS    0C                                                               
         DC    CL19'INVOICES'                                                   
         DC    CL19'LINE ITEMS'                                                 
         DC    CL19'FILE HEADERS'                                               
         DC    CL19'FILE RECORDS OUTPUT'                                        
*                                                                               
AGYTAB   DS    0D                                                               
*&&DO                                                                           
*                                                                               
*        MSNYA - UNILEVER - SPEC-11845                                          
*                                                                               
         DC    C'H7'               AGENCY                                       
         DC    C'Y'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'01'               SPEC/RCSUBPRG                                
         DC    AL2(6050)           DCB/TAPE - BLKSIZE=                          
         DC    AL2(6050)           DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SORTKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SORTRLEN)       SORTER - RECCDLEN                            
         DC    A(ULBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(ULBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(ULBILLR)          BILL ROUTINE                                 
         DC    A(ULBUYR)           BUY ROUTINE                                  
         DC    A(ULOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        SJR - TESTING UNILEVER - SPEC-11845                                    
*                                                                               
         DC    C'SJ'               AGENCY                                       
         DC    C'Y'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'01'               SPEC/RCSUBPRG                                
         DC    AL2(6050)           DCB/TAPE - BLKSIZE=                          
         DC    AL2(6050)           DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SORTKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SORTRLEN)       SORTER - RECCDLEN                            
         DC    A(ULBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(ULBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(ULBILLR)          BILL ROUTINE                                 
         DC    A(ULBUYR)           BUY ROUTINE                                  
         DC    A(ULOUTPR)          OUTPUT ROUTINE                               
*&&                                                                             
*                                                                               
*        M2TOA - BELL - SPEC-14682                                              
*                                                                               
         DC    C'U#'               AGENCY                                       
         DC    C'Y'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'00'               SPEC/RCSUBPRG                                
         DC    AL2(449)            DCB/TAPE - BLKSIZE=                          
         DC    AL2(449)            DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTBKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTBRLEN)       SORTER - RECCDLEN                            
         DC    A(BLBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(BLBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(BLBILLR)          BILL ROUTINE                                 
         DC    A(BLBUYR)           BUY ROUTINE                                  
         DC    A(BLOUTPR)          OUTPUT ROUTINE                               
*&&DO                                                                           
*                                                                               
*        LETO (TESTING) - BELL - SPEC-14682                                     
*                                                                               
         DC    C'FG'               AGENCY                                       
         DC    C'Y'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'00'               SPEC/RCSUBPRG                                
         DC    AL2(449)            DCB/TAPE - BLKSIZE=                          
         DC    AL2(449)            DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTBKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTBRLEN)       SORTER - RECCDLEN                            
         DC    A(BLBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(BLBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(BLBILLR)          BILL ROUTINE                                 
         DC    A(BLBUYR)           BUY ROUTINE                                  
         DC    A(BLOUTPR)          OUTPUT ROUTINE                               
*&&                                                                             
*&&DO                                                                           
*                                                                               
*        TCH1O (TESTING) - BELL - SPEC-14682                                    
*                                                                               
         DC    C'O1'               AGENCY                                       
         DC    C'Y'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'00'               SPEC/RCSUBPRG                                
         DC    AL2(449)            DCB/TAPE - BLKSIZE=                          
         DC    AL2(449)            DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTBKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTBRLEN)       SORTER - RECCDLEN                            
         DC    A(BLBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(BLBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(BLBILLR)          BILL ROUTINE                                 
         DC    A(BLBUYR)           BUY ROUTINE                                  
         DC    A(BLOUTPR)          OUTPUT ROUTINE                               
*&&                                                                             
*                                                                               
*        CARSEC - SPEC-23158                                                    
*                                                                               
UBTABLE  DC    C'UB'               AGENCY                                       
         DC    C'P'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'00'               SPEC/RCSUBPRG                                
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTUKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTURLEN)       SORTER - RECCDLEN                            
         DC    A(UBBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(UBBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(UBBILLR)          BILL ROUTINE                                 
         DC    A(UBBUYR)           BUY ROUTINE                                  
         DC    A(UBOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        PHDTO - SPEC-24094                                                     
*                                                                               
         DC    C'OU'               AGENCY                                       
         DC    C'Y'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'01'               SPEC/RCSUBPRG                                
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBUYR)           BUY ROUTINE                                  
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        LETO  - SPEC-24094                                                     
*                                                                               
         DC    C'FG'               AGENCY                                       
         DC    C'Y'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'01'               SPEC/RCSUBPRG                                
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBUYR)           BUY ROUTINE                                  
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        TCH1O - SPEC-24094                                                     
*                                                                               
         DC    C'O1'               AGENCY                                       
         DC    C'Y'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'01'               SPEC/RCSUBPRG                                
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBUYR)           BUY ROUTINE                                  
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        PHDTO - SPEC-39025                                                     
*                                                                               
         DC    C'OU'               AGENCY                                       
         DC    C'C'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'02'               SPEC/RCSUBPRG                                
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBUYR)           BUY ROUTINE                                  
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        LETO  - SPEC-39025                                                     
*                                                                               
         DC    C'FG'               AGENCY                                       
         DC    C'C'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'02'               SPEC/RCSUBPRG                                
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBUYR)           BUY ROUTINE                                  
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        TCH1O - SPEC-39025                                                     
*                                                                               
         DC    C'O1'               AGENCY                                       
         DC    C'C'                TAPE CODE                                    
         DC    C'1'                SUFFIX = 1 SINCE TAPE CODE IS BLANK          
         DC    X'02'               SPEC/RCSUBPRG                                
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTOKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTORLEN)       SORTER - RECCDLEN                            
         DC    A(OUBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(OUBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(OUBILLR)          BILL ROUTINE                                 
         DC    A(OUBUYR)           BUY ROUTINE                                  
         DC    A(OUOUTPR)          OUTPUT ROUTINE                               
*                                                                               
*        ALL AGENCIES - GENERIC EDI - SPSUG-1667                                
*                                                                               
GENTABLE DC    C'  '               ALL AGENCIES                                 
         DC    C'G'                TAPE CODE G FOR GENERIC EDI                  
         DC    C'1'                SUFFIX = 1                                   
         DC    X'01'               SPEC/RCSUBPRG FOR SPOT                       
         DC    AL2(0)              DCB/TAPE - BLKSIZE=                          
         DC    AL2(0)              DCB/TAPE - LRECL=                            
         DC    C'F'                DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
         DC    AL2(SRTGKEYL)       SORTER - SRTCDLEN                            
         DC    AL2(SRTGRLEN)       SORTER - RECCDLEN                            
         DC    A(GEBKEYL)          BINSRCH - KEY LENGTH                         
         DC    A(GEBRECL2)         BINSRCH - REC LENGTH                         
         DC    F'2000'             MAX NUMBER OF RECORDS                        
         DC    A(GNBILLR)          BILL ROUTINE                                 
         DC    A(GNBUYR)           BUY ROUTINE                                  
         DC    A(GNOUTPR)          OUTPUT ROUTINE                               
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
PVLTAPE  DCB   DDNAME=PVLTAPE,DSORG=PS,MACRF=PM                                 
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
         TITLE 'PRBILL - PROCESS BILL RECORDS'                                  
PRBILL   CSECT                                                                  
         NMOD1 0,PRBILL                                                         
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
***      MVC   ABINRECN,=F'0'      RESET NUMBER OF RECS IN ABINREC              
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R5,KEY              R5 = KEY                                     
         USING PBILLKEY,R5         BILL RECORD DSECT                            
         MVC   PBILKAGY,QAGENCY    AGENCY                                       
         MVC   PBILKMED,QMEDIA     MEDIA                                        
         MVI   PBILKRCD,X'08'      BILL RECORD CODE                             
         MVC   PBILKCLT,QCLIENT    CLIENT CODE                                  
         CLI   ONEPRD,C'Y'         SINGLE PRODUCT REQUEST?                      
         BNE   *+16                NO                                           
         MVC   PBILKPRD,QPRODUCT   YES - SET PRODUCT                            
         MVC   PBILKEST,SVQELOW    SET START ESTIMATE                           
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     BILL10              GO TEST KEY                                  
*                                                                               
BILL05   GOTO1 SEQ                 READ SEQ                                     
*                                                                               
BILL10   CLC   PBILLKEY(7),KEYSAVE SAME AGY/MED/CLIENT?                         
         BNE   BILLXIT             NO - BILLS DONE - GO READ BUYS               
         CLI   ONEPRD,C'Y'         SINGLE PRODUCT REQUEST?                      
         BNE   *+14                NO                                           
         CLC   PBILKPRD,QPRODUCT   MATCH ON PRODUCT?                            
         BNE   BILLXIT             NO - BILLS DONE - GO READ BUYS               
         TM    PBILLKEY+25,X'80'   BILL RECORD DELETED?                         
         BNZ   BILL05              YES - READ SEQ                               
         CLC   PBILKEST,SVQELOW    ESTIMATE IN RANGE?                           
         BL    BILL05              NO - READ SEQ                                
         CLC   PBILKEST,SVQEHI     ESTIMATE IN RANGE?                           
         BH    BILL05              NO - READ SEQ                                
         OC    PBILKEST,PBILKEST   HAVE ESTIMATE ON BILL?                       
         BZ    BILL05              NO - READ SEQ                                
*                                                                               
         DROP  R5                  DROP BILL RECORD USING                       
*                                                                               
         LA    R0,PBILLREC         A(PBILLREC)                                  
         ST    R0,AREC             SAVE A(PBILLREC) IN AREC                     
         GOTO1 GETPRT              READ BILL RECORD                             
*                                                                               
         CLC   PBILLDAT,SVQSTART   BILL DATE BEFORE REQUEST START DATE?         
         BL    BILL05              YES - READ SEQ                               
         CLC   PBILLDAT,SVQEND     BILL DATE AFTER REQUEST END DATE?            
         BH    BILL05              YES - READ SEQ                               
*                                                                               
         L     RF,VBILLR           A(AGENCY SPECIFIC BILL ROUTINE)              
         GOTO1 (RF),DMCB,(RA)      PROCESS AGENCY SPECIFIC BILL ROUTINE         
         B     BILL05              READ SEQ                                     
*                                                                               
BILLXIT  XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'PRBUY - PROCESS BUY RECORDS'                                    
PRBUY    CSECT                                                                  
         NMOD1 0,PRBUY                                                          
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R5,KEY              R5 = KEY                                     
         USING PBUYKEY,R5          BUY RECORD DSECT                             
         MVC   PBUYKAGY,QAGENCY    AGENCY                                       
         MVC   PBUYKMED,QMEDIA     MEDIA                                        
         MVI   PBUYKRCD,X'20'      BUY RECORD CODE                              
         MVC   PBUYKCLT,QCLIENT    CLIENT CODE                                  
         CLI   ONEPRD,C'Y'         SINGLE PRODUCT REQUEST?                      
         BNE   *+10                NO                                           
         MVC   PBUYKPRD,QPRODUCT   YES - SET PRODUCT                            
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     BUY10               GO TEST KEY                                  
*                                                                               
BUY05    LA    R5,KEY              RESET KEY                                    
         GOTO1 SEQ                 READ SEQ                                     
*                                                                               
BUY10    CLC   PBUYKEY(7),KEYSAVE  SAME AGY/MED/CLIENT?                         
         BNE   BUYXIT              NO - DONE                                    
         CLI   ONEPRD,C'Y'         SINGLE PRODUCT REQUEST?                      
         BNE   *+14                NO                                           
         CLC   PBUYKPRD,QPRODUCT   MATCH ON PRODUCT?                            
         BNE   BUYXIT              NO - DONE                                    
         CLC   PBUYKACT,=C'ZZZ'    PASSIVE ?                                    
         BE    BUY05               YES - READ SEQ                               
         CLC   PBUYKEST,SVQELOW    ESTIMATE IN RANGE?                           
         BL    BUY05               NO - READ SEQ                                
         CLC   PBUYKEST,SVQEHI     ESTIMATE IN RANGE?                           
         BH    BUY05               NO - READ SEQ                                
         DROP  R5                  DROP BUY KEY USING                           
*                                                                               
         LA    R0,PBUYREC          R0 = PBUYREC                                 
         ST    R0,AREC             AREC = PBUYREC                               
         GOTO1 GETPRT              READ BUY RECORD                              
*                                                                               
         MVI   ELEMBILL,X'26'      DEFAULT BILL ELEMENT                         
         CLI   QOPT4,C'G'          GENERIC EDI?                                 
         BE    BUY13               YES - SEND BOTH X'26' & X'28' ELEMS          
*                                                                               
         LA    R2,PBUYREC          R2 = BUY RECORD                              
         LA    R2,33(R2)           A(FIRST BUY ELEMENT)                         
*                                                                               
BUY11    CLI   0(R2),0             END OF BUY RECORD?                           
         BE    BUY13               YES - READ SEQ                               
         CLI   0(R2),X'28'         OPEN RATE BILL ELEMENT (PBILELEM)?           
         BNE   BUY12               NO                                           
         MVI   ELEMBILL,X'28'      SEARCH FOR OPEN RATE BILL ELEMENT            
         B     BUY13               YES                                          
BUY12    LLC   RE,1(R2)            ELEMENT LENGTH                               
         AR    R2,RE               BUMP TO NEXT ELEMENT                         
         B     BUY11               CHECK NEXT ELEMENT                           
*                                                                               
BUY13    LA    R2,PBUYREC          R2 = BUY RECORD                              
         LA    R2,33(R2)           A(FIRST BUY ELEMENT)                         
                                                                                
BUY15    CLI   0(R2),0             END OF BUY RECORD?                           
         BNE   BUY15A              NO                                           
         CLI   QOPT4,C'G'          GENERIC EDI?                                 
         BNE   BUY05               NO - READ SEQ                                
         CLI   ELEMBILL,X'28'      SEARCHED FOR OPEN RATE BILL ELEMENT?         
         BE    BUY05               YES - READ SEQ                               
         MVI   ELEMBILL,X'28'      SEARCH FOR OPEN RATE BILL ELEMENT            
         B     BUY13               START CHECKING BUY AGAIN                     
*                                                                               
BUY15A   CLC   0(1,R2),ELEMBILL    BILL ELEMENT (PBILELEM)?                     
         BE    BUY17               YES                                          
BUY16    LLC   RE,1(R2)            ELEMENT LENGTH                               
         AR    R2,RE               BUMP TO NEXT ELEMENT                         
         B     BUY15               CHECK NEXT ELEMENT                           
*                                                                               
         USING PBILELEM,R2         BILL ELEMENT DSECT                           
BUY17    OC    PBLDATE,PBLDATE     BILLED?                                      
         BZ    BUY16               NO - GET NEXT BILL ELEMENT                   
         CLC   PBLDATE,BQS         BILL DATE BEFORE REQ START?                  
         BL    BUY16               YES - GET NEXT BILL ELEMENT                  
         CLC   PBLDATE,BQE         BILL DATE AFTER REQ END?                     
         BH    BUY16               YES - GET NEXT BILL ELEMENT                  
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R5,WORK             R5 = WORK                                    
PK       USING PUBREC,R5           PUB RECORD DSECT                             
         MVC   PK.PUBKMED,PBUYKMED MEDIA                                        
         MVC   PK.PUBKPUB,PBUYKPUB PUB                                          
         MVC   PK.PUBKZON,PBUYKZON ZONE                                         
         MVC   PK.PUBKED,PBUYKEDT  EDITION                                      
         MVC   PK.PUBKAGY,PBUYKAGY AGENCY                                       
         MVI   PK.PUBKCOD,X'81'    PUB RECORD CODE                              
         CLC   PK.PUBKEY,PUBKEY    ALREADY HAVE THE PUB REC IN CORE?            
         BE    BUY20               YES                                          
*                                                                               
         LA    R5,KEY              R5 = KEY                                     
         MVC   PK.PUBKEY,WORK      PUB KEY                                      
         GOTO1 HIGHPUB             READ HIGH FOR THE PUB RECORD                 
         CLC   PK.PUBKEY,KEYSAVE   DID WE FIND THE PUB KEY?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  PK                  DROP LABELED PUB REC USING                   
*                                                                               
         LA    R0,PUBREC           R0 = PUBREC                                  
         ST    R0,AREC             AREC = PUBREC                                
         GOTO1 GETNAME             GET PUB RECORD                               
*                                                                               
         MVC   KEY(25),PBUYKEY     RE-READ THE BUY RECORD                       
         GOTO1 HIGH                READ HIGH                                    
         CLC   PBUYKEY,KEYSAVE     DID WE FIND THE BUY RECORD?                  
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
*                                                                               
BUY20    ST    R2,SVBILEL          SAVE ADDRESS OF PBILELEM                     
         L     RF,VBUYR            A(AGENCY SPECIFIC BUY ROUTINE)               
         GOTO1 (RF),DMCB,(RA)      PROCESS AGENCY SPECIFIC BUY ROUTINE          
         B     BUY16               GET NEXT BILL ELEMENT                        
*                                                                               
BUYXIT   XIT1                      EXIT                                         
         DROP  R2                  DROP BILL ELEMENT USING                      
*                                                                               
         GETELN R2,33,ELCODE1,1    GETEL1 MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'ULBILLR - PROCESS BILL RECORDS FOR UNILEVER'                    
ULBILLR  CSECT                                                                  
         NMOD1 0,ULBILLR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R0,BINKEY           BINSRCH KEY                                  
         ST    R0,ABINKEY          A(BINSRCH KEY)                               
*                                                                               
         LA    R5,BINKEY           R5 = BINSRCH KEY                             
         USING ULBKEYD,R5          BILL BINSRCH KEY DSECT                       
         MVC   ULBMED,PBILKMED     MEDIA                                        
         MVC   ULBCLT,PBILKCLT     CLIENT CODE                                  
         MVC   ULBPRD,PBILKPRD     PRODUCT CODE                                 
         MVC   ULBEST,PBILKEST     ESTIMATE                                     
         MVC   ULBINVM,PBILKBMN+1  BILLING MONTH                                
         MVC   ULBINVN,PBILKBNO    INVOICE NUMBER                               
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         LA    R5,BINREC           R5 = BINSRCH RECORD                          
         USING ULBRECD,R5          BILL BINSRCH KEY DSECT                       
         MVC   ULBREC,SPACES       SPACE-FILL OUTPUT RECORD                     
         MVC   ULBNET,PBILLRCV     NET AMOUNT                                   
         MVC   ULBGROSS,PBILLRCV   GROSS AMOUNT                                 
*                                                                               
         GOTO1 VFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),(PBILKMED,B1PROF),   X        
               B1XPROF                                                          
*                                                                               
         L     RF,DMCB             RF = DMCB                                    
         MVC   ULBINV,0(RF)        FULL INVOICE NUMBER                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILINVD),(20,WORK)                               
*                                                                               
         LA    R4,ULBINVD          INVOICE DATE                                 
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILDUED),(20,WORK)                               
*                                                                               
         LA    R4,ULBINVDD         INVOICE DUE DATE                             
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
PK       USING PPRDREC,R6          PRODUCT RECORD DSECT                         
         MVC   PK.PPRDKAGY,QAGENCY AGENCY                                       
         MVC   PK.PPRDKMED,QMEDIA  MEDIA                                        
         MVI   PK.PPRDKRCD,X'06'   PRODUCT RECORD CODE                          
         MVC   PK.PPRDKCLT,QCLIENT CLIENT CODE                                  
         MVC   PK.PPRDKPRD,PBILKPRD PRODUCT CODE                                
         CLC   PK.PPRDKEY,PPRDKEY  ALREADY HAVE THE PRD REC IN CORE?            
         BE    ULBILL10            YES                                          
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   PK.PPRDKEY,WORK     PRODUCT KEY                                  
         GOTO1 HIGH                READ HIGH                                    
         CLC   PK.PPRDKEY,KEYSAVE  DID WE FIND THE PRODUCT KEY?                 
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  PK                  DROP LABELED PRODUCT REC USING               
*                                                                               
         LA    R0,PPRDREC          R0 = A(PPRDREC)                              
         ST    R0,AREC             AREC = PPRDREC                               
         GOTO1 GETPRT              GET THE PRODUCT RECORD                       
*                                                                               
ULBILL10 MVC   ULBPNAME,PPRDNAME   PRODUCT NAME                                 
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
E        USING PESTREC,R6          ESTIMATE RECORD DSECT                        
         MVC   E.PESTKAGY,QAGENCY  AGENCY                                       
         MVC   E.PESTKMED,QMEDIA   MEDIA                                        
         MVI   E.PESTKRCD,X'07'    ESTIMATE RECORD CODE                         
         MVC   E.PESTKCLT,QCLIENT  CLIENT CODE                                  
         MVC   E.PESTKPRD,PBILKPRD PRODUCT CODE                                 
         MVC   E.PESTKEST,PBILKEST ESTIMATE CODE                                
         CLC   E.PESTKEY,PESTKEY   ALREADY HAVE THE EST RECORD IN CORE?         
         BE    ULBILL20            YES                                          
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   E.PESTKEY,WORK      ESTIMATE KEY                                 
         GOTO1 HIGH                READ HIGH                                    
         CLC   E.PESTKEY,KEYSAVE   DID WE FIND THE ESTIMATE KEY?                
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  E                   DROP LABELED ESTIMATE REC USING              
*                                                                               
         LA    R0,PESTREC          R0 = A(PESTREC)                              
         ST    R0,AREC             AREC = PESTREC                               
         GOTO1 GETPRT              GET THE ESTIMATE RECORD                      
*                                                                               
ULBILL20 MVC   ULBENAME,PESTNAME   ESTIMATE NAME                                
*                                                                               
         LA    R2,PESTREC          ESTIMATE RECORD                              
         USING PESTUDEF,R2         ESTIMATE USER DESCRIPTION DSECT              
         MVI   ELCODE,X'08'        ESTIMATE USER DESCRIPTION ELEMENT            
         BAS   RE,GETEL            HAVE ONE?                                    
         BNE   *+10                NO - LEAVE BLANK                             
         MVC   ULBEUDF2,PEUSER2    ESTIMATE UDEF2                               
         DROP  R2                  DROP EST USER DESCRIPTION USING              
*                                                                               
         CLC   PBILLKEY,KEY        DID WE BREAK THE READ SEQUENCE?              
         BE    ULBILL30            NO                                           
*                                                                               
         MVC   KEY(25),PBILLKEY    RE-READ THE BILL RECORD                      
         GOTO1 HIGH                READ HIGH                                    
         CLC   PBILLKEY,KEYSAVE    DID WE FIND THE BILL RECORD?                 
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
*                                                                               
ULBILL30 ZAP   DUB,PBILLRCV        GET PBILLRCV INTO DUB                        
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) TOTAL               
*                                                                               
         MVI   ABINKEY,1           RESET TO INSERT RECORD IF NOT FOUND          
         GOTO1 VBINSRCH,ABINKEY    CALL BINSRCH                                 
*                                                                               
         CLI   ABINKEY,1           RECORD INSERTED?                             
         BE    ULBILL40            YES - DONE                                   
*                                                                               
         OC    1(3,R1),1(R1)       HAVE A(BINSRCH REC)?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - TABLE IS FULL - EXPAND TABLE!           
         DROP  R5                  DROP BINSRCH REC USING                       
*                                                                               
         L     RF,ABINKEY          ADDRESS OF FOUND RECORD                      
         LA    RF,L'ULBKEY(RF)     PAST KEY                                     
         USING ULBRECD,RF          BILL SORT RECORD DSECT                       
         AP    ULBNET,PBILLRCV     NET AMT (DUP KEY MUST ADD FIELD)             
         AP    ULBGROSS,PBILLRCV   GROSS AMT (DUP KEY MUST ADD FIELD)           
         B     ULBILLX             DO NOT UPDATE INVOICE COUNT                  
         DROP  RF                  DROP BILL SORT USING                         
*                                                                               
ULBILL40 AP    INVCNT,=P'1'        UPDATE INVOICE COUNT                         
*                                                                               
ULBILLX  XIT1                      EXIT                                         
*                                                                               
         GETEL R2,33,ELCODE        GETEL MACRO                                  
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'ULBUYR - PROCESS BUY RECORDS FOR UNILEVER'                      
ULBUYR   CSECT                                                                  
         NMOD1 0,ULBUYR                                                         
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R5,BINKEY           R5 = BINSRCH KEY                             
         USING ULBKEYD,R5          BILL BINSRCH KEY DSECT                       
         L     R2,SVBILEL          ADDRESS OF BILLING ELEMENT                   
         USING PBILELEM,R2         BILL ELEMENT DSECT                           
         XC    BINKEY,BINKEY       CLEAR BINSRCH KEY                            
         MVC   ULBMED,PBUYKMED     MEDIA                                        
         MVC   ULBCLT,PBUYKCLT     CLIENT CODE                                  
         MVC   ULBPRD,PBPRD        PRODUCT CODE FROM BILLING ELEMENT            
         MVC   ULBEST,PBUYKEST     ESTIMATE                                     
         MVC   ULBINVM,PBLDATE+1   BILLING MONTH                                
         MVC   ULBINVN,PBINVNO     INVOICE NUMBER                               
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
         MVC   SORTMED,PBUYKMED    MEDIA                                        
         MVC   SORTCLT,PBUYKCLT    CLIENT                                       
         MVC   SORTPRD,PBPRD       PRODUCT CODE FROM BILLING ELEMENT            
         MVC   SORTEST,PBUYKEST    ESTIMATE                                     
         MVC   SORTINV,PBINVNO     PACKED INVOICE NUMBER                        
         MVC   SORTBDAT,PBLDATE    BILLED DATE COMPRESSED                       
*                                                                               
         IC   R0,PAGYPROF+12       FOR PUBEDIT                                  
         GOTO1 PUBEDIT,DMCB,((R0),PUBREC+1),SORTPUB                             
*                                                                               
         MVC   SORTIGRS,ULBGROSS   GROSS AMOUNT DUE                             
         MVC   SORTINET,ULBNET     NET AMOUNT DUE                               
         MVC   SORTIINV,ULBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SORTIDAT,ULBINVD    INVOICE DATE AS ON BILL                      
         MVC   SORTIDDT,ULBINVDD   INVOICE DUE DATE AS ON BILL                  
         MVC   SORTUDF2,ULBEUDF2   ESTIMATE UDEF 2                              
         MVC   SORTPNAM,ULBPNAME   PRODUCT NAME                                 
         MVC   SORTENAM,ULBENAME   ESTIMATE NAME                                
         DROP  R5                  DROP BINSRCH REC USING                       
*                                                                               
         MVC   SORTMEDN,PAGYMED    MEDIA NAME                                   
         CLI   PBUYKMED,C'I'       MEDIA I?                                     
         BNE   *+10                NO                                           
         MVC   SORTMDNI,=C'INTERACTIVE'  YES - SET INTERACTIVE                  
*                                                                               
         MVC   SORTCNAM,PCLTNAME   CLIENT NAME                                  
*                                                                               
         MVC   WORK(3),PBDBDATE    BILLABLE DATE                                
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         GOTO1 DATCON,DMCB,(3,WORK),(6,SORTMOS)  MMM/YY                         
*                                                                               
         LA    R3,PUBREC           R3 = PUBREC                                  
         USING PUBNAMEL,R3         PUB NAME ELEMENT                             
         MVI   ELCODE2,X'10'       PUB NAME ELEMENT CODE                        
         BAS   RE,GETEL2           DO WE HAVE THE ELEMENT?                      
         BNE   *+10                NO                                           
         MVC   SORTPUBN,PUBNAME    YES - GET THE VENDOR NAME                    
         DROP  R3                  DROP PUB NAME USING                          
*                                                                               
         MVC  FULL,PBGROSS         GROSS                                        
         L    R0,FULL              R0 = GROSS                                   
         MVC  FULL,PBAGYCOM        AGENCY COMMISION                             
         S    R0,FULL              R0 = GROSS-AGY COMMSSION                     
         MVC  FULL,PBCSHDSC        CASH DISCOUNT BILLED                         
         S    R0,FULL              VENDOR NET AMOUNT                            
         STCM R0,15,SORTBNET       VENDOR NET AMOUNT                            
         DROP  R2,R4               DROP BILLING ELEM & SORT REC USINGS          
*                                                                               
         CVD  R0,DUB               CONVERT TO PACKED                            
         AP   DTLTOT,DUB           SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
ULBUYX   XIT1                      EXIT                                         
*                                                                               
         GETELN R3,33,ELCODE2,2    GETEL2 MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'ULOUTPR - OUTPUT ROUTINE UNILEVER'                              
ULOUTPR  CSECT                                                                  
         NMOD1 0,ULOUTPR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
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
         GOTO1 VTAPEWRT,DMCB,(RA)  WRITE RECORD TO TAPE                         
*                                                                               
ULOUT10  GOTO1 VSORTER,DMCB,=C'GET' GET SORT RECORD                             
*                                                                               
         ICM   R5,15,4(R1)         ANY MORE RECORDS FROM SORTER?                
         BZ    ULOUTXIT            NO - DONE                                    
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         MVC   SORTREC,0(R5)       MOVE OUTPUT TO SORTREC                       
*                                                                               
         LA    R5,SORTREC          R5 = SORTREC                                 
         USING SORTRECD,R5         SORTREC DSECT                                
*                                                                               
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         XCEF                                                                   
*                                                                               
         LA    R6,P1               POINT TO PRINT LINE                          
         USING BLINED,R6           BLINED DSECT TO COVER PRINT LINE             
         MVC   BLMEDIA,SORTMED     MEDIA                                        
         MVC   BLCLT,SORTCLT       CLIENT                                       
         MVC   BLCLNAME,SORTCNAM   CLIENT NAME                                  
         MVC   BLPRD,SORTPRD       PRODUCT                                      
         EDIT  SORTEST,(3,BLEST),FILL=0                                         
         MVC   BLINVNO,SORTIINV    FULL INVOICE NUMBER                          
         MVC   BLINVMOS,SORTMOS    MOS MMM/YY                                   
         MVC   BLINVD,SORTIDAT     INVOICE DATE                                 
         EDIT  SORTINET,(13,BLNET),2,MINUS=YES                                  
         MVC   BLPUB,SORTPUB       PUB NUMBER                                   
         MVC   BLPUBNAM,SORTPUBN   PUB NAME                                     
         OC    BLINE,SPACES        SPACE PAD                                    
         EDIT  SORTBNET,(13,BLVAMT),2,MINUS=YES                                 
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R6                  DROP PRINT LINE USING                        
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
         CLC   SORTUDF2,SPACES     HAVE UDEF DATA?                              
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
         LA    R1,L'SORTIDDT       LENGTH OF INV DUE DATE (FIELD 23             
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
         MVC   0(L'SORTPUB,R1),SORTPUB  VENDOR CODE/PUB NUMBER                  
         BAS   RE,FINDEND          LAST CHAR OF DATA RETURNED IN R1             
*                                                                               
         MVC   2(L'SORTPUBN,R1),SORTPUBN  VENDOR/PUB NAME                       
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
         GOTO1 VTAPEWRT,DMCB,(RA)  WRITE RECORD TO TAPE                         
*                                                                               
         B     ULOUT10             GET NEXT SORT RECORD                         
         DROP  R5                  DROP SORT RECORD USING                       
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
         TITLE 'BLBILLR - PROCESS BILL RECORDS FOR BELL'                        
BLBILLR  CSECT                                                                  
         NMOD1 0,BLBILLR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R0,BINKEYBL         BINSRCH KEY                                  
         ST    R0,ABINKEY          A(BINSRCH KEY)                               
*                                                                               
         LA    R5,BINKEYBL         R5 = BINSRCH KEY                             
         USING BLBKEYD,R5          BILLING SORT KEY DSECT                       
         MVC   BLBMED,PBILKMED     MEDIA                                        
         MVC   BLBCLT,PBILKCLT     CLIENT CODE                                  
         MVC   BLBPRD,PBILKPRD     PRODUCT CODE                                 
         MVC   BLBEST,PBILKEST     ESTIMATE                                     
         MVC   BLBINVM,PBILKBMN+1  BILLING MONTH                                
         MVC   BLBINVN,PBILKBNO    INVOICE NUMBER                               
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         LA    R5,BINRECBL         R5 = BINSRCH RECORD                          
         USING BLBRECD,R5          BILL BINSRCH KEY DSECT                       
         MVC   BLBREC,SPACES       SPACE-FILL OUTPUT RECORD                     
         MVC   BLBTYPE,PBILLTYP    BILL TYPE (B1,B2,B3,B4,B5,ETC)               
*                                                                               
         GOTO1 VFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),(PBILKMED,B1PROF),   X        
               B1XPROF                                                          
*                                                                               
         L     RF,DMCB             RF = DMCB                                    
         MVC   BLBINV,0(RF)        FULL INVOICE NUMBER                          
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*                                                                               
         MVC   BLBNET,PPBVEBN      GROSS - AC - CD                              
         AP    BLBNET,PPBVEBC      ADD BACK CASH DISCOUNT                       
         CLI   PBILKMED,C'O'       MEDIA O?                                     
         BNE   BLBILL05            NO                                           
         CLC   BLBTYPE,=C'B6'      BILL TYPE B6?                                
         BE    *+14                YES                                          
         CLC   BLBTYPE,=C'B7'      BILL TYPE B7?                                
         BNE   *+10                NO                                           
         MVC   BLBNET,PPBVEBG      GROSS                                        
*                                                                               
BLBILL05 L     R0,PPBVGST          GST FROM PPBVAL                              
         CVD   R0,DUB              CVD                                          
         ZAP   BLBGST,DUB          GST - PACKED                                 
*                                                                               
         L     R0,PPBVPST          PST FROM PPBVAL                              
         CVD   R0,DUB              CVD                                          
         ZAP   BLBPST,DUB          PST - PACKED                                 
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILINVD),(20,WORK)                               
*                                                                               
         LA    R4,BLBINVD          INVOICE DATE                                 
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
PK       USING PPRDREC,R6          PRODUCT RECORD DSECT                         
         MVC   PK.PPRDKAGY,QAGENCY AGENCY                                       
         MVC   PK.PPRDKMED,QMEDIA  MEDIA                                        
         MVI   PK.PPRDKRCD,X'06'   PRODUCT RECORD CODE                          
         MVC   PK.PPRDKCLT,QCLIENT CLIENT CODE                                  
         MVC   PK.PPRDKPRD,PBILKPRD PRODUCT CODE                                
         CLC   PK.PPRDKEY,PPRDKEY  ALREADY HAVE THE PRD REC IN CORE?            
         BE    BLBILL10            YES                                          
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   PK.PPRDKEY,WORK     PRODUCT KEY                                  
         GOTO1 HIGH                READ HIGH                                    
         CLC   PK.PPRDKEY,KEYSAVE  DID WE FIND THE PRODUCT KEY?                 
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  PK                  DROP LABELED PRODUCT REC USING               
*                                                                               
         LA    R0,PPRDREC          R0 = A(PPRDREC)                              
         ST    R0,AREC             AREC = PPRDREC                               
         GOTO1 GETPRT              GET THE PRODUCT RECORD                       
*                                                                               
BLBILL10 MVC   BLBPNAME,PPRDNAME   PRODUCT NAME                                 
         OC    BLBPNAME,SPACES     SPACE PAD                                    
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
D        USING PDIVREC,R6          DIVISION RECORD DSECT                        
         MVC   D.PDIVKAGY,QAGENCY  AGENCY                                       
         MVC   D.PDIVKMED,QMEDIA   MEDIA                                        
         MVI   D.PDIVKRCD,X'03'    X'03' DIVISION RECORD ID                     
         MVC   D.PDIVKCLT,QCLIENT  CLIENT CODE                                  
         MVC   D.PDIVKDIV,PNBPGR1  DIVISION CODE                                
         OC    PNBPGR1,PNBPGR1     HAVE DIVISION CODE ON BILL RECORD?           
         BNZ   *+10                YES                                          
         MVC   D.PDIVKDIV,PPRDDIV  NO - USE DIVISION CODE FROM PRD REC          
         CLC   D.PDIVKEY,PDIVKEY   ALREADY HAVE THE DIV RECORD IN CORE?         
         BE    BLBILL15            YES                                          
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   D.PDIVKEY,WORK      MOVE DIV KEY TO KEY                          
         GOTO1 HIGH                READ HIGH                                    
         CLC   D.PDIVKEY,KEYSAVE   DID WE FIND THE DIV KEY?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  D                   DROP LABELED ESTIMATE REC USING              
*                                                                               
         LA    R0,PDIVREC          R0 = A(PDIVREC)                              
         ST    R0,AREC             AREC = PDIVREC                               
         GOTO1 GETPRT              GET THE DIV RECORD                           
*                                                                               
BLBILL15 MVC   BLBDIVC,PDIVKDIV    DIVISION CODE                                
         MVC   BLBDIVN,PDIVNAME    DIVISION NAME                                
         MVC   BLBREGN,PNBMGR1     REGION FROM BILL (MAY BE NULL)               
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
E        USING PESTREC,R6          ESTIMATE RECORD DSECT                        
         MVC   E.PESTKAGY,QAGENCY  AGENCY                                       
         MVC   E.PESTKMED,QMEDIA   MEDIA                                        
         MVI   E.PESTKRCD,X'07'    ESTIMATE RECORD CODE                         
         MVC   E.PESTKCLT,QCLIENT  CLIENT CODE                                  
         MVC   E.PESTKPRD,PBILKPRD PRODUCT CODE                                 
         MVC   E.PESTKEST,PBILKEST ESTIMATE CODE                                
         CLC   E.PESTKEY,PESTKEY   ALREADY HAVE THE EST RECORD IN CORE?         
         BE    BLBILL20            YES                                          
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   E.PESTKEY,WORK      ESTIMATE KEY                                 
         GOTO1 HIGH                READ HIGH                                    
         CLC   E.PESTKEY,KEYSAVE   DID WE FIND THE ESTIMATE KEY?                
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  E                   DROP LABELED ESTIMATE REC USING              
*                                                                               
         LA    R0,PESTREC          R0 = A(PESTREC)                              
         ST    R0,AREC             AREC = PESTREC                               
         GOTO1 GETPRT              GET THE ESTIMATE RECORD                      
*                                                                               
BLBILL20 MVC   BLBENAME,PESTNAME   ESTIMATE NAME                                
         OC    BLBENAME,SPACES     SPACE PAD                                    
*                                                                               
         CLI   PBILKMED,C'O'       MEDIA O?                                     
         BNE   BLBILL25            NO                                           
         CLC   BLBTYPE,=C'B6'      BILL TYPE B6?                                
         BE    *+14                YES                                          
         CLC   BLBTYPE,=C'B7'      BILL TYPE B7?                                
         BNE   BLBILL25            NO                                           
         CLI   PESTRTYP,C'C'       'C' RATE ON ESTIMATE?                        
         BE    BLBILL25            YES - THIS IS THE EXPECTATION                
*                                                                               
         MVC   P1+1(41),=C'*****************************************'           
         MVC   P2+1(41),=C'* WARNING: INCORRECT BILL FORMULA FOUND *'           
         MVC   P3+1(41),=C'*****************************************'           
*                                                                               
         GOTO1 REPORT              PRINT THE WARNING MESSAGE                    
*                                                                               
         MVC   WORK(2),PBILKMOS    MOS                                          
         MVI   WORK+2,X'01'        FIRST DAY                                    
         GOTO1 DATCON,DMCB,(3,WORK),(6,P1)                                      
         MVC   P1+07(1),PBILKMED   MEDIA                                        
         MVC   P1+09(3),PBILKCLT   CLIENT                                       
         MVC   P1+13(3),PBILKPRD   PRODUCT                                      
         EDIT  PBILKEST,(3,P1+17),FILL=0                                        
         MVC   P1+21(10),BLBINV    INVOICE                                      
*                                                                               
         GOTO1 REPORT              PRINT THE INVOICE DETAILS                    
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
BLBILL25 GOTO1 =A(READUCOM),DMCB,(RA) READ UCOMM DATA                           
*                                                                               
         MVC   BLBEUCOM,SVEUCOM1   ESTIMATE UCOMM 1                             
         MVC   BLBEUCM3,SVEUCOM3   ESTIMATE UCOMM 3                             
         MVC   BLBEUCM4,SVEUCOM4   ESTIMATE UCOMM 4                             
*                                                                               
         MVC   KEY(25),PBILLKEY    RE-READ THE BILL RECORD                      
         GOTO1 HIGH                READ HIGH                                    
         CLC   PBILLKEY,KEYSAVE    DID WE FIND THE BILL RECORD?                 
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
*                                                                               
         ZAP   DUB,PBILLRCV        GET PBILLRCV INTO DUB                        
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) TOTAL               
*                                                                               
         MVI   ABINKEY,1           RESET TO INSERT RECORD IF NOT FOUND          
         GOTO1 VBINSRCH,ABINKEY    CALL BINSRCH                                 
*                                                                               
         CLI   ABINKEY,1           RECORD INSERTED?                             
         BE    BLBILL40            YES - DONE                                   
*                                                                               
         OC    1(3,R1),1(R1)       HAVE A(BINSRCH REC)?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - TABLE IS FULL - EXPAND TABLE!           
         DROP  R5                  DROP BINSRCH REC USING                       
*                                                                               
         L     RF,ABINKEY          ADDRESS OF FOUND RECORD                      
         LA    RF,L'BLBKEY(RF)     PAST KEY                                     
         USING BLBRECD,RF          BILL SORT RECORD DSECT                       
         CLI   PBILKMED,C'O'       MEDIA O?                                     
         BNE   BLBILL35            NO                                           
         CLC   BLBTYPE,=C'B6'      BILL TYPE B6?                                
         BE    *+14                YES                                          
         CLC   BLBTYPE,=C'B7'      BILL TYPE B7?                                
         BNE   BLBILL35            NO                                           
         AP    BLBNET,PPBVEBG      GROSS                                        
         B     BLBILL36            DO NOT UPDATE INVOICE COUNT                  
*                                                                               
BLBILL35 AP    BLBNET,PPBVEBN      NET AMT (DUP KEY MUST ADD FIELD)             
         AP    BLBNET,PPBVEBC      ADD BACK CASH DISCOUNT                       
*                                                                               
BLBILL36 L     R0,PPBVGST          GST FROM PPBVAL                              
         CVD   R0,DUB              CVD                                          
         AP    BLBGST,DUB          GST - (DUP KEY MUST ADD FIELD)               
         L     R0,PPBVPST          PST FROM PPBVAL                              
         CVD   R0,DUB              CVD                                          
         AP    BLBPST,DUB          PST - (DUP KEY MUST ADD FIELD)               
         B     BLBILLX             DO NOT UPDATE INVOICE COUNT                  
         DROP  RF                  DROP BILL SORT USING                         
*                                                                               
BLBILL40 AP    INVCNT,=P'1'        UPDATE INVOICE COUNT                         
*                                                                               
BLBILLX  XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'BLBUYR - PROCESS BUY RECORDS FOR BELL'                          
BLBUYR   CSECT                                                                  
         NMOD1 0,BLBUYR                                                         
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R5,BINKEYBL         R5 = BINSRCH KEY                             
         USING BLBKEYD,R5          BILL BINSRCH KEY DSECT                       
         L     R2,SVBILEL          ADDRESS OF BILLING ELEMENT                   
         USING PBILELEM,R2         BILL ELEMENT DSECT                           
         XC    BINKEYBL,BINKEYBL   CLEAR BINSRCH KEY                            
         MVC   BLBMED,PBUYKMED     MEDIA                                        
         MVC   BLBCLT,PBUYKCLT     CLIENT CODE                                  
         MVC   BLBPRD,PBPRD        PRODUCT CODE FROM BILLING ELEMENT            
         MVC   BLBEST,PBUYKEST     ESTIMATE                                     
         MVC   BLBINVM,PBLDATE+1   BILLING MONTH                                
         MVC   BLBINVN,PBINVNO     INVOICE NUMBER                               
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
         MVC   SRTBMED,PBUYKMED    MEDIA                                        
         MVC   SRTBCLT,PBUYKCLT    CLIENT                                       
         MVC   SRTBPRD,PBPRD       PRODUCT CODE FROM BILLING ELEMENT            
         MVC   SRTBEST,PBUYKEST    ESTIMATE                                     
         MVC   SRTBINV,PBINVNO     PACKED INVOICE NUMBER                        
         MVC   SRTBBDAT,PBLDATE    BILLED DATE COMPRESSED                       
         DROP  R2                  DROP BILL ELEM USING                         
*                                                                               
         IC    R0,PAGYPROF+12      FOR PUBEDIT                                  
         GOTO1 PUBEDIT,DMCB,((R0),PUBREC+1),SRTBPUB                             
*                                                                               
         MVC   SRTBINET,BLBNET     NET AMOUNT DUE                               
         MVC   SRTBIGST,BLBGST     GST AMOUNT DUE                               
         MVC   SRTBIPST,BLBPST     PST AMOUNT DUE                               
         MVC   SRTBTYPE,BLBTYPE    BILL TYPE (B1,B2,B3,B4,B5,ETC)               
         MVC   SRTBIINV,BLBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SRTBIDAT,BLBINVD    INVOICE DATE AS ON BILL                      
         MVC   SRTBUCM4,BLBEUCM4   ESTIMATE UCOMM 4                             
         MVC   SRTBUCM3,BLBEUCM3   ESTIMATE UCOMM 3                             
         MVC   SRTBUCOM,BLBEUCOM   ESTIMATE UCOMM 1                             
         MVC   SRTBPNAM,BLBPNAME   PRODUCT NAME                                 
         MVC   SRTBDIVC,BLBDIVC    DIVISION CODE                                
         MVC   SRTBDIVN,BLBDIVN    DIVISION CODE NAME                           
         MVC   SRTBENAM,BLBENAME   ESTIMATE NAME                                
*                                                                               
         MVC   SRTBMEDN,PAGYMED    MEDIA NAME                                   
         CLI   PBUYKMED,C'I'       MEDIA I?                                     
         BNE   *+10                NO                                           
         MVC   SRTBMDNI,=C'INTERACTIVE' YES - SET INTERACTIVE                   
         OC    SRTBMDNI,SPACES     SPACE PAD                                    
*                                                                               
         MVC   SRTBCNAM,PCLTNAME   CLIENT NAME                                  
         OC    SRTBCNAM,SPACES     SPACE PAD                                    
*                                                                               
         MVC   WORK(3),PBDBDATE    BILLABLE DATE                                
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         MVC   SRTBMOSP,WORK       SAVE PACKED MOS                              
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTBMOS)  MMM/YY                         
*                                                                               
         LA    R3,PUBREC           R3 = PUBREC                                  
         USING PUBNAMEL,R3         PUB NAME ELEMENT                             
         MVI   ELCODE3,X'10'       PUB NAME ELEMENT CODE                        
         BAS   RE,GETEL3           DO WE HAVE THE ELEMENT?                      
         BNE   *+10                NO                                           
         MVC   SRTBPUBN,PUBNAME    YES - GET THE VENDOR NAME                    
         DROP  R3                  DROP PUB NAME USING                          
*                                                                               
         OC    SRTBPUBN,SPACES     SPACE PAD VENDOR NAME                        
*                                                                               
         XC    REGTAB,REGTAB       CLEAR REGION TABLE                           
         MVC   REGTAB(3),BLBREGN   REGION FROM BILLING                          
         CLC   BLBREGN,SPACES      HAVE REGION FROM BILLING?                    
         BH    BLBUY10             YES                                          
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R6,KEY              R6 = KEY                                     
L        USING LTLREC,R6           PUB SUPPLEMENTAL REC                         
         MVC   L.LTLKMED,PBUYKMED  MEDIA                                        
         MVC   L.LTLKPUB,PBUYKPUB  PUBLICATION CODE                             
         MVC   L.LTLKZON,PBUYKZON  ZONE                                         
         MVC   L.LTLKED,PBUYKEDT   EDITION                                      
         MVC   L.LTLKAGY,QAGENCY   AGENCY                                       
         MVI   L.LTLKCOD,X'85'     PUB SUPPLEMENTAL REC ID                      
*                                                                               
         GOTO1 HIGHPUB             READ HIGH ON PUBFILE                         
*                                                                               
         CLC   L.LTLKEY,KEYSAVE    DID WE FIND THE PUB SUPPLEMENT KEY?          
         BNE   BLBUY10             NO - DONE                                    
         DROP  L                   DROP LABELED PUB SUPPLEMENT USING            
*                                                                               
         L     R3,ALTLREC          READ PUB SUPPLEMENT RECORD HERE              
         USING PUBDSTEL,R3         PUB REG/DIST ELEM                            
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,(R3),(0,DMWORK)               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLBUY05  LA    R2,REGTAB           REGION TABLE                                 
         MVI   ELCODE3,X'71'       PUB NAME ELEMENT CODE                        
         BAS   RE,GETEL3           DO WE HAVE THE ELEMENT?                      
         B     *+8                 GO TEST CC                                   
*                                                                               
BLBUY06  BAS   RE,NEXTEL3          HAVE REG/DIST ELEM?                          
         BNE   BLBUY10             NO - DONE                                    
*                                                                               
BLBUY08  CLC   PUBDCLT,PBUYKCLT    MATCH ON CLIENT CODE?                        
         BNE   BLBUY06             NO                                           
         CLC   PUBDDIV,SRTBDIVC    MATCH ON DIVISION CODE?                      
         BNE   BLBUY06             NO                                           
         MVC   0(3,R2),PUBDREG     SAVE REGION                                  
         MVC   3(2,R2),PUBDSHR     SAVE REGION SHARE                            
         LA    R2,5(R2)            BUMP TO NEXT SLOT                            
         LA    RF,REGTABX          EOT                                          
         CR    R2,RF               PAST EOT?                                    
         BNH   BLBUY06             NO                                           
         DC    H'0'                YES - EXPAND TABLE                           
         DROP  R3                  DROP PUB REG/DIST USING                      
*                                                                               
BLBUY10  LA    R2,REGTAB           REGION TABLE                                 
         MVI   DIVERR,C'N'         INIT DIV ERR PRINTED FLAG                    
*                                                                               
BLBUY11  ST    R2,CURREG           A(CURRENT REGION)                            
***      MVC   BLBREGN,0(R2)       REGION                                       
         MVC   SRTBREGN,=CL20'MASTER REGION'                                    
         OC    0(3,R2),0(R2)       ANY REGION?                                  
         BZ    BLBUY21             NO                                           
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
R        USING PREGREC,R6          REGION RECORD DSECT                          
         MVC   R.PREGKAGY,QAGENCY  AGENCY                                       
         MVC   R.PREGKMED,PBUYKMED MEDIA                                        
         MVI   R.PREGKRCD,X'04'    REGION RECORD ID                             
         MVC   R.PREGKCLT,PBUYKCLT CLIENT CODE                                  
         MVC   R.PREGKDIV,SRTBDIVC DIVISION CODE                                
***      MVC   R.PREGKREG,BLBREGN  REGION                                       
         MVC   R.PREGKREG,0(R2)    REGION                                       
         DROP  R5                  DROP BINSRCH USING                           
*                                                                               
         CLC   R.PREGKEY,PREGKEY   ALREADY HAVE REGION RECORD IN CORE?          
         BE    BLBUY20             YES                                          
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   R.PREGKEY,WORK      MOVE REGION KEY TO KEY                       
         GOTO1 HIGH                READ HIGH                                    
         CLC   R.PREGKEY,KEYSAVE   DID WE FIND THE REGION KEY?                  
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  R                   DROP LABELED REGION REC USING                
*                                                                               
         LA    R0,PREGREC          R0 = A(PREGREC)                              
         ST    R0,AREC             AREC = PREGREC                               
         GOTO1 GETPRT              GET THE REGION RECORD                        
*                                                                               
BLBUY20  MVC   SRTBREGN,PREGNAME   REGION NAME                                  
*                                                                               
BLBUY21  L     R2,SVBILEL          ADDRESS OF BILLING ELEMENT                   
         USING PBILELEM,R2         BILL ELEMENT DSECT                           
         MVC   FULL,PBGROSS        GROSS                                        
         L     R0,FULL             R0 = GROSS                                   
*                                                                               
         CLI  SRTBMED,C'O'         MEDIA O?                                     
         BNE  BLBUY21A             NO                                           
         CLC  SRTBTYPE,=C'B6'      BILLING TYPE B6?                             
         BE   BLBUY21B             YES                                          
         CLC  SRTBTYPE,=C'B7'      BILLING TYPE B7?                             
         BE   BLBUY21B             YES                                          
*                                                                               
BLBUY21A MVC   FULL,PBAGYCOM       AGENCY COMMISION                             
         S     R0,FULL             R0 = GROSS-AGY COMMSSION                     
*                                                                               
BLBUY21B DS    0H                                                               
***      MVC   FULL,PBCSHDSC       CASH DISCOUNT BILLED                         
***      S     R0,FULL             VENDOR NET AMOUNT                            
         STCM  R0,15,FULL          SAVE VENDOR NET AMOUNT IN FULL               
         DROP  R2                  DROP BILLING ELEM & SORT REC USINGS          
*                                                                               
         L     R2,CURREG           A(CURRENT REGION)                            
         OC    3(2,R2),3(R2)       WAS THIS REGION 100%?                        
         BZ    BLBUY25             YES - R0 IS ALREADY SET                      
*                                                                               
         XR    R0,R0               CLEAR R0                                     
         XR    R1,R1               CLEAR R1                                     
         ICM   R1,3,3(R2)          REGION PERCENTAGE                            
         ICM   R3,15,FULL          TOTAL VENDOR NET AMOUNT                      
         MR    R0,R3               TOTAL VENDOR NET AMOUNT * PERCENTAGE         
         D     R0,=F'10000'        DIVIDE BY 10000                              
         CHI   R0,5000             HAVE REMAINDER >= 5000?                      
         BL    *+8                 NO                                           
         AHI   R1,1                YES - ROUND UP BY 1 PENNY                    
         LR    R0,R1               R0 = VENDOR NET AMOUNT FOR REGION            
*                                                                               
BLBUY25  STCM  R0,15,SRTBBNET      VENDOR NET AMOUNT FOR REGION                 
*                                                                               
         CVD  R0,DUB               CONVERT TO PACKED                            
         AP   DTLTOT,DUB           SUM OF DETAILS                               
*                                                                               
         CLI  DIVERR,C'Y'          DID WE ALREADY PRINT A DIVISION ERR?         
         BE   BLBUY25A             YES - DO NOT PRINT IT AGAIN                  
         CLI  SRTBMED,C'O'         MEDIA O?                                     
         BNE  BLBUY25A             NO                                           
         CLC  SRTBTYPE,=C'B6'      BILLING TYPE B6?                             
         BE   *+14                 YES                                          
         CLC  SRTBTYPE,=C'B7'      BILLING TYPE B7?                             
         BNE  BLBUY25A             NO                                           
         OC   3(2,R2),3(R2)        WAS THIS REGION 100%?                        
         BZ   BLBUY25A             YES - THIS IS WHAT'S EXPECTED                
*                                                                               
         LA   R1,=C'*    WARNING: MULTIPLE REGIONS FOUND    *'                  
         BAS  RE,PRNTWARN          PRINT THE WARNING MESSAGE                    
         MVI  DIVERR,C'Y'          SET DIV ERR PRINTED FLAG                     
*                                                                               
BLBUY25A CLC  SRTBREGN,=CL20'MASTER REGION'                                     
         BNE  BLBUY26                                                           
*                                                                               
         LA   R1,=C'*      WARNING: NO REGION ASSIGNED      *'                  
*                                                                               
         BAS  RE,PRNTWARN          PRINT THE WARNING MESSAGE                    
*                                                                               
BLBUY26  GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
         OC    3(2,R2),3(R2)       WAS THIS REGION 100%?                        
         BZ    BLBUY30             YES - WE'RE DONE                             
         LA    R2,5(R2)            BUMP TO NEXT SLOT                            
         OC    0(5,R2),0(R2)       EMPTY SLOT?                                  
         BZ    BLBUY30             YES - WE'RE DONE                             
         LA    RF,REGTABX          EOT                                          
         CR    R2,RF               PAST EOT?                                    
         BNH   BLBUY11             NO - PROCESS NEXT REGION                     
*                                                                               
BLBUY30  MVC   KEY(25),PBUYKEY     RE-READ THE BUY RECORD                       
         GOTO1 HIGH                READ HIGH                                    
         CLC   PBUYKEY,KEYSAVE     DID WE FIND THE BUY RECORD?                  
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
*                                                                               
BLBUYX   XIT1                      EXIT                                         
*                                                                               
         GETELN R3,33,ELCODE3,3    GETEL3 MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
PRNTWARN NTR1                                                                   
*                                                                               
         MVC   P1+1(41),=C'*****************************************'           
         MVC   P2+1(41),0(R1)                                                   
         MVC   P3+1(41),=C'*****************************************'           
*                                                                               
         GOTO1 REPORT              PRINT THE WARNING MESSAGE                    
*                                                                               
         MVC   P1+00(6),SRTBMOS    MOS                                          
         MVC   P1+07(1),SRTBMED    MEDIA                                        
         MVC   P1+09(3),SRTBCLT    CLIENT                                       
         MVC   P1+13(3),SRTBPRD    PRODUCT                                      
         EDIT  SRTBEST,(3,P1+17),FILL=0                                         
         MVC   P1+21(10),SRTBIINV  INVOICE                                      
         MVC   P1+32(17),SRTBPUB   PUB NUMBER                                   
         OC    P1+32(17),SPACES    SPACE PAD                                    
         DROP  R4                  DROP SORT REC USINGS                         
*                                                                               
         GOTO1 REPORT              PRINT THE WARNING MESSAGE                    
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         XIT1                      RETURN TO CALLER                             
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'BLOUTPR - OUTPUT ROUTINE BELL'                                  
BLOUTPR  CSECT                                                                  
         NMOD1 0,BLOUTPR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
         L     R6,AWIDEC           A(WIDE)                                      
         USING WIDED,R6            WIDE DSECT                                   
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
         GOTO1 VTAPEWRT,DMCB,(RA)  WRITE RECORD TO TAPE                         
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
         MVC   BBLDIVC,=C'DIV'                                                  
         MVC   BBLDIVN,=C'   DIVISION NAME    '                                 
         MVC   BBLIVMOS,=C' MOS  '                                              
         MVC   BBLMED,=C'   MEDIA   '                                           
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
         MVC   BBLDIVC,=C'---'                                                  
         MVC   BBLDIVN,=C'--------------------'                                 
         MVC   BBLIVMOS,=C'------'                                              
         MVC   BBLMED,=C'-----------'                                           
         MVC   BBLUCOMM,=C'------------'                                        
         MVC   BBLUCOM3,=C'--------------------'                                
         MVC   BBLUCOM4,=C'--------------------------------'                    
         MVC   BBLGROSS,=C'--------------'                                      
         MVI   FORCEHED,C'Y'       FORCE HEADLINES TO PRINT                     
*                                                                               
         DROP  R7                  DROP PRINT LINE USING                        
*                                                                               
BLOUT10  GOTO1 VSORTER,DMCB,=C'GET' GET SORT RECORD                             
*                                                                               
         ICM   R5,15,4(R1)         ANY MORE RECORDS FROM SORTER?                
         BNZ   BLOUT11             YES                                          
         CP    SORTCNT,=P'1'       DID WE GET ANY SORT RECORDS?                 
         BL    BLOUT50             NO                                           
         BAS   RE,LSTVLINE         PAD LAST VENDOR LINE & TAPE WRITE            
         B     BLOUT50             DONE                                         
*                                                                               
BLOUT11  AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         USING SRTBRECD,R5         SRTBRECD DSECT                               
         CLC   SRTKSVBL,SRTBKEY    SAME AS PREVIOUS KEY?                        
         BE    BLOUT20             YES - VENDOR LEVEL LINE                      
*                                                                               
         CP    SORTCNT,=P'1'       FIRST TIME IN?                               
         BE    BLOUT15             YES                                          
         BAS   RE,LSTVLINE         PAD LAST VENDOR LINE & TAPE WRITE            
*                                                                               
BLOUT15  ZAP   VENDTOT,=P'0'       INIT VENDTOT                                 
         ZAP   INVTOT,SRTBINET     INVOICE TOTAL                                
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
         MVC   BBLDIVC,SRTBDIVC    DIVISION CODE                                
         MVC   BBLDIVN,SRTBDIVN    DIVISION CODE NAME                           
         MVC   BBLIVMOS,SRTBMOS    MOS                                          
         MVC   BBLMED,SRTBMEDN     MEDIA NAME                                   
         MVC   BBLUCOMM(4),SRTBUCOM  EST UCOMM 1 (PROJECT CODE)                 
         MVC   BBLUCOM3,SRTBUCM3   EST UCOMM 3 (CONTACT PIN)                    
         MVC   BBLUCOM4,SRTBUCM4   EST UCOMM 4 (CONTACT NAME)                   
*                                                                               
         ZAP   PAK6,SRTBINET       NET                                          
         AP    PAK6,SRTBIGST       +GST                                         
         AP    PAK6,SRTBIPST       +QST                                         
*                                                                               
         EDIT  PAK6,(13,BBLGROSS),2,FLOAT=-                                     
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
         MVC   BEUDEF2,SRTBUCM3    ESTIMATE UCOMM 3 (EMPLOYEE NUMBER)           
*                                                                               
         LA    R4,BEUDEF1          ESTIMATE UDEF1 (EMPLOYEE NAME + NUM)         
         MVC   0(32,R4),SRTBUCM4   ESTIMATE UCOMM 4 (EMPLOYEE NAME)             
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
         MVC   BQSTPCNT,=C'000.0000' GST PERCENT                                
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
         MVC   BDIVC,SRTBDIVC      DIVISION CODE                                
         MVC   BDIVN,SRTBDIVN      DIVISION NAME                                
*                                                                               
         GOTO1 VTAPEWRT,DMCB,(RA)  WRITE RECORD TO TAPE                         
         B     BLOUT21                                                          
*                                                                               
BLOUT20  GOTO1 VTAPEWRT,DMCB,(RA)  WRITE RECORD TO TAPE                         
*                                                                               
BLOUT21  LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
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
         MVC   BVREGN1,SRTBREGN    REGION NAME                                  
         MVC   BVREGN2,SRTBREGN    REGION NAME                                  
         MVC   BVPUBNAM,SRTBPUBN   PUB/VENDOR NAME                              
*                                                                               
         EDIT  SRTBBNET,(12,BVAMT),2,FILL=0                                     
*                                                                               
         ICM   R0,15,SRTBBNET      R0 = VALUE OF SRTBBNET                       
         C     R0,=F'0'            NEGATIVE VALUE?                              
         BNL   *+8                 NO                                           
         MVI   BVAMT,C'-'          YES - FLOAT NEGATIVE BEFORE VALUE            
*                                                                               
         XR    R4,R4               CLEAR R4                                     
         EDIT  (R4),(12,BVPAMT),2,FILL=0  PREV AMT (ALWAY 0)                    
*                                                                               
***      GOTO1 VTAPEWRT,DMCB,(RA)  WRITE RECORD TO TAPE                         
*                                                                               
         ICM   R0,15,SRTBBNET      VENDOR NET AMOUNT FOR REGION                 
         CVD   R0,DUB              CONVERT TO PACKED                            
         ZAP   LASTVEND,DUB        LAST VENDOR AMOUNT                           
         AP    VENDTOT,DUB         SUM OF DETAILS                               
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
***********************************************************************         
*   CHECK RUNNING TOTALS HERE AND ADD/SUBTRACT DIFFERENCE TO LAST     *         
*   VENDOR LINE TO MATCH INVOICE TOTAL                                *         
***********************************************************************         
LSTVLINE NTR1                                                                   
*                                                                               
         CP    VENDTOT,INVTOT      VENDOR TOTAL MATCHES INV TOTAL?              
         BE    LSTV10              YES - WRITE REC TO TAPE AS IS                
         SP    INVTOT,VENDTOT      INVOICE TOTAL - VENDOR TOTAL                 
         AP    INVTOT,LASTVEND     ADD DIFFERENCE TO LAST VENDOR AMT            
*                                                                               
         EDIT  INVTOT,(12,BVAMT),2,FILL=0                                       
*                                                                               
         CP    INVTOT,=P'0'        NEGATIVE VALUE?                              
         BNL   *+8                 NO                                           
         MVI   BVAMT,C'-'          YES - FLOAT NEGATIVE BEFORE VALUE            
*                                                                               
LSTV10   GOTO1 VTAPEWRT,DMCB,(RA)  WRITE RECORD TO TAPE                         
*                                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'UBBILLR - PROCESS BILL RECORDS FOR CARAT'                       
UBBILLR  CSECT                                                                  
         NMOD1 0,UBBILLR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R0,BINKEYUB         BINSRCH KEY                                  
         ST    R0,ABINKEY          A(BINSRCH KEY)                               
*                                                                               
         LA    R5,BINKEYUB         R5 = BINSRCH KEY                             
         USING UBBKEYD,R5          BILLING SORT KEY DSECT                       
         MVC   UBBMED,PBILKMED     MEDIA                                        
         MVC   UBBCLT,PBILKCLT     CLIENT CODE                                  
         MVC   UBBPRD,PBILKPRD     PRODUCT CODE                                 
         MVC   UBBEST,PBILKEST     ESTIMATE                                     
         MVC   UBBINVM,PBILKBMN+1  BILLING MONTH                                
         MVC   UBBINVN,PBILKBNO    INVOICE NUMBER                               
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         LA    R5,BINRECUB         R5 = BINSRCH RECORD                          
         USING UBBRECD,R5          BILL BINSRCH KEY DSECT                       
         MVC   UBBREC,SPACES       SPACE-FILL OUTPUT RECORD                     
*                                                                               
         GOTO1 VFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),(PBILKMED,B1PROF),   X        
               B1XPROF                                                          
*                                                                               
         L     RF,DMCB             RF = DMCB                                    
         MVC   UBBINV,0(RF)        FULL INVOICE NUMBER                          
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*                                                                               
***      MVC   UBBNET,PPBVEBN      NET AMOUNT                                   
         MVC   UBBNET,PPBVBACT     BILL ACTUAL AMOUNT                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILINVD),(20,WORK)                               
*                                                                               
         LA    R4,UBBINVD          INVOICE DATE                                 
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
***      ZAP   DUB,PPBVEBN         GET PPBVEBN INTO DUB                         
         ZAP   DUB,PPBVBACT        BILL ACTUAL AMOUNT                           
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) TOTAL               
*                                                                               
         MVI   ABINKEY,1           RESET TO INSERT RECORD IF NOT FOUND          
         GOTO1 VBINSRCH,ABINKEY    CALL BINSRCH                                 
*                                                                               
         CLI   ABINKEY,1           RECORD INSERTED?                             
         BE    UBBILL40            YES - DONE                                   
*                                                                               
         OC    1(3,R1),1(R1)       HAVE A(BINSRCH REC)?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - TABLE IS FULL - EXPAND TABLE!           
         DROP  R5                  DROP BINSRCH REC USING                       
*                                                                               
         L     RF,ABINKEY          ADDRESS OF FOUND RECORD                      
         LA    RF,L'UBBKEY(RF)     PAST KEY                                     
         USING UBBRECD,RF          BILL SORT RECORD DSECT                       
         AP    UBBNET,PPBVEBN      NET AMT (DUP KEY MUST ADD FIELD)             
         B     UBBILLX             DO NOT UPDATE INVOICE COUNT                  
         DROP  RF                  DROP BILL SORT USING                         
*                                                                               
UBBILL40 AP    INVCNT,=P'1'        UPDATE INVOICE COUNT                         
*                                                                               
UBBILLX  XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'UBBUYR - PROCESS BUY RECORDS FOR CARAT'                         
UBBUYR   CSECT                                                                  
         NMOD1 0,UBBUYR                                                         
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         NI    PLCMSTAT,X'FF'-PLCMERR TURN OFF PLACEMENT ID ERROR               
*                                                                               
         LA    R2,PBUYREC          R2 = BUY RECORD                              
         USING BYCCELD,R2          CUSTOM COLUMN DSECT                          
         MVI   ELCODE4,BYCCIDQ     X'CC' CUSTOM COLUMN ELEMENT ID               
         BAS   RE,GETEL4           HAVE A BILL ELEMENT?                         
         B     *+8                 GO TEST CONDITION CODE                       
*                                                                               
UBBUY10  BAS   RE,NEXTEL4          HAVE ANOTHER CUSTOM COLUMN ELEMENT?          
         BNE   UBBUY20             NO - IGNORE THIS BUY RECORD                  
         CLC   BYCCSQN,=AL2(8248)  !PLACEMENTID STDCOL IN P/SFM?                
         BNE   UBBUY10             NO                                           
         B     *+12                PROCESS THIS PLACEMENT ID                    
*                                                                               
UBBUY20  OI    PLCMSTAT,PLCMERR    FLAG MISSING PLACEMENT ID                    
         B     UBBUY30             NO PLACEMENT ID TO SAVE OFF                  
*                                                                               
         LLC   RF,BYCCLEN          ELEMENT LENGTH                               
         SHI   RF,BYCCHDRL         MINUS HEADER LENGTH                          
         BNP   UBBUY20             NO DATA - REPORT ERROR AND EXIT              
*                                                                               
         CHI   RF,L'PLCMNTID       ENOUGH ROOM FOR PLACEMENT ID?                
         BNH   *+6                 YES                                          
         DC    H'0'                NO - INCREASE PLCMNTID                       
*                                                                               
         XC    PLCMNTID,PLCMNTID   CLEAR PLACEMENT ID                           
         LA    RE,L'PLCMNTID       RE = L'PLCMNTID                              
         SR    RE,RF               INDEX INTO PLCMNTID                          
         LA    R5,PLCMNTID         R5 = PLCMNTID                                
         AR    R5,RE               ADD INDEX                                    
*                                                                               
         BCTR  RF,0                DECREMENT DATA LENGTH FOR EXECUTE            
         EX    RF,*+8              SAVE THE PLACEMENT ID                        
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   0(0,R5),BYCCDATA    SAVE PLACEMENT ID (RIGHT ALIGHNED)           
*                                                                               
UBBUY30  LA    R5,BINKEYUB         R5 = BINSRCH KEY                             
         USING UBBKEYD,R5          BILL BINSRCH KEY DSECT                       
         L     R2,SVBILEL          ADDRESS OF BILLING ELEMENT                   
         USING PBILELEM,R2         BILL ELEMENT DSECT                           
         XC    BINKEYBL,BINKEYBL   CLEAR BINSRCH KEY                            
         MVC   UBBMED,PBUYKMED     MEDIA                                        
         MVC   UBBCLT,PBUYKCLT     CLIENT CODE                                  
         MVC   UBBPRD,PBPRD        PRODUCT CODE FROM BILLING ELEMENT            
         MVC   UBBEST,PBUYKEST     ESTIMATE                                     
         MVC   UBBINVM,PBLDATE+1   BILLING MONTH                                
         MVC   UBBINVN,PBINVNO     INVOICE NUMBER                               
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
         LA    R5,UBBKEYL(R5)      BUMP PAST KEY TO RECORD                      
         USING UBBRECD,R5          BINSRCH RECORD DSECT                         
*                                                                               
         LA    R4,SORTREC          R4 = SORTREC                                 
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         USING SRTURECD,R4         SORT RECORD DSECT                            
         MVC   SRTUAGY,PBUYKAGY    AGENCY                                       
         MVC   SRTUMED,PBUYKMED    MEDIA                                        
         MVC   SRTUCLT,PBUYKCLT    CLIENT                                       
         MVC   SRTUPRD,PBPRD       PRODUCT CODE FROM BILLING ELEMENT            
         MVC   SRTUEST,PBUYKEST    ESTIMATE                                     
         MVC   SRTUINV,PBINVNO     PACKED INVOICE NUMBER                        
         MVC   SRTUBDAT,PBLDATE    BILLED DATE COMPRESSED                       
         DROP  R2                  DROP BILL ELEM USING                         
*                                                                               
         IC    R0,PAGYPROF+12      FOR PUBEDIT                                  
         GOTO1 PUBEDIT,DMCB,((R0),PUBREC+1),SRTUPUB                             
*                                                                               
         MVC   SRTUINET,UBBNET     NET AMOUNT DUE                               
         MVC   SRTUIINV,UBBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SRTUIDAT,UBBINVD    INVOICE DATE AS ON BILL                      
*                                                                               
         MVC   WORK(3),PBDBDATE    BILLABLE DATE                                
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         MVC   SRTUMOSP,WORK       SAVE PACKED MOS                              
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTUMOS)  MMM/YY                         
*                                                                               
         MVC   SRTUPID,PLCMNTID    PLACEMENT ID                                 
*                                                                               
         L     R2,SVBILEL          ADDRESS OF BILLING ELEMENT                   
         USING PBILELEM,R2         BILL ELEMENT DSECT                           
         MVC   FULL,PBGROSS        GROSS                                        
         L     R0,FULL             R0 = GROSS                                   
         MVC   FULL,PBAGYCOM       AGENCY COMMISION                             
         S     R0,FULL             SUBTRACT AGENCY COMMISSION                   
         STCM  R0,15,SRTUBNET      VENDOR NET AMOUNT                            
         DROP  R2,R4               DROP BILLING ELEM & SORT REC USINGS          
*                                                                               
         TM    PLCMSTAT,PLCMERR    MISSING PLACEMENT ID?                        
         BZ    *+12                NO - ADD TO SORT                             
         BAS   RE,PLACEERR         REPORT PLACEMENT-ID ERROR                    
         B     UBBUY50             DO NOT ADD RECORD TO SORT!                   
*                                                                               
         CVD  R0,DUB               CONVERT TO PACKED                            
         AP   DTLTOT,DUB           SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
UBBUY50  MVC   KEY(25),PBUYKEY     RE-READ THE BUY RECORD                       
         GOTO1 HIGH                READ HIGH                                    
         CLC   PBUYKEY,KEYSAVE     DID WE FIND THE BUY RECORD?                  
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
*                                                                               
UBBUYX   XIT1                      EXIT                                         
*                                                                               
         GETELN R2,33,ELCODE4,4    GETEL4 MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
PLACEERR NTR1                      REPORT MISSING PLACEMENT ID                  
*                                                                               
         LA    R6,P1               POINT TO PRINT LINE                          
         USING UBLINED,R6          UBLINED DSECT TO COVER PRINT LINE            
         USING SRTURECD,R4         SORT RECORD DSECT                            
         TM    PLCMSTAT,PLCMERRH   1ST TIME REPORTING NO PLACEMENT-ID?          
         BNZ   PLCERR10            NO - ALREADY PRINTED HEADER                  
         MVC   UBMEDIA(3),=C'MED'  MEDIA                                        
         MVC   UBCLT,=C'CLT'       CLIENT                                       
         MVC   UBPRD,=C'PRD'       PRODUCT                                      
         MVC   UBEST,=C'EST'       ESTIMATE                                     
         MVC   UBINVNO,=C'INV NUMBER' FULL INVOICE NUMBER                       
         MVC   UBINVMOS(3),=C'MOS' MOS MMM/YY                                   
         MVC   UBINVD(8),=C'INV DATE'                                           
         MVC   UBNET(10),=C'NET BILLED'                                         
         MVC   UBPUB(10),=C'PUB NUMBER'                                         
         MVC   UBPLCMNT(12),=C'PLACEMENT ID'                                    
         MVC   UBVAMT,=C'VENDOR AMOUNT'                                         
*                                                                               
         GOTO1 REPORT              REPORT ON PRINT LINE                         
*                                                                               
         MVC   UBMEDIA(3),=C'---'  MEDIA                                        
         MVC   UBCLT,=C'---'       CLIENT                                       
         MVC   UBPRD,=C'---'       PRODUCT                                      
         MVC   UBEST,=C'---'       ESTIMATE                                     
         MVC   UBINVNO,=C'----------' FULL INVOICE NUMBER                       
         MVC   UBINVMOS,=C'------'                                              
         MVC   UBINVD,=C'----------'                                            
         MVC   UBNET,=C'-------------'                                          
         MVC   UBPUB,=C'-----------'                                            
         MVC   UBPLCMNT,=C'--------------------'                                
         MVC   UBVAMT,=C'-------------'                                         
*                                                                               
         GOTO1 REPORT              REPORT ON PRINT LINE                         
*                                                                               
         OI    PLCMSTAT,PLCMERRH   FLAG HEADER ALREADY REPORTED ONCE            
*                                                                               
PLCERR10 LA    R6,P1               POINT TO PRINT LINE                          
         USING UBLINED,R6          UBLINED DSECT TO COVER PRINT LINE            
         MVC   UBMEDIA,SRTUMED     MEDIA                                        
         MVC   UBCLT,SRTUCLT       CLIENT                                       
         MVC   UBPRD,SRTUPRD       PRODUCT                                      
         EDIT  SRTUEST,(3,UBEST),FILL=0                                         
         MVC   UBINVNO,SRTUIINV    FULL INVOICE NUMBER                          
         MVC   UBINVMOS,SRTUMOS    MOS MMM/YY                                   
         MVC   UBINVD,SRTUIDAT     INVOICE DATE                                 
         EDIT  SRTUINET,(13,UBNET),2,MINUS=YES                                  
         MVC   UBPUB,SRTUPUB       PUB NUMBER                                   
         MVC   UBPLCMNT,=CL20'MISSING PLACEMENT-ID'                             
         EDIT  SRTUBNET,(13,UBVAMT),2,MINUS=YES                                 
         OC    UBLINE,SPACES       SPACE PAD                                    
*                                                                               
         GOTO1 REPORT              REPORT ON PRINT LINE                         
*                                                                               
         DROP  R4,R6               DROP SORT REC & PRINT LINE USINGS            
*                                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'UBOUTPR - OUTPUT ROUTINE CARAT'                                 
UBOUTPR  CSECT                                                                  
         NMOD1 0,UBOUTPR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         XC    SRTKSVUB,SRTKSVUB   CLEAR SAVED SORT KEY                         
         XC    JSONBUFF,JSONBUFF   CLEAR JSONBUFF                               
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         TM    PLCMSTAT,PLCMERRH   DID WE REPORT ANY PLACEMENT ID ERRS?         
         BNZ   UBOUT10             YES - HEADER ALREADY PRINTED                 
*                                                                               
         LA    R6,P1               POINT TO PRINT LINE                          
         USING UBLINED,R6          UBLINED DSECT TO COVER PRINT LINE            
         MVC   UBMEDIA(3),=C'MED'  MEDIA                                        
         MVC   UBCLT,=C'CLT'       CLIENT                                       
         MVC   UBPRD,=C'PRD'       PRODUCT                                      
         MVC   UBEST,=C'EST'       ESTIMATE                                     
         MVC   UBINVNO,=C'INV NUMBER' FULL INVOICE NUMBER                       
         MVC   UBINVMOS(3),=C'MOS' MOS MMM/YY                                   
         MVC   UBINVD(8),=C'INV DATE'                                           
         MVC   UBNET(10),=C'NET BILLED'                                         
         MVC   UBPUB(10),=C'PUB NUMBER'                                         
         MVC   UBPLCMNT(12),=C'PLACEMENT ID'                                    
         MVC   UBVAMT,=C'VENDOR AMOUNT'                                         
*                                                                               
         GOTO1 REPORT              REPORT ON PRINT LINE                         
*                                                                               
         MVC   UBMEDIA(3),=C'---'  MEDIA                                        
         MVC   UBCLT,=C'---'       CLIENT                                       
         MVC   UBPRD,=C'---'       PRODUCT                                      
         MVC   UBEST,=C'---'       ESTIMATE                                     
         MVC   UBINVNO,=C'----------' FULL INVOICE NUMBER                       
         MVC   UBINVMOS,=C'------'                                              
         MVC   UBINVD,=C'----------'                                            
         MVC   UBNET,=C'-------------'                                          
         MVC   UBPUB,=C'-----------'                                            
         MVC   UBPLCMNT,=C'--------------------'                                
         MVC   UBVAMT,=C'-------------'                                         
*                                                                               
         GOTO1 REPORT              REPORT ON PRINT LINE                         
*                                                                               
         DROP  R6                  DROP PRINT LINE USING                        
*                                                                               
UBOUT10  GOTO1 VSORTER,DMCB,=C'GET' GET SORT RECORD                             
*                                                                               
         ICM   R5,15,4(R1)         ANY MORE RECORDS FROM SORTER?                
         BZ    UBOUT50             NO - DONE                                    
*                                                                               
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         USING SRTURECD,R5         SRTBRECD DSECT                               
         CLC   SRTKSVUB,SRTUKEY    SAME AS PREVIOUS KEY?                        
         BE    UBOUT20             YES - KEEP BULDING JSON DATA                 
*                                                                               
         MVC   SRTKSVUB,SRTUKEY    SAVE KEY                                     
*                                                                               
         CP    SORTCNT,=P'1'       FIRST TIME IN?                               
         BE    UBOUT15             YES                                          
*                                                                               
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),X'BB'         END "]" FOR "lineItems"                      
         MVI   INDENT,10           INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'}'          END "}" FOR "invoices"                       
         MVI   INDENT,8            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),X'BB'         END "]" FOR "invoices"                       
         MVI   INDENT,6            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'}'          END "}" FOR "publishers"                     
         MVI   INDENT,4            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),X'BB'         END "]" FOR "publishers"                     
         MVI   INDENT,2            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'}'          END JSON DATA                                
         MVI   INDENT,0            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         BAS   RE,SENDMQ           JSON PAYLOAD TO PRISMA VIA MQ MSG            
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
UBOUT15  AP    HDRCNT,=P'1'        HEADERS                                      
*                                                                               
         L     R4,AOUTREC          R4 = A(OUTREC)                               
         LA    R4,4(R4)            BUMP PAST JSON DATA LENGTH                   
         MVC   SAVEAGY,SRTUAGY     SAVE OFF THE AGENCY                          
*                                                                               
         MVI   0(R4),C'{'          START JSON DATA                              
         MVI   INDENT,0            INDENTATION                                  
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(12,R4),=C'"requestId":'                                        
         MVI   INDENT,2            INDENTATION                                  
         MVI   PRINTSW,C'N'        BUFFER THIS DATA                             
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'"'          START QUOTE FOR requestId                    
         EDIT  SORTCNT,(10,1(R4)),FILL=0                                        
         MVC   11(2,R4),=C'",'     END QUOTE & COMMA FOR requestId              
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(13,R4),=C'"publishers":'                                       
         MVI   13(R4),X'BA'        THIS IS A "["                                
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'{'          OPEN '{' FOR publishers                      
         MVI   INDENT,4            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(11,R4),=C'"invoices":'                                         
         MVI   11(R4),X'BA'        THIS IS A "["                                
         MVI   INDENT,6            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   INDENT,8            INDENTATION                                  
         MVI   0(R4),C'{'          OPEN '{' FOR invoices                        
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   INDENT,10           INDENTATION                                  
         MVC   0(15,R4),=C'"action":"add",'                                     
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(13,R4),=C'"invoiceId":"'                                       
         MVI   PRINTSW,C'N'        BUFFER THIS DATA                             
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(10,R4),SRTUIINV   INVOICE NUMBER                               
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(2,R4),=C'",'      END QUOTE & COMMA FOR invoiceId              
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(24,R4),=C'"invoiceType":"invoice",'                            
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(18,R4),=C'"monthOfService":"'                                  
         MVI   PRINTSW,C'N'        BUFFER THIS DATA                             
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   WORK(3),SRTUMOSP    MOS YMD (BINARY)                             
         GOTO1 DATCON,DMCB,(3,WORK),(20,WORK+3)                                 
*                                                                               
         MVC   0(4,R4),WORK+3      YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+7      MM                                           
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(2,R4),=C'",'      END QUOTE & COMMA FOR monthOfService         
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(15,R4),=C'"invoiceDate":"'                                     
         MVI   PRINTSW,C'N'        BUFFER THIS DATA                             
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(10,R4),SRTUIDAT   YYYY-MM-DD                                   
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(2,R4),=C'",'      END QUOTE & COMMA FOR invoiceDate            
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(16,R4),=C'"invoiceAmount":'                                    
         MVI   PRINTSW,C'N'        BUFFER THIS DATA                             
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         EDIT  SRTUINET,(13,ELEM),2,FLOAT=-,ALIGN=LEFT                          
*                                                                               
         MVC   0(13,R4),ELEM       MOVE IN VALUE                                
*                                                                               
         LA    RE,ELEM+1           RE = ELEM+1                                  
         LA    RF,1(R4)            RF = 1(R4)                                   
         CLI   ELEM,C'-'           IS THIS A NEGATIVE NUMBER?                   
         BE    *+8                 YES                                          
         BCTR  RE,0                NO - DECREMENT RE TO ELEM                    
         BCTR  RF,0                NO - DECREMENT RF TO 0(R4)                   
         CLI   0(RE),C'.'          LESS THAN A DOLLAR?                          
         BNE   *+14                NO                                           
         MVI   0(RF),C'0'          PRIMSA API NEEDS A LEADING 0                 
         MVC   1(3,RF),0(RE)       SHIFT THE DECIMAL PLACE & CENTS              
*                                                                               
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C','          COMMA FOR invoiceAmount                      
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(12,R4),=C'"lineItems":'                                        
         MVI   12(R4),X'BA'        THIS IS A "["                                
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
UBOUT16  MVI   0(R4),C'{'          OPEN '{' FOR lineItems                       
         MVI   INDENT,12           INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(14,R4),=C'"identifiers":'                                      
         MVI   14(R4),X'BA'        THIS IS A "["                                
         MVI   INDENT,14           INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'{'          OPEN '{' FOR identifiers                     
         MVI   INDENT,16           INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(6,R4),=C'"id":"'  id FIELD                                     
         MVI   INDENT,18           INDENTATION                                  
         MVI   PRINTSW,C'N'        BUFFER THIS DATA                             
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         LA    RE,L'SRTUPID        MAX LENGTH OF PLACEMENT ID                   
         LA    RF,SRTUPID          START OF PLACEMENT ID                        
*                                                                               
UBOUT17  CLI   0(RF),X'40'         START OF PLACEMENT ID?                       
         BH    UBOUT18             YES                                          
         LA    RF,1(RF)            NO - BUMP TO NEXT BYTE                       
         BCT   RE,UBOUT17          CHECK NEXT BYTE                              
         DC    H'0'                MISSING PLACEMENT ID!                        
*                                                                               
UBOUT18  BCTR  RE,0                DECREMENT FOR MVC                            
         EX    RE,*+8              EXECUTE MVC                                  
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   0(0,R4),0(RF)       PLACEMENT ID                                 
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(2,R4),=C'",'      END QUOTE & COMMA FOR id                     
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(19,R4),=C'"type":"Mediaocean"'                                 
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'}'          CLOSE '}' FOR identifiers                    
         MVI   INDENT,16           INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),X'BB'         THIS IS A "]"                                
         MVI   1(R4),C','          END SQUARE "]" & "," FOR identifiers         
         MVI   INDENT,14           INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVC   0(17,R4),=C'"lineItemAmount":'                                   
         MVI   PRINTSW,C'N'        BUFFER THIS DATA                             
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         EDIT  SRTUBNET,(13,ELEM),2,FLOAT=-,ALIGN=LEFT                          
*                                                                               
         MVC   0(13,R4),ELEM       MOVE IN VALUE                                
*                                                                               
         LA    RE,ELEM+1           RE = ELEM+1                                  
         LA    RF,1(R4)            RF = 1(R4)                                   
         CLI   ELEM,C'-'           IS THIS A NEGATIVE NUMBER?                   
         BE    *+8                 YES                                          
         BCTR  RE,0                NO - DECREMENT RE TO ELEM                    
         BCTR  RF,0                NO - DECREMENT RF TO 0(R4)                   
         CLI   0(RE),C'.'          LESS THAN A DOLLAR?                          
         BNE   *+14                NO                                           
         MVI   0(RF),C'0'          PRIMSA API NEEDS A LEADING 0                 
         MVC   1(3,RF),0(RE)       SHIFT THE DECIMAL PLACE & CENTS              
*                                                                               
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'}'          CLOSE '}' FOR lineItems                      
         MVI   INDENT,12           INDENTATION                                  
         MVI   PRINTSW,C'N'        BUFFER THIS DATA                             
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         B     UBOUT30             PRINT VENDOR LINE                            
*                                                                               
UBOUT20  MVI   0(R4),C','          COMMA FOR CONTINUED VENDOR LEVEL             
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
         B     UBOUT16             NEXT "identifiers"                           
*                                                                               
UBOUT30  LA    R6,P1               POINT TO PRINT LINE                          
         USING UBLINED,R6          UBLINED DSECT TO COVER PRINT LINE            
         MVC   UBMEDIA,SRTUMED     MEDIA                                        
         MVC   UBCLT,SRTUCLT       CLIENT                                       
         MVC   UBPRD,SRTUPRD       PRODUCT                                      
         EDIT  SRTUEST,(3,UBEST),FILL=0                                         
         MVC   UBINVNO,SRTUIINV    FULL INVOICE NUMBER                          
         MVC   UBINVMOS,SRTUMOS    MOS MMM/YY                                   
         MVC   UBINVD,SRTUIDAT     INVOICE DATE                                 
         EDIT  SRTUINET,(13,UBNET),2,MINUS=YES                                  
         MVC   UBPUB,SRTUPUB       PUB NUMBER                                   
         MVC   UBPLCMNT,SRTUPID    PLACEMENT ID                                 
         EDIT  SRTUBNET,(13,UBVAMT),2,MINUS=YES                                 
         OC    UBLINE,SPACES       SPACE PAD                                    
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R6                  DROP PRINT LINE USING                        
*                                                                               
         B     UBOUT10             GET NEXT SORT RECORD                         
*                                                                               
UBOUT50  CP    SORTCNT,=P'0'       DID I PROCESS ANY SORT RECORDS?              
         BE    UBOUTXIT            NO - DONE                                    
*                                                                               
         MVI   PRINTSW,C'Y'        PRINT THIS LINE                              
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),X'BB'         END "]" FOR "lineItems"                      
         MVI   INDENT,10           INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'}'          END "}" FOR "invoices"                       
         MVI   INDENT,8            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),X'BB'         END "]" FOR "invoices"                       
         MVI   INDENT,6            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'}'          END "}" FOR "publishers"                     
         MVI   INDENT,4            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),X'BB'         END "]" FOR "publishers"                     
         MVI   INDENT,2            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         MVI   0(R4),C'}'          END JSON DATA                                
         MVI   INDENT,0            INDENTATION                                  
         BAS   RE,FNDUBEND         LAST CHAR OF AOUTREC RETURNED IN R4          
*                                                                               
         BAS   RE,SENDMQ           JSON PAYLOAD TO PRISMA VIA MQ MSG            
*                                                                               
UBOUTXIT XIT1                      DONE                                         
***                                                                             
* THIS CODE WILL SAVE US FROM HAVING TO ADJUST DISPLACEMENTS TO R4              
* EVERYWHERE IF/WHEN THE SPEC CHANGES                                           
***                                                                             
FNDUBEND NTR1                                                                   
*                                                                               
         CLI   PRINTSW,C'Y'        PRINT THIS LINE NOW?                         
         BE    FUBEND20            YES                                          
*                                                                               
         OC    JSONBUFF,JSONBUFF   HAVE ANYTHING IN JSONBUFF?                   
         BNZ   FUBEND10            YES - CONTINUE ADDING DATA HERE              
         LLC   RE,INDENT           INDEX INTO JSONBUFF                          
         LA    RF,JSONBUFF         RF = JSONBUFF                                
         AR    RF,RE               START PUTTING DATA HERE                      
         MVC   0(30,RF),0(R4)      SAVE DATA                                    
         B     FUBEND40            DONE                                         
*                                                                               
FUBEND10 LA    RF,JSONBUFF         RF = JSONBUFF                                
         AHI   RF,L'JSONBUFF-1     LAST BYTE OF JSONBUFF                        
         CLI   0(RF),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   RF,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVC   1(30,RF),0(R4)      SAVE DATA                                    
         B     FUBEND40            DONE                                         
*                                                                               
FUBEND20 LA    R5,P1               PRINT LINE                                   
         LLC   RE,INDENT           INDEX                                        
         AR    R5,RE               INDEX INTO PRINT LINE                        
         LR    RF,R4               BUFFER                                       
         OC    JSONBUFF,JSONBUFF   HAVE ANYTHING IN JSONBUFF?                   
         BZ    FUBEND30            NO                                           
*                                                                               
         LA    RF,JSONBUFF         RF = JSONBUFF                                
         AHI   RF,L'JSONBUFF-1     LAST BYTE OF JSONBUFF                        
         CLI   0(RF),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   RF,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         MVC   1(30,RF),0(R4)      SAVE DATA                                    
         LA    R5,P1               PRINT LINE                                   
         LA    RF,JSONBUFF         PRINT DATA FROM HERE                         
*                                                                               
FUBEND30 MVC   0(60,R5),0(RF)      MOVE DATA TO PRINT LINE                      
         MVI   RCWHATPR,2          SET TO SECOND SYSPRINT                       
         MVC   SVLINE,LINE         SAVE PRINT LINE FROM REPORT 1                
         MVC   SVFORCEH,FORCEHED   SAVE FORCEHED                                
         MVI   LINE,0              RESET LINE FOR SECOND SYSPRINT               
         MVI   FORCEHED,C'N'       NO HEADLINES FOR SECOND SYSPRINT             
         GOTO1 REPORT              PRINT JSON DATA TO SECOND SYSPRINT           
*                                                                               
         MVI   RCWHATPR,1          RESET TO FIRST SYSPRINT                      
         MVC   LINE,SVLINE         RESTORE LINE                                 
         MVC   FORCEHED,SVFORCEH   RESTORE FORCEHED                             
         XC    JSONBUFF,JSONBUFF   CLEAR JSONBUFF                               
*                                                                               
FUBEND40 LA    R4,30(R4)           BUMP WELL PAST DATA WE JUST ADDED            
         CLI   0(R4),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R4,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         AHI   R4,1                NEXT AVAILABLE SLOT                          
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         AR    RE,RF               END OF OUTREC                                
         CR    R4,RE               PAST OUTREC BUFFER                           
         BL    *+6                 NO                                           
         DC    H'0'                EXPAND OUTREC                                
*                                                                               
         XIT1  REGS=(R4)           RETURN TO CALLER                             
*                                                                               
SENDMQ   NTR1                                                                   
*                                                                               
         CLI   MQMSG,C'N'          SUPPRESS MQ NOTIFICATION?                    
         BE    SENDMQX             YES                                          
*                                                                               
         MVI   DMCB+8,X'E0'        SUPPRESS LEN, ADD CRLF & MSG HDR             
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'EDIINVOICE******'),,0             
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),(0,=C' '),1,0                            
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),(0,SAVEAGY),2,0                          
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
         L     R6,AOUTREC          R4 = MQ MESSAGE BUILD AREA                   
         LA    R6,4(R6)            BUMP PAST JSON DATA LENGTH                   
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
         TITLE 'OUBILLR - PROCESS BILL RECORD FOR PHDTO AGENCY OU'              
OUBILLR  CSECT                                                                  
         NMOD1 0,OUBILLR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R0,BINKEYOU         BINSRCH KEY                                  
         ST    R0,ABINKEY          A(BINSRCH KEY)                               
*                                                                               
         LA    R5,BINKEYOU         R5 = BINSRCH KEY                             
         USING OUBKEYD,R5          BILLING SORT KEY DSECT                       
         MVC   OUBMED,PBILKMED     MEDIA                                        
         MVC   OUBCLT,PBILKCLT     CLIENT CODE                                  
         MVC   OUBPRD,PBILKPRD     PRODUCT CODE                                 
         MVC   OUBEST,PBILKEST     ESTIMATE                                     
         MVC   OUBINVM,PBILKBMN+1  BILLING MONTH                                
         MVC   OUBINVN,PBILKBNO    INVOICE NUMBER                               
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         LA    R5,BINRECOU         R5 = BINSRCH RECORD                          
         USING OUBRECD,R5          BILL BINSRCH KEY DSECT                       
         MVC   OUBREC,SPACES       SPACE-FILL OUTPUT RECORD                     
*                                                                               
         GOTO1 VFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),(PBILKMED,B1PROF),   X        
               B1XPROF                                                          
*                                                                               
         L     RF,DMCB             RF = DMCB                                    
         MVC   OUBINV,0(RF)        FULL INVOICE NUMBER                          
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*                                                                               
         MVC   OUBNET,PPBVEBN      NET AMOUNT                                   
*                                                                               
         L     R0,PPBVGST          GST FROM PPBVAL                              
         CVD   R0,DUB              CVD                                          
         ZAP   OUBGST,DUB          GST - PACKED                                 
*                                                                               
         L     R0,PPBVPST          PST FROM PPBVAL                              
         CVD   R0,DUB              CVD                                          
         ZAP   OUBPST,DUB          PST - PACKED                                 
***                                                                             
* HERE IS WHAT PRWRITER DOES TO REPORT THE AGY COMMISSION                       
* AC = RCV - CD - (NET-CD) IN MOST CASES                                        
* PPBVBACT = 525.29                                                             
* PPBVEBN  = 475                                                                
*            ------                                                             
* OUBCOM   = 50.29                                                              
***                                                                             
         ZAP   OUBCOM,=P'0'        INIT AGENCY COMMISSION                       
         CLI   SVQOPT4,C'C'        TAPE OPTION C?                               
         BNE   OUBIL05             NO                                           
         ZAP   OUBCOM,PPBVBACT     SET AC TO RCVBL                              
*                                                                               
         TM    PBILCMSW,X'02'      COMMISSION ONLY BILL?                        
         BO    *+8                 YES                                          
         TM    PBILBASA,X'04'      CD ALREADY TAKEN OUT?                        
         BO    *+10                YES                                          
         SP    OUBCOM,PPBVEBC      NO - REMOVE CD                               
*                                                                               
         SP    OUBCOM,PPBVEBN      -NET = TRUE COMMISSION ADUSTMENT AMT         
*                                                                               
OUBIL05  MVI   OUBPROV,0           INIT PROVINCE CODE                           
*                                                                               
         LA    R2,PBILLREC         A(ESTIMATE RECORD)                           
         MVI   ELCODE5,X'84'       PST ELEMENT                                  
         BAS   RE,GETEL5           HAVE A PST ELEMENT?                          
         B     *+8                 GO TEST CC                                   
OUBIL10  BAS   RE,NEXTEL5          HAVE ANOTHER PST ELEMENT?                    
         BNE   OUBIL15             NO - DONE WE DON'T HAVE A PROVINCE           
*                                                                               
         USING PBLPSTEL,R2         PST ELEMENT DSECT                            
         OC    PBLPVAMT,PBLPVAMT   HAVE A VAT AMOUNT?                           
         BNZ   *+14                YES                                          
         OC    PBLPVBAS,PBLPVBAS   HAVE A VAT BASIS AMOUNT?                     
         BZ    OUBIL10             NO, CHECK NEXT ELEMENT                       
         MVC   OUBPROV,PBLPVPRV    SET BILL PROVINCE                            
         DROP  R2                  DROP PST ELEMENT USING                       
*                                                                               
OUBIL15  GOTO1 DATCON,DMCB,(3,PBILINVD),(20,WORK)                               
*                                                                               
         LA    R4,OUBINVD          INVOICE DATE                                 
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         MVC   OUEUDEF1,SPACES     INIT TO SPACES                               
         MVC   OUEUDEF2,SPACES     INIT TO SPACES                               
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
E        USING PESTREC,R6          ESTIMATE RECORD DSECT                        
         MVC   E.PESTKAGY,QAGENCY  AGENCY                                       
         MVC   E.PESTKMED,QMEDIA   MEDIA                                        
         MVI   E.PESTKRCD,X'07'    ESTIMATE RECORD CODE                         
         MVC   E.PESTKCLT,QCLIENT  CLIENT CODE                                  
         MVC   E.PESTKPRD,PBILKPRD PRODUCT CODE                                 
         MVC   E.PESTKEST,PBILKEST ESTIMATE CODE                                
         CLC   E.PESTKEY,PESTKEY   ALREADY HAVE THE EST RECORD IN CORE?         
         BE    OUBILL20            YES                                          
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   E.PESTKEY,WORK      ESTIMATE KEY                                 
         GOTO1 HIGH                READ HIGH                                    
         CLC   E.PESTKEY,KEYSAVE   DID WE FIND THE ESTIMATE KEY?                
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  E                   DROP LABELED ESTIMATE REC USING              
*                                                                               
         LA    R0,PESTREC          R0 = A(PESTREC)                              
         ST    R0,AREC             AREC = PESTREC                               
         GOTO1 GETPRT              GET THE ESTIMATE RECORD                      
*                                                                               
OUBILL20 LA    R2,PESTREC          A(ESTIMATE RECORD)                           
         MVI   ELCODE5,X'08'       UDEF ELEMENT                                 
         BAS   RE,GETEL5           HAVE A UDEF ELEMENT?                         
         BNE   OUBILL30            NO                                           
         USING PESTUDEF,R2         ESTIMATE UDEF DSECT                          
         MVC   OUEUDEF1,PEUSER1    ESTIMATE UDEF 1                              
         MVC   OUEUDEF2,PEUSER2    ESTIMATE UDEF 2                              
         OC    OUEUDEF1,SPACES     SPACE PAD                                    
         OC    OUEUDEF2,SPACES     SPACE PAD                                    
         DROP   R2                 DROP ESTIMATE UDEF USING                     
*                                                                               
OUBILL30 MVC   KEY(25),PBILLKEY    RE-READ THE BILL RECORD                      
         GOTO1 HIGH                READ HIGH                                    
         CLC   PBILLKEY,KEYSAVE    DID WE FIND THE BILL RECORD?                 
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
*                                                                               
         ZAP   DUB,PPBVEBN         GET PPBVEBN INTO DUB                         
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) TOTAL               
*                                                                               
         MVI   ABINKEY,1           RESET TO INSERT RECORD IF NOT FOUND          
         GOTO1 VBINSRCH,ABINKEY    CALL BINSRCH                                 
*                                                                               
         CLI   ABINKEY,1           RECORD INSERTED?                             
         BE    OUBILL40            YES - DONE                                   
*                                                                               
         OC    1(3,R1),1(R1)       HAVE A(BINSRCH REC)?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - TABLE IS FULL - EXPAND TABLE!           
         DROP  R5                  DROP BINSRCH REC USING                       
*                                                                               
         L     RF,ABINKEY          ADDRESS OF FOUND RECORD                      
         LA    RF,L'OUBKEY(RF)     PAST KEY                                     
         USING OUBRECD,RF          BILL SORT RECORD DSECT                       
         AP    OUBNET,PPBVEBN      NET AMT (DUP KEY MUST ADD FIELD)             
*                                                                               
         L     R0,PPBVGST          GST FROM PPBVAL                              
         CVD   R0,DUB              CVD                                          
         AP    OUBGST,DUB          GST - PACKED                                 
*                                                                               
         L     R0,PPBVPST          PST FROM PPBVAL                              
         CVD   R0,DUB              CVD                                          
         AP    OUBPST,DUB          PST - PACKED                                 
*                                                                               
         B     OUBILLX             DO NOT UPDATE INVOICE COUNT                  
         DROP  RF                  DROP BILL SORT USING                         
*                                                                               
OUBILL40 AP    INVCNT,=P'1'        UPDATE INVOICE COUNT                         
*                                                                               
OUBILLX  XIT1                      EXIT                                         
*                                                                               
         GETELN R2,33,ELCODE5,5    GETEL5 MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'OUBUYR - PROCESS BUY RECORDS FOR PHD'                           
OUBUYR   CSECT                                                                  
         NMOD1 0,OUBUYR                                                         
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R5,BINKEYOU         R5 = BINSRCH KEY                             
         USING OUBKEYD,R5          BILL BINSRCH KEY DSECT                       
         L     R2,SVBILEL          ADDRESS OF BILLING ELEMENT                   
         USING PBILELEM,R2         BILL ELEMENT DSECT                           
         XC    BINKEYOU,BINKEYOU   CLEAR BINSRCH KEY                            
         MVC   OUBMED,PBUYKMED     MEDIA                                        
         MVC   OUBCLT,PBUYKCLT     CLIENT CODE                                  
         MVC   OUBPRD,PBPRD        PRODUCT CODE FROM BILLING ELEMENT            
         MVC   OUBEST,PBUYKEST     ESTIMATE                                     
         MVC   OUBINVM,PBLDATE+1   BILLING MONTH                                
         MVC   OUBINVN,PBINVNO     INVOICE NUMBER                               
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
         LA    R5,OUBKEYL(R5)      BUMP PAST KEY TO RECORD                      
         USING OUBRECD,R5          BINSRCH RECORD DSECT                         
*                                                                               
         LA    R4,SORTREC          R4 = SORTREC                                 
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         USING SRTORECD,R4         SORT RECORD DSECT                            
         MVC   SRTOMED,PBUYKMED    MEDIA                                        
         MVC   SRTOCLT,PBUYKCLT    CLIENT                                       
         MVC   SRTOPRD,PBPRD       PRODUCT CODE FROM BILLING ELEMENT            
         MVC   SRTOEST,PBUYKEST    ESTIMATE                                     
         MVC   SRTOINV,PBINVNO     PACKED INVOICE NUMBER                        
         MVC   SRTOBDAT,PBLDATE    BILLED DATE COMPRESSED                       
         DROP  R2                  DROP BILL ELEM USING                         
*                                                                               
         IC    R0,PAGYPROF+12      FOR PUBEDIT                                  
         GOTO1 PUBEDIT,DMCB,((R0),PUBREC+1),SRTOPUB                             
*                                                                               
         MVC   SRTOINET,OUBNET     NET AMOUNT DUE                               
         MVC   SRTOIGST,OUBGST     GST AMOUNT DUE                               
         MVC   SRTOIPST,OUBPST     PST AMOUNT DUE                               
         MVC   SRTOACOM,OUBCOM     AGENCY COMMISSION AMOUNT                     
         MVC   SRTOIINV,OUBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SRTOIDAT,OUBINVD    INVOICE DATE AS ON BILL                      
         MVC   SRTOPROV,OUBPROV    PROVINCE                                     
         MVC   SRTOEUD1,OUEUDEF1   ESTIMATE UDEF 1                              
         MVC   SRTOEUD2,OUEUDEF2   ESTIMATE UDEF 2                              
*                                                                               
         MVC   SRTOCNAM,PCLTNAME   CLIENT NAME                                  
         OC    SRTOCNAM,SPACES     SPACE PAD                                    
*                                                                               
         MVC   SRTOMED,PBUYKMED    MEDIA CODE                                   
*                                                                               
         MVC   WORK(3),PBDBDATE    BILLABLE DATE                                
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         MVC   SRTOMOSP,WORK       SAVE PACKED MOS                              
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTOMOS)  MMM/YY                         
*                                                                               
         LA    R3,PUBREC           R3 = PUBREC                                  
         USING PUBNAMEL,R3         PUB NAME ELEMENT                             
         MVI   ELCODE6,X'10'       PUB NAME ELEMENT CODE                        
         BAS   RE,GETEL6           DO WE HAVE THE ELEMENT?                      
         BNE   *+10                NO                                           
         MVC   SRTOPUBN,PUBNAME    YES - GET THE VENDOR NAME                    
         DROP  R3                  DROP PUB NAME USING                          
*                                                                               
         OC    SRTOPUBN,SPACES     SPACE PAD VENDOR NAME                        
*                                                                               
         L     R2,SVBILEL          ADDRESS OF BILLING ELEMENT                   
         USING PBILELEM,R2         BILL ELEMENT DSECT                           
         MVC   FULL,PBGROSS        GROSS                                        
         L     R0,FULL             R0 = GROSS                                   
         MVC   FULL,PBAGYCOM       AGENCY COMMISION                             
         S     R0,FULL             R0 = GROSS-AGY COMMSSION                     
         MVC   FULL,PBCSHDSC       CASH DISCOUNT BILLED                         
         S     R0,FULL             VENDOR NET AMOUNT                            
         STCM  R0,15,FULL          SAVE VENDOR NET AMOUNT IN FULL               
         STCM  R0,15,SRTOBNET      VENDOR NET AMOUNT                            
         DROP  R2,R4               DROP BILLING ELEM & SORT REC USINGS          
*                                                                               
         CVD  R0,DUB               CONVERT TO PACKED                            
         AP   DTLTOT,DUB           SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
OUBUYX   XIT1                      EXIT                                         
*                                                                               
         GETELN R3,33,ELCODE6,6    GETEL6 MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'OUOUTPR - OUTPUT ROUTINE PHD'                                   
OUOUTPR  CSECT                                                                  
         NMOD1 0,OUOUTPR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         USING PPFILED,RC          PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
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
         CLI   SVQOPT4,C'C'        TAPE OPTION C?                               
         BE    OUOUT55             YES - COUPA FOR CANADIAN TIRE                
*                                                                               
         LA    R6,P1               POINT TO PRINT LINE                          
         USING BLINED,R6           BLINED DSECT TO COVER PRINT LINE             
         MVC   BLMEDIA,SRTOMED     MEDIA                                        
         MVC   BLCLT,SRTOCLT       CLIENT                                       
         MVC   BLCLNAME,SRTOCNAM   CLIENT NAME                                  
         MVC   BLPRD,SRTOPRD       PRODUCT                                      
         EDIT  SRTOEST,(3,BLEST),FILL=0                                         
         MVC   BLINVNO,SRTOIINV    FULL INVOICE NUMBER                          
         MVC   BLINVMOS,SRTOMOS    MOS MMM/YY                                   
         MVC   BLINVD,SRTOIDAT     INVOICE DATE                                 
         EDIT  SRTOINET,(13,BLNET),2,MINUS=YES                                  
         MVC   BLPUB,SRTOPUB       PUB NUMBER                                   
         MVC   BLPUBNAM,SRTOPUBN   PUB NAME                                     
         OC    BLINE,SPACES        SPACE PAD                                    
         EDIT  SRTOBNET,(13,BLVAMT),2,MINUS=YES                                 
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R6                  DROP PRINT LINE USING                        
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
         MVC   PHDVNAME,SRTOPUBN   VENDOR NAME                                  
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
OUOUT50  CLI   SVQOPT4,C'C'        TAPE OPTION C?                               
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
         LA    R6,P1               POINT TO PRINT LINE                          
         USING BLINED,R6           BLINED DSECT TO COVER PRINT LINE             
         OC    BLINE,SPACES        SPACE PAD                                    
         MVC   BLMEDIA,SRTOMED     MEDIA                                        
         MVC   BLCLT,SRTOCLT       CLIENT                                       
         MVC   BLCLNAME,SRTOCNAM   CLIENT NAME                                  
         MVC   BLPRD,SRTOPRD       PRODUCT                                      
         EDIT  SRTOEST,(3,BLEST),FILL=0                                         
         MVC   BLINVNO,SRTOIINV    FULL INVOICE NUMBER                          
         MVC   BLINVMOS,SRTOMOS    MOS MMM/YY                                   
         MVC   BLINVD,SRTOIDAT     INVOICE DATE                                 
         EDIT  SRTOINET,(13,BLNET),2,MINUS=YES                                  
         ZAP   PAK6,SRTOIGST       GST                                          
         AP    PAK6,SRTOIPST       ADD PST TO GET TOTAL TAX                     
         EDIT  PAK6,(8,BLTAX),2,FLOAT=-                                         
         EDIT  SRTOACOM,(11,BLACOM),2,MINUS=YES                                 
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R6                  DROP PRINT LINE USING                        
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
         GOTO1 =A(RDPRD),DMCB,(RA),SRTOMED,SRTOCLT,SRTOPRD                      
*                                                                               
         LA    R3,PPRDREC          PRODUCT RECORD                               
         MVI   ELCODEB,X'06'       PRODUCT X'06' ELEMENT                        
         BAS   RE,GETELB           HAVE THE ELEMENT?                            
         BNE   OUOUT56             NO                                           
*                                                                               
         USING PPRDELEM,R3         PRODUCT X'06' ELEMENT DSECT                  
         MVC   PHDPADD1,PPRDBILL   PRODUCT BILL RECEIPT NAME 1                  
         MVC   PHDPADD2,PPRDBIL2   PRODUCT BILL RECEIPT NAME 2                  
         MVC   PHDPADD3,PPRDLIN1   PRODUCT ADDRESS 1                            
         MVC   PHDPADD4,PPRDLIN2   PRODUCT ADDRESS 2                            
         DROP  R3                  PRODUCT X'06' ELEMENT DSECT                  
*                                                                               
OUOUT56  LA    RE,PHDINVNO         INVOICE NUMBER WITH NO DASHES                
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
         MVC   PHDEUDEF,SRTOEUD1   ESTIMATE UDEF 1                              
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
         AP    PAK6,SRTOACOM       INVOICE PLUS AGENCY COMMISSION               
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
         MVC   2(16,R7),SRTOEUD1   PO NUMBER (EST UDEF 2)                       
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
         GETELN R3,33,ELCODEB,B    GETELA MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
OUTABLE  NTR1  BASE=*,LABEL=*                                                   
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
SPFLDXIT XIT1  REGS=(R4,R6)        EXIT BUT KEEP R2 & R6 INTACT                 
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
         CLI   SVQOPT4,C'C'        TAPE OPTION C?                               
         BNE   ENDINV07            NO                                           
         MVI   PASS,1              PASS 1                                       
         LAY   R6,OUINTB2A         INVOICE LINE NUMBER                          
         MVI   0(R6),C'1'          LINE NUMBER 2                                
         LAY   R6,OUINTB2B         INOICE LINE NUMBER                           
         MVI   0(R6),C'1'          LINE NUMBER 2                                
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
         CLI   SVQOPT4,C'C'        TAPE OPTION C?                               
         BNE   ENDINV08            NO                                           
         LAY   R6,OUFTABGC         PHD CXML FOOTER TABLE GST TAX COUPA          
         LAY   R7,OUFSFTG          PHD FOOTER SPECIAL FIELDS COUPA              
         CLI   PASS,1              PASS 1?                                      
         BE    ENDINV08            YES                                          
         CP    SRTOACOM,=P'0'      HAVE AGENCY COMMISSION?                      
         BNE   ENDINV08            YES                                          
         MVC   PHDIGST,SPACES      NO - INIT TO SPACES                          
         MVC   PHDIGST(3),=C'.00'  NO - SET TO .00                              
ENDINV08 BRAS  RE,OUTABLE          MOVE FOOTER TABLE TO OUTREC                  
*                                                                               
ENDINV10 CLC   =C'.00 ',PHDIPST    HAVE PST?                                    
         BE    ENDINV20            NO                                           
         LAY   R6,OUFTABP          PHD CXML FOOTER TABLE PST TAX                
         LAY   R7,OUFSOFTP         PHD FOOTER SPECIAL FIELDS TABLE              
         CLI   SVQOPT4,C'C'        TAPE OPTION C?                               
         BNE   ENDINV11            NO                                           
         LAY   R6,OUFTABPC         PHD CXML FOOTER TABLE PST TAX COUPA          
         LAY   R7,OUFSFTP          PHD FOOTER SPECIAL FIELDS TABLE              
         CLI   PASS,1              PASS 1?                                      
         BE    ENDINV11            YES                                          
         CP    SRTOACOM,=P'0'      HAVE AGENCY COMMISSION?                      
         BNE   ENDINV11            YES                                          
         MVC   PHDIPST,SPACES      NO - INIT TO SPACES                          
         MVC   PHDIPST(3),=C'.00'  NO - SET TO .00                              
ENDINV11 BRAS  RE,OUTABLE          MOVE FOOTER TABLE TO OUTREC                  
*                                                                               
ENDINV20 CLI   SVQOPT4,C'C'        TAPE OPTION C?                               
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
         CP    SRTOACOM,=P'0'      HAVE AGENCY COMMISSION?                      
         BE    ENDINV22            NO - LEAVE GST & PST AS-IS                   
*                                                                               
         CLC   =C'.00 ',PHDIGST    HAVE GST?                                    
         BE    ENDINV21            NO                                           
         LAY   RF,50000            GST TAX RATE OF 5%                           
         ZAP   DUB,SRTOACOM        AGENCY COMMISSION                            
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
ENDINV21 CLC   =C'.00 ',PHDIPST    HAVE PST?                                    
         BE    ENDINV22            NO                                           
         ICM   RF,15,PHDTAXRB      PST TAX RATE                                 
         ZAP   DUB,SRTOACOM        AGENCY COMMISSION                            
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
ENDINV22 LAY   R6,OUINVTBL         invoiceLineNumber="2"                        
         B     ENDINV06                                                         
*                                                                               
ENDINV25 LAY   R6,OUFTAB2          PHD CXML FOOTER TABLE 2                      
         LAY   R7,OUFSOFT2         PHD FOOTER SPECIAL FIELDS TABLE              
*                                                                               
         CLI   SVQOPT4,C'C'        TAPE OPTION C?                               
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
         GOTO1 =A(UNIXFCRT),DMCB,(RA)   CREATE UNIX FILE                        
         GOTO1 =A(UNIXFPUT),DMCB,(RA)   ADD DATA TO UNIX FILE                   
         GOTO1 =A(UNIXFCLO),DMCB,(RA)   CLOSE UNIX FILE                         
*                                                                               
         MVC   QUALIFY,BILLING     QUALIFIER IS BILLING                         
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
         CLI   SVQOPT4,C'C'        TAPE OPTION C?                               
         BNE   ENDINV30            NO                                           
         MVC   AGENCYID,=C'OUU2'   YES - OUTO ALREADY SET UP ON HUB             
*                                                                               
ENDINV30 GOTO1 AMQMSG,DMCB,(RA)    SEND MQ MESSAGE WITH FILENAME                
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
         MVC   P1(L'FILENAME),FILENAME                                          
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
         LA    RE,P1               PRINT LINE                                   
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
OUISOFTC DC    X'04',AL2(PHDEUDEF-PHDSFLDD),AL1(L'PHDEUDEF)                     
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
         TITLE 'GNBILLR - PROCESS BILL RECORDS FOR GENERIC EDI'                 
GNBILLR  CSECT                                                                  
         NMOD1 0,BLBILLR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R0,BINKEYGN         BINSRCH KEY                                  
         ST    R0,ABINKEY          A(BINSRCH KEY)                               
*                                                                               
         LA    R5,BINKEYGN         R5 = BINSRCH KEY                             
         USING GEBKEYD,R5          BILLING SORT KEY DSECT                       
         MVC   GEBMED,PBILKMED     MEDIA                                        
         MVC   GEBCLT,PBILKCLT     CLIENT CODE                                  
         MVC   GEBPRD,PBILKPRD     PRODUCT CODE                                 
         MVC   GEBEST,PBILKEST     ESTIMATE                                     
         MVC   GEBIMOS,PBILKMOS    MOS                                          
         MVC   GEBINVM,PBILKBMN+1  BILLING MONTH                                
         MVC   GEBINVN,PBILKBNO    INVOICE NUMBER                               
         DROP  R5                  DROP BINSRCH KEY USING                       
*                                                                               
         LA    R5,BINRECGN         R5 = BINSRCH RECORD                          
         USING GEBRECD,R5          BILL BINSRCH KEY DSECT                       
         XC    GEBREC,GEBREC       CLEAR OUTPUT RECORD                          
*                                                                               
         GOTO1 VFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),(PBILKMED,B1PROF),   X        
               B1XPROF                                                          
*                                                                               
         L     RF,DMCB             RF = DMCB                                    
         MVC   GEBINV,0(RF)        FULL INVOICE NUMBER                          
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*                                                                               
         MVC   GEBGROSS,PPBVEBG    GROSS                                        
         MVC   GEBNET,PPBVEBN      NET                                          
         MVC   GEBACT,PPBVBACT     ACTUAL                                       
         MVC   GEBCSHD,PPBVEBC     CASH DISCOUNT                                
         MVC   GEBTAX,PPBVTAX      TAX (US)                                     
         MVC   GEBGST,PPBVGST      GST                                          
         MVC   GEBPST,PPBVPST      PST                                          
         MVC   GEBHST,PPBVHST      HST                                          
*                                                                               
         LA    RF,TYPETAB          BILL TYPE TRANSLATION TABLE                  
         LA    RE,1                ASSUME NEW BILL TYPE                         
         CLI   PBILLTYP,C'0'       OLD BILL TYPE?                               
         BNH   *+6                 NO                                           
         BCTR  RE,0                YES - ONLY MATCH ON 1 CHAR                   
*                                                                               
GEB00    CLI   0(RF),0             UNKNOWN TYPE?                                
         BE    GEB10               YES - DONE                                   
         EX    RE,*+8              EXECUTE                                      
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         CLC   0(0,RF),PBILLTYP    MATCH ON BILL TYPE?                          
         BE    GEB10               YES                                          
         LA    RF,5(RF)            BUMP TO NEXT TABLE ENTRY                     
         B     GEB00               PROCESS NEXT TABLE ENTRY                     
*                                                                               
GEB10    MVC   GEBTYPE,2(RF)       TRANSLATED BILL TYPE                         
*                                                                               
         MVC   GEBSTAT,PBILCMSW    STATUS BYTE 1                                
         MVC   GEBSTAT2,PBILSTAT   STATUS BYTE 2                                
         MVC   GEBSEP,PBILSEP      SEPARATE BILL                                
         MVC   GEBRETFL,PBRETAIL   RETAIL FLAGS                                 
         TM    PBRETAIL,X'03'      RETAIL BILL?                                 
         BZ    *+10                NO                                           
         MVC   GEBRETAC,PBRACCT    RETAIL ACCOUNT CODE                          
         MVC   GEBWBFLT,PBILWBF    WB FLIGHT CODE                               
*                                                                               
         LA    R3,PBILLREC         A(BILL RECORD)                               
         MVI   ELCODE7,X'09'       OTHERS ELEMENT - ADDITIONAL DATA             
         BAS   RE,GETEL7           HAVE A PST ELEMENT?                          
         BNE   *+10                                                             
         MVC   GEBSTAT3,PBILLIND   STATUS BYTE 3                                
*                                                                               
         MVI   GEBPROV,0           INIT PROVINCE CODE                           
*                                                                               
         LA    R3,PBILLREC         A(BILL RECORD)                               
         MVI   ELCODE7,X'84'       PST ELEMENT                                  
         BAS   RE,GETEL7           HAVE A PST ELEMENT?                          
         B     *+8                 GO TEST CC                                   
GEB15    BAS   RE,NEXTEL7          HAVE ANOTHER PST ELEMENT?                    
         BNE   GEB20               NO - DONE WE DON'T HAVE A PROVINCE           
*                                                                               
         USING PBLPSTEL,R3         PST ELEMENT DSECT                            
         OC    PBLPVAMT,PBLPVAMT   HAVE A VAT AMOUNT?                           
         BNZ   *+14                YES                                          
         OC    PBLPVBAS,PBLPVBAS   HAVE A VAT BASIS AMOUNT?                     
         BZ    GEB15               NO, CHECK NEXT ELEMENT                       
         MVC   GEBPROV,PBLPVPRV    SET BILL PROVINCE                            
         DROP  R3                  DROP PST ELEMENT USING                       
*                                                                               
GEB20    GOTO1 DATCON,DMCB,(0,PBILLDAT),(20,WORK)                               
*                                                                               
         LA    R4,GEBINVD          INVOICE RUN DATE                             
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILDUED),(20,WORK)                               
*                                                                               
         LA    R4,GEBINVDD         INVOICE DUE DATE                             
         MVC   0(4,R4),WORK        YYYY                                         
         MVI   4(R4),C'-'          -                                            
         MVC   5(2,R4),WORK+4      MM                                           
         MVI   7(R4),C'-'          -                                            
         MVC   8(2,R4),WORK+6      DD                                           
*                                                                               
         GOTO1 =A(RDPRD),DMCB,(RA),PBILKMED,PBILKCLT,PBILKPRD                   
*                                                                               
         TM    PBRETAIL,X'03'      RETAIL BILL?                                 
         BNZ   GEB25               YES                                          
*                                                                               
         MVC   GEBDIV,PNBPGR1      DIVISION                                     
***      OC    PNBPGR1,PNBPGR1     HAVE DIVISION CODE ON BILL RECORD?           
***      BNZ   *+10                YES                                          
***      MVC   GEBDIV,PPRDDIV      NO - USE DIVISION CODE FROM PRD REC          
         CLC   GEBDIV,=C'000'      EBCIDIC ZEROES?                              
         BNE   *+10                NO                                           
         XC    GEBDIV,GEBDIV       YES - CLEAR TO BINARY ZEROES                 
*                                                                               
         MVC   GEBREG,PNBMGR1      REGION                                       
         CLC   GEBREG,=C'000'      EBCIDIC ZEROES?                              
         BNE   *+10                NO                                           
         XC    GEBREG,GEBREG       YES - CLEAR TO BINARY ZEROES                 
*                                                                               
         MVC   GEBDIS,PNBMGR2      DISTRICT                                     
         CLC   GEBDIS,=C'000'      EBCIDIC ZEROES?                              
         BNE   *+10                NO                                           
         XC    GEBDIS,GEBDIS       YES - CLEAR TO BINARY ZEROES                 
*                                                                               
GEB25    MVC   GEBMEDN,PAGYMED     MEDIA NAME                                   
         OC    GEBMEDN,SPACES      SPACE PAD                                    
*                                                                               
         MVC   GEBCNAME,PCLTNAME   CLIENT NAME                                  
         OC    GEBCNAME,SPACES     SPACE PAD                                    
*                                                                               
         MVC   KEY(25),PBILLKEY    RE-READ THE BILL RECORD                      
         GOTO1 HIGH                READ HIGH                                    
         CLC   PBILLKEY,KEYSAVE    DID WE FIND THE BILL RECORD?                 
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
*                                                                               
         ZAP   DUB,PBILLRCV        GET PBILLRCV INTO DUB                        
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) TOTAL               
*                                                                               
         MVI   ABINKEY,1           RESET TO INSERT RECORD IF NOT FOUND          
         GOTO1 VBINSRCH,ABINKEY    CALL BINSRCH                                 
*                                                                               
         CLI   ABINKEY,1           RECORD INSERTED?                             
         BE    GEB30               YES - DONE                                   
*                                                                               
         OC    1(3,R1),1(R1)       HAVE A(BINSRCH REC)?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO - TABLE IS FULL - EXPAND TABLE!           
         DROP  R5                  DROP BINSRCH REC USING                       
*                                                                               
         L     RF,ABINKEY          ADDRESS OF FOUND RECORD                      
         LA    RF,L'GEBKEY(RF)     PAST KEY                                     
         USING GEBRECD,RF          BILL SORT RECORD DSECT                       
         OI    GEBIFLAG,GEBIFDUP   MARK THIS AS A DUPLICATE!                    
         ZAP   DUB,PBILLRCV        GET PBILLRCV INTO DUB                        
         SP    BILTOT,DUB          DON'T ADD TWICE                              
*&&DO                                                                           
         AP    GEBGROSS,PPBVEBG    GROSS                                        
         AP    GEBNET,PPBVEBN      NET                                          
         AP    GEBACT,PPBVBACT     ACTUAL                                       
         ZAP   DUB,PPBVEBB         GROSS-CD                                     
         SP    DUB,PPBVEBN         MINUS GROSS-AC-CD                            
         AP    GEBCOM,DUB          AGENCY COMMISSION                            
         AP    GEBCSHD,PPBVEBC     CASH DISCOUNT                                
*                                                                               
         ICM   RE,15,GEBTAX        TAX FROM PREVIOUS INVOICE                    
         ICM   RF,15,PPBVTAX       TAX FROM THIS INVOICE                        
         AR    RE,RF               ADD THEM UP                                  
         STCM  RE,15,GEBTAX        UPDATE TAX FIELD                             
*                                                                               
         ICM   RE,15,GEBGST        GST FROM PREVIOUS INVOICE                    
         ICM   RF,15,PPBVGST       GST FROM THIS INVOICE                        
         AR    RE,RF               ADD THEM UP                                  
         STCM  RE,15,GEBGST        UPDATE GST FIELD                             
*                                                                               
         ICM   RE,15,GEBPST        PST FROM PREVIOUS INVOICE                    
         ICM   RF,15,PPBVPST       PST FROM THIS INVOICE                        
         AR    RE,RF               ADD THEM UP                                  
         STCM  RE,15,GEBPST        UPDATE PST FIELD                             
*                                                                               
         ICM   RE,15,GEBHST        HST FROM PREVIOUS INVOICE                    
         ICM   RF,15,PPBVHST       HST FROM THIS INVOICE                        
         AR    RE,RF               ADD THEM UP                                  
         STCM  RE,15,GEBHST        UPDATE HST FIELD                             
*&&                                                                             
         B     GEBXIT              DO NOT UPDATE INVOICE COUNT                  
         DROP  RF                  DROP BILL SORT USING                         
*                                                                               
GEB30    AP    INVCNT,=P'1'        UPDATE INVOICE COUNT                         
*                                                                               
GEBXIT   XIT1                      EXIT                                         
*                                                                               
TYPETAB  DC    C'1 ',C'MAN'                                                     
         DC    C'3 ',C'ORG'                                                     
         DC    C'4 ',C'DET'                                                     
         DC    C'B4',C'B4 '                                                     
         DC    C'B5',C'B5 '                                                     
         DC    C'B6',C'B6 '                                                     
         DC    C'B7',C'B7 '                                                     
         DC    C'M4',C'M4 '                                                     
         DC    C'M5',C'M5 '                                                     
         DC    C'M6',C'M6 '                                                     
         DC    C'M7',C'M7 '                                                     
         DC    C'R4',C'R4 '        FINANCIAL REBATE BILLS                       
         DC    C'R5',C'R5 '                                                     
         DC    C'R6',C'R6 '                                                     
         DC    C'R7',C'R7 '                                                     
         DC    X'FB20',C'T  '      GRP M MIDAS                                  
         DC    X'FF41',C'S  '      RETAIL SUMMARY BILLS                         
         DC    X'FF81',C'C  '      RETAIL CONTROL BILLS                         
         DC    X'FE00',C'AOR'      AOR BILLS                                    
         DC    X'0000',C'UNK'                                                   
*                                                                               
         GETELN R3,33,ELCODE7,7    GETEL7 MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GNBUYR - PROCESS BUY RECORDS FOR GENERIC EDI'                   
GNBUYR   CSECT                                                                  
         NMOD1 0,GNBUYR                                                         
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R5,BINKEYGN         R5 = BINSRCH KEY                             
         USING GEBKEYD,R5          BILL BINSRCH KEY DSECT                       
         L     R2,SVBILEL          ADDRESS OF BILLING ELEMENT                   
         USING PBILELEM,R2         BILL ELEMENT DSECT                           
         XC    BINKEYGN,BINKEYGN   CLEAR BINSRCH KEY                            
         MVC   GEBMED,PBUYKMED     MEDIA                                        
         MVC   GEBCLT,PBUYKCLT     CLIENT CODE                                  
         MVC   GEBPRD,PBPRD        PRODUCT CODE FROM BILLING ELEMENT            
         MVC   GEBEST,PBUYKEST     ESTIMATE                                     
         MVC   GEBIMOS,PBDBDATE    MOS                                          
         MVC   GEBINVM,PBLDATE+1   BILLING MONTH                                
         MVC   GEBINVN,PBINVNO     INVOICE NUMBER                               
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
         TM    GEBIFLAG,GEBIFDUP   IS THIS A DUPLICATE BILL HEADER?             
         BNZ   GNBUYX              YES - DO NOT PROCESS!                        
         OI    GEBIFLAG,GEBIFVEN   FLAG THIS AS HAVING VENDOR INFO              
*                                                                               
         LA    R4,SORTREC          R4 = SORTREC                                 
         XC    SORTREC,SORTREC     CLEAR SORTREC                                
         USING SRTGRECD,R4         SORT RECORD DSECT                            
         MVC   SRTGMED,PBUYKMED    MEDIA                                        
         MVC   SRTGCLT,PBUYKCLT    CLIENT                                       
         MVC   SRTGPRD,PBPRD       PRODUCT CODE FROM BILLING ELEMENT            
         MVC   SRTGEST,PBUYKEST    ESTIMATE                                     
         MVC   SRTGINV,PBINVNO     PACKED INVOICE NUMBER                        
         MVC   SRTGBDAT,PBLDATE    BILLED DATE COMPRESSED                       
         MVC   SRTGMOS,PBDBDATE    MOS                                          
*                                                                               
         IC    R0,PAGYPROF+12      FOR PUBEDIT                                  
         GOTO1 PUBEDIT,DMCB,((R0),PUBREC+1),SRTGPUB                             
*                                                                               
         CLC   =X'FFFF',PUBKZON    ALL ZONES AND EDITIONS?                      
         BE    GNBUY10             YES                                          
         CLI   PUBKZON,0           ANY ZONE?                                    
         BE    GNBUY05             NO                                           
*                                                                               
         XR    RF,RF               CLEAR RF                                     
         MVC   BYTE,PUBKZON        ZONE                                         
         NI    BYTE,X'0F'          KEEP A-F                                     
         IC    RF,BYTE             RF = A-F                                     
         LA    RF,183(RF)          A-F                                          
         CLI   BYTE,10             >= 10?                                       
         BNL   *+8                 YES                                          
         LA    RF,57(RF)           0-9                                          
         STC   RF,SRTGZONE+1       ZONE+1                                       
         IC    RF,PUBKZON          ZONE                                         
         SRL   RF,4                DROP OFF RIGHT 4 BITS                        
         LA    RF,183(RF)          A-F                                          
         CLI   PUBKZON,160         >= 160?                                      
         BNL   *+8                 YES                                          
         LA    RF,57(RF)           0-9                                          
         STC   RF,SRTGZONE         ZONE                                         
*                                                                               
GNBUY05  CLI   PUBKED,X'FF'        ALL EDITIONS?                                
         BE    GNBUY10             YES                                          
*                                                                               
         LA    RE,EDTAB            EDITION TAB                                  
*                                                                               
GNBUY06  CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    GNBUY10             YES                                          
         CLC   PUBKED,0(RE)        MATCH ON EDITION?                            
         BE    GNBUY07             FOUND                                        
         LA    RE,15(RE)           BUMP TO NEXT ENTRY                           
         B     GNBUY06                                                          
*                                                                               
GNBUY07  MVC   SRTGEDIT(3),1(RE)   3 CHAR EDITION                               
         CLI   PAGYPROF+12,C'E'    EDITION EXPANSION?                           
         BNE   *+10                NO                                           
         MVC   SRTGEDIT,4(RE)      EDITION EXPANSION                            
*                                                                               
GNBUY10  MVC   WORK(3),PBDBDATE    BILLABLE DATE                                
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTGBMOS)  MMM/YY                        
*                                                                               
         MVC   SRTGBGRS,GEBGROSS   GROSS                                        
         MVC   SRTGBNET,GEBNET     NET                                          
         MVC   SRTGBACT,GEBACT     ACTUAL                                       
***      MVC   SRTGBCOM,GEBCOM     AGENCY COMMISSION                            
         MVC   SRTGBCDC,GEBCSHD    CASH DISCOUNT                                
         MVC   SRTGBTAX,GEBTAX     TAX AMOUNT DUE (USA)                         
         MVC   SRTGBGST,GEBGST     GST AMOUNT DUE (CAN)                         
         MVC   SRTGBPST,GEBPST     PST AMOUNT DUE (CAN)                         
         MVC   SRTGBHST,GEBHST     HST AMOUNT DUE (CAN)                         
         MVC   SRTGBPRV,GEBPROV    PROVINCE       (CAN)                         
         MVC   SRTGBTYP,GEBTYPE    BILL TYPE (B1,B2,B3,B4,B5,ETC)               
         MVC   SRTGBINV,GEBINV     FULL INVOICE NUMBER AS ON BILL               
         MVC   SRTGSTAT,GEBSTAT    STATUS BYTE 1 (PBILCMSW)                     
         MVC   SRTGSTA2,GEBSTAT2   STATUS BYTE 2 (PBILSTAT)                     
         MVC   SRTGSTA3,GEBSTAT3   STATUS BYTE 3 (PBILLIND)                     
         MVC   SRTGSEP,GEBSEP      SEPARATE BILL (PBILSEP)                      
         MVC   SRTGRTLF,GEBRETFL   RETAIL STATUS BITS                           
         MVC   SRTGRTLA,GEBRETAC   RETAIL ACCOUNT CODE                          
         MVC   SRTGWBFL,GEBWBFLT   WB FLIGHT CODE                               
         MVC   SRTGIDAT,GEBINVD    INVOICE RUN DATE                             
         MVC   SRTGIDDT,GEBINVDD   INVOICE DUE DATE                             
         MVC   SRTGBDIV,GEBDIV     DIVISION                                     
         MVC   SRTGBREG,GEBREG     REGION                                       
         MVC   SRTGBDIS,GEBDIS     DISTRICT                                     
         MVC   SRTGMEDN,PAGYMED    MEDIA NAME                                   
         OC    SRTGMEDN,SPACES     SPACE PAD                                    
         MVC   SRTGCNAM,PCLTNAME   CLIENT NAME                                  
         OC    SRTGCNAM,SPACES     SPACE PAD                                    
         MVC   SRTGPUBK,PBUYKPUB   PUB (KEY)                                    
         MVC   SRTGZONK,PBUYKZON   ZONE (KEY)                                   
         MVC   SRTGEDTK,PBUYKEDT   EDITION (KEY)                                
*                                                                               
         LA    R3,PUBREC           R3 = PUBREC                                  
         USING PUBNAMEL,R3         PUB NAME ELEMENT                             
         MVI   ELCODE8,X'10'       PUB NAME ELEMENT CODE                        
         BAS   RE,GETEL8           DO WE HAVE THE ELEMENT?                      
         BNE   *+16                NO                                           
         MVC   SRTGPUBN,PUBNAME    YES - GET THE VENDOR NAME                    
         OC    SRTGPUBN,SPACES     SPACE PAD VENDOR NAME                        
         DROP  R3                  DROP PUB NAME USING                          
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'E',0(R2)),PPBVALD                             
*                                                                               
         MVC   SRTGVGRS,PPBVEEG    GROSS                                        
         MVC   SRTGVAGC,PPBVEEA    AGENCY COMMISSION                            
         MVC   SRTGVCDC,PPBVEEC    CASH DISCOUNT                                
         MVC   SRTGVNET,PPBVEEN    NET                                          
         CLI   PBILELEM,X'28'      COST2 RECORD?                                
         BNE   *+8                 NO                                           
         MVI   SRTGCOS2,C'Y'       YES                                          
*                                                                               
         ICM   R0,15,SRTGVNET      VENDOR NET AMOUNT                            
*                                                                               
         CVD  R0,DUB               CONVERT TO PACKED                            
         AP   DTLTOT,DUB           SUM OF DETAILS                               
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
*                                                                               
         MVC   KEY(25),PBUYKEY     RE-READ THE BUY RECORD                       
         GOTO1 HIGH                READ HIGH                                    
         CLC   PBUYKEY,KEYSAVE     DID WE FIND THE BUY RECORD?                  
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
*                                                                               
GNBUYX   XIT1                      EXIT                                         
*                                                                               
         DROP   R2,R4,R5           DROP ALL USINGS                              
*                                                                               
         GETELN R3,33,ELCODE8,8    GETEL8 MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
EDTAB    DC    CL15'AM  MORNING'                                                
         DC    CL15'BE  EVENING'                                                
         DC    CL15'CD  DAILY'                                                  
         DC    CL15'DME MORN/EVE'                                               
         DC    CL15'ESAMSAT MORN'                                               
         DC    CL15'FSAESAT EVE'                                                
         DC    CL15'GSD SAT DAILY'                                              
         DC    CL15'HSMESAT M/E'                                                
         DC    CL15'ISU SUNDAY'                                                 
         DC    CL15'JOM MORN COMBO'                                             
         DC    CL15'KOE EVE COMBO'                                              
         DC    CL15'LOD DAILY COMBO'                                            
         DC    CL15'MOMEM/E COMBO'                                              
         DC    CL15'NPR PRINTED'                                                
         DC    CL15'PP  PAINTED'                                                
         DC    CL15'RR  ROTARY'                                                 
         DC    CL15'SS  SUNDAY'                                                 
         DC    CL15'TT  TRANSIT'                                                
         DC    CL15'UMONMONTHLY'                                                
         DC    CL15'WW  WEEKLY'                                                 
         DC    CL15'XPM PERMANENT'                                              
         DC    X'FF'                                                            
*                                                                               
         TITLE 'GNOUTPR - OUTPUT ROUTINE FOR GENERIC EDI'                       
GNOUTPR  CSECT                                                                  
         NMOD1 0,GNOUTPR                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         BRAS  RE,NOVENDOR         ADD SORT RECS FOR INV W/NO VENDORS           
*                                                                               
         XC    SRTKSVGE,SRTKSVGE   CLEAR SAVED SORT KEY                         
         ZAP   VENCOUNT,=P'0'      ZAP ACCUMULATOR                              
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         MVI   CANADIAN,C'N'       INIT TO USA AGENCY                           
         CLI   PAGYNAT,C'C'        IS THIS CANADIAN                             
         BNE   *+8                 NO                                           
         MVI   CANADIAN,C'Y'       CANADIAN AGENCY                              
*                                                                               
         L     R2,ALISREC          GENERIC EDI SOFT INV FIELDS BLOCK            
         USING GENSFLDH,R2         GENERIC EDI SOFT INV FIELDS DSECT            
*                                                                               
         L     RE,VMASTC           A(MASTC)                                     
         USING MASTD,RE            MASTD DSECT                                  
         MVC   GENUSRID,MCUSERID   USER-ID                                      
         DROP  RE                  DROP MASTD USING                             
*                                                                               
         MVC   GENAGNCY,SAVEAGY    AGENCY ALPHA                                 
         MVC   GENFRMAT,SVQOPT4    FORMAT/FLAVOR                                
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
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         CLC   SRTKSVGE,SRTGKEY    SAME BILL HDR INFO?                          
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
         GOTO1 =A(GNTABLE),DMCB,(RA)                                            
*                                                                               
         BRAS  RE,SENDMQG          SEND JSON PAYLOAD VIA MQ MSG                 
*                                                                               
         BRAS  RE,PRNTJSON         PRINT JSON DATA                              
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
GNOUT15  L     R0,ALISREC          INVOICE BLOCK                                
         AHI   R0,GENCLLEN         BUMP PAST DATA WE DON'T CLEAR                
         LHI   R1,GENSFDHQ         LENGTH OF INVOICE BLOCK                      
         SHI   R1,GENCLLEN         MINUS DATA WE AREN'T CLEARING                
         SR    RE,RE               CLEAR RE                                     
         SR    RF,RF               CLEAR RF                                     
         MVCL  R0,RE               CLEAR THE INVOICE BLOCK                      
*                                                                               
         AP    VENCOUNT,=P'1'      UPDATE VENDOR COUNT                          
*                                                                               
         MVC   GENMEDCD,SRTGMED    MEDIA CODE                                   
         MVC   GENMEDNM,SRTGMEDN   MEDIA NAME                                   
         MVC   GENCLTCD,SRTGCLT    CLIENT CODE                                  
         MVC   GENCLTNM,SRTGCNAM   CLIENT NAME                                  
*                                                                               
         GOTO1 =A(RDPRD),DMCB,(RA),SRTGMED,SRTGCLT,SRTGPRD                      
*                                                                               
         MVC   GENPRDCD,SRTGPRD    PRODUCT CODE                                 
*                                                                               
***      XC    GENPRDNM,GENPRDNM   CLEAR PRODUCT NAME                           
***      XC    GENPRDR1,GENPRDR1   CLEAR PRODUCT BILL RECEIPT NAME 1            
***      XC    GENPRDR2,GENPRDR2   CLEAR PRODUCT BILL RECEIPT NAME 2            
***      XC    GENPRDA1,GENPRDA1   CLEAR PRODUCT ADDRESS 1                      
***      XC    GENPRDA2,GENPRDA2   CLEAR PRODUCT ADDRESS 2                      
***      XC    GENPRDAT,GENPRDAT   CLEAR PRODUCT ATTENTION                      
***      XC    GENPRDAN,GENPRDAN   CLEAR PRODUCT ACCOUNT NUMBER                 
*                                                                               
         LA    R3,PPRDREC          PRODUCT RECORD                               
         MVI   ELCODE9,X'06'       PRODUCT X'06' ELEMENT                        
         BAS   RE,GETEL9           HAVE THE ELEMENT?                            
         BNE   GNOUT16             NO                                           
*                                                                               
         USING PPRDELEM,R3         PRODUCT X'06' ELEMENT DSECT                  
         MVC   GENPRDNM,PPRDNAME   PRODUCT NAME                                 
         MVC   GENPRDR1,PPRDBILL   PRODUCT BILL RECEIPT NAME 1                  
         MVC   GENPRDR2,PPRDBIL2   PRODUCT BILL RECEIPT NAME 2                  
         MVC   GENPRDA1,PPRDLIN1   PRODUCT ADDRESS 1                            
         MVC   GENPRDA2,PPRDLIN2   PRODUCT ADDRESS 2                            
         MVC   GENPRDAT,PPRDATTN   PRODUCT ATTENTION                            
         MVC   GENPRDAN,PPRDACCT   PRODUCT ACCOUNT NUMBER                       
         DROP  R3                  PRODUCT X'06' ELEMENT DSECT                  
*                                                                               
GNOUT16  DS    0H                                                               
***      XC    GENPRUS1,GENPRUS1   CLEAR PRODUCT UDEF 1                         
***      XC    GENPRUS2,GENPRUS2   CLEAR PRODUCT UDEF 2                         
*                                                                               
         LA    R3,PPRDREC          PRODUCT RECORD                               
         MVI   ELCODE9,X'08'       PRODUCT USER DESCRIPTION ELEMENT             
         BAS   RE,GETEL9           HAVE THE ELEMENT?                            
         BNE   GNOUT17             NO                                           
*                                                                               
         USING PPRDUDEF,R3         PRODUCT USER DESCRIPTION DSECT               
         MVC   GENPRUS1,PUSER1     PRODUCT UDEF 1                               
         MVC   GENPRUS2,PUSER2     PRODUCT UDEF 2                               
         DROP  R3                  DROP PRD USER DESCRIPTION USING              
*                                                                               
GNOUT17  DS    0H                                                               
***      XC    GENPRDIC,GENPRDIC   CLEAR PRODUCT INTERFACE CODE                 
*                                                                               
         LA    R3,PPRDREC          PRODUCT RECORD                               
         MVI   ELCODE9,X'30'       PRODUCT INTERFACE CODE ELEMENT               
         BAS   RE,GETEL9           HAVE THE ELEMENT?                            
         BNE   GNOUT17A            NO                                           
*                                                                               
         USING PPRDICEL,R3         PRODUCT INTERFACE CODE DSECT                 
         MVC   GENPRDIC,PPRDINFC   PRODUCT INTERFACE CODE                       
         DROP  R3                  DROP PRD INTERFACE CODE USING                
*                                                                               
GNOUT17A GOTO1 =A(PRDUCOMM),DMCB,(RA) READ PRODUCT UCOMM DATA                   
*                                                                               
         MVC   GENPRDU1,SVPUCOM1   PRODUCT UCOMM 1                              
         MVC   GENPRDU2,SVPUCOM2   PRODUCT UCOMM 2                              
         MVC   GENPRDU3,SVPUCOM3   PRODUCT UCOMM 3                              
         MVC   GENPRDU4,SVPUCOM4   PRODUCT UCOMM 4                              
*                                                                               
         GOTO1 =A(RDEST),DMCB,(RA),SRTGMED,SRTGCLT,SRTGPRD,SRTGEST              
*                                                                               
         EDIT  SRTGEST,(3,GENESTCD),FILL=0                                      
*                                                                               
***      XC    GENESTNM,GENESTNM   CLEAR ESTIMATE NAME                          
*                                                                               
         LA    R3,PESTREC          ESTIMATE RECORD                              
         MVI   ELCODE9,X'07'       ESTIMATE X'07' ELEMENT                       
         BAS   RE,GETEL9           HAVE THE ELEMENT?                            
         BNE   GNOUT18             NO                                           
*                                                                               
         USING PESTELEM,R3         ESTIMATE X'07' ELEMENT DSECT                 
         MVC   GENESTNM,PESTNAME   ESTIMATE NAME                                
         DROP  R3                  ESTIMATE X'07' ELEMENT DSECT                 
*                                                                               
GNOUT18  DS    0H                                                               
***      XC    GENESUS1,GENESUS1   CLEAR ESTIMATE UDEF 1                        
***      XC    GENESUS2,GENESUS2   CLEAR ESTIMATE UDEF 2                        
*                                                                               
         LA    R3,PESTREC          ESTIMATE RECORD                              
         MVI   ELCODE9,X'08'       ESTIMATE USER DESCRIPTION ELEMENT            
         BAS   RE,GETEL9           HAVE THE ELEMENT?                            
         BNE   GNOUT19             NO                                           
*                                                                               
         USING PESTUDEF,R3         ESTIMATE USER DESCRIPTION DSECT              
         MVC   GENESUS1,PEUSER1    ESTIMATE UDEF 1                              
         MVC   GENESUS2,PEUSER2    ESTIMATE UDEF 2                              
         DROP  R3                  DROP EST USER DESCRIPTION USING              
*                                                                               
GNOUT19  GOTO1 =A(ESTUCOMM),DMCB,(RA) READ UCOMM DATA                           
*                                                                               
         MVC   GENESUC1,SVEUCOM1   ESTIMATE UCOMM 1                             
         MVC   GENESUC2,SVEUCOM2   ESTIMATE UCOMM 2                             
         MVC   GENESUC3,SVEUCOM3   ESTIMATE UCOMM 3                             
         MVC   GENESUC4,SVEUCOM4   ESTIMATE UCOMM 4                             
         MVC   GENESUC5,SVEUCOM5   ESTIMATE UCOMM 5                             
         MVC   GENESUC6,SVEUCOM6   ESTIMATE UCOMM 6                             
         MVC   GENESUC7,SVEUCOM7   ESTIMATE UCOMM 7                             
         MVC   GENESUC8,SVEUCOM8   ESTIMATE UCOMM 8                             
*                                                                               
         OC    SRTGBDIV,SRTGBDIV   HAVE A DIVISION?                             
         BZ    GNOUT22             NO                                           
*                                                                               
         MVC   GENINDIV,SRTGBDIV   DIVISION ON BILL                             
***      XC    GENINDNM,GENINDNM   CLEAR DIVISION NAME                          
*                                                                               
         GOTO1 =A(RDDIV),DMCB,(RA),SRTGMED,SRTGCLT,SRTGBDIV                     
         BNE   GNOUT20             DIVISION NOT FOUND                           
*                                                                               
         LA    R3,PDIVREC          DIVISION RECORD                              
         MVI   ELCODE9,X'03'       DIVISION X'03' ELEMENT                       
         BAS   RE,GETEL9           HAVE THE ELEMENT?                            
         BNE   GNOUT20             NO                                           
*                                                                               
         USING PESTELEM,R3         DIVISION X'03' ELEMENT DSECT                 
         MVC   GENINDNM,PDIVNAME   DIVISION NAME                                
         DROP  R3                  DIVISION X'03' ELEMENT DSECT                 
*                                                                               
GNOUT20  OC    SRTGBREG,SRTGBREG   HAVE A REGION?                               
         BZ    GNOUT22             NO                                           
*                                                                               
         MVC   GENINREG,SRTGBREG   REGION CODE                                  
***      XC    GENINRNM,GENINRNM   CLEAR REGION NAME                            
*                                                                               
         GOTO1 =A(RDREG),DMCB,(RA),SRTGMED,SRTGCLT,SRTGBDIV,SRTGBREG            
         BNE   GNOUT21A            REGION NOT FOUND                             
*                                                                               
         LA    R3,PREGREC          REGION RECORD                                
         MVI   ELCODE9,X'04'       REGION X'04' ELEMENT                         
         BAS   RE,GETEL9           HAVE THE ELEMENT?                            
         BNE   GNOUT21             NO                                           
*                                                                               
         USING PREGELEM,R3         REGION X'04' ELEMENT DSECT                   
         MVC   GENINRNM,PREGNAME   REGION NAME                                  
         DROP  R3                  REGION X'04' ELEMENT DSECT                   
*                                                                               
GNOUT21  GOTO1 =A(REGUCOMM),DMCB,(RA) READ UCOMM DATA                           
*                                                                               
         MVC   GENRGUC1,SVRUCOM1   1ST REGION UCOMM                             
         MVC   GENRGUC2,SVRUCOM2   2ND REGION UCOMM                             
         MVC   GENRGUC3,SVRUCOM3   3RD REGION UCOMM                             
         MVC   GENRGUC4,SVRUCOM4   4TH REGION UCOMM                             
*                                                                               
GNOUT21A OC    SRTGBDIS,SRTGBDIS   HAVE A DISTRICT?                             
         BZ    GNOUT22             NO                                           
*                                                                               
         MVC   GENINDIS,SRTGBDIS   DISTRICT                                     
*                                                                               
         GOTO1 =A(DISUCOMM),DMCB,(RA),SRTGBDIS                                  
*                                                                               
         MVC   GENDGUC1,SVDUCOM1   1ST DISTRICT UCOMM                           
         MVC   GENDGUC2,SVDUCOM2   2ND DISTRICT UCOMM                           
         MVC   GENDGUC3,SVDUCOM3   3RD DISTRICT UCOMM                           
         MVC   GENDGUC4,SVDUCOM4   4TH DISTRICT UCOMM                           
*                                                                               
GNOUT22  MVC   GENINMOS,SRTGBMOS   INVOICE MOS (APR/20)                         
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
         ZAP   PAK6,SRTGBACT       ACTUAL                                       
         SP    PAK6,SRTGBNET       MINUS NET                                    
*                                                                               
         EDIT  PAK6,(13,GENINCOM),2,ALIGN=LEFT,FLOAT=-                          
*                                                                               
         CLC   =C'.00',GENINCOM    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENINCOM(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGBCDC,(13,GENINCDC),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENINCDC    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENINCDC(3),=C'0  ' YES - MAKE IT ZERO                           
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
         BZ    GNOUT23             NO                                           
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
         XC    GENINPST,GENINPST   YES - MAKE IT ZERO                           
*                                                                               
         OC    SRTGBGST,SRTGBGST   HAVE GST?                                    
         BZ    *+10                NO                                           
         MVC   GENINGSR,=C'5    '  GST RATE (5.000)                             
*                                                                               
GNOUT23  MVC   GENINTYP,SRTGBTYP   BILL TYPE (BTYPE FROM SPGENBILL)             
         MVC   GENRETAC,SRTGRTLA   RETAIL ACCOUNT CODE                          
         MVC   GENWBFLT,SRTGWBFL   WB FLIGHT                                    
*                                                                               
         MVI   GENSADJB,C'N'       SEP ADJUSTMENT BILL FLAG = N                 
         CLI   SRTGSEP,C'A'        SEP ADJUSTMENT BILL?                         
         BNE   *+8                 NO                                           
         MVI   GENSADJB,C'Y'       YES, SEP ADJUSTMENT BILL FLAG = Y            
*                                                                               
         MVI   GENGMTDM,C'N'       GROUPM TRADE MIDAS = N                       
         TM    SRTGSTA2,X'20'      GROUPM TRADE MIDAS?                          
         BZ    *+8                 NO                                           
         MVI   GENGMTDM,C'Y'       YES, GROUPM TRADE MIDAS = Y                  
*                                                                               
         MVI   GENCOST2,C'N'       COST2 = N                                    
         TM    SRTGSTA3,X'82'      COST2?                                       
         BZ    *+8                 NO                                           
         MVI   GENCOST2,C'Y'       YES, COST2 = Y                               
                                                                                
         LA    R3,GENCOMAJ         COMMISSION ADJUSTMENT BILL FLAG              
         LA    R4,SRTGSTAT         BILL STATUS (PBILCMSW)                       
         BRAS  RE,TRFLAG           TRANSLATE THE FLAG INTO FIELDS               
*                                                                               
         LA    R3,GENRETCB         BRETAIL FLAG FROM SORT                       
         LA    R4,SRTGRTLF         BRETAIL FLAG SOFT FIELDS                     
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
         L     RE,ALISREC          GENERIC EDI SOFT INV FIELDS BLOCK            
         ST    RE,DMCB+12          PARM 4                                       
         LAY   RE,GNHEXL           INVOICE HEADER EXCLUSION TABLE               
         ST    RE,DMCB+16          PARM 5                                       
*                                                                               
         GOTO1 =A(GNTABLE),DMCB,(RA)                                            
***                                                                             
* LOOP HERE FOR EACH VENDOR                                                     
***                                                                             
GNOUT46  LA    R6,P1               POINT TO PRINT LINE                          
         USING BLINED,R6           BLINED DSECT TO COVER PRINT LINE             
         MVC   BLMEDIA,SRTGMED     MEDIA                                        
         MVC   BLCLT,SRTGCLT       CLIENT                                       
         MVC   BLCLNAME,SRTGCNAM   CLIENT NAME                                  
         MVC   BLPRD,SRTGPRD       PRODUCT                                      
         EDIT  SRTGEST,(3,BLEST),FILL=0                                         
         MVC   BLINVNO,SRTGBINV    FULL INVOICE NUMBER                          
         MVC   BLINVMOS,SRTGBMOS   MOS MMM/YY                                   
         MVC   BLINVD,SRTGIDAT     INVOICE DATE                                 
         EDIT  SRTGBNET,(13,BLNET),2,MINUS=YES                                  
         MVC   BLPUB,SRTGPUB       PUB NUMBER                                   
         MVC   BLPUBNAM,SRTGPUBN   PUB NAME                                     
         OC    BLINE,SPACES        SPACE PAD                                    
         EDIT  SRTGVNET,(13,BLVAMT),2,MINUS=YES                                 
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R6                  DROP PRINT LINE USING                        
*                                                                               
         OC    SRTGPUBK,SRTGPUBK   DO WE HAVE A VENDOR?                         
         BNZ   GNOUT46A            YES                                          
         MVI   ONLYINVC,C'Y'       INDICATE INVOICE ONLY INFO                   
         BCTR  R4,0                EXTRA COMMA BEFORE VENDORS ARRAY             
         B     GNOUT49             SKIP VENDOR SECTION                          
*                                                                               
GNOUT46A L     R3,ALISREC          GENERIC EDI SOFT INV FIELDS BLOCK            
         AHI   R3,GENSFDHQ         BUMP TO EDI SOFT VENDOR FIELDS BLOCK         
         USING GENSFLDV,R3         GENERIC EDI SOFT INV FIELDS DSECT            
*                                                                               
         LR    R0,R3               VENDOR BLOCK                                 
         LHI   R1,GENSFDVQ         LENGTH OF VENDOR BLOCK                       
         SR    RE,RE               CLEAR RE                                     
         SR    RF,RF               CLEAR RF                                     
         MVCL  R0,RE               CLEAR THE VENDOR BLOCK                       
*                                                                               
         MVC   GENVPUBN,SRTGPUB    PUB NUMBER                                   
         MVC   GENVPBNM,SRTGPUBN   PUB NAME                                     
         MVC   GENVZONE,SRTGZONE   ZONE                                         
         MVC   GENVEDIT,SRTGEDIT   EDITION                                      
*                                                                               
         EDIT  SRTGVGRS,(13,GENVGRSD),2,ALIGN=LEFT,FLOAT=-                      
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
         EDIT  SRTGVAGC,(13,GENVAGCM),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENVAGCM    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENVAGCM(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         EDIT  SRTGVCDC,(13,GENVCDSC),2,ALIGN=LEFT,FLOAT=-                      
*                                                                               
         CLC   =C'.00',GENVCDSC    ZERO AMOUNT?                                 
         BNE   *+10                NO                                           
         MVC   GENVCDSC(3),=C'0  ' YES - MAKE IT ZERO                           
*                                                                               
         MVI   GENVCOS2,0          COST2 = N                                    
         CLI   SRTGCOS2,C'Y'       COST2?                                       
         BNE   *+8                 NO                                           
         MVI   GENVCOS2,C'Y'       YES, COST2 = Y                               
*                                                                               
         LAY   RE,GVENTAB          FIRST VENDOR JSON TABLE                      
         CLI   FIRSTINV,C'Y'       FIRST TIME FOR THIS INVOICE?                 
         BE    *+10                YES                                          
         LAY   RE,GVENTAB2         NO - ADDITIONAL VENDOR JSON TABLE            
         ST    RE,DMCB+4           PARM 2                                       
         LAY   RE,GNVSOFT          VENDOR SOFT FIELDS TABLE                     
         ST    RE,DMCB+8           PARM 3                                       
         L     RE,ALISREC          GENERIC EDI SOFT INV FIELDS BLOCK            
         AHI   RE,GENSFDHQ         BUMP TO EDI SOFT VENDOR FIELDS BLOCK         
         ST    RE,DMCB+12          PARM 4                                       
         LA    RE,=X'FF'           NO VENDOR EXCLUSION TABLE                    
         ST    RE,DMCB+16          PARM 5                                       
*                                                                               
         GOTO1 =A(GNTABLE),DMCB,(RA)                                            
*                                                                               
         MVI   ONLYINVC,C'N'       INDICATE HAVE VENDOR INFO                    
*                                                                               
         CLC   SRTGBREG,SPACES     DO WE HAVE A REGION ON THE INVOICE?          
         BH    GNOUT49             YES                                          
*&&DO                                                                           
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R6,KEY              R6 = KEY                                     
         USING LTLREC,R6           PUB SUPPLEMENTAL REC                         
         MVC   LTLKMED,SRTGMED     MEDIA                                        
         MVC   LTLKPUB,SRTGPUBK    PUBLICATION CODE                             
         MVC   LTLKZON,SRTGZONK    ZONE                                         
         MVC   LTLKED,SRTGEDTK     EDITION                                      
         MVC   LTLKAGY,SAVEAGY     AGENCY                                       
         MVI   LTLKCOD,X'85'       PUB SUPPLEMENTAL REC ID                      
*                                                                               
         GOTO1 HIGHPUB             READ HIGH ON PUBFILE                         
*                                                                               
         CLC   LTLKEY,KEYSAVE      DID WE FIND THE PUB SUPPLEMENT KEY?          
         BNE   GNOUT49             NO - DONE                                    
         DROP  R6                  DROP PUB SUPPLEMENT USING                    
*                                                                               
         L     R3,ALTLREC          READ PUB SUPPLEMENT RECORD HERE              
         USING PUBDSTEL,R3         PUB REG/DIST ELEM                            
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,(R3),(0,DMWORK)               
         CLI   8(R1),0             ANY ERROR ON RETURN?                         
         BE    *+6                 NO                                           
         DC    H'0'                YES - NO ERRORS TOLERATED                    
*                                                                               
         MVI   REGCODE,C'N'        INIT TO NO REGION                            
*                                                                               
         MVI   ELCODE9,X'71'       PUB NAME ELEMENT CODE                        
         BAS   RE,GETEL9           DO WE HAVE THE ELEMENT?                      
         B     *+8                 GO TEST CC                                   
*                                                                               
GNOUT47  BAS   RE,NEXTEL9          HAVE REG/DIST ELEM?                          
         BNE   GNOUT48             NO - DONE                                    
*                                                                               
         CLC   PUBDCLT,SRTGCLT     MATCH ON CLIENT CODE?                        
         BNE   GNOUT47             NO                                           
         CLC   PUBDDIV,SRTGBDIV    MATCH ON DIVISION CODE?                      
         BNE   GNOUT47             NO                                           
*                                                                               
         CLI   REGCODE,C'N'        FIRST VENDOR LEVEL REGION?                   
         BNE   *+6                 NO - ONLY BCTR THE 1ST TIME                  
         BCTR  R4,0                YES, WE NED TO REPLACE } WITH ,              
         MVI   0(R4),C','          ALWAYS START WITH A COMMA                    
         AHI   R4,1                BUMP R4 TO NEXT BYTE                         
*                                                                               
         GOTO1 =A(VDREG),DMCB,(RA)                                              
*                                                                               
         LAY   RE,GREGTAB          FIRST REGION JSON TABLE                      
         CLI   REGCODE,C'N'        FIRST TIME FOR THIS VENDOR?                  
         BE    *+10                YES                                          
         LAY   RE,GREGTAB2         NO - ADDITIONAL REGIONS START HERE           
         ST    RE,DMCB+4           PARM 2                                       
         LAY   RE,GNHSOFT          INVOICE SOFT FIELDS TABLE                    
         ST    RE,DMCB+8           PARM 3                                       
         L     RE,ALISREC          GENERIC EDI SOFT INV FIELDS BLOCK            
         ST    RE,DMCB+12          PARM 4                                       
         LA    RE,=X'FF'           NO EXCLUSION TABLE FOR THIS DATA             
         ST    RE,DMCB+16          PARM 5                                       
*                                                                               
         GOTO1 =A(GNTABLE),DMCB,(RA)                                            
*                                                                               
         MVI   REGCODE,C'Y'        WE PROCESSED A VENDOR REGION                 
*                                                                               
         B     GNOUT47             GET NEXT ELEMENT                             
         DROP  R3                  DROP PUB REG/DIST USING                      
*                                                                               
GNOUT48  CLI   REGCODE,C'N'        DID WE HAVE VENDOR LEVEL REGIONS?            
         BE    GNOUT49             NO                                           
         MVI   0(R4),X'BB'         CLOSE REGION ARRAY WITH A "]"                
         MVI   1(R4),C'}'          CLOSE OFF GVENTAB                            
         AHI   R4,2                BUMP R4                                      
*&&                                                                             
GNOUT49  MVI   FIRSTINV,C'N'       NO LONGER FIRST TIME FOR THIS INV            
         B     GNOUT10             GET NEXT SORT RECORD                         
*                                                                               
GNOUT50  CP    SORTCNT,=P'0'       DID WE PROCESS ANY RECORDS?                  
         BE    GNOUTXIT            NO - NOTHING IN THE PIPELINE TO SEND         
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
         GOTO1 =A(GNTABLE),DMCB,(RA)                                            
*                                                                               
         BRAS  RE,SENDMQG          SEND JSON PAYLOAD VIA MQ MSG                 
*                                                                               
         BRAS  RE,PRNTJSON         PRINT JSON DATA                              
*                                                                               
         LAY   RF,OUTRECLN-16      LENGTH OF OUTREC                             
         L     RE,AOUTREC          RE = A(OUTREC)                               
         LR    R0,RE               R0 = A(OUTREC)                               
         LA    R1,C' '             R1 = SPACE                                   
         SLL   R1,24               MOVE TO HOB                                  
         MVCL  RE,R0               INIT OUTREC TO SPACES                        
*                                                                               
         EDIT  VENCOUNT,GENINCNT,0,COMMAS=YES,ALIGN=LEFT                        
*                                                                               
         L     R4,AOUTREC          R4 = A(OUTREC)                               
*                                                                               
         LAY   RE,GFINJSON         FINAL JSON TABLE                             
         ST    RE,DMCB+4           PARM 2                                       
         LAY   RE,GNHSOFT          INVOICE HEADER SOFT FIELDS TABLE             
         ST    RE,DMCB+8           PARM 3                                       
         L     RE,ALISREC          A(SOFT FIELDS)                               
         ST    RE,DMCB+12          PARM 4                                       
         LA    RE,=X'FF'           NO SPECIAL TABLE                             
         ST    RE,DMCB+16          PARM 5                                       
*                                                                               
         GOTO1 =A(GNTABLE),DMCB,(RA)                                            
*                                                                               
         BRAS  RE,SENDMQG          SEND JSON PAYLOAD VIA MQ MSG                 
*                                                                               
         BRAS  RE,PRNTJSON         PRINT JSON DATA                              
*                                                                               
GNOUTXIT XIT1                      DONE                                         
*                                                                               
         DROP  R2,R5               DROP USINGS                                  
*                                                                               
         GETELN R3,33,ELCODE9,9    GETEL9 MACRO                                 
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
         MVC   SRTGMED,GEBMED      AGENCY/MEDIA                                 
         MVC   SRTGCLT,GEBCLT      3-CHARACTER CLIENT                           
         MVC   SRTGPRD,GEBPRD      3-CHARACTER PRODUCT                          
         MVC   SRTGEST,GEBEST      BINARY ESTIMATE                              
         MVC   SRTGINV,GEBINVN     PACKED INVOICE NUMBER                        
         MVC   SRTGBDAT,GEBINVM    BILLED DATE COMPRESSED                       
         MVC   SRTGMOS,GEBIMOS     MOS                                          
*                                                                               
         MVC   WORK(3),SRTGMOS     BILLABLE DATE                                
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         GOTO1 DATCON,DMCB,(3,WORK),(6,SRTGBMOS)  MMM/YY                        
*                                                                               
         MVC   SRTGBGRS,GEBGROSS   GROSS                                        
         MVC   SRTGBNET,GEBNET     NET                                          
         MVC   SRTGBACT,GEBACT     ACTUAL                                       
***      MVC   SRTGBCOM,GEBCOM     COMMISSION ADJUSTMENT                        
         MVC   SRTGBCDC,GEBCSHD    CASH DISCOUNT                                
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
         MVC   SRTGSEP,GEBSEP      SEPARATE BILL                                
         MVC   SRTGBDIV,GEBDIV     DIVISION                                     
         MVC   SRTGBREG,GEBREG     REGION                                       
         MVC   SRTGBDIS,GEBDIS     DISTRICT                                     
         MVC   SRTGMEDN,GEBMEDN    MEDIA NAME                                   
         MVC   SRTGCNAM,GEBCNAME   CLIENT NAME                                  
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
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
SENDMQG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   MQMSG,C'N'          SUPPRESS MQ NOTIFICATION?                    
         BE    SENDMQGX            YES                                          
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
SENDMQGX XIT1                      DONE                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
GNHTAB   DC    X'00',X'01',C'{'                                                 
         DC    X'00',X'19',C'"messageType": "INVOICE",'                         
         DC    X'05',X'13',C'"exportDateTime": "'                               
         DC    X'00',X'02',C'",'                                                
         DC    X'00',X'12',C'"system": "PRINT",'                                
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
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'02',C'},'                                                
         DC    X'00',X'0B',C'"client": {'                                       
         DC    X'09',X'09',C'"code": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'0A',X'09',C'"name": "'                                         
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
         DC    X'35',X'15',C'"billReceiptName1": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'36',X'15',C'"billReceiptName2": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'33',X'11',C'"addressLine1": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'34',X'11',C'"addressLine2": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'37',X'15',C'"productInterface": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'38',X'09',C'"attn": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'28',X'0F',C'"accountNum": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'37',X'15',C'"productInterface": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'39',X'0B',C'"ucomm1": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'3A',X'0B',C'"ucomm2": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'3B',X'0B',C'"ucomm3": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'3C',X'0B',C'"ucomm4": "'                                       
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
         DC    X'00',X'0D',C'"division": {'                                     
         DC    X'29',X'09',C'"code": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'2A',X'09',C'"name": "'                                         
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'02',C'},'                                                
         DC    X'00',X'0B',C'"region": {'                                       
         DC    X'2B',X'09',C'"code": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'2C',X'09',C'"name": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'2D',X'0B',C'"ucomm1": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'2E',X'0B',C'"ucomm2": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'2F',X'0B',C'"ucomm3": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'30',X'0B',C'"ucomm4": "'                                       
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'02',C'},'                                                
         DC    X'00',X'0D',C'"district": {'                                     
         DC    X'32',X'09',C'"code": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'54',X'0B',C'"ucomm1": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'59',X'0B',C'"ucomm2": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'5A',X'0B',C'"ucomm3": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'5B',X'0B',C'"ucomm4": "'                                       
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
         DC    X'5C',X'14',C'"cashDiscountAmt": "'                              
         DC    X'00',X'02',C'",'                                                
         DC    X'1E',X'0B',C'"taxAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'25',X'0D',C'"billType": "'                                     
         DC    X'00',X'02',C'",'                                                
         DC    X'42',X'19',C'"commissionAdjustFlag": "'                         
         DC    X'00',X'02',C'",'                                                
         DC    X'41',X'13',C'"sepAdjBillFlag": "'                               
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
         DC    X'50',X'19',C'"groupMTradeMidasFlag": "'                         
         DC    X'00',X'02',C'",'                                                
         DC    X'51',X'11',C'"wbFlightCode": "'                                 
         DC    X'00',X'02',C'",'                                                
         DC    X'52',X'15',C'"calculatedNetAmt": "'                             
         DC    X'00',X'02',C'",'                                                
         DC    X'53',X'0E',C'"cost2Flag": "'                                    
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
         DC    X'4E',X'0C',C'"hstRate": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'27',X'0C',C'"taxType": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'24',X'0D',C'"province": "'                                     
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'02',C'},'                                                
         DC    X'FF'                                                            
***                                                                             
* THE TABLE BELOW IS AN EXCLUSION TABLE                                         
* THIS INDICATES THAT IF THE FIRST BYTE OF THE GNHTAB TABLE                     
* ENTRY MATCHES THE FIRST BYTE OF THIS TABLE ENTRY, WE NEED                     
* TO EXCLUDE THAT FIELD IF THE BITS IN THE SECOND BYTE DON'T MATCH              
* THE FOLLOWING                                                                 
* X'01' = CANADIAN FIELD                                                        
* X'02' = US FIELD                                                              
***                                                                             
***********************************************************************         
* THE TABLE BELOW IS AN EXCLUSION TABLE - HERE IS HOW THIS WORKS      *         
*                                                                     *         
* IF( 1ST BYTE OF GNHTAB ENTRY = 1ST BYTE OF GNHEXL ENTRY ){          *         
*   SEE CODE IN EXCLD10                                               *         
* }                                                                   *         
*                                                                     *         
* IF 2ND BYTE OF TABLE HAS THE X'01' BIT SET = CANADIAN ONLY FIELD    *         
* IF 2ND BYTE OF TABLE HAS THE X'02' BIT SET = US ONLY FIELD          *         
***********************************************************************         
GNHEXL   DC    X'1F',X'01'   gstAmt                 = CANADIAN                  
         DC    X'20',X'01'   gstRate                = CANADIAN                  
         DC    X'21',X'01'   pstAmt                 = CANADIAN                  
         DC    X'22',X'01'   pstRate                = CANADIAN                  
         DC    X'23',X'01'   hstAmt                 = CANADIAN                  
         DC    X'4E',X'01'   hstRate                = CANADIAN                  
         DC    X'27',X'01'   taxType                = CANADIAN                  
         DC    X'24',X'01'   province               = CANADIAN                  
         DC    X'1E',X'02'   taxAmt                 = US                        
         DC    X'FF'                                                            
*                                                                               
GVENTAB  DC    X'00',X'0B',C'"vendors": '                                       
         DC    X'00',X'01',X'BA'                THIS IS A "["                   
         DC    X'00',X'01',C'{'                                                 
         DC    X'01',X'0C',C'"pubCode": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'02',X'0C',C'"pubName": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'03',X'0C',C'"pubZone": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'04',X'0F',C'"pubEdition": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'05',X'0D',C'"grossAmt": "'                                     
         DC    X'00',X'02',C'",'                                                
         DC    X'06',X'0B',C'"netAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'07',X'18',C'"agencyCommissionAmt": "'                          
         DC    X'00',X'02',C'",'                                                
         DC    X'08',X'14',C'"cashDiscountAmt": "'                              
         DC    X'00',X'02',C'",'                                                
         DC    X'09',X'0E',C'"cost2Flag": "'                                    
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'01',C'}'                                                 
         DC    X'FF'                                                            
*                                                                               
GVENTAB2 DC    X'00',X'01',C','                                                 
         DC    X'00',X'01',C'{'                                                 
         DC    X'01',X'0C',C'"pubCode": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'02',X'0C',C'"pubName": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'03',X'0C',C'"pubZone": "'                                      
         DC    X'00',X'02',C'",'                                                
         DC    X'04',X'0F',C'"pubEdition": "'                                   
         DC    X'00',X'02',C'",'                                                
         DC    X'05',X'0D',C'"grossAmt": "'                                     
         DC    X'00',X'02',C'",'                                                
         DC    X'06',X'0B',C'"netAmt": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'07',X'18',C'"agencyCommissionAmt": "'                          
         DC    X'00',X'02',C'",'                                                
         DC    X'08',X'14',C'"cashDiscountAmt": "'                              
         DC    X'00',X'02',C'",'                                                
         DC    X'09',X'0E',C'"cost2Flag": "'                                    
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'01',C'}'                                                 
         DC    X'FF'                                                            
*                                                                               
GREGTAB  DC    X'00',X'0A',C'"region": '                                        
         DC    X'00',X'01',X'BA'                THIS IS A "["                   
GREGTAB2 DC    X'00',X'01',C'{'                                                 
         DC    X'2B',X'09',C'"code": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'2C',X'09',C'"name": "'                                         
         DC    X'00',X'02',C'",'                                                
         DC    X'5D',X'12',C'"percentageAmt": "'                                
         DC    X'00',X'02',C'",'                                                
         DC    X'2D',X'0B',C'"ucomm1": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'2E',X'0B',C'"ucomm2": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'2F',X'0B',C'"ucomm3": "'                                       
         DC    X'00',X'02',C'",'                                                
         DC    X'30',X'0B',C'"ucomm4": "'                                       
         DC    X'00',X'01',C'"'                                                 
         DC    X'00',X'01',C'}'                                                 
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
         DC    X'00',X'12',C'"system": "PRINT",'                                
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
         DC    X'4E',AL2(GENINHSR-GENSFLDH),AL1(L'GENINHSR)                     
         DC    X'24',AL2(GENINPRV-GENSFLDH),AL1(L'GENINPRV)                     
         DC    X'25',AL2(GENINTYP-GENSFLDH),AL1(L'GENINTYP)                     
         DC    X'26',AL2(GENINCNT-GENSFLDH),AL1(L'GENINCNT)                     
         DC    X'27',AL2(GENINTXT-GENSFLDH),AL1(L'GENINTXT)                     
         DC    X'28',AL2(GENPRDAN-GENSFLDH),AL1(L'GENPRDAN)                     
         DC    X'29',AL2(GENINDIV-GENSFLDH),AL1(L'GENINDIV)                     
         DC    X'2A',AL2(GENINDNM-GENSFLDH),AL1(L'GENINDNM)                     
         DC    X'2B',AL2(GENINREG-GENSFLDH),AL1(L'GENINREG)                     
         DC    X'2C',AL2(GENINRNM-GENSFLDH),AL1(L'GENINRNM)                     
         DC    X'2D',AL2(GENRGUC1-GENSFLDH),AL1(L'GENRGUC1)                     
         DC    X'2E',AL2(GENRGUC2-GENSFLDH),AL1(L'GENRGUC2)                     
         DC    X'2F',AL2(GENRGUC3-GENSFLDH),AL1(L'GENRGUC3)                     
         DC    X'30',AL2(GENRGUC4-GENSFLDH),AL1(L'GENRGUC4)                     
         DC    X'31',AL2(GENINVDD-GENSFLDH),AL1(L'GENINVDD)                     
         DC    X'32',AL2(GENINDIS-GENSFLDH),AL1(L'GENINDIS)                     
         DC    X'33',AL2(GENPRDA1-GENSFLDH),AL1(L'GENPRDA1)                     
         DC    X'34',AL2(GENPRDA2-GENSFLDH),AL1(L'GENPRDA2)                     
         DC    X'35',AL2(GENPRDR1-GENSFLDH),AL1(L'GENPRDR1)                     
         DC    X'36',AL2(GENPRDR2-GENSFLDH),AL1(L'GENPRDR2)                     
         DC    X'37',AL2(GENPRDIC-GENSFLDH),AL1(L'GENPRDIC)                     
         DC    X'38',AL2(GENPRDAT-GENSFLDH),AL1(L'GENPRDAT)                     
         DC    X'39',AL2(GENPRDU1-GENSFLDH),AL1(L'GENPRDU1)                     
         DC    X'3A',AL2(GENPRDU2-GENSFLDH),AL1(L'GENPRDU2)                     
         DC    X'3B',AL2(GENPRDU3-GENSFLDH),AL1(L'GENPRDU3)                     
         DC    X'3C',AL2(GENPRDU4-GENSFLDH),AL1(L'GENPRDU4)                     
         DC    X'3D',AL2(GENESUC5-GENSFLDH),AL1(L'GENESUC5)                     
         DC    X'3E',AL2(GENESUC6-GENSFLDH),AL1(L'GENESUC6)                     
         DC    X'3F',AL2(GENESUC7-GENSFLDH),AL1(L'GENESUC7)                     
         DC    X'40',AL2(GENESUC8-GENSFLDH),AL1(L'GENESUC8)                     
         DC    X'41',AL2(GENSADJB-GENSFLDH),AL1(L'GENSADJB)                     
         DC    X'42',AL2(GENCOMAJ-GENSFLDH),AL1(L'GENCOMAJ)                     
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
         DC    X'50',AL2(GENGMTDM-GENSFLDH),AL1(L'GENGMTDM)                     
         DC    X'51',AL2(GENWBFLT-GENSFLDH),AL1(L'GENWBFLT)                     
         DC    X'52',AL2(GENINNTC-GENSFLDH),AL1(L'GENINNTC)                     
         DC    X'53',AL2(GENCOST2-GENSFLDH),AL1(L'GENCOST2)                     
         DC    X'54',AL2(GENDGUC1-GENSFLDH),AL1(L'GENDGUC1)                     
         DC    X'59',AL2(GENDGUC2-GENSFLDH),AL1(L'GENDGUC2)                     
         DC    X'5A',AL2(GENDGUC3-GENSFLDH),AL1(L'GENDGUC3)                     
         DC    X'5B',AL2(GENDGUC4-GENSFLDH),AL1(L'GENDGUC4)                     
         DC    X'5C',AL2(GENINCDC-GENSFLDH),AL1(L'GENINCDC)                     
         DC    X'5D',AL2(GENREGPC-GENSFLDH),AL1(L'GENREGPC)                     
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
GNVSOFT  DC    X'01',AL2(GENVPUBN-GENSFLDV),AL1(L'GENVPUBN)                     
         DC    X'02',AL2(GENVPBNM-GENSFLDV),AL1(L'GENVPBNM)                     
         DC    X'03',AL2(GENVZONE-GENSFLDV),AL1(L'GENVZONE)                     
         DC    X'04',AL2(GENVEDIT-GENSFLDV),AL1(L'GENVEDIT)                     
         DC    X'05',AL2(GENVGRSD-GENSFLDV),AL1(L'GENVGRSD)                     
         DC    X'06',AL2(GENVNETD-GENSFLDV),AL1(L'GENVNETD)                     
         DC    X'07',AL2(GENVAGCM-GENSFLDV),AL1(L'GENVAGCM)                     
         DC    X'08',AL2(GENVCDSC-GENSFLDV),AL1(L'GENVCDSC)                     
         DC    X'09',AL2(GENVCOS2-GENSFLDV),AL1(L'GENVCOS2)                     
         DC    X'FF'                                                            
*                                                                               
         TITLE 'MQMESSGE - SEND MQ MESSAGE WITH FILE'                           
MQMESSGE CSECT                                                                  
         NMOD1 0,MQMESSGE                                                       
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
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
         MVC   MQSYS,=C'PRT'       PRINT SYSTEM                                 
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
         TITLE 'READUCOM - READ THE UCOMM RECORD'                               
READUCOM CSECT                                                                  
         NMOD1 0,READUCOM                                                       
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R3,UCOMBLK          UCOMM CONTROL BLOCK                          
         USING DDUCOMD,R3          UCOMM DSECT                                  
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM CONTROL BLOCK                    
         MVC   UCACOMF,VCOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'P'          SYSTEM                                       
         MVC   UCAGY,PBILKAGY      AGENCY                                       
         MVC   UCMED,PBILKMED      MEDIA                                        
         MVC   UCCLT,PBILKCLT      CLIENT                                       
         MVC   UCPRD,PBILKPRD      PRODUCT                                      
         MVC   UCEST,PBILKEST      ESTIMATE                                     
         OI    UCOPT,UCOEST        RETURN ESTIMATE UCOMMS                       
*                                                                               
         MVC   SVEUCOM1,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM2,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM3,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVEUCOM4,SPACES     INIT IN CASE OF ERROR                        
*                                                                               
         GOTO1 =V(DDUCOM),UCOMBLK  GET ESTIMATE UCOMMS                          
*                                                                               
         CLI   UCERROR,0           ANY ERROR?                                   
         BNE   RDUXIT              YES                                          
         TM    UCDATA,UCDNOEST     FOUND EST LEVEL UCOMM REC?                   
         BO    RDUXIT              NO                                           
*                                                                               
         L     R1,UCEDATA          ESTIMATE UCOMM DATA                          
         LA    RF,UCELENS          ESTIMATE UCOMM LENGTHS                       
         LA    RE,SVEUCOM1         START OF FIRST SAVED ESTIMATE UCOMM          
         LHI   R0,4                4 ESTIMATE UCOMMS                            
*                                                                               
RDU05    CLI   0(RF),0             IS ESTIMATE UCOMM LENGTH ZERO?               
         BE    *+16                YES - DON'T HAVE THIS ESTIMATE UCOMM         
         MVC   0(32,RE),0(R1)      MOVE IN ESTIMATE UCOMM                       
         OC    0(32,RE),SPACES     SPACE PAD                                    
         LA    RE,32(RE)           BUMP TO NEXT SAVED ESTIMATE UCOMM            
         LA    R1,32(R1)           BUMP TO NEXT ESTIMATE UCOMM                  
         LA    RF,1(RF)            BUMP TO NEXT ESTIMATE UCOMM LENGTH           
         BCT   R0,RDU05            PROCESS NEXT ESTIMATE UCOMM                  
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
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R3,UCOMBLK          UCOMM CONTROL BLOCK                          
         USING DDUCOMD,R3          UCOMM DSECT                                  
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM CONTROL BLOCK                    
         MVC   UCACOMF,VCOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'P'          SYSTEM                                       
         MVC   UCAGY,PPRDKAGY      AGENCY                                       
         MVC   UCMED,PPRDKMED      MEDIA                                        
         MVC   UCCLT,PPRDKCLT      CLIENT                                       
         MVC   UCPRD,PPRDKPRD      PRODUCT                                      
*                                                                               
         CLC   SVUCPKEY,UCAGY      DID I JUST CHECK THIS?                       
         BE    PUXIT               YES                                          
         MVC   SVUCPKEY,UCAGY      SAVE KEY FOR OPTIMIZATION                    
         MVC   SVPUCOM1,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVPUCOM2,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVPUCOM3,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVPUCOM4,SPACES     INIT IN CASE OF ERROR                        
*                                                                               
         OI    UCOPT,UCOPRD        RETURN PRODUCT UCOMMS                        
*                                                                               
         GOTO1 =V(DDUCOM),UCOMBLK  GET ESTIMATE UCOMMS                          
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
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R3,UCOMBLK          UCOMM CONTROL BLOCK                          
         USING DDUCOMD,R3          UCOMM DSECT                                  
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM CONTROL BLOCK                    
         MVC   UCACOMF,VCOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'P'          SYSTEM                                       
         MVC   UCAGY,PESTKAGY      AGENCY                                       
         MVC   UCMED,PESTKMED      MEDIA                                        
         MVC   UCCLT,PESTKCLT      CLIENT                                       
         MVC   UCPRD,PESTKPRD      PRODUCT                                      
         MVC   UCEST,PESTKEST      ESTIMATE                                     
*                                                                               
         CLC   SVUCEKEY,UCAGY      DID I JUST CHECK THIS?                       
         BE    EUXIT               YES                                          
         MVC   SVUCEKEY,UCAGY      SAVE KEY FOR OPTIMIZATION                    
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
*                                                                               
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
         TITLE 'REGUCOMM - READ THE REGION UCOMMS'                              
REGUCOMM CSECT                                                                  
         NMOD1 0,REGUCOMM                                                       
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R3,UCOMBLK          UCOMM CONTROL BLOCK                          
         USING DDUCOMD,R3          UCOMM DSECT                                  
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM CONTROL BLOCK                    
         MVC   UCACOMF,VCOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'P'          SYSTEM                                       
         MVC   UCAGY,PPRDKAGY      AGENCY                                       
         MVC   UCMED,PPRDKMED      MEDIA                                        
         MVC   UCCLT,PPRDKCLT      CLIENT                                       
         MVC   UCPRD,PPRDKPRD      PRODUCT                                      
         MVC   UCEST,PESTKEST      ESTIMATE                                     
         MVC   UCDIV,PREGKDIV      DIVISION                                     
         MVC   UCREG,PREGKREG      REGION                                       
*                                                                               
         CLC   SVUCRKEY,UCAGY      DID I JUST CHECK THIS?                       
         BE    RUXIT               YES                                          
         MVC   SVUCRKEY,UCAGY      SAVE KEY FOR OPTIMIZATION                    
         MVC   SVRUCOM1,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVRUCOM2,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVRUCOM3,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVRUCOM4,SPACES     INIT IN CASE OF ERROR                        
*                                                                               
         OI    UCOPT,UCOREG        RETURN REGION UCOMMS                         
*                                                                               
         GOTO1 =V(DDUCOM),UCOMBLK  GET REGION UCOMMS                            
*                                                                               
         CLI   UCERROR,0           ANY ERROR?                                   
         BNE   RUXIT               YES                                          
         TM    UCDATA,UCDNOREG     FOUND REG LEVEL UCOMM REC?                   
         BO    RUXIT               NO                                           
*                                                                               
         L     R1,UCRDATA          REGION UCOMM DATA                            
         LA    RF,UCRLENS          REGION UCOMM LENGTHS                         
         LA    RE,SVRUCOM1         START OF FIRST SAVED REGION UCOMM            
         LHI   R0,4                4 REGION UCOMMS                              
*                                                                               
RU05     CLI   0(RF),0             IS REGION UCOMM LENGTH ZERO?                 
         BE    *+10                YES - DON'T HAVE THIS PRODUCT UCOMM          
         MVC   0(32,RE),0(R1)      MOVE IN REGION UCOMM                         
***                                                                             
* DO NOT SPACE PAD THIS DATA                                                    
* AS PER SPEC-48613, IF SPECIAL CHARACTERS SUCH AS A TILDA ARE USED             
* A X'A1' BECOMES A X'E1'                                                       
***                                                                             
***      OC    0(32,RE),SPACES     SPACE PAD                                    
         LA    RE,32(RE)           BUMP TO NEXT SAVED REGION UCOMM              
         LA    R1,32(R1)           BUMP TO NEXT REGION UCOMM                    
         LA    RF,1(RF)            BUMP TO NEXT REGION UCOMM LENGTH             
         BCT   R0,RU05             PROCESS NEXT REGION UCOMM                    
*                                                                               
RUXIT    XIT1                      EXIT                                         
         DROP  R3                  DROP UCOMM CONTROL BLOCK USING               
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'DISUCOMM - READ THE DISTRICT UCOMMS'                            
DISUCOMM CSECT                                                                  
         NMOD1 0,DISUCOMM                                                       
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         LA    R3,UCOMBLK          UCOMM CONTROL BLOCK                          
         USING DDUCOMD,R3          UCOMM DSECT                                  
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM CONTROL BLOCK                    
         MVC   UCACOMF,VCOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'P'          SYSTEM                                       
         MVC   UCAGY,PPRDKAGY      AGENCY                                       
         MVC   UCMED,PPRDKMED      MEDIA                                        
         MVC   UCCLT,PPRDKCLT      CLIENT                                       
         MVC   UCPRD,PPRDKPRD      PRODUCT                                      
         MVC   UCEST,PESTKEST      ESTIMATE                                     
         MVC   UCDIV,PREGKDIV      DIVISION                                     
         MVC   UCREG,PREGKREG      REGION                                       
         ICM   RE,15,4(R1)         A(DISTRICT)                                  
         MVC   UCDST,0(RE)         DISTRICT                                     
*                                                                               
         CLC   SVUCDKEY,UCAGY      DID I JUST CHECK THIS?                       
         BE    DUXIT               YES                                          
         MVC   SVUCDKEY,UCAGY      SAVE KEY FOR OPTIMIZATION                    
         MVC   SVDUCOM1,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVDUCOM2,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVDUCOM3,SPACES     INIT IN CASE OF ERROR                        
         MVC   SVDUCOM4,SPACES     INIT IN CASE OF ERROR                        
*                                                                               
         OI    UCOPT,UCODST        RETURN DISTRICT UCOMMS                       
*                                                                               
         GOTO1 =V(DDUCOM),UCOMBLK  GET DISTRICT UCOMMS                          
*                                                                               
         CLI   UCERROR,0           ANY ERROR?                                   
         BNE   DUXIT               YES                                          
         TM    UCDATA,UCDNODST     FOUND DIS LEVEL UCOMM REC?                   
         BO    DUXIT               NO                                           
*                                                                               
         L     R1,UCDDATA          DISTRICT UCOMM DATA                          
         LA    RF,UCDLENS          DISTRICT UCOMM LENGTHS                       
         LA    RE,SVDUCOM1         START OF FIRST SAVED DISTRICT UCOMM          
         LHI   R0,4                4 DISTRICT UCOMMS                            
*                                                                               
DU05     CLI   0(RF),0             IS DISTRICT UCOMM LENGTH ZERO?               
         BE    *+10                YES - DON'T HAVE THIS DISTRICT UCOMM         
         MVC   0(32,RE),0(R1)      MOVE IN DISTRICT UCOMM                       
***                                                                             
* DO NOT SPACE PAD THIS DATA                                                    
* AS PER SPEC-48613, IF SPECIAL CHARACTERS SUCH AS A TILDA ARE USED             
* A X'A1' BECOMES A X'E1'                                                       
***                                                                             
***      OC    0(32,RE),SPACES     SPACE PAD                                    
         LA    RE,32(RE)           BUMP TO NEXT SAVED DISTRICT UCOMM            
         LA    R1,32(R1)           BUMP TO NEXT DISTRICT UCOMM                  
         LA    RF,1(RF)            BUMP TO NEXT DISTRICT UCOMM LENGTH           
         BCT   R0,DU05             PROCESS NEXT DISTRICT UCOMM                  
*                                                                               
DUXIT    XIT1                      EXIT                                         
         DROP  R3                  DROP UCOMM CONTROL BLOCK USING               
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDPRD - READ PRODUCT RECORD IF NOT ALREADY IN CORE'             
RDPRD    CSECT                                                                  
         NMOD1 0,RDPRD                                                          
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
KP       USING PPRDREC,R6          PRODUCT RECORD DSECT                         
         MVC   KP.PPRDKAGY,SAVEAGY AGENCY                                       
         ICM   RE,15,4(R1)         A(MEDIA CODE)                                
         MVC   KP.PPRDKMED,0(RE)   MEDIA                                        
         MVI   KP.PPRDKRCD,X'06'   PRODUCT RECORD CODE                          
         ICM   RE,15,8(R1)         A(CLIENT CODE)                               
         MVC   KP.PPRDKCLT,0(RE)   CLIENT CODE                                  
         ICM   RE,15,12(R1)        A(PRODUCT CODE)                              
         MVC   KP.PPRDKPRD,0(RE)    PRODUCT CODE                                
         CLC   KP.PPRDKEY,PPRDKEY  ALREADY HAVE THE PRD REC IN CORE?            
         BE    RDPRDXIT            YES - DO NOT RE-READ                         
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   KP.PPRDKEY,WORK     PRODUCT KEY                                  
         GOTO1 HIGH                READ HIGH                                    
         CLC   KP.PPRDKEY,KEYSAVE  DID WE FIND THE PRODUCT KEY?                 
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  KP                  DROP LABELED PRODUCT REC USING               
*                                                                               
         LA    R0,PPRDREC          R0 = A(PPRDREC)                              
         ST    R0,AREC             AREC = PPRDREC                               
         GOTO1 GETPRT              GET THE PRODUCT RECORD                       
*                                                                               
RDPRDXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDEST - READ ESTIMATE RECORD IF NOT ALREADY IN CORE'            
RDEST    CSECT                                                                  
         NMOD1 0,RDEST                                                          
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
KE       USING PESTREC,R6          ESTIMATE RECORD DSECT                        
         MVC   KE.PESTKAGY,SAVEAGY AGENCY                                       
         ICM   RE,15,4(R1)         A(MEDIA CODE)                                
         MVC   KE.PESTKMED,0(RE)   MEDIA                                        
         MVI   KE.PESTKRCD,X'07'   ESTIMATE RECORD CODE                         
         ICM   RE,15,8(R1)         A(CLIENT CODE)                               
         MVC   KE.PESTKCLT,0(RE)   CLIENT CODE                                  
         ICM   RE,15,12(R1)        A(PRODUCT CODE)                              
         MVC   KE.PESTKPRD,0(RE)   PRODUCT CODE                                 
         ICM   RE,15,16(R1)        A(ESTIMATE CODE)                             
         MVC   KE.PESTKEST,0(RE)   ESTIMATE CODE                                
         CLC   KE.PESTKEY,PESTKEY  ALREADY HAVE THE EST RECORD IN CORE?         
         BE    RDESTXIT            YES - DO NOT RE-READ                         
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   KE.PESTKEY,WORK     ESTIMATE KEY                                 
         GOTO1 HIGH                READ HIGH                                    
         CLC   KE.PESTKEY,KEYSAVE  DID WE FIND THE ESTIMATE KEY?                
         BE    *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         DROP  KE                  DROP LABELED ESTIMATE REC USING              
*                                                                               
         LA    R0,PESTREC          R0 = A(PESTREC)                              
         ST    R0,AREC             AREC = PESTREC                               
         GOTO1 GETPRT              GET THE ESTIMATE RECORD                      
*                                                                               
RDESTXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDDIV - READ DIVISION RECORD IF NOT ALREADY IN CORE'            
RDDIV    CSECT                                                                  
         NMOD1 0,RDDIV                                                          
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
KD       USING PDIVREC,R6          DIVISION RECORD DSECT                        
         MVC   KD.PDIVKAGY,SAVEAGY AGENCY                                       
         ICM   RE,15,4(R1)         A(MEDIA CODE)                                
         MVC   KD.PDIVKMED,0(RE)   MEDIA                                        
         MVI   KD.PDIVKRCD,X'03'   X'03' DIVISION RECORD ID                     
         ICM   RE,15,8(R1)         A(CLIENT CODE)                               
         MVC   KD.PDIVKCLT,0(RE)   CLIENT CODE                                  
         ICM   RE,15,12(R1)        A(DIVISION CODE)                             
         MVC   KD.PDIVKDIV,0(RE)   DIVISION CODE                                
         CLC   KD.PDIVKEY,PDIVKEY  ALREADY HAVE THE DIV RECORD IN CORE?         
         BE    RDDIVXIT            YES - DO NOT RE-READ                         
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   KD.PDIVKEY,WORK     MOVE DIV KEY TO KEY                          
         GOTO1 HIGH                READ HIGH                                    
         CLC   KD.PDIVKEY,KEYSAVE  DID WE FIND THE DIV KEY?                     
         BNE   RDDIVNEQ            NO - SET CC NEQ                              
         DROP  KD                  DROP LABELED DIVISION REC USING              
*                                                                               
         LA    R0,PDIVREC          R0 = A(PDIVREC)                              
         ST    R0,AREC             AREC = PDIVREC                               
         GOTO1 GETPRT              GET THE DIV RECORD                           
*                                                                               
         CR    RE,RE               SET CC EQU                                   
         B     RDDIVXIT            AND EXIT                                     
*                                                                               
RDDIVNEQ LTR   RE,RE               SET CC NEQ                                   
*                                                                               
RDDIVXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'RDREG - READ REGION RECORD IF NOT ALREADY IN CORE'              
RDREG    CSECT                                                                  
         NMOD1 0,RDREG                                                          
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         LA    R6,WORK             R6 = WORK                                    
KR       USING PREGREC,R6          REGION RECORD DSECT                          
         MVC   KR.PREGKAGY,SAVEAGY AGENCY                                       
         ICM   RE,15,4(R1)         A(MEDIA CODE)                                
         MVC   KR.PREGKMED,0(RE)   MEDIA                                        
         MVI   KR.PREGKRCD,X'04'   REGION RECORD ID                             
         ICM   RE,15,8(R1)         A(CLIENT CODE)                               
         MVC   KR.PREGKCLT,0(RE)   CLIENT CODE                                  
         ICM   RE,15,12(R1)        A(DIVISION CODE)                             
         MVC   KR.PREGKDIV,0(RE)   DIVISION CODE                                
         ICM   RE,15,16(R1)        A(REGION CODE)                               
         MVC   KR.PREGKREG,0(RE)   REGION CODE                                  
         CLC   KR.PREGKEY,PREGKEY  ALREADY HAVE REGION RECORD IN CORE?          
         BE    RDREGXIT            YES - DO NOT RE-READ                         
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         MVC   KR.PREGKEY,WORK     MOVE REGION KEY TO KEY                       
         GOTO1 HIGH                READ HIGH                                    
         CLC   KR.PREGKEY,KEYSAVE  DID WE FIND THE REGION KEY?                  
         BNE   RDREGNEQ            NO - SET CC NEQ                              
         DROP  KR                  DROP LABELED DIVISION REC USING              
*                                                                               
         LA    R0,PREGREC          R0 = A(PREGREC)                              
         ST    R0,AREC             AREC = PREGREC                               
         GOTO1 GETPRT              GET THE REGION RECORD                        
*                                                                               
         CR    RE,RE               SET CC EQU                                   
         B     RDREGXIT            AND EXIT                                     
*                                                                               
RDREGNEQ LTR   RE,RE               SET CC NEQ                                   
*                                                                               
RDREGXIT XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'VDREG - GET VENDOR LEVEL REGION INFO'                           
VDREG    CSECT                                                                  
         NMOD1 0,VDREG                                                          
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         USING GENSFLDH,R2         GENERIC EDI SOFT INV FIELDS DSECT            
         USING PUBDSTEL,R3         PUB REG/DIST ELEM                            
         USING SRTGRECD,R5         SRTGRECD DSECT                               
*                                                                               
         L     R0,ALISREC          INVOICE BLOCK                                
         AHI   R0,GENCLLEN         BUMP PAST DATA WE DON'T CLEAR                
         LHI   R1,GENSFDHQ         LENGTH OF INVOICE BLOCK                      
         SHI   R1,GENCLLEN         MINUS DATA WE AREN'T CLEARING                
         SR    RE,RE               CLEAR RE                                     
         SR    RF,RF               CLEAR RF                                     
         MVCL  R0,RE               CLEAR THE INVOICE BLOCK                      
*                                                                               
         MVC   GENINREG,PUBDREG    REGION CODE                                  
         EDIT  (B2,PUBDSHR),GENREGPC,2,ALIGN=LEFT                               
         DROP  R3                  DROP PUB REG/DIST ELEM USING                 
*                                                                               
         GOTO1 =A(RDREG),DMCB,(RA),SRTGMED,SRTGCLT,SRTGBDIV,GENINREG            
*                                                                               
         LA    R3,PREGREC          REGION RECORD                                
         USING PREGELEM,R3         REGION X'04' ELEMENT DSECT                   
         MVI   ELCODEA,X'04'       REGION X'04' ELEMENT                         
         BAS   RE,GETELA           HAVE THE ELEMENT?                            
         BNE   *+10                NO                                           
         MVC   GENINRNM,PREGNAME   REGION NAME                                  
         DROP  R3                  REGION X'04' ELEMENT DSECT                   
*                                                                               
         GOTO1 =A(REGUCOMM),DMCB,(RA) READ UCOMM DATA                           
*                                                                               
         MVC   GENRGUC1,SVRUCOM1   1ST REGION UCOMM                             
         MVC   GENRGUC2,SVRUCOM2   2ND REGION UCOMM                             
         MVC   GENRGUC3,SVRUCOM3   3RD REGION UCOMM                             
         MVC   GENRGUC4,SVRUCOM4   4TH REGION UCOMM                             
*                                                                               
VDREGXIT XIT1                      EXIT                                         
*                                                                               
         GETELN R3,33,ELCODEA,A    GETELA MACRO                                 
*                                                                               
         LTORG                     LITERAL POOL                                 
*                                                                               
         TITLE 'GNTABLE - MOVE TABLE DATA TO OUTPUT AREA'                       
GNTABLE  CSECT                                                                  
         NMOD1 0,GNTABLE                                                        
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         ICM   R6,15,4(R1)         A(TABLE)                                     
         ICM   R7,15,8(R1)         A(SOFT FIELD MAPPING TABLE)                  
         ICM   R2,15,12(R1)        A(SOFT FIELD DATA AREA)                      
         ICM   R5,15,16(R1)        A(EXCLUSIONS TABLE)                          
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
* IF 2ND BYTE OF TABLE HAS THE X'01' BIT SET = CANADIAN ONLY FIELD    *         
* IF 2ND BYTE OF TABLE HAS THE X'02' BIT SET = US ONLY FIELD          *         
*                                                                               
EXCLD10  CLI   CANADIAN,C'Y'       CANADIAN AGENCY?                             
         BNE   *+12                NO                                           
         TM    1(R5),X'01'         IS THIS A CANADIAN ONLY FIELD?               
         BNZ   EXCLDNEQ            YES - SET CC NEQ (DON'T EXCLUDE)             
*                                                                               
         CLI   CANADIAN,C'Y'       CANADIAN AGENCY?                             
         BE    *+12                YES                                          
         TM    1(R5),X'02'         IS THIS A US ONLY FIELD?                     
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
         EX    RF,*+8              EXECUTE THE CLC                              
         B     *+10                BRANCH OVER CLC                              
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
         BE    EMPTYEQU            YES                                          
         CLI   0(R4),C'{'          IS THERE AN OPENING BRACKET HERE?            
         BE    *+6                 YES                                          
         DC    H'0'                NO - THERE SHOULD BE!                        
*                                                                               
EMPTY20  CLI   0(R4),C','          IS THERE A COMMA HERE?                       
         BE    EMPTY30             YES                                          
         BCTR  R4,0                KEEP BACKING UP UNTIL WE HIT A COMMA         
         B     EMPTY20             CHECK TO SEE IF WE REACHED THE COMMA         
*                                                                               
*  If we get here then we had an entire JSON block that's empty                 
*  take for instance the example below                                          
*  If there is no region on the invoice, then all these fields                  
*  will be null. In that case, we need to skip over the C'},'                   
*  at the end as well                                                           
*                                                                               
*  DC    X'00',X'0B',C'"region": {'                                             
*  DC    X'2B',X'09',C'"code": "'                                               
*  DC    X'00',X'02',C'",'                                                      
*  DC    X'2C',X'09',C'"name": "'                                               
*  DC    X'00',X'02',C'",'                                                      
*  DC    X'2D',X'0B',C'"ucomm1": "'                                             
*  DC    X'00',X'02',C'",'                                                      
*  DC    X'2E',X'0B',C'"ucomm2": "'                                             
*  DC    X'00',X'02',C'",'                                                      
*  DC    X'2F',X'0B',C'"ucomm3": "'                                             
*  DC    X'00',X'02',C'",'                                                      
*  DC    X'30',X'0B',C'"ucomm4": "'                                             
*  DC    X'00',X'01',C'"'                                                       
*  DC    X'00',X'02',C'},'                                                      
*                                                                               
EMPTY30  AHI   R4,1                BUMP PAST THE COMMA                          
         CLI   2(R6),C'}'          CLOSING BRACE?                               
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
         LLC   RF,1(R6)            LENGTH OF LITERAL                            
         LA    R6,2(RF,R6)         BUMP TO NEXT TABLE ENTRY                     
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
UNIXFCRT NMOD1 0,UNIXFCRT                                                       
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
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
UNIXFPUT NMOD1 0,UNIXFPUT                                                       
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
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
UNIXFCLO NMOD1 0,UNIXFCLO                                                       
*                                                                               
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
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
         TITLE 'TAPEWRT - WRITE RECORD TO TAPE'                                 
TAPEWRT  CSECT                                                                  
         NMOD1 0,TAPEWRT                                                        
         L     RA,0(R1)            RA = A(P1)                                   
         USING PPWORKD,RA          GLOBAL WORKING STORAGE                       
         L     R9,PPWORK2C         R9 = A(PPWORK2C)                             
         USING PPWORK2D,R9         GLOBAL WORKING STORAGE 2                     
         L     RC,PPFILEC          RC = PPFILEC                                 
         LR    R7,RC               R7 = PPFILEC                                 
         AHI   R7,4096             R7 = PPFILEC+4096 (2ND BASE REG)             
         USING PPFILED,RC,R7       PPFILED DSECT                                
         LA    R8,SPACEND          START OF REMAINING WORK SPACE                
         USING PPVLWRKD,R8         OUR WORKING STORAGE                          
*                                                                               
         CLI   SVQOPT7,C'P'        PRINT RECORD OUT?                            
         BNE   TW20                NO                                           
*                                                                               
         L     R2,AOUTREC          R2 = A(OUTREC)                               
         LA    R3,OUTRECLN/L'P1-1  MAX LINES TO PRINT                           
*                                                                               
TW10     MVC   P1+1(L'P1-1),0(R2)  MOVE TO PRINT LINE                           
         GOTO1 REPORT              PRINT THE RECORD                             
*                                                                               
         LA    R2,L'P1-1(R2)       BUMP TO NEXT LINE                            
         OC    0(L'P1-1,R2),0(R2)  ANY MORE DATA TO REPORT?                     
         BZ    TW20                NO                                           
         BCT   R3,TW10                                                          
*                                                                               
TW20     CLI   SVQOPT6,C'Y'        TEST RUN?                                    
         BE    TW30                YES - NO TAPE                                
*                                                                               
         L     R1,APVLTAPE         R1 = TAPE                                    
         L     R0,AOUTREC          R0 = A(OUTREC)                               
         PUT   (1),(0)             ADD RECORD TO TAPE                           
*                                                                               
TW30     AP    TOTCNT,=P'1'        UPDATE RUNNING TOTAL OR TAPE RECS            
*                                                                               
         XIT1                      EXIT                                         
*                                                                               
         LTORG                     LITERAL POOL                                 
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
BLINVMOS DS    CL6                 INVOICE MOS MMM/YY                           
         DS    CL1                                                              
BLINVD   DS    CL10                INVOICE DATE                                 
         DS    CL1                                                              
BLNET    DS    CL14                NET BILLED                                   
         DS    CL1                                                              
BLPUB    DS    CL11                PUB                                          
         DS    CL1                                                              
BLPUBNAM DS    CL20                PUB NAME                                     
         DS    CL1                                                              
BLVAMT   DS    CL14                VENDOR AMOUNT                                
         ORG   BLPUB                                                            
BLTAX    DS    CL8                 INVOICE TAX                                  
         DS    CL1                                                              
BLACOM   DS    CL10                AGENCY COMMISSION                            
         ORG                                                                    
BKEYLENQ EQU   *-BLINE                                                          
*                                                                               
BBLINED  DSECT                     AUDITING REPORT DSECT FOR BELL               
BBLINE   DS    0CL198              198 CHAR PRINT LINE (WIDE)                   
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
BBLDIVC  DS    CL3                 DIVISION CODE                                
         DS    CL1                                                              
BBLDIVN  DS    CL20                DIVISION CODE NAME                           
         DS    CL1                                                              
BBLIVMOS DS    CL6                 INVOICE MOS                                  
         DS    CL1                                                              
BBLMED   DS    CL11                MEDIA TYPE                                   
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
BEUDEF2  DS    CL16                ESTIMATE UDEF2 (EMPLOYEE NUMBER)             
         DS    CL4                                                              
BEUDEF1  DS    CL32                ESTIMATE UDEF1 (EMPLOYEE NAME + NUM)         
         DS    CL28                                                             
BMEDMON  DS    CL8                 MEDIA MONTH (ALWAYS 1ST DAY OF MON)          
BINVAMT  DS    CL12                INVOICE AMOUNT (INCLUDING TAXES)             
BINVGST  DS    CL12                GST AMOUNT                                   
BGSTPCNT DS    CL8                 GST PERCENT                                  
         DS    CL20                FEDERAL TAX CODE (BLANK)                     
BINVQST  DS    CL12                QST AMOUNT                                   
BQSTPCNT DS    CL8                 QST PERCENT                                  
         DS    CL20                                                             
BPRDNAME DS    CL60                PRODUCT CODE + PRODUCT NAME                  
BDIVC    DS    CL3                 DIVISION CODE                                
BDIVN    DS    CL20                DIVISION NAME                                
BLFLENQ  EQU   *-BTYPEA            LENGTH OF INVOICE FILE LINE                  
         ORG   BTYPEA                                                           
BTYPEB   DS    CL1                 RECORD TYPE B (VENDOR LINE)                  
BVINVNUM DS    CL10                INVOICE NUMBER                               
         DS    CL2                                                              
BVCLIENT DS    CL3                 CLIENT CODE                                  
         DS    CL3                                                              
BVREGN1  DS    CL20                REGION NAME                                  
         DS    CL10                                                             
BVREGN2  DS    CL20                REGION NAME                                  
         DS    CL10                                                             
BVPUBNAM DS    CL20                VENDOR NAME                                  
         DS    CL10                                                             
BVAMT    DS    CL12                AMOUNT BEFORE TAXES                          
BVPAMT   DS    CL12                PREVIOUS AMOUNT (ALWAY 0)                    
         DS    CL12                                                             
BLVLENQ  EQU   *-BTYPEB            LENGTH OF VENDOR FILE LINE                   
*                                                                               
UBLINED  DSECT                     AUDITING REPORT DSECT FOR UB                 
UBLINE   DS    0CL132              132 CHAR PRINT LINE                          
         DS    CL4                                                              
UBMEDIA  DS    CL1                 MEDIA                                        
         DS    CL3                                                              
UBCLT    DS    CL3                 CLIENT                                       
         DS    CL1                                                              
UBPRD    DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
UBEST    DS    CL3                 ESTIMATE                                     
         DS    CL1                                                              
UBINVNO  DS    CL10                INVOICE NUMBER                               
         DS    CL1                                                              
UBINVMOS DS    CL6                 INVOICE MOS MMM/YY                           
         DS    CL1                                                              
UBINVD   DS    CL10                INVOICE DATE                                 
         DS    CL1                                                              
UBNET    DS    CL13                NET BILLED                                   
         DS    CL1                                                              
UBPUB    DS    CL11                PUB                                          
         DS    CL1                                                              
UBPLCMNT DS    CL20                PLACEMENT ID                                 
         DS    CL1                                                              
UBVAMT   DS    CL13                VENDOR AMOUNT                                
*                                                                               
PPVLWRKD DSECT                                                                  
*                                                                               
RCONS    DS    0F                  RELOCATED ADDRESSES                          
APRBILL  DS    A                   A(PRBILL)                                    
APRBUY   DS    A                   A(PRBUY)                                     
VFMTINO  DS    A                   A(PPFMTINO)                                  
VPUBFLOT DS    A                   V(PUBFLOAT)                                  
VSORTER  DS    A                   V(SORTER)                                    
VBINSRCH DS    A                   V(BINSRCH)                                   
VTAPEWRT DS    A                   V(TAPEWRT)                                   
VDDUCOM  DS    A                   V(DDUCOM)                                    
AWIDEC   DS    A                   V(WIDE)                                      
AMQMSG   DS    A                   A(MQMSG)                                     
*                                                                               
VBILLR   DS    A                   A(BILL ROUTINE)                              
VBUYR    DS    A                   A(BUY ROUTINE)                               
VOUTPR   DS    A                   A(OUTPUT ROUTINE)                            
*                                                                               
APVLTAPE DS    A                   A(PVLTAPE)                                   
AMQRPT   DS    A                   A(MQRPT)                                     
*                                                                               
ONEPRD   DS    CL1                 SET TO Y IF DOING ONE PRODUCT                
SVQELOW  DS    H                   EST LOW (X'0000' IF EST=ALL)                 
SVQEHI   DS    H                   EST HIGH (X'FFFF' IF EST=ALL)                
*                                                                               
SVBILEL  DS    F                   A(BILL ELEMENT)                              
ELEMBILL DS    XL1                 X'26' OR X'28' ELEMENT                       
SVQSTART DS    CL6                 REQUEST START                                
SVQEND   DS    CL6                 REQUEST END                                  
BQS      DS    XL3                 REQUEST START                                
BQE      DS    XL3                 REQUEST END                                  
ZEROS    DS    CL30                ZEROS                                        
LASTINV  DS    CL9                 MED/CLT/PRD/INV                              
MYWORK   DS    XL100               WORK AREA                                    
ELCODE1  DS    XL1                 GET GETEL1 MACRO                             
ELCODE2  DS    XL1                 GET GETEL2 MACRO                             
ELCODE3  DS    XL1                 GET GETEL3 MACRO                             
ELCODE4  DS    XL1                 GET GETEL4 MACRO                             
ELCODE5  DS    XL1                 GET GETEL5 MACRO                             
ELCODE6  DS    XL1                 GET GETEL6 MACRO                             
ELCODE7  DS    XL1                 GET GETEL7 MACRO                             
ELCODE8  DS    XL1                 GET GETEL8 MACRO                             
ELCODE9  DS    XL1                 GET GETEL9 MACRO                             
ELCODEA  DS    XL1                 GET GETELA MACRO                             
ELCODEB  DS    XL1                 GET GETELA MACRO                             
*                                                                               
PLCMNTID DS    XL20                PLACEMENT ID                                 
*                                                                               
PLCMSTAT DS    XL1                 PLACEMENT ID STATUS                          
PLCMERRH EQU   X'80'               REPORT HEADER ALREADY PRINTED                
PLCMERR  EQU   X'40'               MISSING PLACEMENT ID                         
*                                                                               
NOFILE   DS    CL1                 SET ON ERROR AND SUPPRESS FILE               
*                                                                               
SVLINE   DS    XL1                 SAVED PRINT LINE                             
SVFORCEH DS    XL1                 SAVED FORCEHED                               
*                                                                               
BINKEY   DS    CL(ULBKEYL)         BINSRCH KEY                                  
BINREC   DS    CL(ULBRECL)         BINSRCH RECORD DATA                          
*                                                                               
BINKEYBL DS    CL(BLBKEYL)         BINSRCH KEY BELL                             
BINRECBL DS    CL(BLBRECL)         BINSRCH RECORD DATA BELL                     
*                                                                               
BINKEYUB DS    CL(UBBKEYL)         BINSRCH KEY CARAT                            
BINRECUB DS    CL(UBBRECL)         BINSRCH RECORD DATA CARAT/                   
*                                                                               
BINKEYOU DS    CL(OUBKEYL)         BINSRCH KEY                                  
BINRECOU DS    CL(OUBRECL)         BINSRCH RECORD DATA                          
*                                                                               
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
VENDTOT  DS    PL6                 RUNNING TOTAL FOR VENDOR LINE                
INVTOT   DS    PL6                 RUNNING TOTAL FOR VENDOR LINE                
LASTVEND DS    PL6                 LAST VENDOR AMOUNT                           
*                                                                               
B1PROF   DS    XL16                B1 PROFILE                                   
B1XPROF  DS    XL16                B1X PROFILE                                  
SVQOPT4  DS    CL1                 SAVED QOPT4                                  
SVQOPT6  DS    CL1                 SAVED QOPT6                                  
SVQOPT7  DS    CL1                 SAVED QOPT7                                  
OPENSW   DS    X                   FIRST TIME SWITCH                            
*                                                                               
SORTREC  DS    XL255               SORT REC FROM BUY                            
*                                                                               
SRTKSVBL DS    XL(SRTBKYL1)        SORT KEY SAVE FOR BELL                       
*                                                                               
SRTKSVUB DS    XL(SRTUKYL1)        SORT KEY SAVE FOR UB                         
*                                                                               
SRTKSVOU DS    XL(SRTOKYL1)        SORT KEY SAVE FOR PHD                        
*                                                                               
SRTKSVGE DS    XL(SRTGKYL1)        SORT KEY SAVE FOR GENERIC EDI                
CANADIAN DS    CL1                 FLAG CANADIAN AGENCY                         
FIRSTINV DS    CL1                 FIRST TIME PROCESSING INVOICE FLAG           
ONLYINVC DS    CL1                 ONLY HAVE INVOICE INFO                       
REGCODE  DS    CL1                 Y/N                                          
*                                                                               
SVUCPKEY DS    CL9                 UCOMM AGY/MED, CLT, PRD                      
SVPUCOM1 DS    CL32                SAVED 1ST PRODUCT UCOMM                      
SVPUCOM2 DS    CL32                SAVED 2ND PRODUCT UCOMM                      
SVPUCOM3 DS    CL32                SAVED 3RD PRODUCT UCOMM                      
SVPUCOM4 DS    CL32                SAVED 4TH PRODUCT UCOMM                      
*                                                                               
SVUCEKEY DS    CL11                UCOMM AGY/MED, CLT, PRD, EST                 
SVEUCOM1 DS    CL32                SAVED 1ST ESTIMATE UCOMM                     
SVEUCOM2 DS    CL32                SAVED 2ND ESTIMATE UCOMM                     
SVEUCOM3 DS    CL32                SAVED 3RD ESTIMATE UCOMM                     
SVEUCOM4 DS    CL32                SAVED 4TH ESTIMATE UCOMM                     
SVEUCOM5 DS    CL32                SAVED 5TH ESTIMATE UCOMM                     
SVEUCOM6 DS    CL32                SAVED 6TH ESTIMATE UCOMM                     
SVEUCOM7 DS    CL32                SAVED 7TH ESTIMATE UCOMM                     
SVEUCOM8 DS    CL32                SAVED 8TH ESTIMATE UCOMM                     
*                                                                               
SVUCRKEY DS    CL17                UCOMM AGY/MED,CLT,PRD,EST,DIV,REG            
SVRUCOM1 DS    CL32                SAVED 1ST REGION UCOMM                       
SVRUCOM2 DS    CL32                SAVED 2ND REGION UCOMM                       
SVRUCOM3 DS    CL32                SAVED 3RD REGION UCOMM                       
SVRUCOM4 DS    CL32                SAVED 4TH REGION UCOMM                       
*                                                                               
SVUCDKEY DS    CL17                AGY/MED,CLT,PRD,EST,DIV,REG,DIS              
SVDUCOM1 DS    CL32                SAVED 1ST DISTRICT UCOMM                     
SVDUCOM2 DS    CL32                SAVED 2ND DISTRICT UCOMM                     
SVDUCOM3 DS    CL32                SAVED 3RD DISTRICT UCOMM                     
SVDUCOM4 DS    CL32                SAVED 4TH DISTRICT UCOMM                     
*                                                                               
CLOSETAG DS    C                   INDICATE IF WE JUST CLOSED XML TAG           
*                                                                               
PASS     DS    CL1                 PASS 1/PASS 2                                
*                                                                               
CTODAY   DS    CL8                 YYYYMMDD                                     
TIMEOFD  DS    CL8                 HH.MM.SS                                     
TIMEOFDU DS    CL8                 HH.MM.SS UTC                                 
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
REGTAB   DS    XL(5*REGMAX)        REGION TABLE                                 
REGTABX  EQU   *                   EOT                                          
REGMAX   EQU   20                  MAX 20 REGIONS PER CLT/DIV                   
*                                                                               
CURREG   DS    A                   A(CURRENT REGION)                            
DIVERR   DS    CL1                 DIVISION ERROR PRINTED FLAG                  
*                                                                               
PRINTSW  DS    CL1                 PRINT SWITCH                                 
SAVEAGY  DS    CL2                 SAVED AGENCY CODE                            
INDENT   DS    XL1                 INDENTATION FOR JSON DATA                    
JSONBUFF DS    CL100               JSON DATA BUFFER                             
*                                                                               
       ++INCLUDE PPBVALD           PPBVALD IN WORKING STORAGE                   
*                                                                               
AOUTREC  DS    A                   A(OUTREC)                                    
OUTRECLN EQU   80000               OUTREC LENGTH = 40K                          
*                                                                               
PDUMREC  DS    CL1000              COVER PPDUM00 DSECT IN PPNEWFILE             
*                                                                               
AGYTABD  DSECT                                                                  
AGYTAGY  DS    CL2                 AGENCY CODE                                  
AGYTAPE  DS    C                   TAPE CODE (C' ' = NONE)                      
AGYSUFX  DS    C                   SUFFIX IF TAPE CODE IS NOT BLANK             
AGYPSPEC DS    X                   RCSUBPRG                                     
AGYBLKS  DS    AL2                 DCB/TAPE - BLKSIZE=                          
AGYLRCL  DS    AL2                 DCB/TAPE - LRECL=                            
AGYRECFM DS    C                   DCB/TAPE - RECFM= 'F'=FB, 'V'=VB             
AGYSKEYL DS    AL2                 SORTER - SRTCDLEN                            
AGYSRECL DS    AL2                 SORTER - RECCDLEN                            
AGYBKEYL DS    F                   BINSRCH - KEY LENGTH                         
AGYBRECL DS    F                   BINSRCH - REC LENGTH                         
AGYBMAXR DS    F                   BINSRCH - MAX NUMBER OF RECORDS              
ABILLR   DS    A                   A(BILL ROUTINE)                              
ABUYR    DS    A                   A(BUY ROUTINE)                               
AOUTPR   DS    A                   A(OUTPUT ROUTINE)                            
AGYTABL  EQU   *-AGYTABD           LENGTH OF TABLE ENTRY                        
*                                                                               
ULBKEYD  DSECT                     BILLING SORT KEY DSECT                       
ULBKEY   DS    0XL(ULBKEYL)        KEY OF SORT RECORD                           
ULBMED   DS    CL1                 AGENCY/MEDIA                                 
ULBCLT   DS    CL3                 CLIENT CODE                                  
ULBPRD   DS    CL3                 PRODUCT CODE                                 
ULBEST   DS    XL2                 ESTIMATE CODE                                
ULBINVM  DS    XL1                 INVOICE MONTH                                
ULBINVN  DS    XL2                 INVOICE NUMBER                               
ULBKEYL  EQU   *-ULBMED            KEY LENGTH                                   
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
ULBRECL  EQU   *-ULBGROSS          SORT RECORD LENGTH                           
ULBRECL2 EQU   ULBRECL+ULBKEYL     SORT KEY+RECORD LENGTH                       
*                                                                               
SORTRECD DSECT                     SECONDARY SORT FROM BUY                      
SORTKEY  DS    0XL(SORTKEYL)       SECONDARY SORT KEY                           
SORTMED  DS    CL1                 MEDIA                                        
SORTCLT  DS    CL3                 3-CHARACTER CLIENT CODE                      
SORTPRD  DS    CL3                 3-CHARACTER PRODUCT CODE                     
SORTEST  DS    XL2                 ESTIMATE                                     
SORTINV  DS    XL2                 INVOICE NUMBER                               
SORTBDAT DS    XL3                 BILLED DATE                                  
SORTPUB  DS    CL17                PUB NUMBER                                   
SORTKEYL EQU   *-SORTMED           KEY LENGTH                                   
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
*                                                                               
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
*                                                                               
SORTMEDN DS    CL10                MEDIA NAME                                   
         DS    CL1                 BLANK                                        
         ORG   SORTMEDN                                                         
SORTMDNI DS    CL11                MEDIA NAME FOR INTERACTIVE                   
         ORG                                                                    
SORTCNAM DS    CL20                CLIENT NAME                                  
SORTMOS  DS    CL6                 MOS (MMM/YY)                                 
SORTPUBN DS    CL20                PUB/VENDOR NAME                              
SORTBNET DS    XL4                 VENDOR NET AMOUNT                            
SORTRLEN EQU   *-SORTMED                                                        
***                                                                             
* DSECTS FOR MEDIA EXPERTS CLIENT BELL (AGENCY U# M2TOA) SPEC-14682             
***                                                                             
BLBKEYD  DSECT                     BILLING SORT KEY DSECT                       
BLBKEY   DS    0XL(BLBKEYL)        KEY OF SORT RECORD                           
BLBMED   DS    CL1                 MEDIA                                        
BLBCLT   DS    CL3                 CLIENT CODE                                  
BLBPRD   DS    CL3                 PRODUCT CODE                                 
BLBEST   DS    XL2                 ESTIMATE CODE                                
BLBINVM  DS    XL1                 INVOICE MONTH                                
BLBINVN  DS    XL2                 INVOICE NUMBER                               
BLBKEYL  EQU   *-BLBMED            KEY LENGTH                                   
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
BLBDIVC  DS    CL3                 DIVISION CODE                                
BLBDIVN  DS    CL20                DIVISION CODE NAME                           
BLBREGN  DS    CL3                 REGION CODE FROM BILL (MAY BE NULL)          
BLBENAME DS    CL20                ESTIMATE NAME                                
BLBRECL  EQU   *-BLBNET            SORT RECORD LENGTH                           
BLBRECL2 EQU   BLBRECL+BLBKEYL     SORT KEY+RECORD LENGTH                       
*                                                                               
SRTBRECD DSECT                     SECONDARY SORT FROM BUY                      
SRTBKEY  DS    0XL(SRTBKEYL)       SECONDARY SORT KEY                           
SRTBMED  DS    CL1                 MEDIA                                        
SRTBCLT  DS    CL3                 3-CHARACTER CLIENT CODE                      
SRTBPRD  DS    CL3                 3-CHARACTER PRODUCT CODE                     
SRTBEST  DS    XL2                 ESTIMATE                                     
SRTBINV  DS    XL2                 INVOICE NUMBER                               
SRTBBDAT DS    XL3                 BILLED DATE                                  
SRTBKYL1 EQU   *-SRTBMED           KEY LENGTH 1 FOR REPORTING                   
SRTBPUB  DS    CL17                PUB NUMBER                                   
SRTBKEYL EQU   *-SRTBMED           KEY LENGTH                                   
*                                                                               
*  NEXT SEVERAL FIELDS FROM BINSRCH INVOICE DATA                                
*                                                                               
SRTBINET DS    PL6                 GROSS AMOUNT DUE                             
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
SRTBDIVC DS    CL3                 DIVISION CODE                                
SRTBDIVN DS    CL20                DIVISION CODE NAME                           
SRTBENAM DS    CL20                ESTIMATE NAME                                
*                                                                               
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
*                                                                               
SRTBMEDN DS    CL10                MEDIA NAME                                   
         DS    CL1                 BLANK                                        
         ORG   SRTBMEDN                                                         
SRTBMDNI DS    CL11                MEDIA NAME FOR INTERACTIVE                   
         ORG                                                                    
SRTBMOSP DS    XL3                 MOS YMD (BINARY)                             
SRTBMOS  DS    CL6                 MOS (MMM/YY)                                 
SRTBREGN DS    CL20                REGION NAME                                  
SRTBPUBN DS    CL20                PUB/VENDOR NAME                              
SRTBBNET DS    XL4                 VENDOR NET AMOUNT                            
SRTBRLEN EQU   *-SRTBMED                                                        
***                                                                             
* DSECTS FOR AGENCY UB - SPEC-23158                                             
***                                                                             
UBBKEYD  DSECT                     BILLING SORT KEY DSECT                       
UBBKEY   DS    0XL(UBBKEYL)        KEY OF SORT RECORD                           
UBBMED   DS    CL1                 MEDIA                                        
UBBCLT   DS    CL3                 CLIENT CODE                                  
UBBPRD   DS    CL3                 PRODUCT CODE                                 
UBBEST   DS    XL2                 ESTIMATE CODE                                
UBBINVM  DS    XL1                 INVOICE MONTH                                
UBBINVN  DS    XL2                 INVOICE NUMBER                               
UBBKEYL  EQU   *-UBBMED            KEY LENGTH                                   
*                                                                               
UBBRECD  DSECT                     BILL SORT RECORD DSECT                       
UBBREC   DS    0XL(UBBRECL)        BILL RECORD DATA                             
UBBNET   DS    PL6                 NET AMOUNT DUE                               
UBBINV   DS    CL10                FULL INVOICE NUMBER AS ON BILL               
UBBINVD  DS    CL10                INVOICE DATE AS ON BILL                      
UBBRECL  EQU   *-UBBNET            SORT RECORD LENGTH                           
UBBRECL2 EQU   UBBRECL+UBBKEYL     SORT KEY+RECORD LENGTH                       
*                                                                               
SRTURECD DSECT                     SECONDARY SORT FROM BUY                      
SRTUKEY  DS    0XL(SRTUKEYL)       SECONDARY SORT KEY                           
SRTUAGY  DS    CL2                 AGENCY                                       
SRTUMED  DS    CL1                 MEDIA                                        
SRTUCLT  DS    CL3                 3-CHARACTER CLIENT CODE                      
SRTUPRD  DS    CL3                 3-CHARACTER PRODUCT CODE                     
SRTUEST  DS    XL2                 ESTIMATE                                     
SRTUINV  DS    XL2                 INVOICE NUMBER                               
SRTUBDAT DS    XL3                 BILLED DATE                                  
SRTUKYL1 EQU   *-SRTUAGY           KEY LENGTH 1 FOR REPORTING                   
SRTUPUB  DS    CL17                PUB NUMBER                                   
SRTUKEYL EQU   *-SRTUAGY           KEY LENGTH                                   
*                                                                               
*  NEXT SEVERAL FIELDS FROM BINSRCH INVOICE DATA                                
*                                                                               
SRTUINET DS    PL6                 NET AMOUNT DUE                               
SRTUIINV DS    CL10                FULL INVOICE NUMBER AS ON BILL               
SRTUIDAT DS    CL10                INVOICE DATE AS ON BILL                      
*                                                                               
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
*                                                                               
SRTUMOSP DS    XL3                 MOS YMD (BINARY)                             
SRTUMOS  DS    CL6                 MOS (MMM/YY)                                 
SRTUPID  DS    CL20                PLACEMENT ID                                 
SRTUBNET DS    XL4                 VENDOR NET AMOUNT                            
SRTURLEN EQU   *-SRTUAGY                                                        
***                                                                             
* DSECTS FOR PHDTO (AGENCY OU) SPEC-24094                                       
***                                                                             
OUBKEYD  DSECT                     BILLING SORT KEY DSECT                       
OUBKEY   DS    0XL(OUBKEYL)        KEY OF SORT RECORD                           
OUBMED   DS    CL1                 AGENCY/MEDIA                                 
OUBCLT   DS    CL3                 CLIENT CODE                                  
OUBPRD   DS    CL3                 PRODUCT CODE                                 
OUBEST   DS    XL2                 ESTIMATE CODE                                
OUBINVM  DS    XL1                 INVOICE MONTH                                
OUBINVN  DS    XL2                 INVOICE NUMBER                               
OUBKEYL  EQU   *-OUBMED            KEY LENGTH                                   
*                                                                               
OUBRECD  DSECT                     BILL SORT RECORD DSECT                       
OUBREC   DS    0XL(OUBRECL)        BILL RECORD DATA                             
OUBNET   DS    PL6                 NET AMOUNT DUE                               
OUBGST   DS    PL6                 GST AMOUNT DUE                               
OUBPST   DS    PL6                 PST AMOUNT DUE                               
OUBCOM   DS    PL6                 AGENCY COMMISSION AMOUNT                     
OUBPROV  DS    CL1                 PROVINCE                                     
OUBINV   DS    CL10                FULL INVOICE NUMBER AS ON BILL               
OUBINVD  DS    CL10                INVOICE DATE AS ON BILL                      
OUEUDEF1 DS    CL32                ESTIMATE UDEF 1                              
OUEUDEF2 DS    CL16                ESTIMATE UDEF 2                              
OUBRECL  EQU   *-OUBNET            SORT RECORD LENGTH                           
OUBRECL2 EQU   OUBRECL+OUBKEYL     SORT KEY+RECORD LENGTH                       
*                                                                               
SRTORECD DSECT                     SECONDARY SORT FROM BUY                      
SRTOKEY  DS    0XL(SRTOKEYL)       SECONDARY SORT KEY                           
*SRTORCRD DS    0XL(SRTORLEN)       SECONDARY SORT RECORD LENGTH                
SRTOMED  DS    CL1                 MEDIA                                        
SRTOCLT  DS    CL3                 3-CHARACTER CLIENT CODE                      
SRTOPRD  DS    CL3                 3-CHARACTER PRODUCT CODE                     
SRTOEST  DS    XL2                 ESTIMATE                                     
SRTOINV  DS    XL2                 INVOICE NUMBER                               
SRTOBDAT DS    XL3                 BILLED DATE                                  
SRTOKYL1 EQU   *-SRTOMED           KEY LENGTH 1 FOR REPORTING                   
SRTOPUB  DS    CL17                PUB NUMBER                                   
SRTOKEYL EQU   *-SRTOMED           KEY LENGTH                                   
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
SRTOACOM DS    PL6                 AGENCY COMMISSION AMOUNT                     
*                                                                               
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
*                                                                               
SRTOMEDN DS    CL10                MEDIA NAME                                   
         DS    CL1                 BLANK                                        
         ORG   SRTOMEDN                                                         
SRTOMDNI DS    CL11                MEDIA NAME FOR INTERACTIVE                   
         ORG                                                                    
SRTOMOSP DS    XL3                 MOS YMD (BINARY)                             
SRTOMOS  DS    CL6                 MOS (MMM/YY)                                 
SRTOPUBN DS    CL20                STATION NAME                                 
SRTOBNET DS    XL4                 VENDOR NET AMOUNT                            
SRTORLEN EQU   *-SRTOMED                                                        
***                                                                             
* DSECTS FOR GENERIC EDI SPSUG-1667                                             
***                                                                             
GEBKEYD  DSECT                     BILLING SORT KEY DSECT                       
GEBKEY   DS    0XL(GEBKEYL)        KEY OF SORT RECORD                           
GEBMED   DS    CL1                 MEDIA                                        
GEBCLT   DS    CL3                 CLIENT CODE                                  
GEBPRD   DS    CL3                 PRODUCT CODE                                 
GEBEST   DS    XL2                 ESTIMATE CODE                                
GEBIMOS  DS    XL2                 MOS (PBILKMOS)                               
GEBINVM  DS    XL1                 INVOICE MONTH                                
GEBINVN  DS    XL2                 INVOICE NUMBER                               
GEBKEYL  EQU   *-GEBMED            KEY LENGTH                                   
*                                                                               
GEBRECD  DSECT                     BILL SORT RECORD DSECT                       
GEBREC   DS    0XL(GEBRECL)        BILL RECORD DATA                             
GEBGROSS DS    PL6                 GROSS                                        
GEBNET   DS    PL6                 NET                                          
GEBACT   DS    PL6                 ACTUAL                                       
GEBCSHD  DS    PL6                 CASH DISCOUNT                                
GEBTAX   DS    XL4                 TAX                                          
GEBGST   DS    XL4                 GST                                          
GEBPST   DS    XL4                 PST                                          
GEBHST   DS    XL4                 HST                                          
GEBPROV  DS    XL1                 PROVINCE                                     
GEBTYPE  DS    CL3                 BILL TYPE (B1,B2,B3,B4,B5,ETC)               
GEBSTAT  DS    XL1                 STATUS BYTE 1 (PBILCMSW)                     
GEBSTAT2 DS    XL1                 STATUS BYTE 2 (PBILSTAT)                     
GEBSTAT3 DS    XL1                 STATUS BYTE 3 (PBILLIND)                     
GEBSEP   DS    XL1                 SEPARATE BILL (PBILSEP)                      
GEBRETFL DS    XL1                 RETAIL FLAGS (PBRETAIL)                      
GEBRETAC DS    CL12                RETAIL ACCOUNT CODE (PBRACCT)                
GEBWBFLT DS    CL10                WB FLIGHT CODE (PBILWBF)                     
GEBINV   DS    CL10                FULL INVOICE NUMBER AS ON BILL               
GEBINVD  DS    CL10                INVOICE RUN DATE                             
GEBINVDD DS    CL10                INVOICE DUE DATE                             
GEBDIV   DS    CL3                 DIVISION                                     
GEBREG   DS    CL3                 REGION                                       
GEBDIS   DS    CL3                 DISTRICT                                     
GEBMEDN  DS    CL10                MEDIA NAME                                   
GEBCNAME DS    CL20                CLIENT NAME                                  
GEBIFLAG DS    XL1                 INTERNAL FLAG                                
GEBIFDUP EQU   X'80'               THIS IS A DUPLICATE                          
GEBIFVEN EQU   X'40'               THIS INVOICE HAS VENDOR DATA                 
GEBRECL  EQU   *-GEBGROSS          SORT RECORD LENGTH                           
GEBRECL2 EQU   GEBRECL+GEBKEYL     SORT KEY+RECORD LENGTH                       
*                                                                               
SRTGRECD DSECT                     SECONDARY SORT FROM STA BUCKET/UNIT          
SRTGKEY  DS    0XL(SRTGKEYL)       VENDOR KEY LENGTH                            
SRTGRCRD DS    0XL(SRTGRLEN)       VENDOR RECORD LENGTH (W/KEY)                 
SRTGMED  DS    CL1                 MEDIA                                        
SRTGCLT  DS    CL3                 3-CHARACTER CLIENT CODE                      
SRTGPRD  DS    CL3                 3-CHARACTER PRODUCT CODE                     
SRTGEST  DS    XL2                 ESTIMATE                                     
SRTGINV  DS    XL2                 INVOICE NUMBER                               
SRTGBDAT DS    XL3                 BILLED DATE                                  
SRTGMOS  DS    XL2                 MOS                                          
SRTGKYL1 EQU   *-SRTGMED           KEY LENGTH 1 (JUST BILL HEADER INFO)         
SRTGPUB  DS    XL17                PUB NUMBER                                   
SRTGKEYL EQU   *-SRTGMED           KEY LENGTH 2 (BILL HEADER + VENDOR)          
***                                                                             
*  NEXT SEVERAL FIELDS FROM BILL HEADER                                         
***                                                                             
SRTGBMOS DS    CL6                 MOS (MMM/YY)                                 
SRTGBGRS DS    PL6                 GROSS AMOUNT DUE                             
SRTGBNET DS    PL6                 NET AMOUNT DUE                               
SRTGBACT DS    PL6                 ACTUAL AMOUNT DUE                            
SRTGBCDC DS    PL6                 CASH DISCOUNT                                
SRTGBTAX DS    XL4                 TAX AMOUNT DUE (USA)                         
SRTGBGST DS    XL4                 GST AMOUNT DUE (CAN)                         
SRTGBPST DS    XL4                 PST AMOUNT DUE (CAN)                         
SRTGBHST DS    XL4                 HST AMOUNT DUE (CAN)                         
SRTGBPRV DS    XL1                 PROVINCE       (CAN)                         
SRTGBTYP DS    CL3                 BILL TYPE (B1,B2,B3,B4,B5,ETC)               
SRTGBINV DS    CL10                FULL INVOICE NUMBER AS ON BILL               
SRTGSTAT DS    XL1                 STATUS BYTE 1 (PBILCMSW)                     
SRTGSTA2 DS    XL1                 STATUS BYTE 2 (PBILSTAT)                     
SRTGSTA3 DS    XL1                 STATUS BYTE 3 (PBILLIND)                     
SRTGSEP  DS    XL1                 SEPARATE BILL (PBILSEP)                      
SRTGRTLF DS    XL1                 RETAIL FLAG (PBRETAIL)                       
SRTGRTLA DS    CL12                RETAIL ACCOUNT CODE (PBRACCT)                
SRTGWBFL DS    CL10                WB FLIGHT CODE (PBILWBF)                     
SRTGIDAT DS    CL10                INVOICE RUN DATE                             
SRTGIDDT DS    CL10                INVOICE DUE DATE                             
SRTGBDIV DS    CL3                 DIVISION                                     
SRTGBREG DS    CL3                 REGION                                       
SRTGBDIS DS    CL3                 DISTRICT                                     
SRTGMEDN DS    CL10                MEDIA NAME                                   
SRTGCNAM DS    CL20                CLIENT NAME                                  
SRTGPUBN DS    CL20                PUB/VENDOR NAME                              
SRTGPUBK DS    CL4                 PACKED PUB CODE (KEY)                        
SRTGZONK DS    CL1                 ZONE (KEY)                                   
SRTGEDTK DS    CL1                 EDITION (KEY)                                
SRTGZONE DS    CL2                 ZONE (EDITED)                                
SRTGEDIT DS    CL11                EDITION (EDITED)                             
***                                                                             
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
***                                                                             
SRTGVGRS DS    XL4                 PPBVEEG - VENDOR GROSS                       
SRTGVAGC DS    XL4                 PPBVEEA - VENDOR AGENCY COMMISSION           
SRTGVCDC DS    XL4                 PPBVEEC - VENDOR CASH DISCOUNT               
SRTGVNET DS    XL4                 PPBVEEN - VENDOR NET                         
SRTGCOS2 DS    CL1                 X'28' BILL ELEM = VENDOR COST2 = Y           
SRTGRLEN EQU   *-SRTGMED                                                        
*                                                                               
PHDSFLDD DSECT                     PHD SPECIAL FIELDS TABLE                     
PHDINVNO DS    CL10                HDR - INVOICE NUMBER WITH NO DASHES          
PHDPUPSE DS    CL10                HDR - "standard" OR "creditMemo"             
PHDEUDEF DS    CL16                HDR - ESTIMATE UDEF 2                        
PHDINVLN DS    CL3                 INV - INVOICE LINE NUMBER                    
PHDINVLB DS    XL2                 INV - INVOICE LINE NUMBER BINARY             
PHDINVQT DS    CL2                 INV - QUANTITY WILL BE 1 OR -1               
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
PHDPADD1 DS    CL20                PRODUCT RECORD ADDRESS LINE 1                
PHDPADD2 DS    CL20                PRODUCT RECORD ADDRESS LINE 2                
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
GENPRDCD DS    CL3                 PRODUCT CODE                                 
GENPRDNM DS    CL20                PRODUCT NAME                                 
GENPRUS1 DS    CL32                PRODUCT UDEF 1                               
GENPRUS2 DS    CL16                PRODUCT UDEF2                                
GENPRDR1 DS    CL20                Product Bill Receipt Name 1                  
GENPRDR2 DS    CL20                Product Bill Receipt Name 2                  
GENPRDA1 DS    CL30                PRODUCT ADDRESS 1                            
GENPRDA2 DS    CL30                PRODUCT ADDRESS 2                            
GENPRDIC DS    CL5                 PRODUCT INTERFACE CODE                       
GENPRDAT DS    CL24                PRODUCT ATTENTION                            
GENPRDAN DS    CL4                 PRODUCT ACCOUNT NUMBER                       
GENPRDU1 DS    CL32                PRODUCT UCOMM 1                              
GENPRDU2 DS    CL32                PRODUCT UCOMM 2                              
GENPRDU3 DS    CL32                PRODUCT UCOMM 3                              
GENPRDU4 DS    CL32                PRODUCT UCOMM 4                              
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
GENINDIV DS    CL3                 DIVISION CODE                                
GENINDNM DS    CL20                DIVISION NAME                                
GENINREG DS    CL3                 REGION CODE                                  
GENINRNM DS    CL20                REGION NAME                                  
GENRGUC1 DS    CL32                REGION UCOMM 1                               
GENRGUC2 DS    CL32                REGION UCOMM 2                               
GENRGUC3 DS    CL32                REGION UCOMM 3                               
GENRGUC4 DS    CL32                REGION UCOMM 4                               
GENREGPC DS    CL6                 REGION PERCENTAGE (MAX IS 100.00)            
GENINDIS DS    CL3                 DISTRICT CODE                                
GENDGUC1 DS    CL32                DISTRICT UCOMM 1                             
GENDGUC2 DS    CL32                DISTRICT UCOMM 2                             
GENDGUC3 DS    CL32                DISTRICT UCOMM 3                             
GENDGUC4 DS    CL32                DISTRICT UCOMM 4                             
GENSADJB DS    CL1                 PBILSEP  - SEP ADJUSTMENT BILL               
GENCOMAJ DS    CL1                 PBILCMSW - COMM ADJUSTMENT                   
         DS    CL1                 PBILCMSW - X'40' DUMMY FLAG                  
GENTAORB DS    CL1                 PBILCMSW - TRUE AOR BILL                     
GENCAORB DS    CL1                 PBILCMSW - CLIENT AOR BILL                   
GENNETBL DS    CL1                 PBILCMSW - NET BILL (IN SEP COMM )           
GENNTAOR DS    CL1                 PBILCMSW - NO TAX IN AOR CALC                
GENCOMBL DS    CL1                 PBILCMSW - COMM ONLY BILL                    
GENCOMSB DS    CL1                 PBILCMSW - SEPARATE COMMISSION BILL          
GENCOST2 DS    CL1                 PBILLIND X'80'/X'02' - COST2 BILL            
GENGMTDM DS    CL1                 PBILSTAT X'20' - GROUPM TRADE MIDAS          
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
GENINCDC DS    CL13                INVOICE CASH DISCOUNT                        
GENINTAX DS    CL13                INVOICE TAX (SPBVETAX FROM SPBVAL)           
GENINGST DS    CL13                INVOICE GST (SPBVGST FROM SPBVAL)            
GENINGSR DS    CL5                 GST RATE (5.000)                             
GENINPST DS    CL13                INVOICE PST (SPBVPST FROM SPBVAL)            
GENINPSR DS    CL5                 PST RATE (5.000)                             
GENINHST DS    CL13                INVOICE HST (SPBVHST FROM SPBVAL)            
GENINHSR DS    CL5                 HST RATE (5.000)                             
GENINTXT DS    CL3                 TAX TYPE (PST/HST/QST)                       
GENINPRV DS    CL20                PROVINCE (PROVTAB IN SPGETRATE)              
GENINTYP DS    CL3                 BILL TYPE                                    
GENINCNT DS    CL9                 TOTAL INVOICE COUNT                          
GENSFDHQ EQU   *-GENSFLDH          SPECIAL FIELDS BILL HDR TABLE LENGTH         
*                                                                               
GENSFLDV DSECT                     GENERIC EDI VENDOR SOFT FIELDS               
GENVPUBN DS    CL17                PUB                                          
GENVPBNM DS    CL20                PUB NAME                                     
GENVZONE DS    CL2                 PUB ZONE                                     
GENVEDIT DS    CL11                PUB EDITION                                  
GENVGRSD DS    CL13                VENDOR GROSS                                 
GENVNETD DS    CL13                VENDOR NET                                   
GENVAGCM DS    CL13                VENDOR AGENCY COMMISSION                     
GENVCDSC DS    CL13                VENDOR CASH DISCOUNT                         
GENVCOS2 DS    CL1                 COST2 FLAG                                   
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
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPGENBYCC                                                      
       ++INCLUDE PBLPSTEL                                                       
       ++INCLUDE DDUCOMD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDMASTD                                                        
*                                                                               
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'139PPREPVL02 01/28/21'                                      
         END                                                                    
