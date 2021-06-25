*          DATA SET PPREPF302  AT LEVEL 185 AS OF 09/10/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE PPF302A                                                                  
*INCLUDE GENIOS                                                                 
*INCLUDE BUCKPRT                                                                
         TITLE 'PPF302 - PRINT DAILY ACTIVITY'                                  
*                                                                               
*        CHANGE LOG                                                             
*         MM/YY                                                                 
*   AHYD  08/19    CHANGE TO SUPPORT NEW RSIN FORMAT                            
*                                                                               
*   BPLA  12/13    CHANGES FOR 2 CHARACTER SYSTEMS                              
*                  USES SYSNAID INSTEAD OF FILENUM                              
*                  PROCESSING MOVED TO RLAST (RUNLAST)                          
*                                                                               
*                                                                               
*   BOBY  11/09    ADD AUTONTES TO ERROR MESSAGES                               
*                                                                               
*   BPLA  08/08    CHANGE CHECK FOR OFF-LINE RECOVERY CHANGES                   
*                  DON'T PRODUCE P53'S WITHOUT A USER ID                        
*                                                                               
*   BOBY  11/06    ADD PLANNED COSTS TO BUCKETS AND REPORT THEM                 
*                                                                               
*   SMYE  10/05    NEW SEARS CLIENT ONLY RE-DIRECTS FOR MINDSHARE AND           
*                   REMOVED (*SMY*) THESE CLIENTS FROM H7PDTAB                  
*                                                                               
*   SMYE  01/03    MORE RE-DIRECTS FOR MINDSHARE - SEARS (H7PDTAB)              
*                                                                               
*   SMYE  10/02    ANOTHER PRD FOR SEARS REDIRECT M/SR1/SWP (H7PDTAB)           
*                                                                               
*   BOBY  10/02    CHANGE                                                       
*                                                                               
*   BPLA  09/02    ANOTHER PRODUCT FOR SEARS REDIRECT M/SR1/GCB                 
*                                                                               
*   BPLA  07/02    ANOTHER PRODUCT FOR SEARS REDIRECT                           
*                                                                               
*   BPLA  04/02    ADDITIONAL RE-DIRECTS FOR MINDSHARE - SEARS                  
*                                                                               
*   BPLA  03/06    REDIRECT P78 FOR CERTAIN MINDSHARE PRODUCTS                  
*                                                                               
*   BPLA  02/02    NO-OP REDIRECT OF FORD P72 AND P78 FOR CLIENT MJ             
*                                                                               
*   SMYE  10/01    INCLUDE MEL'S "FIX FOR NEW RECOVERY RECORDS" AT RL8          
*                                                                               
*   SMYE  08/01    NO 78S FOR "NO TRAFFIC" BUYS AT CHKTRAF                      
*                                                                               
*   BPLA   8/01    REDIRECT FORD CLIENT MJ P72'S AND P78'S                      
*                  TO FDMOMN   - ID # 6724 (AGY = FR)                           
*                                                                               
*   YKAP  4/01     CHANGE MADE FOR H7 ADD ID TABLE                              
*                                                                               
*   BPLA  4/99     REDIRECT P78'S FOR OMNY CLIENT                               
*                  BPC TO ORIGIN ID 1376  (OMNYT)                               
*                                                                               
*   BPLA  12/99    RENAME AS "LIVE" SOURCE  (GENOISR TO GENIOS)                 
*                  OLD IS PPREPF302S                                            
*                                                                               
*   BPLA  11/99    REMOVE P78 PROFILE CHECK - ONLY USE P72A                     
*                  TRIGGER                                                      
*                                                                               
*   BPLA  10/99    CHECK NEW P78 PROFILE                                        
*                  RATE CHANGE TRIGGER                                          
*                  ALSO NEW VERSION OF GENIOS THAT CHECKS                       
*                  NEW P72A PROFILE RATE CHANGE TRIGGER (GENIOSR)               
*                  COPY OF PPREF302 AT LEVEL 151                                
*                  MADE 10/13/99                                                
*                                                                               
*   BPLA  9/99     WHEN PRODUCING P53 REQUESTS, CHECK TO SEE IF                 
*                  I HAVE TO CREATE A RF REQUEST AS WELL.                       
*                  RFP GRPUP CODE IS CARRIED IN X'46' ELEMENT                   
*                  ON THE CLIENT HEADER.                                        
*                                                                               
*   BPLA  4/99     REDIRECT P78'S FOR OMNY CLIENT                               
*                  SH4 TO ORIGIN ID 7811                                        
*                                                                               
*   BPLA  3/99     FIX TRAFREQ, BYREQ AND TEARREQ TO SET                        
*                  RQAGY(6) BEFORE GOING TO READ REPORT PROFILES                
*                                                                               
*   BPLA 10/98     OPTION ON P13A PROFILE TO SUPPRESS CONTRACT                  
*                  TURNAROUND                                                   
*                                                                               
*   SMYE 6/22/98      CHECK 78 PROFILE AT TRAFREQ TO SEE IF AUTOMATIC           
*                      TRAFFIC TURNAROUND SHOULD BE GENERATED.                  
*                                                                               
*   BPLA 3/97         CHANGE ORIGIN ID FOR WESTERN - CLT                        
*                     GROUP M01 TO MNAT (ID=6198)                               
*                                                                               
*   SMYE 1/97         CHECK CLIENT GROUP FOR POSSIBLE ALTERATION OF             
*                     ORIGIN ID FOR P78'S FOR WI AGENCY (AT WRTREQ)             
*                                                                               
*   BPLA 9/96         REMOVE BLKSIZE PARAMETER FROM RECOVERY DTF                
*                                                                               
*   BPLA 6/96          USE STORED ACTIVITY DATE WHEN GENERATING                 
*                      P78'S ALSO                                               
*                                                                               
*   BPLA 6/96          STORE THE ACTIVITY DATE IN THE SORT RECORD               
*                      AND WHEN GENERATING P53'S USE EARLIEST DATE              
*                      FOUND AND STORE IT IN QCNTLDT (COL 58)                   
*                                                                               
*   BPLA 12/95         IN CHKEST, CHKTRAF, RL12 AND RL13 USE RDATE              
*                      (DATE FROM RECOVERY HEADER) INSTEAD OF BTODAY.           
*                      THIS IS REQUIRED SINCE WE NOW WILL HAVE                  
*                      TWO DAY'S RECOVERY RECORDS ON THE SAME TAPE              
*                      FOR SOME HOLIDAYS.                                       
*                      -NOTE THAT WRTREQ STILL USES TODAY AS DO                 
*                       THE "TODAY" BUCKETS AND THE AGENCY HEADER               
*                                                                               
*   BPLA 12/22/94      FIX IN SETVAL - CLEAR CPSTTAX(8)                         
*                                                                               
*                                                                               
*   BPLA 11/94         CHANGES FOR TEARSHEET TURNAROUNDS                        
*                                                                               
*   BPLA 9/94          READ 53 PROFILE TO SEE IF REQUEST                        
*                      SHOULD BE GENERATED FOR TEST BUYS                        
*                                                                               
*   BPLA 8/94          NOW LONGER NEED TO USE GETINSA - NOW                     
*                      I CAN USE GETINS                                         
*                                                                               
*   BPLA 7/94          CHANGES FOR FAXING 13'S AND 15'S                         
*                                                                               
*   BPLA 4/27/94       PST CHANGES                                              
*                                                                               
*   BPLA 2/8/93        READ 78 PROFILE TO SEE IF REQUEST SHOULD                 
*                      BE GERNERATED FOR TEST BUYS.                             
*                                                                               
*   BPLA 11/19/91      CHANGES FOR 13,15,72 TURNAROUNDS TO GET OUTPUT           
*                      TYPE FROM PROFILE                                        
*                                                                               
*   BPLA 12/20/90      CHANGES TO UPDATE NEW PBKREC "TODAY"                     
*                      ELEMENT (X'31') THAT INCLUDE GST                         
*                      COPIED FROM SPREPF302 AT LEVEL=069                       
*                                                                               
PPF302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPF302,R9,R7                                                   
*                           ** NOTE USE OF R7 AND R9 AS BASE REGS **            
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PPF3WK,R8                                                        
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    PREQ                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RFRST                                                            
         CLI   MODE,RUNLAST                                                     
         BE    RLAST                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
BKTLST   DC    32X'00'                                                          
                                                                                
*********************************************************************           
*        FIRST TIME                                                             
*********************************************************************           
RFRST    RELOC (R3)                RELOCATE ACONS                               
         LA    R1,ACONS                                                         
         LA    R0,(ACONSX-ACONS)/4                                              
         LA    R2,AAGYLST                                                       
RF2      DS    0H                                                               
         L     RF,0(R1)                                                         
         AR    RF,R3                                                            
         ST    RF,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,RF2                                                           
*                             SET BINSRCH PARS FOR AGYLST                       
         SR    R0,R0                                                            
         L     R1,AAGYLST                                                       
         SR    R2,R2                                                            
         LA    R3,AGYTBEL                                                       
         LA    R4,3                                                             
         LA    R5,300                                                           
         STM   R0,R5,BSPARS                                                     
         MVC   WORK+0(2),RCDATE+6                                               
         MVC   WORK+2(2),RCDATE+0                                               
         MVC   WORK+4(2),RCDATE+3                                               
         MVC   TODAY,WORK                                                       
         GOTO1 DATCON,DMCB,WORK,(3,BTODAY)                                      
         GOTO1 (RF),(R1),,(1,PTODAY)                                            
*                                                                               
         STM   R7,RC,SVREGS        SAVE REGS FOR RPRT                           
         ZAP   RCADD,=P'0'                                                      
         ZAP   RCPUT,=P'0'                                                      
         MVI   DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,X'FD'                                                   
         XC    LAST13,LAST13                                                    
         XC    LAST15,LAST15                                                    
         XC    LAST72,LAST72                                                    
         XC    LAST78,LAST78                                                    
         XC    LASTTT,LASTTT                                                    
         XC    LAST53,LAST53                                                    
         XC    LAST13O,LAST13O                                                  
         XC    LAST15O,LAST15O                                                  
         XC    LAST72O,LAST72O                                                  
                                                                                
**********************************************************************          
*        PROC REQ - SAVE AGYHDR                                                 
**********************************************************************          
PREQ     LA    R3,X                                                             
         USING AGYTBD,R3                                                        
         MVC   AGYMED,PAGYKAGY                                                  
         MVC   AGYPROF,PAGYPROF                                                 
         MVC   AGYTNAM,PAGYNAME                                                 
         LA    R1,AGTTOTS                                                       
         LA    R0,ACCN                                                          
         BRAS  RE,CLRTOTS                                                       
         MVC   AGYSTAT,PROGPROF    SET ACTIVITY CTK                             
*                                  CHECK BYPASS LIST                            
         LA    R1,QPROG+11                                                      
         LA    R0,5                                                             
PR4      CLI   0(R1),C' '          EOL                                          
         BNH   PR8                                                              
         CLC   PAGYKAGY(3),0(R1)                                                
         BE    PR6                                                              
         LA    R1,3(R1)                                                         
         BCT   R0,PR4                                                           
*                                                                               
PR6      MVI   AGYSTAT,C'B'        BYPASS                                       
*                                                                               
PR8      GOTO1 BINSRCH,BSPARS,(1,X)                                             
         MVC   SVOPTS,QOPT1                                                     
         B     EXIT                                                             
         DROP  R3                                                               
                                                                                
**********************************************************************          
*        RUN LAST                                                               
*         - RECOVERY FILE PROCESSING DONE HERE                                  
**********************************************************************          
RLAST    CLI   FRST,0              ONLY TO PROCESSING ONCE                      
         BNE   EXIT                                                             
         MVI   FRST,1                                                           
         MVI   RCSUBPRG,10                                                      
         CLI   BKDOPT,C'D'         DUMP BUCKETS                                 
         BNE   RL2                                                              
         GOTO1 ABKTDMP                                                          
         B     RL3                                                              
*                                                                               
RL2      CLI   BKDOPT,C'R'         RESTORE BUCKETS                              
         BNE   RL3                                                              
         GOTO1 ABKTRSTR                                                         
*                                                                               
RL3      GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(45,SORTLOC)                        
*                                                                               
RL4      OPEN  (RECVIN,(INPUT))                                                 
         B     GETRCV2                                                          
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
                                                                                
*********************************************************************           
* GET RECOVERY RECORDS                                                          
*********************************************************************           
GETRCV   L     R3,ARECVR                                                        
         USING RECD,R3                                                          
         MVC   LASTTYP,RRECTY     SAVE LAST RECORD TYPE                         
***TEST***                                                                      
         MVI   LASTBFD,0                                                        
         LA    R6,RECVR                                                         
BUY      USING PBUYREC,R6                                                       
         CLI   RECVR+3,X'20'      SEE IF BUYREC                                 
         BNE   GETRCV2                                                          
         MVC   LASTBFD,BUY.PBDBFD SAVE TEST STATUS                              
         DROP  BUY                                                              
***TEST***                                                                      
*                                                                               
GETRCV2  LA    R1,RECVIN                                                        
         L     R0,ARECVR                                                        
         GET   (1),(0)                                                          
*                                                                               
         L     R3,ARECVR                                                        
         CLI   RSIN,RBACKOUT       UNWIND TRANSACTION                           
         BE    GETRCV2             YES SO SKIP                                  
**NEW 1/30/90       WAS X'12'                                                   
         CLI   RFILTY,X'42'        TEST PRTFILE                                 
         BNE   GETRCV                                                           
         CLI   RPRG,X'01'          SKIP PFM                                     
         BE    GETRCV                                                           
         CLI   RPRG,X'13'          SKIP REMOTE IO'S                             
         BE    GETRCV                                                           
*                                                                               
         CLI   RECVR+3,X'10'       CONTRACT - OK                                
         BE    RL8                                                              
         CLI   RECVR+3,X'20'       BUY - OK                                     
         BNE   GETRCV                                                           
                                                                                
**********************************************************************          
* FIX FOR NEW RECOVERY RECORDS                                                  
**********************************************************************          
RL8      LA    RE,RECVR            POINT TO RECORD                              
         SR    R0,R0                                                            
         ICM   R0,3,25(RE)         GET RECORD LENGTH                            
         AR    RE,R0                                                            
         XC    0(2,RE),0(RE)       CLEAR 2 BYTES                                
*                                                                               
         LA    RF,RECLN                                                         
         AH    RF,RECLN                                                         
         XC    0(2,RF),0(RF)                                                    
         CLI   RRECTY,RRECTCPY     COPY                                         
         BE    RL10                                                             
         CLI   RRECTY,RRECTCHG     CHG                                          
         BE    RL12                                                             
         CLI   RRECTY,RRECTADD     ADD                                          
         BE    RL14                                                             
         MVC   P(23),=C'**INVALID RECORD TYPE**'                                
         MVC   X(56),P             SAVE ERROR MESSAGE                           
*                                                                               
         BRAS  RE,RPRT                                                          
         L     R5,ARECVR                                                        
         BRAS  RE,DMPREC                                                        
         MVC   LASTTYP,RRECTY                                                   
         MVI   LASTBFD,0           SAVE LAST TEST STATUS                        
*                                                                               
         TM    MSGFLAG,MSGINVQ     SKIP IF EMAIL SENT ALREADY                   
         BO    GETRCV                                                           
         OI    MSGFLAG,MSGINVQ     SET MESSAGE SENT FLAG                        
         MVC   P(24),=C'AUTONOTE*KWAN,DEIS,SKUI:' SET RECIPIENT                 
         MVC   P+24(56),X          RETRIEVE ERROR MESSAGE                       
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(72,P) SEND EMAIL                         
         MVC   P,SPACES            CLEAR PRINT LINE                             
         B     GETRCV                                                           
                                                                                
*********************************************************************           
*        COPY  RECOVERY RECORD                                                  
*********************************************************************           
RL10     CLI   LASTTYP,RRECTCPY    WAS LAST A COPY                              
         BE    RL11                YES - ERROR                                  
*                                                                               
RL10B    LH    R1,RECLN                                                         
         LA    R1,2(R1)            2 EXTRA BYTES                                
         L     RF,ASVREC                                                        
         MOVE  ((RF),(R1)),RECD    SAVE COPY                                    
         B     GETRCV                                                           
*                                                                               
* REPORT ERROR, TWO COPIES IN A ROW                                             
*                                                                               
RL11     TM    MSGFLAG,MSGCPYQ     SKIP IF EMAIL SENT ALREADY                   
         BO    RL11A                                                            
         OI    MSGFLAG,MSGCPYQ     SET MESSAGE SENT FLAG                        
         MVC   P(24),=C'AUTONOTE*KWAN,DEIS,SKUI:' SET RECIPIENT                 
         MVC   P+24(20),=C'**STAND ALONE COPY**' SET MSG                        
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(72,P) SEND EMAIL                         
         MVC   P,SPACES            CLEAR PRINT LINE                             
*                                                                               
RL11A    MVC   P(20),=C'**STAND ALONE COPY**'                                   
         BRAS  RE,RPRT                                                          
         L     R5,ASVREC                                                        
         BRAS  RE,DMPREC                                                        
         CLI   RRECTY,RRECTCHG                                                  
         BE    RL10B               RETURN TO COPY CODE                          
         B     RL12A               OR ADD CODE                                  
                                                                                
*********************************************************************           
*        CHANGE RECOVERY RECORD                                                 
*********************************************************************           
RL12     CLI   LASTTYP,RRECTCPY    WAS LAST RECOVERY RECORD A COPY?             
         BNE   RL13F               NO, IF OFF-LINE SKIP, ONLINE ERROR           
*                                                                               
RL12A    LA    R4,X                                                             
         USING AGYTBD,R4                                                        
         MVC   AGYMED,RECVR                                                     
         GOTO1 BINSRCH,BSPARS,AGYTBD                                            
         CLI   0(R1),1                                                          
         BE    GETRCV                                                           
         L     R4,0(R1)                                                         
         MVC   PAGYPROF,AGYPROF                                                 
         CLI   AGYSTAT,C'B'        BYPASS THIS AGENCY                           
         BE    GETRCV                                                           
         DROP  R4                                                               
                                                                                
**********************************************************************          
*        BUY RECORD                                                             
**********************************************************************          
         CLI   RECVR+3,X'10'       CONTRACT RECORD                              
         BE    RL13                YES                                          
                                                                                
         XC    THISPRD,THISPRD     SET FIRST TIME SWITCH                        
RL12C    MVI   PAYCHSW,0                                                        
         BRAS  RE,SETVAL           GET $'S ETC FROM BUYREC                      
         CLI   THISPRD,X'FF'       DONE IF LAST PRD                             
         BE    GETRCV                                                           
*                                                                               
*        BUILD SORT RECORD                                                      
*                                                                               
         LA    R5,X                                                             
         USING SRKEYD,R5           ESTABLISH SORT RECORD                        
         XC    X,X                                                              
*                                                                               
         LA    R6,RECVR            ESTABLISH CURRENT BUY RECOVERY REC           
BUY      USING PBUYREC,R6                                                       
         MVI   SRTYPE,C'A'         ANALYSIS RECORD                              
         MVC   SRAGY(3),BUY.PBUYKAGY AGENCY                                     
         MVC   SRCLT,BUY.PBUYKCLT  CLIENT                                       
         MVC   SRPRD,THISPRD       PRODUCT                                      
         MVC   SREST,BUY.PBUYKEST  ESTIMATE                                     
         MVC   SRPUB,BUY.PBUYKPUB  PUB                                          
         MVC   SRDATE,BUY.PBUYKDAT INSERTION DATE                               
         MVC   SRLIN,BUY.PBUYKLIN  BUY LINE                                     
         MVC   SRPCSW,PCSW         SET PLANNED COSTS INDICATOR                  
*                                                                               
         L     RF,SEQNO            INTERNAL SEQUENCE NUMBER                     
         LA    RF,1(RF)                                                         
         ST    RF,SEQNO                                                         
         MVC   SRSEQ,SEQNO+1                                                    
*                                                                               
         MVC   SRRDATE,RDATE       ACTIVITY DATE                                
         MVC   SRSIN,RSIN+1        INPUT SEQUENCE NUMBER                        
         MVC   SRUSER,RUSER        USER ID                                      
         MVC   SRPRG,RPRG          PROGRAM NUMBER                               
*                                                                               
         CLI   SRPRG,0             IF OFF-LINE                                  
         BNZ   *+8                                                              
         MVI   SRPRG,X'11'         PRETEND ITS BUY PROGRAM                      
         MVC   SRBUYER,BUY.PBDBUYER BUYER                                       
         MVC   SRBFD,BUY.PBDBFD    NEED TO SAVE TEST STATUS                     
         MVC   SRBLMO,BUY.PBDBDATE BILLABLE DATE                                
         MVC   SRINMO,BUY.PBUYKDAT INSERTION MONTH                              
*                                                                               
         MVC   SRCPBM,SVBLMO       BILL MONTH                                   
         MVC   SRCPIM,SVINMO       INSERTION MONTH                              
                                                                                
**********************************************************************          
*        PAY PROGRAM - PASS PAID DOLLARS TO SORT                                
*           AMOUNTS ARE DIFFERENCE BETWEEN NEW BUYREC AND                       
*           COPY OF OLD BUYREC                                                  
**********************************************************************          
         CLI   RPRG,X'03'          SKIP IF NOT FROM PAY PROGRAM                 
         BNE   RL12D4                                                           
*                                                                               
         LA    RF,PGROSS           PAID AMOUNTS                                 
         LA    RE,CPYVALS+28 PAID AMOUNTS FROM COPY RECORD                      
*                                                                               
         L     R1,0(RF)            GROSS                                        
         S     R1,0(RE)                                                         
         CVD   R1,DUB                                                           
         ZAP   SRGRS,DUB                                                        
*                                                                               
         A     R1,4(RE)            NET                                          
         S     R1,4(RF)                                                         
         CVD   R1,DUB                                                           
         ZAP   SRNET,DUB                                                        
*                                                                               
         L     R1,8(RF)            CD                                           
         S     R1,8(RE)                                                         
         CVD   R1,DUB                                                           
         ZAP   SRCD,DUB                                                         
*                                                                               
         L     R1,MGSTTAXP         GST                                          
         S     R1,CGSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRGST,DUB                                                        
*                                                                               
         L     R1,MGSTTAXP         PAID GST TAX                                 
         S     R1,CGSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRGSTP,DUB                                                       
*                                                                               
         L     R1,MPSTTAXP         PST                                          
         S     R1,CPSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRPST,DUB                                                        
*                                                                               
         L     R1,MPSTTAXP         PAID PST TAX                                 
         S     R1,CPSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRPSTP,DUB                                                       
*                                                                               
         ZAP   SRPCGRS,=P'0'                                                    
         ZAP   SRPCNET,=P'0'                                                    
         ZAP   SRPCCD,=P'0'                                                     
*                                                                               
*        CLEAR OUT COPY VALUES                                                  
*                                                                               
         ZAP   SRCPGRS,=P'0'                                                    
         ZAP   SRCPNET,=P'0'                                                    
         ZAP   SRCPCD,=P'0'                                                     
         ZAP   SRCPGST,=P'0'                                                    
         ZAP   SRCPGSTP,=P'0'                                                   
         ZAP   SRCPPST,=P'0'                                                    
         ZAP   SRCPPSTP,=P'0'                                                   
         ZAP   SRCPCGRS,=P'0'                                                   
         ZAP   SRCPCNET,=P'0'                                                   
         ZAP   SRCPCCD,=P'0'                                                    
         B     RL12E                                                            
                                                                                
********************************************************************            
*        BUY PROG ACTIVITY - PASS ORDERED AND PLANNED COSTS                     
********************************************************************            
RL12D4   L     R1,GROSS                                                         
         CVD   R1,DUB                                                           
         ZAP   SRGRS,DUB                                                        
*                                                                               
         S     R1,GROSS+4          LESS AGYCOM                                  
         CVD   R1,DUB                                                           
         ZAP   SRNET,DUB                                                        
*                                                                               
         L     R1,GROSS+8          CD                                           
         CVD   R1,DUB                                                           
         ZAP   SRCD,DUB                                                         
*                                                                               
         L     R1,MGSTTAX          GST                                          
         CVD   R1,DUB                                                           
         ZAP   SRGST,DUB                                                        
*                                                                               
         L     R1,MGSTTAXP         GST PAID                                     
         CVD   R1,DUB                                                           
         ZAP   SRGSTP,DUB                                                       
*                                                                               
         L     R1,MPSTTAX          PST                                          
         CVD   R1,DUB                                                           
         ZAP   SRPST,DUB                                                        
*                                                                               
         L     R1,MPSTTAXP         PST PAID                                     
         CVD   R1,DUB                                                           
         ZAP   SRPSTP,DUB                                                       
*                                                                               
         L     R1,CPYVALS          OLD GROSS                                    
         CVD   R1,DUB                                                           
         ZAP   SRCPGRS,DUB                                                      
*                                                                               
         S     R1,CPYVALS+4        LESS AGYCOM                                  
         CVD   R1,DUB                                                           
         ZAP   SRCPNET,DUB                                                      
*                                                                               
         L     R1,CPYVALS+8        CD                                           
         CVD   R1,DUB                                                           
         ZAP   SRCPCD,DUB                                                       
*                                                                               
         L     R1,CGSTTAX         COPY GST                                      
         CVD   R1,DUB                                                           
         ZAP   SRCPGST,DUB                                                      
*                                                                               
         L     R1,CGSTTAXP        COPY GST PAID                                 
         CVD   R1,DUB                                                           
         ZAP   SRCPGSTP,DUB                                                     
*                                                                               
         L     R1,CPSTTAX         COPY PST                                      
         CVD   R1,DUB                                                           
         ZAP   SRCPPST,DUB                                                      
*                                                                               
         L     R1,CPSTTAXP        COPY PST PAID                                 
         CVD   R1,DUB                                                           
         ZAP   SRCPPSTP,DUB                                                     
*                                                                               
         LA    RF,PCVALS           POINT TO PLANNED COST BUCKETS                
*                                                                               
         L     R1,GROSS-GROSS(RF)  PLANNED COST GROSS                           
         CVD   R1,DUB                                                           
         ZAP   SRPCGRS,DUB                                                      
*                                                                               
         S     R1,GROSS+4-GROSS(RF)  LESS AGYCOM                                
         CVD   R1,DUB                                                           
         ZAP   SRPCNET,DUB                                                      
*                                                                               
         L     R1,GROSS+8-GROSS(RF)  CD                                         
         CVD   R1,DUB                                                           
         ZAP   SRPCCD,DUB                                                       
*                                                                               
         LA    RF,CPPCVALS         POINT TO COPY PLANNED COSTS                  
*                                                                               
         L     R1,GROSS-GROSS(RF)  PLANNED COST GROSS                           
         CVD   R1,DUB                                                           
         ZAP   SRCPCGRS,DUB                                                     
*                                                                               
         S     R1,GROSS+4-GROSS(RF)  LESS AGYCOM                                
         CVD   R1,DUB                                                           
         ZAP   SRCPCNET,DUB                                                     
*                                                                               
         L     R1,GROSS+8-GROSS(RF)  CD                                         
         CVD   R1,DUB                                                           
         ZAP   SRCPCCD,DUB                                                      
*                                                                               
         CLI   RPRG,X'03'          IF PAY IGNORE HERE UNLESS                    
         BE    RL12D8              'ORDERED' CHANGE (CD ONLY)                   
*                                                                               
         CLI   RSIN,ROCHGOLY       CHANGES ONLY                 NEW             
         BE    RL12E               YES, NO OFF-LINE COPIES      NEW             
         OC    RSIN+1(3),RSIN+1    CHANGES ONLY                 OLD             
         BNZ   RL12E               YES, NO OFF-LINE COPIES      OLD             
*                                                                               
RL12D8   CLC   SRAMTS(SRAMTSL),SRCPYS   IF SAME DOLLAR AMOUNTS                  
         BNE   RL12E                                                            
         CLC   SRBLMO,SRCPBM            AND SAME BILL MONTH                     
         BNE   RL12E                                                            
         CLC   SRINMO,SRCPIM            AND SAME INSERTION MONTH                
         BE    RL12C                 SKIP IF NO REAL ACTIVITY                   
*                                  SET ACTION CODE                              
RL12E    MVI   SRACT,C'A'          ADD                                          
         CLI   RRECTY,RRECTADD     ADDING BUY                                   
         BE    RL12F                                                            
*                                                                               
         MVI   SRACT,C'P'                                                       
         CLI   RPRG,X'03'          PAYING BUY                                   
         BE    RL12R                                                            
*                                                                               
         MVI   SRACT,C'C'          ELSE CHANGING BUY                            
         TM    BUY.PBUYCNTL,X'80'  DELETING BUY                                 
         BZ    RL12F                                                            
         MVI   SRACT,C'D'                                                       
*                                                                               
RL12F    CLC   THISPRD,PRDLST      NO TA'S   UNLESS FIRST PRODUCT               
         BNE   RL12P                                                            
*                                                                               
         CLC   BUY.PBUYKPRD,=C'ZZZ'                                             
         BNE   *+8                                                              
         OI    SRRQS,X'20'         SET ZZZ TA'S                                 
*                                  SEE IF WILL NEED IO.                         
         CLI   PAGYPROF+22,C'N'    NO IO'S                                      
         BE    RL12M                                                            
*                                                                               
         MVC   WORK+00(4),DATCON                                                
         MVC   WORK+04(4),GETDAY                                                
         MVC   WORK+08(4),ADDAY                                                 
         MVC   WORK+12(4),DATAMGR                                               
         MVC   WORK+16(4),GETPROF                                               
*                                                                               
         GOTO1 AGENIOS,DMCB,BUY.PBUYREC,PAGYREC,REQHDR,WORK                     
*                                                                               
         CLI   RQCRD,C' '                                                       
         BNH   *+8                                                              
         OI    SRRQS,X'40'         SET NEED IO TA                               
*                                                                               
RL12M    DS    0H                                                               
         CLI   PAGYPROF+27,C'0'     BUY + CON                                   
         BE    RL12N                                                            
         CLI   PAGYPROF+27,C'A'     BUY + CON + TRAF                            
         BE    RL12N                                                            
         CLI   PAGYPROF+27,C'B'     BUY ONLY                                    
         BE    RL12N                                                            
         CLI   PAGYPROF+27,C'D'     BUY + TRAF ONLY                             
         BNE   RL12O                                                            
*                                                                               
RL12N    BRAS  RE,CHKEST          SEE IF I NEED 53 T/A                          
         CLI   ESTSW,0                                                          
         BE    RL12O                                                            
         OI    SRRQS,X'80'         SET NEED BUY TA                              
*                                                                               
RL12O    CLI   PAGYPROF+27,C'A'     BUY + CON + TRAF                            
         BE    RL12O5                                                           
         CLI   PAGYPROF+27,C'T'     TRAF ONLY                                   
         BE    RL12O5                                                           
         CLI   PAGYPROF+27,C'D'     BUY + TRAFFIC ONLY                          
         BE    RL12O5                                                           
         CLI   PAGYPROF+27,C'E'     CON + TRAF ONLY                             
         BNE   RL12P                                                            
*                                                                               
RL12O5   BRAS  RE,CHKTRAF                                                       
         CLI   TRAFSW,0                                                         
         BE    RL12P                                                            
         OC    SRRQS,TRAFSW                                                     
******** OI    SRRQS,X'10'         SET NEED TRAF TURNAROUND                     
RL12P    LA    R2,BUY.PBUYREC+33                                                
         MVI   ELCODE,X'95'        SEARCH FOR TEARSHEET ELEM                    
         BRAS  RE,NEXTEL                                                        
         BNE   RL12P8                                                           
         USING PTSHTELD,R2                                                      
         CLC   PTSHIDAT,RDATE     ADDED TODAY * WAS BTODAY                      
         BE    RL12P5                                                           
         CLC   PTSHCDAT,RDATE     OR CHANGED TODAY * WAS BTODAY                 
         BNE   RL12P8                                                           
         DROP  R2                                                               
*                                                                               
RL12P5   OI    SRRQS,X'01'        SET T/S REQUEST                               
*                                                                               
RL12P8   CLI   RPRG,0              IF NO PROGRAM - MUST BE OFF-LINE             
         BNE   RL12R                                                            
**OLD**  OC    RSIN,RSIN           SEE IF OFF-LINE                              
**OLD**  BNZ   RL12R                                                            
         CLI   SRACT,C'A'          SEE IF ADD                                   
         BNE   RL12PX              NO-THEN NO TURNAROUNDS                       
         BRAS  RE,CHKOID           MUST GO SET ORIGIN ID                        
         B     RL12R               OFF-LINE ADDS GET TURNAROUNDS                
*                                                                               
RL12PX   MVI   SRRQS,0              NO TURNAROUNDS FOR OFF-LINE CHANGES         
*                                                                               
RL12R    CLI   PAYCHSW,1                                                        
         BNE    *+8                                                             
         OI    SRPRG,X'80'         PAY THAT IS REALLY A CHANGE                  
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SRKEY                                        
*                                                                               
         CLI   RPRG,X'03'          PAY TRANS                                    
         BNE   RL12S               MAY PRODUCE CD CHANGE                        
         CLI   PAYCHSW,0           TEST IF ALREADY DONE                         
         BNE   RL12C               YES                                          
         MVI   PAYCHSW,1           SET DONE                                     
         B     RL12D4                                                           
*                                                                               
RL12S    CLC   SRBLMO(4),SRCPBM    TEST MONTH CHANGE                            
         BE    RL12C               NO                                           
         OC    PGROSS(12),PGROSS   TEST PAID                                    
         BZ    RL12C                                                            
*                                  CREATE POSTING TO SHIFT MONTHS               
         L     R1,PGROSS                                                        
         CVD   R1,DUB                                                           
         ZAP   SRGRS,DUB                                                        
*                                                                               
         S     R1,PGROSS+4          LESS AGYCOM                                 
         CVD   R1,DUB                                                           
         ZAP   SRNET,DUB                                                        
*                                                                               
         L     R1,PGROSS+8          CD                                          
         CVD   R1,DUB                                                           
         ZAP   SRCD,DUB                                                         
*                                                                               
         L     R1,MGSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRGST,DUB                                                        
*                                                                               
         L     R1,MGSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRGSTP,DUB                                                       
*                                                                               
         L     R1,MPSTTAXP             PST                                      
         CVD   R1,DUB                                                           
         ZAP   SRPST,DUB                                                        
*                                                                               
         L     R1,MPSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRPSTP,DUB                                                       
*                                                                               
         L     R1,CPYVALS+28          OLD GROSS                                 
         CVD   R1,DUB                                                           
         ZAP   SRCPGRS,DUB                                                      
*                                                                               
         S     R1,CPYVALS+4+28        LESS AGYCOM                               
         CVD   R1,DUB                                                           
         ZAP   SRCPNET,DUB                                                      
*                                                                               
         L     R1,CPYVALS+8+28     CD                                           
         CVD   R1,DUB                                                           
         ZAP   SRCPCD,DUB                                                       
*                                                                               
         L     R1,CGSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRCPGST,DUB                                                      
*                                                                               
         L     R1,CGSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRCPGSTP,DUB                                                     
*                                                                               
         L     R1,CPSTTAXP            PST                                       
         CVD   R1,DUB                                                           
         ZAP   SRCPPST,DUB                                                      
*                                                                               
         L     R1,CPSTTAXP                                                      
         CVD   R1,DUB                                                           
         ZAP   SRCPPSTP,DUB                                                     
*                                                                               
         OI    SRPRG,X'80'         BUY THAT MOVES PAID BUCKETS                  
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SRKEY                                        
         B     RL12C                                                            
         DROP  BUY                                                              
                                                                                
*******************************************************************             
*        CONTRACT CHG OR ADD                                                    
*******************************************************************             
RL13     CLI   PAGYPROF+27,C'0'    TEST NEED CONTRACT TA                        
         BE    RL13B                                                            
         CLI   PAGYPROF+27,C'C'    CON ONLY                                     
         BE    RL13B                                                            
         CLI   PAGYPROF+27,C'A'    BUY + CON + TRAF                             
         BE    RL13B                                                            
         CLI   PAGYPROF+27,C'E'    CON + TRAF                                   
         BNE   RL13D                                                            
*                                                                               
RL13B    LA    R5,X                                                             
         XC    X,X                                                              
         USING SRKEYD,R5                                                        
*                                                                               
         LA    R6,RECVR                                                         
         USING PCONREC,R6                                                       
*                                                                               
         MVI   SRTYPE,C'C'                                                      
         MVC   SRAGY(3),PCONKAGY                                                
         MVC   SRCLT,PCONKCLT                                                   
         MVC   SREST,PCONNUM                                                    
         MVC   SRPUB,PCONKPUB                                                   
         MVC   SRUSER,RUSER                                                     
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SRKEY                                        
         DROP  R5,R6                                                            
*                                                                               
RL13D    LA    R5,X                                                             
         XC    X,X                                                              
         USING SRKEYD,R5                                                        
         LA    R6,RECVR                                                         
         USING PCONREC,R6                                                       
         LA    R2,RECVR+33                                                      
         MVI   ELCODE,X'85'        CHK FOR AUTO SPACE ELEM                      
*                                                                               
RL13E    BRAS  RE,NEXTEL                                                        
         BNE   GETRCV                                                           
         USING PASRELD,R2                                                       
         CLC   PASRCDAT,RDATE        * WAS BTODAY                               
         BNE   RL13E                                                            
*                                                                               
         MVI   SRTYPE,C'R'                                                      
         MVC   SRAGY(3),PCONKAGY                                                
         MVC   SRCLT,PCONKCLT                                                   
         OC    PASRCLT,PASRCLT                                                  
         BZ    *+10                                                             
         MVC   SRPRD,PASRCLT            SLAVE CLIENT CODE                       
         MVC   SREST,PCONNUM                                                    
         MVC   SRPUB,PCONKPUB                                                   
         MVC   SRUSER,RUSER                                                     
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SRKEY                                        
         XC    X,X                                                              
         B     RL13E                 SINCE MASTER CONTRACTS MAY                 
         DROP  R2                    HAVE ELEMS FOR SLAVE CLTS                  
**********************************************************************          
* LAST RECORD WAS NOT A COPY                                                    
**********************************************************************          
RL13F    CLI   RSIN,ROCHGOLY       TEST OFF-LINE CHANGES ONLY    NEW            
         BE    GETRCV              SO OKAY                       NEW            
         OC    RSIN+1(3),RSIN+1    TEST OFF-LINE CHANGES ONLY    OLD            
         BZ    GETRCV              SO OKAY                       OLD            
*                                                                               
         MVC   P(23),=C'**CHANGE WITHOUT COPY**'                                
         MVC   X(56),P             SAVE ERROR MESSAGE                           
         BRAS  RE,RPRT                                                          
*                                                                               
         L     R5,ARECVR                                                        
         BRAS  RE,DMPREC                                                        
*                                                                               
         TM    MSGFLAG,MSGCHGQ     SKIP IF EMAIL SENT ALREADY                   
         BO    RL13G                                                            
         OI    MSGFLAG,MSGCHGQ     SET MESSAGE SENT FLAG                        
         MVC   P(24),=C'AUTONOTE*KWAN,DEIS,SKUI:' SET RECIPIENT                 
         MVC   P+24(56),X          RETRIEVE ERROR MESSAGE                       
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(72,P) SEND EMAIL                         
         MVC   P,SPACES            CLEAR PRINT LINE                             
*                                                                               
RL13G    B     GETRCV                                                           
                                                                                
**********************************************************************          
*        ADD   RECOVER RECORD                                                   
**********************************************************************          
RL14     CLI   LASTTYP,RRECTCPY    CHECK FOR STAND ALONE CPY                    
         BE    RL11                                                             
         B     RL12A                                                            
*                                                                               
RECVEOF  DS    0H                                                               
         CLOSE (RECVIN,)                                                        
*                                                                               
         TM    RCUPSI,X'80'                                                     
         BZ    EOF4                                                             
*                                                                               
EOF2     MVC   P(42),=C'*PP133* ANY MORE INPUT TAPES  REPLY Y OR N'             
         GOTO1 LOGIO,DMCB,1,(42,P)                                              
         MVI   DUB,C'N'                                                         
         GOTO1 (RF),(R1),0,(3,DUB)                                              
         CLI   DUB,C'N'                                                         
         BE    EOF4                                                             
         CLI   DUB,C'Y'                                                         
         BE    RL4                                                              
         B     EOF2                                                             
*                                                                               
EOF4     B     FROMSRT                                                          
                                                                                
*********************************************************************           
* NEXTEL ROUTINE                                                                
*********************************************************************           
NEXTEL   LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         J     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
********************************************************************            
*        PROCESS RECORDS OUT OF SORT                                            
********************************************************************            
FROMSRT  LA    R1,ESTTOTS                                                       
         LA    R0,ACCTN             CLEAR ALL ACCUMS                            
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         XC    LSTKEY,LSTKEY                                                    
         MVI   RQSW,0                                                           
         XC    USER53,USER53                                                    
         XC    DATE53,DATE53                                                    
*                                                                               
         CLI   RQWOPT,C'Y'                                                      
         BNE   FS2                                                              
***                                                                             
*** NOTE SYSNAID IS THE 2 CHARACTER FILE NUMBER                                 
***                                                                             
         MVC   PBUYRQN+40+6(2),SYSNAID       SET TRUE FILE NAME                 
         OPEN  (PBUYRQN,(OUTPUT))                                               
*                                                                               
FS2      DS    0H                                                               
         B     FS4B                                                             
*                                                                               
FS4      DS    0H                                                               
         MVC   LSTKEY,SRKEY                                                     
FS4B     DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,4(R1)                                                         
         USING SRKEYD,R5                                                        
*                                                                               
         XC    PCSW,PCSW           INIT PCSW                                    
*                                                                               
         LTR   R5,R5               TEST EOF                                     
         BNZ   *+8                                                              
         LA    R5,=X'FF'                                                        
*                                                                               
         MVC   THISKEY,0(R5)                                                    
         LA    R5,THISKEY                                                       
*                                                                               
         CLC   SRKEYR,LSTKEY                                                    
         BE    FS50                                                             
*                                  KEY NOT SAME                                 
         CLI   LSTKEY,C'A'         ANALYSIS                                     
         BE    FS20                                                             
         CLI   LSTKEY,C'P'         PAYS                                         
         BE    FS30                                                             
         CLI   LSTKEY,C'C'         CONTRACTS                                    
         BE    FS40                                                             
         CLI   LSTKEY,C'R'         AUTO SPACE RESV                              
         BE    FS45                                                             
         CLI   LSTKEY,0            FIRST                                        
         BE    FS50                                                             
         DC    H'0'                                                             
         SPACE 3                                                                
FS20     DS    0H                  ANALYSIS RECORD BRKS                         
*                                                                               
         TM    RQSW,X'01'          TEARSHEET DATA ADDED OR CHANGED              
         BZ    *+8                                                              
         BRAS  RE,TEARREQ          PRODUCE TEARSHEET T/A (IF NEEDED)            
*                                                                               
         TM    RQSW,X'40'          TEST IO REQ NEEDED                           
         BZ    *+8                                                              
         BRAS  RE,IOREQ                                                         
*                                                                               
         NI    RQSW,X'BF'          RESET IO TA                                  
*                                                                               
         TM    RQSW,X'12'          TEST TRAFFIC TURNAROUND REQUIRED             
         BZ    *+8                 (X'10' OR X'02')                             
         BRAS  RE,TRAFREQ                                                       
*                                                                               
         NI    RQSW,X'ED'          RESET TRAF TA                                
*                                                                               
         CLC   SRKEY(18),LSTKEY         THRU PUB                                
         BE    FS50                                                             
*                                                                               
         TM    RQSW,X'80'          TEST NEED BUY TA                             
         BZ    *+8                                                              
         BRAS  RE,BYREQ                                                         
*                                                                               
         MVI   RQSW,0              RESET REQUEST SW - NO TA'S                   
         XC    USER53,USER53                                                    
         XC    DATE53,DATE53                                                    
*                                                                               
*                                                                               
         CLC   SRKEY(12),LSTKEY         THRU EST                                
         BE    FS50                                                             
         BRAS  RE,READEST               GO READ ESTHDR                          
         LA    R1,ESTTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         BRAS  RE,BUCKPUT               ADD BUCKET DATA TO FILE                 
         MVI   ESTACT,C'N'                                                      
*                                                                               
         CLC   SRKEY(10),LSTKEY         THRU PRD                                
         BE    FS50                                                             
*                                                                               
         LA    R1,PRDTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
*                                                                               
         CLC   SRKEY(7),LSTKEY          THRU CLT                                
         BE    FS50                                                             
*                                                                               
         MVC   P+25(20),=CL20'**CLIENT TOTALS**'                                
         LA    R1,CLTTOTS                                                       
         BRAS  RE,PRTOTS                                                        
         BRAS  RE,ROLTOTS                                                       
*                                                                               
         CLC   SRKEY(4),LSTKEY          THRU AGY                                
         BE    FS28                                                             
*                                                                               
         MVC   P+25(20),=CL20'**AGY/MED TOTALS**'                               
         LA    R1,MEDTOTS                                                       
         BRAS  RE,PRTOTS                                                        
*                                  SET TOTALS IN AGYTAB                         
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING AGYTBD,R4                                                        
         MVC   AGYMED,LSTKEY+1                                                  
         GOTO1 BINSRCH,BSPARS,AGYTBD                                            
         CLI   0(R1),1                                                          
         BE    FS22                                                             
         L     R4,0(R1)                                                         
         CLI   AGYSTAT,C'Y'        NO TOTALS FOR THIS AGY/MED                   
         BE    FS22                                                             
         MVC   AGTTOTS,MEDTOTS                                                  
         LA    R1,MEDTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         B     FS22B                                                            
*                                                                               
*                                                                               
FS22     DS    0H                                                               
         LA    R1,MEDTOTS                                                       
         LA    R0,ACCN                                                          
         BRAS  RE,CLRTOTS                                                       
*                                                                               
FS22B    DS    0H                                                               
         CLC   SRKEY(3),LSTKEY          THRU AGY                                
         BE    FS28                                                             
*                                  TEST ANY PAID                                
         ZAP   DUB,AGYTOTS+PNDSP(6)   PD NET                                    
         SP    DUB,AGYTOTS+PCDDSP(6)  - CD                                      
         AP    DUB,AGYTOTS+PGDSP(6)   ADD GST PAID                              
         AP    DUB,AGYTOTS+PPDSP(6)   ADD PST PAID                              
         BZ    FS24                                                             
*                                  WRITE PAID TOTALS TO 1ST AGYHDR              
         XC    KEY,KEY                                                          
         MVC   KEY(2),LSTKEY+1     USE FIRST FOR AGY                            
         GOTO1 HIGH                                                             
*                                                                               
FSAGYLP  CLC   KEY(2),KEYSAVE      MAKE SURE WE HAVE RIGHT AGY                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   KEY+3,1             OKAY IF AGENCY HEADER FOUND                  
         BE    FSAGYFD                                                          
*                                                                               
FSAGYCN  GOTO1 SEQ                 CHECK NEXT RECORD ON FILE                    
         B     FSAGYLP                                                          
*                                                                               
FSAGYFD  LA    R0,PAGYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         MVC   PAGYTDAY,PTODAY                                                  
         ZAP   PAGYPAID,DUB                                                     
         CLI   RCWRITE,C'Y'                                                     
         BNE   FS24                                                             
         GOTO1 PUTPRT                                                           
*                                                                               
FS24     MVC   P+25(20),=CL20'**AGENCY TOTALS**'                                
         LA    R1,AGYTOTS                                                       
         BRAS  RE,PRTOTS                                                        
         BRAS  RE,ROLTOTS                                                       
*                                                                               
         CLC   SRKEY(1),LSTKEY          TYPE                                    
         BE    FS28                                                             
*                                                                               
         MVC   P+25(20),=CL20'**FILE TOTALS**'                                  
         LA    R1,RUNTOTS                                                       
         BRAS  RE,PRTOTS                                                        
*                                                                               
FS28     MVI   SPACING,2                                                        
         BRAS  RE,RPRT                                                          
*                                                                               
FS30     B     FS50                PAYMENT RECDS                                
*                                                                               
FS40     DS    0H                  CONTRACT RECS                                
         BRAS  RE,CONREQ                                                        
         B     FS50                                                             
*                                                                               
FS45     BRAS  RE,RESVREQ          AUTO SPACE RESV                              
*                                                                               
FS50     CLI   SRTYPE,C'A'                                                      
         BE    FS60                                                             
         CLI   SRTYPE,C'P'                                                      
         BE    FS70                                                             
         CLI   SRTYPE,C'C'            CONTRACTS                                 
         BE    FS4                                                              
         CLI   SRTYPE,C'R'            AUTO SPACE RESV                           
         BE    FS4                                                              
         CLI   SRTYPE,X'FF'                                                     
         BE    FS90                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
FS60     DS    0H                  ANALYSIS DETAILS                             
         OC    RQSW,SRRQS          REQS NEEDED                                  
*                                                                               
         MVI   PAYBUY,C'B'         BUY (ORDERED $)                              
         CLI   SRPRG,X'11'         IF BUY PROG                                  
         BE    FS62                                                             
         CLI   SRPRG,X'1C'         OR SHAKE RATTLE ROLL                         
         BE    FS62                                                             
         CLI   SRPRG,X'18'         OR MULTI-BUY CHANGE                          
         BE    FS62                                                             
         CLI   SRPRG,X'83'         OR SPECIAL PAY TRANS                         
         BE    FS62                                                             
         MVI   PAYBUY,C'P'         ELSE PAID $                                  
*                                                                               
FS62     DS    0H                                                               
         CLI   PAYBUY,C'B'                                                      
         BNE   FS64                                                             
         TM    SRRQS,X'80'        P53 T/A NEEDED                                
         BZ    FS64                                                             
         MVC   USER53,SRUSER      SAVE USERID OF LAST BUY ACTIVITY              
*                                                                               
         OC    DATE53,DATE53                                                    
         BZ    FS62C                                                            
         CLC   DATE53,SRRDATE     CHECK FOR EARLIEST ACTIVITY DATE              
         BL    *+10                                                             
FS62C    MVC   DATE53,SRRDATE                                                   
*                                                                               
FS64     BRAS  RE,BUCKUP           POST TO CREATED BUCKET RECS                  
         BRAS  RE,PRTANAL          PRINT DETAILS                                
*                                                                               
FS70     B     FS4                 PAYMENT DETAILS                              
*                                                                               
FS90     CLI   RQWOPT,C'Y'                                                      
         BNE   FS91                                                             
         CLOSE (PBUYRQN,)                                                       
*                                                                               
FS91     MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20                                                      
*                                                                               
         L     R4,AAGYLST                                                       
         USING AGYTBD,R4                                                        
         L     R5,BSPARS+8         COUNT                                        
         LA    R6,P                                                             
         USING PLD,R6                                                           
*                                                                               
FS92     DS    0H                                                               
         CLI   AGYSTAT,C'Y'        SKIP THIS AGY/MED                            
         BE    FS93                                                             
         MVC   PLAGY,AGYMED                                                     
         MVC   PLMED,AGYMED+2                                                   
         MVC   PLPRD(33),AGYTNAM                                                
*                                                                               
         LA    R1,AGTTOTS                                                       
         BRAS  RE,PRTOTS                                                        
*                                                                               
FS93     DS    0H                                                               
         A     R4,BSPARS+12        LENGTH                                       
         BCT   R5,FS92                                                          
*                                                                               
         BRAS  RE,RPRT                                                          
         MVC   PLPRD(15),=C'**FILE TOTALS**'                                    
         LA    R1,RUNTOTS                                                       
         BRAS  RE,PRTOTS                                                        
*                                                                               
         GOTO1 SORTER,DMCB,=C'END'     TELL SORTER WE'RE DONE                   
         B     EXIT                                                             
*********************************************************************           
*        CREATE BUY TA                                                          
*********************************************************************           
BYREQ    NTR1                                                                   
         LA    R5,LSTKEY                                                        
         MVC   RQCRD,SPACES                                                     
         MVC   RQPROG,=C'53'                                                    
         MVC   RQAGY(6),SRAGY                                                   
*                                                                               
         BRAS  RE,GET53OP         READ 53 PROFILE                               
*                                                                               
         CLI   L53PROF+13,C'Y'    SEE IF SUPPRESSING                            
         BE    BYREQX                                                           
*                                                                               
         CLI   L53PROF+11,C'N'    SEE IF DOING FOR TEST BUYS                    
         BNE   BYREQ5                                                           
         LA    R5,LSTKEY                                                        
         CLI   SRBFD,C'T'         NO 53 FOR TEST BUY                            
         BE    BYREQX                                                           
*                                                                               
BYREQ5   DS    0H                                                               
         LA    R5,LSTKEY                                                        
         MVC   RQPRD,SRPRD                                                      
         TM    RQSW,X'20'                                                       
         BZ    *+10                                                             
         MVC   RQPRD,=C'ZZZ'                                                    
         EDIT  (B2,SREST),(3,RQEST),FILL=0                                      
         GOTO1 PUBEDIT,DMCB,SRPUB,(C'Q',RQPUB)                                  
         MVC   RQDATES(2),=C'ES'                                                
         MVC   RQSTR(5),=C'AUTO-'                                               
         MVC   RQSTR+5(3),SRBUYER                                               
*                                                                               
         BRAS  RE,WRTREQ                                                        
*                                                                               
*        SEARCH FOR X'46' (RFP GROUP CODE ELEMENT)                              
*        TO SEE IF I HAVE TO CREATE RF REQUEST                                  
*                                                                               
         BRAS  RE,READCLT  MUST READ CLIENT                                     
*                                                                               
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE,X'46'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BYREQX                                                           
         MVC   RQPROG(2),=C'RF'                                                 
         MVC   RQPROG+49(8),2(R2)      RFP GROUP CODE IN COL 50                 
         OC    RQPROG+49(8),SPACES                                              
         BRAS  RE,WRTREQ                                                        
*                                                                               
BYREQX   XIT1                                                                   
                                                                                
*********************************************************************           
* IO REQUEST                                                                    
*********************************************************************           
IOREQ    NTR1                                                                   
         LA    R5,LSTKEY                                                        
         CLI   SRBFD,C'T'                                                       
         BE    IOREQX            NO P72 FOR TEST BUYS                           
         MVC   RQCRD,SPACES                                                     
         MVC   RQPROG,=C'72'                                                    
         MVC   RQAGY(6),SRAGY                                                   
         MVC   RQPRD,SRPRD                                                      
         TM    RQSW,X'20'                                                       
         BZ    *+10                                                             
         MVC   RQPRD,=C'ZZZ'                                                    
         EDIT  (B2,SREST),(3,RQEST),FILL=0                                      
         GOTO1 PUBEDIT,DMCB,SRPUB,(C'Q',RQPUB)                                  
         GOTO1 DATCON,DMCB,(3,SRDATE),RQDATES                                   
         MVC   RQDATES+6(6),RQDATES                                             
         MVI   RQCRD+63,C'A'                                                    
         EDIT  (B1,SRLIN),(2,RQCRD+59),FILL=0                                   
*                                                                               
         BRAS  RE,GET72OP          CHECK PROFILES AND SET OUTPUT                
*                                  RETURNED IN LAST72O                          
         BRAS  RE,WRTREQ                                                        
IOREQX   XIT1                                                                   
                                                                                
**********************************************************************          
*                                                                               
**********************************************************************          
TRAFREQ  NTR1                                                                   
         LA    R5,LSTKEY                                                        
         MVC   RQCRD,SPACES                                                     
         MVC   RQPROG,=C'78'                                                    
         MVC   RQAGY(6),SRAGY     NEEDED FOR CHECKING PROFILE                   
*                                                                               
         BRAS  RE,GET78OP         READ 78 PROFILE                               
         BRAS  RE,GET72A          READ 72A PROFILE                              
*                                                                               
*******  CLI   L78PROF+8,C'Y'     CHECK RATE CHG TRIGGER                        
         CLI   L72PROFA+14,C'Y'   CHECK RATE CHG TRIGGER                        
         BE    TRAFQ3             NOW ONLY ON 72A PROFILE                       
         MVC   TESTER,RQSW                                                      
         NI    TESTER,X'12'   SET OF ALL BUT X'10' AND X'02'                    
         CLI   TESTER,X'02'  IF ONLY RATE CHANGE - THEN NO T/A                  
         BE    TRAFREQX      UNLESS L72PROFA+14 IS 'Y'                          
*                                                                               
TRAFQ3   DS    0H                                                               
         CLI   L78PROF+7,C'Y'     SEE IF AUTO TURNAROUND "SUPPRESSED"           
         BE    TRAFREQX           YES - NO 78                                   
         CLI   L78PROF+2,C'Y'     SEE IF DOING FOR TEST BUYS                    
         BE    TRAFQ5                                                           
         LA    R5,LSTKEY                                                        
         CLI   SRBFD,C'T'         NO 78 FOR TEST BUY                            
         BE    TRAFREQX                                                         
*                                                                               
TRAFQ5   LA    R5,LSTKEY                                                        
         MVC   RQPRD,SRPRD                                                      
         TM    RQSW,X'20'                                                       
         BZ    *+10                                                             
         MVC   RQPRD,=C'ZZZ'                                                    
         EDIT  (B2,SREST),(3,RQEST),FILL=0                                      
         GOTO1 PUBEDIT,DMCB,SRPUB,(C'Q',RQPUB)                                  
         GOTO1 DATCON,DMCB,(3,SRDATE),RQDATES                                   
         MVC   RQDATES+6(6),RQDATES                                             
         MVI   RQCRD+61,C'N'             FOR UNORDERED BUYS ONLY                
         MVI   RQCRD+63,C'A'                                                    
         EDIT  (B1,SRLIN),(2,RQCRD+59),FILL=0                                   
         MVC   RQSTR(5),=C'AUTO-'                                               
         MVC   RQSTR+5(3),SRBUYER                                               
         OC    SRRDATE,SRRDATE        SEE IF THERE                              
         BZ    *+10                                                             
         MVC   RQCRD+65(3),SRRDATE     ACTIVITY DATE IN QOPT 5-7                
         BRAS  RE,WRTREQ                                                        
TRAFREQX XIT1                                                                   
                                                                                
*********************************************************************           
*                                                                               
*********************************************************************           
TEARREQ  NTR1                                                                   
         LA    R5,LSTKEY                                                        
         MVC   RQCRD,SPACES                                                     
         MVC   RQPROG,=C'TT'                                                    
         MVC   RQAGY(6),SRAGY                                                   
         BRAS  RE,GETTTOP         READ TT PROFILE                               
         CLI   LTTPROF+12,C'Y'    SEE IF PRODUCING                              
         BNE   TEARREQX                                                         
         LA    R5,LSTKEY                                                        
         CLI   SRBFD,C'T'         NO TT FOR TEST BUY                            
         BE    TEARREQX                                                         
*                                                                               
TEARQ5   LA    R5,LSTKEY                                                        
         MVC   RQPRD,SRPRD                                                      
         TM    RQSW,X'20'                                                       
         BZ    *+10                                                             
         MVC   RQPRD,=C'ZZZ'                                                    
         EDIT  (B2,SREST),(3,RQEST),FILL=0                                      
         GOTO1 PUBEDIT,DMCB,SRPUB,(C'Q',RQPUB)                                  
         GOTO1 DATCON,DMCB,(3,SRDATE),RQDATES                                   
         MVC   RQDATES+6(6),RQDATES                                             
         MVI   RQCRD+61,C'X'             FOR ALL STATUS                         
         CLI   LTTPROF+13,C'Y'           SEE IF FLAGGING BL/PD/TRAF             
         BNE   *+8                                                              
         MVI   RQCRD+62,C'Y'                                                    
         EDIT  (B1,SRLIN),(2,RQCRD+59),FILL=0                                   
         MVC   RQSTR(5),=C'AUTO-'                                               
         MVC   RQSTR+5(3),SRBUYER                                               
         BRAS  RE,WRTREQ                                                        
TEARREQX XIT1                                                                   
                                                                                
*********************************************************************           
*                                                                               
*********************************************************************           
CONREQ   NTR1                      CREATE CON T/A                               
         LA    R5,LSTKEY                                                        
         MVC   RQCRD,SPACES                                                     
         MVC   RQCRD(2),=C'13'                                                  
         MVC   RQAGY(6),SRAGY                                                   
         GOTO1 PUBEDIT,DMCB,SRPUB,(C'Q',RQPUB)                                  
         EDIT  (B2,SREST),(3,RQEST),FILL=0                                      
         MVC   RQSTR(4),=C'AUTO'                                                
*                                                                               
         BRAS  RE,GET13OP        CHECK PROFILES AND RETURN OUTPUT               
*                                IN LAST13O                                     
         CLI   L13PROFA+5,C'Y'     SEE IF T/A BEING SUPPRESSED                  
         BE    CONREQX             DON'T WRITE REQUEST                          
         BRAS  RE,WRTREQ                                                        
*                                                                               
CONREQX  DS    0H                                                               
         XIT1                                                                   
                                                                                
*********************************************************************           
*                                                                               
*********************************************************************           
RESVREQ  NTR1                      CREATE AUTO SPACE RESV                       
         LA    R5,LSTKEY                                                        
         MVC   RQCRD,SPACES                                                     
         MVC   RQCRD(2),=C'15'                                                  
         MVC   RQAGY(6),SRAGY                                                   
         MVC   RQDIV,SRPRD            SRPRD MIGHT HAVE SLAVE CLIENT             
         OC    RQDIV,SPACES                                                     
         GOTO1 PUBEDIT,DMCB,SRPUB,(C'Q',RQPUB)                                  
         EDIT  (B2,SREST),(3,RQEST),FILL=0                                      
         MVC   RQSTR(4),=C'AUTO'                                                
         BRAS  RE,GET15OP            CHECK PROFILES AND RETURN                  
*                                    OUTPUT IN LAST15O                          
         BRAS  RE,WRTREQ                                                        
         XIT1                                                                   
         SPACE 3                                                                
WRTREQ   NTR1                      WRITE REQ REC                                
         XC    REQHDR,REQHDR                                                    
         MVC   REQORIG,SRUSER                                                   
         MVC   REQDATE,TODAY                                                    
         CLC   RQCRD(2),=C'78'      SEE IF TRAFFIC TURNAROUND                   
         BNE   WRTREQ4               SKIP TO P72 CHECKS                         
*                                                                               
         CLC   RQAGY(2),=C'OM'       CHECK FOR OMNY                             
         BNE   WRTREQ3E                                                         
*                                                                               
WRTREQ3  DS    0H                                                               
         CLC   RQCLT,=C'SH4'    AND CLIENT SH4                                  
         BNE   WRTREQ3A                                                         
         MVC   REQORIG(2),=H'7811'  ALTER ORIGIN ID                             
         B     WRTREQ8              SO P78 TURNAROUNDS GO ELSEWHERE             
*                                                                               
WRTREQ3A DS    0H                                                               
         CLC   RQCLT,=C'BPC'    CLIENT BPC TO OMNYT                             
         BNE   WRTREQ8                                                          
         MVC   REQORIG(2),=H'1376'  ALTER ORIGIN ID                             
         B     WRTREQ8              SO P78 TURNAROUNDS GO ELSEWHERE             
*                                                                               
WRTREQ3E DS    0H                                                               
         CLC   RQAGY(2),=C'WI'                                                  
         BNE   WRTREQ3G                                                         
         BRAS  RE,CHKGROUP          POSSIBLE REQORIG REPLACEMENT                
         B     WRTREQ8                                                          
*                                                                               
WRTREQ3G DS    0H                                                               
         CLC   RQAGY(2),=C'H7'                                                  
         BNE   WRTREQ3H                                                         
         BRAS  RE,CHKGRPH7          POSSIBLE REQORIG REPLACEMENT                
         BRAS  RE,CHKPDH7           CHECK FOR PRODUCT - REDIRECT                
         BRAS  RE,CHKCLH7           CHECK FOR CLIENT  - REDIRECT                
         B     WRTREQ8                                                          
*                                                                               
WRTREQ3H DS    0H                                                               
         B     WRTREQ8                                                          
***      NO-OP RE-DIRECT OF P78'S FOR FORD                                      
***      CLC   RQAGY(2),=C'FR'      FORD                                        
***      BNE   WRTREQ8                                                          
***      CLC   RQCLT,=C'MJ '    AND CLIENT MJ                                   
***      BNE   WRTREQ8                                                          
***      MVC   REQORIG(2),=H'6724'  ALTER ORIGIN ID TO FDMOMN                   
***      B     WRTREQ8              SO P78 TURNAROUNDS GO ELSEWHERE             
*                                                                               
WRTREQ4  DS    0H                                                               
         CLC   RQCRD(2),=C'72'     SEE IF I/O REQUEST                           
         BNE   WRTREQ4X                                                         
         B     WRTREQ4X                                                         
***      NO-OP RE-DIRECT OF P72'S FOR FORD                                      
***      CLC   RQAGY(2),=C'FR'      FORD                                        
***      BNE   WRTREQ4X                                                         
***      CLC   RQCLT,=C'MJ '    AND CLIENT MJ                                   
***      BNE   WRTREQ4X                                                         
***      MVC   REQORIG(2),=H'6724'  ALTER ORIGIN ID TO FDMOMN                   
***      B     WRTREQ4X             SO P72 TURNAROUNDS GO ELSEWHERE             
*                                                                               
WRTREQ4X LA    R4,LAST72O                                                       
         CLC   RQCRD(2),=C'72'                                                  
         BE    WRTREQ5                                                          
         LA    R4,LAST13O                                                       
         CLC   RQCRD(2),=C'13'                                                  
         BE    WRTREQ5                                                          
         LA    R4,LAST15O                                                       
         CLC   RQCRD(2),=C'15'                                                  
         BE    WRTREQ5                                                          
         CLC   RQCRD(2),=C'53'       SEE IF BUY T/A                             
         BNE   WRTREQ8                                                          
         OC    DATE53,DATE53     HAVE AN EARLIEST ACTIVITY DATE?                
         BZ    *+10                                                             
         MVC   RQCNTLD,DATE53       SET AS CONTROL DATE                         
         OC    USER53,USER53        JUST IN CASE I DON'T HAVE AN ID             
         BZ    WRTREQ4Z             USE SRUSER                                  
         MVC   REQORIG,USER53                                                   
         B     WRTREQ8                                                          
*                                                                               
WRTREQ4Z OC    SRUSER,SRUSER          SEE IF I HAVE ONE                         
         BZ    WQ4                    IF NOT, DON'T CREATE THE P53              
         B     WRTREQ8                                                          
*                                                                               
WRTREQ5  MVC   REQOUT(6),0(R4)        SET OUTPUT TYPE (6 CHARS)                 
*                                                                               
WRTREQ8  CLI   RQWOPT,C'Y'                                                      
         BNE   WQ2                                                              
*                                                                               
         LA    R1,PBUYRQN                                                       
         LA    R0,REQHDR                                                        
         PUT   (1),(0)                                                          
*                                                                               
WQ2      DS    0H                                                               
         CLI   RQPOPT,C'Y'                                                      
         BNE   WQ4                                                              
         MVC   P(80),RQCRD                                                      
*                                                                               
         CLC   SRUSER,REQORIG         CHECK IF CODE IS CHANGED                  
         BE    WQ200100               IF NO LEAVE IT ALONE                      
*                                                                               
         MVC   P+86(4),=C'OLD='       IF YES LABELS "OLD=" AND "NEW="           
         MVC   P+99(4),=C'NEW='       WILL SHOW WHICH ONE IS WHICH              
         SR    R0,R0                                                            
         ICM   R0,3,REQORIG                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+103(5),DUB+5(3)                                                
*                                                                               
WQ200100 DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SRUSER                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+90(5),DUB+5(3)                                                 
*                                                                               
         MVC   PSECOND(7),=C'HEADER='                                           
         MVC   PSECOND+7(26),REQHDR                                             
         GOTO1 HEXOUT,DMCB,REQHDR,PSECOND+40,26,=C'N'                           
*                                                                               
         BRAS  RE,RPRT                                                          
         MVC   P(14),=C'REQ HEX 01-40='                                         
         GOTO1 HEXOUT,DMCB,RQCRD,P+15,40,=C'N'                                  
         BRAS  RE,RPRT                                                          
         MVC   P(14),=C'REQ HEX 41-80='                                         
         GOTO1 HEXOUT,DMCB,RQCRD+40,P+15,40,=C'N'                               
         BRAS  RE,RPRT                                                          
WQ4      DS    0H                                                               
         XIT1                                                                   
                                                                                
**********************************************************************          
*                                                                               
**********************************************************************          
CLRTOTS  DS    0H                                                               
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,CLRTOTS                                                       
         BR    RE                                                               
                                                                                
ROLTOTS  DS    0H                                                               
         LA    R0,ACCN                                                          
RT2      DS    0H                                                               
         AP    ACCL(6,R1),0(6,R1)                                               
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,RT2                                                           
         BR    RE                                                               
                                                                                
**********************************************************************          
*        READ FOR CLIENT GROUP                                                  
**********************************************************************          
CHKGROUP NTR1                                                                   
         LA    R2,IDTAB            GROUP ID TABLE                               
CHKGR10  XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING GRPRECD,RE          CLIENT GROUP PASSIVE POINTER                 
*                                                                               
         MVI   GRPPTYP,GRPPCGQ     RECORD TYPE                                  
         MVC   GRPPAGY(3),RQAGY    AGENCY/MEDIA                                 
         MVC   GRPPVAL(3),RQCLT    CLIENT                                       
         OC    GRPPVAL,SPACES      SPACE PADDED                                 
         MVC   GRPPID(1),0(R2)     GROUP ID                                     
         MVI   GRPPCODE,X'01'      GROUP CODE 01                                
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE     CHECK THROUGH GROUP CODE                     
         BE    CHKGR30             MATCH FOUND                                  
*                                  NO MATCH FOUND                               
CHKGR20  LA    R2,3(R2)            BUMP TO NEXT ID                              
         CLI   0(R2),X'FF'         END ?                                        
         BNE   CHKGR10             NO - GO TEST NEXT                            
         B     CHKGXIT             LEAVE REQORIG ALONE                          
*                                                                               
CHKGR30  DS    0H                  MATCHING CLIENT GROUP RECORD FOUND           
         TM    KEY+25,X'80'        RECORD DELETED ?                             
         BO    CHKGR20             YES - DO NOT USE                             
         MVC   REQORIG(2),1(R2)    REPLACE ORIGIN ID FROM TABLE                 
*                                                                               
CHKGXIT  XIT1                                                                   
*                                                                               
*************                                                                   
*****************************************************************               
*****************************************************************               
*****************************************************************               
*************                                                                   
*                                  READ FOR CLIENT GROUP                        
CHKGRPH7 NTR1                                                                   
*                                                                               
         LA    R2,IDTABH7          GROUP ID TABLE                               
CHKGRH10 XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING GRPRECD,RE          CLIENT GROUP PASSIVE POINTER                 
*                                                                               
         MVI   GRPPTYP,GRPPCGQ     RECORD TYPE                                  
         MVC   GRPPAGY(3),RQAGY    AGENCY/MEDIA                                 
         MVC   GRPPVAL(3),RQCLT    CLIENT                                       
         OC    GRPPVAL,SPACES      SPACE PADDED                                 
         MVC   GRPPID(1),0(R2)     GROUP ID                                     
         MVC   GRPPCODE(1),1(R2)   GROUP CODE 01 OR 02 (2 BYTE IN TAB)          
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE     CHECK THROUGH GROUP CODE                     
         BE    CHKGRH30            MATCH FOUND                                  
*                                  NO MATCH FOUND                               
CHKGRH20 LA    R2,H7EQU(R2)        BUMP TO NEXT ID                              
         CLI   0(R2),X'FF'         END ?                                        
         BNE   CHKGRH10            NO - GO TEST NEXT                            
         B     CHKGHXIT            LEAVE REQORIG ALONE                          
*                                                                               
CHKGRH30 DS    0H                  MATCHING CLIENT GROUP RECORD FOUND           
*                                                                               
         TM    KEY+25,X'80'        RECORD DELETED ?                             
         BO    CHKGRH20            YES - DO NOT USE                             
         MVC   REQORIG(2),2(R2)    REPLACE ORIGIN ID FROM TABLE                 
*        DC    H'0'                                                             
*                                                                               
                                                                                
CHKGHXIT XIT1                                                                   
                                                                                
*********************************************************************           
*        REDIRECT P78 FOR CERTAIN MINDSHARE PRODUCTS                            
*********************************************************************           
CHKPDH7  NTR1                                                                   
         LA    R1,H7PDTAB                                                       
         MVC   WORK(3),RQCLT                                                    
         MVC   WORK+3(3),RQPRD                                                  
CKPD5    CLI   0(R1),X'FF'    CHECK FOR END OF TABLE                            
         BE    CHKPDH7X                                                         
         CLC   WORK(6),0(R1)                                                    
         BE    CKPD10                                                           
         LA    R1,8(R1)      NEXT TABLE ENTRY                                   
         B     CKPD5                                                            
*                                                                               
CKPD10   DS    0H                                                               
         MVC   REQORIG(2),6(R1)    REPLACE ORIGIN ID FROM TABLE                 
CHKPDH7X XIT1                                                                   
                                                                                
*********************************************************************           
*        REDIRECT P78 FOR CERTAIN MINDSHARE CLIENTS                             
*********************************************************************           
CHKCLH7  NTR1                                                                   
         LA    R1,H7CLTAB                                                       
CKCL5    CLI   0(R1),X'FF'    CHECK FOR END OF TABLE                            
         BE    CHKCLH7X                                                         
         CLC   RQCLT,0(R1)                                                      
         BE    CKCL10                                                           
         LA    R1,5(R1)      NEXT TABLE ENTRY                                   
         B     CKCL5                                                            
*                                                                               
CKCL10   DS    0H                                                               
         MVC   REQORIG(2),3(R1)    REPLACE ORIGIN ID FROM TABLE                 
CHKCLH7X XIT1                                                                   
                                                                                
**********************************************************************          
*        CREATE AND PRINT DETAIL LINE                                           
**********************************************************************          
PRTANAL  NTR1                                                                   
         LA    R6,P                                                             
         USING PLD,R6                                                           
*                                                                               
         MVC   PLAGY,SRAGY                                                      
         MVC   PLMED,SRMED                                                      
         MVC   PLCLT,SRCLT                                                      
         MVC   PLPRD,SRPRD                                                      
         EDIT  (B2,SREST),(3,PLEST),FILL=0                                      
         TM    PESTTEST,X'80'           SEE IF TEST ESTIMATE                    
         BZ    *+8                                                              
         MVI   PLEST-1,C'T'                                                     
         GOTO1 PUBEDIT,DMCB,SRPUB,(C'S',PLPUB)                                  
         GOTO1 DATCON,DMCB,(3,SRDATE),(5,PLDATE)                                
         CLI   SRLIN,1                                                          
         BNH   PA4                                                              
         MVI   PLDATE+8,C'-'                                                    
         EDIT  (B1,SRLIN),(2,PLDATE+9),FILL=0                                   
PA4      DS    0H                                                               
***TEST***                                                                      
         CLI   SRBFD,C'T'       SEE IF TEST BUY                                 
         BNE   *+8                                                              
         MVI   PLDATE-1,C'T'                                                    
***TEST***                                                                      
         MVC   PLACT,SRACT                                                      
         MVC   PCSW,SRPCSW         PLANNED COSTS INDICATOR                      
*                                                                               
         LA    R0,ACCN                                                          
         LA    R1,WKTOTS                                                        
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         LA    R1,WKTOTS           ORDERED                                      
         CLI   PAYBUY,C'P'                                                      
         BNE   *+8                                                              
         LA    R1,WKTOTS+ACCHL         PAY                                      
*                                                                               
*        CHANGE IN GRS/NET/CD/GST/PST/PC GRS/PC NET/PC CD                       
*                                                                               
         MVC   0(ACCHL,R1),SRGRS                                                
         SP    00(6,R1),SRCPGRS                                                 
         SP    06(6,R1),SRCPNET                                                 
         SP    12(6,R1),SRCPCD                                                  
         SP    18(6,R1),SRCPGST                                                 
         SP    24(6,R1),SRCPGSTP                                                
         SP    30(6,R1),SRCPPST                                                 
         SP    36(6,R1),SRCPPSTP                                                
         SP    42(6,R1),SRCPCGRS                                                
         SP    48(6,R1),SRCPCNET                                                
         SP    54(6,R1),SRCPCCD                                                 
*                                                                               
PA6      DS    0H                                                               
*                                                                               
         LA    R1,WKTOTS                                                        
         BRAS  RE,PRTOTS                                                        
***TEST***                                                                      
         CLI   SRBFD,C'T'          SEE IF TEST BUY                              
         BNE   PA8                                                              
         LA    R0,ACCN                                                          
         LA    R1,WKTOTS                                                        
         BRAS  RE,CLRTOTS          DON'T ADD TEST BUYS TO TOTALS                
*                                  FOR BALANCING - ALL TEST DATA                
*                                  IS EXCLUDED                                  
         LA    R1,WKTOTS           RESET R1 FOR ROLTOTS                         
***TEST***                                                                      
PA8      BRAS  RE,ROLTOTS                                                       
         XIT1                                                                   
         SPACE 3                                                                
*                                  FORMAT $ AMOUNTS                             
PRTOTS   NTR1                                                                   
         LA    R6,P                                                             
         USING PLD,R6                                                           
         LR    R4,R1                                                            
*                                                                               
         ZAP   DUB,0(6,R4)         ORD GRS                                      
         LA    RF,PLOGRS                                                        
         BRAS  RE,PRTEDT                                                        
*                                                                               
         SP    DUB,12(6,R4)        LESS CD                                      
         LA    RF,PLOGLCD                                                       
         BRAS  RE,PRTEDT                                                        
*                                                                               
         ZAP   DUB,6(6,R4)         NET                                          
         SP    DUB,12(6,R4)        LESS CD                                      
         LA    RF,PLONLCD                                                       
         BRAS  RE,PRTEDT                                                        
*                                                                               
         ZAP   DUB,PGRDSP(6,R4)    PAID GROSS                                   
         SP    DUB,PCDDSP(6,R4)    LESS CD                                      
         LA    RF,PLPGLCD                                                       
         BRAS  RE,PRTEDT                                                        
*                                                                               
         ZAP   DUB,PNDSP(6,R4)     PAID NET                                     
         SP    DUB,PCDDSP(6,R4)    LESS CD                                      
         LA    RF,PLPNLCD                                                       
         BRAS  RE,PRTEDT                                                        
*                                                                               
         CP    PGDSP(6,R4),=P'0'     SEE IF I HAVE ANY PAID GST                 
         BE    PRTOT5                                                           
*                                                                               
         ZAP   DUB,PGDSP(6,R4)     PAID GST                                     
         LA    RF,PLGST                                                         
         MVI   PLGST+13,C'G'                                                    
         BRAS  RE,PRTEDT                                                        
*                                                                               
         BRAS  RE,RPRT                                                          
*                                                                               
PRTOT5   DS    0H                                                               
*                                                                               
         CP    PPDSP(6,R4),=P'0'    SEE IF I HAVE                               
         BE    PRTOT20                                                          
*                                                                               
         ZAP   DUB,PPDSP(6,R4)     PAID PST                                     
         LA    RF,PLGST                                                         
*                                                                               
         BRAS  RE,PRTEDT                                                        
*                                                                               
         MVI   PLGST+13,C'P'                                                    
*                                                                               
PRTOT20  DS    0H                                                               
*                                                                               
         CLI   QOPT7,C'P'          IF PRINTING PLANNED COSTS                    
         BNE   PRTOT30                                                          
*                                                                               
         CP    42(6,R4),=P'0'      IF NO PLANNED COSTS                          
         BNE   PRTOT22                                                          
*                                                                               
         CLI   PCSW,C'P'              CONTINUE IF BUY HAS PLANNED COSTS         
         BE    PRTOT22                                                          
*                                                                               
         B     PRTOT30                ELSE SKIP PRINTING                        
*                                                                               
PRTOT22  DS    0H                                                               
*                                                                               
         BRAS  RE,RPRT                PRINT CURRENT LINE                        
*                                                                               
         ZAP   DUB,42(6,R4)           PLANNED COSTS GRS                         
         LA    RF,PLOGRS                                                        
         BRAS  RE,PRTEDT                                                        
*                                                                               
         SP    DUB,54(6,R4)           LESS CD                                   
         LA    RF,PLOGLCD                                                       
         BRAS  RE,PRTEDT                                                        
*                                                                               
         ZAP   DUB,48(6,R4)           PLANNED COSTS NET                         
         SP    DUB,54(6,R4)           LESS CD                                   
         LA    RF,PLONLCD                                                       
         BRAS  RE,PRTEDT                                                        
*                                                                               
         MVI   PLOGRS+13,C'P'                                                   
         MVI   PLOGLCD+13,C'P'                                                  
         MVI   PLONLCD+13,C'P'                                                  
*                                                                               
PRTOT30  DS    0H                                                               
*                                                                               
PRTOTX   BRAS  RE,RPRT                                                          
         MVI   PCSW,0              RESET SWITCH                                 
         XIT1                                                                   
*                                                                               
         SPACE 3                                                                
PRTEDT   DS    0H                                                               
         EDIT  (P8,DUB),(14,X),2,COMMAS=YES,FLOAT=-                             
         CLI   X+3,C','         REMOVE MILLIONS COMMA                           
         BNE   PRTEDTX                                                          
         MVC   X+20(3),X                                                        
         MVC   X+1(3),X+20                                                      
PRTEDTX  DS    0H                                                               
         MVC   0(13,RF),X+1                                                     
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
***TEST***                                                                      
READEST  NTR1                    ROUTINE TO READ EST FROM                       
*                                SORTED RECORD                                  
         XC    KEY,KEY                                                          
         MVC   KEY(3),SRAGY        AGY/MED                                      
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(8),SRCLT     CLT/PRD/EST                                   
         CLC   KEY(13),PESTREC                                                  
         BE    READEX                                                           
         GOTO1 HIGH                                                             
         XC    PESTREC(250),PESTREC       CLEAR OLD                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   READEX                                                           
         GOTO1 GETEST                                                           
READEX   XIT1                                                                   
***TEST***                                                                      
         EJECT                                                                  
READCLT  NTR1                    ROUTINE TO READ CLT FROM                       
*                                SORTED RECORD                                  
         XC    KEY,KEY                                                          
         MVC   KEY(3),SRAGY        AGY/MED                                      
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),SRCLT                                                   
         CLC   KEY(10),PCLTREC                                                  
         BE    READCX                                                           
         GOTO1 HIGH                                                             
         XC    PCLTREC(250),PCLTREC       CLEAR                                 
         CLC   KEY(10),KEYSAVE                                                  
         BNE   READCX                     NOT FOUND - DON'T DIE                 
         GOTO1 GETCLI                                                           
READCX   XIT1                                                                   
                                                                                
***********************************************************************         
*                                                                     *         
*        POST TO BUCKET RECORD                                        *         
*                                                                     *         
***********************************************************************         
BUCKUP   NTR1                                                                   
*                                                                               
         CLC   SRAMTS(SRAMTSL),SRCPYS   IF SAME DOLLAR AMOUNTS                  
         BNE   *+10                                                             
         CLC   SRBLMO,SRCPBM            AND SAME BILL MONTH                     
         BNE   *+10                                                             
         CLC   SRINMO,SRCPIM            AND SAME INSERTION MONTH                
         BE    BUX                 NO ACTIVITY - SKIP UPDATING BUCKETS          
*                                                                               
*                                  TEMP LIST OF BUCKET AGYS                     
***TEST***                                                                      
         TM    PESTTEST,X'80'      SEE IF A TEST ESTIMATE                       
         BNZ   BU01                YES - UPDATE BUCKETS                         
         CLI   SRBFD,C'T'          SEE IF TEST BUY ON LIVE ESTIMATE             
         BE    BUX                 YES - DON'T UPDATE BUCKETS                   
***TEST***                                                                      
*                                                                               
*        IF QOPT6=B, UPDATE BUCKETS ONLY FOR AGENCIES                           
*              IN HARDCODED LIST                                                
*                                                                               
BU01     CLI   QOPT6,C'B'          SKIP IF OPTION NOT ON                        
         BNE   BU1D                                                             
*                                                                               
*        SEARCH LIST FOR AGENCY                                                 
*                                                                               
         LA    RF,BKTLST                                                        
BU01B    DS    0H                                                               
         CLI   0(RF),0                                                          
         BE    BUX                 NOT IN LIST                                  
         CLC   SRAGY,0(RF)                                                      
         BE    BU1D                                                             
         LA    RF,2(RF)                                                         
         B     BU01B                                                            
*                                                                               
BU1D     DS    0H                                                               
*                                                                               
*        DIFFERENCE BETWEEN OLD AND NEW AMOUNTS                                 
*                                                                               
         MVC   WKTOTS(ACCHL),SRGRS                                              
         SP    WKTOTS+00(6),SRCPGRS                                             
         SP    WKTOTS+06(6),SRCPNET                                             
         SP    WKTOTS+12(6),SRCPCD                                              
         SP    WKTOTS+18(6),SRCPGST                                             
         SP    WKTOTS+24(6),SRCPGSTP                                            
         SP    WKTOTS+30(6),SRCPPST                                             
         SP    WKTOTS+36(6),SRCPPSTP                                            
         SP    WKTOTS+42(6),SRCPCGRS                                            
         SP    WKTOTS+48(6),SRCPCNET                                            
         SP    WKTOTS+54(6),SRCPCCD                                             
*                                                                               
BU1F     DS    0H                                                               
*                                                                               
BU2      DS    0H                                                               
*                                                                               
*        FIND BUCKET RECORD                                                     
*                                                                               
         L     R6,ADBKT                                                         
         USING PBKREC,R6                                                        
*                                                                               
         MVI   ESTACT,C'Y'                                                      
*                                                                               
         XC    WORK(25),WORK       BUILD BUCKET KEY IN WORK                     
         MVC   WORK(3),SRAGY       AGY + MED                                    
         MVI   WORK+3,9                                                         
         MVC   WORK+4(8),SRCLT     CLT, PRD, EST                                
*                                                                               
         CLC   WORK(25),PBKREC     SKIP IF WE HAVE RECORD IN IOAREA             
         BE    BU4                                                              
*                                  CREATE NEW RECORD IN WORK AREA               
         XC    PBKREC(40),PBKREC                                                
         MVC   PBKREC(25),WORK     SET KEY                                      
         MVC   PBKLEN,=H'33'       MINIMUM RECORD LENGTH                        
*                             CREATE ELEMS IN WORK                              
         DROP  R6                                                               
*                             CREATE ELEMS IN WORK                              
BU4      DS    0H                                                               
*                                                                               
         XC    WORK(50),WORK                                                    
         LA    R3,WORK                                                          
         USING BKELEMD,R3          ESTABLISH BUCKET ELEMENT                     
*                                                                               
         MVI   BKELEM,X'21'        'TODAY' ELEM                                 
         MVI   BKELEM+1,46         LENGTH                                       
         MVC   BKDATE,BTODAY                                                    
*                                                                               
         LA    R6,WKTOTS                                                        
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
*                                                                               
         MVI   BKELEM,X'22'        'REGULAR' BILL MONTH                         
         MVC   BKYM,SRBLMO                                                      
         MVC   BKDATE,SRBLMO                                                    
         MVI   BKDATE+2,1          DAY=1                                        
*                                                                               
         OC    SRCPBM,SRCPBM                                                    
         BZ    BU4B                                                             
*                                                                               
         CLC   SRBLMO,SRCPBM       TEST BILL MONTH CHANGE                       
         BNE   BU5                 YES                                          
*                                                                               
BU4B     DS    0H                                                               
*                                                                               
         LA    R6,WKTOTS           NO - NORMAL POSTING                          
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
         B     BU6                                                              
*                                                                               
BU5      DS    0H                  BILL MONTH CHG                               
*                                                                               
         LA    R6,SRGRS            ADD TO NEW MONTH                             
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
         MVC   BKYM,SRCPBM         SUB FROM OLD MONTH                           
         MVC   BKDATE(2),SRCPBM                                                 
         LA    R6,SRCPGRS                                                       
         MVI   BYTE,C'S'                                                        
         BRAS  RE,FINDEL                                                        
*                                                                               
BU6      DS    0H                                                               
*                                                                               
         MVI   BKELEM,X'23'        'SPECIAL' - INSERTION MONTH                  
         MVC   BKYM,SRDATE         BUY DATE                                     
         MVC   BKDATE(2),SRDATE                                                 
         MVI   BKTYPE,C'I'                                                      
         MVI   BKDATE+2,1                                                       
*                                                                               
         OC    SRCPIM,SRCPIM                                                    
         BZ    BU6B                                                             
*                                                                               
         CLC   SRINMO,SRCPIM       TEST INS MONTH CHANGE                        
         BNE   BU7                 YES                                          
*                                                                               
BU6B     DS    0H                                                               
         LA    R6,WKTOTS           NO - NORMAL POST                             
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
         B     BU8                                                              
*                                                                               
BU7      DS    0H                  INS MONTH CHANGE                             
         LA    R6,SRGRS            ADD TO NEW MONTH                             
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
         MVC   BKYM,SRCPIM         SUB FROM OLD MONTH                           
         MVC   BKDATE(2),SRCPIM                                                 
         LA    R6,SRCPGRS                                                       
         MVI   BYTE,C'S'                                                        
         BRAS  RE,FINDEL                                                        
*                                                                               
BU8      DS    0H                                                               
*                                                                               
         XC    WORK(50),WORK                                                    
         LA    R3,WORK                                                          
         USING BKELEMD,R3                                                       
*                                                                               
         MVI   BKELEM,X'31'        'TODAY' ELEM WITH GST                        
         MVI   BKELEM+1,46                                                      
         MVC   BKDATE,BTODAY                                                    
         LA    R6,WKTOTS                                                        
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
*                                                                               
*        ADD PLANNED COSTS ELEMENTS                                             
*                                                                               
         CLI   PAYBUY,C'P'         SKIP IF FROM PAY PROGRAM                     
         BE    BUPCX                                                            
*                                                                               
         CP    WKTOTS+42(6),=P'0'  SKIP IF NO PLANNED COSTS                     
         BE    BUPCX                                                            
*                                                                               
         XC    WORK(50),WORK                                                    
         LA    R3,WORK                                                          
         USING BKELEMD,R3          ESTABLISH BUCKET ELEMENT                     
*                                                                               
         MVI   BKELEM,X'41'        'TODAY' ELEM                                 
         MVI   BKELEM+1,BKPGRS-BKELEM  LENGTH                                   
         MVC   BKDATE,BTODAY                                                    
*                                                                               
         LA    R6,WKTOTS+42                                                     
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
*                                                                               
         MVI   BKELEM,X'42'        'REGULAR' BILL MONTH                         
         MVC   BKYM,SRBLMO                                                      
         MVC   BKDATE,SRBLMO                                                    
         MVI   BKDATE+2,1          DAY=1                                        
*                                                                               
         OC    SRCPBM,SRCPBM                                                    
         BZ    BUPC4B                                                           
*                                                                               
         CLC   SRBLMO,SRCPBM       TEST BILL MONTH CHANGE                       
         BNE   BUPC5                 YES                                        
*                                                                               
BUPC4B   DS    0H                                                               
*                                                                               
         LA    R6,WKTOTS+42        NO - NORMAL POSTING                          
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
         B     BUPC6                                                            
*                                                                               
BUPC5    DS    0H                  BILL MONTH CHG                               
*                                                                               
         LA    R6,SRPCGRS          ADD TO NEW MONTH                             
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
         MVC   BKYM,SRCPBM         SUB FROM OLD MONTH                           
         MVC   BKDATE(2),SRCPBM                                                 
         LA    R6,SRCPCGRS                                                      
         MVI   BYTE,C'S'                                                        
         BRAS  RE,FINDEL                                                        
*                                                                               
BUPC6    DS    0H                                                               
*                                                                               
         MVI   BKELEM,X'43'        'SPECIAL' - INSERTION MONTH                  
         MVC   BKYM,SRDATE         BUY DATE                                     
         MVC   BKDATE(2),SRDATE                                                 
         MVI   BKTYPE,C'I'                                                      
         MVI   BKDATE+2,1                                                       
*                                                                               
         OC    SRCPIM,SRCPIM                                                    
         BZ    BUPC6B                                                           
*                                                                               
         CLC   SRINMO,SRCPIM       TEST INS MONTH CHANGE                        
         BNE   BUPC7                 YES                                        
*                                                                               
BUPC6B   DS    0H                                                               
         LA    R6,WKTOTS+42        NO - NORMAL POST                             
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
         B     BUPC8                                                            
*                                                                               
BUPC7    DS    0H                  INS MONTH CHANGE                             
         LA    R6,SRPCGRS          ADD TO NEW MONTH                             
         MVI   BYTE,C'A'                                                        
         BRAS  RE,FINDEL                                                        
         MVC   BKYM,SRCPIM         SUB FROM OLD MONTH                           
         MVC   BKDATE(2),SRCPIM                                                 
         LA    R6,SRCPCGRS                                                      
         MVI   BYTE,C'S'                                                        
         BRAS  RE,FINDEL                                                        
*                                                                               
BUPC8    DS    0H                                                               
*                                                                               
BUPCX    DS    0H                                                               
*                                                                               
BUX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*                        UPDATE ELEM IF PRESENT, ELSE ADD                       
FINDEL   NTR1                                                                   
         L     R2,ADBKT            POINT TO FIRST ELM IN BUCKET REC             
         LA    R2,33(R2)                                                        
*                                  FIND ELEMENT IN BUCKET RECORD                
FE2      DS    0H                                                               
         CLC   BKELEM(8),0(R2)                                                  
         BE    FE4                                                              
         BL    FE8                                                              
         CLI   0(R2),0             DONE AT END OF RECORD                        
         BE    FE8                                                              
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     FE2                                                              
*                                                                               
*                                  HAVE ELEM                                    
FE4      DS    0H                                                               
***TESTBUY***                                                                   
         TM    PESTTEST,X'80'        SEE IF DOING A TEST EST                    
         BZ    *+8                                                              
         MVI   BKIND-BKELEM(R2),X'01'                                           
***TESTBUY***                                                                   
         LA    R4,BKOGRS-BKELEM(R2)     ORDERED                                 
*                                                                               
         CLI   PAYBUY,C'P'         TEST PAY                                     
         BNE   *+8                                                              
         LA    R4,18(R4)        PAID DATA IN BUCKET                             
*                                                                               
         CLI   BYTE,C'A'           NORMAL POST                                  
         BNE   FE6                                                              
*                                                                               
         AP    00(6,R4),00(6,R6)   GROSS                                        
         AP    06(6,R4),06(6,R6)   NET                                          
         AP    12(6,R4),12(6,R6)   CD                                           
*                                                                               
         CLI   BKELEM,X'31'        SEE IF DOING TODAY WITH GST/PST              
         BNE   FEX                                                              
*                                                                               
         CLI   PAYBUY,C'P'         SEE IF PAY                                   
         BE    FE5                                                              
*                                                                               
         AP    00(6,R4),18(6,R6)   ADD GST TO GROSS                             
         AP    06(6,R4),18(6,R6)   ADD GST TO NET                               
         AP    00(6,R4),30(6,R6)   ADD PST TO GROSS                             
         AP    06(6,R4),30(6,R6)   ADD PST TO NET                               
*                                                                               
         B     FEX                                                              
*                                                                               
FE5      AP    00(6,R4),24(6,R6)   ADD PAID GST TO GROSS                        
         AP    06(6,R4),24(6,R6)   ADD PAID GST TO NET                          
         AP    00(6,R4),36(6,R6)   ADD PAID PST TO GROSS                        
         AP    06(6,R4),36(6,R6)   ADD PAID PST TO NET                          
         B     FEX                                                              
*                                                                               
FE6      DS    0H                                                               
         SP    00(6,R4),00(6,R6)                                                
         SP    06(6,R4),06(6,R6)                                                
         SP    12(6,R4),12(6,R6)                                                
*                                                                               
         CLI   BKELEM,X'31'        SEE IF DOING TODAY WITH GST/PST              
         BNE   FEX                                                              
         CLI   PAYBUY,C'P'          SEE IF PAY                                  
         BE    FE7                                                              
*                                                                               
         SP    00(6,R4),18(6,R6)   GST FROM GROSS                               
         SP    06(6,R4),18(6,R6)   GST FROM NET                                 
         SP    00(6,R4),30(6,R6)   PST FROM GROSS                               
         SP    06(6,R4),30(6,R6)   PST FROM NET                                 
         B     FEX                                                              
*                                                                               
FE7      SP    00(6,R4),24(6,R6)   PAID GST FROM GROSS                          
         SP    06(6,R4),24(6,R6)   PAID GST FROM NET                            
         SP    00(6,R4),36(6,R6)   PAID PST FROM GROSS                          
         SP    06(6,R4),36(6,R6)   PAID PST FROM NET                            
         B     FEX                                                              
*                                                                               
*        CREATING NEW ELEMENT                                                   
*                                                                               
FE8      DS    0H                  ADD ELM WITH 0 AMONTS                        
*                                  THEN TREAT AS A FOUND ELM IN RECORD          
         MVC   BKOGRS(36),=6PL6'0'                                              
*                                            ADD CLEARED ELEM                   
         GOTO1 RECUP,DMCB,(1,ADBKT),BKELEM,(C'R',(R2))                          
         CLI   8(R1),C'R'          TEST RECORD FULL                             
         BE    FE4                 NO - OK- ADD TO ELEM                         
*                                                                               
         MVC   P(26),=C'**BUCKET RECORD OVERFLOW**'                             
         MVC   X(56),P             SAVE ERROR MESSAGE                           
*                                                                               
         BRAS  RE,RPRT                                                          
*                                                                               
         TM    MSGFLAG,MSGOVRQ     SKIP IF EMAIL SENT ALREADY                   
         BO    FE8A                                                             
*                                                                               
         OI    MSGFLAG,MSGOVRQ     SSSSSSSSSSSSSSSSSFLAG                        
*                                                                               
         MVC   P(24),=C'AUTONOTE*KWAN,DEIS,SKUI:' SET RECIPIENT                 
         MVC   P+24(56),X          RETRIEVE ERROR MESSAGE                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(72,P) SEND EMAIL                         
*                                                                               
         MVC   P,SPACES            CLEAR PRINT LINE                             
*                                                                               
FE8A     DS    0H                                                               
*                                                                               
         GOTO1 ABKTPRT,DMCB,(C'P',ADBKT),P,X,DATCON,RPRTNTR                     
*                                                                               
FEX      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        ADD BUCKET DATA TO FILE                                                
         SPACE 2                                                                
BUCKPUT  NTR1                                                                   
         CLI   ESTACT,C'Y'                                                      
         BNE   BPX                                                              
         L     R6,ADBKT                                                         
         USING PBKREC,R6                                                        
*                                                                               
         MVC   KEY(25),PBKREC                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    BP4                                                              
*                                  ADD NEW BUCKET REC                           
         LA    RF,PBKREC                                                        
         ST    RF,AREC                                                          
         CLI   BKWOPT,C'Y'                                                      
         BNE   BP3                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   BP3                                                              
         GOTO1 ADDPRT                                                           
*                                                                               
BP3      DS    0H                                                               
         AP    RCADD,=P'1'                                                      
         L     R5,ADBKT                                                         
         B     BP28                                                             
*                                  UPDATE EXISTING RECORD                       
BP4      DS    0H                                                               
         L     R4,ALSTBKT                                                       
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
*                                  REMOVE EXISTING TODAY ELEMS                  
         LA    R2,33(R4)                                                        
         SR    R0,R0                                                            
BP5      DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    BP8                                                              
         CLI   0(R2),X'21'                                                      
         BE    BP7                                                              
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     BP5                                                              
*                                                                               
BP7      DS    0H                                                               
         GOTO1 RECUP,DMCB,(1,(R4)),(R2)                                         
         B     BP5                                                              
*                                                                               
BP8      DS    0H                                                               
*                                  REMOVE EXISTING PC TODAY ELEMS               
         LA    R2,33(R4)                                                        
         SR    R0,R0                                                            
BPPC5    DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    BPPC8                                                            
         CLI   0(R2),X'41'                                                      
         BE    BPPC7                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     BPPC5                                                            
*                                                                               
BPPC7    DS    0H                                                               
         GOTO1 RECUP,DMCB,(1,(R4)),(R2)                                         
         B     BPPC5                                                            
*                                                                               
BPPC8    DS    0H                  REMOVE EXISTING TODAY ELEMS                  
         LA    R2,33(R4)           WITH GST                                     
         SR    R0,R0                                                            
BP8C     DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    BP9                                                              
         CLI   0(R2),X'31'                                                      
         BE    BP8E                                                             
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     BP8C                                                             
*                                                                               
BP8E     DS    0H                                                               
         GOTO1 RECUP,DMCB,(1,(R4)),(R2)                                         
         B     BP8C                                                             
*                                                                               
BP9      DS    0H                                                               
         LA    R2,33(R4)                                                        
         L     R3,ADBKT                                                         
         LA    R3,33(R3)                                                        
*                                                                               
BP10     DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   *+10                                                             
         XC    0(10,R2),0(R2)                                                   
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         XC    0(10,R3),0(R3)                                                   
*                                  COMPARE OLD TO NEW                           
         CLC   0(8,R2),0(R3)                                                    
         BL    BP14                                                             
         BH    BP16                                                             
*                                  EQUAL ELEMS                                  
         CLI   0(R2),0                                                          
         BE    BP26                DONE                                         
         CLI   0(R2),X'21'         'TODAY'                                      
         BE    BP11                                                             
         CLI   0(R2),X'41'         'TODAY' WITH PLANNED COSTS                   
         BE    BP11                                                             
         CLI   0(R2),X'31'         'TODAY' WITH GST                             
         BNE   BP12                                                             
BP11     MVC   0(L'BKELEM,R2),0(R3)     REPLACE OLD ELEM                        
         B     BP13                                                             
*                                                                               
BP12     DS    0H                                                               
         LA    RE,BKOGRS-BKELEM(R2)                                             
         LA    RF,BKOGRS-BKELEM(R3)                                             
*                                                                               
*                                       ADD NEW TO OLD                          
         AP    00(6,RE),00(6,RF)                                                
         AP    06(6,RE),06(6,RF)                                                
         AP    12(6,RE),12(6,RF)                                                
*                                                                               
         CLI   1(R2),28            SKIP IF SHORT ELEMENT                        
         BNH   BP13                                                             
*                                                                               
         AP    18(6,RE),18(6,RF)                                                
         AP    24(6,RE),24(6,RF)                                                
         AP    30(6,RE),30(6,RF)                                                
*                                                                               
BP13     DS    0H                                                               
         BRAS  RE,NXTA                                                          
         BRAS  RE,NXTB                                                          
         B     BP10                                                             
*                                  OLD IS LOW - SKIP                            
BP14     DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    BP16                                                             
*                                                                               
         BRAS  RE,NXTA                                                          
         B     BP10                                                             
*                                  OLD IS HIGH - ADD NEW                        
BP16     DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    BP14                                                             
         GOTO1 RECUP,DMCB,(1,(R4)),(R3),(C'R',(R2))                             
         CLI   8(R1),C'R'          TEST RECORD FULL                             
         BE    BP25                NO - OK                                      
*                                                                               
         MVC   P(26),=C'**BUCKET RECORD OVERFLOW**'                             
         MVC   X(56),P             SAVE ERROR MESSAGE                           
*                                                                               
         BRAS  RE,RPRT                                                          
*                                                                               
         TM    MSGFLAG,MSGOVRQ     SKIP IF EMAIL SENT ALREADY                   
         BO    BP16A                                                            
*                                                                               
         OI    MSGFLAG,MSGOVRQ     SET SENT FLAG                                
*                                                                               
         MVC   P(24),=C'AUTONOTE*KWAN,DEIS,SKUI:' SET RECIPIENT                 
         MVC   P+24(56),X          RETRIEVE ERROR MESSAGE                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(72,P) SEND EMAIL                         
*                                                                               
         MVC   P,SPACES            CLEAR PRINT LINE                             
*                                                                               
BP16A    DS    0H                                                               
*                                                                               
         GOTO1 ABKTPRT,DMCB,(C'P',(R4)),P,X,DATCON,RPRTNTR                      
*                                                                               
         B     BPX                                                              
*                                                                               
BP25     DS    0H                                                               
         BRAS  RE,NXTB                  NEXT NEW                                
         B     BP10                                                             
BP26     DS    0H                                                               
         CLI   BKWOPT,C'Y'                                                      
         BNE   BP27                                                             
         CLI   RCWRITE,C'Y'                                                     
         BNE   BP27                                                             
         TM    KEY+25,X'80'        SEE IF DELETED                               
         BZ    BP26C                                                            
         NI    KEY+25,X'FF'        UNDELETE                                     
         GOTO1 WRT                                                              
*                                                                               
BP26C    DS    0H                                                               
         GOTO1 PUTPRT                                                           
*                                                                               
BP27     DS    0H                                                               
*                                                                               
         L     R5,ALSTBKT                                                       
*                                                                               
BP28     DS    0H                                                               
         ST    R5,FULL                                                          
         CLI   BKDOPT,C'Y'         SEE IF PDUMPING BUCKETS                      
         BNE   BP29                                                             
         BRAS  RE,DMPBUC                                                        
         BRAS  RE,RPRT             SKIP A LINE                                  
*                                                                               
BP29     L     R5,FULL                                                          
         LA    R2,RPRTNTR                                                       
         CLI   BKPOPT,C'Y'                                                      
         BE    *+6                                                              
         SR    R2,R2                                                            
*                                                                               
         GOTO1 ABKTPRT,DMCB,(C'P',(R5)),P,X,DATCON,(R2)                         
*                                                                               
         CLC   X(36),X+36          TEST BUCKET OK                               
         BE    BP30                                                             
*                                                                               
         MVC   P(28),=C'**UNBALANCED BUCKET RECORD**'                           
         MVC   X(56),P             SAVE ERROR MESSAGE                           
*                                                                               
         BRAS  RE,RPRT                                                          
*                                                                               
         TM    MSGFLAG,MSGUNBQ     SKIP IF EMAIL SENT ALREADY                   
         BO    BP29A                                                            
*                                                                               
         OI    MSGFLAG,MSGUNBQ     SET SENT FLAG                                
*                                                                               
         MVC   P(24),=C'AUTONOTE*KWAN,DEIS,SKUI:' SET RECIPIENT                 
         MVC   P+24(56),X          RETRIEVE ERROR MESSAGE                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(72,P) SEND EMAIL                         
*                                                                               
         MVC   P,SPACES            CLEAR PRINT LINE                             
*                                                                               
BP29A    DS    0H                                                               
*                                                                               
         CLI   BKPOPT,C'Y'                                                      
         BE    BP30                DONT PRINT TWICE                             
         GOTO1 ABKTPRT,DMCB,(C'P',(R5)),P,X,DATCON,RPRTNTR                      
*                                                                               
BP30     DS    0H                                                               
*                                                                               
BPX      DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
NXTA     DS    0H                                                               
         CLI   0(R2),0                                                          
         BER   RE                                                               
         CLI   1(R2),0                                                          
         BNE   *+10                                                             
         MVI   0(R2),0                                                          
         BR    RE                                                               
*                                                                               
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
NXTB     DS    0H                                                               
         CLI   0(R3),0                                                          
         BER   RE                                                               
         CLI   1(R3),0                                                          
         BNE   *+10                                                             
         MVI   0(R3),0                                                          
         BR    RE                                                               
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
RPRT     DS    0H                                                               
         LA    RF,RPRTNTR                                                       
RPRTNTR  NTR1  LABEL=*                                                          
         LM    R7,RC,SVREGS-RPRTNTR(RF)     RESTORE REGS                        
         J     RPRT2                                                            
SVREGS   DS    6F                                                               
*                                                                               
RPRT2    DS    0H                                                               
         MVC   HEAD1(5),=C'PRINT'                                               
         MVC   HEAD1+6(2),SYSNAID                                               
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
DMPREC   NTR1                                                                   
*                                  R5 POINTS TO RECOVERY RECORD                 
         LH    R2,0(R5)            LENGTH                                       
         B     DMPREC1                                                          
*                                                                               
DMPBUC   NTR1                                                                   
*                                                                               
         LH    R2,25(R5)            R5 POINTS TO BUCKET RECORD                  
*                                                                               
DMPREC1  LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CHI   R4,32                                                            
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BRAS  RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
         EJECT                                                                  
*                    ROUTINE TO SEE IF PP78(TRAFFIC TURNAROUND)                 
*                    IS REQUIRED                                                
*                    CHANGE IN DATE,SPACE,JOB,IC=COMS,PI=COMMS                  
*                                                                               
CHKTRAF  NTR1                                                                   
         L     R3,ARECVR                                                        
         USING RECD,R3                                                          
         LA    R6,RECVR                                                         
BUY      USING PBUYREC,R6                                                       
         MVI   TRAFSW,0                                                         
*                                                                               
         TM    BUY.PBDSTAT,X'20'   "NO TRAFFIC" BUY ?                           
         BO    CHKTX               YES - DO NOT DO 78                           
*                                                                               
         CLC   BUY.PBDBUYDT,RDATE  SEE IF NEW TODAY  * WAS BTODAY               
         BE    CHKT80               YES - DO 78                                 
*                                                                               
         CLC   BUY.PBDDATE,RDATE  SEE IF LAST CHG IS TODAY * WAS BTODAY         
         BNE   CHKTX                                                            
         TM    BUY.PBUYCNTL,X'80'  SEE IF DELETED TODAY                         
         BNZ   CHKT80                                                           
         TM    BUY.PBDDTIND,X'39'  CHKS UNITS,DESCRIPTION,DATE,IC=COM           
         BNZ   CHKT80                                                           
         TM    BUY.PBDDTIN2,X'89'  CHKS CLOSING,JOB,IO DATE                     
         BNZ   CHKT80                                                           
         TM    BUY.PBDDTIN3,X'47'  CHKS MADE LIVE,MAT CLOSING                   
         BNZ   CHKT80         POSITION INSTRUCTIONS,AD CODE ADDED               
*                                                                               
         TM    BUY.PBDDTIND,X'40'  RATE CHANGE (ONLY)                           
         BZ    CHKTX                                                            
         MVI   TRAFSW,X'02'    RATE CHG (ONLY) TRAF T/A                         
         B     CHKTX                                                            
*                                                                               
CHKT80   MVI   TRAFSW,X'10'                                                     
CHKTX    XIT1                                                                   
         DROP  R3                                                               
         DROP  BUY                                                              
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
CHKEST   NTR1                      SEE IF BUY T/A NEEDED                        
         L     R3,ARECVR                                                        
         USING RECD,R3                                                          
         LA    R6,RECVR                                                         
BUY      USING PBUYREC,R6                                                       
         MVI   ESTSW,0                                                          
         CLC   BUY.PBDBUYDT,RDATE   SEE IF NEW TODAY   * WAS BTODAY             
         BE    CHKE80               YES - DO 53                                 
*                                                                               
         CLC   BUY.PBDDATE,RDATE SEE IF LAST CHG IS TODAY  * WAS BTODAY         
         BNE   CHKEX                                                            
         TM    BUY.PBUYCNTL,X'80'  SEE IF DELETED TODAY                         
         BNZ   CHKE80                                                           
         CLI   BUY.PBDDTIND,0                                                   
         BNE   CHKE80                                                           
         CLI   BUY.PBDDTIN2,0                                                   
         BNE   CHKE80                                                           
         CLI   BUY.PBDDTIN3,0                                                   
         BE    CHKEX               IF ALL 3 ARE O - NO T/A                      
         CLI   BUY.PBDDTIN3,X'40'  SEE IF ONLY CHG IS JOB ADDED                 
         BNE   CHKE80              NO                                           
*                                                                               
*        MUST READ CLIENT TO GET OFFICE TO GET 53 PROFILE                       
         XC    PROF53,PROF53                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(7),BUY.PBUYREC                                               
         MVI   KEY+3,X'02'                                                      
         CLC   KEY(7),PCLTREC      SEE IF I ALREADY HAVE CLIENT                 
         BE    CHKE50                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CHKE80              CLIENT NOT ON FILE                           
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
CHKE50   XC    WORK(20),WORK                                                    
         MVC   WORK(4),=C'P053'                                                 
         MVC   WORK+4(3),BUY.PBUYKAGY AGY/MEDIA                                 
         MVC   WORK+7(3),BUY.PBUYKCLT                                           
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 GETPROF,DMCB,WORK,PROF53,DATAMGR                                 
         CLI   PROF53+12,C'N'        SEE IF GETING T/A FOR JOB ADDS             
         BE    CHKEX                 NO - THEN NO T/A                           
*                                                                               
CHKE80   MVI   ESTSW,1                                                          
CHKEX    XIT1                                                                   
         DROP  R3                                                               
         DROP  BUY                                                              
         SPACE 3                                                                
         EJECT                                                                  
CHKOID   NTR1                    ROUTINE TO SET ORIGIN ID                       
*                                FOR OFF-LINE ADDS                              
         L     R3,ARECVR                                                        
         USING RECD,R3                                                          
         LA    R6,RECVR                                                         
BUY      USING PBUYREC,R6                                                       
         LA    R1,OAGYTAB                                                       
CKOID3   CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    CKOIDX              AGY NOT FOUND CANN'T SET ID                  
         CLC   0(2,R1),BUY.PBUYKAGY                                             
         BE    CKOID5                                                           
         LA    R1,4(R1)                                                         
         B     CKOID3                                                           
*                                                                               
CKOID5   MVC   SRUSER,2(R1)        SET USER ID FROM OAGYTAB                     
*                                                                               
CKOIDX   XIT1                                                                   
         DROP  R3                                                               
         DROP  BUY                                                              
         EJECT                                                                  
IDTAB    DC    C'J',AL2(6195)      GROUP ID TABLE FOR P78 WI AGY TEST           
         DC    C'L',AL2(6196)                                                   
         DC    C'M',AL2(6198)           WAS 6197                                
         DC    X'FFFF'                                                          
*                                                                               
IDTABH7  DC    C'T',X'01',AL2(8972) GROUP ID TABLE FOR P78 H7 AGY TEST          
H7EQU    EQU   *-IDTABH7                                                        
         DC    C'T',X'02',AL2(8971)                                             
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
*        MINDSHARE TABLE FOR CLT/PRD REDIRECTS FOR P78'S                        
*                                                                               
H7PDTAB  DC    C'NCCADI',AL2(8967)        ID=MSCJPT                             
         DC    C'NCCBTF',AL2(8967)                                              
         DC    C'NCCMRS',AL2(8967)                                              
         DC    C'NCCPBZ',AL2(8967)                                              
         DC    C'NCCTUR',AL2(8967)                                              
         DC    C'NCCMMD',AL2(8967)                                              
         DC    C'NCCNIP',AL2(8967)                                              
         DC    C'NCCHD ',AL2(8967)                                              
         DC    C'NCCPUM',AL2(8967)                                              
*                                                                               
         DC    C'JOCCCM',AL2(8967)                                              
*                                                                               
         DC    C'NM GEN',AL2(8967)                                              
         DC    C'NM NMF',AL2(8967)                                              
********                                  SEARS ENTRIES                         
*                                                                               
*SMY*    DC    C'SR1FTB',AL2(10296)       MSSROC (TRAFFIC ID)                   
*SMY*    DC    C'SR1CFB',AL2(10296)                                             
*SMY*    DC    C'SR1KEB',AL2(10296)                                             
*SMY*    DC    C'SR1EVP',AL2(10296)                                             
*SMY*    DC    C'SR1GRP',AL2(10296)                                             
*                                                                               
*SMY*    DC    C'SR3CFB',AL2(10296)                                             
*SMY*    DC    C'SR3EVP',AL2(10296)                                             
*SMY*    DC    C'SR3HIP',AL2(10296)                                             
*SMY*    DC    C'SR3KEB',AL2(10296)                                             
*                                                                               
*SMY*    DC    C'SOHCOR',AL2(10296)                                             
***                                                                             
*SMY*    DC    C'SR1HPD',AL2(10297)       MSSRYC  (TRAFFIC ID)                  
*SMY*    DC    C'SR1RWP',AL2(10297)                                             
*SMY*    DC    C'SR1JAP',AL2(10297)                                             
*SMY*    DC    C'SR1FWP',AL2(10297)                                             
*SMY*    DC    C'SR1BSB',AL2(10297)                                             
*SMY*    DC    C'SR1CVT',AL2(10297)                                             
*SMY*    DC    C'SR1GCB',AL2(10297)     ADDED 09/04/02 - BPLA                   
*SMY*    DC    C'SR1SWP',AL2(10297)     ADDED 10/25/02 - SMYE                   
*                                                                               
*SMY*    DC    C'SR3BSB',AL2(10297)                                             
*SMY*    DC    C'SR3CVT',AL2(10297)                                             
*SMY*    DC    C'SR3GCB',AL2(10297)                                             
*SMY*    DC    C'SR3HPD',AL2(10297)                                             
*SMY*    DC    C'SR3JAP',AL2(10297)                                             
*SMY*    DC    C'SR3STP',AL2(10297)                                             
*SMY*    DC    C'SR3TGI',AL2(10297)                                             
*                                                                               
*SMY*    DC    C'SGRTGI',AL2(10297)                                             
         DC    X'FFFF'                                                          
*                                                                               
*        MINDSHARE TABLE FOR CLT REDIRECTS FOR P78'S                            
*                                                                               
H7CLTAB  DC    C'SR1',AL2(10297)       MSSRYC                                   
         DC    C'SR3',AL2(10297)                                                
         DC    C'SOH',AL2(10297)                                                
         DC    C'SGR',AL2(10297)                                                
         DC    X'FFFF'                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
         DS    0H                                                               
FORMTAB  DC    C'N',C'&&4AS  '          NEW                                     
         DC    C'L',C'&&CON  '          OLD                                     
         DC    C'P',C'1X1L8 '           ** ONLY FOR P13                         
         DC    X'FFFF'                                                          
*                                                                               
         DS    0H                                                               
CFORMTAB DC    C'NN',C'&&4AS  '          NEW                                    
         DC    C'LN',C'&&CON  '          OLD                                    
         DC    C'PN',C'1X1L8 '           ** ONLY FOR P13                        
*                                                                               
*        FAXING OUTPUTS                                                         
*                                                                               
         DC    C'NY',C'&&B4AKS'          NEW                                    
         DC    C'LY',C'&&BCKS '          OLD                                    
         DC    C'PY',C'BFX1L8'           ** ONLY FOR P13                        
         DC    X'FFFF'                                                          
*                                                                               
ACONS    DS    0F                                                               
         DC    A(AGYLST)                                                        
         DC    A(LSTBKT)                                                        
         DC    A(RECREC)                                                        
         DC    A(SAVREC)                                                        
         DC    V(GENIOS)                                                        
         DC    V(BUCKPRT)                                                       
         DC    A(BKTDMP)                                                        
         DC    A(BKTRSTR)                                                       
*                                                                               
ACONSX   EQU   *                                                                
*                                                                               
OAGYTAB  DS    0H         TABLE OF ORIGIN IDS FOR OFF-LINE ADDS                 
         DC    C'OM',H'1339'                                                    
         DC    X'FFFF'                                                          
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,          DOS SYS004                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04628,                                            X        
               MACRF=GM,                                               X        
               EODAD=RECVEOF                                                    
********************************   OLD BLKSIZE=08000                            
*                                                                               
*                                                                               
PBUYRQN  DCB   DDNAME=PBUYRQN,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00106,                                            X        
               BLKSIZE=02120,          DOS BLKSIZE=02120               X        
               MACRF=PM                                                         
         SPACE 3                                                                
         LTORG                                                                  
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         DROP  R7,R9                                                            
         TITLE 'PPREPF302 - EOD PROCESSING - SETVAL'                            
***********************************************************************         
*                                                                     *         
*        SET VALUES FOR COPY AND CHANGE (OR ADD)                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SETVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    PCSW,PCSW           INIT PCSW                                    
*                                                                               
         USING RECD,R3             ESTABLISH RECOVERY RECORD                    
*                                                                               
         CLI   THISPRD,0           FIRST TIME FOR REC                           
         BNE   SV4                 NO                                           
*                                                                               
*        INITIALIZATION FOR FIRST TIME TO BUY RECORD                            
*                                                                               
BUY      USING PBUYREC,R6                                                       
*                                                                               
         XC    CPYVALS,CPYVALS     CLEAR SAVED VALUES                           
         XC    CGSTTAX(8),CGSTTAX  CLEAR ORDERED AND PAID GST                   
         XC    CPSTTAX(8),CPSTTAX  CLEAR ORDERED AND PAID PST                   
         XC    CPPCVALS,CPPCVALS   CLEAR SAVED VALUES                           
*                                                                               
         XC    SVINMO(4),SVINMO    CLEAR SAVED INSERT & BILL MN                 
*                                                                               
         CLI   RRECTY,RRECTADD     SKIP IF ADDING BUY RECORD                    
         BE    SV4                                                              
*                                                                               
         L     R6,ASVREC           POINT TO COPY OF BUY RECORD                  
         LA    R6,RECVR-RECD(R6)   THAT IS RECOVERY RECORD                      
*                                                                               
         MVC   SVINMO,BUY.PBUYKDAT SAVE INSERTION MONTH                         
         MVC   SVBLMO,BUY.PBDBDATE SAVE BILLABLE  MONTH                         
         DROP  R6                                                               
*                                                                               
SV4      DS    0H                                                               
*                                                                               
         LA    R6,RECVR            POINT TO CURRENT RECOVERY BUY RECORD         
*                                                                               
         BRAS  RE,NXTPRD           FIND NEXT PRODUCT TO PROCESS                 
*                                                                               
         CLI   THISPRD,X'FF'       DONE AT END OF PRODUCTS                      
         BE    SETVALX                                                          
*                                                                               
         GOTO1 GETINS,DMCB,RECVR,GROSS,THISPRD,0,=C'PST'                        
*                                                                               
         L     R1,DMCB+16          POINT TO RETURNED GST/PST DATA               
         USING GVALUESD,R1                                                      
*                                                                               
         MVC   MGSTTAX(8),GSTTAX  SAVE RETURNED GST ORDERED AND PAID            
*                                                                               
*        SAVE PST DATA                                                          
*                                                                               
         XC    MPSTTAX(8),MPSTTAX  INIT PST AREA                                
*                                                                               
         LA    RE,PSTAREA                                                       
*                                                                               
SVPSTLP  CLI   0(RE),C' '          SKIP IF NO PST DATA                          
         BNH   SVPSTDN                                                          
*                                                                               
         L     R0,MPSTTAX          ACCUMULATE ORDER PST TAX                     
         A     R0,PSTTAX-PSTAREA(RE)                                            
         ST    R0,MPSTTAX                                                       
*                                                                               
         L     R0,MPSTTAXP         ACCUMULATE PAID GST TAX                      
         A     R0,PSTTAXPD-PSTAREA(RE)                                          
         ST    R0,MPSTTAXP                                                      
*                                                                               
SVPSTCN  DS    0H                                                               
         LA    RE,PSTAREAL(RE)                                                  
         B     SVPSTLP              GO DO NEXT PROVINCE                         
*                                                                               
         DROP  R1                                                               
*                                                                               
SVPSTDN  DS    0H                                                               
*                                                                               
*        GET PLANNED COST DATA                                                  
*                                                                               
         GOTO1 GETINS,DMCB,RECVR,(C'P',PCVALS),THISPRD,0,0                      
*                                                                               
         CLI   DMCB+4,C'X'         IF THERE ARE NO PLANNED COSTS                
         BE    *+12                                                             
         MVI   PCSW,C'P'           BUY HAS PLANNED COSTS                        
         B     *+10                                                             
         XC    PCVALS,PCVALS          CLEAR RESULTS                             
*                                                                               
         TM    RECVR+27,X'80'      IF A DELETED BUY RECORD                      
         BZ    SVDELN                                                           
*                                                                               
         XC    GROSS(20),GROSS        ZERO OUT ORDERED DOLLARS AND TAX          
         XC    MGSTTAX,MGSTTAX                                                  
         XC    MPSTTAX,MPSTTAX                                                  
         XC    PCVALS(20),PCVALS      CLEAR ORDERED PLANNED COSTS               
*                                                                               
SVDELN   DS    0H                                                               
*                                                                               
*        IF ADDING A NEW BUYREC M THE COPY OF A                                 
*              SKIP GETTING OLD VAUES FROM BUYREC'S SAVED COPY                  
*                                                                               
         CLI   RRECTY,RRECTADD    SKIP IF ADDING BUY RECORD                     
         BE    SVCPYX                                                           
*                                                                               
         CLI   RRECTY,RRECTCHG    IF A CHANGED BUY RECORD                       
         BNE   SVCPY                                                            
*                                                                               
         CLI   LASTBFD,C'T'       AND IF IT WAS ORIGINALLY A TEST BUY           
         BNE   SVCPY                                                            
         CLI   BUY.PBDBFD,C'T'    NOW BEING MADE LIVE                           
         BNE   SVCPYX                TREAT AS ADD                               
*                                                                               
*                                  ELSE TREAT AS CHANGE OF RECORD               
*                                                                               
SVCPY    L     R6,ASVREC           POINT TO SAVED COPY OF BUYREC                
         LA    R6,RECVR-RECD(R6)                                                
*                                  RETRIEVE BUY DOLLARS VIA GETINS              
         GOTO1 GETINS,DMCB,(R6),CPYVALS,THISPRD,0,=C'PST'                       
*                                                                               
         L     R1,DMCB+16          ESTABLISH RETURNED GST DATA                  
         USING GVALUESD,R1                                                      
*                                                                               
         MVC   CGSTTAX(8),GSTTAX   SAVE RETURNED GST  (ORD + PAID)              
*                                                                               
         XC    CPSTTAX(8),CPSTTAX   CLEAR ORDERED AND PAID PST                  
*                                                                               
*        ACCUMULATE PST DOLLARS FOR ALL PROVINCES                               
*                                                                               
         LA    RE,PSTAREA                                                       
*                                                                               
SVCPYPLP CLI   0(RE),C' '          DONE AT END OF PROVINCES                     
         BNH   SVCPYPDN                                                         
*                                                                               
         L     R0,CPSTTAX          ACCUMULATE ORDERED PST                       
         A     R0,PSTTAX-PSTAREA(RE)                                            
         ST    R0,CPSTTAX                                                       
*                                                                               
         L     R0,CPSTTAXP         ACCUMULATE PAID PST                          
         A     R0,PSTTAXPD-PSTAREA(RE)                                          
         ST    R0,CPSTTAXP                                                      
*                                                                               
SVCPYPCN DS    0H                                                               
         LA    RE,PSTAREAL(RE)                                                  
         B     SVCPYPLP             GO DO NEXT PROVINCE                         
*                                                                               
         DROP  R1                                                               
*                                                                               
SVCPYPDN DS    0H                                                               
*                                                                               
*        GET PLANNED COST DATA                                                  
*                                                                               
         GOTO1 GETINS,DMCB,(R6),(C'P',CPPCVALS),THISPRD,0,0                     
*                                                                               
         CLI   DMCB+4,C'X'         IF THERE ARE NO PLANNED COSTS                
         BE    *+12                                                             
         MVI   PCSW,C'P'           BUY HAS PLANNED COSTS                        
         B     *+10                                                             
         XC    CPPCVALS,CPPCVALS      CLEAR RESULTS                             
*                                                                               
         TM    27(R6),X'80'        IF COPY DELETED                              
         BZ    SVCPYX                                                           
*                                                                               
         XC    CPYVALS(20),CPYVALS    YES - CLEAR ORDERED                       
         XC    CGSTTAX,CGSTTAX        YES - CLEAR ORDERED GST ALSO              
         XC    CPSTTAX,CPSTTAX        YES - CLEAR ORDERED PST ALSO              
         XC    CPPCVALS(20),CPPCVALS  YES - CLEAR ORDERED PLANNED COSTS         
*                                                                               
SVCPYX   DS    0H                                                               
*                                                                               
SETVALX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPREPF302 - EOD PROCESSING - GET720P'                           
***********************************************************************         
*                                                                     *         
*                                 CHECKS 72 PROFILES AND RETURNS      *         
*                                 OUTPUT IN LAST72O                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GET72OP  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),=C'P0'                                                   
         MVC   WORK+2(2),=C'72'                                                 
         MVC   WORK+4(6),RQAGY      AGY/MED/CLT                                 
         CLC   LAST72(8),WORK+2                                                 
         BE    GET72X                                                           
         XC    LAST72O,LAST72O                                                  
         MVC   LAST72(8),WORK+2                                                 
         BRAS  RE,READCLT          MUST READ CLIENT TO GET OFFICE               
         CLI   PCLTOFF,C' '                                                     
         BNH   GET72C                                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
GET72C   DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,L72PROF,DATAMGR                                
         CLI   L72PROF+15,C' '                                                  
         BNH   GET72X                                                           
         CLI   L72PROF+15,C'*'                                                  
         BE    GET72X                                                           
         LARL  R4,FORMTAB                                                       
         MVC   WORK(1),L72PROF+15                                               
GET72G   CLC   0(2,R4),=X'FFFF'                                                 
         BE    GET72X             FORM NOT FOUND                                
         CLC   0(1,R4),WORK                                                     
         BE    GET72L                                                           
         LA    R4,7(R4)                                                         
         B     GET72G                                                           
*                                                                               
GET72L   MVC   LAST72O,1(R4)                                                    
         MVI   LAST72O+4,C'K'       KEEP                                        
*                                                                               
GET72X   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPREPF302 - EOD PROCESSING - GET72A'                            
***********************************************************************         
*                                                                     *         
*                                 CHECKS 72A PROFILES AND RETURNS     *         
*                                 OUTPUT IN L72PROFA                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GET72A   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(20),WORK                                                    
         MVC   WORK(4),=C'P72A'                                                 
         NI    WORK,X'BF'           SYSTEM LOWER CASE                           
         MVC   WORK+4(6),RQAGY      AGY/MED/CLT                                 
         CLC   LAST72A(8),WORK+2                                                
         BE    GET72AX                                                          
         MVC   LAST72A(8),WORK+2                                                
         BRAS  RE,READCLT          MUST READ CLIENT TO GET OFFICE               
         CLI   PCLTOFF,C' '                                                     
         BNH   GET72AC                                                          
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
GET72AC  DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,L72PROFA,DATAMGR                               
*                                                                               
GET72AX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPREPF302 - EOD PROCESSING - GET72A'                            
***********************************************************************         
*                                                                     *         
*                                 CHECKS 13 PROFILES AND RETURNS      *         
*                                 OUTPUT IN LAST130                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GET13OP  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),=C'P0'                                                   
         MVC   WORK+2(2),=C'13'                                                 
         MVC   WORK+4(6),RQAGY      AGY/MED/CLT                                 
         CLC   LAST13(8),WORK+2                                                 
         BE    GET13X                                                           
         XC    LAST13O,LAST13O                                                  
         MVC   LAST13(8),WORK+2                                                 
         BRAS  RE,READCLT          MUST READ CLIENT TO GET OFFICE               
         CLI   PCLTOFF,C' '                                                     
         BNH   GET13C                                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
GET13C   DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,L13PROF,DATAMGR                                
*                                                                               
*                         NOW READ 13A PROFILE TO SEE IF FAXING                 
*                                                                               
         MVC   WORK(4),=C'P13A'                                                 
         NI    WORK,X'BF'      MAKE SYSTEM LOWER CASE                           
         GOTO1 GETPROF,DMCB,WORK,L13PROFA,DATAMGR                               
*                                                                               
         CLI   L13PROF+15,C' '                                                  
         BNH   GET13X                                                           
         CLI   L13PROF+15,C'*'                                                  
         BE    GET13X                                                           
*                                                                               
         LARL  R4,CFORMTAB                                                      
         MVC   WORK(1),L13PROF+15                                               
         MVC   WORK+1(1),L13PROFA+2                                             
         OC    WORK(2),SPACES                                                   
         CLI   WORK+1,C'Y'                                                      
         BE    *+8                                                              
         MVI   WORK+1,C'N'        DEFAULT TO N                                  
*                                                                               
GET13G   CLC   0(2,R4),=X'FFFF'                                                 
         BE    GET13X             FORM NOT FOUND                                
         CLC   0(2,R4),WORK                                                     
         BE    GET13L                                                           
         LA    R4,8(R4)                                                         
         B     GET13G                                                           
*                                                                               
GET13L   MVC   LAST13O,2(R4)                                                    
*                                                                               
GET13X   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPREPF302 - EOD PROCESSING - GET150P'                           
***********************************************************************         
*                                                                     *         
*                                 CHECKS 15 PROFILES AND RETURNS      *         
*                                 OUTPUT IN LAST150                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GET15OP  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),=C'P0'                                                   
         MVC   WORK+2(2),=C'15'                                                 
         MVC   WORK+4(6),RQAGY      AGY/MED/CLT                                 
         CLC   LAST15(8),WORK+2                                                 
         BE    GET15X                                                           
         XC    LAST15O,LAST15O                                                  
         MVC   LAST15(8),WORK+2                                                 
         BRAS  RE,READCLT          MUST READ CLIENT TO GET OFFICE               
         CLI   PCLTOFF,C' '                                                     
         BNH   GET15C                                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
GET15C   DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,L15PROF,DATAMGR                                
*                                                                               
*                         NOW READ 15A PROFILE TO SEE IF FAXING                 
*                                                                               
         MVC   WORK(4),=C'P15A'                                                 
         NI    WORK,X'BF'      MAKE SYSTEM LOWER CASE                           
         GOTO1 GETPROF,DMCB,WORK,L15PROFA,DATAMGR                               
*                                                                               
         CLI   L15PROF+15,C' '                                                  
         BNH   GET15X                                                           
         CLI   L15PROF+15,C'*'                                                  
         BE    GET15X                                                           
*                                                                               
         LARL  R4,CFORMTAB                                                      
         MVC   WORK(1),L15PROF+15                                               
         MVC   WORK+1(1),L15PROFA+2                                             
         OC    WORK(2),SPACES                                                   
         CLI   WORK+1,C'Y'                                                      
         BE    *+8                                                              
         MVI   WORK+1,C'N'        DEFAULT TO N                                  
*                                                                               
GET15G   CLC   0(2,R4),=X'FFFF'                                                 
         BE    GET15X             FORM NOT FOUND                                
         CLC   0(2,R4),WORK                                                     
         BE    GET15L                                                           
         LA    R4,8(R4)                                                         
         B     GET15G                                                           
*                                                                               
GET15L   MVC   LAST15O,2(R4)                                                    
*                                                                               
GET15X   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPREPF302 - EOD PROCESSING - GET780P'                           
***********************************************************************         
*                                                                     *         
*                                 CHECKS 78 PROFILES AND RETURNS      *         
*                                 OUTPUT IN L78PROF                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GET78OP  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),=C'P0'                                                   
         MVC   WORK+2(2),=C'78'                                                 
         MVC   WORK+4(6),RQAGY      AGY/MED/CLT                                 
         CLC   LAST78(8),WORK+2                                                 
         BE    GET78X                                                           
         MVC   LAST78(8),WORK+2                                                 
         BRAS  RE,READCLT          MUST READ CLIENT TO GET OFFICE               
         CLI   PCLTOFF,C' '                                                     
         BNH   GET78C                                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
GET78C   DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,L78PROF,DATAMGR                                
GET78X   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPREPF302 - EOD PROCESSING - GETTT0P'                           
***********************************************************************         
*                                                                     *         
*                                 CHECKS TT PROFILES AND RETURNS      *         
*                                 OUTPUT IN LTTPROF                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETTTOP  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),=C'P0'                                                   
         MVC   WORK+2(2),=C'TT'                                                 
         MVC   WORK+4(6),RQAGY      AGY/MED/CLT                                 
         CLC   LASTTT(8),WORK+2                                                 
         BE    GETTTX                                                           
         MVC   LASTTT(8),WORK+2                                                 
         BRAS  RE,READCLT          MUST READ CLIENT TO GET OFFICE               
         CLI   PCLTOFF,C' '                                                     
         BNH   GETTTC                                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
GETTTC   DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,LTTPROF,DATAMGR                                
GETTTX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPREPF302 - EOD PROCESSING - GET530P'                           
***********************************************************************         
*                                                                     *         
*                                 CHECKS 53 PROFILES AND RETURNS      *         
*                                 OUTPUT IN L53PROF                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                 IT IN L53PROF                                 
GET53OP  NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),=C'P0'                                                   
         MVC   WORK+2(2),=C'53'                                                 
         MVC   WORK+4(6),RQAGY      AGY/MED/CLT                                 
         CLC   LAST53(8),WORK+2                                                 
         BE    GET53X                                                           
         MVC   LAST53(8),WORK+2                                                 
         BRAS  RE,READCLT          MUST READ CLIENT TO GET OFFICE               
         CLI   PCLTOFF,C' '                                                     
         BNH   GET53C                                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
GET53C   DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,L53PROF,DATAMGR                                
GET53X   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPREPF302 - EOD PROCESSING - NXTPRD'                            
***********************************************************************         
*                                                                     *         
*        FIND NEXT PRODUCT IN BUY                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NXTPRD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECD,R3             ESTABLISH RECOVERY RECORD                    
*                                                                               
         CLI   THISPRD,0           IF FIRST TIME TO ROUTINE                     
         BNE   NP20                                                             
*                                                                               
*        BUILD LIST OF PRODUCTS IN BUY                                          
*                                                                               
         MVC   PRDLST(3),BUY.PBUYKPRD ASSUME NON-POOL BUY                       
         MVI   PRDLST+3,X'C0'         AND USE BUY KEY PRD AS 1ST                
         MVI   PRDLST+4,X'FF'         SET END OF LIST MARKER                    
*                                                                               
         LA    R4,PRDLST              INIT A(NXTPRD) TO FIRST IN LIST           
         ST    R4,ANPRD                                                         
*                                                                               
         CLC   BUY.PBUYKPRD,=C'ZZZ' SKIP IF NOT A ZZZ BUY                       
         BNE   NPZZZX                                                           
*                                                                               
*        BUILD PRDLST FOR ZZZ BUY                                               
*                                                                               
         LA    R4,PRDLST           INITIALIZE PRDLST TO NO ENTRIES              
         MVI   0(R4),X'FF'                                                      
*                                                                               
         MVI   PRDLST+3,0          ZERO FROM COPY/CHANGE INDICATOR              
*                                                                               
         MVI   ELCODE,X'21'        SET FOR ZZZ PRODUCT ELEM                     
*                                                                               
         CLI   RRECTY,RRECTADD     SKIP IF ADD  RECOVERY RECORD                 
         BE    NPCPYX                                                           
*                                                                               
         L     R2,ASVREC           START WITH COPY OF CURRENT RECORD            
         LA    R2,RECVR-RECD(R2)   START OF BUYRECORD                           
         LA    R2,PBDELEM-PBUYREC(R2) FIRST ELEM IN BUY RECORD                  
*                                                                               
NPCPYLP  DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND NEXT PRODUCT ELM                        
         BNE   NPCPYDN             DONE IF NO MORE IN RECORD                    
*                                                                               
         MVC   0(3,R4),2(R2)       ADD PRODUCT TO LIST                          
         MVI   3(R4),X'80'         SET FROM COPY                                
         MVI   4(R4),X'FF'         SET EOL                                      
*                                                                               
NPCPYCN  DS    0H                                                               
         LA    R4,4(R4)            BUMP TO NEXT SLOT IN LIST                    
         B     NPCPYLP                                                          
*                                                                               
NPCPYDN  DS    0H                                                               
*                                                                               
NPCPYX   DS    0H                                                               
*                                  NOW DO CHANGE RECORD                         
         LA    R2,RECVR+33         POINT TO FIRST ELM IN CHANGE RECORD          
*                                                                               
NPCHGLP  DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           FIND 1ST/NEXT PRODUCT ELM                    
         BNE   NPCHGDN             END OF ELEMENTS                              
*                                                                               
         LA    R4,PRDLST           SEE IF IT IS ALREADY IN LIST                 
*                                                                               
NPCHKLP  DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF LIST                          
         BE    NPCHKDN                                                          
*                                                                               
         CLC   0(3,R4),2(R2)       CHECK IF IN LIST ALREADY                     
         BE    NPCHKFD             YES                                          
*                                                                               
NPCHKCN  DS    0H                  NO - BUMP TO NEXT ENTRY IN LIST              
*                                                                               
         LA    R4,4(R4)                                                         
         B     NPCHKLP                                                          
*                                                                               
NPCHKFD  DS    0H                  PRODUCT IN LIST FROM COPY OF BUY             
*                                                                               
         OI    3(R4),X'40'         SET ALSO FROM CHANGE RECORD                  
         B     NPCHKX                                                           
*                                                                               
NPCHKDN  DS    0H                  ADD PRODUCT TO END OF LIST                   
*                                                                               
         MVC   0(3,R4),2(R2)       PRODUCT TO LIST                              
         MVI   3(R4),X'40'         SET FROM CHA                                 
         MVI   4(R4),X'FF'         SET EOL                                      
*                                                                               
NPCHKX   DS    0H                                                               
*                                                                               
NPCHGCN  DS    0H                                                               
         B     NPCHGLP             HANDLE NEXT PRODUCT IN RECORD                
*                                                                               
NPCHGDN  DS    0H                                                               
*                                  INIT A(NEXT PRODUCT IN LIST)                 
         LA    R4,PRDLST              TO FIRST IN LIST                          
         ST    R4,ANPRD                                                         
         B     NPZZZX                                                           
*                                                                               
*        SECOND OF LATER CALL TO ROUTINE                                        
*                                                                               
NP20     DS    0H                                                               
         L     R4,ANPRD            BUMP TO NEXT PRODUCT IN LIST                 
         LA    R4,4(R4)                                                         
         ST    R4,ANPRD                                                         
*                                                                               
NPZZZX   DS    0H                                                               
*                                                                               
         MVC   THISPRD,0(R4)       RETURN NEXT PRODUCT IN LIST                  
*                                                                               
NPXT     DS    0H                                                               
         XIT1                                                                   
         DROP  BUY                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 3                                                                
         EJECT                                                                  
*                             BUCKET DUMP                                       
         SPACE 3                                                                
BKTDMP   NMOD1 0,BKTDMP                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         ZAP   BKTDCNT,=P'0'                                                    
*                                                                               
         OPEN  (BKTOUT,(OUTPUT))                                                
*                                                                               
         XC    KEY,KEY                                                          
BD3      DS    0H                                                               
         GOTO1 HIGH                                                             
         B     BD4B                                                             
*                                                                               
BD4      DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
BD4B     DS    0H                                                               
         CLI   KEY,X'FF'           EOF                                          
         BE    BD10                                                             
         TM    DMCB+8,X'80'                                                     
         BNZ   BD10                                                             
         CLI   KEY+3,9                                                          
         BL    BD6                                                              
         BH    BD8                                                              
*                                  HAVE BUCKET RECORD                           
         GOTO1 GETBKT                                                           
         L     R4,ADBKT                                                         
         SHI   R4,8                                                             
         MVC   4(4,R4),KEY+27      DISK ADDR                                    
         SR    RF,RF                                                            
         ICM   RF,3,25+8(R4)         REC LEN                                    
         LA    RF,8(RF)            + 4 FOR DA                                   
         STH   RF,0(R4)                                                         
         LA    R1,BKTOUT                                                        
         LR    R0,R4                                                            
         PUT   (1),(0)                                                          
         AP    BKTDCNT,=P'1'                                                    
         B     BD4                                                              
*                                                                               
BD6      DS    0H                                                               
         MVC   KEY+3(2),=X'0900'                                                
         B     BD3                                                              
*                                                                               
BD8      DS    0H                                                               
         LLC   RF,KEY+2            BUMP TO NEXT A/M                             
         LA    RF,1(RF)                                                         
         STC   RF,KEY+2                                                         
         B     BD6                                                              
*                                                                               
BD10     DS    0H                                                               
         CLOSE (BKTOUT,)                                                        
*                                                                               
         EDIT  (P6,BKTDCNT),(9,BDCOUNT)                                         
         GOTO1 LOGIO,DMCB,1,(30,BDMSG)                                          
*                                                                               
BDX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
BDMSG    DC    C'PRINT BUCKETS DUMPED '                                         
BDCOUNT  DC    CL9' '                                                           
         SPACE 2                                                                
BKTOUT   DCB   DDNAME=BKTOUT,          DOS SYS008                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=03000,                                            X        
               BLKSIZE=08000,          DOS BLKSIZE=08000               X        
               MACRF=PM                                                         
*                                                                               
         LTORG                                                                  
                                                                                
********************************************************************            
*        BUCKET RESTORE                                                         
********************************************************************            
BKTRSTR  NMOD1 0,BKTRSTR                                                        
         LA    RC,SPACEND                                                       
         ZAP   BKTRCNT,=P'0'                                                    
         OPEN  (BKTIN,(INPUT))                                                  
*                                                                               
BR2      GOTO1 LOGIO,DMCB,1,(37,BRMSG1)                                         
         GOTO1 (RF),(R1),,(15,BRMSG2)                                           
         GOTO1 (RF),(R1),0,(4,BRANS)                                            
         OC    BRANS,SPACES                                                     
         CLC   =C'GO',BRANS                                                     
         BE    BR4                                                              
         CLC   =C'STOP',BRANS                                                   
         BE    BRXIT                                                            
         B     BR2                                                              
*                                                                               
BR4      LA    R1,BKTIN                                                         
         L     R0,ALSTBKT                                                       
         GET   (1),(0)                                                          
*                                                                               
         L     R5,ALSTBKT                                                       
         MVC   KEY+27(4),4(R5)     DISK ADDR                                    
         GOTO1 GETBKT                                                           
         CLI   DMCB+8,0            ERROR ON GET - WILL BE TRACED                
         BNE   BR4                                                              
         L     R6,ADBKT                                                         
         CLC   8(25,R5),0(R6)      TEST RECORDS HAVE SAME KEY                   
         BE    BR8                                                              
*                                                                               
         MVC   P(34),=C'BKT RESTORE ERROR - XXXXXXXX FILE='                     
         GOTO1 HEXOUT,DMCB,KEY+27,P+20,4,=C'N'                                  
         GOTO1 (RF),(R1),(R6),P+35,25                                           
         MVC   PSECOND+29(5),=C'TAPE='                                          
         GOTO1 (RF),(R1),8(R5),PSECOND+35,25                                    
*                                                                               
         MVC   X(56),P             SAVE ERROR MESSAGE                           
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         TM    MSGFLAG,MSGBKTQ     SKIP IF EMAIL SENT ALREADY                   
         BO    BR4A                                                             
*                                                                               
         OI    MSGFLAG,MSGBKTQ     SET MESSAGE SENT FLAG                        
*                                                                               
         MVC   P(24),=C'AUTONOTE*KWAN,DEIS,SKUI:' SET RECIPIENT                 
         MVC   P+24(56),X          RETRIEVE ERROR MESSAGE                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(72,P) SEND EMAIL                         
*                                                                               
         MVC   P,SPACES            CLEAR PRINT LINE                             
*                                                                               
BR4A     DS    0H                                                               
*                                                                               
         B     BR4                                                              
*                                                                               
BR8      DS    0H                                                               
         LA    R5,8(R5)                                                         
         ST    R5,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   BR9                                                              
         GOTO1 PUTPRT                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                ERROR ON PUT MEANS TROUBLE                   
*                                                                               
BR9      DS    0H                                                               
         AP    BKTRCNT,=P'1'                                                    
         B     BR4                                                              
*                                                                               
BREOF    DS    0H                                                               
         CLOSE (BKTIN,)                                                         
         EDIT  (P6,BKTRCNT),(9,BRCOUNT)                                         
         GOTO1 LOGIO,DMCB,1,(32,BRMSG3)                                         
*                                                                               
BRXIT    DS    0H                                                               
         XIT1                                                                   
BRMSG1   DC    C'ABOUT TO RESTORE PRINT BUCKET RECORDS'                         
BRMSG2   DC    C'TYPE,GO OR STOP'                                               
BRMSG3   DC    C'PRINT BUCKETS RESTORED '                                       
BRCOUNT  DC    CL9' '                                                           
BRANS    DC    C'    '                                                          
*                                                                               
BKTIN    DCB   DDNAME=BKTIN,           DOS SYS008                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=03000,                                            X        
               MACRF=GM,                                               X        
               EODAD=BREOF                                                      
         LTORG                                                                  
********************************************************************            
* SORT DSECT                                                                    
********************************************************************            
SRKEYD   DSECT                                                                  
SRKEY    DS    0CL25                                                            
SRKEYR   DS    0CL22                                                            
SRTYPE   DS    CL1                 A=ANALYSIS,P=PAYMENT                         
*                                  C=CONTRACT,R=AUTO SPACE RESV                 
SRAGY    DS    CL2                                                              
SRMED    DS    CL1                                                              
SRCLT    DS    CL3                                                              
SRPRD    DS    CL3                COULD BE SLAVE CLT FOR SRTYPE=R               
*                                 AUTO-SPACE RESV                               
SREST    DS    XL2                                                              
SRPUB    DS    XL6                                                              
SRDATE   DS    XL3                                                              
SRLIN    DS    XL1                                                              
SRSEQ    DS    XL3                                                              
*                                                                               
SRRDATE  DS    XL3                 ACTIVITY DATE (RDATE)                        
SRSIN    DS    XL3                                                              
SRUSER   DS    XL2                                                              
SRPRG    DS    XL1                 PROGRAM - 03=PAY,11=BUY,1E=SRR               
*                                                                               
SRAMTS   DS    0X                  AMOUNTS PLUS MTHS                            
SRGRS    DS    PL6                                                              
SRNET    DS    PL6                                                              
SRCD     DS    PL6                                                              
SRGST    DS    PL6                 GST - NEW 12/20/90                           
SRGSTP   DS    PL6                 GST PD  - NEW 1/2/91                         
SRPST    DS    PL6                 PST - ADDED 4/27/94                          
SRPSTP   DS    PL6                 PST PAID - ADDED 4/27/94                     
SRPCGRS  DS    PL6                 PLANNED COST GROSS                           
SRPCNET  DS    PL6                 PLANNED COST NET                             
SRPCCD   DS    PL6                 PLANNED COST CD                              
SRAMTSL  EQU   *-SRAMTS            LENGTH OF CURRENT $ BUCKETS                  
*                                                                               
SRBLMO   DS    XL2                                                              
SRINMO   DS    XL2                                                              
*                                                                               
SRCPYS   DS    0C                  COPY VALUES - AMOUNTS + MTHS                 
SRCPGRS  DS    PL6                                                              
SRCPNET  DS    PL6                                                              
SRCPCD   DS    PL6                                                              
SRCPGST  DS    PL6                 GST - NEW 12/20/90                           
SRCPGSTP DS    PL6                 GST PD  - NEW 1/2/91                         
SRCPPST  DS    PL6                 PST - ADDED 4/27/94                          
SRCPPSTP DS    PL6                 PST PD - ADDED 4/27/94                       
SRCPCGRS DS    PL6                 PLANNED COST GROSS                           
SRCPCNET DS    PL6                 PLANNED COST NET                             
SRCPCCD  DS    PL6                 PLANNED COST CD                              
*                                                                               
SRCPBM   DS    XL2                                                              
SRCPIM   DS    XL2                                                              
*                                                                               
SRCPYSL  EQU   *-SRCPYS            LENGTH OF COPY VALUES                        
*                                                                               
SRRQS    DS    X                   REQS NEEDED - X'80' =BUY,X'40' = IO          
*                                  X'20' = ZZZ  X'01' = TEARSHEET               
*                                  X'10' = TRAFFIC (P78)                        
*                                  X'02' = TRAFFIC RATE CHG ONLY                
SRACT    DS    C                                                                
SRBUYER  DS    CL3                 BUYER                                        
SRBFD    DS    CL1            TO SAVE TEST STATUS OF BUY                        
*                                                                               
SRPCSW   DS    XL1                 C'P' - PLANNED COST BUY                      
*                             FOR P RECORDS                                     
         ORG   SRCLT                                                            
SRPPUB   DS    XL6                                                              
SRPCLT   DS    CL3                                                              
SRPSIN   DS    XL3                                                              
SRPPRD   DS    CL3                                                              
SRPEST   DS    XL2                                                              
SRPLIN   DS    XL1                                                              
*                                                                               
         SPACE 3                                                                
*                                                                               
PBKRECD  DSECT                                                                  
       ++INCLUDE PBKREC                                                         
         SPACE 2                                                                
*                                                                               
BKELEMD  DSECT                                                                  
       ++INCLUDE DDBKELEM                                                       
*                                                                               
PTSHTELD DSECT                                                                  
       ++INCLUDE PTSHTEL                                                        
*                                                                               
AGYTBD   DSECT                                                                  
AGYMED   DS    CL3                                                              
AGTTOTS  DS    XL120              20 X PL6 = 120                                
AGYSTAT  DS    CL1                                                              
AGYTNAM  DS    CL33                                                             
AGYPROF  DS    CL30                                                             
AGYTBEL  EQU   *-AGYTBD                                                         
*                                                                               
****************************************************                            
*        RECOVERY RECORD AREA                                                   
****************************************************                            
RECD     DSECT                                                                  
RECLN    DS    XL2                                                              
         DS    CL2                                                              
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RECVR    DS    0H                                                               
*                                                                               
**************************************************************                  
*              DSECT FOR ACTIVITY PRINT LINE                                    
**************************************************************                  
PLD      DSECT                                                                  
         DS    0CL110                                                           
PLAGY    DS    CL2                                                              
         DS    CL1                                                              
PLMED    DS    CL1                                                              
         DS    CL1                                                              
PLCLT    DS    CL3                                                              
         DS    CL1                                                              
PLPRD    DS    CL3                                                              
         DS    CL1                    WILL BE 'T' FOR TEST ESTS                 
PLEST    DS    CL3                                                              
         DS    CL1                                                              
PLPUB    DS    CL15                                                             
         DS    CL1                    WILL BE 'T' FOR TEST BUYS                 
PLDATE   DS    CL11                                                             
PLACT    DS    C                                                                
*                                                                               
PLOGRS   DS    CL13                                                             
PLOGLCD  DS    CL13                                                             
PLONLCD  DS    CL13                                                             
PLPGLCD  DS    CL13                                                             
PLPNLCD  DS    CL13                                                             
PLGST    DS    CL13               GST/PST                                       
*                                                                               
         EJECT                                                                  
PPF3WK   DSECT                                                                  
AAGYLST  DS    A                                                                
ALSTBKT  DS    A                                                                
ARECVR   DS    A                                                                
ASVREC   DS    A                                                                
AGENIOS  DS    A                                                                
ABKTPRT  DS    A                                                                
ABKTDMP  DS    A                                                                
ABKTRSTR DS    A                                                                
*                                                                               
FRST     DS    X                                                                
ESTACT   DS    X                                                                
TESTER   DS    X                                                                
*                                                                               
MSGFLAG  DS    XL1                 ERROR MESSAGES EMAILED                       
MSGCPYQ  EQU   X'80'                 STAND ALONE COPY                           
MSGBKTQ  EQU   X'40'                 BUCKET RESTORE ERROR                       
MSGINVQ  EQU   X'20'                 INVALID RECORD TYPE                        
MSGCHGQ  EQU   X'10'                 CHANGE WIOTHOUT COPY                       
MSGOVRQ  EQU   X'08'                 BUCKET RECORD OVERFLOW                     
MSGUNBQ  EQU   X'04'                 UNBALANCED BUCKET RECORD                   
*                                                                               
RCADD    DS    PL6                                                              
RCPUT    DS    PL6                                                              
BKTDCNT  DS    PL6                                                              
BKTRCNT  DS    PL6                                                              
SVOPTS   DS    0CL6      SAVED QOPT FIELDS                                      
RQPOPT   DS    CL1       QOPT1: DUMP REQUEST CARDS IN HEX                       
RQWOPT   DS    CL1       QOPT2: WRITE TO PBUYRQN                                
BKPOPT   DS    CL1       QOPT3: PRINT BUCKETS TO SYSPRINT                       
BKWOPT   DS    CL1       QOPT4: WRITE BUCKETS TO DISK                           
BKDOPT   DS    CL1       QOPT5: DUMP BUCKETS OPTION                             
*                        Y= PDUMP, D= DUMP TO TAPE, R= RESTORE                  
         DS    CL1       QOPT6                                                  
BSPARS   DS    6F                                                               
ANPRD    DS    F                                                                
*                                                                               
LAST72   DS    CL8       AGY/MED/CLT OF LAST 72 PROFILE READ                    
LAST72O  DS    CL6       OUTPUT TYPE FOR LAST 72 PROFILE READ                   
L72PROF  DS    CL16      LAST 72 PROFILE                                        
*                                                                               
LAST72A  DS    CL8       AGY/MED/CLT OF LAST 72A PROFILE READ                   
L72PROFA DS    CL16      LAST 72A PROFILE                                       
*                                                                               
LAST13   DS    CL8       AGY/MED/CLT OF LAST 13 PROFILE READ                    
LAST13O  DS    CL6       OUTPUT TYPE FOR LAST 13 PROFILE READ                   
L13PROF  DS    CL16      LAST 13 PROFILE                                        
L13PROFA DS    CL16      LAST 13A PROFILE                                       
*                                                                               
LAST15   DS    CL8       AGY/MED/CLT OF LAST 15 PROFILE READ                    
LAST15O  DS    CL6       OUTPUT TYPE FOR LAST 15 PROFILE READ                   
L15PROF  DS    CL16      LAST 15 PROFILE                                        
L15PROFA DS    CL16      LAST 15A PROFILE                                       
*                                                                               
LAST78   DS    CL8       AGY/MED/CLT OF LAST 78 PROFILE READ                    
L78PROF  DS    CL16      LAST 78 PROFILE                                        
*                                                                               
LASTTT   DS    CL8       AGY/MED/CLT OF LAST TT PROFILE READ                    
LTTPROF  DS    CL16      LAST TT PROFILE                                        
*                                                                               
LAST53   DS    CL8       AGY/MED/CLT OF LAST 53 PROFILE READ                    
L53PROF  DS    CL16      LAST 53 PROFILE                                        
*                                                                               
THISPRD  DS    CL3                                                              
PRDLST   DS    XL100                                                            
LASTTYP  DS    X                                                                
LASTBFD  DS    CL1                                                              
ELCODE   DS    X                                                                
         DS    0F                                                               
MGSTTAX  DS    XL4    RETURNED FROM GETINS - ORDERED GST                        
MGSTTAXP DS    XL4    RETURNED FROM GETINS - PAID GST                           
*                                                                               
MPSTTAX  DS    XL4    RETURNED FROM GETINS - ORDERED PST                        
MPSTTAXP DS    XL4    RETURNED FROM GETINS - PAID PST                           
*                                                                               
MPCVAL   DS    0XL12  RETURNED PLANNED COST DATA FROM GETINS                    
MPCGRS   DS    XL4    RETURNED PLANNED COST - GROSS                             
MPCAGCM  DS    XL4    RETURNED PLANNED COST - AGENCY COMMISSION                 
MPCCD    DS    XL4    RETURNED PLANNED COST - CASH DISCOUNT                     
*                                                                               
CPYVALS  DS    XL96     COPY VALUES                                             
*                                                                               
CGSTTAX  DS    XL4      COPY GST ORDERED                                        
CGSTTAXP DS    XL4      COPY GST PAID                                           
*                                                                               
CPSTTAX  DS    XL4      COPY PST ORDERED                                        
CPSTTAXP DS    XL4      COPY PST PAID                                           
*                                                                               
SVINMO   DS    XL2                                                              
SVBLMO   DS    XL2                                                              
*                                                                               
PROF53   DS    CL16     FOR PP53 REPORT PROFILE                                 
*                                                                               
X        DS    XL200                                                            
BTODAY   DS    XL3      BINARY                                                  
PTODAY   DS    XL3      PACKED                                                  
TODAY    DS    CL6                                                              
TRAFSW   DS    CL1                                                              
ESTSW    DS    CL1                                                              
*                                                                               
*                                                                               
REQHDR   DS    0CL26                                                            
*                                                                               
       ++INCLUDE DMREQHDR                                                       
*                                                                               
RQCRD    DS    0CL80                                                            
RQPROG   DS    CL2                                                              
RQAGY    DS    CL2                                                              
RQMED    DS    CL1                                                              
RQCLT    DS    CL3                                                              
RQDIV    DS    CL3                                                              
RQPRD    DS    CL3                                                              
         DS    CL6                                                              
RQEST    DS    CL3                                                              
         DS    CL3                                                              
RQPUB    DS    CL11                                                             
RQDATES  DS    CL12                                                             
         DS    CL8                                                              
RQCNTLD  DS    CL3          CONTROL DATE  (COL 58)                              
         DS    CL8                                                              
RQSTR    DS    CL12                                                             
*                                                                               
PCSW     DS    XL1                 C'P' - BUY HAS PLANNED COSTS                 
*                                                                               
THISKEY  DS    CL200                                                            
LSTKEY   DS    CL200                                                            
SEQNO    DS    F                                                                
*                                                                               
*                          ACCUMLATOR FORMAT                                    
*                          ORDERED 10 X PL6                                     
*                                  GROSS,NET,CD,GST,PD GST,PST,PD PST           
*                                  PC GROSS,PC NET,PC CD                        
*                          PAID  10 X PL6                                       
*                                  GROSS,NET,CD,GST,PD GST,PST,PD PST           
*                                  PC GROSS,PC NET,PC CD                        
*                                                                               
WKTOTS   DS    20PL6                                                            
*                                                                               
ESTTOTS  DS    20PL6                                                            
PRDTOTS  DS    20PL6                                                            
CLTTOTS  DS    20PL6                                                            
MEDTOTS  DS    20PL6                                                            
AGYTOTS  DS    20PL6                                                            
RUNTOTS  DS    20PL6                                                            
*                                                                               
ACCN     EQU   20                                                               
ACCL     EQU   120         20 X PL6 =120                                        
ACCHL    EQU   60          10 X PL6 =60     60 ORDERED, 60 PAID                 
ACCTN    EQU   120         6  X 20  =120    TOTAL ACCUMS                        
*                                                                               
PGRDSP   EQU   ACCHL+0      PAID GROSS                                          
PNDSP    EQU   ACCHL+6      PAID NET                                            
PCDDSP   EQU   ACCHL+12     PAID CD                                             
PGDSP    EQU   ACCHL+24     PAID GST                                            
PPDSP    EQU   ACCHL+36     PAID PST                                            
PPCGDSP  EQU   ACCHL+42     PAID PC GROSS                                       
PPCNDSP  EQU   ACCHL+48     PAID PC NET                                         
PPCCDDSP EQU   ACCHL+54     PAID PC CD                                          
*                                                                               
RQSW     DS    X                                                                
PAYCHSW  DS    X                                                                
PAYBUY   DS    C                                                                
USER53   DS    XL2         SAVED ID FOR BUY P53 T/A'S                           
DATE53   DS    XL3         EARLIEST ACTIVITY DATE FOR P53 T/A'S                 
*                                                                               
PCVALS   DS    XL96        RETURN AREA FOR PLANNED COST                         
*                             CALL TO GETINS                                    
*                             USE PVALUES AS DSECT                              
CPPCVALS DS    XL96        RETURN AREA FOR COPY PLANNED COST                    
*                             CALL TO GETINS                                    
*                             USE PVALUES AS DSECT                              
PPF3WKL  EQU   *-PPF3WK            LENGTH OF WORKAREA                           
         SPACE 3                                                                
PPF302   CSECT                                                                  
RECREC   DS    4200C                                                            
SAVREC   DS    4200C                                                            
AGYLST   DS    0F                                                               
         ORG   *+300*AGYTBEL                                                    
LSTBKT   DS    4200C                                                            
*                                                                               
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PGENGRP                                                        
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
PASRELD  DSECT                                                                  
       ++INCLUDE PASRELEM                                                       
*                                                                               
GVALUESD DSECT                                                                  
       ++INCLUDE GVALUES                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'185PPREPF302 09/10/19'                                      
         END                                                                    
