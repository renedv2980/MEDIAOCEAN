*          DATA SET ACBIL06    AT LEVEL 010 AS OF 12/17/12                      
*PHASE T60E06A                                                                  
         TITLE 'ACBIL06 - CREATIVE BILLING - ALLOCATE'                          
ACBIL06  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**BIL6**,RA,CLEAR=YES                                  
*                                                                               
         USING GWS,R9                                                           
         USING TWAD,R8                                                          
         USING LWSD,RC                                                          
*                                                                               
HD1      USING LINED,R4            MAP  HEADER    LINE 1                        
HD2      USING LINED,R5            MAP  HEADER    LINE 2                        
         LA    R4,ALCHEDH          ->   HDR  LINE 1                             
         LA    R5,ALCHED2H         ->   HDR  LINE 2                             
         OI    ALCHEDH+6,FVOXMT    TRANSMIT  HEADERS                            
         OI    ALCHED2H+6,FVOXMT                                                
         OI    ALCHEDAH+6,FVOXMT                                                
*                                                                               
*                                  TRANSLATE LOCAL DICTIONARY ITEMS             
         GOTO1 VDICTAT,DMCB,C'LL  ',DICTB06,DICLS06                             
*                                                                               
         MVC   HD2.LINWC(3),AC$WC2                W/C                           
*                                                                               
         MVC   ALCHEDA,AC$ALCT2                   ALLOCATED                     
*                                                                               
         LA    R4,1(,R4)           ADJUST    FOR                                
         LA    R5,1(,R5)           .    TWO  BYTE WC                            
*                                                                               
         MVC   HD2.LINSUP(8),AC$SUPC              SUPPLIER                      
*                                                                               
         MVC   HD1.LININV(L'LININV+1),AC$7INV     INVOICE                       
         MVC   HD2.LININV(L'LININV),AC$NUM        NUMBER                        
*                                                                               
         MVC   HD1.LINDT(L'LINDT),AC$INV          INVOICE                       
         MVC   HD2.LINDT(L'LINDT),AC$DATE         DATE                          
*                                                                               
*                                                 BILLABLE                      
         MVC   HD1.LINHRS(L'LINHRS+1+L'LINRTE+1+L'LINBIL),AC$BLB                
         MVC   HD2.LINHRS(L'LINHRS),AC$HOURS      HOURS                         
         MVC   HD2.LINRTE(L'LINRTE),AC$RATE       RATE                          
         MVC   HD2.LINBIL(L'LINBIL),AC$AMT        AMOUNT                        
*                                                                               
*                                  CHANGE    SPACES    IN   'BILLABLE'          
*                                       TO   C'-'                               
         LA    R2,HD1.LINHRS                                                    
         LA    R3,L'LINHRS+1+L'LINRTE+1+L'LINBIL                                
*                                                                               
INIT10   CLI   0(R2),C' '          SPACE ?                                      
         BNE   *+8                 NO,  SKIP                                    
         MVI   0(R2),C'-'          MAKE C'-'                                    
         LA    R2,1(,R2)           ->   NEXT CHARACTER                          
         BCT   R3,INIT10           TEST NEXT CHARACTER                          
*                                                                               
*                                  INITIALIZE PACKED FIELDS                     
         ZAP   TRNCSD,=P'0'                                                     
         ZAP   TRNNET,=P'0'                                                     
         ZAP   BILRTE,=P'0'                                                     
         ZAP   BILCSD,=P'0'                                                     
         ZAP   BILNET,=P'0'                                                     
         ZAP   ALLCSD,=P'0'                                                     
         ZAP   ALLNET,=P'0'                                                     
*                                                                               
*        B     INIT40              CONTINUE                                     
         DROP  HD1,HD2                                                          
         EJECT ,                                                                
         SPACE 1                                                                
         USING OVRTWAD,R7          MAP  OVRTWA                                  
         SPACE 1                                                                
INIT40   LA    R7,OVRTWA           OVERLAY   SAVED     W/S                      
         CLI   ANYKEY,C'Y'         DID  KEY  CHANGE ?                           
         BNE   INIT50              NO,  SKIP                                    
         BAS   RE,INITOTWA         INITIALIZE     OVRTWA                        
*                                                                               
INIT50   MVC   ALCTOT,SPACES                                                    
         OI    ALCTOTH+6,FVOXMT    TRANSMIT                                     
*                                                                               
         OI    BILSRVH+1,X'01'     MODIFIED                                     
         OI    BILSRVH+6,FVOXMT    TRANSMIT                                     
         MVI   UPDSW,C'N'          ASSUME    NO   ALLOCATION    CHANGE          
         CLI   ACTION,REV          IF   ACTION    IS   REVERSE                  
         BNE   *+8                                                              
         MVI   ALLCSW,C'Y'         ONLY DISPLAY   REVERSED  ITEMS               
*                                                                               
         CLI   OVMODE,DSPLY                                                     
         BE    DSP02                                                            
         CLI   OVMODE,MARK                                                      
         BE    UPD00                                                            
*                                                                               
         CLI   ACTION,REV          REVERSING?                                   
         BNE   INIT60              NO                                           
         MVI   OVMODE,REV          YES, SET MODE                                
         ZAP   SCIUNET,=P'0'       CLEAR SCIEL BUCKETS                          
         ZAP   SCIUCOM,=P'0'                                                    
         BAS   RE,RDMK             GET NEGATIVE ALLOCATIONS                     
*                                                                               
INIT60   MVI   OVMODE,INIT                                                      
         B     DSP00                                                            
         EJECT ,                                                                
         SPACE 1                                                                
DSP00    GOTO1 ABLDCHR             BUILD TABLE OF CHARGES                       
         XC    LASTWC,LASTWC                                                    
         ZAP   TOTALLWC,=P'0'                                                   
         ZAP   TOTBILWC,=P'0'                                                   
*                                                                               
         OC    LASTKEY,LASTKEY                                                  
         BNZ   DSP02                                                            
*                                                                               
         CLC   FLTAMT(4),=X'00000000'                                           
         BE    DSP02               IF "AMOUNT=$$$', JOB MUST HAVE               
         CP    TOTALL,=P'0'            NO ALLOCATED CHARGES TO PROCESS          
         BNE   ZEROCH                                                           
*                                                                               
         BAS   RE,GETTOT           GET  TOTAL NET FOR JOB                       
         ZAP   DUB,AMTOT           MUST USE  DUB HERE !                         
         BZ    DSP02               SKIP REST IF AMOUNT ZERO                     
*                                                                               
         ICM   R1,15,FLTAMT        GET AMOUNT TO ALLOCATE                       
         CVD   R1,DUB1                                                          
         CP    DUB1,DUB            IS AMOUNT < BILLABLE CHARGES?                
         BH    NUM2BIG             YES, PUT OUT AN ERROR MESSAGE                
*                                                                               
* ALLOCATED AMOUNT IS DIVIDED BY NET TOTAL OF JOB GENERATING A                  
* PERCENTAGE TO BE USED IN A LATER ROUTINE                                      
*                                                                               
         XC    DUB4,DUB4                                                        
         CVD   R1,DUB4+8           CONVERT AMOUNT TO ALLOCATE                   
         SRP   DUB4,4,5            MULTIPLY BY 10000 & ROUND                    
         DP    DUB4,DUB(8)         DIVIDE BY THE NET TOTAL                      
         ZAP   PERCENT,DUB4(8)     RESULT IS PERCENT OF TOTAL                   
*                                                                               
DSP02    MVC   KEY,SPACES                                                       
         MVC   KEY(15),JOBKEY                                                   
         OC    LASTKEY,LASTKEY     MAY BE STARTING FROM SAVED KEY               
         BZ    DSP04                                                            
         MVC   KEY,LASTKEY                                                      
*                                                                               
         USING LINED,R4                                                         
         USING KEYTABD,R6                                                       
DSP04    LA    R4,ALCFRSTH                                                      
         LA    R6,KEYTAB                                                        
         LA    R0,MAXLNS                                                        
*                                                                               
DSP06    NI    LINDH+1,X'FF'-X'09'   TURN-OFF HIGH/MODIFIED                     
         NI    LINAMTH+1,X'FF'-X'29' TURN-OFF PROTECTED/HIGH/MODIFIED           
         LA    R4,LINLNQ(,R4)                                                   
         XC    0(KEYTABLQ,R6),0(R6)  CLEAR KEYTAB                               
         ZAP   KEYTBBIL,=P'0'                                                   
         ZAP   KEYTBALC,=P'0'                                                   
         LA    R6,KEYTABLQ(,R6)                                                 
         BCT   R0,DSP06                                                         
*                                                                               
         TWAXC ALCFRSTH,ALCTABH,PROT=Y                                          
         MVI   LINES,0                                                          
*                                                                               
         USING LINED,R4                                                         
         USING KEYTABD,R6                                                       
         LA    R4,ALCFRSTH                                                      
         LA    R6,KEYTAB                                                        
         GOTO1 ARDHI,AIOAREA1                                                   
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
DSP08    L     R2,AIOAREA1                                                      
         CLC   JOBKEY,TRNKCULA     SAME JOB?                                    
         BNE   DSP90               NO - DO END OF ACCOUNT STUFF                 
*                                  YES - BILLED?                                
         OC    ACCOUSED(2,R2),ACCOUSED(R2)                                      
         BNZ   DSP30               YES - SKIP IT                                
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION ELEMENT                     
         LA    R3,ACCORFST(,R2)    ->   TRANSACTION ELEMENT                     
         CLI   0(R3),TRNELQ        X'44' - TRANSACTION ELEMENT ?                
         BNE   DSP30                                                            
         TM    TRNSTAT,TRNSHOLD    SKIP HELD ITEMS                              
         BO    DSP30                                                            
*                                                                               
         TM    TRNRSTAT,TRNSDRFT   SKIP DRAFTS                                  
         BO    DSP30                                                            
         TM    TRNRSTAT,TRNSREVS   SKIP REVERSALS                               
         BO    DSP30                                                            
         CLI   TRNTYPE,57          SKIP WRITEOFFS                               
         BE    DSP30                                                            
         DROP  R3                                                               
*                                                                               
         CLC   TRNKWORK,=C'99'     SKIP BILLS                                   
         BE    DSP30                                                            
         CLC   TRNKWORK,=C'**'     SKIP ORDERS                                  
         BE    DSP30                                                            
         CLC   TRNKCCPY,TRNKCPY    SAME COMPANY ?                               
         BNE   DSP30               MUST HAVE SAME COMPANY CODE                  
*                                  IN CONTRA AND ACCT TO DISPLAY ITEM           
*                                                                               
         GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         BAS   RE,GETALL           GET TOTALS FOR ALLOCATE                      
         BNE   DSP30               SKIP IT IF IT'S NON-BILLABLE TIME            
         BAS   RE,FILTRN           FILTER THE TRANSACTION                       
         BNE   DSP30               SKIP IT                                      
*                                                                               
         CLI   LINES,MAXLNS        SCREEN FULL?                                 
         BE    DSP60               YES                                          
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION ELEMENT                     
         LA    R3,ACCORFST(,R2)                                                 
*                                                                               
         OC    LASTWC,LASTWC       FIRST TIME?                                  
         BZ    DSP10               YES                                          
         CLC   LASTWC,TRNKWORK                                                  
         BE    DSP10                                                            
         BAS   RE,DSPWCT           SET-UP WORK CODE TOTAL LINE                  
         CLI   LINES,MAXLNS        SCREEN FULL?                                 
         BE    DSP60               YES                                          
*                                                                               
DSP10    MVC   LASTWC,TRNKWORK     SAVE LAST WORKCODE                           
         CLI   UNALSW,C'Y'         MAKE BILLABLE ALLOCATED ?                    
         BNE   DSP12               NO                                           
         CP    ALLNET,=P'0'        YES, ANYTHING ALLOCATED ?                    
         BNE   DSP12               YES                                          
         ZAP   ALLNET,BILNET       NO,  MAKE BILLABLE ALLOCATED                 
         ZAP   ALLCSD,BILCSD                                                    
         MVC   ALLHRS,BILHRS                                                    
*                                                                               
         AP    TOTALL,ALLNET       UPDATE ALLOCATED TOTAL                       
*                                                                               
DSP12    LA    R2,TRNKWORK                                                      
         BAS   RE,WKCUP            GET  ENTRY IN ALLOCATED TABLE                
         DROP  R2                                                               
*                                                                               
         USING ALLTD,R5                                                         
         AP    ALLTAMT,ALLNET                                                   
         AP    TOTALLWC,ALLNET                                                  
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         L     R2,AIOAREA1                                                      
         CLI   SUMMSW,C'Y'         SUPPRESS DETAIL ?                            
         BE    DSP16               YES                                          
         MVC   LINWC,TRNKWORK      FORMAT THE DETAIL LINE                       
         MVC   LINSUP,TRNKULC                                                   
         MVC   LININV,TRNREF                                                    
         MVI   LINSTA,C' '                                                      
         DROP  R2                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(1,TRNDATE),(8,LINDT)                               
*                                                                               
         OC    BILHRS,BILHRS       ANY HOURS?                                   
         BZ    DSP14               NO                                           
         EXTED (B4,BILHRS),LINHRS,2,FLOAT=-                                     
         EXTED BILRTE,LINRTE,2,FLOAT=-                                          
*                                                                               
DSP14    EXTED BILNET,LINBIL,2,FLOAT=-                                          
*                                                                               
DSP16    AP    WRKTOT,BILNET       ADD  TO WORK CODE TOTAL                      
         CLI   SUMMSW,C'Y'         SUPPRESS DETAIL ?                            
         BE    DSP30               YES                                          
*                                                                               
         CLI   UNALSW,C'N'         ALLOCATE BILLABLE ?                          
         BNE   DSP18               YES                                          
         CP    ALLNET,=P'0'        ANYTHING ALLOCATED ?                         
         BNE   DSP18               YES                                          
         MVI   LINAMT,C'N'         NO,  LEAVE AS UNALLOCATED                    
         B     DSP28                                                            
*                                                                               
DSP18    CLC   FLTAMT(4),=X'00000000'   WAS  AMOUNT SPECIFIED?                  
         BE    DSP24                    NO                                      
*                                                                               
* THE BILLABLE AMOUNT IS MULTIPLIED BY THE PERCENTAGE DETERMINED                
* AT THE BEGINNING.                                                             
         ZAP   DUB,BILNET               BILLABLE AMOUNT                         
*                                                                               
         ZAP   DUB4(16),DUB(8)          TRANSFER THE BILNET                     
         MP    DUB4(16),PERCENT+3(5)    MULTIPLY BY PERCENTAGE                  
         SRP   DUB4(16),64-4,5          DIVIDE BY 10000 AND ROUND               
*                                                                               
         CP    BILRTE,=P'0'        DO   WE HAVE A RATE ?                        
         BNE   DSP20               YES                                          
         ZAP   DUB,DUB4+8(8)                                                    
         B     DSP22                                                            
*                                                                               
DSP20    ZAP   DUB,BILRTE          CONVERT THE RATE                             
         MP    DUB4,=P'10000'      MULTIPLY CALCULATED AMOUNT                   
         DP    DUB4,DUB+4(4)       DIVIDE BY RATE                               
         ZAP   DUB,DUB4(12)        MOVE AMOUNT                                  
         DP    DUB,=P'25'          ROUND TO NEAREST 1/4 HR                      
         SRP   DUB(6),64-2,5                                                    
         MP    DUB(6),=P'25'                                                    
         ZAP   DUB4,DUB(6)                                                      
*                                                                               
         ZAP   DUB,BILRTE          GET  HOURLY RATE AGAIN                       
         MP    DUB4,DUB+3(5)                                                    
         SRP   DUB4,64-2,5                                                      
         ZAP   DUB,DUB4                                                         
         CP    DUB,=P'0'           GET NEW ALLOCATED AMOUNT                     
         BNE   DSP22                                                            
         MVI   LINAMT,C'N'                                                      
*                                                                               
DSP22    ZAP   ALLNET,DUB          STORE CALCULATED AMOUNT                      
         AP    TOTALL,DUB                                                       
         LA    R2,LINWC                                                         
         BAS   RE,WKCUP                                                         
*                                                                               
         USING ALLTD,R5                                                         
         AP    ALLTAMT,DUB                                                      
         AP    TOTALLWC,DUB                                                     
*                                                                               
DSP24    CP    ALLNET,=P'0'                                                     
         BE    DSP26                                                            
         EXTED ALLNET,LINAMT,2,FLOAT=-                                          
         B     DSP28                                                            
*                                                                               
DSP26    OC    ALLHRS,ALLHRS       IF NO AMOUNT SHOW HOURS                      
         BZ    DSP28                                                            
         MVI   LINAMT,C'H'         DISPLAY HOURS                                
         EXTED (B4,ALLHRS),(L'LINAMT-1,LINAMT+1),2,FLOAT=-                      
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
DSP28    L     R2,AIOAREA1                                                      
         OI    KEYTBSW,KEYTBINU    IN   USE                                     
         MVC   KEYTBWC,LASTWC      WORK CODE                                    
         MVC   KEYTBDAT,TRNKDATE   SAVE DATE                                    
         MVC   KEYTBSBR,TRNKSBR    SUB  REFERENCE                               
         ZAP   KEYTBBIL,BILNET     BILLABLE                                     
         ZAP   KEYTBALC,ALLNET     ALLOCATED IN TABLE                           
         OI    LINAMTH+4,X'20'     TURN ON VALIDATED BIT                        
         LA    R6,KEYTABLQ(,R6)    NEXT ENTRY                                   
         LA    R4,LINLNQ(,R4)      GET  ADDRESS OF NEXT SCREEN LINE             
         ZIC   R0,LINES                                                         
         AH    R0,=H'1'                                                         
         STC   R0,LINES            COUNT LINES                                  
*                                                                               
DSP30    GOTO1 ASEQ,AIOAREA1                                                    
         B     DSP08                                                            
         DROP  R2,R3,R4,R5,R6                                                   
         EJECT ,                                                                
***********************************************************************         
* ROUTINE FOR END OF SCREEN                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         SPACE 1                                                                
DSP60    MVC   LASTKEY,TRNKEY      SAVE KEY OF NEXT                             
         CLI   SUMMSW,C'Y'                                                      
         BNE   DSP94                                                            
*                                  TOTALS DISPLAYED -                           
         MVC   FVMSGNO,=AL2(2102)    DEPRESS ENTER FOR NEXT                     
         MVI   FVMTYPE,FVMINFO                                                  
         MVI   OVMODE,DSPLY        NEXT TIME CONTINUE DISPLAY                   
         B     DSP100                                                           
*                                                                               
*              ROUTINE FOR END OF ACCOUNT                                       
*                                                                               
DSP90    CLI   LINES,MAXLNS                                                     
         BE    DSP60               SCREEN IS FULL                               
         BAS   RE,DSPWCT           DISPLAY LAST WORK CODE TOTAL                 
         CLI   LINES,MAXLNS                                                     
         BE    DSP60               SCREEN IS FULL                               
         BAS   RE,TOTWCT           DISPLAY OVERALL TOTAL                        
*                                                                               
         CLI   LINES,0                                                          
         BNE   DSP92               NO DATA DISPLAYED                            
         MVC   FVMSGNO,=AL2(2103)  NO ALLOCATED ITEMS TO DISPLAY                
         MVI   FVMTYPE,FVMINFO                                                  
         CLI   ALLCSW,C'Y'                                                      
         BE    DSP100                                                           
         CLI   PARTSW,C'Y'                                                      
         BE    DSP100                                                           
         MVC   FVMSGNO,=AL2(2104)  NO ITEMS AVAILABLE FOR ALLOCATION            
         MVI   FVMTYPE,FVMINFO                                                  
         B     DSP100                                                           
*                                                                               
DSP92    XC    LASTKEY,LASTKEY     THIS IS THE LAST TIME                        
         MVI   OVMODE,INIT         NEXT TIME START FROM SCRATCH                 
         CLI   LINES,3                                                          
         BL    DSP93                                                            
         CLI   SUMMSW,C'Y'         SUPPRESSING DETAIL ?                         
         BNE   DSP94               IF NOT OK TO DISPLAY                         
*                                                                               
DSP93    MVI   ALLTCNT,0                                                        
         MVC   FVMSGNO,=AL2(2101)  ACTION COMPLETED - ENTER NEXT ACTION         
         MVI   FVMTYPE,FVMINFO                                                  
         CLI   ERROR,0                                                          
         BE    DSP100                                                           
         MVC   FVMSGNO,=AL2(2211)  ERROR - INVALID BILL NUMBER                  
         MVI   FVMTYPE,FVMERR                                                   
         LA    R4,BILNUMH                                                       
         B     DSP102                                                           
*                                                                               
DSP94    MVC   FVMSGNO,=AL2(2105)  INPUT/CHANGE ALLOCATION AMOUNT               
         MVI   FVMTYPE,FVMINFO                                                  
         MVI   OVMODE,MARK                                                      
*                                                                               
         USING LINED,R4                                                         
DSP96    LA    R4,ALCFRSTH                                                      
         LA    R4,LINAMTH                                                       
         LA    R0,MAXLNS                                                        
*                                                                               
DSP98    TM    1(R4),X'20'         FIND FIRST UNPROTECTED AMOUNT                
         BZ    DSP102                                                           
         LA    R4,LINLNQ(,R4)                                                   
         BCT   R0,DSP98                                                         
*                                                                               
DSP100   LA    R4,BILACTH          NO UNPROTECTED  AMOUNTS                      
*                                                                               
DSP102   ST    R4,FADR                                                          
*        GOTO1 VSQUASHR,DMCB,MSG,72                                             
*        GOTO1 (RF),(R1),MSG,72    SQUASH TOGETHER MSG AND XTRAMESS             
         BAS   RE,DSPTOT           DISPLAY TOTAL                                
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* FIRST PART OF UPDATE IS TO VALIDATE SCREEN                          *         
***********************************************************************         
         SPACE 1                                                                
         USING LINED,R4                                                         
         USING KEYTABD,R6                                                       
         SPACE 1                                                                
UPD00    LA    R4,ALCFRSTH                                                      
         LA    R6,KEYTAB                                                        
         LA    R3,MAXLNS                                                        
*                                                                               
UPD02    TM    KEYTBSW,KEYTBWCT    WORK CODE TOTALS ?                           
         BO    UPD12               YES, SKIP                                    
         TM    KEYTBSW,KEYTBTOT    OVERALL   TOTAL ?                            
         BO    UPD12               YES, SKIP                                    
         CLI   KEYTBSW,0           UNUSED    LINE ?                             
         BE    UPD12               YES, SKIP                                    
         ZIC   RF,LINAMTH+5                                                     
         LTR   RF,RF               CHECK INPUT LENGTH                           
         BZ    UPD12               NO INPUT ASSUME BILLABLE AMOUNT              
         CLI   LINAMT,C'N'         NO IS ALLOWED                                
         BE    UPD12                                                            
*                                                                               
         CLI   LINAMT,C'H'         INPUT HOURS                                  
         BNE   UPD08                                                            
         OC    LINHRS,LINHRS                                                    
         BZ    UPD14               NOT ALLOWED IF NO HOURS                      
         BCTR  RF,0                                                             
         LTR   RF,RF               FIX LENGTH                                   
         BZ    UPD14                                                            
         GOTO1 VCASHVAL,DMCB,LINAMT+1,(RF)                                      
         CLI   0(R1),X'FF'                                                      
         BE    UPD14               BAD NUMBER OF HOURS                          
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         ZAP   SVHRS,DUB                                                        
         DP    DUB,=P'25'                                                       
         CP    DUB+6(2),=P'0'                                                   
         BNE   UPD14               MUST BE QUARTER HOURS                        
         ST    R1,INHRS            SAVE NUMBER OF HOURS - INPUT                 
*                                                                               
         LA    RF,L'LINHRS         GET BILLABLE HOURS FROM SCREEN               
         LA    R5,LINHRS                                                        
*                                                                               
UPD03    CLI   0(R5),X'40'                                                      
         BH    UPD03A                                                           
         LA    R5,1(,R5)                                                        
         BCT   RF,UPD03                                                         
*                                                                               
UPD03A   GOTO1 VCASHVAL,DMCB,(R5),(RF)                                          
         CLI   0(R1),X'FF'                                                      
         BE    UPD14                                                            
         ICM   RE,15,DMCB+4        BILLABLE HOURS                               
         ST    RE,BILHRS           SAVE BILLABLE HOURS                          
*                                                                               
         ZAP   SVRTE,=P'0'                                                      
         LA    RF,L'LINRTE         THE SCREEN                                   
         LA    R5,LINRTE           GET LINE RATE FROM SCREEN                    
*                                                                               
UPD04    CLI   0(R5),X'40'                                                      
         BH    UPD04A                                                           
         LA    R5,1(,R5)                                                        
         BCT   RF,UPD04                                                         
*                                                                               
UPD04A   GOTO1 VCASHVAL,DMCB,(R5),(RF)                                          
         CLI   0(R1),X'FF'                                                      
         BE    UPD14                                                            
         L     R1,DMCB+4                                                        
         CVD   R1,SVRTE                                                         
         CP    SVRTE,=P'0'                                                      
         BNE   UPD06               ZERO RATE                                    
         L     RE,BILHRS           BILLABLE HOURS                               
         L     RF,INHRS            INPUT HOURS                                  
*                                                                               
*                                  VERIFY INPUT NOT MORE THAN BILLABLE          
         LTR   RE,RE               IS   BILLABLE HOURS NEGATIVE ?               
         BNP   UPD05               YES, TEST FOR NEGATIVE                       
         LTR   RF,RF               IS   INPUT NEGATIVE ?                        
         BNP   UPD14               YES, INVALID                                 
         CR    RF,RE               IS   INPUT HOURS > BILLABLE HOURS ?          
         BH    UPD14               YES, INVALID                                 
         B     UPD12               NO,  OKAY                                    
*                                                                               
UPD05    LTR   RF,RF               IS   INPUT HOURS NEGATIVE?                   
         BP    UPD14               NO,  INVALID                                 
         CR    RF,RE               IS   INPUT HOURS < BILLABLE HOURS ?          
         BL    UPD14               YES, INVALID                                 
         B     UPD12               NO,  OKAY                                    
*                                                                               
UPD06    MP    SVRTE,SVHRS+5(3)    RATE * HOURS                                 
         SRP   SVRTE,64-2,5                                                     
         EXTED (P8,SVRTE),(L'LINAMT,TEMP),2,FLOAT=-                             
         LA    RF,L'LINAMT                                                      
         STC   RF,LINAMTH+5        FIX  LENGTH                                  
         MVC   LINAMT,SPACES                                                    
         MVC   LINAMT,TEMP                                                      
         OI    LINAMTH+6,FVOXMT    DISPLAY AMOUNT                               
*                                  NOW HANDLE AS REGULAR INPUT                  
*                                                                               
UPD08    GOTO1 VCASHVAL,DMCB,LINAMT,(RF)                                        
         CLI   0(R1),X'FF'                                                      
         BE    UPD14               INVALID AMOUNT                               
         L     RF,DMCB+4           INPUT AMOUNT                                 
         CVD   RF,DUB                                                           
         CP    KEYTBBIL,=P'0'      IS   BILLABLE AMOUNT NEGATIVE ?              
         BNH   UPD10               YES, FOR  NEGATIVE                           
         CP    DUB,=P'0'           IS   INPUT AMOUNT POSITIVE ?                 
         BNH   UPD14               NO,  INVALID                                 
         CP    DUB,KEYTBBIL        IS   INPUT AMOUNT > BILLABLE AMOUNT?         
         BNH   UPD12               NO, OKAY                                     
         ZAP   DUB5,DUB            SEE HOW MUCH HIGHER                          
         SP    DUB5,KEYTBBIL                                                    
         CP    DUB5,=P'1'          IF MORE THAN A PENNY, ERROR                  
         BH    UPD14               YES, INVALID                                 
         B     UPD12               NO,  OKAY                                    
*                                                                               
UPD10    CP    DUB,=P'0'           IS   INPUT AMOUNT NEGATIVE ?                 
         BH    UPD14               YES, INVALID                                 
         CP    DUB,KEYTBBIL        IS   INPUT AMOUNT < BILLABLE AMOUNT?         
         BL    UPD14               YES, INVALID                                 
*        B     UPD12               NO,  OKAY                                    
*                                                                               
UPD12    LA    R6,KEYTABLQ(,R6)                                                 
         LA    R4,LINLNQ(,R4)                                                   
         BCT   R3,UPD02                                                         
         B     UPD30                                                            
*                                                                               
*                                                                               
UPD14    MVI   FERN,INVAMNT                                                     
         LA    R2,LINAMTH                                                       
         ST    R2,FADR                                                          
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT ,                                                                
***********************************************************************         
* NOW DO THE ACTUAL UPDATE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING LINED,R4                                                         
         USING KEYTABD,R6                                                       
         SPACE 1                                                                
UPD30    LA    R4,ALCFRSTH                                                      
         LA    R6,KEYTAB                                                        
         LA    R3,MAXLNS                                                        
         ZAP   SCIUNET,=P'0'       CLEAR SCIEL BUCKETS FOR ACTION=BILL          
         ZAP   SCIUCOM,=P'0'                                                    
*                                                                               
UPD32    ZAP   MKAMT,=P'0'                                                      
         XC    MKHRS,MKHRS                                                      
         TM    KEYTBSW,KEYTBWCT    WORK CODE TOTAL LINE ?                       
         BO    UPD46               YES, GO   PROCESS                            
         TM    KEYTBSW,KEYTBTOT    OVER ALL  TOTAL ?                            
         BO    UPD48               YES, GO   PROCESS                            
         CLI   KEYTBSW,0           UNUSED    LINE ?                             
         BE    UPD50               YES, SKIP                                    
         TM    LINAMTH+4,X'20'     WAS  FIELD ALTERED ?                         
         BO    *+8                                                              
         MVI   UPDSW,C'Y'          IF   SO RE-DISPLAY SCREEN                    
         CLI   LINAMT,C'N'                                                      
         BE    UPD40                                                            
         CLI   LINAMT,C'H'         H -  AT THIS POINT MEANS HOURS               
         BE    UPD34               AND  NO RATE                                 
         CP    KEYTBBIL,=P'0'      ANY  BILLABLE AMOUNT ?                       
         BNE   UPD38               YES, PROCESS  AMOUNT                         
         LA    RF,L'LINHRS         GET  BILLABLE HOURS FROM SCREEN              
         LA    R5,LINHRS                                                        
*                                                                               
UPD33    CLI   0(R5),X'40'                                                      
         BH    UPD33A                                                           
         LA    R5,1(,R5)                                                        
         BCT   RF,UPD33                                                         
*                                                                               
UPD33A   GOTO1 VCASHVAL,DMCB,(R5),(RF)                                          
         B     UPD36                                                            
*                                                                               
UPD34    ZIC   RF,LINAMTH+5        GET  HOURS                                   
         BCTR  RF,0                                                             
         GOTO1 VCASHVAL,DMCB,LINAMT+1,(RF)                                      
*                                                                               
UPD36    L     R1,DMCB+4                                                        
         CVD   R1,DUB              SAVE NUMBER OF HOURS                         
         ST    R1,MKHRS            WHEN NO RATE                                 
         B     UPD40                                                            
*                                                                               
UPD38    ZAP   MKAMT,KEYTBBIL                                                   
         ZIC   RF,LINAMTH+5                                                     
         LTR   RF,RF                                                            
         BZ    UPD40               NO   INPUT, ASSUME SAVED AMOUNT              
         GOTO1 VCASHVAL,DMCB,LINAMT,(RF)                                        
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,4(,R1)           GET  AMOUNT MARKED                           
         CVD   RE,DUB2             PACK IT                                      
         ZAP   MKAMT,DUB2          SAVE AMOUNT MARKED                           
*                                                                               
UPD40    BAS   RE,RDMK                                                          
         CLI   LINAMT,C'N'                                                      
         BE    UPD41                                                            
         CLI   LINAMT,C'H'                                                      
         BE    UPD41                                                            
         MVI   LINAMT,C' '                                                      
*                                                                               
UPD41    MVC   LINAMT+1(L'LINAMT-1),SPACES                                      
*                                                                               
         CP    MKAMT,=P'0'                                                      
         BE    UPD42                                                            
         EXTED MKAMT,(L'LINAMT,TEMP),2,FLOAT=-                                  
         MVC   LINAMT,TEMP                                                      
         B     UPD44                                                            
*                                                                               
UPD42    OC    MKHRS,MKHRS                                                      
         BZ    UPD44                                                            
         MVI   TEMP,C'H'                                                        
         EXTED MKHRS,(L'LINAMT-1,TEMP+1),2,FLOAT=-                              
         MVC   LINAMT,TEMP                                                      
*                                                                               
UPD44    OI    LINAMTH+6,FVOXMT    TRANSMIT                                     
         OI    LINAMTH+4,X'20'                                                  
         ZAP   DUB,MKAMT           CURRENT ALLOCATION                           
         SP    DUB,KEYTBALC        LESS PREVIOUS ALLOCATION                     
         ZAP   KEYTBALC,MKAMT      SAVE LAST ALLOCATED AMOUNT                   
         ZAP   MKAMT,DUB           NOW  HAVE NET CHANGE                         
         CP    DUB,=P'0'           ANY  CHANGE ?                                
         BE    UPD50               NO                                           
         MVI   UPDSW,C'Y'          INDICATE ALLOCATION CHANGE                   
         AP    TOTALL,DUB          UPDATE ALLOCATED TOTAL                       
         LA    R2,LINWC                                                         
         BAS   RE,WKCUP            GET  ENTRY IN ALLOCATED TABLE                
*                                                                               
         USING ALLTD,R5                                                         
         AP    ALLTAMT,DUB                                                      
         AP    TOTALLWC,DUB        ADJUST OVERALL TOTAL                         
         B     UPD50                                                            
*                                                                               
UPD46    LA    R2,KEYTBWC          DISPLAY TOTAL FOR W/C                        
         BAS   RE,WKCUP            FIND ENTRY FOR THIS WORKCODE                 
*                                                                               
         USING ALLTD,R5                                                         
         ZAP   DUB,ALLTAMT         ALLOCATED TOTAL                              
         EXTED ALLTAMT,LINAMT,2,FLOAT=-                                         
         B     UPD49                                                            
*                                  OVERALL TOTAL                                
UPD48    EXTED TOTALLWC,LINAMT,2,FLOAT=-                                        
*                                                                               
UPD49    OI    LINAMTH+6,FVOXMT    TRANSMIT                                     
*                                                                               
UPD50    LA    R6,KEYTABLQ(,R6)                                                 
         LA    R4,LINLNQ(,R4)                                                   
         BCT   R3,UPD32                                                         
         DROP  R4,R5,R6                                                         
*                                                                               
         MVC   KEY,SPACES          REREAD JOB                                   
         MVC   KEY(15),JOBKEY                                                   
         GOTO1 AREADL,AIOAREA1                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA1         ADD/UPDATE SCIEL                             
         MVI   ELCODE,SCIELQ                                                    
         BAS   RE,GETEL                                                         
         B     UPD63                                                            
*                                                                               
UPD62    BAS   RE,NEXTEL                                                        
*                                                                               
UPD63    BNE   UPD64                                                            
*                                                                               
         USING SCIELD,R2                                                        
         CLI   SCITYPE,SCITCBAP    LOOKING FOR ALLOCATE                         
         BNE   UPD62                                                            
         ZAP   SCIAMNT,SCIUNET                                                  
         CLI   SCILN,SCILN2Q                                                    
         BL    UPD66                                                            
         ZAP   SCIADMN,SCIUCOM                                                  
         B     UPD66                                                            
*                                                                               
UPD64    LA    R2,TEMP             NOT THERE, ADD ONE                           
         XC    TEMP,TEMP                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN2Q                                                    
         MVI   SCITYPE,SCITCBAP                                                 
         ZAP   SCIAMNT,SCIUNET                                                  
         ZAP   SCIADMN,SCIUCOM                                                  
         GOTO1 AADDELM,AIOAREA1                                                 
*                                                                               
UPD66    MVC   KEY,0(R2)           UPDATE THE JOB                               
         GOTO1 AWRITE,AIOAREA1                                                  
         BE    UPD80                                                            
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
*                                  ALLOCATION CHANGED -                         
UPD80    MVC   FVMSGNO,=AL2(2106)   YOU MAY MAKE ADDITIONAL ADJUSTMENTS         
         MVI   FVMTYPE,FVMINFO                                                  
         MVI   OVMODE,MARK                                                      
         CLI   UPDSW,C'Y'                                                       
         BE    DSP96               REDISPLAY DATA FOR ADDITIONL CHANGES         
*                                                                               
         MVI   OVMODE,DSPLY        NO CHANGES MADE TO SCREEN                    
         MVC   KEY,LASTKEY                                                      
         OC    LASTKEY,LASTKEY                                                  
         BNZ   DSP04               DISPLAY NEXT DATA                            
*                                                                               
         MVC   FVMSGNO,=AL2(2101)  ACTION COMPLETED - ENTER NEXT ACTION         
         MVI   FVMTYPE,FVMINFO                                                  
*                                                                               
         MVI   OVMODE,INIT                                                      
         MVI   ALLTCNT,0                                                        
*                                                                               
         USING LINED,R4                                                         
         LA    R4,ALCFRSTH         CLEAR SCREEN                                 
         LA    R0,MAXLNS                                                        
*                                                                               
UPD82    NI    LINDH+1,X'FF'-X'09'   TURN-OFF HIGH/MODIFIED                     
         NI    LINAMTH+1,X'FF'-X'29' TURN-OFF PROTECTED/HIGH/MODIFIED           
         LA    R4,LINLNQ(,R4)                                                   
         BCT   R0,UPD82                                                         
*                                                                               
         TWAXC ALCFRSTH,ALCTABH,PROT=Y                                          
         B     DSP100                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* READ AND UPDATE JOB POSTING                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         USING LINED,R4                                                         
         USING KEYTABD,R6                                                       
         SPACE 1                                                                
RDMK     NTR1                                                                   
         MVI   DATASW,C'N'         INDICATE NO DATA YET                         
         CLI   OVMODE,REV                                                       
         BNE   RDMK02                                                           
         BAS   RE,CKBILL                                                        
*                                                                               
RDMK02   MVI   COMMSW,C'Y'                                                      
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         MVC   TRNKCULA,JOBKEY     BUILD KEY FROM VARIOUS BITS                  
         CLI   OVMODE,REV                                                       
         BE    RDMK04                                                           
         MVC   TRNKWORK,LINWC                                                   
         MVC   TRNKCCPY,COMPANY                                                 
         MVC   TRNKULC,LINSUP                                                   
         OC    TRNKULC,SPACES                                                   
         MVC   TRNKREF,LININV                                                   
         OC    TRNKREF,SPACES                                                   
         MVC   TRNKDATE,KEYTBDAT   DATE AND SUBREF FROM TABLE                   
         MVC   TRNKSBR,KEYTBSBR                                                 
*                                                                               
RDMK04   GOTO1 AREADL,AIOAREA1                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RDMK06   L     R2,AIOAREA1                                                      
         CLC   TRNKCULA,JOBKEY     ACCOUNT                                      
         BNE   RDMK60                                                           
         OC    ACCOUSED(2,R2),ACCOUSED(R2)                                      
         BNZ   RDMK30                                                           
         LA    R3,ACCORFST(,R2)    ->   1ST  ELEMENT                            
         CLI   0(R3),TRNELQ        X'44' -   TRANSACTION    ELEMENT ?           
         BNE   RDMK30                                                           
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         TM    TRNRSTAT,TRNSDRFT   IS   THIS A    DRAFT ?                       
         BO    RDMK30              YES, SKIP RECORD                             
         CLC   TRNKWORK,=C'99'     SKIP BILLING                                 
         BE    RDMK30                                                           
         CLI   OVMODE,REV          REVERSE ?                                    
         BE    RDMK07              YES, CONTINUE  WITH RECORD                   
         TM    TRNRSTAT,TRNSREVS   DID  RCD  CHANGE    TO   REVERSED ?          
         BO    RDMKREVS            YES, ERROR     EXIT                          
*                                                                               
         USING TRNELD,R3                                                        
RDMK07   TM    TRNSTAT,X'01'                                                    
         BZ    *+8                                                              
         MVI   COMMSW,C'N'         NON-COMMISSIONABLE                           
         DROP  R2                                                               
*                                                                               
         GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         CLI   OVMODE,REV                                                       
         BNE   RDMK08                                                           
         MVI   BYTE,0                                                           
         BAS   RE,SETREV           REVERSE PREVIOUS ALLOCATION                  
         CLI   BYTE,C'W'                                                        
         BE    RDMK26              NEED TO WRITE BACK TRANSACTION               
         B     RDMK30                                                           
*                                                                               
RDMK08   CP    MKAMT,=P'0'                                                      
         BE    RDMK10                                                           
         BAS   RE,GETALL           GET  TOTALS FOR ALLOCATION                   
         OC    TRNHRS,TRNHRS       ANY  HOURS ?                                 
         BZ    RDMK10              NO                                           
         CP    BILRTE,=P'0'        ANY  RATE ?                                  
         BE    RDMK10              NO                                           
*                                                                               
         ZAP   DUB,MKAMT           AMOUNT ALLOCATED                             
         ZAP   PK16,DUB                                                         
         MP    PK16,=P'1000'                                                    
         ZAP   SVRTE,BILRTE        RATE                                         
         DP    PK16,SVRTE+4(4)     AMOUNT/RATE                                  
         ZAP   DUB,PK16(12)        GIVES  HOURS                                 
         DP    DUB,=P'25'          GET    TO NEAREST QTR HOUR                   
         SRP   DUB(6),64-1,5                                                    
         MP    DUB(6),=P'25'                                                    
         ZAP   PK16,DUB(6)                                                      
         ZAP   DUB,PK16                                                         
         CVB   R1,DUB                                                           
         ST    R1,MKHRS            SAVE NEW HOURS                               
         MP    PK16,SVRTE+3(5)     RATE *   HOURS                               
         SRP   PK16,64-2,5                                                      
         ZAP   DUB,PK16                                                         
                                                                                
         ZAP   DUB5,DUB            SEE IF IT'S THE PENNY PROBLEM                
         SP    DUB5,KEYTBBIL                                                    
         CP    DUB5,=P'1'                                                       
         BE    *+14                                                             
         CP    DUB5,=P'-1'                                                      
         BNE   *+10                                                             
         ZAP   DUB,KEYTBBIL                                                     
                                                                                
         ZAP   MKAMT,DUB           NEW  MARKED AMOUNT                           
*                                                                               
         USING PTAELD,R3                                                        
RDMK10   L     R3,APROLST                                                       
         CLI   0(R3),PTAELQ                                                     
         B     RDMK12+8                                                         
*                                                                               
RDMK12   MVI   ELCODE,PTAELQ       LOOK AT 77'S FOR ALLOCATIONS                 
         BAS   RE,NEXTEL3          IF   NONE, ADD ONE                           
         BNE   RDMK14                                                           
*                                                                               
         CLI   PTATYPE,PTATRAL     IS   THIS AN ALLOCATION ?                    
         BNE   RDMK12              NO,  KEEP LOOKING                            
         TM    PTASTAT1,PTASPEND   YES, IS   IT PENDING ?                       
         BO    RDMK20              YES, USE  IT                                 
         B     RDMK12              NO,  LOOK AGAIN                              
*                                                                               
RDMK14   L     R3,APROLST                                                       
         CLI   0(R3),PTAELQ                                                     
         B     RDMK16+8                                                         
*                                                                               
RDMK16   MVI   ELCODE,PTAELQ       NOTHING ALLOCATED, LOOK FOR FREE 77          
         BAS   RE,NEXTEL3          IF NONE, ADD ONE                             
         BNE   RDMK18                                                           
*                                                                               
         CLI   PTATYPE,0           IS THIS UNUSED?                              
         BE    RDMK20              YES, USE IT                                  
         B     RDMK16              NO, LOOK AGAIN                               
*                                                                               
RDMK18   XC    ELEMENT,ELEMENT     NO 77 ELEMENT FOUND, ADD ONE                 
         MVI   ELEMENT,PTAELQ                                                   
         MVI   ELEMENT+1,PTARLN1Q                                               
*                                                                               
         LA    R1,PTARLN1Q                                                      
         BCTR  R1,0                                                             
*                                  ADDRESS OF NEW ELEMENT                       
         EXMVC R1,0(R3),ELEMENT                                                 
         LA    R1,1(R1,R3)                                                      
         MVI   0(R1),0             MARK END OF RECORD                           
*                                                                               
RDMK20   SR    R1,R1               INITIALIZE ELEMENT                           
         IC    R1,PTALN                                                         
         SH    R1,=Y(PTADATE-PTAELD)                                            
         BCTR  R1,0                                                             
         EXXC  R1,PTADATE,PTADATE                                               
         ZAP   PTANETF,=P'0'                                                    
         ZAP   PTACDSC,=P'0'                                                    
         ZAP   PTARCORT,=P'0'                                                   
         ZAP   PTARCOM,=P'0'                                                    
         MVC   PTACUR,CURCOD                                                    
*                                                                               
         MVI   PTATYPE,PTATRAL                                                  
         MVC   PTADATE,TODAYC                                                   
         OI    PTASTAT1,PTASPEND+PTASCASH                                       
*                                                                               
         ZAP   PLTEMP,MKAMT                                                     
         ZAP   DUB,MKAMT                                                        
         ZAP   PTANET,MKAMT                                                     
*                                                                               
         CLI   COMMSW,C'Y'         COMMISSIONABLE?                              
         BNE   RDMK21              NO                                           
*                                                                               
         BRAS  RE,GETRUL           GET  COMMISSION RATE                         
         ZAP   PTARCORT,DUB        SAVE IT                                      
*                                                                               
         ZAP   PK16,PTANET                                                      
         MP    PK16,DUB+2(6)       NET  * RATE                                  
         SRP   PK16,64-6,5         RATE HAS 4DP SO ROUND                        
         ZAP   PTARCOM,PK16                                                     
*                                                                               
RDMK21   CP    MKAMT,=P'0'         ANY  AMOUNT ?                                
         BNE   RDMK22              YES                                          
*                                                                               
         NI    PTASTAT1,X'FF'-PTASCASH                                          
         OI    PTASTAT1,PTASHOUR                                                
         OC    MKHRS,MKHRS         ANY  HOURS                                   
         BNZ   RDMK22              YES                                          
         XC    PTASTAT1,PTASTAT1   NO,  CLEAR STATUS                            
         XC    PTADATE,PTADATE     AND  DATE                                    
         XC    PTATYPE,PTATYPE     AND  TYPE                                    
         ZAP   PTARCORT,=P'0'      AND  RATE                                    
         MVI   PTAEL,X'FF'         JUST DELETE THE EXTRA ELEMENT                
         B     RDMK24                                                           
*                                                                               
*                                  NEED HOURS AND CD ETC                        
RDMK22   ZAP   PLTEMP,TRNCSD                                                    
         CP    PLTEMP,=P'0'        ANY  CD ON THIS TRANSACTION ?                
         BE    *+8                 NO,  SKIP                                    
         BAS   RE,DIV              GET  AMOUNT FOR THIS ALLOCATION              
         ZAP   PTACDSC,PLTEMP      PUT  CASH DISCOUNT                           
         ZAP   DUB,PLTEMP          SAVE CASH DISCOUNT IN DUB                    
*                                                                               
         L     RF,MKHRS                                                         
         STH   RF,PTAHOURS         PUT  HOURS IN ELEMENT                        
*                                                                               
RDMK24   AP    SCIUNET,PTANET      TOTAL THE NET AND COMMISSION                 
         AP    SCIUCOM,PTARCOM                                                  
*                                                                               
RDMK25   MVI   ELCODE,PTAELQ       LOOK AT 77'S FOR ALLOCATIONS                 
         BAS   RE,NEXTEL3          IF   NONE, ADD ONE                           
         BNE   RDMK26                                                           
*                                                                               
         CLI   PTATYPE,PTATRAL     IS   THIS AN ALLOCATION ?                    
         BNE   RDMK25              NO,  KEEP LOOKING                            
         TM    PTASTAT1,PTASPEND   YES, IS   IT PENDING ?                       
         BZ    RDMK25              NO, KEEP LOOKING                             
         MVI   0(R3),X'FF'         YES, DELETE IT                               
         B     RDMK25              LOOK AGAIN                                   
*                                                                               
RDMK26   GOTO1 VPRORATA,DMCB,(X'A1',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
         TM    0(R1),X'10'         ANY ERRORS ?                                 
         BNO   RDMK28              NO                                           
         LA    R1,LINAMTH          YES, GENERATE A MESSAGE                      
         B     MAXALL                                                           
*                                                                               
RDMK28   MVC   KEY,0(R2)                                                        
         GOTO1 ATRXEL,AIOAREA1     UPDATE X'75'                                 
         GOTO1 AWRITE,AIOAREA1                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   DATASW,C'Y'         INDICATE WE HAVE DATA                        
*                                                                               
RDMK30   CLI   OVMODE,REV                                                       
         BNE   RDMK60                                                           
         GOTO1 ASEQL,AIOAREA1                                                   
         B     RDMK06                                                           
*                                                                               
RDMK60   CLI   DATASW,C'Y'         DO WE HAVE DATA?                             
         BNE   EXIT                NO                                           
         MVC   KEY,SPACES          YES, READ JOB RECORD AGAIN                   
         MVC   KEY(15),JOBKEY                                                   
         GOTO1 AREADL,AIOAREA1                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA1         UPDATE/CREATE SCIEL                          
         MVI   ELCODE,SCIELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
RDMK62   BAS   RE,NEXTEL                                                        
         BNE   RDMK64                                                           
*                                                                               
         USING SCIELD,R2                                                        
         CLI   SCITYPE,SCITCBAP    LOOKING FOR ALLOCATE                         
         BNE   RDMK62                                                           
         AP    SCIAMNT,SCIUNET                                                  
         CLI   SCILN,SCILN2Q                                                    
         BL    RDMK66                                                           
         AP    SCIADMN,SCIUCOM                                                  
         B     RDMK66                                                           
*                                                                               
RDMK64   LA    R2,TEMP             NOT THERE, ADD ONE                           
         XC    TEMP,TEMP                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN2Q                                                    
         MVI   SCITYPE,SCITCBAP                                                 
         ZAP   SCIAMNT,SCIUNET                                                  
         ZAP   SCIADMN,SCIUCOM                                                  
         GOTO1 AADDELM,AIOAREA1                                                 
*                                                                               
RDMK66   MVC   KEY,0(R2)           UPDATE JOB                                   
         GOTO1 AWRITE,AIOAREA1                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
         DROP  R2,R3,R4,R6                                                      
         EJECT ,                                                                
***********************************************************************         
* CHECK IF POSTINGS WERE ALREADY MADE                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP TRANSACTION RECORD                       
         SPACE 1                                                                
CKBILL   NTR1                                                                   
         XC    ERROR,ERROR         ASSUME NO ERRORS WILL OCCUR                  
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         MVC   TRNKCULA,JOBKEY     LOAD IN CLI,PRD,JOB                          
         MVC   TRNKWORK,=C'99'     BILLING ON JOB                               
         GOTO1 ARDHI,AIOAREA2                                                   
         L     R2,AIOAREA2                                                      
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION ELEMENT                     
CKBILL1  CLC   TRNKCULA,JOBKEY     SAME JOB?                                    
         BNE   CKBILLX                                                          
         LA    R3,ACCORFST(,R2)    ->   FIRST ELEMENT                           
         CLI   TRNEL,TRNELQ        TRANSACTION ELEMENT ?                        
         BNE   CKBILL5                                                          
         CLC   TRNREF,BILNUM       SAME BILL #                                  
         BNE   CKBILL5                                                          
         CLI   TRNBTYPE,TRNBTCLI   IS THIS A CLIENT BILL?                       
         BE    EXIT                YES,IS OK TO PROCEED                         
*                                                                               
CKBILL5  GOTO1 ASEQ,AIOAREA2                                                    
         B     CKBILL1                                                          
*                                                                               
CKBILLX  MVI   ERROR,1                                                          
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* GET TOTALS FOR ALLOCATE                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP TRANSACTION RECORD                       
         USING TRNELD,R3           MAP TRANSACTION ELEMENT                      
         SPACE 1                                                                
GETALL   NTR1                                                                   
         ZAP   TRNCSD,=P'0'        TRANSACTION CASH DISCOUNT                    
         XC    TRNHRS,TRNHRS                   HOURS                            
         ZAP   TRNNET,=P'0'                    NET                              
         ZAP   BILRTE,=P'0'        BILLABLE    HOURLY RATE                      
         ZAP   BILCSD,=P'0'                    CASH DISCOUNT                    
         XC    BILHRS,BILHRS                   HOURS                            
         ZAP   BILNET,=P'0'                    NET                              
         ZAP   ALLCSD,=P'0'        ALLOCATED   CASH DISCOUNT                    
         XC    ALLHRS,ALLHRS                   HOURS                            
         ZAP   ALLNET,=P'0'                    NET                              
         LA    R3,ACCORFST(,R2)    GET  1ST  ELEMENNT                           
         ZAP   TRNNET,TRNAMNT      TOTALS DEBITS                                
         ZAP   BILNET,TRNAMNT      ASSUME ALL IS BILLABLE                       
         ZAP   DUB,TRNAMNT         SAVE TRNAMNT                                 
         XC    FRSTPTA,FRSTPTA     CLEAR ELEMENT ADDRESS                        
         LR    R5,R3                                                            
*                                                                               
GETA2    ZIC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         CLI   0(R3),0                                                          
         BE    GETA10              GET 77 DATA AFTER 40 AND 50 ELS              
*                                                                               
         CLI   0(R3),X'40'         HOURS                                        
         BE    GETA4                                                            
         CLI   0(R3),X'50'         CASH DISCOUNT                                
         BE    GETA8                                                            
         B     GETA2                                                            
*                                                                               
         USING PRTELD,R3                                                        
GETA4    CLI   PRTLN,PRTLNQ        NEW ELEMENT?                                 
         BL    GETA6               NO                                           
         TM    PRTSTAT,PRTSBILQ    YES, IS IT BILLABLE?                         
         BZ    GETANO              NO                                           
*                                                                               
GETA6    ZAP   DUB,PRTHOUR         GET  TOTAL HOURS                             
         CVB   R1,DUB                                                           
         ST    R1,TRNHRS                                                        
         ST    R1,BILHRS                                                        
         ZAP   BILRTE,PRTRATE      AND  RATE                                    
         ZAP   DUB,PRTRATE                                                      
         B     GETA2                                                            
*                                                                               
         USING SCIELD,R3                                                        
GETA8    CLI   SCITYPE,C'D'        CASH DISCOUNT                                
         BNE   GETA2                                                            
         ZAP   TRNCSD,SCIAMNT                                                   
         ZAP   BILCSD,SCIAMNT                                                   
         ZAP   DUB,SCIAMNT                                                      
         B     GETA2                                                            
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* SET BUCKETS BASED ON 77 DATA                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING PTAELD,R3                                                        
         SPACE 1                                                                
GETA10   MVI   BYTE,0                                                           
         L     R3,APROLST                                                       
         CLI   0(R3),PTAELQ                                                     
         B     GETA12+8                                                         
*                                                                               
GETA12   MVI   ELCODE,PTAELQ       GET 77 DATA                                  
         BAS   RE,NEXTEL3          EXIT WHEN DONE                               
         BNE   GETA20                                                           
*                                                                               
         TM    PTASTAT1,PTASPEND   YES, IS IT PENDING ?                         
         BO    GETA16              YES, SEE IF IT'S FOR BILLING                 
*                                                                               
         CLI   PTATYPE,PTATRAL     NO,  IS IT A BILLING ELEMENT ?               
         BE    GETA14              YES, TAKE THE DATA                           
         CLI   PTATYPE,PTATWOF     WRITE-OFF ?                                  
         BE    GETA14              YES, TAKE THE DATA                           
         CLI   PTATYPE,PTATWOFR    WRITE-OFF RECOVERY ?                         
         BNE   GETA12              NO,  GET NEXT 77                             
*                                                                               
*                                  SUBTRACT PREVIOUS BILLINGS, W/O'S            
*                                  AND  RECOVERIES TO GET BILLABLE              
GETA14   SP    BILNET,PTANET                                                    
         SP    BILCSD,PTACDSC      CASH DISCOUNT                                
         ZAP   DUB,PTACDSC                                                      
*                                                                               
         OC    TRNHRS,TRNHRS       ANY  HOURS ON THIS TRANSACTION ?             
         BZ    GETA12              NO,  GET NEXT 77                             
*                                                                               
         L     RE,BILHRS           GET  HOURS                                   
         LH    RF,PTAHOURS                                                      
         SR    RE,RF                                                            
         ST    RE,BILHRS                                                        
         B     GETA12              GET  NEXT ELEMENT                            
*                                                                               
GETA16   CLI   PTATYPE,PTATRAL     ALLOCATED TO BILL ?                          
         BNE   GETA12              NO,  SKIP IT                                 
*                                                                               
         AP    ALLNET,PTANET       YES, GET CURRENT ALLOCATION                  
         AP    ALLCSD,PTACDSC                                                   
*                                                                               
         OC    TRNHRS,TRNHRS       NO HOURS ON THIS TRANSACTION                 
         BZ    GETA17                                                           
         L     RF,ALLHRS                                                        
         AH    RF,PTAHOURS                                                      
         ST    RF,ALLHRS                                                        
*                                                                               
GETA17   L     R1,FRSTPTA                                                       
         LTR   R1,R1               DO WE HAVE A 77 ALREADY?                     
         BZ    GETA18              NO                                           
         MVI   0(R3),X'FF'         YES, MARK THE NEW DELETED                    
         ZAP   PTANET-PTAEL(L'PTANET,R1),ALLNET                                 
         ZAP   PTACDSC-PTAEL(L'PTACDSC,R1),ALLCSD                               
         MVC   PTAHOURS-PTAEL(L'PTAHOURS,R1),ALLHRS                             
         MVI   BYTE,C'W'                                                        
         B     GETA12                                                           
*                                                                               
GETA18   ST    R3,FRSTPTA          SAVE ADDRESS OF THIS PTA                     
         CLI   ACTION,REV          IF ACTION IS REVERSE                         
         BNE   GETA12                                                           
         ZAP   MKAMT,PTANET                                                     
         ZAP   BILNET,PTANET                                                    
*                                                                               
         ZAP   BILCSD,PTACDSC                                                   
         ZAP   DUB,PTACDSC                                                      
*                                                                               
         LH    RF,PTAHOURS                                                      
         ST    RF,BILHRS                                                        
         B     GETAYES                                                          
*                                                                               
GETA20   CLI   BYTE,C'W'                                                        
         BNE   GETAYES                                                          
         GOTO1 VPRORATA,DMCB,(X'A0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
         TM    0(R1),X'10'         ANY  ERRORS ?                                
         BNO   *+6                 NO                                           
         DC    H'0'                PRORATA TRANSACTION TOO BIG                  
         GOTO1 AWRITE,AIOAREA1                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETAYES  B     OKXIT               AND WE DON'T CARE ABOUT OTHER 77'S           
*                                                                               
GETANO   B     ERRXIT              SKIP THIS ITEM                               
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* GET TOTAL FOR FILTERED TRANSACTIONS                                 *         
***********************************************************************         
         SPACE 1                                                                
GETTOT   NTR1                                                                   
         ZAP   AMTOT,=P'0'         CLEAR OUT AMOUNT TOTAL                       
         MVC   KEY,SPACES          CLEAR OUT KEY                                
         MVC   KEY(15),JOBKEY      FIRST TRANS ON JOB                           
         GOTO1 AREAD,AIOAREA1      READ  FIRST TRANS                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         USING TRNELD,R3           MAP  TRANSACTION ELEMENT                     
GETTOT1  GOTO1 ASEQ,AIOAREA1                                                    
         L     R2,AIOAREA1                                                      
         CLC   JOBKEY,TRNKCULA     SAME JOB?                                    
         BNE   EXIT                EXIT ROUTINE                                 
         OC    ACCOUSED(2,R2),ACCOUSED(R2)                                      
         BNZ   GETTOT1                                                          
         LA    R3,ACCORFST(,R2)    GET  1ST  ELEMENT                            
         CLI   0(R3),TRNELQ        X'44' - TRANSACTION ELEMENT ?                
         BNE   GETTOT1                                                          
*                                                                               
         TM    TRNRSTAT,TRNSDRFT   IS THIS A DRAFT ?                            
         BO    GETTOT1             YES, SKIP IT                                 
*                                                                               
         CLI   TRNTYPE,57          SKIP WRITEOFFS                               
         BE    GETTOT1                                                          
         DROP  R3                                                               
*                                                                               
         CLC   TRNKWORK,=C'99'     SKIP BILLING                                 
         BE    GETTOT1                                                          
         CLC   TRNKWORK,=C'**'     SKIP ORDERS                                  
         BE    GETTOT1                                                          
         CLC   TRNKCCPY,TRNKCPY    SAME COMPANY CODE FOR ACCOUNT AND            
         BNE   GETTOT1             CONTRA ? NO, NEXT RECORD                     
         BAS   RE,GETALL                                                        
         BAS   RE,FILTRN           FILTER THE TRANSACTION                       
*                                  WAS  TRANSACTION FILTERED OUT ?              
         BNE   GETTOT1             YES, LOOP BACK                               
*                                                                               
         AP    AMTOT,BILNET        ADD  TRANSACTION TO ACCUMULATOR              
         B     GETTOT1             LOOP BACK UP                                 
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO FILTER TRANSACTION                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         USING TRNELD,R3           MAP  TRANSACTION ELEMENT                     
         SPACE 1                                                                
FILTRN   NTR1                                                                   
         L     R2,AIOAREA1                                                      
         LA    R3,ACCORFST(,R2)    GET  1ST ELEMENT                             
*                                                                               
         CP    BILNET,=P'0'        IF   BILLABLE AMOUNT                         
         BNE   FILTR02                                                          
         OC    BILHRS,BILHRS       OR   BILLABLE HOURS                          
         BNZ   FILTR02                                                          
         CP    ALLNET,=P'0'        OR   ALLOCATED AMOUNT                        
         BNE   FILTR02                                                          
         OC    ALLHRS,ALLHRS       OR   ALLOACATED HOURS - TAKE IT              
         BZ    FILTNO              ELSE SKIP IT                                 
*                                                                               
FILTR02  OC    BILDATE,BILDATE                                                  
         BZ    FILTR03                                                          
         CLC   TRNDATE,BILDATE     TRANSACTION DATE                             
         BNE   FILTNO                                                           
*                                                                               
FILTR03  OC    BILMOS,BILMOS                                                    
         BZ    FILTR03A                                                         
         CLC   TRNBTCH(2),BILMOS   MONTH OF SERVICE                             
         BNE   FILTNO                                                           
*                                                                               
FILTR03A OC    SDATE,SDATE                                                      
         BZ    FILTR03B                                                         
         CLC   TRNDATE,SDATE       START DATE                                   
         BL    FILTNO                                                           
*                                                                               
FILTR03B OC    EDATE,EDATE                                                      
         BZ    FILTR03C                                                         
         CLC   TRNDATE,EDATE       END DATE                                     
         BH    FILTNO                                                           
*                                                                               
FILTR03C OC    WCF,WCF                                                          
         BZ    FILTR03D                                                         
         CLC   TRNANAL,WCF         WORKCODE                                     
         BE    FILTR04                                                          
*                                                                               
FILTR03D TM    WCF,X'40'                                                        
         BO    FILTNO                                                           
         MVC   HALF,WCF            NEGATIVE FILTER                              
         OI    HALF,X'40'                                                       
         CLC   TRNANAL,HALF                                                     
         BE    FILTNO              ALL BUT                                      
*                                                                               
FILTR04  CLI   ALLCSW,C'Y'         WANT ALLOCATED ONLY                          
         BNE   FILTR06                                                          
         CP    ALLNET,=P'0'        ALLOCATED CASH                               
         BNE   FILTR08                                                          
         OC    ALLHRS,ALLHRS       ALLOCATED HOURS                              
         BNZ   FILTR08                                                          
         B     FILTNO                                                           
*                                                                               
FILTR06  CLI   ALLCSW,C'N'         WANT UNALLOCATED ONLY                        
         BNE   FILTR08                                                          
         CP    ALLNET,=P'0'        HAS  ALLOCATED CASH                          
         BNE   FILTNO                                                           
         OC    ALLHRS,ALLHRS       HAS  ALLOCATED HOURS                         
         BNZ   FILTNO                                                           
*                                                                               
FILTR08  CLI   LABSW,C'Y'          LABOR ONLY                                   
         BNE   FILTR12             CHECK FOR OTHER OPTION                       
         CLI   PFLAB,C'Y'          TREAT SK AS LABOR ?                          
         BNE   FILTR10             YES,  TAKE 1R & SK ACCTS                     
         CLC   TRNKCUNT(2),=C'SK'  AN    SK   ACCNT ?                           
         BE    FILTR16                                                          
*                                                                               
FILTR10  OC    BILHRS,BILHRS                                                    
         BZ    FILTNO              THIS HAS NO LABOR                            
         B     FILTR16                                                          
*                                                                               
FILTR12  CLI   LABSW,C'N'          NON-LABOR ONLY                               
         BNE   FILTR16                                                          
         CLI   PFLAB,C'Y'          TREAT SK AS LABOR?                           
         BNE   FILTR14                                                          
         CLC   TRNKCUNT(2),=C'SK'                                               
         BE    FILTNO                                                           
*                                                                               
FILTR14  CLC   TRNKCUNT(2),=C'1R'                                               
         BE    FILTNO                                                           
         OC    BILHRS,BILHRS                                                    
         BNZ   FILTNO              THIS HAS NO LABOR                            
*                                                                               
FILTR16  CLI   PARTSW,C'Y'         ONLY PARTIALLY ALLOCATED                     
         BNE   FILTR18                                                          
         CP    BILNET,ALLNET       BILLABLE  =    ALLOCATED ?                   
         BE    FILTNO              YES, THEN NOT  PARTIAL                       
         CP    ALLNET,=P'0'        NOT  ALLOCATED ?                             
         BE    FILTNO              YES, THEN NOT  PARTIAL                       
*                                                                               
FILTR18  CLI   PARTSW,C'N'         EXCLUDE   PARTIALLY ALLOCATED ?              
         BNE   FILTR20                                                          
         CP    ALLNET,=P'0'        ZERO IS   NOT  PARTIAL                       
         BE    FILTR20                                                          
         CP    BILNET,ALLNET       BILLABLE  =    ALLOCATED ?                   
         BNE   FILTNO              NO,  IT'S PARTIAL - SO EXCLUDE IT            
*                                                                               
FILTR20  OC    WOTYP,WOTYP         FILTERING BY WORKCODE TYPE ?                 
         BZ    FILTR26             NO                                           
         OC    LASTWC,LASTWC       DO   WE   HAVE A WORKCODE ALREADY ?          
         BZ    FILTR22             NO,  GET  SET  TYPE                          
         CLC   LASTWC,TRNKWORK     YES, DID  IT   CHANGE ?                      
         BE    FILTR24             NO,  JUST CHECK IT THEN                      
*                                                                               
FILTR22  MVC   KEY,SPACES          GET  WORKCODE  TYPE                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),JOBKEY     GET  CUL                                     
         MVC   KEY+4(2),TRNKWORK                                                
         GOTO1 ARDHI,AIOAREA2                                                   
         CLC   KEY,KEYSAVE         FIND IT ?                                    
         BE    *+6                 YOU  MUST                                    
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA2                                                      
         MVI   ELCODE,X'12'        GET TYPE                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING WCOELD,R2                                                        
         MVC   HOLDTYP,WCOTYPE     SAVE WORKCODE TYPE                           
*                                                                               
         MVC   KEY,SPACES          RE-READ PREVIOUS RECORD                      
         L     R2,AIOAREA1                                                      
         MVC   KEY,0(R2)                                                        
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   HOLDTYP,C' '        NO, IS THERE A TYPE TO MATCH TO ?            
         BH    *+8                 YES                                          
         MVI   HOLDTYP,C'O'        NO, FORCE IT TO BE O                         
*                                                                               
FILTR24  CLC   WOTYP,HOLDTYP       DO TYPES MATCH ?                             
         BE    FILTR26             YES                                          
         TM    WOTYP,X'40'         NO, IS IT EXCLUSIVE ?                        
         BO    FILTNO              NO                                           
         MVC   BYTE,WOTYP          YES                                          
         OI    BYTE,X'40'                                                       
         CLC   BYTE,HOLDTYP                                                     
         BE    FILTNO                                                           
*                                                                               
FILTR26  OC    BATYP,BATYP         FILTERING BY BATCH TYPE ?                    
         BZ    FILTR28             NO                                           
         SR    R1,R1                                                            
         IC    R1,TRNTYPE                                                       
         CVD   R1,DUB                                                           
         UNPK  WORK(2),DUB+6(2)                                                 
         OI    WORK+1,X'F0'                                                     
         CLC   BATYP,WORK          DO BATCHES MATCH ?                           
         BE    FILTYES             YES                                          
         TM    BATYP,X'40'         NO, IS IT EXCLUSIVE ?                        
         BO    FILTNO              NO                                           
         MVC   HALF,BATYP          YES                                          
         OI    HALF,X'40'                                                       
         CLC   HALF,WORK                                                        
         BE    FILTNO                                                           
*                                                                               
FILTR28  OC    OFFOPT,OFFOPT       FILTERING BY OFFSET STATUS ?                 
         BZ    FILTYES             NO                                           
         TM    TRNSTAT,X'20'       IS TRANSACTION REVERSED ?                    
         BZ    FILTR30             NO                                           
         CLI   OFFOPT,C'Y'         WANT OFFSETS ONLY ?                          
         B     *+8                 YES                                          
*                                                                               
FILTR30  CLI   OFFOPT,C'N'         WANT NON-OFFSETS ONLY ?                      
         BNE   FILTNO              SKIP THIS ONE                                
*                                                                               
FILTYES  B     OKXIT                                                            
*                                                                               
FILTNO   B     ERRXIT                                                           
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* DISPLAY WORK CODE TOTAL                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING LINED,R4                                                         
         SPACE 1                                                                
DSPWCT   NTR1                                                                   
         OC    LASTWC,LASTWC                                                    
         BZ    DSPWCTX                                                          
         ST    R6,SAVR6            SAVE R6                                      
         MVC   LINWC(LINDLNQ),SPACES                                            
         MVC   LINSUP(2),=C'**'    **                                           
*                                  SPACE                                        
         MVC   LINSUP+3(19),AC$TWC TOTAL FOR WORKCODE                           
*                                                                               
         LA    R6,LINSUP+3                                                      
         LA    R1,19               19   BYTE FIELD                              
         BAS   RE,FINDFREE         FIND 1ST  FREE BYTE                          
         LA    R6,1(,R6)           SPACE                                        
         MVC   0(2,R6),LASTWC      WORK CODE                                    
*                                  SPACE                                        
         MVC   3(2,R6),=C'**'      **                                           
*                                                                               
         EXTED WRKTOT,LINBIL,2,FLOAT=-                                          
         OI    LINAMTH+1,X'28'     PROTECT/HIGH INSENSITY AMOUNT                
         OI    LINDH+1,X'08'       HIGH INTENSITY                               
*                                                                               
         AP    TOTBILWC,WRKTOT     ADD  TO OVERALL TOTAL                        
*                                                                               
         LA    R2,LASTWC                                                        
         BAS   RE,WKCUP            FIND ENTRY FOR THIS WORKCODE                 
*                                                                               
         USING ALLTD,R5                                                         
         ZAP   DUB,ALLTAMT         ALLOCATED TOTAL                              
         EXTED ALLTAMT,LINAMT,2,FLOAT=-                                         
         OI    LINAMTH+6,FVOXMT    TRANSMIT                                     
*                                                                               
         ZAP   WRKTOT,=P'0'                                                     
         ZIC   R0,LINES                                                         
         AH    R0,=H'1'                                                         
         STC   R0,LINES                                                         
         LA    R4,LINLNQ(,R4)                                                   
*                                                                               
         USING KEYTABD,R6                                                       
         L     R6,SAVR6            RESTORE R6                                   
         OI    KEYTBSW,KEYTBWCT    WORK CODE TOTAL INDICATOR                    
         MVC   KEYTBWC,LASTWC      WORK CODE                                    
         LA    R6,KEYTABLQ(,R6)                                                 
         XC    LASTWC,LASTWC                                                    
*                                                                               
DSPWCTX  XIT1  REGS=(R4,R6)                                                     
         DROP  R4,R5,R6                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY TOTAL ALL WORKCODES                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING LINED,R4                                                         
         USING KEYTABD,R6                                                       
         SPACE 1                                                                
TOTWCT   NTR1                                                                   
*                                  TOTAL DISPLAYED WORK CODES                   
         LA    R2,LINBIL-LINSUP-1                                               
         GOTO1 ATXTGET,DMCB,('FVMINFO',2112),((R2),LINSUP),0,0                  
         EXTED TOTBILWC,LINBIL,2,FLOAT=-                                        
         EXTED TOTALLWC,LINAMT,2,FLOAT=-                                        
         OI    LINAMTH+1,X'28'     PROTECT/HIGH INSENSITY AMOUNT                
         OI    LINDH+1,X'08'       HIGH INTENSITY                               
         ZIC   RF,LINES                                                         
         LA    RF,1(,RF)                                                        
         STC   RF,LINES                                                         
         LA    R4,LINLNQ(,R4)                                                   
         OI    KEYTBSW,KEYTBTOT    OVERALL TOTAL INDICATOR                      
         LA    R6,KEYTABLQ(,R6)                                                 
*                                                                               
TOTWCTX  DS    0H                                                               
         XIT1  REGS=(R4,R6)                                                     
*                                                                               
         DROP  R4,R6                                                            
         EJECT ,                                                                
***********************************************************************         
* DISPLAY SCREEN TOTALS                                               *         
***********************************************************************         
         SPACE 1                                                                
DSPTOT   NTR1                                                                   
         MVC   ALCTOT,SPACES                                                    
         OI    ALCTOTH+6,FVOXMT    TRANSMIT                                     
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
*                                                                               
         LA    R6,TEMP                                                          
         MVC   0(7,R6),AC$CHGS     CHARGES                                      
         LA    R1,7                                                             
         BAS   RE,FINDFREE                                                      
*                                                                               
         MVI   0(R6),C'='          =                                            
         LA    R6,1(,R6)           NUMBER                                       
         EXTED TOTNET,(13,0(R6)),2,MINUS=YES,ALIGN=LEFT                         
         LA    R1,12                                                            
         BAS   RE,FINDFREE                                                      
*                                                                               
         CP    TOTBIL,=P'0'        TOTAL     BILL = 0 ?                         
         BE    DSPTOT2             YES, SKIP                                    
         MVI   0(R6),C','          COMMA                                        
         LA    R6,1(,R6)                                                        
         MVC   0(7,R6),AC$BLD      BILLED                                       
         LA    R1,7                                                             
         BAS   RE,FINDFREE                                                      
*                                                                               
         MVI   0(R6),C'='          =                                            
         LA    R6,1(,R6)           NUMBER                                       
         EXTED TOTBIL,(13,0(R6)),2,MINUS=YES,ALIGN=LEFT                         
         LA    R1,12                                                            
         BAS   RE,FINDFREE                                                      
*                                                                               
DSPTOT2  ZAP   DUB,TOTNET          TOTAL LESS                                   
         SP    DUB,TOTBIL          BILLED                                       
         MVI   0(R6),C','          COMMA                                        
         LA    R6,1(,R6)                                                        
         MVC   0(9,R6),AC$9BLB     BILLABLE                                     
         LA    R1,9                                                             
         BAS   RE,FINDFREE                                                      
*                                                                               
         MVI   0(R6),C'='          =                                            
         LA    R6,1(,R6)           NUMBER                                       
         EXTED (P8,DUB),(13,0(R6)),2,MINUS=YES,ALIGN=LEFT                       
         LA    R1,12                                                            
         BAS   RE,FINDFREE                                                      
*                                                                               
         CP    TOTALL,=P'0'        TOTAL ALLOCATED = 0 ?                        
         BE    DSPTOT4             YES, SKIP                                    
         MVI   0(R6),C','          COMMA                                        
         LA    R6,1(,R6)                                                        
         MVC   0(10,R6),AC$AALCT   ALLOCATED                                    
         LA    R1,10                                                            
         BAS   RE,FINDFREE                                                      
*                                                                               
         MVI   0(R6),C'='          =                                            
         LA    R6,1(,R6)           NUMBER                                       
         EXTED TOTALL,(13,0(R6)),2,MINUS=YES,ALIGN=LEFT                         
         LA    R1,12                                                            
         BAS   RE,FINDFREE                                                      
*                                                                               
DSPTOT4  MVC   ALCTOT,TEMP                                                      
*                                  NOTE: THERE IS A SMALL CHANCE THAT           
*                                        THE MESSAGE BUILT DID NOT FIT          
*                                        INTO ALCTOT                            
         LA    RE,TEMP             DOES  THE WHOLE MESSAGE FIT ?                
         SR    R6,RE                                                            
         CH    R6,=AL2(L'ALCTOT)                                                
         BNH   DSPTOTX             FITS, EXIT                                   
         MVI   ALCTOT+L'ALCTOT-1,C'>'                                           
*                                                                               
DSPTOTX  B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* FIND ENTRY FOR WORK CODE (R2)                                       *         
* RETURN ADDRESS IN R5                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TWA1D,RF                                                         
         USING CHRD,R5                                                          
         SPACE 1                                                                
WKCUP    NTR1                                                                   
         L     RF,ATWA1                                                         
         LA    R5,CHARGES          SEE IF ROOM IN CHARGES TABLE                 
         DROP  RF                                                               
*                                                                               
         ZIC   R0,CHRCNT                                                        
         LTR   R0,R0                                                            
         BZ    WKCUP04             NOTHING IN TABLE - YET                       
*                                                                               
WKCUP02  CLC   CHRWK,0(R2)         MATCH WORKCODE                               
         BE    WKCUP06             ALREADY IN TABLE - NO NEED TO UPDATE         
         LA    R5,CHRLEN(,R5)                                                   
         BCT   R0,WKCUP02                                                       
*                                                                               
WKCUP04  ZIC   R0,CHRCNT           NOT IN TABLE                                 
         AH    R0,=H'1'            MUST ADD AND UPDATE COUNT                    
         LA    R1,CHRGMX                                                        
         CR    R0,R1                                                            
         BH    TOOCHR              TABLE IS ALREADY FULL                        
         STC   R0,CHRCNT                                                        
         MVC   CHRWK,0(R2)                                                      
*                                                                               
         USING ALLTD,R5                                                         
WKCUP06  LA    R5,ALLTAB                                                        
         ZIC   R0,ALLTCNT                                                       
         LTR   R0,R0                                                            
         BZ    WKCUP10                                                          
*                                                                               
WKCUP08  CLC   ALLTWC,0(R2)        FIND WORK CODE IN TABLE                      
         BE    WKCUPX                                                           
         LA    R5,ALLTLEN(,R5)                                                  
         BCT   R0,WKCUP08                                                       
*                                                                               
WKCUP10  MVC   ALLTWC(2),0(R2)     SET-UP NEW ENTRY                             
         ZAP   ALLTAMT,=P'0'                                                    
         ZIC   R0,ALLTCNT                                                       
         AH    R0,=H'1'                                                         
         STC   R0,ALLTCNT                                                       
*                                                                               
WKCUPX   XIT1  REGS=(R5)                                                        
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* SET-UP NEGATIVE ELEMENTS FOR REVERSE                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         USING PTAELD,R3           MAP  PROD TRANSACTION ACTIVITY               
         SPACE 1                                                                
SETREV   NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         L     R2,AIOAREA1                                                      
         L     R3,APROLST                                                       
         CLI   0(R3),PTAELQ                                                     
         B     SETREV2+8                                                        
*                                                                               
SETREV2  MVI   ELCODE,PTAELQ       LOOK FOR 77'S TO REVERSE/DELETE              
         BAS   RE,NEXTEL3                                                       
         BNE   SETREV6                                                          
*                                                                               
         CLI   PTATYPE,0           IS THIS A SPARE?                             
         BE    SETREV4             YES, REMOVE IT                               
         CLI   PTATYPE,PTATRAL     YES, IS IT A BILLING ALLOCATION?             
         BNE   SETREV2             NO, KEEP LOOKING                             
         TM    PTASTAT1,PTASPEND   YES, IS IT PENDING?                          
         BO    SETREV4             YES, REMOVE IT                               
         TM    PTASTAT1,PTASREVS+PTASREVU+PTASREVD                              
         BNZ   SETREV2                                                          
*                                                                               
         CLC   PTARBLNO,BILNUM     IS THIS THE ONE WE WANT?                     
         BNE   SETREV2             NO, GET THE NEXT                             
*                                                                               
         ZIC   R1,PTALN                                                         
         BCTR  R1,0                                                             
         EXMVC R1,ELEMENT,PTAEL    SAVE ITEM TO BE REVERSED                     
         B     SETREV2                                                          
*                                                                               
SETREV4  MVI   0(R3),X'FF'         MARK UNUSED FOR DELETION                     
         MVI   BYTE,C'W'           FLAG FOR WRITE                               
         B     SETREV2                                                          
*                                                                               
SETREV6  CLI   ELEMENT,0                                                        
         BE    EXIT                NOTHING TO ALLOCATE                          
*                                                                               
         L     R3,APROLST                                                       
SETREV7  BAS   RE,NEXTEL3          LOOK FOR END OF ELEMENTS                     
         BE    SETREV7                                                          
         EXMVC R1,0(R3),ELEMENT                                                 
         LA    R1,1(R1,R3)                                                      
         MVI   0(R1),0                                                          
*                                                                               
         OI    PTASTAT1,PTASPEND   MAKE IT PENDING                              
         XC    PTARBLNO,PTARBLNO   CLEAR BILL NUMBER                            
*                                                                               
*                                  REVERSE AMOUNTS                              
         ZAP   DUB5,PTANET         .       NET                                  
         MP    DUB5,=P'-1'         ..                                           
         ZAP   PTANET,DUB5         ..                                           
         ZAP   DUB5,PTACDSC        .       CASH DISCOUNT                        
         MP    DUB5,=P'-1'         ..                                           
         ZAP   PTACDSC,DUB5        ..                                           
         ZAP   DUB5,PTARCOM        .       COMMISSION                           
         MP    DUB5,=P'-1'         ..                                           
         ZAP   PTARCOM,DUB5        ..                                           
*                                                                               
         LH    RE,PTAHOURS                                                      
         LCR   RE,RE                                                            
         STH   RE,PTAHOURS                                                      
*                                                                               
         AP    SCIUNET,PTANET      TOTAL THE NET AND COMMISSION                 
         AP    SCIUCOM,PTARCOM                                                  
*                                                                               
         MVI   BYTE,C'W'           FLAG FOR WRITE                               
         B     EXIT                                                             
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* GET ALLOCATED PORTION OF HOURS, C.D. ETC                            *         
*                                                                     *         
*   INPUT:                                                            *         
*     TRNNET - TRANSACTION      AMOUNT (FROM TRNAMNT)                 *         
*     MKAMT  - MARKED           AMOUNT                                *         
*     PLTEMP - SUBSIDIARY  CASH AMOUNT (FROM SCIAMNT)                 *         
*                                                                     *         
*   OUTPUT:                                                           *         
*     PLTEMP - WAS FULL                                               *         
*                                                                     *         
*   USING:                                                            *         
*     PK16   - WORK AREA                                              *         
***********************************************************************         
         SPACE 1                                                                
DIV      NTR1                                                                   
         ZAP   PK16,MKAMT          AMOUNT  ALLOCATED THIS TIME                  
         MP    PK16,PLTEMP         TIMES   HOURS OR C.D.                        
         MP    PK16,=P'10'         TIMES   10                                   
         DP    PK16,TRNNET(6)      DIVIDED BY NET                               
         SRP   PK16(10),64-1,5     DIVIDE  BY 10 AND ROUND                      
         ZAP   PLTEMP,PK16(10)     HOURS   C.D. THIS TIME                       
         B     EXIT                                                             
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* INITIALIZE OVRTWA                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING OVRTWAD,R7                                                       
         SPACE 1                                                                
INITOTWA DS    0H                  INITIALIZE     OVRTWA                        
         ST    RE,SAVRE            SAVE      RE                                 
         LR    RE,R7               CLEAR     SAVE STORAGE                       
         LA    RF,L'OVRTWA                                                      
         XCEF                                                                   
*                                  INITIALIZE     PACKED    FIELDS              
         ZAP   WRKTOT,=P'0'                                                     
         ZAP   PERCENT,=P'0'                                                    
         ZAP   TOTBILWC,=P'0'                                                   
         ZAP   TOTALLWC,=P'0'                                                   
*                                                                               
         L     RE,SAVRE            RESTORE   RE                                 
         BSM   0,RE                RETURN                                       
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* CALL DICTATE                                                        *         
*                                                                     *         
* INPUT:                                                              *         
*   R6=  ADDRESS OF FIELD TO BE TRANSLATED                            *         
*        NOTE: THE FIELD MUST HAVE BEEN PREVIOUSLY INITIALIZED WITH   *         
*              AN MVCDD INSTRUCTION.                                  *         
***********************************************************************         
         SPACE 1                                                                
CALLDICT DS    0H                                                               
         ST    RE,SAVRE            SAVE     RE                                  
         GOTO1 VDICTAT,DMCB,C'SL  ',(R6),0                                      
         L     RE,SAVRE            RESTORE  RE                                  
         BSM   0,RE                RETURN                                       
         EJECT ,                                                                
***********************************************************************         
* FIND FIRST AVAILABLE (BYTE)                                         *         
*                                                                     *         
*   INPUT:                                                            *         
*     R1 = NUMBER  OF BYTES TO CONSIDER                               *         
*     R6 = ADDRESS OF FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
FINDFREE AR    R6,R1               FIND LAST BYTE IN   FIELD                    
         BCTR  R6,0                                                             
*                                                                               
FINDFR10 CLI   0(R6),C' '          FIND LAST CHARACTER                          
         BH    FINDFR20                                                         
         BCTR  R6,0                                                             
         BCT   R1,FINDFR10                                                      
*                                                                               
FINDFR20 LA    R6,1(,R6)           1ST  FREE BYTE                               
         BSM   0,RE                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
* ERROR ROUTINES - EXIT QUICKLY WHEN ANY OF THESE ERRORS OCCUR        *         
***********************************************************************         
         SPACE 1                                                                
         USING OVRTWAD,R7                                                       
         SPACE 1                                                                
*                                  RECORD CHANGED AT ANOTHER TERMINAL,          
RDMKREVS MVC   FVMSGNO,=AL2(2224)    RESTART ALLOCATE                           
         BAS   RE,INITOTWA         CLEAR     OVRTWA                             
         LR    R1,R4               SET  CURSOR                                  
         B     SETERR1                                                          
*                                                                               
*                                  ************************************         
*                                  * THE TEXT FOR THESE ERRORS IS:    *         
*                                  *   ERROR - ...                    *         
*                                  ************************************         
*                                                                               
TOOCHR   MVC   FVMSGNO,=AL2(2208)  TOO MANY ALLOCATED CHARGES ON JOB            
         LA    R1,BILJOBH                                                       
         B     SETERR1                                                          
*                                                                               
ZEROCH   MVC   FVMSGNO,=AL2(2212)  JOB HAS ALLOCATED CHARGES                    
         B     SETERR                                                           
*                                                                               
MAXALL   MVC   FVMSGNO,=AL2(2235)  CAN'T ALLOCATE - RECORD TOO BIG              
         B     SETERR1                                                          
*                                                                               
NUM2BIG  MVC   FVMSGNO,=AL2(2213)  AMOUNT TOO LARGE                             
*                                                                               
SETERR   LA    R1,BILACTH                                                       
*                                                                               
SETERR1  MVI   FVMTYPE,FVMERR      MSG  TYPE ERROR                              
         ST    R1,FADR             SET  CURSOR                                  
         GOTO1 AERRORX2            EXIT QUICKLY                                 
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         GETEL R2,DATADISP,ELCODE                                               
         GETELN R3,DATADISP,ELCODE,3                                            
         SPACE 2                                                                
*                                                                               
* EXITS TO ROOT                                                                 
*                                                                               
OKEND    MVI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
OKXIT    SR    RB,RB               CC = EQU                                     
*                                                                               
ERRXIT   LTR   RB,RB               CC = NEQ                                     
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
DICTB06  DS    0X                  LOCAL DICTIONARY ITEMS                       
         DCDDL AC#ALCT2,L'ALCHEDA  ALLOCATED                                    
*                                  ALLOCATED                                    
         DCDDL AC#ALCT2,10,LABEL=AC@ALCTB                                       
         DCDDL AC#AMT,L'LINBIL,R   AMOUNT                                       
*                                  BILLABLE                                     
         DCDDL AC#BLB,L'LINHRS+1+L'LINRTE+1+L'LINBIL,C                          
*                                  BILLABLE                                     
         DCDDL AC#BLB,9,LABEL=AC@BLB2                                           
         DCDDL AC#BLD,7            BILLED                                       
         DCDDL AC#CHGS,7           CHARGES                                      
         DCDDL AC#DATE,L'LINDT     DATE                                         
         DCDDL AC#HOURS,L'LINHRS,R HOURS                                        
         DCDDL AC#INV,L'LINDT      INVOICE                                      
*                                  INVOICE                                      
         DCDDL AC#INV,L'LININV+1,LABEL=AC@INV2                                  
         DCDDL AC#NUM,L'LININV     NUMBER                                       
         DCDDL AC#RATE,L'LINRTE,R  RATE                                         
         DCDDL AC#SUPC,8           SUPPLIER                                     
         DCDDL AC#TWC,19           TOTAL FOR WORK CODE                          
         DCDDL AC#WC2,3            W/C                                          
*                                                                               
DICTB06X DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO GET COMMISSION RULES                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING LINED,R4                                                         
         USING GOBLOCK,R7                                                       
GETRUL   NTR1  BASE=*,LABEL=*                                                   
         L     R7,AGOBLOCK                                                      
         MVC   GOAKEY,AIOAREA1                                                  
         MVC   GOADM,VDATAMGR      SET-UP GOBLOCK                               
         MVC   GOSELCUL,JOBKEY                                                  
*        MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELCLI,LCLI                                                    
*        MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELPRO,LPRO                                                    
*        MVC   GOSELJOB,SPACES                                                  
         MVC   GOSELJOB,LJOB                                                    
         MVC   GOACOMP,ADCMP                                                    
         MVC   GOACLI,ADCLI                                                     
         MVC   GOAPRO,ADPRD                                                     
         MVC   GOAJOB,ADJOB                                                     
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
         MVC   GOSELWC,LINWC                                                    
         GOTO1 VGETOPT,DMCB,AGOBLOCK                                            
         ZAP   DUB,GOAGYCOM                                                     
         XIT1                                                                   
         DROP  R4,R7                                                            
                                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
SVRTE    DS    D                                                                
SVHRS    DS    D                                                                
*                                                                               
         DS    0D                                                               
DUB4     DS    CL16                                                             
DUB5     DS    D                   DOUBLE    WORD WORK AREA                     
*                                                                               
SAVRE    DS    F                   SAVE AREA FOR  RE                            
SAVR6    DS    F                   SAVE AREA FOR  R6                            
*                                                                               
TRNHRS   DS    F                   TRANSACTION HOURS                            
BILHRS   DS    F                   BILLABLE    HOURS                            
ALLHRS   DS    F                   ALLOACTED   HOURS                            
*                                                                               
MKHRS    DS    F                   HOURS THIS ALLOCATION                        
INHRS    DS    F                   INPUT HOURS                                  
*                                                                               
TRNCSD   DS    PL(L'SCIAMNT)       TRANSACTION CASH DISCOUNT                    
TRNNET   DS    PL(L'TRNAMNT)                   NET                              
BILRTE   DS    PL4                 BILLABLE    HOURLY RATE                      
BILCSD   DS    PL8                             CASH DISCOUNT                    
BILNET   DS    PL8                             NET                              
ALLCSD   DS    PL8                 ALLOCATED   CASH DISCOUNT                    
ALLNET   DS    PL8                             NET                              
*                                                                               
MKAMT    DS    PL8                 AMOUNT THIS ALLOCATION                       
*                                                                               
AMTOT    DS    PL8                                                              
*                                                                               
PLTEMP   DS    PL8                                                              
*                                                                               
ERROR    DS    XL1                 ERROR FLAG                                   
DATASW   DS    C                   INDICATE DATA READ                           
LINES    DS    CL1                                                              
COMMSW   DS    CL1                                                              
UPDSW    DS    CL1                                                              
*                                                                               
DICLS06  DS    0X                  LOCAL DICTIONARY ITEMS                       
AC$ALCT2 DS    CL(L'ALCHEDA)       ALLOCATED                                    
AC$AALCT DS    CL(10)              ALLOCATED                                    
AC$AMT   DS    CL(L'LINBIL)        AMOUNT                                       
*                                  BILLABLE                                     
AC$BLB   DS    CL(L'LINHRS+1+L'LINRTE+1+L'LINBIL)                               
AC$9BLB  DS    CL9                 BILLABLE                                     
AC$BLD   DS    CL7                 BILLED                                       
AC$CHGS  DS    CL7                 CHARGES                                      
AC$DATE  DS    CL(L'LINDT)         DATE                                         
AC$HOURS DS    CL(L'LINHRS)        HOURS                                        
AC$INV   DS    CL(L'LINDT)         INVOICE                                      
AC$7INV  DS    CL(L'LININV+1)      INVOICE                                      
AC$NUM   DS    CL(L'LININV)        NUMBER                                       
AC$RATE  DS    CL(L'LINRTE)        RATE                                         
AC$SUPC  DS    CL8                 SUPPLIER                                     
AC$TWC   DS    CL19                TOTAL FOR WORK CODE                          
AC$WC2   DS    CL3                 W/C                                          
*                                                                               
DICLS06X DS    AL1                 EOT                                          
*                                                                               
FRSTPTA  DS    A                                                                
*                                                                               
LWSX     DS    0C                                                               
*                                                                               
MAXLNS   EQU   13                                                               
DSPLY    EQU   2                                                                
MARK     EQU   3                                                                
         EJECT ,                                                                
***********************************************************************         
* DSECT FOR LOCAL SAVE AREA (OVRTWA)                                  *         
***********************************************************************         
         SPACE 1                                                                
OVRTWAD  DSECT                                                                  
WRKTOT   DS    PL8                 WORKCODE(NET) TOTAL BILLABLE                 
TOTBILWC DS    PL8                 TOTAL ALL WORKCODES - BILLABLE               
TOTALLWC DS    PL8                 TOTAL ALL WORKCODES - ALLOCATED              
*                                                                               
OVMODE   DS    CL1                                                              
LASTKEY  DS    CL42                                                             
PERCENT  DS    PL(PLAMTLNQ)                                                     
LASTWC   DS    CL2                 LAST WORK CODE                               
ALLTCNT  DS    CL1                 NUMBER IN ALLTAB                             
*                                                                               
*                                  IND/DATE/SUBREF/BILLABLE/ALLOCATED           
*                                  OF EACH SCREEN LINE                          
KEYTAB   DS    CL((MAXLNS*KEYTABLQ)+1)                                          
*                                                                               
ALLTAB   DS    (CHRGMX*ALLTLEN)C   TABLE OF ALLOCATED CHARGES                   
*                                                                               
OVRTWAQ  EQU   *-OVRTWAD                                                        
         EJECT ,                                                                
***********************************************************************         
* DSECT FOR SCREEN DISPLAY LINE                                       *         
***********************************************************************         
         SPACE 1                                                                
LINED    DSECT                                                                  
LINDH    DS    CL8                 HEADER FOR DATA LINE                         
LINWC    DS    CL2                 WORK-CODE                                    
         DS    CL1                                                              
LINSUP   DS    CL14                SUPPLIER                                     
         DS    CL1                                                              
LININV   DS    CL6                 INVOICE NUMBER                               
         DS    CL1                                                              
LINSTA   DS    CL1                 * IF REVERSAL                                
LINDT    DS    CL8                 INVOICE DATE                                 
         DS    CL1                                                              
LINHRS   DS    CL7                 HOURS                                        
         DS    CL1                                                              
LINRTE   DS    CL8                 RATE                                         
         DS    CL1                                                              
LINBIL   DS    CL11                BILLABLE                                     
         DS    CL1                                                              
LINDLNQ  EQU   *-LINWC                                                          
LINAMTH  DS    CL8                 AMOUNT HEADER                                
LINAMT   DS    CL11                AMOUNT                                       
LINAMTX  DS    CL8                 AMOUNT TRAILER                               
LINLNQ   EQU   *-LINDH                                                          
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER ENTRY IN TABLE OF ALLOCATED TOTALS                   *         
***********************************************************************         
         SPACE 1                                                                
ALLTD    DSECT                                                                  
ALLTWC   DS    CL2                 WORKCODE                                     
ALLTAMT  DS    PL8                 AMOUNT                                       
ALLTLEN  EQU   *-ALLTD                                                          
         EJECT ,                                                                
***********************************************************************         
* DSECT TO KEY TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
KEYTABD  DSECT                                                                  
KEYTBSW  DS    CL1                 INDICATOR                                    
KEYTBINU EQU   X'80'               .    IN   USE                                
KEYTBWCT EQU   X'40'               .    WORK CODE TOTAL                         
KEYTBTOT EQU   X'20'               .    OVER ALL  TOTAL                         
*                                                                               
KEYTBWC  DS    CL2                 WORK CODE                                    
KEYTBDAT DS    PL3                 DATE                                         
KEYTBBIL DS    PL(PLAMTLNQ)        BILLABLE                                     
KEYTBALC DS    PL(PLAMTLNQ)        ALLOCATED                                    
KEYTBSBR DS    XL1                 SUB  REFERENCE                               
*                                                                               
KEYTABLQ EQU   *-KEYTABD           LENGTH    OF   ENTRY                         
         EJECT ,                                                                
         SPACE 1                                                                
*ACBILDSECT                                                                     
       ++INCLUDE ACBILDSECT                                                     
*ACBILWORK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACBILWORK                                                      
         PRINT ON                                                               
         EJECT ,                                                                
         SPACE 1                                                                
*ACBILFAD                                                                       
       ++INCLUDE ACBILFAD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACBIL06   12/17/12'                                      
         END                                                                    
