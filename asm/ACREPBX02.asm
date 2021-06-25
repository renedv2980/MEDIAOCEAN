*          DATA SET ACREPBX02  AT LEVEL 116 AS OF 11/04/11                      
*PHASE ACBX02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'ACBX02 - TRANSACTION REPORT AND EDI OUTPUT'                     
ACBX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACBX**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACBXD,RC             ACREPBXD                                    
         USING PRND,P                                                           
*                                                                               
         CLI   MODE,RUNFRST        FIRST TIME FOR A RUN?                        
         BE    RUNF                 Y, DO RUN INITIALIZATION                    
         CLI   MODE,RUNLAST        LAST TIME FOR A RUN?                         
         BE    RUNL                 Y,                                          
         CLI   MODE,REQFRST        FIRST TIME FOR A REQUEST?                    
         BE    REQF                 Y, DO REQUEST INITIALIZATION                
         CLI   MODE,REQLAST        END OF REQUEST?                              
         BE    REQL                 Y, PRINT TRANSMISSION DIFFERENCE            
         CLI   MODE,LEVAFRST       FIRST CLIENT?                                
         BE    LEVA                 Y, PROCESS                                  
         CLI   MODE,LEVALAST       LAST CLIENT?                                 
         BE    LEVAL                N, PROCESS                                  
         CLI   MODE,LEVBFRST       FIRST PRODUCT?                               
         BE    LEVB                 Y, PROCESS                                  
         CLI   MODE,LEVBLAST       LAST PRODUCT?                                
         BE    LEVBL                N, PROCESS                                  
         CLI   MODE,PROCACC        AN ACCOUNT RECORD?                           
         BE    PRAC                 Y, PROCESS ACCOUNT LEVEL RECORD             
         CLI   MODE,ANALFRST       WORKCODE AND NAME?                           
         BE    ANALF                Y, GO GET THEM                              
         CLI   MODE,PROCTRNS       TRANSACTIONS?                                
         BE    PRTR                 Y, FIND AND ADD TO TABLE                    
         CLI   MODE,ACCLAST        LAST (NEED TO PRINT)?                        
         BE    ACCL                 Y, PRINT THE REPORT                         
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN                                                       *         
***********************************************************************         
*                                                                               
RUNF     NI    PRTFLG,X'FF'-OPENFIL                                             
         BAS   RE,GETIDI            GET THE IDI RECORD                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
*                                                                               
REQF     ZAP   RCBLAMT,=P'0'                                                    
         L     R2,AAGYTAB                                                       
REQF2    CLI   0(R2),X'FF'         MAKE SURE VALID AGENCY TO OUTPUT             
         BNE   *+12                EDI REPORT                                   
         MVI   FCRDTRNS,C'N'       CAN'T HAVE EDI REPORT                        
         B     EXIT                EDI REPORT                                   
         MVC   ADLIST,0(R2)        SAVE ADDRESS OF AGENCY TABLE                 
         CLC   AGYALPH,ALPHAID                                                  
         BNE   REQF4                                                            
         CLC   AGYCLI,SPACES       DEFAULT FOR AGENCY                           
         BE    REQF3                                                            
         CLC   AGYCLI,QACCOUNT     MATCH CLIENT                                 
         BNE   REQF4                                                            
REQF3    CLI   AGYFILT,C' '                                                     
         BE    REQF5                                                            
         CLC   AGYFILT,QFILTER5                                                 
         BE    REQF5                                                            
*                                                                               
REQF4    LA    R2,L'AGYTAB(R2)                                                  
         B     REQF2                                                            
*                                                                               
REQF5    L     R3,ADAGY            A(AGENCY DATA)                               
         MVC   AGYDATA(AGYLNQ),0(R3)       SAVE AGENCY DATA                     
*                                                                               
         MVI   FORCEHED,C'Y'       PAGE BREAK                                   
         MVC   PAGE,=H'1'          BACK TO PAGE ONE                             
         GOTO1 DATCON,DMCB,(0,QSTART),(2,STDAT2) BILL RUN DATE FILTER           
         GOTO1 DATCON,DMCB,(0,QSTART),(8,STDAT8) COMPRSSD & MMMDD/YY            
         MVC   ENDAT2,STDAT2       NO, FILTER ONLY START DATE                   
         MVC   ENDAT8,STDAT8                                                    
*                                                                               
         CLC   QEND,SPACES                                                      
         BE    REQF7                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(2,ENDAT2) BILL RUN DATE FILTER             
         GOTO1 DATCON,DMCB,(0,QEND),(8,ENDAT8) COMPRSSD & MMMDD/YY              
*                                                                               
REQF7    CLI   QOPT1,C'Y'             DO WE WANT THE EDI REPORT?                
         BNE   REQF9                   NO                                       
         TM    PRTFLG,OPENFIL         FILE ALREADY OPEN?                        
         BO    REQF9                   NO                                       
*                                                                               
         CLC   ALPHAID,TESTDDSB       IS THIS A TEST?                           
         BE    REQF8                                                            
*                                                                               
         MVC   DSPARM+13(2),ALPHAID   FILL IN TAPE DATASET NAME                 
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),DSPARM                                  
REQF8    OPEN  (EDIOUT,OUTPUT)        NO, OPEN IT                               
         OI    PRTFLG,OPENFIL                                                   
*                                                                               
REQF9    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CLIENT NAME AND ADDRESS                                             *         
***********************************************************************         
*                                                                               
LEVA     MVI   FORCEHED,C'Y'       PAGE BREAK                                   
         L     R4,ADLVANAM                                                      
         LA    R5,CLTNAM           AND THE CLIENT NAME                          
         BAS   RE,GETNAME                                                       
*                                                                               
         ZAP   CLTTOT,=P'0'        CLEAR CLIENT TOTAL                           
         MVC   CLTADD,SPACES       CLEAR ADDRESS LINES                          
*                                                                               
         ICM   R4,15,ADLVAADD      GET CLIENT ADDRESS                           
         BZ    EXIT                                                             
         USING ADRELD,R4                                                        
         SR    R3,R3                                                            
         ICM   R3,1,ADRNUM         AT LEAST ONE ADDRESS LINE?                   
         BZ    EXIT                 NO                                          
         LA    R5,CLTADD                                                        
         LA    R6,ADRADD1                                                       
*                                                                               
LEVA3    MVC   0(L'ADRADD1,R5),0(R6) PICK UP AS MANY LINES AS NEEDED            
         LA    R5,L'ADRADD1(R5)                                                 
         LA    R6,L'ADRADD1(R6)                                                 
         BCT   R3,LEVA3                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PRODUCT AND NAME                                                    *         
***********************************************************************         
*                                                                               
LEVB     OI    PRTFLG,PRDDIFF      DIFFERENT PRODUCT                            
         ZAP   PRDTOT,=P'0'        CLEAR PRD AMOUNT ACCUMULATOR                 
         L     R4,ADLVBNAM                                                      
         LA    R5,PRDNAM                                                        
         BAS   RE,GETNAME                                                       
         B     EXIT                                                             
         EJECT                                                                  
********************************************************************            
* JOB CODE AND NAME                                                *            
********************************************************************            
*                                                                               
PRAC     MVI   FCRDTRNS,C'Y'       PASS TRANSACTIONS                            
*                                                                               
         CLC   ALPHAID,TESTDDSB      DDSB TEST?                                 
         BE    PRAC010               YES, DON'T BOTHER CHECKING                 
*                                                                               
         L     R5,ADGOBLOC                                                      
         USING GOBLOCKD,R5                                                      
         CLI   GOEDI,C'Y'          VALID FOR EDI REPORT?                        
         BE    *+12                YES                                          
         DROP  R5                                                               
         MVI   FCRDTRNS,C'N'       NO, NO MORE TRANSACTIONS                     
         B     EXIT                                                             
*                                                                               
PRAC010  L     R4,ADACC            GET JOB CODE AND NAME                        
         USING ACTRECD,R4                                                       
         MVC   ACCT,ACTKACT                                                     
         L     R4,ADACCNAM                                                      
         LA    R5,JOBNAM                                                        
         BAS   RE,GETNAME                                                       
         DROP  R4                                                               
*                                                                               
         ZAP   JOBTOT,=P'0'        CLEAR JOB AMOUNT ACCUMULATOR                 
         L     R5,ATRNSTBL                                                      
         ST    R5,TBLPNTR                                                       
         XC    TBLENT,TBLENT       TABLE ENTRY COUNTER                          
         XC    CBILNO,CBILNO                                                    
         XC    PBILNO,PBILNO                                                    
         OI    PRTFLG,FRSTBIL                                                   
PRACX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET WORKCODE AND DESCRIPTION                                        *         
***********************************************************************         
*                                                                               
ANALF    L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         MVC   WRKCD,TRNANAL                                                    
         MVC   WRKPTR,TBLPNTR      1ST TABLE ENTRY FOR THIS WRKCODE             
         DROP  R4                                                               
*                                                                               
         L     R4,ADLEDGER                                                      
         MVI   ELCODE,WCOELQ                                                    
         BAS   RE,GETEL            GET WORKCODE ELEMENT                         
         B     *+8                                                              
ANALF05  BAS   RE,NEXTEL                                                        
         BNE   ANALFX                                                           
*                                                                               
         USING WCOELD,R4                                                        
         CLC   WRKCD,WCOCODE       IS THIS THE ONE I'M LOOKING FOR?             
         BNE   ANALF05              N,GET NEXT ELEM                             
         MVC   WRKDSC,WCODESC       Y,SAVE THE DESCRIPTION                      
*                                                                               
ANALFX   B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS                                                *         
***********************************************************************         
*                                                                               
PRTR     L     R7,ADTRANS                                                       
         USING TRNELD,R7                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   PRTRX                                                            
         CLC   TRNANAL,=C'**'      TEST ORDER RECORD                            
         BE    PRTRX                                                            
         ZAP   GSTTOT,=P'0'                                                     
         ZAP   QSTTOT,=P'0'                                                     
*                                                                               
         L     R5,AMONACC                                                       
         USING ACMD,R5                                                          
         L     R4,ACMAPRO2                                                      
         USING PTAELD,R4                                                        
*                                                                               
         MVI   ELCODE,PTAELQ                                                    
         CLI   0(R4),PTAELQ        POINTING TO PTA ELEMENT?                     
         B     *+8                                                              
PRTR10   BAS   RE,NEXTEL           FIND ANOTHER PTA ELEMENT?                    
         BNE   PRTRX               NO                                           
*                                                                               
         CLI   PTATYPE,PTATRAL                                                  
         BNE   PRTR10                                                           
*                                                                               
         TM    PTASTAT1,PTASPEND    TRANSACTION PENDING?                        
         BO    PRTR10               YES, GET NEXT                               
*                                                                               
         TM    REPFLAG,CHKREVUS      CHECK FOR REVERSALS?                       
         BZ    PRTR13                NO, SKIP                                   
         TM    PTASTAT1,PTASREVU+PTASREVS       REVERSALS?                      
         BNZ   PRTR10                 YES, GET NEXT                             
*                                                                               
PRTR13   CLC   TRNANAL,=C'99'      IS THIS AMOUNT RECEIVABLE?                   
         BNE   PRTR20                                                           
*                                                                               
         TM    PTASTAT1,PTASREVU+PTASREVS       REVERSALS?                      
         BNZ   PRTR20                 YES, DO NOT SAVE BILL NUM                 
*                                                                               
         TM    PRTFLG,FRSTBIL      FIRST TIME THROUGH FOR THIS JOB?             
         BO    PRTR15              YES, NO PREVIOUS                             
         OC    PBILNO,PBILNO       ORIG BILL NO SET                             
         BNZ   PRTR15              YES, KEEP IT                                 
         MVC   PBILNO,CBILNO       NO, SET IT                                   
PRTR15   NI    PRTFLG,X'FF'-FRSTBIL                                             
         MVC   CBILNO,PTARBLNO                                                  
*                                                                               
PRTR20   CLC   QSELECT,SPACES      BILL NUMBER SPECIFIED?                       
         BE    *+14                NO                                           
         CLC   QSELECT,PTARBLNO    YES,IS BILL NO. WHAT WE WANT?                
         BNE   PRTR10              NO                                           
*                                                                               
         CLI   QOPT1,C'R'          DRAFT REPORT ON TRANSMITTED TRANS            
         BE    PRTR21                                                           
*                                                                               
         CLC   PTARDATE,STDAT2     RUN DATE < USER SPEC. START DATE?            
         BL    PRTR10              YES                                          
         CLC   PTARDATE,ENDAT2     RUN DATE > USER SPEC. END DATE?              
         BH    PRTR10              YES                                          
*                                                                               
PRTR21   CLC   TRNANAL,=C'99'      IS THIS AMOUNT RECIEVABLE?                   
         BE    PRTR30                                                           
         TM    REPFLAG,WKCDDET     WORK CODE DETAIL?                            
         BNO   PRTRX                                                            
*        ----------------------------------------------------                   
         USING BXTABLED,R6                                                      
         L     R6,WRKPTR           FIRST TABLE ENTRY OF THIS WORKCODE           
PRTR22   C     R6,TBLPNTR          ANY MORE ENTRIES WITH THIS W/C?              
         BNL   PRTR28                NO                                         
         CLC   BXWRKCD,=C'99'                                                   
         BE    PRTR25                                                           
         CLC   PTARBLNO,BXBILNO    SAME BILL NUMBER?                            
         BNE   PRTR25                NO,TRY NEXT ENTRY                          
         CLC   PTARBLDT,BXBLDTE    SAME BILL DATE?                              
         BNE   PRTR25                NO,TRY NEXT ENTRY                          
         AP    BXAMOUNT,PTANET       YES,ADD AMOUNTS                            
         AP    BXAMOUNT,PTARCOM    ADD COMMISSION TO GET ACT.RCB.               
         B     PRTR10              NEXT TRANSACTION                             
PRTR25   LA    R6,BXTABLNQ(R6)     BUMP TO NEXT ENTRY                           
         B     PRTR22                                                           
         DROP  R6                                                               
*                                                                               
         USING BXTABLED,R3                                                      
PRTR28   L     R3,TBLPNTR                                                       
         MVC   BXBILNO,PTARBLNO                                                 
         MVC   BXBLDTE,PTARBLDT                                                 
         MVC   BXWRKCD,WRKCD                                                    
         MVC   BXWRKDSC,WRKDSC                                                  
         ZAP   BXAMOUNT,PTANET                                                  
         AP    BXAMOUNT,PTARCOM    ADD COMMISSION TO GET GROSS                  
         BAS   RE,PRTRBMP                                                       
         B     PRTR10                                                           
*        -----------------------------------------------------                  
PRTR30   BAS   RE,STMPDAEL         CHECK TRANSMISSION DATE STAMP                
         BNE   PRTRX                                                            
         BAS   RE,STMPRC           CHECK RC ACCOUNT DATE STAMP                  
         BNE   PRTRX                                                            
*                                                                               
         L     R3,TBLPNTR          PUT ITS INFO IN THE TABLE                    
         MVC   BXBILNO,PTARBLNO                                                 
         MVC   BXBLDTE,PTARBLDT                                                 
         MVC   BXWRKCD,TRNANAL                                                  
         MVC   BXWRKDSC,WRKDSC                                                  
         MVC   BXPBILNO,PBILNO                                                  
         BAS   RE,GETDUE                                                        
         ZAP   BXAMOUNT,TRNBLPAY                                                
         ZAP   BXGST,=P'0'                                                      
         ZAP   BXQST,=P'0'                                                      
         BAS   RE,GETTAX                                                        
         BAS   RE,PRTRBMP          BUMP UP THE ENTRY COUNT                      
*                                                                               
PRTRX    B     EXIT                                                             
         DROP  R3,R7                                                            
*                                                                               
*---------------------------------------------------------------------          
*        BUMP TO THE NEXT ENTRY IN TRAN TABLE                                   
*---------------------------------------------------------------------          
*                                                                               
PRTRBMP  LA    R3,BXTABLNQ(R3)     POINT TO NEXT ENTRY SLOT IN TABLE            
         ST    R3,TBLPNTR                                                       
         LH    RF,TBLENT           BUMP UP THE ENTRY COUNT                      
         LA    RF,1(RF)                                                         
         STH   RF,TBLENT                                                        
         CHI   RF,MXENTRY                                                       
         BNE   *+6                                                              
         DC    H'0'                MAX ENTRIES REACHED                          
         BR    RE                                                               
*                                                                               
*---------------------------------------------------------------------          
*        GET GST/HST AND QST VALUES                                             
*---------------------------------------------------------------------          
*                                                                               
         USING BXTABLED,R3                                                      
         USING VBIELD,R4                                                        
GETTAX   NTR1  ,                                                                
         L     R4,ADTRANS                                                       
         MVI   ELCODE,VBIELQ       X'48'                                        
GET05    BAS   RE,NEXTEL                                                        
         BNE   GET10                                                            
         AP    BXGST,VBIVAT        ADD GST                                      
         B     GET05                                                            
*                                                                               
         USING PBIELD,R4                                                        
GET10    L     R4,ADTRANS                                                       
         MVI   ELCODE,PBIELQ       X'7D'                                        
GET15    BAS   RE,NEXTEL                                                        
         BNE   GETTAXX                                                          
         CLC   PBIPRV,=C'PQ'       TEST FOR QUEBEC                              
         BNE   *+14                                                             
         AP    BXQST,PBIPST        ADD QST                                      
         B     GET15                                                            
         AP    BXGST,PBIPST        ADD HST                                      
         B     GET15                                                            
*                                                                               
GETTAXX  B     EXIT                                                             
         DROP  R3,R4                                                            
*---------------------------------------------------------------------          
*        CHECK THE EDI DATE STAMP ELEMENT ON THE PRODUCTION RECORD              
*              ON EXIT ELEM MUST BE SET WITH GDAELD ON PROD RECORD              
*---------------------------------------------------------------------          
*                                                                               
STMPDAEL NTR1  ,                   CHECK EDI DATE STAMP ELEMENT                 
*                                                                               
         USING GDAELD,R4                                                        
         L     R4,ADTRANS                                                       
         MVI   ELCODE,GDAELQ       X'E5' GENERAL DATE ELEMENT                   
STDAE10  BAS   RE,NEXTEL                                                        
         BNE   STDAE50                                                          
         CLI   GDATYPE,GDATEDI     DATE FOR EDI TRANSMISSION?                   
         BNE   STDAE10             NO                                           
         MVC   ELEM(GDALN2Q),0(R4) COPY ELEMENT INTO ELEM                       
*                                                                               
         CLI   QOPT1,C'R'          TRANSMIT DATE REPORT OPTION?                 
         BNE   STDAE40             NO, DON'T NEED TO CHECK DATES                
*                                                                               
         GOTO1 DATCON,DMCB,(2,STDAT2),(1,WORK)                                  
         GOTO1 DATCON,DMCB,(2,ENDAT2),(1,WORK+3)                                
*                                                                               
         CLC   GDADATE,WORK        TRANSMIT DATE < USER SPEC. DATE?             
         BL    STDAEXN             YES                                          
         CLC   GDADATE,WORK+3      TRANSMIT DATE > USER SPEC. END DATE?         
         BH    STDAEXN             YES                                          
         B     STDAEX                                                           
*                                                                               
STDAE40  CLI   QOPT3,C'N'          STAMP NEW TRANSMITS ONLY?                    
         BE    STDAEXN             YES                                          
*                                                                               
         CLI   QOPT3,C'T'          RE-TRANSMIT? (KEEP DATE STAMP)               
         BE    STDAEXY             YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,GDADATE)                                    
         MVC   ELEM(GDALN2Q),0(R4) COPY ELEMENT INTO ELEM                       
         B     STDAEX                                                           
*                                                                               
STDAE50  CLI   QOPT1,C'R'          TRANSMIT DATE REPORT OPTION?                 
         BE    STDAEXN             YES, NEVER TRANSMITTED SO EXIT               
         CLI   QOPT3,C'T'          RE-TRANSMIT OPTION? (NO CHNGNG DTES)         
         BE    STDAEXN             YES, DO NOT TRANSMIT FIRST TIME TRNS         
*                                                                               
         LA    R4,ELEM                                                          
         MVI   GDAEL,GDAELQ          ELEMENT CODE                               
         MVI   GDALN,GDALN2Q         ELEMENT LENGTH (2 DATES)                   
         MVI   GDATYPE,GDATEDI       TYPE = EDI TRANSMISSION DATE (16)          
         GOTO1 DATCON,DMCB,(5,0),(1,GDADATE)                                    
         MVC   GDADATE2,GDADATE                                                 
*                                                                               
         L     R2,ADTRANS                                                       
         SH    R2,DATADISP                                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCOUNT '),(R2),ELEM,0                      
         CLI   DMCB+12,X'00'                                                    
         BE    STDAEX                                                           
         DC    H'0'                                                             
*                                                                               
STDAEX   CLI   QOPT1,C'Y'          DO WE WANT THE EDI FILE?                     
         BNE   STDAEXY             NO                                           
         CLI   RCWRITE,C'N'                                                     
         BE    STDAEXY                                                          
         MVI   MODE,WRITRANS                                                    
*                                                                               
STDAEXY  CR    RB,RB                 EQUAL EXIT                                 
         B     EXIT                                                             
STDAEXN  LTR   RB,RB                 UNEQUAL EXIT                               
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*---------------------------------------------------------------------          
*        STAMP ON THE EDI DATE ELEMENT TO THE RECEIVALBE RECORD                 
*              ON ENTRY ELEM MUST BE SET WITH GDAELD ON PROD RECORD             
*---------------------------------------------------------------------          
*                                                                               
STMPRC   NTR1  ,                                                                
*                                                                               
         MVC   RCKEY,SPACES         USED TO BUILD RECEIVABLE KEY                
         LA    R3,RCKEY                                                         
R        USING TRNRECD,R3           NEED TO LABEL USINGS ON TRNRECD             
*                                                                               
         CLI   QOPT3,C'T'           RE-TRANSMIT? (DO NOT CHANGE DATE)           
         BE    STRCX                YES                                         
*                                                                               
         USING PPRELD,R4            PRODUCTION PROFILE ELEMENT                  
         L     R4,ADHEIRB           PRODUCT RECORD                              
         MVI   ELCODE,PPRELQ        X'24' PROD PROF EL                          
         BAS   RE,GETEL                                                         
         BNE   STRC10                                                           
         CLC   PPRGAOFF,SPACES      OFFICE AVAILABLE ON PRODUCT?                
         BNE   STRC12               YES                                         
*                                                                               
STRC10   L     R4,ADHEIRA           GET OFFICE FROM CLIENT RECORD               
         BAS   RE,GETEL                                                         
         MVI   ERRFLG,1                                                         
         BNE   STRCERR                                                          
         CLC   PPRGAOFF,SPACES      OFFICE AVAILABLE ON CLIENT                  
         BE    STRCERR              NO                                          
*                                                                               
STRC12   L     R1,ADCMPEL                                                       
         TM    CPYSTAT4-CPYELD(R1),CPYSOFF2    NEW OFFICES?                     
         BNO   *+10                            NO, DON'T PUT IN KEY             
         MVC   R.TRNKOFF,PPRGAOFF   OFFICE                                      
         MVC   RCOFF,PPRGAOFF       OFFICE                                      
         DROP  R4                                                               
*                                                                               
         MVI   R.TRNKSBR,0          ZERO OUT SUB-REFERENCE                      
*                                                                               
         USING TRNELD,R4            TRANSACTION ELEMENT X'44'                   
         L     R4,ADTRANS                                                       
         MVC   R.TRNKDATE,TRNDATE   BILL DATE                                   
         MVC   R.TRNKREF,TRNREF     BILL NUMBER                                 
         AP    RCBLAMT,TRNBLPAY     BILL AMOUNT                                 
         ZAP   RCBLTAMT,TRNAMNT     TRANSACTION AMOUNT                          
         MVC   RCMOS,TRNMOS         MONTH OF SERVICE                            
         MVC   RCBREF,TRNBREF       MONTH OF SERVICE                            
         DROP  R4                                                               
*                                                                               
O        USING TRNRECD,R4           TRANSACTION RECORD KEY                      
         SH    R4,DATADISP                                                      
         MVC   R.TRNKCULA,O.TRNKCULC    RECEIVABLE ACCOUNT                      
         DROP  O                                                                
*                                                                               
         USING TRSELD,R4            TRANSACTION STATUS ELEMENT X'60'            
         MVI   ELCODE,TRSELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RCACTDTE,TRSDATE     ACTIVITY/RUN DATE                           
         DROP  R4                                                               
*                                                                               
         L     R4,ADTRANS                                                       
         USING BSCELD,R4            BILLING SOURCE ELEMENT X'E3'                
         MVI   ELCODE,BSCELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   STRC20                                                           
         MVC   R.TRNKCACT,BSCBSRC   BILLING SOURCE                              
         CLI   BSCLN,BSCLNQ                                                     
         BL    STRC30                                                           
         MVC   R.TRNKOFF,BSCBOFF    OFFICE                                      
         B     STRC30                                                           
         DROP  R4                                                               
*                                                                               
STRC20   L     R4,ADTRANS                                                       
         SH    R4,DATADISP                                                      
         CLC   R.TRNKUNT(2),=C'SR'  RECEIVABLE IS SR?                           
         BE    STRC25               YES                                         
O        USING TRNRECD,R4           TRANSACTION RECORD KEY                      
         MVC   R.TRNKCACT,O.TRNKACT BILLING SOURCE  (EX. SB ACCOUNT)            
         B     STRC30                                                           
         DROP  O                                                                
*                                                                               
         USING PTAELD,R4            PROD TRANSACTION ACTIVITY X'77'             
STRC25   MVI   ELCODE,PTAELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   STRC27                                                           
         CLI   PTARFORM+1,C'C'      CLIENT GROUP BILLING                        
         BE    STRC26                                                           
         CLI   PTARFORM+1,C'P'      PRODUCT LEVEL GROUP BILLING                 
         BNE   STRC27                                                           
STRC26   MVC   R.TRNKCACT,=CL12'PRODUCTION  '                                   
         B     STRC30                                                           
         DROP  R4                                                               
*                                                                               
STRC27   L     R4,AIO                                                           
         USING PMDRECD,R4           PRODUCTION MEDIA RECORD X'09'               
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ     X'09'                                       
*                                                                               
         L     R2,ADTRANS                                                       
         SH    R2,DATADISP                                                      
O        USING TRNRECD,R2           TRANSACTION RECORD KEY                      
         MVC   PMDKCPY,O.TRNKCPY    COMPANY                                     
         MVC   PMDKMED,O.TRNKACT+6  MEDIA IS FIRST BYTE OF JOB                  
         DROP  O                                                                
*                                                                               
         MVI   ERRFLG,2                                                         
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT ',(R4),(R4),0                     
         BNE   STRCERR              CANNOT FIND PROD MEDIA RECORD               
*                                                                               
         MVI   ERRFLG,3                                                         
         USING PMDELD,R4            PRODUCTION MEDIA ELEMENT X'11'              
         MVI   ELCODE,PMDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   STRCERR              CANNOT FIND PROD MEDIA ELEM                 
         MVC   R.TRNKCACT,PMDDESC   BILL SOURCE IS MEDIA DESCRIPTION            
         DROP  R4                                                               
*                                                                               
STRC30   L     R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT ',RCKEY,(R4),0                    
         BE    STRC38                                                           
         MVI   ERRFLG,4                                                         
         B     STRCERR              TROUBLE READING RECEIVABLE RECORD           
*                                                                               
STRC35   L     R4,AIO                                                           
         MVI   ERRFLG,5                                                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT ',(R4),(R4),0                     
         BNE   STRCERR              RAN OUT OF RECORDS                          
*                                                                               
STRC38   CLC   RCKEY(L'RCKEY-1),0(R4)    EVERYTHING EXCEPT SUBREF               
         BE    STRC40                                                           
*                                                                               
         MVI   ERRFLG,6                                                         
         CLC   R.TRNKCACT,=CL12'PRINT MEDIA '                                   
         BE    STRCERR              COULD NOT FIND RECEIVABLE RECORD            
         MVC   R.TRNKCACT,=CL12'PRINT MEDIA '                                   
         B     STRC30                                                           
         DROP  R                                                                
*                                                                               
         USING TRSELD,R4            TRANSACTION STATUS ELEMENT X'60'            
STRC40   MVI   ELCODE,TRSELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   RCACTDTE,TRSDATE     SAME ACTIVITY/RUN DATE?                     
         BNE   STRC35                                                           
         DROP  R4                                                               
*                                                                               
         USING TRNELD,R4            TRANSACTION ELEMENT X'44'                   
         L     R4,AIO                                                           
         MVI   ELCODE,TRNELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   RCMOS,TRNMOS         MONTH OF SERVICE                            
         BNE   STRC35                                                           
         CLC   RCBREF,TRNBREF       BATCH REFERENCE                             
         BNE   STRC35                                                           
         CLC   RCOFF,TRNOFFC        OFFICE (NEEDED FOR 1 CHAR OFFICE)           
         BNE   STRC35                                                           
         SP    RCBLAMT,TRNAMNT                                                  
         DROP  R4                                                               
*                                                                               
         USING GDAELD,R4            X'E5' GENERAL DATE ELEMENT                  
         L     R4,AIO                                                           
         MVI   ELCODE,GDAELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
STRC45   BAS   RE,NEXTEL                                                        
         BNE   STRC50                                                           
         CLI   GDATYPE,GDATEDI      DATE FOR EDI TRANSMISSION?                  
         BNE   STRC45               NO                                          
         LA    R2,ELEM                                                          
         CLC   GDADATE,GDADATE-GDAELD(R2)  STAMPING THE SAME DATE?              
         BE    STRCX                       YES, DO NOT BOTHER TO WRITE          
         MVC   GDADATE,GDADATE-GDAELD(R2)                                       
         B     STRC70                                                           
*                                                                               
STRC50   L     R4,AIO                                                           
         MVI   ERRFLG,7                                                         
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCOUNT '),(R4),ELEM,0                      
         CLI   DMCB+12,X'00'                                                    
         BNE   STRCERR              COULD NOT ADD DATE STAMP ELEMENT            
         CLI   QOPT3,C'F'           RUNNING A FIX AND NEED TO MARK REC?         
         BNE   STRC70               YES, RUNS ON PREVIOUS TRANSMITS             
         CLI   QOPT1,C'R'           SUPOSED TO BE ALREADY TRANSMITTED?          
         BE    STRC75               YES, CHANGE RECEIVABLE RECORD               
         DROP  R4                                                               
*                                                                               
STRC70   CLI   QOPT1,C'Y'           DO WE WANT THE EDI FILE?                    
         BNE   STRCX                NO                                          
STRC75   CLI   RCWRITE,C'N'                                                     
         BE    STRCX                                                            
         L     R4,AIO                                                           
         MVI   ERRFLG,8                                                         
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT ',(R4),(R4),0                      
         BNE   STRCERR              COULD NOT WRITE RECEIVABLE RECORD           
*                                                                               
STRCX    B     STDAEXY             EQUAL EXIT                                   
*                                                                               
*                                                                               
STRCERR  MVI   MODE,PROCTRNS       DO NOT WRITE DATE STAMP, RESET MODE          
         USING TRNRECD,R4                                                       
         L     R4,ADTRANS                                                       
         SH    R4,DATADISP                                                      
         USING ERRMSGD,R2                                                       
         LA    R2,P                                                             
         MVC   ERRMSG,STRCERMS                                                  
         MVC   ERRULA,TRNKULA                                                   
         MVC   ERRWORK,TRNKOFF                                                  
         MVC   ERRULC,TRNKULC                                                   
         MVC   ERRREF,TRNKREF                                                   
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(21,ERRDATE)                            
         EDIT  RCBLTAMT,ERRAMT,2,FLOAT=-     TRANSACITION AMOUNT                
         EDIT  ERRFLG,ERRCODE                                                   
         BAS   RE,REPRT                                                         
         B     STDAEXN                                                          
         DROP  R2,R4                                                            
*                                                                               
STRCERMS DC    CL35'ERROR - TRANSACTION NOT TRANSMITTED'                        
*---------------------------------------------------------------------          
*        GET THE TRANSACTION DUE DATE                                           
*---------------------------------------------------------------------          
*                                                                               
GETDUE   NTR1  ,                   GET THE DUE DATE                             
         USING BXTABLED,R3                                                      
         XC    BXDUEDTE,BXDUEDTE                                                
         MVI   ELCODE,X'61'                                                     
         L     R4,ADTRANS                                                       
         BAS   RE,NEXTEL           FIND DUE DATE ELEMENT                        
         BNE   EXIT                NONE                                         
         USING DUEELD,R4                                                        
         MVC   BXDUEDTE,DUEDATE                                                 
         B     EXIT                                                             
*                                                                               
*                                                                               
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LAST - PRINT REPORT PAGE                                    *         
***********************************************************************         
*                                                                               
ACCL     ICM   RF,15,AGYACCL       TEST AGENCY HOOK                             
         BZ    *+6                                                              
         BASR  RE,RF                                                            
         BAS   RE,GETPO                                                         
         L     R4,ATRNSTBL         POINT TO BEGINNING OF TABLE                  
         USING BXTABLED,R4                                                      
         LH    R5,TBLENT                                                        
         LTR   R5,R5               ANY TABLE ENTRIES?                           
         BNP   ACCLX               NO                                           
         ZAP   BLNTOT,=P'0'                                                     
*                                                                               
         GOTO1 VXSORT,DMCB,(0,(R4)),(R5),BXTABLNQ,10,0                          
*                                                                               
         BAS   RE,PRTJOB           PRINT JOB INFO                               
*                                                                               
         XC    TBLPNTR,TBLPNTR                                                  
ACCL10   CLC   BXWRKCD,=C'99'      END OF TRANSACTIONS FOR BILL & BLDT?         
         BE    ACCL15              NO                                           
         BAS   RE,WRKTOT                                                        
         B     ACCL22                                                           
*                                                                               
ACCL15   ST    R4,CURBILL                                                       
         BAS   RE,BILLTOT                                                       
         CLI   QOPT1,C'Y'          DO WE WANT THE EDI FILE?                     
         BNE   *+8                 NO                                           
         BAS   RE,PUT              WRITE OUTPUT RECORD                          
         ZAP   BLNTOT,=P'0'                                                     
         ZAP   GSTTOT,=P'0'                                                     
         ZAP   QSTTOT,=P'0'                                                     
*                                                                               
ACCL22   LA    R4,BXTABLNQ(R4)                                                  
         BCT   R5,ACCL10                                                        
*                                                                               
         OI    PRTFLG,PRDLAST      OUTPUT FOR JOB, PRINT PROD TOTAL             
         OI    PRTFLG,CLTLAST      OUTPUT FOR JOB, PRINT CLI TOTAL              
         LA    RF,JOBBLK                                                        
         MVI   SPACING,3                                                        
         BAS   RE,TOTAL            JOB TOTAL                                    
*                                                                               
ACCLX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* ACCOUNT LAST - GET PO NUMBER                                        *         
*                AND USER FIELDS FOR AGENCY OU                        *         
*----------------------------------------------------------------------         
*                                                                               
GETPO    NTR1  ,                                                                
         MVI   PONLEN,0            GET PO NUMBER                                
         MVC   PONUM,SPACES                                                     
*                                                                               
         LA    R2,UFLDS                                                         
         MVI   0(R2),X'40'                                                      
         SR    R1,R1                                                            
         LA    R1,L'UFLDS-1                                                     
GETPO2   MVC   1(1,R2),0(R2)                                                    
         LA    R2,1(R2)                                                         
         BCT   R1,GETPO2                                                        
*                                                                               
         L     R4,ADACC                                                         
         MVI   ELCODE,UFSELQ       X'A2'                                        
         BAS   RE,GETEL            FIND THE USER ELEMENT                        
         B     *+8                                                              
GETPO3   BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         USING UFSELD,R4                                                        
         CLC   UFSCODE,USERPO      USER CODE FOR ORDER NUMBER?                  
         BE    GETPO7                                                           
*                                                                               
         CLC   UFSCODE,=CL2'1 '    USER CODE FOR G/L ACCOUNT?  (OU)             
         BNE   *+12                                                             
         LA    R5,UFLD1                                                         
         B     GETPO5                                                           
         CLC   UFSCODE,=CL2'2 '    USER CODE FOR BILL TO CODE? (OU)             
         BNE   *+12                                                             
         LA    R5,UFLD2                                                         
         B     GETPO5                                                           
         CLC   UFSCODE,=CL2'3 '    USER CODE FOR ATTN NAME?    (OU)             
         BNE   *+12                                                             
         LA    R5,UFLD3                                                         
         B     GETPO5                                                           
         CLC   UFSCODE,=CL2'4 '    USER CODE FOR PROG ID?      (OU)             
         BNE   GETPO3                                                           
         LA    R5,UFLD4                                                         
*                                                                               
GETPO5   SR    R3,R3                                                            
         IC    R3,UFSLN            ELEMENT LENGTH                               
         AHI   R3,-UFSLN1Q         R3=LENGTH OF DATA                            
         BNP   EXIT                                                             
         CHI   R3,L'UFLD1          LENGTH OF USER FIELD                         
         BL    *+8                                                              
         LHI   R3,L'UFLD1          GET SHORTEST                                 
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),UFSDATA     SAVE FOR REPORT                              
         B     GETPO3                                                           
*                                                                               
GETPO7   SR    R3,R3                                                            
         IC    R3,UFSLN            ELEMENT LENGTH                               
         AHI   R3,-UFSLN1Q         R3=LENGTH OF DATA                            
         BNP   EXIT                                                             
         CHI   R3,L'PONUM          LENGTH OF PO NUMBER FIELD                    
         BL    *+8                                                              
         LHI   R3,L'PONUM          GET SHORTEST                                 
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PONUM(0),UFSDATA    SAVE FOR REPORT                              
*                                                                               
         LA    R3,1(R3)                                                         
         STC   R3,PONLEN           SAVE LENGTH                                  
         B     GETPO3                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LAST PRODUCT - PRINT TOTAL FOR THE PRODUCT                          *         
***********************************************************************         
*                                                                               
LEVBL    TM    PRTFLG,PRDLAST                                                   
         BZ    EXIT                                                             
         LA    RF,PRDBLK                                                        
         MVI   SPACING,2                                                        
         BAS   RE,TOTAL            PRODUCT TOTAL                                
         NI    PRTFLG,X'FF'-PRDLAST                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST CLIENT - PRINT TOTAL FOR THE CLIENT                            *         
***********************************************************************         
*                                                                               
LEVAL    TM    PRTFLG,CLTLAST                                                   
         BZ    EXIT                                                             
         LA    RF,CLIBLK                                                        
         BAS   RE,TOTAL            CLIENT TOTAL                                 
         NI    PRTFLG,X'FF'-CLTLAST                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REQLAST                                                             *         
***********************************************************************         
*                                                                               
REQL     MVC   P(33),=CL33'PRODUCTION/RECEIVABLE DIFFERENCE='                   
         EDIT  RCBLAMT,(18,P+34),2,FLOAT=-                                      
         BAS   RE,REPRT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RUNLAST                                                             *         
***********************************************************************         
*                                                                               
RUNL     TM    PRTFLG,OPENFIL      WAS OUPUT FILE OPENED?                       
         BZ    EXIT                   NO                                        
         CLOSE (EDIOUT)                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET THE IDI RECORD                                                  *         
***********************************************************************         
*                                                                               
GETIDI   NTR1  ,                                                                
         MVC   CMPNM,SPACES                                                     
         MVC   CMPADD,SPACES                                                    
*                                                                               
         USING CTIREC,R2       GET DEST NAME AND ADDR FROM IDI RECORD           
         L     R2,AIO              IO AREA                                      
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    IDI RECORD "I"                               
         MVC   CTIKNUM,ORIGINUM    ORIGIN ID                                    
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,(R2),(R2)                             
         BNE   EXIT                                                             
*                                                                               
         SR    R0,R0                                                            
         LA    R2,CTIDATA          POINT TO FIRST ELEMENT                       
GETIDI3  CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLI   0(R2),CTORGELQ                                                   
         BE    GETIDI5                                                          
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETIDI3             NEXT ELEMENT                                 
         DROP  R2                                                               
*                                                                               
         USING CTORGD,R2                                                        
GETIDI5  MVC   CMPNM,CTORGNAM                                                   
         MVC   CMPADD,CTORGADD                                                  
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT JOB INFO                                                      *         
***********************************************************************         
*                                                                               
PRTJOB   TM    REPFLAG,NEEDREP     HARD COPY REPORT NEEDED?                     
         BNOR  RE                                                               
PRTJOB1  NTR1  ,                                                                
         TM    PRTFLG,PRDDIFF      DIFFERENT PRODUCT?                           
         BZ    PRTJOB3             NO,DON'T PRINT CODE AND NAME                 
         MVC   P+1(L'PRDCD),PRDCD                                               
         MVC   P+L'PRDCD+2(1),=C'-'                                             
         MVC   P+L'PRDCD+4(L'PRDNAM),PRDNAM                                     
         MVC   PSECOND+1(20),=C'--------------------'                           
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
         NI    PRTFLG,X'FF'-PRDDIFF                                             
*                                                                               
PRTJOB3  MVC   P+2(5),=C'JOB -'    PRINT JOB CODE AND NAME                      
         MVC   PRNJOB,JOBCD                                                     
         SR    R1,R1                                                            
         ICM   R1,1,PONLEN                                                      
         BZ    EXIT                                                             
         MVC   PSECOND+1(8),=C'ORDER # '                                        
         LA    R2,PSECOND+10                                                    
*                                                                               
         CLC   ALPHAID,=C'NE'      SPECIAL FOR DELL IMDB#1715311                
         BNE   PRTJOB4                                                          
         CLC   USERPO,=C'ON'                                                    
         BNE   PRTJOB4                                                          
         MVC   PSECOND+1(11),=C'ORDER # DO '                                    
         LA    R2,PSECOND+13                                                    
*                                                                               
PRTJOB4  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PONUM        PRINT ORDER NUMBER                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT TOTAL FOR EACH INVOICE #                                      *         
*   ON ENTRY - R4 WILL POINT TO BX TABLE ENTRY                        *         
***********************************************************************         
*                                                                               
         USING BXTABLED,R4                                                      
BILLTOT  NTR1  ,                                                                
*        TM    REPFLAG,WKCDDET     WORK CODE DETAIL? IMDB#1715311               
*        BO    BILLTOT3                                                         
         AP    CLTTOT,BXAMOUNT                                                  
         AP    PRDTOT,BXAMOUNT                                                  
         AP    JOBTOT,BXAMOUNT                                                  
         AP    BLNTOT,BXAMOUNT                                                  
         AP    GSTTOT,BXGST                                                     
         AP    QSTTOT,BXQST                                                     
*                                                                               
BILLTOT3 TM    REPFLAG,NEEDREP                                                  
         BNO   BILLTOT7                                                         
         MVC   PRNTXT,=C'TOTAL FOR'                                             
         MVC   PRNLEV,=C'BILL-'                                                 
         MVC   PRNCODE,BXBILNO                                                  
         EDIT  BLNTOT,PRNTOT,2,COMMAS=YES,MINUS=YES                             
         CLC   P+2(3),=C'JOB'    PRINTING JOB CD AND NM ON THIS LINE            
         BE    BILLTOT5                                                         
         MVI   SPACING,2                                                        
BILLTOT5 BAS   RE,REPRT                                                         
*                                                                               
BILLTOT7 MVC   BILNO,BXBILNO                                                    
         GOTO1 DATCON,DMCB,(2,BXBLDTE),(20,BILDTC)                              
         OC    BXDUEDTE,BXDUEDTE                                                
         BZ    BILLTOT8                                                         
         GOTO1 DATCON,DMCB,(2,BXDUEDTE),(X'20',DUEDT)                           
         GOTO1 DATCON,DMCB,(2,BXDUEDTE),(20,DUEDT2)                             
*                                                                               
BILLTOT8 MVC   TRNTYP,=C'DI'                                                    
         CLC   AGYALPH,=C'M2'                                                   
         BE    BILLTOT9                                                         
         CLC   AGYALPH,=C'H7'                                                   
         BE    BILLTOT9                                                         
         MVC   TRNTYP,=C'DR'                                                    
BILLTOT9 CP    BXAMOUNT,=P'0'                                                   
         BNL   *+10                                                             
         MVC   TRNTYP,=C'CR'                                                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PUT RECORDS TO TAPE                                                 *         
***********************************************************************         
*                                                                               
PUT      NTR1  ,                                                                
         XC    RTNADR,RTNADR        INITIALIZE RETURN ADDRESS                   
         MVI   RTNROU,C'N'          INITIALIZE RETURN STATUS                    
         MVI   SKIPREC,C'N'                                                     
*                                                                               
         L     R4,ADAGY            R4=RECORD TABLE                              
         LA    R4,AGYLNQ(R4)                                                    
         XC    ALOOP,ALOOP         CLEAR LOOP POINTER                           
         XC    LOOPFLG,LOOPFLG                                                  
*                                                                               
PUT1     L     R2,AIO                                                           
         USING EDIHDRD,R2                                                       
         MVC   EDISET,=C'810'       SET                                         
         MVC   EDISPA,SPACES                                                    
         MVC   EDISEG(6),0(R4)      SEGMENT AND SEQUENCE                        
         MVC   EDIZRO,=C'00000'                                                 
         LA    R3,EDILNQ                                                        
         STCM  R3,3,RECLEN          INITIALIZE RECORD LENGTH                    
         LA    R2,EDIDATA-EDIHDRD(R2)                                           
         TM    LOOPFLG,LPSTRT      HAVE WE STARTED A LOOP?                      
         BO    *+8                                                              
         ST    R4,RTNADR           SAVE START OF LOOP                           
         TM    LOOPFLG,LPSKIP      DO WE NEED TO END LOOP?                      
         BZ    *+8                                                              
         MVI   SKIPREC,C'Y'                                                     
*        NI    LOOPFLG,X'FF'-LPSKIP                                             
         LA    R4,6(R4)                                                         
*                                                                               
PUT2     SR    R3,R3               R3=LENGTH OF DATA                            
         LR    RE,R4               RE=START OF DATA                             
PUT3     DS    0H                                                               
         CLI   0(R4),BEGQ          TEST BEGINING OF LOOP                        
         BNE   PUT4                                                             
         OI    LOOPFLG,LPSTRT                                                   
         B     PUT5                                                             
*                                                                               
PUT4     CLI   0(R4),ESC           TEST ESCAPE SEQUENCE                         
         BE    PUT5                                                             
PUT4A    CLI   0(R4),EOR           TEST END OF RECORD                           
         BE    PUT5                                                             
         CLI   0(R4),ENDQ          TEST END OF LOOP                             
         BE    PUT7A                                                            
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         B     PUT3                                                             
*                                                                               
PUT5     LTR   R3,R3               TEST ANY DATA TO MOVE                        
         BZ    PUT7                                                             
         SR    R1,R1               UPDATE RECORD LENGTH                         
         ICM   R1,3,RECLEN                                                      
         AR    R1,R3                                                            
         STCM  R1,3,RECLEN                                                      
         LR    R0,R2               R0=DESTINATION                               
         LR    R1,R3               R1 & RF = LENGTH                             
         LR    RF,R3                                                            
         MVCL  R0,RE               DATA TO IO AREA                              
         AR    R2,R3               R2 TO NEXT AREA                              
*                                                                               
PUT7     CLI   0(R4),EOR           TEST END OF RECORD                           
         BE    PUT13                                                            
         CLI   0(R4),ENDQ          END OF LOOP                                  
         BNE   PUT9                                                             
PUT7A    CLI   RTNROU,C'Y'         TEST RETURN REQUESTED                        
         BNE   PUT8                                                             
         L     R4,RTNADR           PROCESS NEXT                                 
         XC    RTNADR,RTNADR                                                    
         NI    LOOPFLG,X'FF'-LPSTRT  RESET LOOP STARTED BIT                     
         B     PUT1                                                             
PUT8     XC    RTNADR,RTNADR                                                    
         NI    LOOPFLG,X'FF'-LPSKIP  RESET END BIT                              
         B     PUT14                                                            
*                                                                               
PUT9     BAS   RE,XTRA             EXTRACT SPECIAL DATA                         
PUT10    L     R2,RECNXT           R2=NEXT DATA AREA                            
         LA    R4,4(R4)            BUMP R4 PASSED ESCAPE SEQUENCE               
         B     PUT2                                                             
*                                                                               
PUT13    L     R2,AIO                                                           
         XC    EDILN(4),EDILN                                                   
         MVC   EDILN,RECLEN        SET LENGTH                                   
         CLI   SKIPREC,C'Y'        DO WE WANT TO SKIP THIS SEGMENT?             
         BE    PUT14                                                            
*                                                                               
         PUT   EDIOUT,EDIHDRD                                                   
*                                                                               
PUT14    MVI   SKIPREC,C'N'        RESET SKIPREC BYTE                           
         LA    R4,1(R4)                                                         
         CLI   0(R4),ENDQ                                                       
         BE    PUT7A                                                            
         CLI   0(R4),EOT           TEST END OF TABLE                            
         BNE   PUT1                                                             
PUTX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXTRACT AGENCY DATA                                                 *         
*  R4=A(4 BYTE ESCAPE SEQUENCE)                                                 
*  R2=A(OUTPUT AREA)                                                            
***********************************************************************         
*                                                                               
XTRA     NTR1  ,                                                                
         ST    R2,RECNXT                                                        
         SR    R1,R1                                                            
         IC    R1,1(R4)             R1=LENGTH OF OUTPUT DATA                    
         SR    R3,R3                                                            
         ICM   R3,3,2(R4)           R3=SOURCE CODE                              
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         LA    R5,XDATA(R3)                                                     
         L     RF,0(R5)             RF=A(SOURCE)                                
         TM    0(R5),X'80'                                                      
         BNO   XTRA5                                                            
         BASR  RE,RF                RF=A(ROUTINE)                               
         BCTR  R1,0                                                             
         B     XTRA7                                                            
*                                                                               
XTRA5    BCTR  R1,0                                                             
         EX    R1,*+8              DATA TO OUTPUT RECORD                        
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
         SR    RE,RE                                                            
         IC    RE,0(R5)            RF=LENGTH OF DATA                            
         BCTR  RE,0                                                             
         CR    RE,R1                                                            
         BL    *+6                                                              
         LR    RE,R1               USE SHORTEST LENGTH                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
*                                                                               
XTRA7    LA    R2,1(R1,R2)                                                      
         ST    R2,RECNXT           UPDATE A(NEXT BYTE)                          
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,RECLEN         UPDATE LENGTH                                
         LA    R3,1(R1,R3)                                                      
         STCM  R3,3,RECLEN                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* WORKCODE DETAIL                                                     *         
***********************************************************************         
                                                                                
         USING BXTABLED,R4                                                      
WRKTOT   TM    REPFLAG,WKCDDET     WORK CODE DETAIL?                            
         BNOR  RE                                                               
WRKTOT1  NTR1  ,                                                                
         MVC   PRNWKC,BXWRKCD                                                   
         MVC   PRNDSC,BXWRKDSC                                                  
         MVC   PRNBILL,BXBILNO                                                  
         GOTO1 DATCON,DMCB,(2,BXBLDTE),(11,PRNDTE)                              
         MVC   PRNUNT,=C'DO'                                                    
         EDIT  BXAMOUNT,PRNQTY,2,COMMAS=YES,MINUS=YES                           
         EDIT  BXAMOUNT,PRNTOT,2,COMMAS=YES,MINUS=YES                           
*        AP    CLTTOT,BXAMOUNT     IMDB#1715311                                 
*        AP    PRDTOT,BXAMOUNT                                                  
*        AP    JOBTOT,BXAMOUNT                                                  
*        AP    BLNTOT,BXAMOUNT                                                  
         TM    REPFLAG,NEEDREP                                                  
         BNO   EXIT                                                             
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GETNAME     MOVE A NAME INTO THE DESIGNATED STORAGE AREA (R5)       *         
***********************************************************************         
                                                                                
         USING NAMELD,R4                                                        
GETNAME  MVC   0(36,R5),SPACES                                                  
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),NAMEREC                                                  
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TOTAL FOR CLIENT/PRODUCT/ OR JOB                              *         
*   RF=A(LEVEL CONTROL BLOCK)                                         *         
***********************************************************************         
                                                                                
TOTAL    NTR1  ,                                                                
         TM    REPFLAG,NEEDREP                                                  
         BNO   EXIT                                                             
         MVC   PRNTXT,=C'TOTAL FOR'                                             
*                                                                               
         LM    R2,R5,0(RF)         A(TEXT),A(CODE),A(NAME),A(TOTAL)             
         SR    RE,RE                                                            
         IC    RE,4(RF)            LENGTH                                       
         MVC   PRNLEV,0(R2)        TEXT                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRNCODE(0),0(R3)                                                 
         MVC   PRNNAME,0(R4)                                                    
         EDIT  (P8,0(R5)),PRNTOT,2,COMMAS=YES,MINUS=YES                         
         BAS   RE,REPRT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
                                                                                
REPRT    NTR1  ,                                                                
         MVC   HEAD3+63(15),CONTACT          CONTACT NAME                       
         MVC   HEAD3+97(3),PHONE             CONTACT PHONE #                    
         MVI   HEAD3+100,C'-'                      "                            
         MVC   HEAD3+101(3),PHONE+3                "                            
         MVI   HEAD3+104,C'-'                      "                            
         MVC   HEAD3+105(4),PHONE+6                "                            
*                                                                               
         TM    REPFLAG,SHOVENNO              SHOW VENDOR NO.?                   
         BNO   REPRT10                                                          
         MVC   HEAD5+21(L'VENDOR),VENDOR     VENDOR NUMBER                      
         CLC   ORIGINUM,=X'0584'             DNNYE?                             
         BNE   REPRT10                                                          
         CLI   QFILTER5,C'5'                 FILTER5=5?                         
         BE    REPRT10                                                          
         MVC   HEAD5+21(L'VENDOR),=C'8202450'                                   
*                                                                               
REPRT10  MVC   HEAD6+11(L'CMPNM),CMPNM                                          
         MVC   HEAD7+11(L'CMPADD),CMPADD                                        
*                                                                               
         MVC   HEAD3+16(L'STDAT8),STDAT8                                        
         MVI   HEAD3+25,C'-'                                                    
         MVC   HEAD3+27(L'ENDAT8),ENDAT8                                        
         MVC   HEAD5+76(L'CLTCD),CLTCD                                          
         MVC   HEAD5+82(L'CLTNAM),CLTNAM                                        
         MVC   HEAD6+82(L'ADRADD1),CLTADD                                       
         MVC   HEAD7+82(L'ADRADD1),CLTADD+(L'ADRADD1)                           
         MVC   HEAD8+82(L'ADRADD1),CLTADD+(L'ADRADD1*2)                         
         MVC   HEAD9+82(L'ADRADD1),CLTADD+(L'ADRADD1*3)                         
         MVC   HEAD10+82(L'ADRADD1),CLTADD+(L'ADRADD1*4)                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* EDIT BILL TOTAL TO RECORD                                           *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
BILTOTR  NTR1  ,                                                                
         UNPK  WORK(15),BLNTOT                                                  
         LA    R3,WORK+14                                                       
         GOTOR FIXNEG                                                           
         LA    RF,0(R1,R2)                                                      
         BCTR  RF,0                RF TO END OF DESTINATION                     
         LA    RE,WORK+14          RE TO END OF EDITED AMOUNT                   
         MVC   0(1,RF),0(RE)       MOVE FROM RIGHT TO LEFT                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REFERENCE NUMBER BASED ON AGY/CLIENT/PRODUCT      IMDB#2172271      *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
REFNUM   NTR1  ,                                                                
         CLC   ALPHAID,=C'NE'      SPECIAL FOR DDB NE                           
         BNE   REFNUMX                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES      INIT FIELD WITH SPACES                       
         MVC   0(4,R2),=C'1100'    DEFAULT IS 1100 FOR NE                       
*                                                                               
         CLC   =C'ABC',CLTCD       IS CLIENT 'ABC' ?                            
         BNE   REFNUMX             NO EXIT                                      
         CLC   =C'IN ',PRDCD       IS PRODUCT 'IN ' ?                           
         BNE   REFNUMX                                                          
*                                                                               
         L     R4,ADACC                                                         
         MVI   ELCODE,UFSELQ       X'A2'                                        
         BAS   RE,GETEL            FIND THE USER ELEMENT                        
         B     *+8                                                              
REFNUM10 BAS   RE,NEXTEL                                                        
         BNE   REFNUMX                                                          
         USING UFSELD,R4                                                        
         CLC   UFSCODE,=C'CC'      USER CODE FOR REF/CPY CODE                   
         BNE   REFNUM10            NO                                           
*                                                                               
         SR    R3,R3                                                            
         IC    R3,UFSLN            ELEMENT LENGTH                               
         AHI   R3,-UFSLN1Q         R3=LENGTH OF DATA                            
         BNP   REFNUMX                                                          
*                                                                               
         MVC   0(4,R2),UFSDATA      1300 OR 1100                                
REFNUMX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* IBM PO DESCRIPTION FIELD                                            *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
IBMPO    NTR1  ,                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
         MVC   0(3,R2),=C'001'                                                  
         SR    R1,R1                                                            
         IC    R1,PONLEN               USER FIELD LENGTH                        
         LHI   RF,PORDNOLQ+1           ORDER NUMBER LENGTH + 1                  
         CR    R1,RF                   IS USER FIELD MORE THAN JUST #?          
         BNH   IBMPOX                    NO, LEAVE C'001'                       
         CLI   PONUM+PORDNOLQ,C'-'     MUST BE C'-'                             
         BNE   IBMPOX                   NO, LEAVE C'001'                        
         SR    R1,RF                   FIND LENGTH OF DATA AFTER PO#            
         BNP   IBMPOX                    NOT +, LEAVE C'001'                    
         LHI   RF,3                    3=MAXIMUM SPACE IN E34I1AID              
         SR    RF,R1                   FIND INDEX INTO AID FIELD                
         BNP   IBMPOX                                                           
         LR    RE,R2                                                            
         AR    RE,RF                                                            
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),PONUM+PORDNOLQ+1                                         
IBMPOX   B     EXIT                                                             
PORDNOLQ EQU   10                                                               
         EJECT                                                                  
***********************************************************************         
* IBM JOB DESCRIPTION FIELD                                           *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
IBMJD    NTR1  ,                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
*                                                                               
         L     R4,CURBILL                                                       
         USING BXTABLED,R4                                                      
         MVC   0(6,R2),JOBCD       JOB                                          
         MVC   14(3,R2),CLTCD      CLIENT                                       
         MVC   17(3,R2),PRDCD      PRODUCT                                      
         MVC   21(36,R2),JOBNAM    JOB NAME                                     
         CP    BXAMOUNT,=P'0'                                                   
         BNL   *+10                                                             
         MVC   60(6,R2),=C'CREDIT'                                              
         OC    BXPBILNO,BXPBILNO   PREVIOUS BILL                                
         BZ    EXIT                                                             
         MVI   6(R2),C'/'                                                       
         MVC   7(L'BXPBILNO,R2),BXPBILNO                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* IBM PRODUCT DESCRIPTION                                             *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
IBMPD    NTR1  ,                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
*                                                                               
         L     R4,CURBILL                                                       
         USING BXTABLED,R4                                                      
         MVC   0(6,R2),JOBCD       JOB                                          
         MVC   14(3,R2),CLTCD      CLIENT                                       
         MVC   17(3,R2),PRDCD      PRODUCT                                      
         MVC   21(36,R2),PRDNAM    PRODUCT NAME                                 
         CP    BXAMOUNT,=P'0'                                                   
         BNL   *+10                                                             
         MVC   60(6,R2),=C'CREDIT'                                              
         OC    BXPBILNO,BXPBILNO   PREVIOUS BILL                                
         BZ    EXIT                                                             
         MVI   6(R2),C'/'                                                       
         MVC   7(L'BXPBILNO,R2),BXPBILNO                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* TRANSACTION TYPE FOR NE TO ANHEUSER BUSCH BIG07 FIELD BASED ON PONUM*         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
NEBG07   NTR1  ,                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
*                                                                               
         CLC   =C'48',PONUM                                                     
         BNE   *+10                                                             
         MVC   0(2,R2),=C'PR'                                                   
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
* READ USER FIELDS LI/PD/AF                                           *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
*  2(R4)=TYPE EQUATE  UFLDLIQ/UFLDPDQ/UFLDAFQ                         *         
*----------------------------------------------------------------------         
*                                                                               
UFLDNE   NTR1  ,                                                                
         LTR   R1,R1                                                            
         BZ    UFLD05                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
UFLD05   SR    R3,R3                                                            
         ICM   R3,3,2(R4)          LAST TWO BYTES OF ESC SEQUENCE               
*                                                                               
         L     R4,ADACC                                                         
         MVI   ELCODE,UFSELQ       X'A2'                                        
         BAS   RE,GETEL            FIND THE USER ELEMENT                        
         B     *+8                                                              
UFLD10   BAS   RE,NEXTEL                                                        
         BNE   UFLD60                                                           
         USING UFSELD,R4                                                        
*                                                                               
         CHI   R3,UFLDLIQ                                                       
         BNE   UFLD20                                                           
         CLC   UFSCODE,=C'LI'      USER CODE FOR ORDER NUMBER?                  
         BE    UFLD50                                                           
*                                                                               
UFLD20   CHI   R3,UFLDPDQ          DO WE WANT PD USER FIELD                     
         BNE   UFLD30                                                           
         CLC   UFSCODE,=C'PD'                                                   
         BE    UFLD50                                                           
*                                                                               
UFLD30   CHI   R3,UFLDAFQ                                                       
         BNE   UFLD40                                                           
         CLC   UFSCODE,=C'AF'                                                   
         BE    UFLD50                                                           
UFLD40   B     UFLD10              NO                                           
*                                                                               
UFLD50   SR    R5,R5                                                            
         IC    R5,UFSLN            ELEMENT LENGTH                               
         AHI   R5,-UFSLN1Q         R3=LENGTH OF DATA                            
         BNP   UFLD60                                                           
         CR    R5,R1                                                            
         BL    *+6                                                              
         LR    R5,R1               GET SHORTEST                                 
         LTR   R5,R5                                                            
         BZ    UFLDX                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),UFSDATA     TO OUTPUT LINE                               
         B     UFLDX                                                            
*                                                                               
UFLD60   CHI   R3,UFLDAFQ          USER FIELD AF                                
         BE    UFLD70                                                           
         CHI   R3,UFLDPDQ          USER FIELD PD                                
         BE    UFLD70                                                           
         CHI   R3,UFLDLIQ          USER FIELD LI                                
         BNE   UFLDX                                                            
UFLD70   MVI   SKIPREC,C'Y'        SKIP IF NOTHING IN USERFLD                   
*                                                                               
UFLDX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* WORK CODE 99 MINUS WORK CODE 05 AMOUNT EXTRACTION ROUTINE           *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
WC99M05  NTR1  ,                                                                
*                                                                               
*        ZAP   PKAMNT,=P'0'        INIT WC 99 MINUS WC 05 AMOUNT FIELD.         
         ZAP   PKAMNT,BLNTOT       ZAP WC 99 AMOUNT                             
         ZAP   WC05AMNT,=P'0'      INIT WC 05 AMOUNT.                           
         LR    R3,R1               SAVE OFF R1                                  
*                                                                               
         L     R4,ATRNSTBL         POINT TO BEGINNING OF TABLE                  
         USING BXTABLED,R4                                                      
         LH    R5,TBLENT                                                        
         LTR   R5,R5               ANY TABLE ENTRIES?                           
         BNP   WCNEX               NO                                           
WCNE10   CLC   BXWRKCD,=C'05'      IS THIS WORK CODE 05?                        
         BNE   WCNE20              NO                                           
         CLC   BXBILNO,BILNO       TABLE ENTRY MATCHES CURRENT BILL#?           
         BNE   WCNE20                                                           
         GOTO1 DATCON,DMCB,(2,BXBLDTE),(20,WORK)                                
         CLC   BILDTC,WORK         DOES WC 05'S BILL DATE MATCH 99'S?           
         BNE   WCNE20                                                           
*                                                                               
         SP    PKAMNT,BXAMOUNT                                                  
         AP    WC05AMNT,BXAMOUNT    WORK CODE 05 AMOUNT                         
         B     WCNE30                                                           
*                                                                               
WCNE20   LA    R4,BXTABLNQ(R4)                                                  
         BCT   R5,WCNE10                                                        
WCNE30   LR    R1,R3               RESET R1 AND MOVE AMNT TO OUTPUT             
         UNPK  WORK(15),PKAMNT     WC99-WC05                                    
         LA    R3,WORK+14                                                       
         GOTOR FIXNEG                                                           
         LA    RF,0(R1,R2)                                                      
         BCTR  RF,0                RF TO END OF DESTINATION                     
         LA    RE,WORK+14          RE TO END OF EDITED AMOUNT                   
         MVC   0(1,RF),0(RE)       MOVE FROM RIGHT TO LEFT                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
WCNEX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT WC 05 AMOUNT TO RECORD                                         *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
WC05     NTR1  ,                                                                
         UNPK  WORK(15),WC05AMNT                                                
         LA    R3,WORK+14                                                       
         GOTOR FIXNEG                                                           
         LA    RF,0(R1,R2)                                                      
         BCTR  RF,0                RF TO END OF DESTINATION                     
         LA    RE,WORK+14          RE TO END OF EDITED AMOUNT                   
         MVC   0(1,RF),0(RE)       MOVE FROM RIGHT TO LEFT                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VENDOR CODE ROUTINE                                                 *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
VENCDE   NTR1  ,                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES      INIT FIELD WITH SPACES                       
*                                                                               
         CLC   ALPHAID,=C'NE'      CODE FOR DDB                                 
         BE    VENCNE                                                           
         CLC   ALPHAID,=C'OU'      CODE FOR OMD                                 
         BE    VENCOU                                                           
*                                                                               
VENCDEX  B     EXIT                                                             
*                                                                               
* VENDOR CODE INTO REFERENCE # FOR DDB                                          
*                                                                               
VENCNE   DS    0H                                                               
         MVC   0(10,R2),=C'0000466883'  DEFAULT IS 466883 FOR NE                
         CLC   ORIGINUM,=X'0584'        SPECIAL FOR DNNYE                       
         BNE   VENCNEX                                                          
         MVC   0(10,R2),=C'0008202450'                                          
VENCNEX  B     VENCDEX                                                          
*                                                                               
* VENDOR CODE FOR OMD                                                           
*                                                                               
VENCOU   DS    0H                                                               
         MVC   0(8,R2),=C'60070580'  DEFAULT IS 60070580 FOR OMD                
         CLI   ACCT+6,C'F'           SPECIAL FOR MEDIA F                        
         BNE   VENCOUX                                                          
         MVC   0(8,R2),=C'60066119'                                             
VENCOUX  B     VENCDEX                                                          
         EJECT                                                                  
***********************************************************************         
* MEDIA NAME ROUTINE                                                  *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
MEDNME   NTR1  ,                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES      INIT FIELD WITH SPACES                       
*                                                                               
         L     R4,AIO2                                                          
         USING PMDRECD,R4           PRODUCTION MEDIA RECORD X'09'               
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ     X'09'                                       
*                                                                               
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         USING TRNRECD,R5           TRANSACTION RECORD KEY                      
         MVC   PMDKCPY,TRNKCPY      COMPANY                                     
         MVC   PMDKMED,TRNKACT+6    MEDIA IS FIRST BYTE OF JOB                  
         DROP  R5                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT ',(R4),(R4),0                     
         BNE   MEDNMEX                                                          
*                                                                               
         USING PMDELD,R4            PRODUCTION MEDIA ELEMENT X'11'              
         MVI   ELCODE,PMDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   MEDNMEX              CANNOT FIND PROD MEDIA ELEM                 
         MVC   0(L'PMDDESC,R2),PMDDESC     MOVE IN DESCRIPTION                  
MEDNMEX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BILL #/PROD CODE-NAME/JOB CODE                                      *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
BPJ      NTR1  ,                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES      INIT FIELD WITH SPACES                       
*                                                                               
         MVC   0(6,R2),BILNO       BILL NUMBER                                  
         MVC   7(3,R2),PRDCD       PRODUCT CODE                                 
         MVI   10(R2),C'-'                                                      
         MVC   11(36,R2),PRDNAM    PRODUCT NAME                                 
         MVC   48(6,R2),JOBCD      JOB CODE                                     
*                                                                               
BPJX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET GST                                                             *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
GST      NTR1  ,                                                                
         CP    GSTTOT,=P'0'                                                     
         BNE   *+8                                                              
         MVI   SKIPREC,C'Y'                                                     
*                                                                               
         BCTR  R1,0                                                             
*                                                                               
         LR    R3,R1                                                            
         LR    RE,R2                                                            
         MVI   0(RE),C'0'                                                       
GST10    MVC   1(1,RE),0(RE)                                                    
         AHI   RE,1                                                             
         BCT   R3,GST10                                                         
*                                                                               
         UNPK  WORK(11),GSTTOT                                                  
         LA    R3,WORK+10                                                       
         GOTOR FIXNEG                                                           
         LA    RF,0(R1,R2)         RF TO END OF DESTINATION                     
         LA    RE,WORK+10          RE TO END OF EDITED AMOUNT                   
         LA    R6,11               LENGTH OF EDITED AMOUNT                      
         MVC   0(1,RF),0(RE)       MOVE FROM RIGHT TO LEFT                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         BCT   R6,*-10                                                          
GSTX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET QST                                                             *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
QST      NTR1  ,                                                                
         CP    QSTTOT,=P'0'                                                     
         BNE   *+8                                                              
         MVI   SKIPREC,C'Y'                                                     
*                                                                               
         BCTR  R1,0                                                             
*                                                                               
         LR    R3,R1                                                            
         LR    RE,R2                                                            
         MVI   0(RE),C'0'                                                       
QST10    MVC   1(1,RE),0(RE)                                                    
         AHI   RE,1                                                             
         BCT   R3,QST10                                                         
*                                                                               
         UNPK  WORK(11),QSTTOT                                                  
         LA    R3,WORK+10                                                       
         GOTOR FIXNEG                                                           
         LA    RF,0(R1,R2)         RF TO END OF DESTINATION                     
         LA    RE,WORK+10          RE TO END OF EDITED AMOUNT                   
         LA    R6,11               LENGTH OF EDITED AMOUNT                      
         MVC   0(1,RF),0(RE)       MOVE FROM RIGHT TO LEFT                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         BCT   R6,*-10                                                          
QSTX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UNIT PRICE                                                          *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
UPRICE   NTR1  ,                                                                
         USING BXTABLED,R4                                                      
         L     R4,ALOOP            POINT TO WHERE WE ARE IN LOOP                
         LTR   R4,R4                                                            
         BNZ   UPR10                INITIALIZE COUNT IF START OF LOOP           
         ZAP   IT1TOT,=P'1'        ACCOUNT FOR PREVIOUS IT1 SEGMENT             
         L     R4,ATRNSTBL         POINT TO BEGINNING OF TABLE                  
UPR10    CLC   BXBILNO,BILNO       CURRENT BILL NUMBER?                         
         BE    UPR20                                                            
         LA    R4,BXTABLNQ(R4)     BUMP TO NEXT ENTRY                           
         B     UPR10                                                            
*                                                                               
UPR20    CLC   BXWRKCD,=C'99'      STOP AT 99                                   
         BE    UPR50                                                            
         UNPK  WORK(15),BXAMOUNT                                                
         LA    R3,WORK+14                                                       
         GOTOR FIXNEG                                                           
         LA    RF,0(R1,R2)                                                      
         BCTR  RF,0                RF TO END OF DESTINATION                     
         LA    RE,WORK+14          RE TO END OF EDITED AMOUNT                   
         MVC   0(1,RF),0(RE)       MOVE FROM RIGHT TO LEFT                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         LA    R4,BXTABLNQ(R4)     BUMP TO NEXT ENTRY                           
         ST    R4,ALOOP            SAVE PLACE IN TABLE FOR LOOP                 
         MVI   RTNROU,C'Y'         SET RETURN STATUS TO Y                       
         AP    IT1TOT,=P'1'                                                     
         B     UPRICEX                                                          
*                                                                               
UPR50    MVI   RTNROU,C'N'         CHANGE RETURN STATUS TO N                    
         MVI   SKIPREC,C'Y'        SKIP THIS SEGMENT                            
         OI    LOOPFLG,LPSKIP      TURN ON SKIP FLAG                            
UPRICEX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* COUNT OF IT1 SEGMENTS                                               *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
IT1CNT   NTR1  ,                                                                
         BCTR  R1,0                                                             
*                                                                               
         LR    R3,R1                                                            
         LR    RE,R2                                                            
         MVI   0(RE),C'0'                                                       
IT10     MVC   1(1,RE),0(RE)                                                    
         AHI   RE,1                                                             
         BCT   R3,IT10                                                          
*                                                                               
         UNPK  WORK(11),IT1TOT                                                  
         LA    R3,WORK+10                                                       
         GOTOR FIXNEG                                                           
         LA    RF,0(R1,R2)         RF TO END OF DESTINATION                     
         LA    RE,WORK+10          RE TO END OF EDITED AMOUNT                   
         LA    R6,11               LENGTH OF EDITED AMOUNT                      
         MVC   0(1,RF),0(RE)       MOVE FROM RIGHT TO LEFT                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         BCT   R6,*-10                                                          
ITX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EDIT BILL TOTAL + TAX TO RECORD                                     *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
BNTTOTR  NTR1  ,                                                                
         ZAP   BNTTOT,BLNTOT       GET TOTAL                                    
         AP    BNTTOT,GSTTOT       ADD TAX                                      
         AP    BNTTOT,QSTTOT       ADD TAX                                      
         UNPK  WORK(15),BNTTOT                                                  
         LA    R3,WORK+14                                                       
         GOTOR FIXNEG                                                           
         LA    RF,0(R1,R2)                                                      
         BCTR  RF,0                RF TO END OF DESTINATION                     
         LA    RE,WORK+14          RE TO END OF EDITED AMOUNT                   
         MVC   0(1,RF),0(RE)       MOVE FROM RIGHT TO LEFT                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EDIT BILL TOTAL TO RECORD  ( NO NEGATIVES SPECIAL FOR SHELL )       *         
*  R1 = LENGTH OF DESTINATION FIELD                                   *         
*  R2 = A(DESTINATION FIELD)                                          *         
***********************************************************************         
                                                                                
BILTOTS  NTR1  ,                                                                
         CP    BLNTOT,=P'0'                                                     
         BP    *+8                                                              
         XI    BLNTOT+L'BLNTOT-1,X'01' REVERSE SIGN TO MAKE IT +VE.             
         UNPK  WORK(15),BLNTOT                                                  
         LA    R3,WORK+14                                                       
         GOTOR FIXNEG                                                           
         LA    RF,0(R1,R2)                                                      
         BCTR  RF,0                RF TO END OF DESTINATION                     
         LA    RE,WORK+14          RE TO END OF EDITED AMOUNT                   
         MVC   0(1,RF),0(RE)       MOVE FROM RIGHT TO LEFT                      
         BCTR  RF,0                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
                                                                                
VXSORT   DC    V(XSORT)                                                         
VHELLO   DC    V(HELLO)                                                         
AIO      DC    A(BXIO)                                                          
AIO2     DC    A(IO2)                                                           
ATRNSTBL DC    A(TRNSTBL)          TRANSACTION TABLE                            
AAGYTAB  DC    A(AGYTAB)           AGENCY TABLES                                
*                                                                               
*                                  EQUATES FOR EXTRACT DATA                     
CPYNMQ   EQU   1                   COMPANY NAME                                 
CLNMQ    EQU   2                   CLIENT NAME                                  
JOBCDQ   EQU   3                   JOB NUMBER                                   
BILNOQ   EQU   4                   BILL NUMBER                                  
BILDTQ   EQU   5                   BILL DATE                                    
BILTOTQ  EQU   6                   BILL TOTAL                                   
DUEDTQ   EQU   7                   DUE DATE                                     
PONUMQ   EQU   8                   PO NUMBER                                    
CONTACTQ EQU   9                   CONTACT                                      
PHONEQ   EQU   10                  PHONE                                        
TRNTYPQ  EQU   11                  TRANSACTION TYPE                             
ADDR1Q   EQU   12                  ADDRESS LINE 1                               
IBMPOR   EQU   13                  IBM SPECIAL PO NUMBER                        
IBMJDR   EQU   14                  IBM JOB DESCRIPTION FIELD                    
BILDTCQ  EQU   15                  BILL DATE                                    
REFNUMQ  EQU   16                  REFERENCE NUMBER SPECIAL FOR NE              
IBMPDR   EQU   17                                                               
H7RMNMQ  EQU   18                  REMITTER'S NAME H7 CLT=BP                    
H7RMAD1Q EQU   19                  REMITTER'S ADDRS LINE 1 H7 CLT=BP            
H7RMCITQ EQU   20                  REMITTER'S CITY                              
H7RMSTAQ EQU   21                  REMITTER'S STATE                             
H7RMZIPQ EQU   22                  REMITTER'S ZIP CODE                          
WC99M05Q EQU   23                  GET WORK CODE 99 MINUS 05 AMOUNT             
WC05Q    EQU   24                  WORK CODE 05 AMOUNT                          
NEBG07Q  EQU   25                  TRANSACTION TYPE FOR NE                      
JOBNAMQ  EQU   26                  JOB NAME                                     
UFLDLIQ  EQU   27                  USER FIELD LI                                
UFLDPDQ  EQU   28                  USER FIELD PD                                
UFLDAFQ  EQU   29                  USER FIELD AF                                
CLTCDQ   EQU   30                  CLIENT CODE                                  
PRDCDQ   EQU   31                  PRODUCT CODE                                 
VENCDEQ  EQU   32                  VENDOR CODE                                  
UFLD1Q   EQU   33                  USER FIELD 1                                 
UFLD2Q   EQU   34                  USER FIELD 2                                 
UFLD3Q   EQU   35                  USER FIELD 3                                 
UFLD4Q   EQU   36                  USER FIELD 4                                 
MEDNMEQ  EQU   37                  MEDIA NAME                                   
BPJQ     EQU   38                  BILL CODE/PROD CODE-NAME/JOB CODE            
DUEDT2Q  EQU   39                  DUE DATE                                     
GSTQ     EQU   40                  GST AMOUNT                                   
QSTQ     EQU   41                  QST AMOUNT                                   
UPRICEQ  EQU   42                  UNIT PRICE                                   
IT1CNTQ  EQU   43                  LINE COUNT                                   
BNTTOTQ  EQU   44                  BILL+TAX                                     
PRDNAMQ  EQU   45                  PRODUCT NAME                                 
BILTOTSH EQU   46                  BILL TOTAL +VE ONLY, FOR SHELL               
*                                                                               
EOT      EQU   X'FF'               END OF TABLE                                 
EOR      EQU   X'00'               END OF RECORD                                
ESC      EQU   X'01'               ESCAPE                                       
BEGQ     EQU   X'02'               BEGINING OF LOOP                             
ENDQ     EQU   X'03'               END OF LOOP                                  
*                                                                               
XDATA    DS    0F                                                               
         DC    AL1(L'CMPNM),AL3(CMPNM)       COMPANY NAME                       
         DC    AL1(L'CLTNAM),AL3(CLTNAM)     CLIENT NAME                        
         DC    AL1(L'JOBCD),AL3(JOBCD)       JOB CODE                           
         DC    AL1(L'BILNO),AL3(BILNO)       BILL NUMBER                        
         DC    AL1(L'BILDT),AL3(BILDT)       BILL DATE YYMMDD                   
         DC    X'80',AL3(BILTOTR)            BILL TOTAL - ROUTINE               
         DC    AL1(0),AL3(DUEDT)             DUE DATE                           
         DC    AL1(PORDNOLQ),AL3(PONUM)      P.O. NUMBER                        
         DC    AL1(L'CONTACT),AL3(CONTACT)   CONTACT                            
         DC    AL1(L'PHONE),AL3(PHONE)       PHONE                              
         DC    AL1(L'TRNTYP),AL3(TRNTYP)     TRANSACTION TYPE                   
         DC    AL1(L'CMPADD),AL3(CMPADD)     ADDRESS 1                          
         DC    X'80',AL3(IBMPO)              IBM PO DESCRIPTION                 
         DC    X'80',AL3(IBMJD)              IBM JOB DESCRIPTION                
         DC    AL1(L'BILDTC),AL3(BILDTC)     BILL DATE CCYYMMDD                 
         DC    X'80',AL3(REFNUM)             REF NUM BASED ON CLT/PRD           
         DC    X'80',AL3(IBMPD)              IBM PRODUCT DESCRIPTION            
         DC    AL1(L'H7RMTNAM),AL3(H7RMTNAM) REMITTERS NAME HARD CODED          
         DC    AL1(L'H7RMTAD1),AL3(H7RMTAD1) REMITTERS ADDRS LINE 1             
         DC    AL1(L'H7RMTCIT),AL3(H7RMTCIT) REMITTERS CITY                     
         DC    AL1(L'H7RMTSTA),AL3(H7RMTSTA) REMITTERS STATE                    
         DC    AL1(L'H7RMTZIP),AL3(H7RMTZIP) REMITTERS ZIP CODE                 
         DC    X'80',AL3(WC99M05)            WC99 - WC05 AMOUNT                 
         DC    X'80',AL3(WC05)               WORK CODE 05 AMOUNT                
         DC    X'80',AL3(NEBG07)             TRAN TYPE BASED ON PONUM           
         DC    AL1(L'JOBNAM),AL3(JOBNAM)     JOB NAME                           
         DC    X'80',AL3(UFLDNE)             USER FIELD LI                      
         DC    X'80',AL3(UFLDNE)             USER FIELD PD                      
         DC    X'80',AL3(UFLDNE)             USER FIELD AF                      
         DC    AL1(L'CLTCD),AL3(CLTCD)       CLIENT CODE                        
         DC    AL1(L'PRDCD),AL3(PRDCD)       PRODUCT CODE                       
         DC    X'80',AL3(VENCDE)             VENDOR CODE FOR DNNYE              
         DC    AL1(L'UFLD1),AL3(UFLD1)       USER FIELD 1 FOR OMD               
         DC    AL1(L'UFLD2),AL3(UFLD2)       USER FIELD 2 FOR OMD               
         DC    AL1(L'UFLD3),AL3(UFLD3)       USER FIELD 2 FOR OMD               
         DC    AL1(L'UFLD4),AL3(UFLD4)       USER FIELD 4 FOR OMD               
         DC    X'80',AL3(MEDNME)             MEDIA NAME                         
         DC    X'80',AL3(BPJ)                BILL CDE/PROD CDE-NME/JOB          
         DC    AL1(0),AL3(DUEDT2)            DUE DATE YYYYMMDD                  
         DC    X'80',AL3(GST)                GST AMOUNT                         
         DC    X'80',AL3(QST)                QST AMOUNT                         
         DC    X'80',AL3(UPRICE)             UNIT PRICE                         
         DC    X'80',AL3(IT1CNT)             LINE COUNT                         
         DC    X'80',AL3(BNTTOTR)            BILL+TAX TOTAL - ROUTINE           
         DC    AL1(L'PRDNAM),AL3(PRDNAM)     PRODUCT NAME                       
         DC    X'80',AL3(BILTOTS)            BILL TOTAL  NO NEGATIVES           
*                                                                               
CMPNM    DS    CL33                COMPANY NAME                                 
CMPADD   DS    CL33                COMPANY ADDRESS                              
CLTNAM   DS    CL36                CLIENT NAME                                  
PRDNAM   DS    CL36                PRODUCT NAME                                 
JOBNAM   DS    CL36                JOB NAME                                     
*                                                                               
ACCT     DS    0CL12               ACCOUNT                                      
CLTCD    DS    CL3                 CLIENT CODE                                  
PRDCD    DS    CL3                 PRODUCT CODE                                 
JOBCD    DS    CL6                 JOB CODE                                     
BILNO    DS    CL6                 BILL NUMBER                                  
BILDTC   DS    0CL8                BILL DATE CC                                 
         DS    CL2                                                              
BILDT    DS    CL6                 BILL DATE YYMMDD                             
DUEDT    DS    CL8                 DUE DATE YYMMDD                              
DUEDT2   DS    CL8                 DUE DATE YYYYMMDD                            
PONUM    DS    CL25                P.O. NUMBER                                  
PONLEN   DS    XL1                 LENGTH OF PO NUMBER                          
TRNTYP   DS    CL2                 TRANSACTION TYPE                             
PKAMNT   DS    PL8                 PACKED FIELD                                 
WC05AMNT DS    PL8                 WORK CODE 05 AMNT FOR NE ROUTINE             
UFLDS    DS    0CL320                                                           
UFLD1    DS    CL80                USER FIELD 1                                 
UFLD2    DS    CL80                           2                                 
UFLD3    DS    CL80                           3                                 
UFLD4    DS    CL80                           4                                 
ALOOP    DS    A                   LOOP POINTER                                 
*                                                                               
AGYDATA  DS    0D                  AGENCY DATA                                  
AGYACCL  DS    A                   A(ACCLAST HOOK)                              
         DS    4A                  N/D                                          
CONTACT  DS    CL35                CONTACT                                      
PHONE    DS    CL10                PHONE                                        
VENDOR   DS    CL10                VENDOR                                       
REPFLAG  DS    X                                                                
CHKREVUS EQU   X'10'               CHECK PTASTAT1 FOR PTASREVU+PTASREVS         
SHOVENNO EQU   X'08'               SHOW VENDOR NUMBER ON REPORT                 
WKCDDET  EQU   X'04'               SHOW WORK CODE DETAIL                        
NEEDREP  EQU   X'01'               PRINTED REPORT NEEDED                        
USERPO   DS    CL2                 USER CODE FOR PO NUMBER                      
AGYLNQ   EQU   *-AGYDATA                                                        
*                                                                               
H7DATA   DS    0D                  AGY SPECIFIC DATA/REUSABLE STORAGE           
H7RMTNAM DS    CL35                BP REMITTER'S NAME                           
H7RMTAD1 DS    CL35                BP REMITTER'S ADDRESS LINE 1                 
H7RMTCIT DS    CL30                CITY                                         
H7RMTSTA DS    CL2                 STATE                                        
H7RMTZIP DS    CL9                 ZIP CODE                                     
*                                                                               
BLNTOT   DC    PL8'0'                                                           
JOBTOT   DC    PL8'0'                                                           
PRDTOT   DC    PL8'0'                                                           
CLTTOT   DC    PL8'0'                                                           
GSTTOT   DC    PL6'0'                                                           
QSTTOT   DC    PL6'0'                                                           
IT1TOT   DC    PL6'1'                                                           
BNTTOT   DC    PL8'0'                                                           
*                                                                               
CLIBLK   DC    AL4(CLITXT)                                                      
         DC    AL1(3),AL3(CLTCD)                                                
         DC    AL4(CLTNAM)                                                      
         DC    AL4(CLTTOT)                                                      
*                                                                               
PRDBLK   DC    AL4(PRDTXT)                                                      
         DC    AL1(3),AL3(PRDCD)                                                
         DC    AL4(PRDNAM)                                                      
         DC    AL4(PRDTOT)                                                      
*                                                                               
JOBBLK   DC    AL4(JOBTXT)                                                      
         DC    AL1(6),AL3(JOBCD)                                                
         DC    AL4(JOBNAM)                                                      
         DC    AL4(JOBTOT)                                                      
*                                                                               
TESTDDSB DC    C'*B'                                                            
*                                                                               
CLITXT   DC    C'CLT -'                                                         
PRDTXT   DC    C'PRD -'                                                         
JOBTXT   DC    C'JOB -'                                                         
*                                                                               
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
DDPARM   DC    CL8'EDIOUT'                                                      
DSPARM   DC    CL20'ACCTAPE.AC0BX**1'                                           
*                                                                               
EDIOUT   DCB   DDNAME=EDIOUT,DSORG=PS,MACRF=(PM),                      +        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
TRNSTBL  DS    (MXENTRY)XL(BXTABLNQ)  TRANSACTION TABLE                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* AGENCY TABLES                                                       *         
***********************************************************************         
         DS    0D                                                               
AGYTAB   DS    0CL(L'ADLIST)                                                    
*                                                                               
         DC    C'*B',AL2(0)         DDSB - TESTING                              
         DC    C'   ',C' '          CLIENT                                      
         DC    A(DDSB)              A(AGENCY DATA)                              
*                                                                               
         DC    C'H7',AL2(0)         MSNYA - MINDSHARE                           
         DC    C'BPC',C' '          CLIENT BP PLC                               
         DC    A(H7BPC)             A(AGENCY DATA)                              
*                                                                               
         DC    C'H7',AL2(0)         MSNYA - MINDSHARE                           
         DC    C'BPE',C' '          CLIENT BP PLC                               
         DC    A(H7BPC)             A(AGENCY DATA)                              
*                                                                               
         DC    C'NE',AL2(0)         DNCH - DDB NEEDHAM                          
         DC    C'DPA',C' '          CLIENT                                      
         DC    A(NEDELL)            A(AGENCY DATA)                              
*                                                                               
         DC    C'NE',AL2(0)         DNCH - DDB NEEDHAM                          
         DC    C'DPC',C' '          CLIENT                                      
         DC    A(NEDELL)            A(AGENCY DATA)                              
*                                                                               
         DC    C'NE',AL2(0)         DNCH - DDB NEEDHAM                          
         DC    C'   ',C'5'          CLIENT                                      
         DC    A(NEOLD)             A(AGENCY DATA)                              
*                                                                               
         DC    C'NE',AL2(0)         DNCH - DDB NEEDHAM                          
         DC    C'   ',C' '          CLIENT                                      
         DC    A(NE)                A(AGENCY DATA)                              
*                                                                               
         DC    C'OA',AL2(0)         O&M - OGILVY AND MATHER                     
         DC    C'AIG',C' '          CLIENT                                      
         DC    A(OAAIG)             A(AGENCY DATA)                              
*                                                                               
         DC    C'OA',AL2(0)         O&M - OGILVY AND MATHER                     
         DC    C'   ',C' '          CLIENT                                      
         DC    A(OA)                A(AGENCY DATA)                              
*                                                                               
         DC    C'OD',AL2(0)         OGILVYONE - OGILVY DIRECT                   
         DC    C'   ',C' '          CLIENT                                      
         DC    A(OA)                A(AGENCY DATA)                              
*                                                                               
         DC    C'OU',AL2(0)         OMDTOA - OMD CANADA                         
         DC    C'   ',C' '          CLIENT                                      
         DC    A(OU)                A(AGENCY DATA)                              
*                                                                               
         DC    C'M2',AL2(0)         MEDIACOM CL#0289105N                        
         DC    C'   ',C' '          CLIENT                                      
         DC    A(SHELL)             A(AGENCY DATA)                              
*                                                                               
         DC    C'H7',AL2(0)         MINDSHARE CL#0289105N                       
         DC    C'   ',C' '          CLIENT                                      
         DC    A(SHELL)             A(AGENCY DATA)                              
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DDSB - DDSB IS FOR TESTING PURPOSES                                 *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
DDSB     DS    0CL(AGYLNQ)                    AGENCY DATA                       
         DC    A(0)                           A(ACCLAST HOOK)                   
         DC    4A(0)                          N/D                               
         DC    CL(L'CONTACT)'ANTHONY WILLIAMS' NAME                             
         DC    CL(L'PHONE)'2126335802'        PHONE                             
         DC    CL(L'VENDOR)' '                VENDOR NUMBER                     
         DC    AL1(NEEDREP)                   REPORT OPTIONS                    
         DC    C'PO'                          USER CODE FOR PO                  
*                                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                                                        
         DC    CL15'DDSB TESTS'               PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BIG'                                                           
         DC    C'002'                                                           
         DC    AL1(ESC),AL1(6),AL2(BILDTQ)    INVOICE DATE                      
         DC    AL1(ESC),AL1(22),AL2(BILNOQ)   INVOICE NUMBER                    
         DC    CL6' '                         PO ORDER DATE                     
         DC    AL1(ESC),AL1(22),AL2(PONUMQ)   PO ORDER NUMBER                   
         DC    CL30' '                        RELEASE NUMBER                    
         DC    CL8' '                         CHANGE ORDER SEQ                  
         DC    AL1(ESC),AL1(2),AL2(TRNTYPQ)   TRANSACTION TYPE CODE             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'005'                                                           
         DC    CL6'CR'                        REF. NUMBER QUALIFIER             
         DC    CL22'1000098360'               REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PER'                                                           
         DC    C'006'                                                           
         DC    C'IC'                          CONTACT FUNCTION CODE             
         DC    AL1(ESC),AL1(35),AL2(CONTACTQ) CONTACT                           
         DC    C'TE'                          COMMUNICATION QUALIFIER           
         DC    AL1(ESC),AL1(80),AL2(PHONEQ)   COMMUNICATION NUMBER              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'007'                                                           
         DC    CL2'RE'                        ENTITY IDENTIFIER                 
         DC    AL1(ESC),AL1(35),AL2(CPYNMQ)   COMPANY NAME                      
         DC    CL2' '                         IDENTIFICATION QUALIFIER          
         DC    CL17' '                        IDENTIFICATION CODE               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N3 '                                                           
         DC    C'009'                                                           
         DC    AL1(ESC),AL1(35),AL2(ADDR1Q)   ADDRESS 1                         
         DC    CL35' '                        ADDRESS 2                         
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N4 '                                                           
         DC    C'010'                                                           
         DC    CL30'NEW YORK'                 CITY                              
         DC    CL2'NY'                        STATE                             
         DC    CL9' '                         POSTAL CODE                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'027'                                                           
         DC    AL1(ESC),AL1(11),AL2(IBMPOR)    ASSIGNED ID.                     
         DC    C'0'                            NUMBER OF DP                     
         DC    CL10'0000000001'                QUANTITY INVOICED                
         DC    CL2'EA'                         UNIT OF BASIS                    
         DC    C'2'                            NUMBER OF DP                     
         DC    AL1(ESC),AL1(14),AL2(BILTOTQ)   UNIT PRICE                       
         DC    CL2' '                          BASIS OF UNIT PRICE CODE         
         DC    CL2' '                          PRODUCT ID QUALIFIER             
         DC    CL30' '                         PRODUCT/SERVICE ID               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'035'                                                           
         DC    C'F'                            ITEM DESCRIPTION TYPE            
         DC    CL3' '                          PRODUCT/PROCESS CODE             
         DC    CL2' '                          AGENCY QUALIFIER CODE            
         DC    CL12' '                         PRODUCT DESCRIPTION CODE         
         DC    AL1(ESC),AL1(80),AL2(IBMJDR)    DESCRIPTION                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TDS'                                                           
         DC    C'063'                                                           
         DC    C'2'                            NUMBER OF DP                     
         DC    AL1(ESC),AL1(10),AL2(BILTOTQ)   TOTAL INVOICE                    
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CTT'                                                           
         DC    C'069'                                                           
         DC    C'0'                            NUMBER OF DP                     
         DC    CL6'000001'                     NUMBER OF LINE ITEMS             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                        END OF TABLE                     
         EJECT                                                                  
***********************************************************************         
* DDB NEEDHAM - ANHEUSER BUSCH IMDB#2262961                          **         
***********************************************************************         
*                                                                               
         DS    0D                                                               
NE       DS    0CL(AGYLNQ)                    AGENCY DATA                       
         DC    A(0)                           A(ACCLAST HOOK)                   
         DC    4A(0)                          N/D                               
         DC    CL(L'CONTACT)'GINNY ZOOK'      NAME                              
         DC    CL(L'PHONE)'3125526823'        PHONE                             
         DC    CL(L'VENDOR)'466883'           VENDOR NUMBER                     
         DC    AL1(NEEDREP+WKCDDET+SHOVENNO+CHKREVUS)                           
         DC    C'ON'                          USER CODE FOR PO                  
*                                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                      =C'DDS'                           
         DC    CL15'AB-BILLING 4010'          PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BIG'                                                           
         DC    C'002'                                                           
         DC    AL1(ESC),AL1(8),AL2(BILDTCQ)   INVOICE DATE                      
         DC    AL1(ESC),AL1(6),AL2(BILNOQ)    BILL NUMBER                       
         DC    CL16' '                                                          
         DC    CL8' '                         PURCHASE ORDER DATE               
         DC    AL1(ESC),AL1(22),AL2(PONUMQ)   PO NUMBER                         
         DC    CL30' '                        RELEASE NUMBER                    
         DC    CL8' '                         CHANGE ORDER SEQ. NUMBER          
*        DC    CL2' '                         TRANSACTION TYPE CODE             
         DC    AL1(ESC),AL1(2),AL2(NEBG07Q)   TRANSACTION TYPE CODE             
         DC    CL2'00'                        TRANS SET PURPOSE CODE            
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'008'                                                           
         DC    CL3'VN'                        ENTITY IDENTIFIER                 
         DC    AL1(ESC),AL1(60),AL2(CPYNMQ)   COMPANY NAME                      
         DC    CL2'92'                        ID. CODE QUALIFIER                
         DC    AL1(ESC),AL1(17),AL2(VENCDEQ)  REFERENCE NUMBER                  
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PER'                                                           
         DC    C'013'                                                           
         DC    CL2'BI'                        CONTACT FUNCTION CODE             
         DC    AL1(ESC),AL1(60),AL2(CONTACTQ) CONTACT                           
         DC    CL2'TE'                        COMMUNICATION QUALIFIER           
         DC    AL1(ESC),AL1(80),AL2(PHONEQ)   COMMUNICATION NUMBER              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'008'                                                           
         DC    CL3'BT'                        ENTITY IDENTIFIER                 
         DC    CL60'ANHEUSER-BUSCH, INC.'     CLIENT NAME                       
         DC    CL2' '                         ID. CODE QUALIFIER                
         DC    CL17' '                        ID. CODE                          
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N9 '                                                           
         DC    C'028'                                                           
         DC    CL3'ME'                        REF ID QUALIFIER                  
         DC    CL30' '                        REFERECNCE ID                     
         DC    AL1(ESC),AL1(45),AL2(JOBNAMQ)  FREE-FORM DESCRIPTION             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'MSG'                                                           
         DC    C'029'                                                           
         DC    AL1(ESC),AL1(L'CLTCD),AL2(CLTCDQ) FREE-FORM MESSAGE              
         DC    C'/'                                                             
         DC    AL1(ESC),AL1(L'PRDCD),AL2(PRDCDQ) FREE-FORM MESSAGE TEXT         
         DC    C'/'                                                             
         DC    AL1(ESC),AL1(L'JOBCD),AL2(JOBCDQ) FREE-FORM MESSAGE TEXT         
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'035'                                                           
         DC    AL1(ESC),AL1(20),AL2(UFLDLIQ)  FREE-FORM MESSAGE TEXT            
         DC    C'0'                           NUMBER OF DECIMAL PLACES          
         DC    CL10'0000000001'               QUANTITY INVOICED                 
         DC    CL2'KU'                        UNIT OR BASIS                     
         DC    C'2'                           NUMBER OF DP                      
         DC    CL17'00000000000000000'        QUANTITY INVOICED                 
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'044'                                                           
         DC    AL1(ESC),AL1(0),AL2(UFLDLIQ)   DO WE WANT TO SKIP THIS?          
         DC    C'F'                           ITEM DESCRIPTION TYPE             
         DC    CL3' '                         CHARACTERISTIC CODE               
         DC    CL2' '                         AGENCY QUALIFIER CODE             
         DC    CL12' '                        PRODUCT DESCRIPTION CODE          
         DC    AL1(ESC),AL1(80),AL2(JOBNAMQ)  JOB DESCRIPTION                   
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'SLN'                         SUBLINE ITEM DETAIL               
         DC    C'060'                                                           
         DC    AL1(ESC),AL1(20),AL2(UFLDPDQ)  ASSIGNED IDENTIFICATION           
         DC    CL20'  '                       ASSIGNED IDENTIFICATION           
         DC    C'I'                           RELATIONSHIP CODE                 
         DC    C'2'                           NUMBER OF DECIMAL PLACES          
         DC    AL1(ESC),AL1(15),AL2(WC99M05Q) TOTAL MINUS WORKCODE 05           
         DC    CL2'DO'                        UNIT OR BASIS FOR MSRMNT          
         DC    CL16' '                        EXPONENT                          
         DC    CL11'00000000001'              MULTIPLIER                        
         DC    CL29' '                                                          
         DC    CL29' '                                                          
         DC    CL29' '                                                          
         DC    CL29' '                                                          
         DC    CL18'000000000000000001'       UNIT PRICE                        
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'063'                                                           
         DC    AL1(ESC),AL1(0),AL2(UFLDPDQ)   DO WE WANT TO SKIP THIS?          
         DC    C'F'                           ITEM DESCRIPTION TYPE             
         DC    CL3' '                         CHARACTERISTIC CODE               
         DC    CL2' '                         AGENCY QUALIFIER CODE             
         DC    CL12' '                        PRODUCT DESCRIPTION CODE          
         DC    AL1(ESC),AL1(80),AL2(JOBNAMQ)  JOB DESCRIPTION                   
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'SLN'                         SUBLINE ITEM DETAIL               
         DC    C'060'                                                           
         DC    AL1(ESC),AL1(20),AL2(UFLDAFQ)  ASSIGNED IDENTIFICATION           
         DC    CL20'  '                       ASSIGNED IDENTIFICATION           
         DC    C'I'                           RELATIONSHIP CODE                 
         DC    C'2'                           NUMBER OF DECIMAL PLACES          
         DC    AL1(ESC),AL1(15),AL2(WC05Q)    WORKCODE 05 AMOUNT QTY            
         DC    CL2'DO'                        UNIT OR BASIS                     
         DC    CL16' '                        EXPONENT                          
         DC    CL11'00000000001'              MULTIPLIER                        
         DC    CL29' '                                                          
         DC    CL29' '                                                          
         DC    CL29' '                                                          
         DC    CL29' '                                                          
         DC    CL18'000000000000000001'       UNIT PRICE                        
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'063'                                                           
         DC    AL1(ESC),AL1(0),AL2(UFLDAFQ)    DO WE WANT TO SKIP THIS?         
         DC    C'F'                           ITEM DESCRIPTION TYPE             
         DC    CL3' '                         CHARACTERISTIC CODE               
         DC    CL2' '                         AGENCY QUALIFIER CODE             
         DC    CL12' '                        PRODUCT DESCRIPTION CODE          
         DC    AL1(ESC),AL1(80),AL2(JOBNAMQ)  JOB DESCRIPTION                   
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TDS'                                                           
         DC    C'081'                                                           
         DC    C'2'                NUMBER OF DECIMAL PLACES                     
         DC    AL1(ESC),AL1(15),AL2(BILTOTQ)  TOTAL INVOICE AMOUNT              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CTT'                                                           
         DC    C'089'                                                           
         DC    C'0'                            NUMBER OF DP                     
         DC    CL6'000002'                     NUMBER OF LINE ITEMS             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                       END OF TABLE                      
         EJECT                                                                  
***********************************************************************         
* DDB NEEDHAM - OLD                                                  **         
***********************************************************************         
*                                                                               
         DS    0D                                                               
NEOLD    DS    0CL(AGYLNQ)                    AGENCY DATA                       
         DC    A(0)                           A(ACCLAST HOOK)                   
         DC    4A(0)                          N/D                               
         DC    CL(L'CONTACT)'GINNY ZOOK'      NAME                              
         DC    CL(L'PHONE)'3146328440'        PHONE                             
         DC    CL(L'VENDOR)'466883'           VENDOR NUMBER                     
         DC    AL1(NEEDREP+WKCDDET+SHOVENNO+CHKREVUS)                           
         DC    C'ON'                          USER CODE FOR PO                  
*                                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                      =C'DDS'                           
         DC    CL15'AB-BILLING 3040'          PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BIG'                                                           
         DC    C'002'                                                           
         DC    AL1(ESC),AL1(6),AL2(BILDTQ)    INVOICE DATE                      
         DS    0CL22                          BILL NUMBER                       
         DC    AL1(ESC),AL1(1),AL2(JOBCDQ)    JOB CODE                          
         DC    AL1(ESC),AL1(6),AL2(BILNOQ)    BILL NUMBER                       
         DC    CL15' '                                                          
         DC    CL6' '                         PURCHASE ORDER DATE               
         DC    AL1(ESC),AL1(22),AL2(PONUMQ)   PO NUMBER                         
         DC    CL30' '                        RELEASE NUMBER                    
         DC    CL8' '                         CHANGE ORDER SEQ. NUMBER          
         DC    CL2' '                         TRANSACTION TYPE CODE             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'007'                                                           
         DC    CL2'VN'                        ENTITY IDENTIFIER                 
         DC    AL1(ESC),AL1(33),AL2(CPYNMQ)   COMPANY NAME                      
         DC    CL2' '                                                           
         DC    CL2'92'                        ID. CODE QUALIFIER                
         DC    CL17'0000466883'               ID. CODE                          
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'007'                                                           
         DC    CL2'BT'                        ENTITY IDENTIFIER                 
         DC    AL1(ESC),AL1(35),AL2(CLNMQ)    CLIENT NAME                       
         DC    CL2' '                         ID. CODE QUALIFIER                
         DC    CL17' '                        ID. CODE                          
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'011'                                                           
         DC    CL6'2U'                        REF. NUMBER QUALIFIER             
         DC    CL22'250'                      REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'011'                                                           
         DC    CL6'EO'                        REF. NUMBER QUALIFIER             
         DC    CL22'BMN1'                     REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                         IMDB#2172271                      
         DC    C'011'                                                           
         DC    CL6'IO'                        REF. NUMBER QUALIFIER             
         DC    AL1(ESC),AL1(22),AL2(REFNUMQ)  REFERENCE NUMBER                  
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'011'                                                           
         DC    CL6'QQ'                        REF. NUMBER QUALIFIER             
         DC    CL22'AD1'                      REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PER'                                                           
         DC    C'012'                                                           
         DC    CL2'BI'                        CONTACT FUNCTION CODE             
         DC    AL1(ESC),AL1(35),AL2(CONTACTQ) CONTACT                           
         DC    CL2'TE'                        COMMUNICATION QUALIFIER           
         DC    AL1(ESC),AL1(80),AL2(PHONEQ)   COMMUNICATION NUMBER              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'027'                                                           
         DC    CL11'10'                       ASSIGNED IDENTIFICATION           
         DC    C'2'                           NUMBER OF DECIMAL PLACES          
         DC    AL1(ESC),AL1(10),AL2(BILTOTQ)  QUANTITY INVOICED (TOTAL)         
         DC    CL2'DO'                        UNIT OF BASIS                     
         DC    C'2'                           NUMBER OF DP                      
         DC    AL1(ESC),AL1(14),AL2(BILTOTQ)  UNIT PRICE (TOTAL)                
         DC    CL2'  '                        BASIS OF UNIT PRICE CODE          
         DC    CL2'BP'                        PRODUCT ID QUALIFIER              
         DC    CL30'1006084'                  PRODUCT/SERVICE ID                
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'035'                                                           
         DC    C'F'                           ITEM DESCRIPTION TYPE             
         DC    CL3' '                         CHARACTERISTIC CODE               
         DC    CL2' '                         AGENCY QUALIFIER CODE             
         DC    CL12' '                        PRODUCT DESCRIPTION CODE          
         DC    CL80'FX'                       DESCRIPTION                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TDS'                                                           
         DC    C'063'                                                           
         DC    C'2'                NUMBER OF DECIMAL PLACES                     
         DC    AL1(ESC),AL1(10),AL2(BILTOTQ)  TOTAL INVOICE AMOUNT              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CTT'                                                           
         DC    C'069'                                                           
         DC    C'0'                            NUMBER OF DP                     
         DC    CL6'000001'                     NUMBER OF LINE ITEMS             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                       END OF TABLE                      
         EJECT                                                                  
***********************************************************************         
* DDB NEEDHAM FOR DELL                                               **         
***********************************************************************         
*                                                                               
         DS    0D                                                               
NEDELL   DS    0CL(AGYLNQ)                    AGENCY DATA                       
         DC    A(0)                           A(ACCLAST HOOK)                   
         DC    4A(0)                          N/D                               
         DC    CL(L'CONTACT)'GINNY ZOOK'      NAME                              
         DC    CL(L'PHONE)'3146328440'        PHONE                             
         DC    CL(L'VENDOR)'466883'           VENDOR NUMBER                     
         DC    AL1(NEEDREP+WKCDDET+SHOVENNO+CHKREVUS)                           
         DC    C'ON'                          USER CODE FOR PO                  
*                                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                      =C'DDS'                           
         DC    CL15'DELL-BILLING'             PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BIG'                                                           
         DC    C'002'                                                           
         DC    AL1(ESC),AL1(8),AL2(BILDTCQ)   INVOICE DATE                      
         DC    AL1(ESC),AL1(6),AL2(BILNOQ)    BILL NUMBER                       
         DC    CL16' '                                                          
         DC    AL1(ESC),AL1(8),AL2(BILDTCQ)   INVOICE DATE                      
         DC    C'DO'                                                            
         DC    AL1(ESC),AL1(11),AL2(PONUMQ)   PO NUMBER                         
         DC    CL09' '                        BILL NUMBER                       
         DC    CL30' '                        RELEASE NUMBER                    
         DC    CL8' '                         CHANGE ORDER SEQ. NUMBER          
         DC    CL6' '                         TRAN TYPE/SET/ACTION CODE         
         DC    AL1(ESC),AL1(6),AL2(BILNOQ)    BILL NUMBER                       
         DC    CL16' '                                                          
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CUR'                                                           
         DC    C'004'                                                           
         DC    CL3'ZZ '                       ENTITY IDENTIFIER                 
         DC    CL3'USD'                       CURRENCY CODE                     
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'035'                                                           
         DC    CL20'  '                       ASSIGNED IDENTIFICATION           
         DC    C'2'                           NUMBER OF DECIMAL PLACES          
         DC    AL1(ESC),AL1(10),AL2(BILTOTQ)  QUANTITY INVOICED (TOTAL)         
         DC    CL2'EA'                        UNIT OF BASIS                     
         DC    C'0'                           NUMBER OF DP                      
         DC    CL17'00000000000000001'        UNIT PRICE                        
         DC    CL2'  '                        PRODUCT ID QUALIFIER              
         DC    CL48'  '                       PRODUCT ID                        
         DC    CL2'  '                        PRODUCT ID QUALIFIER              
         DC    CL48' '                        PRODUCT/SERVICE ID                
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CAD'                                                           
         DC    C'055'                                                           
         DC    CL2' '              TRANSPORTATION CODE                          
         DC    CL4' '              EQUIPMENT INITIAL                            
         DC    CL10' '             EQUIPMENT NUMBER                             
         DC    C'NONE'             STANDARD CARRIER ALPHA CODE                  
         DC    CL35' '             ROUTING                                      
         DC    CL2' '              SHIPMENT/ORDER STATUS CODE                   
         DC    C'LI '              REFERENCE IDENTIFICATION QUAILIFIER          
         DC    CL15'1'             REFERENCE IDENTIFICATION                     
         DC    CL15' '                                                          
         DC    CL2' '              SERVICE LEVEL CODE                           
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TDS'                                                           
         DC    C'081'                                                           
         DC    C'2'                NUMBER OF DECIMAL PLACES                     
         DC    AL1(ESC),AL1(15),AL2(BILTOTQ)  TOTAL INVOICE AMOUNT              
         DC    CL2' '                                                           
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CTT'                                                           
         DC    C'089'                                                           
         DC    C'0'                            NUMBER OF DP                     
         DC    CL6'000001'                     NUMBER OF LINE ITEMS             
         DC    CL1' '                                                           
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                       END OF TABLE                      
         EJECT                                                                  
***********************************************************************         
* MINDSHARE (H7) TO CLIENT BPC TRANSMISSION                          **         
* USES ANSI 810 VERSION 3030  (NO DSECT WAS ADDED)                   **         
***********************************************************************         
*                                                                               
         DS    0D                                                               
H7BPC    DS    0CL(AGYLNQ)                    AGENCY DATA                       
         DC    A(H7HOOK)                      A(ACCLAST HOOK)                   
         DC    4A(0)                          N/D                               
         DC    CL(L'CONTACT)'MATT WLODARCZYK' NAME                              
         DC    CL(L'PHONE)'2122978625'        PHONE                             
         DC    CL(L'VENDOR)' '                VENDOR NUMBER                     
         DC    AL1(NEEDREP+WKCDDET)                                             
         DC    C'PK'                          USER CODE FOR PK                  
*                                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                      =C'DDS'                           
         DC    CL15'BP-BILLING'               PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BIG'                                                           
         DC    C'002'                                                           
         DC    AL1(ESC),AL1(6),AL2(BILDTQ)    INVOICE DATE                      
         DC    AL1(ESC),AL1(1),AL2(JOBCDQ)    BILL NUMBER                       
         DC    AL1(ESC),AL1(21),AL2(BILNOQ)   BILL NUMBER                       
         DC    CL6' '                         INVOICE DATE                      
         DC    AL1(ESC),AL1(22),AL2(PONUMQ)   PO NUMBER                         
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'007'                                                           
         DC    CL2'BT'                        ENTITY IDENTIFIER                 
         DC    CL35'BP'                       COMPANY NAME                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N3 '                                                           
         DC    C'009'                                                           
         DC    CL35'P.O.BOX 22024'            ADDRESS 2                         
         DC    CL35' '                        ADDRESS 2                         
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N4 '                                                           
         DC    C'010'                                                           
         DC    CL30'TULSA'                    CITY                              
         DC    CL2'OK'                        STATE                             
         DC    CL9'741212024'                 POSTAL CODE                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PER'                                                           
         DC    C'012'                                                           
         DC    C'SR'                          CONTACT FUNCTION CODE             
         DC    AL1(ESC),AL1(35),AL2(CONTACTQ) CONTACT                           
         DC    C'TE'                          COMMUNICATION QUALIFIER           
         DC    AL1(ESC),AL1(25),AL2(PHONEQ)   COMMUNICATION NUMBER              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'007'                                                           
         DC    CL2'RE'                         ENTITY IDENTIFIER                
         DC    AL1(ESC),AL1(L'H7RMTNAM),AL2(H7RMNMQ)  REMITTER'S NAME           
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N3 '                                                           
         DC    C'009'                                                           
         DC    AL1(ESC),AL1(L'H7RMTAD1),AL2(H7RMAD1Q)  ADDRS 1                  
         DC    CL35' '                         ADDRESS 2                        
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N4 '                                                           
         DC    C'010'                                                           
         DC    AL1(ESC),AL1(30),AL2(H7RMCITQ)  CITY                             
         DC    AL1(ESC),AL1(2),AL2(H7RMSTAQ)   STATE                            
         DC    AL1(ESC),AL1(9),AL2(H7RMZIPQ)   ZIP CODE                         
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PER'                                                           
         DC    C'012'                                                           
         DC    C'SR'                          CONTACT FUNCTION CODE             
         DC    AL1(ESC),AL1(35),AL2(CONTACTQ) CONTACT                           
         DC    C'TE'                          COMMUNICATION QUALIFIER           
         DC    AL1(ESC),AL1(25),AL2(PHONEQ)   COMMUNICATION NUMBER              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'016'                                                           
         DC    C'F'                            ITEM DESCRIPTION TYPE            
         DC    CL3' '                          PRODUCT/PROCESS CODE             
         DC    CL2' '                          AGENCY QUALIFIER CODE            
         DC    CL12' '                         PRODUCT DESCRIPTION CODE         
         DC    AL1(ESC),AL1(80),AL2(IBMPDR)    PROD DESCRIPTION                 
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'026'                                                           
         DC    CL11'00000000001'               ASSIGNED ID                      
         DC    C'0'                            NUMBER OF DP                     
         DC    CL10'0000000001'                QUANTITY INVOICED                
         DC    CL2'EA'                         UNIT OF BASIS                    
         DC    C'2'                            NUMBER OF DP                     
         DC    AL1(ESC),AL1(14),AL2(BILTOTQ)   UNIT PRICE                       
         DC    CL2' '                          BASIS OF UNIT PRICE CODE         
         DC    CL2' '                          PRODUCT ID QUALIFIER             
         DC    CL30' '                         PRODUCT/SERVICE ID               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'033'                                                           
         DC    C'F'                            ITEM DESCRIPTION TYPE            
         DC    CL3' '                          PRODUCT/PROCESS CODE             
         DC    CL2' '                          AGENCY QUALIFIER CODE            
         DC    CL12' '                         PRODUCT DESCRIPTION CODE         
         DC    AL1(ESC),AL1(80),AL2(IBMJDR)    DESCRIPTION                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TDS'                                                           
         DC    C'060'                                                           
         DC    C'2'                          NUMBER OF DECIMAL PLACES           
         DC    AL1(ESC),AL1(10),AL2(BILTOTQ) TOTAL INVOICE AMOUNT               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CTT'                                                           
         DC    C'066'                                                           
         DC    C'0'                            NUMBER OF DP                     
         DC    CL6'000001'                     NUMBER OF LINE ITEMS             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                       END OF TABLE                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
* O&M - OGILVY AND MATHER                                            **         
***********************************************************************         
*                                                                               
         DS    0D                                                               
OA       DS    0CL(AGYLNQ)                    AGENCY DATA                       
         DC    A(0)                           A(ACCLAST HOOK)                   
         DC    4A(0)                          N/D                               
         DC    CL(L'CONTACT)'AMANI ALY'       NAME                              
         DC    CL(L'PHONE)'2122375175'        PHONE                             
         DC    CL(L'VENDOR)' '                VENDOR NUMBER                     
         DC    AL1(NEEDREP)                   REPORT OPTIONS                    
         DC    C'AN'                          USER CODE FOR PO                  
*                                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                                                        
         DC    CL15'OA-BILLING'               PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BIG'                                                           
         DC    C'002'                                                           
         DC    AL1(ESC),AL1(6),AL2(BILDTQ)    INVOICE DATE                      
         DC    AL1(ESC),AL1(22),AL2(BILNOQ)   INVOICE NUMBER                    
         DC    CL6' '                         PO ORDER DATE                     
         DC    AL1(ESC),AL1(22),AL2(PONUMQ)   PO ORDER NUMBER                   
         DC    CL30' '                        RELEASE NUMBER                    
         DC    CL8' '                         CHANGE ORDER SEQ                  
         DC    AL1(ESC),AL1(2),AL2(TRNTYPQ)   TRANSACTION TYPE CODE             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'005'                                                           
         DC    CL6'CR'                        REF. NUMBER QUALIFIER             
         DC    CL22'1000098360'               REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PER'                                                           
         DC    C'006'                                                           
         DC    C'IC'                          CONTACT FUNCTION CODE             
         DC    AL1(ESC),AL1(35),AL2(CONTACTQ) CONTACT                           
         DC    C'TE'                          COMMUNICATION QUALIFIER           
         DC    AL1(ESC),AL1(80),AL2(PHONEQ)   COMMUNICATION NUMBER              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'007'                                                           
         DC    CL2'RE'                        ENTITY IDENTIFIER                 
         DC    AL1(ESC),AL1(35),AL2(CPYNMQ)   COMPANY NAME                      
         DC    CL2' '                         IDENTIFICATION QUALIFIER          
         DC    CL17' '                        IDENTIFICATION CODE               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N3 '                                                           
         DC    C'009'                                                           
         DC    AL1(ESC),AL1(35),AL2(ADDR1Q)   ADDRESS 1                         
         DC    CL35' '                        ADDRESS 2                         
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N4 '                                                           
         DC    C'010'                                                           
         DC    CL30'NEW YORK'                 CITY                              
         DC    CL2'NY'                        STATE                             
         DC    CL9' '                         POSTAL CODE                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'027'                                                           
         DC    AL1(ESC),AL1(11),AL2(IBMPOR)    ASSIGNED ID.                     
         DC    C'0'                            NUMBER OF DP                     
         DC    CL10'0000000001'                QUANTITY INVOICED                
         DC    CL2'EA'                         UNIT OF BASIS                    
         DC    C'2'                            NUMBER OF DP                     
         DC    AL1(ESC),AL1(14),AL2(BILTOTQ)   UNIT PRICE                       
         DC    CL2' '                          BASIS OF UNIT PRICE CODE         
         DC    CL2' '                          PRODUCT ID QUALIFIER             
         DC    CL30' '                         PRODUCT/SERVICE ID               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'035'                                                           
         DC    C'F'                            ITEM DESCRIPTION TYPE            
         DC    CL3' '                          PRODUCT/PROCESS CODE             
         DC    CL2' '                          AGENCY QUALIFIER CODE            
         DC    CL12' '                         PRODUCT DESCRIPTION CODE         
         DC    AL1(ESC),AL1(80),AL2(IBMJDR)    DESCRIPTION                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TDS'                                                           
         DC    C'063'                                                           
         DC    C'2'                            NUMBER OF DP                     
         DC    AL1(ESC),AL1(10),AL2(BILTOTQ)   TOTAL INVOICE                    
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CTT'                                                           
         DC    C'069'                                                           
         DC    C'0'                            NUMBER OF DP                     
         DC    CL6'000001'                     NUMBER OF LINE ITEMS             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                        END OF TABLE                     
         EJECT                                                                  
***********************************************************************         
* O&M - OGILVY AND MATHER - AIG                                      **         
***********************************************************************         
*                                                                               
         DS    0D                                                               
OAAIG    DS    0CL(AGYLNQ)                    AGENCY DATA                       
         DC    A(0)                           A(ACCLAST HOOK)                   
         DC    4A(0)                          N/D                               
         DC    CL(L'CONTACT)'AMANI ALY'       NAME                              
         DC    CL(L'PHONE)'2122375175'        PHONE                             
         DC    CL(L'VENDOR)' '                VENDOR NUMBER                     
         DC    AL1(0)                         REPORT OPTIONS                    
         DC    C'AN'                          USER CODE FOR PO                  
*                                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                                                        
         DC    CL15'OA-BILLING'               PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BIG'                                                           
         DC    C'002'                                                           
         DC    AL1(ESC),AL1(6),AL2(BILDTQ)    INVOICE DATE                      
         DC    AL1(ESC),AL1(22),AL2(BILNOQ)   INVOICE NUMBER                    
         DC    CL6' '                         PO ORDER DATE                     
         DC    CL22' '                        PO ORDER NUMBER                   
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'007'                                                           
         DC    CL2'PE'                        ENTITY IDENTIFIER                 
         DC    AL1(ESC),AL1(35),AL2(CPYNMQ)   COMPANY NAME                      
         DC    CL2' '                         IDENTIFICATION QUALIFIER          
         DC    CL17' '                        IDENTIFICATION CODE               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'031'                                                           
         DC    CL11'1'                        ASSIGNED IDENTIFICATION           
         DC    C' '                           NUMBER OF DECIMAL PLACES          
         DC    CL10'0000000001'               QUANTITY INVOICED                 
         DC    CL2'EA'                        UNIT OF BASIS                     
         DC    C'2'                           NUMBER OF DP                      
         DC    AL1(ESC),AL1(14),AL2(BILTOTQ)  UNIT PRICE (TOTAL)                
         DC    CL2'  '                        BASIS OF UNIT PRICE CODE          
         DC    CL2'ZZ'                        PRODUCT ID QUALIFIER              
         DC    CL30'02'                       PRODUCT/SERVICE ID                
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'046'                                                           
         DC    CL6'DX'                        REF. NUMBER QUALIFIER             
         DC    AL1(ESC),AL1(22),AL2(PONUMQ)   REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'DTM'                                                           
         DC    C'049'                                                           
         DC    CL3'814'                       DATE TIME QUALIFIER               
         DC    AL1(ESC),AL1(6),AL2(DUEDTQ)       DUE DATE                       
         DC    CL8'  '                        TIME                              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TDS'                                                           
         DC    C'072'                                                           
         DC    C'2'                           NUMBER OF DP                      
         DC    AL1(ESC),AL1(10),AL2(BILTOTQ)  TOTAL INVOICE AMOUNT              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CTT'                                                           
         DC    C'079'                                                           
         DC    C'0'                            NUMBER OF DP                     
         DC    CL6'000001'                     NUMBER OF LINE ITEMS             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                        END OF TABLE                     
         EJECT                                                                  
***********************************************************************         
* OMD CANADA                                                          *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
OU       DS    0CL(AGYLNQ)                    AGENCY DATA                       
         DC    A(0)                           A(ACCLAST HOOK)                   
         DC    4A(0)                          N/D                               
         DC    CL(L'CONTACT)'UNKNOWN'         NAME                              
         DC    CL(L'PHONE)'2125551212'        PHONE                             
         DC    CL(L'VENDOR)'60070580'         VENDOR NUMBER                     
         DC    AL1(NEEDREP+WKCDDET+SHOVENNO)                                    
         DC    C'ON'                          USER CODE FOR PO                  
*                                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                      =C'DDS'                           
         DC    CL15'OU-BILLING 4010'          PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BIG'                                                           
         DC    C'002'                                                           
         DC    AL1(ESC),AL1(8),AL2(BILDTCQ)   INVOICE DATE                      
         DC    AL1(ESC),AL1(22),AL2(BILNOQ)   INVOICE NUMBER                    
         DC    CL8' '                                                           
         DC    CL22' '                                                          
         DC    CL30' '                                                          
         DC    CL8' '                                                           
         DC    CL2'DI'                                                          
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'005'                                                           
         DC    CL3'23'                        REF. NUMBER QUALIFIER             
         DC    CL30'CAN'                      REF. NUMBER                       
         DC    CL80' '                        DESCRIPTION                       
         DC    CL3' '                         REF. NUMBER QUALIFIER             
         DC    CL30' '                        REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PER'                                                           
         DC    C'007'                                                           
         DC    C'OC'                          CONTACT FUNCTION CODE             
         DC    AL1(ESC),AL1(60),AL2(UFLD3Q)   ATTN NAME                         
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'008'                                                           
         DC    CL3'ST'                        ENTITY IDENTIFIER                 
         DC    AL1(ESC),AL1(60),AL2(CLNMQ)    CLIENT NAME                       
         DC    CL2'92'                        IDENTIFICATION QUALIFIER          
         DC    AL1(ESC),AL1(80),AL2(UFLD2Q)   IDENTIFICATION CODE               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'008'                                                           
         DC    CL3'VN'                        ENTITY IDENTIFIER                 
         DC    AL1(ESC),AL1(60),AL2(CPYNMQ)   COMPANY NAME                      
         DC    CL2'92'                        IDENTIFICATION QUALIFIER          
         DC    AL1(ESC),AL1(17),AL2(VENCDEQ)  IDENTIFICATION CODE               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'DTM'                                                           
         DC    C'016'                                                           
         DC    CL3'035'                       DATE TIME QUALIFIER               
         DC    AL1(ESC),AL1(8),AL2(BILDTCQ)   INVOICE DATE                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'035'                                                           
         DC    CL20' '                         ASSIGNED ID.                     
         DC    C'0'                                                             
         DC    CL10'0000000001'                QUANTITY INVOICED                
         DC    CL2'EA'                         UNIT OF BASIS                    
         DC    C'2'                                                             
         DC    CL17'00000000000000000'         UNIT PRICE                       
         DC    CL2' '                                                           
         DC    CL2'VP'                         PRODUCT ID QUALIFIER             
         DC    CL48'NOTE'                      PRODUCT/SERVICE ID               
         DC    CL2'BS'                         PRODUCT ID QUALIFIER             
         DC    AL1(ESC),AL1(48),AL2(UFLD1Q)    PRODUCT/SERVICE ID               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'044'                                                           
         DC    C'F'                            ITEM DESCRIPTION TYPE            
         DC    CL3' '                          PRODUCT/PROCESS CODE             
         DC    CL2' '                          AGENCY QUALIFIER CODE            
         DC    CL12' '                         PRODUCT DESCRIPTION CODE         
         DC    AL1(ESC),AL1(80),AL2(MEDNMEQ)   DESCRIPTION                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'035'                                                           
         DC    CL20' '                         ASSIGNED ID.                     
         DC    C'0'                                                             
         DC    CL10'0000000001'                QUANTITY INVOICED                
         DC    CL2'EA'                         UNIT OF BASIS                    
         DC    C'2'                                                             
         DC    C'00'                           1ST 2 CHAR OF UNIT PRICE         
         DC    AL1(BEGQ),AL1(15),AL2(UPRICEQ)  UNIT PRICE                       
         DC    CL2' '                                                           
         DC    CL2'VP'                         PRODUCT ID QUALIFIER             
         DC    AL1(ESC),AL1(48),AL2(MEDNMEQ)                                    
         DC    CL2'BS'                         PRODUCT ID QUALIFIER             
         DC    AL1(ESC),AL1(48),AL2(UFLD1Q)    PRODUCT/SERVICE ID               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'044'                                                           
         DC    C'F'                            ITEM DESCRIPTION TYPE            
         DC    CL3' '                          PRODUCT/PROCESS CODE             
         DC    CL2' '                          AGENCY QUALIFIER CODE            
         DC    CL12' '                         PRODUCT DESCRIPTION CODE         
         DC    AL1(ESC),AL1(80),AL2(BPJQ)      DESCRIPTION                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'050'                                                           
         DC    CL3'P4'                         REF. NUMBER QUALIFIER            
         DC    AL1(ESC),AL1(30),AL2(UFLD4Q)    REF. NUMBER                      
         DC    AL1(EOR)                                                         
         DC    AL1(ENDQ)                       END LOOP                         
*                                                                               
         DC    C'TDS'                                                           
         DC    C'081'                                                           
         DC    C'2'                            NUMBER OF DP                     
         DC    AL1(ESC),AL1(15),AL2(BNTTOTQ)   TOTAL INVOICE (+TAX)             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TXI'                                                           
         DC    C'082'                                                           
         DC    CL2'GS'                                                          
         DC    C'2'                                                             
         DC    AL1(ESC),AL1(18),AL2(GSTQ)      GST AMOUNT                       
         DC    C' '                                                             
         DC    CL10' '                                                          
         DC    CL2'  '                                                          
         DC    CL10' '                                                          
         DC    C' '                                                             
         DC    C' '                                                             
         DC    C' '                                                             
         DC    CL9' '                                                           
         DC    CL20' '                                                          
         DC    CL20' '                                                          
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TXI'                                                           
         DC    C'082'                                                           
         DC    C'SP'                           TAX TYPE CODE                    
         DC    C'2'                                                             
         DC    AL1(ESC),AL1(18),AL2(QSTQ)      QST AMOUNT                       
         DC    C' '                                                             
         DC    CL10' '                                                          
         DC    CL2'CD'                         TAX JUR. CODE QUAL               
         DC    CL10'QC'                        TAX JURISDICTION CODE            
         DC    C' '                                                             
         DC    C' '                                                             
         DC    C' '                                                             
         DC    CL9' '                                                           
         DC    CL20' '                                                          
         DC    CL20' '                                                          
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CTT'                                                           
         DC    C'089'                                                           
         DC    C'0'                            NUMBER OF DP                     
*        DC    CL6'000002'                     NUMBER OF LINE ITEMS             
         DC    AL1(ESC),AL1(6),AL2(IT1CNTQ)    NUMBER OF LINE ITEMS             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                        END OF TABLE                     
         EJECT                                                                  
***********************************************************************         
* MINDSHARE TO SHELL                                                  *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
SHELL    DS    0CL(AGYLNQ)                    AGENCY DATA                       
         DC    A(0)                           A(ACCLAST HOOK)                   
         DC    4A(0)                          N/D                               
         DC    CL(L'CONTACT)'UNKNOWN'         NAME                              
         DC    CL(L'PHONE)'9999999999'        PHONE                             
         DC    CL(L'VENDOR)'        '         VENDOR NUMBER                     
         DC    AL1(NEEDREP+WKCDDET)                                             
         DC    C'ON'                          USER CODE FOR PO                  
*                                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                      =C'DDS'                           
         DC    CL15'M2-BILLING 4010'          PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BIG'                                                           
         DC    C'002'                                                           
         DC    AL1(ESC),AL1(8),AL2(BILDTCQ)   INVOICE DATE                      
         DC    AL1(ESC),AL1(22),AL2(BILNOQ)   INVOICE NUMBER                    
         DC    CL8' '                                                           
         DC    CL22' '                                                          
         DC    CL30' '                                                          
         DC    CL8' '                                                           
         DC    AL1(ESC),AL1(2),AL2(TRNTYPQ)   TRANSACTION TYPE CODE             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CUR'                                                           
         DC    C'004'                                                           
         DC    CL3'ZZ '                       ENTITY IDENTIFIER                 
         DC    CL3'USD'                       CURRENCY CODE                     
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'005'                                                           
         DC    CL3'8N'                        REF. NUMBER QUALIFIER             
         DC    AL1(ESC),AL1(4),AL2(UFLD1Q)    REF ID                            
         DC    CL26' '                                                          
         DC    CL80' '                        DESCRIPTION                       
         DC    CL3' '                         REF. NUMBER QUALIFIER             
         DC    CL30' '                        REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'005'                                                           
         DC    CL3'74'                        REF. NUMBER QUALIFIER             
         DC    AL1(ESC),AL1(30),AL2(UFLD3Q)   REF ID                            
         DC    CL80' '                        DESCRIPTION                       
         DC    CL3' '                         REF. NUMBER QUALIFIER             
         DC    CL30' '                        REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'005'                                                           
         DC    CL3'CA'                        REF. NUMBER QUALIFIER             
         DC    AL1(ESC),AL1(30),AL2(UFLD1Q)   REF ID                            
         DC    CL80' '                        DESCRIPTION                       
         DC    CL3' '                         REF. NUMBER QUALIFIER             
         DC    CL30' '                        REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'REF'                                                           
         DC    C'005'                                                           
         DC    CL3'GZ'                        REF. NUMBER QUALIFIER             
         DC    AL1(ESC),AL1(30),AL2(UFLD2Q)   REF ID                            
         DC    CL80' '                        DESCRIPTION                       
         DC    CL3' '                         REF. NUMBER QUALIFIER             
         DC    CL30' '                        REF. NUMBER                       
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'008'                                                           
         DC    CL3'VN'                        ENTITY IDENTIFIER                 
         DC    AL1(ESC),AL1(60),AL2(CPYNMQ)   COMPANY NAME                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N1 '                                                           
         DC    C'008'                                                           
         DC    CL3'BT'                        ENTITY IDENTIFIER                 
         DC    CL60'SHELL PRODUCTS US - LUBES' CLIENT NAME                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N3 '                                                           
         DC    C'010'                                                           
         DC    CL55'P.O.BOX 4484'             IDENTIFICATION CODE               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'N4 '                                                           
         DC    C'011'                                                           
         DC    CL30'HOUSTON'                  CITY                              
         DC    CL2'TX'                        STATE                             
         DC    CL15'77210-4484'               POSTAL CODE                       
         DC    CL3'US'                        COUNTRY CODE                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'DTM'                                                           
         DC    C'016'                                                           
         DC    CL3'011'                       DATE TIME QUALIFIER               
         DC    AL1(ESC),AL1(8),AL2(BILDTCQ)   INVOICE DATE                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'IT1'                                                           
         DC    C'035'                                                           
         DC    CL20'1'                         ASSIGNED ID.                     
         DC    C'0'                                                             
         DC    CL10'0000000001'                QUANTITY INVOICED                
         DC    CL2'EA'                         UNIT OF BASIS                    
         DC    C'2'                                                             
         DC    C'00'                                                            
         DC    AL1(ESC),AL1(15),AL2(BILTOTSH) UNIT PRICE (TOTAL)                
         DC    CL2' '                                                           
         DC    CL2'CB'                         PRODUCT ID QUALIFIER             
SHIT107  DC    CL48' '                                                          
         ORG   SHIT107                                                          
         DC    AL1(ESC),AL1(L'PRDCD),AL2(PRDCDQ)                                
         DC    C' '                                                             
         DC    AL1(ESC),AL1(L'PRDNAM),AL2(PRDNAMQ)                              
         ORG                                                                    
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'PID'                                                           
         DC    C'044'                                                           
         DC    C'F'                            ITEM DESCRIPTION TYPE            
         DC    CL3' '                          PRODUCT/PROCESS CODE             
         DC    CL2' '                          AGENCY QUALIFIER CODE            
         DC    CL12' '                         PRODUCT DESCRIPTION CODE         
         DC    AL1(ESC),AL1(80),AL2(MEDNMEQ)   DESCRIPTION                      
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TDS'                                                           
         DC    C'081'                                                           
         DC    C'2'                            NUMBER OF DP                     
         DC    AL1(ESC),AL1(15),AL2(BILTOTSH)  TOTAL INVOICE (+TAX)             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'AMT'                                                           
         DC    C'084'                                                           
         DC    CL3'001'                        AMOUNT QUALIFIER CODE            
         DC    C'2'                            NUMBER OF DP                     
         DC    C'000'                                                           
         DC    AL1(ESC),AL1(15),AL2(BILTOTSH)  TOTAL INVOICE (+TAX)             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'CTT'                                                           
         DC    C'089'                                                           
         DC    C'0'                            NUMBER OF DP                     
         DC    CL6'000001'                     NUMBER OF LINE ITEMS             
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                        END OF TABLE                     
         EJECT                                                                  
***********************************************************************         
* BX IO AREA                                                                    
***********************************************************************         
*                                                                               
         DS    0D                                                               
BXIO     DS    XL2000              IO AREA                                      
IO2      DS    XL2000              IO AREA                                      
*                                                                               
*********************************************************************           
* CONVERT NEGATIVE NUMBERS                                          *           
*********************************************************************           
FIXNEG   NTR1  BASE=*,LABEL=*                                                   
         LA    RF,NEGTAB                                                        
         LA    R0,10                                                            
FIXNEG3  CLC   0(1,R3),0(RF)       MATCH NUMBER TO TABLE                        
         BE    FIXNEG5                                                          
         LA    RF,2(RF)                                                         
         BCT   R0,FIXNEG3                                                       
         OI    0(R3),X'F0'                                                      
         J     EXIT                                                             
FIXNEG5  MVC   0(1,R3),1(RF)       REPLACE NEGATIVES                            
         J     EXIT                                                             
*                                                                               
NEGTAB   DC   X'D097'              MINUS ZERO IS A LITTLE 'P'                   
         DC   X'D198'                                                           
         DC   X'D299'                                                           
         DC   X'D3A2'                                                           
         DC   X'D4A3'                                                           
         DC   X'D5A4'                                                           
         DC   X'D6A5'                                                           
         DC   X'D7A6'                                                           
         DC   X'D8A7'                                                           
         DC   X'D9A8'                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
* SPECIAL ROUTINE FOR MINSHARE TO CLIENT BP H7HOOK                  *           
*********************************************************************           
H7HOOK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   CLTCD,=C'BPC'                                                    
         BNE   H7HK10                                                           
         MVC   H7RMTNAM,=CL35'MINDSHARE USA LLC'                                
         MVC   H7RMTAD1,=CL35'P O BOX 601689'                                   
         MVC   H7RMTCIT,=CL30'CHARLOTTE'                                        
         MVC   H7RMTSTA,=CL2'NC'                                                
         MVC   H7RMTZIP,=CL9'282601689'                                         
         B     H7HKX                                                            
H7HK10   DS    0H                                                               
         CLC   CLTCD,=C'BPE'                                                    
         BNE   H7HKX                                                            
         MVC   H7RMTNAM,=CL35'MONE WORLDWIDE'                                   
         MVC   H7RMTAD1,=CL35'P O BOX 601611'                                   
         MVC   H7RMTCIT,=CL30'CHARLOTTE'                                        
         MVC   H7RMTSTA,=CL2'NC'                                                
         MVC   H7RMTZIP,=CL9'282601611'                                         
H7HKX    J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PROGRAM WORK AREA                                                   *         
***********************************************************************         
*                                                                               
ACBXD    DSECT                                                                  
ELCODE   DS    X                                                                
ELEM     DS    XL256                                                            
*                                                                               
ERRFLG   DS    X              ERROR NUMBER MOVED IN BEFORE ACTION TAKEN         
*                                                                               
PRTFLG   DS    X              FLAG TO TELL WHEN TO PRNT LEVEL INFO              
PRDDIFF  EQU   X'01'                                                            
CLTLAST  EQU   X'02'                                                            
PRDLAST  EQU   X'04'                                                            
OPENFIL  EQU   X'08'                                                            
FRSTBIL  EQU   X'10'                                                            
*                                                                               
STDAT2   DS    XL2            STARTING RUN DATE NTRD BY USER (QSTART)           
STDAT8   DS    CL8            PRINTABLE STARTING RUN DATE                       
ENDAT2   DS    XL2            ENDING RUN DATE NTRD BY USER (QSTART)             
ENDAT8   DS    CL8            PRINTABLE ENDING RUN DATE                         
*                                                                               
CLTADD   DS    CL(5*L'ADRADD1)     CLIENT ADDRESS                               
*                                                                               
WRKCD    DS    CL2                 WORK CODE                                    
WRKDSC   DS    CL15                WORK DESCRIPTION                             
WRKPTR   DS    A                   1ST ENTRY WITH CURRENT WRK CODE              
*                                                                               
CURBILL  DS    A                   A(CURRENT TABLE ENTRY)                       
         DS    0F                                                               
ADLIST   DS    0CL12                                                            
AGYALPH  DS    CL2                 AGENCY ALPHA                                 
         DS    XL2                 N/D                                          
AGYCLI   DS    CL3                 CLIENT CODE                                  
AGYFILT  DS    CL1                 FILTER 5 VALUE                               
ADAGY    DS    A                   A(AGENCY CONSTANT DATA)                      
*                                                                               
RECLEN   DS    H                   RECORD LENGTH                                
RECNXT   DS    F                   A(NEXT OUTPUT BYTE)                          
RTNADR   DS    F                   RETURN ADDRESS FOR LOOP                      
RTNROU   DS    C                   RETURN TO ROUTINE 'Y' OR 'N'                 
SKIPREC  DS    CL1                 DO WE WANT TO SKIP THIS SEGMENT              
LOOPFLG  DS    X                   FLAG TO INDICATE LOOP HAS STARTED            
LPSTRT   EQU   X'80'                                                            
LPSKIP   EQU   X'40'                                                            
*                                                                               
SVBILLNO DS    CL(L'BXBILNO)                                                    
*                                                                               
CBILNO   DS    CL(L'PTARBLNO)                                                   
PBILNO   DS    CL(L'PTARBLNO)                                                   
*                                                                               
TBLPNTR  DS    A                   POINTER TO TABLE                             
TBLENT   DS    H                   ACTUAL NUMBER OF TABLE ENTRIES               
MXENTRY  EQU   150                 MAX TABLE ENTRIES                            
*                                                                               
*                                  FOR THE RECEIVABLE RECORD                    
RCKEY    DS    CL(L'TRNKEY)                                                     
*                                                                               
RCBLTAMT DS    PL8                 TRANSACTION AMOUNT                           
RCBLAMT  DS    PL8                 TOTAL OF TRANSACTION AMOUNTS                 
RCOFF    DS    CL(L'TRNOFFC)       OFFICE                                       
RCMOS    DS    CL(L'TRNMOS)        TRANSACTION MONTH OF SERVICE                 
RCBREF   DS    CL(L'TRNBREF)       BATCH REFERENCE                              
RCACTDTE DS    CL(L'TRSDATE)       ACTIVITY/RUN DATE                            
                                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGE LINE - USED IN STMPRC ROUTINE                         *         
***********************************************************************         
ERRMSGD  DSECT                                                                  
ERRMSG   DS    CL35                                                             
         DS    CL1                                                              
ERRCODE  DS    CL2                                                              
         DS    CL1                                                              
ERRULA   DS    CL(L'TRNKULA)                                                    
         DS    CL1                                                              
ERRWORK  DS    CL(L'TRNKWORK)                                                   
         DS    CL1                                                              
ERRULC   DS    CL(L'TRNKULC)                                                    
         DS    CL1                                                              
ERRDATE  DS    CL10                                                             
         DS    CL1                                                              
ERRREF   DS    CL(L'TRNKREF)                                                    
         DS    CL1                                                              
ERRAMT   DS    CL12                                                             
*                                                                               
***********************************************************************         
* DSECT FOR TRANSACTION TABLE                                         *         
***********************************************************************         
*                                                                               
BXTABLED DSECT                     TRANSACTION TABLE DSECT                      
BXBILNO  DS    CL6                 BILL NUMBER                                  
BXBLDTE  DS    CL2                 BILL DATE                                    
BXWRKCD  DS    CL2                 WORK CODE                                    
BXWRKDSC DS    CL15                WORK CODE DESCRIPTION                        
BXACTDTE DS    CL2                 ACTIVITY DATE                                
BXPBILNO DS    CL6                 PREVIOUS BILL NUMBER                         
BXDUEDTE DS    CL2                 DUE DATE                                     
BXAMOUNT DS    PL8                 AMOUNT RECIEVABLE                            
BXGST    DS    PL6                 GST AMOUNT                                   
BXQST    DS    PL6                 QST AMOUNT                                   
BXTABLNQ EQU   *-BXTABLED          LENGTH OF EACH ENTRY                         
         EJECT                                                                  
***********************************************************************         
* DSECT FOR DETAIL PRINT LINE                                         *         
***********************************************************************         
*                                                                               
PRND     DSECT                                                                  
         DS    CL8                                                              
PRNJOB   DS    CL6                 JOB CODE                                     
         DS    CL8                                                              
PRNBILL  DS    CL6                 BILL NUMBER                                  
         DS    CL4                                                              
PRNDTE   DS    CL8                 BILL DATE                                    
         DS    CL8                                                              
PRNWKC   DS    CL2                 WORKCODE                                     
         DS    CL5                                                              
PRNDSC   DS    CL15                DESCRIPTION                                  
         DS    CL2                                                              
PRNQTY   DS    PL16                QUANTITY                                     
         DS    CL2                                                              
PRNUNT   DS    CL2                 UNITS                                        
         DS    CL10                                                             
PRNTOT   DS    PL16                TOTAL                                        
*                                                                               
         ORG   PRND                DEFINE TOTAL LINES                           
         DS    CL28                                                             
PRNTXT   DS    CL9                 TEXT                                         
         DS    CL2                                                              
PRNLEV   DS    CL5                 LEVEL                                        
         DS    CL1                                                              
PRNCODE  DS    CL6                 CODE                                         
         DS    CL2                                                              
PRNNAME  DS    CL36                NAME                                         
         EJECT                                                                  
***********************************************************************         
* DSECT FOR EDI HEADER LINE                                           *         
***********************************************************************         
*                                                                               
EDIHDRD  DSECT                                                                  
EDILN    DS    XL2                 LENGTH                                       
         DS    XL2                                                              
EDISET   DS    CL3                 SET (810)                                    
EDISPA   DS    CL3                 SPACES                                       
EDISEG   DS    CL3                 SEGEMENT                                     
EDISEQ   DS    CL3                 SEQUENCE                                     
EDIZRO   DS    CL5                 SPARE (00000)                                
EDILNQ   EQU   *-EDILN                                                          
EDIDATA  DS    0C                                                               
*                                                                               
         ORG   EDILN                                                            
EDIHDLN  DS    XL2                 RECORD LENGTH                                
         DS    XL2                 NULLS                                        
EDIHDSID DS    XL6                 RECORD LENGTH                                
EDIHDZ   DS    XL11                MUST BE CHARACTER ZEROS                      
EDIHDDN  DS    CL22                DOCUMENT NAME                                
EDIHDGID DS    CL2                 FUNCTIONAL GROUP ID                          
EDIHDDDS DS    CL15                =C'DDS'                                      
EDIHDPID DS    CL15                PARTNER'S PROFILE ID (EXPORT)                
*                                  PARTNER'S APPLICATION CODE (IMPORT)          
EDIHDRQ  EQU   *-EDIHDRD                                                        
*                                                                               
*---------------------------------------------------------------------          
*   DSECT FOR EDI 3040 RECORDS                                                  
*---------------------------------------------------------------------          
*          ------------ HEAD LINE -----------                                   
EDI3040D DSECT                                                                  
EDI34LN  DS    XL2                 RECORD LENGTH                                
         DS    XL2                 NULLS                                        
EDI34SET DS    CL6                 SET                                          
EDI34SEG DS    CL3                 SEGMENT                                      
EDI34SEQ DS    CL3                 SEQUENCE NUMBER                              
EDI34SPR DS    CL5                 SPARE                                        
EDI34CON DS    0C                                                               
*          ------------ BIG-002 -------------                                   
         ORG   EDI34CON                                                         
E34BGDT  DS    CL6                 INVOICE DATE                                 
E34BGNO  DS    CL22                INVOICE NUMBER                               
E34BGPOD DS    CL6                 PURCHASE ORDER DATE                          
E34BGPON DS    CL22                PURCHASE ORDER NUMBER                        
E34BGREL DS    CL30                RELEASE NUMBER                               
E34BGCOS DS    CL8                 CHANGE ORDER SEQUENCE NUMBER                 
E34BGTTC DS    CL2                 TRANSACTION TYPE CODE                        
EDI34BGQ EQU   *-EDI3040D                                                       
*           ----------- N1 -007 -------------                                   
         ORG   EDI34CON                                                         
E34N1EIC DS    CL2                 ENTITY IDENTIFIER CODE                       
E34N1NM  DS    CL35                NAME                                         
E34N1ICQ DS    CL2                 IDENTIFICATION CODE QUALIFIER                
E34N1IDC DS    CL17                IDENTIFICATION CODE                          
EDI34N1Q EQU   *-EDI3040D                                                       
*          ------------ N3 -009 -------------                                   
         ORG   EDI34CON                                                         
E34N3AD1 DS    CL35                ADDRESS INFORMATION                          
E34N3AD2 DS    CL35                ADDRESS INFORMATION                          
EDI34N3Q EQU   *-EDI3040D                                                       
*          ------------ N4 -010 --------------                                  
         ORG   EDI34CON                                                         
E34N4CTY DS    CL30                CITY NAME                                    
E34N4SOP DS    CL2                 STATE OR PROVINCE CODE                       
E34N4PSC DS    CL9                 POSTAL CODE                                  
EDI34N4Q EQU   *-EDI3040D                                                       
*          ------------ REF-011 --------------                                  
         ORG   EDI34CON                                                         
E34RFNOQ DS    CL6                 REFERECE NUMBER QUALIFIER                    
E34RFNO  DS    CL22                REFERENCE NUMBER                             
EDI34RFQ EQU   *-EDI3040D                                                       
*         ------------- PER-012 --------------                                  
         ORG   EDI34CON                                                         
E34PRCFC DS    CL2                 CONTACT FUNCTION CODE                        
E34PRNM  DS    CL35                NAME                                         
E34PRCMQ DS    CL2                 COMMUNICATION QUALIFIER                      
E34PRCMN DS    CL80                COMMUNICATION NUMBER                         
EDI34PRQ EQU   *-EDI3040D                                                       
*         ------------- N9 -025 --------------                                  
         ORG   EDI34CON                                                         
E34N9RNQ DS    CL2                 REFERENCE NUMBER QUALIFIER                   
E34N9RFN DS    CL30                REFERENCE NUMBER                             
E34N9FFD DS    CL36                FREE FORM DESCRIPTION                        
EDI34N9Q EQU   *-EDI3040D                                                       
*             --------- MSG-026 -------------                                   
         ORG   EDI34CON                                                         
E34MGPOB DS    CL50                PRINT ON BILLS                               
EDI34MGQ EQU   *-EDI3040D                                                       
*             --------- IT1-027 -------------                                   
         ORG   EDI34CON                                                         
E34I1AID DS    CL11                ASSIGNED IDENTIFICATION                      
E34I1ND1 DS    CL1                 NUMBER OF DECIMAL PLACES                     
E34I1QIN DS    CL10                QUANTITY INVOICED                            
E34I1UNT DS    CL2                 UNIT OF BASIS FOR MEASUREMENT CODE           
E34I1ND2 DS    CL1                 NUMBER OF DECIMAL PLACES                     
E34I1UPR DS    CL14                UNIT PRICE                                   
E34I1BUC DS    CL2                 BASIS OF UNIT PRICE CODE                     
E34I1PQ1 DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
E34I1PD1 DS    CL30                PRODUCT/SERVICE ID                           
EDI34I1Q  EQU   *-EDI3040D                                                      
*            ---------- PID-035 --------------                                  
         ORG   EDI34CON                                                         
E34PDDTY DS    CL1                 ITEM DESCRIPTION TYPE                        
E34PDPCC DS    CL3                 PRODUCT/PROCESS CHARACTERISTIC CODE          
E34PDAQC DS    CL2                 AGENCY QUALIFIER CODE                        
E34PDPDC DS    CL12                PRODUCT DESCRIPTION CODE                     
E34PDDSC DS    CL80                DESCRIPTION                                  
EDI34PDQ EQU   *-EDI3040D                                                       
*            ---------- DTM-044 -------------                                   
         ORG   EDI34CON                                                         
E34DMDTQ DS    CL3                 DATE/TIME QUALIFIER                          
E34DMDT  DS    CL6                 DATE                                         
E34DMTM  DS    CL8                 TIME                                         
EDI34DMQ EQU   *-EDI3040D                                                       
*           ----------- TDS-063 --------------                                  
         ORG   EDI34CON                                                         
E34TSND  DS    CL1                 NUMBER OF DECIMAL PLACES                     
E34TSTIA DS    CL10                TOTAL INVOICE AMOUNT                         
EDI34TSQ EQU   *-EDI3040D                                                       
*            ---------- CTT-069 ---------------                                 
         ORG   EDI34CON                                                         
E34CTND  DS    CL1                 NUMBER OF DECIMAL PLACES                     
E34CTNL  DS    CL6                 NUMBER OF LINE ITEMS                         
EDI34CTQ EQU   *-EDI3040D                                                       
*            ----------------------------------                                 
          EJECT                                                                 
*---------------------------------------------------------------------          
*   DSECT FOR EDI 3050 RECORDS                                                  
*---------------------------------------------------------------------          
*          ------------ HEAD LINE -----------                                   
EDI3050D DSECT                                                                  
EDI35LN  DS    XL2                 RECORD LENGTH                                
         DS    XL2                 NULLS                                        
EDI35SET DS    CL6                 SET                                          
EDI35SEG DS    CL3                 SEGMENT                                      
EDI35SEQ DS    CL3                 SEQUENCE NUMBER                              
EDI35SPR DS    CL5                 SPARE                                        
EDI35CON DS    0C                                                               
*          ------------ BIG ----------------                                    
         ORG   EDI35CON                                                         
E35BGDT  DS    CL6                 INVOICE DATE                                 
E35BGNO  DS    CL22                INVOICE NUMBER                               
E35BGPOD DS    CL6                 PURCHASE ORDER DATE                          
E35BGPON DS    CL22                PURCHASE ORDER NUMBER                        
EDI35BGQ EQU   *-EDI3050D                                                       
*          ------------ N1 -007 --------------                                  
         ORG   EDI35CON                                                         
E35N1EIC DS    CL2                 ENTITY IDENTIFIER CODE                       
E35N1NM  DS    CL35                NAME                                         
E35N1ICQ DS    CL2                 IDENTIFICATION CODE QUALIFIER                
E35N1IDC DS    CL17                IDENTIFICATION CODE                          
EDI35N1Q EQU   *-EDI3050D                                                       
*           ----------- REF-011 ---------------                                 
         ORG   EDI35CON                                                         
E35RFNOQ DS    CL6                 REFERECE NUMBER QUALIFIER                    
E35RFNO  DS    CL22                REFERENCE NUMBER                             
EDI35RFQ EQU   *-EDI3050D                                                       
*           ----------- PER-012 -------------                                   
         ORG   EDI35CON                                                         
E35PRCFC DS    CL2                 CONTACT FUNCTION CODE                        
E35PRNM  DS    CL35                NAME                                         
E35PRCMQ DS    CL2                 COMMUNICATION QUALIFIER                      
E35PRCMN DS    CL80                COMMUNICATION NUMBER                         
EDI35PRQ EQU   *-EDI3050D                                                       
*            ---------- N9 -025 ------------                                    
         ORG   EDI35CON                                                         
E35N9RNQ DS    CL2                 REFERENCE NUMBER QUALIFIER                   
E35N9RFN DS    CL30                REFERENCE NUMBER                             
E35N9FFD DS    CL36                FREE FORM DESCRIPTION                        
EDI35N9Q EQU   *-EDI3050D                                                       
*          ------------ MSG-027--------------                                   
         ORG   EDI35CON                                                         
E35MGPOB DS    CL50                PRINT ON BILLS                               
EDI35MGQ EQU   *-EDI3050D                                                       
*           ----------- IT1-031 -------------                                   
         ORG   EDI35CON                                                         
E35I1AID DS    CL11                ASSIGNED IDENTIFICATION                      
E35I1ND1 DS    CL1                 NUMBER OF DECIMAL PLACES                     
E35I1QIN DS    CL10                QUANTITY INVOICED                            
E35I1UNT DS    CL2                 UNIT OF BASIS FOR MEASUREMENT CODE           
E35I1ND2 DS    CL1                 NUMBER OF DECIMAL PLACES                     
E35I1UPR DS    CL14                UNIT PRICE                                   
E35I1BUC DS    CL2                 BASIS OF UNIT PRICE CODE                     
E35I1PQ1 DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
E35I1PD1 DS    CL30                PRODUCT/SERVICE ID                           
EDI35I1Q  EQU   *-EDI3050D                                                      
*           ----------- PID-040 ---------------                                 
         ORG   EDI35CON                                                         
E35PDDTY DS    CL1                 ITEM DESCRIPTION TYPE                        
E35PDPCC DS    CL3                 PRODUCT/PROCESS CHARACTERISTIC CODE          
E35PDAQC DS    CL2                 AGENCY QUALIFIER CODE                        
E35PDPDC DS    CL12                PRODUCT DESCRIPTION CODE                     
E35PDDSC DS    CL80                DESCRIPTION                                  
EDI35PDQ EQU   *-EDI3050D                                                       
*           ----------- DTM-049 ----------------                                
         ORG   EDI35CON                                                         
E35DMDTQ DS    CL3                 DATE/TIME QUALIFIER                          
E35DMDT  DS    CL6                 DATE                                         
E35DMTM  DS    CL8                 TIME                                         
EDI35DMQ EQU   *-EDI3050D                                                       
*            ---------- TDS-072 -------------                                   
         ORG   EDI35CON                                                         
E35TSND  DS    CL1                 NUMBER OF DECIMAL PLACES                     
E35TSTIA DS    CL10                TOTAL INVOICE AMOUNT                         
EDI35TSQ EQU   *-EDI3050D                                                       
*             --------- CTT-079 -------------                                   
         ORG   EDI35CON                                                         
E35CTND  DS    CL1                 NUMBER OF DECIMAL PLACES                     
E35CTNL  DS    CL6                 NUMBER OF LINE ITEMS                         
EDI35CTQ EQU   *-EDI3050D                                                       
***********************************************************************         
*                                                                               
* ACGENFILE                                                                     
         PRINT   OFF                                                            
       ++INCLUDE ACGENFILE                                                      
         PRINT   ON                                                             
* ACGENMODES                                                                    
         PRINT   OFF                                                            
       ++INCLUDE ACGENMODES                                                     
         PRINT   ON                                                             
* ACMASTD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE ACMASTD                                                        
         PRINT   ON                                                             
* ACREPWORKD                                                                    
         PRINT   OFF                                                            
       ++INCLUDE ACREPWORKD                                                     
         PRINT   ON                                                             
* CTGENFILE                                                                     
         PRINT   OFF                                                            
       ++INCLUDE CTGENFILE                                                      
         PRINT   ON                                                             
* DDMASTD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE DDMASTD                                                        
         PRINT   ON                                                             
*                                                                               
         PRINT   OFF                                                            
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'116ACREPBX02 11/04/11'                                      
         END                                                                    
