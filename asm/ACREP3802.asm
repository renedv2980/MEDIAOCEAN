*          DATA SET ACREP3802  AT LEVEL 004 AS OF 04/15/16                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 046700.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE AC3802A                                                                  
*INCLUDE ACCEDIT                                                                
*INCLUDE ACLIST                                                                 
*INCLUDE CHOPCON                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'CONTRA-ACC''T STATEMENT PRINTING'                               
*----------------------------------------------------------------               
*              AC38 CONTRA ACCOUNT REPORT                                       
*----------------------------------------------------------------               
*                                                                               
AC3802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC3802,R6       R6=2ND BASE REGISTER                         
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         L     RC,=A(WORKC)                                                     
         USING WORKD,RC            RC=A(SAVE W/S)                               
         LA    R9,MYEND                                                         
         USING WORK1D,R9                                                        
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   MODE,PROCTRNS                                                    
         BE    A38TRNS                                                          
         CLI   MODE,RUNFRST                                                     
         BE    A38RUNF                                                          
         CLI   MODE,REQFRST                                                     
         BE    A38REQF                                                          
         CLI   MODE,REQLAST                                                     
         BE    A38REQL                                                          
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(BUFFALOC)                                                      
         DC    A(SORTAREA)                                                      
         DC    V(SORTER)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(ACCEDIT)                                                       
         DC    V(CHOPCON)                                                       
         DC    A(OFFBUFF)                                                       
         DC    V(ACLIST)                                                        
         DC    A(IOAREA)                                                        
         DC    A(NAMETBL)                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
*----------------------------------------------------------------               
*              RUN FIRST                                                        
*----------------------------------------------------------------               
*                                                                               
A38RUNF  LA    RE,VTYPES           MOVE VTYPES TO W/S                           
         LA    R0,ADCONS                                                        
         LA    RF,VTYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         BAS   RE,STATER           HAND MODE TO STATEMENT MODULE.               
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------               
*              REQUEST FIRST                                                    
*----------------------------------------------------------------               
*                                                                               
A38REQF  LA    RE,WORK1ST                                                       
         LA    RF,4095                                                          
         XCEF                                                                   
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAY3)                                
         GOTO1 (RF),(R1),,(2,TODAY2)                                            
*&&US                                                                           
         CLI   QOPT3,C'Y'          TAPE OUTPUT(FOR O&M)                         
         BNE   REQF110                                                          
         CLC   ALPHAID,=C'OM'      ONLY FOR O&M                                 
         BE    *+12                                                             
         MVI   QOPT3,C'N'                                                       
         B     REQF110                                                          
*                                                                               
         MVC   DSNAME,SPACES                                                    
         MVC   DSNAME(16),=C'ACCTAPE.AC038XX1'                                  
         MVC   DSNAME+13(2),ALPHAID                                             
         ZIC   R4,TAPECNT                                                       
         AH    R4,=H'1'                                                         
         STC   R4,TAPECNT                                                       
         GOTO1 DYNALLOC,DMCB,(0,=CL8'TAPE'),((R4),DSNAME)                       
         L     R4,=A(TAPE)                                                      
         OPEN  ((R4),OUTPUT)                                                    
*&&                                                                             
REQF110  MVC   LEDGPROF,PROGPROF                                                
         OC    LEDGPROF,LEDGPROF                                                
         BNZ   *+10                                                             
         MVC   LEDGPROF,DEFAULT                                                 
         XC    XFLAG,XFLAG         CLEAR LEVEL FLAG                             
*                                  BUILD SORTCARD FOR SORTER                    
         MVC   SORTCARD(28),=C'SORT FIELDS=(5,15,A,22,15,A,'                    
         MVC   SORTCARD+28(19),=C'52,15,A),FORMAT=BI '                          
*                                                                               
RQF120   XC    PSTART,PSTART                                                    
         MVC   PEND,=3X'FF'                                                     
         CLC   QSTART,SPACES                                                    
         BE    RQF130                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,PSTART)                                
RQF130   CLC   QEND,SPACES                                                      
         BE    RQF140                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,PEND)                                    
*                                                                               
RQF140   GOTO1 SORTER,PARM,SORTCARD,RECCARD,0                                   
         MVC   THISHED1,SPACES                                                  
         MVC   THISHED2,SPACES                                                  
         MVC   THISHED3,SPACES                                                  
         MVC   THISHED1+84(4),=C'UNIT'                                          
         MVC   THISHED1+93(1),QUNIT                                             
         MVC   THISHEDM,SPACES                                                  
         L     R3,AMONACC                                                       
         USING ACMD,R3         GET MONACC _                                     
         CLI   ACMMEND,X'FF'                                                    
         BE    RQF150                                                           
RQF150   CLI   QLEDGER,C' '                                                     
         BE    RQF165                                                           
         MVC   THISHED2+84(6),=C'LEDGER'                                        
         MVC   THISHED2+93(1),QLEDGER                                           
*                                                                               
RQF165   GOTO1 PROLLER,DMCB,0,OTHERTOT,2,3   INITIALIZE OTHERS                  
         CLC   QOPT4(3),SPACES     NO NEED TO BUILD TABLE?                      
         BE    EXIT                                                             
         LA    R3,ULTBLE           POINT TO POSSIBLE U/L'S                      
         LA    R4,HIERTBLE         POINT TO HIERARCHY TABLE                     
         XCEF  HIERTBLE,70         CLEAR THE TABLE                              
RQF170   CLI   0(R3),X'FF'         ARE WE AT THE END OF ULTBLE?                 
         BNE   RQF172              YES, THEN LEAVE                              
         B     EXIT                                                             
*                                                                               
RQF172   MVC   READKEY,SPACES      GO READ HIERCHY ELEMENT                      
         MVC   READKEY(1),QCOMPANY LOAD IN COMPANY                              
         MVC   READKEY+1(2),1(R3)  LOAD IN U/L                                  
         L     R2,VIOAREA          POINT TO WHERE RECORD WILL GO                
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',READKEY,(R2),0                   
         CLI   8(R1),0             WAS THE READ SUCESSFUL?                      
         BE    *+6                                                              
         DC    H'0'                DIE IF AN ERROR IN READ                      
         L     R2,VIOAREA                                                       
         AH    R2,DATADISP         R2=A(RECORD)                                 
RQF175   CLI   0(R2),0             DID WE HIT END OF RECORD??                   
         BNE   *+6                 YES, THEN DIE                                
         DC    H'0'                THERE MUST BE HIERCHY ELEMENT                
         CLI   0(R2),X'16'         CHECK FOR HIERCHY ELEMENT                    
         BE    RQF180              WE FOUND THE RIGHT ELEMENT                   
         ZIC   R1,1(R2)            RETRIEVE LENGTH OF ELEMENT                   
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         B     RQF175              LOOP BACK UP AGAIN                           
*                                                                               
         USING ACHEIRD,R2                                                       
RQF180   MVC   0(2,R4),1(R3)       LOAD IN THE U/L TO HIERTABLE                 
         MVC   2(1,R4),ACHRLEVA    LOAD IN LENGTH LEVEL A                       
         MVC   3(1,R4),ACHRLEVB    STORE LENGT OF LEVEL B                       
         MVC   4(1,R4),ACHRLEVC    STORE THE LENGTH                             
         MVC   5(1,R4),ACHRLEVD    STORE TOTAL LENGTH                           
*                                                                               
         LA    R4,6(R4)            BUMP TO NEXT SPOT IN HIERTBLE                
         LA    R3,3(R3)            BUMP TO NEXT SPOT IN ULTBLE                  
         B     RQF170              LOOP BACK UP THROUGH TABLE                   
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------------------               
*              CONVERT INPUT IN QOPT4-6 TO ACCOUNTS                             
*----------------------------------------------------------------               
*                                                                               
LOOKUP   NTR1                      R6 MUST POINT TO ELEM IN ULTBLE              
         L     R7,ADSUBAC                                                       
         USING TRSUBHD,R7                                                       
         XC    LEV1ACCT,LEV1ACCT   CLEAR TEMP WORK STORAGE                      
         LA    R5,ULTBLE           POINT TO CONVERSION TABLE                    
LOOKUP1  CLC   0(1,R5),TEMPCODE    SEE IF A MATCH                               
         BE    LOOKUP2             PROCESS THEN                                 
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   LOOKUP1B                                                         
         CLC   TEMPCODE(1),=C'V'   IS THIS SPECIAL CASE FOR VENDOR?             
         BNE   LOOKUP1A                                                         
         MVC   LEV1ACCT(15),TRSBACNT                                            
         B     EXIT                                                             
*                                                                               
LOOKUP1A MVI   VALID,X'FF'         SET NOT VALID FLAG                           
         B     EXIT                EXIT ROUTINE                                 
*                                                                               
LOOKUP1B LA    R5,3(R5)            BUMP TO NEXT CONVERSION ELEM                 
         B     LOOKUP1             BRANCH BACK UP                               
*                                                                               
LOOKUP2  LA    R4,ANALTBLE         POINT TO DYNAMIC TABLE                       
         MVC   UNITLEDG(2),1(R5)   READ IN CONVERSION U/L                       
LOOKUP3  CLC   1(2,R4),UNITLEDG    LOOK FOR MATCH                               
         BE    LOOKUP4             SAVE ACCOUNT FROM ANALTBLE                   
         LA    R4,15(R4)           BUMP TO NEXT ELEMENT IN TABLE                
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   LOOKUP3             KEEP LOOPING                                 
         LA    R5,3(R5)            BUMP TO NEXT UL IN ULTBLE                    
         B     LOOKUP1             SEARCH ON THROUGH ULTBLE                     
*                                                                               
LOOKUP4  MVC   LEV1ACCT(15),0(R4)  LOAD IN ACCOUNT FROM TABLE                   
LOOKUP5  B     EXIT                EXIT ROUTINE                                 
         DROP  R7                                                               
         EJECT                                                                  
*----------------------------------------------------------------               
*              PROCESS TRANSACTIONS                                             
*----------------------------------------------------------------               
*                                                                               
A38TRNS  L     R7,ADTRANS          POINT TO TRANSACTION ELEMENT                 
         USING TRANSD,R7           '44' ELEMENT DSECT                           
         CLI   TRNSEL,X'44'        CONFIRM THAT IT'S A TRANSACTION              
         BNE   EXIT                IF NOT LEAVE                                 
*                                                                               
         XC    ANFLAG,ANFLAG       FLAG USED IN BUILDING SORTKEY                
*        CLC   QSRTAREA,SPACES     IS ANY INPUT IN CONTRA FIELD?                
*        BE    A38TRN1             NO INPUT IN CONTRA FIELD                     
*        CLC   QSRTAREA(3),=C'CL='                                              
*        BE    *+6                                                              
*        DC    H'0'                SHOULD BE CL=UL                              
         CLC   QCUL,SPACES                                                      
         BE    A38TRN1                                                          
         L     R4,ADSUBAC          POINT TO CONTRA ACCOUNT                      
         USING TRSUBHD,R4          '43' ELEMENT DSECT                           
*        CLC   TRSBACNT+1(2),QSRTAREA+3   CONTRA UNIT AND LEDGER                
         CLC   TRSBACNT+1(2),QCUL         CONTRA UNIT AND LEDGER                
         BNE   EXIT                LEAVE IF NOT CONTRA WANTED                   
*                                                                               
A38TRN1  L     RF,VEXTRAS          LOAD EXTERNAL ADDRESSES                      
         USING RUNXTRAD,RF                                                      
         OC    VLISTREC,VLISTREC   ANY LIST RECORD                              
         BZ    A38TRN5             CHECK IF BLANK                               
         L     R4,ADSUBAC          POINT TO CONTRA ACCOUNT                      
         USING TRSUBHD,R4                                                       
         LA    R5,TRSBACNT+1       POINT TO CORRECT PLACE IN CONTRA KEY         
         LA    R3,X'FF'            INDICATE WE WANT PARTIAL LENGTH              
         L     R1,ADACC            FOR 2P/29                                    
         CLC   1(2,R1),=C'2P'                                                   
         BE    A38TRN3                                                          
         CLC   1(2,R1),=C'29'                                                   
         BE    A38TRN3                                                          
         LA    R5,TRSBACNT+3                                                    
         SR    R3,R3                                                            
A38TRN3  GOTO1 VACLIST,DMCB,VLISTREC,((R3),(R5))                                
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         CLI   DMCB,C'E'                                                        
         BE    EXIT                EXCLUDE IT                                   
*                                                                               
A38TRN5  CLC   QOPT4(3),SPACES     INPUT IN ANALYSIS FIELDS?                    
         BE    A38T2               CONTINUE IF NO INPUT IN ANALYSIS             
*                                                                               
         USING ACAPD,R3            'C0' ELEMENT DSECT                           
         L     R3,ADTRANS          POINT TO START OF TRANSACTIONS               
A38TRN7  CLI   ACAPEL,X'00'        CHECK IF THERE ARE NO 'C0' ELEMS             
         BE    OTHERS              LEAVE GRACEFULLY                             
*                                                                               
         CLI   ACAPEL,X'C0'        AN ANALYSIS POINTER ELEMENT?                 
         BE    A38TRN9             PREPARE TO PROCESS 'C0' ELEM                 
*                                                                               
         ZIC   R1,ACAPLEN          RETRIEVE LENGTH OF ELEMENT                   
         AR    R3,R1               BUMP TO NEXT ELEMENT                         
         B     A38TRN7             LOOP BACK UP                                 
*                                                                               
A38TRN9  LA    R5,ANALTBLE         POINT TO DYNAMIC TABLE                       
         MVC   ANALTBLE(100),SPACES                                             
         MVC   ANALTBLE+100(65),SPACES                                          
         L     R4,ADSUBAC          POINT TO CONTRA ACCOUNT                      
         USING TRSUBHD,R4          COVER CONTRA ACCOUNT                         
         MVC   0(15,R5),TRSBACNT   STORE CONTRA ACCOUNT IN TABLE                
         LA    R5,15(R5)           BUMP TO NEXT SPACE IN TABLE                  
         LR    RF,R7               LOAD IN A(TRANS)                             
         SH    RF,DATADISP         DROP BACK TO START OF KEY                    
         USING ACKEYD,RF           COVER KEY AREA                               
         MVC   0(15,R5),ACKEYACC                                                
         LA    R5,15(R5)           BUMP TO NEXT SPACE IN TABLE                  
         ZIC   R1,ACAPMIN          RETRIEVE NUMBER OF MINI ELEMS                
         LTR   R1,R1                                                            
         BZ    OTHERS                                                           
         LR    R2,R1               SAVE NUMBER OF MINI ELEMENTS                 
         LA    R2,2(R2)            COUNT CONTRA AND ACCOUNT                     
*                                                                               
A38TRN13 ZIC   R4,ACAPMLEN         RETRIEVE LENGTH OF MINI                      
         SH    R4,=H'3'            ONLY STORE ACCOUNT NAME                      
         EX    R4,*+8              MOVE ACCOUNT NAME TO TABLE                   
         B     *+10                BRANCH OVER EXECUTED MOVE                    
         MVC   1(0,R5),ACAPACCT    STORE MINI ELEM IN TABLE                     
         MVC   0(1,R5),ACKEYACC    LOAD IN COMPANY                              
         LA    R5,15(R5)           BUMP TO NEXT FREE SPOT IN TBLE               
         LA    R4,3(R4)            RESTORE PROPER LENGTH                        
         AR    R3,R4               BUMP TO NEXT MINI ELEMENT                    
         BCT   R1,A38TRN13         LOOP UNTIL DONE WITH ELEMENTS                
A38TRN14 MVI   0(R5),X'FF'         SET END OF TABLE MARKER                      
*                                                                               
A38TRN15 L     R5,=A(SORTREC)      GET ADDRESS OF SORT RECORD                   
         USING SORTRECD,R5         COVER THE AREA                               
         XC    SORTKEY,SORTKEY     CLEAR KEY AREA                               
         MVI   VALID,0             SET FLAG FOR VALID RECORD                    
         MVC   TEMPCODE(1),QOPT4   LOAD IN FIRST CODE                           
         BAS   RE,LOOKUP           CONVERT                                      
         MVC   SORTSBAC(15),LEV1ACCT STORE FIRST SORT ACCOUNT                   
         CLC   QOPT5(1),SPACES     ANY INPUT IN 2ND FIELD?                      
         BE    A38TRN21            NO MORE INPUT TO PROCESS                     
         MVC   TEMPCODE(1),QOPT5   LOAD IN SECOND CODE                          
         BAS   RE,LOOKUP           CONVERT                                      
         MVC   SORTACC(15),LEV1ACCT STORE SECOND SORT ACCOUNT                   
         CLC   QOPT6(1),SPACES     ANY INPUT IN 3RD FIELD?                      
         BE    A38TRN21            NO MORE INPUT TO PROCESS                     
         MVC   TEMPCODE(1),QOPT6   LOAD IN THIRD CODE                           
         BAS   RE,LOOKUP           CONVERT                                      
         MVC   SORTACC2(15),LEV1ACCT STORE THIRD SORT ACCOUNT                   
*                                                                               
A38TRN21 CLI   VALID,X'FF'         ERROR FLAG SET                               
         BE    OTHERS              LEAVE ROUTINE                                
         MVI   ANFLAG,X'FF'        SET FLAG                                     
         OC    SORTSBAC+1(14),SPACES PAD WITH BLANKS                            
         OC    SORTACC2+1(14),SPACES                                            
         OC    SORTACC+1(14),SPACES                                             
         B     A38T2                                                            
*                                                                               
OTHERS   LA    R0,1                                                             
         TM    TRNSSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    R0,2                                                             
         GOTO1 PROLLER,DMCB,3,OTHERTOT,TRNSAMNT,1,(R0)                          
         GOTO1 (RF),(R1),3,OTHERTOT,TRNSAMNT,2,(R0)                             
         B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
A38T2    CLC   TRNSDATE,PSTART     CHECK IT FALLS WITHIN DATES                  
         BL    EXIT                                                             
         CLC   TRNSDATE,PEND                                                    
         BH    EXIT                                                             
         LR    RF,R7                AND IS NOT PEELED                           
         SH    RF,DATADISP                                                      
         USING ACKEYD,RF                                                        
         OC    ACDTPEEL,ACDTPEEL                                                
         BZ    *+14                                                             
         CLC   ACDTPEEL,TODAY2                                                  
         BL    EXIT                                                             
*                                                                               
* SPECIAL REQUEST OPTIONS TO ONLY SHOW BILLABLE TIME FOR TYPE 49                
* ITEMS.  'DDSBOBL' MUST BE IN REQUESTOR                                        
*                                                                               
         CLC   =C'DDSBOBL',QUESTOR TEST SPECIAL REQUESTOR                       
         BNE   A38T2A                                                           
         CLC   =C'1R',ACKEYACC+1   TEST 1R                                      
         BNE   A38T2A                                                           
         CLI   TRNSTYPE,49         TEST BATCH TYPE 49                           
         BNE   EXIT                                                             
         CLC   =C'1C',ACKEYCON+1   TEST CONTRA=1C                               
         BNE   EXIT                                                             
         BAS   RE,TSTTIME          TEST IF BILLABLE TIME                        
         BNE   EXIT                NO                                           
*                                  BUILD SORT RECORD FROM TRANSACTION           
A38T2A   L     R5,=A(SORTREC)                                                   
         USING SORTRECD,R5                                                      
         CLI   ANFLAG,0                                                         
         BNZ   *+10                                                             
         XC    SORTKEY,SORTKEY                                                  
         XC    SORTRLEN(4),SORTRLEN                                             
         MVC   SORTANAL,TRNSANAL                                                
         OC    SORTANAL,SPACES                                                  
         CLI   ANFLAG,0            WAS ANALYSIS FIELD USED?                     
         BNZ   A38T3               USE ANALYSIS FIELDS AS ACCOUNT               
         MVC   SORTSBAC,ACKEYCON   SET COMPANY FIELD+CONTRA ACC                 
         MVC   SORTACC,ACKEYACC    CONTRA BECOMES ACC'T.                        
         MVC   SORTACC2,SPACES     FILL UNUSED ACCOUNT AREA                     
*                                                                               
A38T3    CLI   SORTSBAC,C' '                                                    
         BNH   *+10                                                             
         MVC   SORTSBAC(1),SORTACC FORCE COMPANY TO BE SAME                     
         MVC   SORTDATE,TRNSDATE                                                
         MVC   SORTREF,TRNSREF                                                  
         MVC   SORTMOS,TRNSBTCH                                                 
         ZIC   R1,SORTMOS+1                                                     
         CLI   SORTMOS+1,C'C'      CONVERT C'A/B/C' TO X'FA/FB/FC'              
         BH    *+8                                                              
         AH    R1,=H'57'                                                        
         STC   R1,SORTMOS+1                                                     
         MVC   SORTTYPE,TRNSTYPE                                                
         MVC   SORTSTAT,TRNSSTAT                                                
         ZAP   SORTAMNT,TRNSAMNT                                                
         CLC   ACKEYACC+1(2),=C'SJ'  FOR PRODUCTION                             
         BNE   A38T4                                                            
         L     RE,ADPROFIL                                                      
         USING ACPROFD,RE                                                       
         LTR   RE,RE                                                            
         BZ    A38T4                                                            
         MVC   SORTOFFC,ACPRUNIT   GET OFFICE CODE                              
         DROP  RF                                                               
A38T4    LA    R0,SORTLEN                                                       
         STH   R0,SORTRLEN                                                      
         CLI   QOPT1,C'N'          DON'T FORMAT NARRATIVE IF SUMMARY            
         BE    AC118                                                            
         LA    R0,SORTNARR                                                      
         LA    R1,500                                                           
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R3,SORTNARR         POINT TO NARRATION AREA IN SORT              
*                                                                               
         ZIC   R1,TRNSLEN          RETRIEVE TRANSACTION LENGTH                  
         SH    R1,=H'29'           SUBTRACT OFF LENGTH OF TRNS KEY              
         BM    AC102               DON'T MOVE TRNS IF LEN<0                     
         CLC   TRNSANAL,=C'99'                                                  
         BNE   *+8                                                              
         LA    R1,14                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TRNSNARR    MOVE TRANSACTION NARRATIVE                   
         LA    R3,2(R1,R3)                                                      
*                                  LOOP FOR SPECIAL ELEMENTS                    
AC102    ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0                                                          
         BE    AC112                                                            
         CLI   0(R3),X'23'         NUMBER ELEMENT                               
         BE    AC104                                                            
         CLI   0(R3),X'25'         EXTRA NUMBER ELEMENT                         
         BE    AC105                                                            
         CLI   0(R3),X'4C'         SUBSIDIARY POSTINGS ELEMENT                  
         BE    AC106                                                            
         CLI   0(R3),X'4E'         POSTING TRANSFER ELEMENT                     
         BE    AC107                                                            
         CLI   0(R3),X'4F'         CLIENT ETC ELEMENT                           
         BE    AC107A                                                           
         CLI   0(R3),X'50'         CASH ELEMENT                                 
         BE    AC108                                                            
         CLI   0(R3),X'60'         ACTIVITY DATE ELEMENT                        
         BE    AC110                                                            
         B     AC102                                                            
*                                  HANDLE NUMBER ELEMENT                        
         USING ACOTHERD,R2                                                      
AC104    LA    RE,PROFLIST                                                      
         CLC   QUNIT(2),=C'SR'                                                  
         BE    AC104A                                                           
         MVC   0(4,R3),=C'PRD='    NON-RECEIVABLE LEDGER                        
         MVC   4(6,R3),ACOTNUM                                                  
         MVC   10(5,R3),=C',JOB='                                               
         MVC   15(6,R3),ACOTNUM+6                                               
         LA    R3,22(R3)                                                        
         B     AC102                                                            
AC104A   CLI   0(RE),X'FF'         LOOK-UP KEYWORD IN TABLE                     
         BE    AC104B                                                           
         CLC   0(1,RE),ACOTPROF                                                 
         BE    AC104B                                                           
         LA    RE,L'PROFLIST(RE)                                                
         B     AC104A                                                           
AC104B   MVC   0(4,R3),1(RE)                                                    
         MVC   4(L'ACOTNUM,R3),ACOTNUM                                          
         LA    R3,14(R3)                                                        
         B     AC102                                                            
*                                  DEAL WITH EXTRA NUMBER ELEMENT               
         USING ACNOD,R2                                                         
AC105    MVC   0(4,R3),=C'ORD='                                                 
         MVC   4(6,R3),ACNO        ORDER NUMBER                                 
         LA    R3,10(R3)                                                        
         B     AC102                                                            
*                                  HANDLE SUBSIDIARY POSTINGS ELEMENT           
         USING TRSDESCD,R2                                                      
AC106    MVC   0(25,R3),=C'ANALYSIS POSTINGS MADE TO'                           
         ZIC   R1,TRSDLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   26(0,R3),TRSDACCS                                                
         LA    R3,27(R3,R1)                                                     
         B     AC102                                                            
*                                  HANDLE POSTING TRANSFER ELEMENT              
         USING TRTRANSD,R2                                                      
AC107    MVC   0(14,R3),=C'TRANSFERRED TO'                                      
         CLI   TRTRTYPE,C'T'                                                    
         BE    *+10                                                             
         MVC   12(4,R3),=C'FROM'                                                
         MVC   17(L'TRTRACC-1,R3),TRTRACC+1                                     
         MVC   32(2,R3),=C'ON'                                                  
         GOTO1 DATCON,DMCB,(1,TRTRDATE),(8,35(R3))                              
         LA    R3,44(R3)                                                        
         B     AC102                                                            
*                                  HANDLE CLI/PROD/JOB ELEMENT                  
         USING TRCPJD,R2                                                        
AC107A   MVC   0(4,R3),=C'JOB='                                                 
         CLI   TRCPTYPE,C'J'                                                    
         BE    *+10                                                             
         MVC   0(3,R3),=C'EXP'                                                  
         MVC   WORK(18),TRCPCLI                                                 
         GOTO1 VSQUASH,DMCB,WORK,18                                             
         MVC   4(18,R3),WORK                                                    
         LA    R3,22(R3)                                                        
         B     AC102                                                            
*                                  HANDLE CASH ELEMENT                          
         USING TRCASHD,R2                                                       
AC108    CLI   QOPT7,C'Y'          SPECIAL REQUEST OPTION                       
         BNE   AC102                                                            
         MVC   0(7,R3),=C'AMOUNT='                                              
         LA    R3,7(R3)                                                         
         EDIT  (P6,TRCSAMNT),(10,0(R3)),2,ALIGN=LEFT,MINUS=YES                  
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         MVC   1(1,R3),TRCSTYPE                                                 
         LA    R3,3(R3)                                                         
         B     AC102                                                            
*                                  HANDLE DATE ELEMENT                          
         USING TRSTATD,R2                                                       
AC110    CLI   QOPT7,C'Y'          SPECIAL REQUEST OPTION                       
         BNE   AC102                                                            
         MVC   0(5,R3),=C'DATE='                                                
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(8,5(R3))                               
         CLI   5(R3),C' '                                                       
         BNE   *+8                                                              
         OI    5(R3),X'F0'                                                      
         LA    R3,14(R3)                                                        
         B     AC102                                                            
*                                  OTHER USEFULL INFO                           
AC112    TM    SORTSTAT,X'40'                                                   
         BZ    *+14                                                             
         MVC   0(3,R3),=C'*U*'     URGENT MARKER                                
         LA    R3,4(R3)                                                         
         CLI   QOPT7,C'Y'          OTHER SPECIAL REQUEST OPTIONS                
         BNE   AC114                                                            
         MVC   0(5,R3),=C'TYPE='   INPUT TYPE                                   
         ZIC   R1,SORTTYPE                                                      
         EDIT  (R1),(3,5(R3)),ALIGN=LEFT                                        
         OI    5(R3),X'F0'                                                      
         LA    R3,9(R3)                                                         
         MVC   0(7,R3),=C'STATUS=' TRANSACTION STATUS                           
         GOTO1 HEXOUT,DMCB,SORTSTAT,7(R3),1,=C'TOG'                             
         LA    R3,10(R3)                                                        
*                                  SQUASH NARRATIVE & SET LENGTH                
AC114    LA    R0,SORTNARR                                                      
         SR    R3,R0                                                            
         GOTO1 VSQUASH,DMCB,SORTNARR,(R3)                                       
         L     R0,4(R1)                                                         
         AH    R0,SORTRLEN                                                      
         STH   R0,SORTRLEN                                                      
*                                  SORT OR PRINT TRANSACTION                    
AC118    GOTO1 SORTER,PARM,=C'PUT',(R5)                                         
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*----------------------------------------------------                           
* FILTER OUT BILLABLE TIME TYPE 49 TRANSACTIONS                                 
*----------------------------------------------------                           
*                                                                               
TSTTIME  NTR1  ,                                                                
         LR    RF,R7                                                            
*                                                                               
TSTTIME1 CLI   0(RF),0             TEST FOR EOR                                 
         BE    TSTTIMEN            YES-DO NOT WANT IT                           
         CLI   0(RF),X'40'         TEST FOR HOURS ELEMENT                       
         BE    TSTTIME2                                                         
         ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     TSTTIME1                                                         
*                                                                               
         USING ACPERSD,RF                                                       
TSTTIME2 CLI   ACPSLEN,ACPSLNQ     TEST ELEMENT BIG ENOUGH                      
         BL    TSTTIMEN                                                         
         TM    ACPSSTAT,ACPSBIL    TEST BILLABLE ITEM                           
         BNO   TSTTIMEN                                                         
         ZAP   DUB,ACPSRATE        GET RATE                                     
         MP    DUB,ACPSHOUR        MULTIPLY BY HOURS                            
         SRP   DUB,64-2,5                                                       
         ZAP   TRNSAMNT,DUB        FUDGE AMOUNT                                 
         B     TSTTIMEY                                                         
*                                                                               
TSTTIMEN LTR   RB,RB                                                            
         B     TSTTIMEX                                                         
*                                                                               
TSTTIMEY CR    RB,RB                                                            
*                                                                               
TSTTIMEX B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
*----------------------------------------------------------------               
*              REQUEST LAST                                                     
*----------------------------------------------------------------               
*                                                                               
A38REQL  XC    SAVEKEY,SAVEKEY                                                  
         MVI   SUBMODE,0                                                        
         MVI   MODE,REQFRST                                                     
         BAS   RE,STATER                                                        
*                                                                               
         USING SORTRECD,R8                                                      
RQL020   GOTO1 SORTER,PARM,=C'GET',0                                            
         ZICM  R8,PARA2,4                                                       
         BZ    RQLEOF                                                           
         ST    R8,ADTRANS                                                       
         CLC   SORTSBAC,SAVSBAC                                                 
         BNE   *+14                CB ON SUB ACC'T.                             
         CLC   SORTACC,SAVACCT                                                  
         BE    RQL040              NO CB, GOTO WRITER.                          
         BAS   RE,KEYANAL          CB, FIND IT & DO IT ALL.                     
         MVC   SAVEKEY,SORTKEY                                                  
         B     RQL060                                                           
*                                                                               
RQL040   DS    0H                                                               
         CLC   SAVANAL,SORTANAL                                                 
*        BE    RQL060              NO ANALYSIS/WORK CODE CHANGE.                
*        CLC   SAVANAL,SPACES                                                   
*        BE    *+12                OLD CODE WAS SPACES, IGNORE.                 
*        MVI   MODE,ANALLAST       HAND WRITER MODE FOR NON-SPACE CODE.         
*        BAS   RE,STATER                                                        
*        MVC   SAVANAL,SORTANAL                                                 
*        CLC   SORTANAL,SPACES                                                  
*        BE    RQL060              NEW CODE IS SPACES, IGNORE.                  
*        MVI   MODE,ANALFRST       HAND WRITER MODE FOR NON-SPACE CODE.         
*        BAS   RE,STATER                                                        
*                                                                               
RQL060   MVI   MODE,PROCTRNS                                                    
         BAS   RE,STATER                                                        
         B     RQL020                                                           
*                                                                               
RQLEOF   L     R8,=A(SORTREC)      EOF, FORCE ALL LASTS.                        
         XC    SORTKEY,SORTKEY                                                  
         MVI   SUBMODE,REQLAST     SET SPECIAL MODE FOR SUMMARY PRINT.          
         BAS   RE,KEYANAL                                                       
         MVI   MODE,REQLAST                                                     
         BAS   RE,STATER                                                        
*&&US                                                                           
         CLI   QOPT3,C'Y'                                                       
         BNE   EXIT                                                             
         L     R4,=A(TAPE)                                                      
         CLOSE ((R4))                                                           
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------               
*              BUILD ACCOUNT HIERARCHY TABLE                                    
*----------------------------------------------------------------               
*                                                                               
KEYANAL  NTR1                                                                   
         USING SORTRECD,R8                                                      
         USING KEYTABD,R7                                                       
         LA    R7,KEYTAB                                                        
         LA    R2,KEYTBLNQ         BXLE SET UP.                                 
         L     R3,ALSTLVL                                                       
         CLI   SAVACCT,0                                                        
         BNE   KYL020              NOT FIRST READ.                              
         MVC   KEYTAB,HIERTAB      SET HIERARCHY TABLE, PRO-TEMPORE.            
         LA    R3,KEYTAB+SBAC*KEYTBLNQ                                          
         ST    R3,ALSTLVL          ALSO CONTROL ADDRESSES, PRO-TEMP.            
         ST    R7,ACBLVL                                                        
         B     KYL080              HAND ON FIRST MODES WITH RECORDS.            
*                                                                               
KYL020   ZIC   RE,KEYLEN           RE = KEY FIELD LENGTH-1.                     
         ZIC   RF,KEYDISP                                                       
         LA    R5,SORTKEY(RF)      R5 = A(INCOMING KEY FIELD).                  
         LA    R4,SAVEKEY(RF)      R6 = A(LAST KEY FIELD).                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R5)                                                    
         BNE   KYL040              CB AT THIS FIELD.                            
         BXLE  R7,R2,KYL020        TRY THE NEXT.                                
         DC    H'0'                READ LIED.                                   
*                                                                               
KYL040   ST    R7,ACBLVL           HOLD A(CNTRL BREAK LEVEL).                   
         LR    R3,R7               LO LIMIT FOR BXH.                            
         L     R7,ALSTLVL          A(LOEST ACC LVL ENTRY).                      
         LNR   R2,R2                                                            
         BCTR  R3,0                                                             
*                                                                               
KYL060   MVC   MODE,KEYLAST        GET LASTMODE FROM TABLE ENTRY,               
         BAS   RE,STATER           PASS TO WRITER.                              
         BXH   R7,R2,KYL060        DO FROM LOWEST TO CB LEVEL.                  
*                                                                               
         CLI   SORTKEY,0                                                        
         BE    EXIT                EOF, NO FIRSTS OR PROCS.                     
         LPR   R2,R2                                                            
         L     R7,ACBLVL           WORK DOWN TABLE READING RECORDS,             
         L     R3,ALSTLVL          DOING ANY SPECIAL HOUSE-KEEPING FOR          
KYL080   ZICM  RF,KEYAREC,2        EACH RECORD TYPE AND HAND PROC MODES         
         BZ    KYL090              NO RECORD TO READ.                           
         LA    RF,ACWORKD(RF)      TO WRITER FOR EACH TYPE.                     
         MVC   MODE,KEYTYP                                                      
         BAS   RE,READER                                                        
         L     R3,ALSTLVL          LEDGER READ MAY CHANGE L'TABLE.              
KYL090   MVC   MODE,KEYFRST                                                     
         BAS   RE,STATER                                                        
         CLI   KEYPROC,0                                                        
         BE    *+14                                                             
         MVC   MODE,KEYPROC                                                     
         BAS   RE,STATER                                                        
         BXLE  R7,R2,KYL080                                                     
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------               
*              READ THE RECORD FROM SORTER                                      
*----------------------------------------------------------------               
READER   NTR1                                                                   
         USING KEYTABD,R7                                                       
         ST    RF,AARECORD         SAVE A(A(RECORD)).                           
         ZIC   RF,KEYLEN           SET UP KEY FOR RECORD TYPE.                  
         ZIC   RE,KEYDISP                                                       
         AR    RF,RE                                                            
         MVC   WORKEY,SPACES                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORKEY(0),SORTKEY   GET RELEVANT BIT OF KEY.     .               
         CLI   MODE,SBAC                                                        
         BE    READ060             'SBAC' IS ALWAYS GOOD.                       
         CLI   WORKEY,C' '          CHECK FOR FUNNY CONTRA KEY.                 
         BE    READ040                                                          
         CLI   WORKEY,C'*'                                                      
         BE    READ040                                                          
         CLI   WORKEY,C'+'                                                      
         BNE   READ060                                                          
*                                                                               
READ040  L     R2,AARECORD                                                      
         L     R2,0(R2)            R2 = A(RECORD AREA)                          
         USING ACKEYD,R2           BUILD A DUMMY RECORD.                        
         MVC   ACKEYACC(L'SORTKEY),SPACES                                       
         MVC   ACKEYACC(L'KEY),WORKEY DUMMY KEY                                 
         LH    R1,DATADISP                                                      
         LA    R1,1(R1)            LNTH OF DUMMY REC = DATADISP+1.              
         STH   R1,ACLENGTH                                                      
         MVI   ACRECORD,0          RECORD END.                                  
         B     READ080                                                          
*                                                                               
READ060  L     R2,AARECORD                                                      
         L     R2,0(R2)            R2 = A(RECORD AREA)                          
         MVC   WORKEY+(ACKEYWRK-ACKEYD)(2),SPACES                               
         CLI   MODE,SBAC           SBAC READS FOR ACC'T REC OF SBAC KEY         
         BNE   READ070                                                          
         MVC   WORKEY(L'ACKEYCON),WORKEY+ACKEYCON-ACKEYD                        
         MVC   WORKEY+(ACKEYCON-ACKEYD)(L'ACKEYCON),SPACES                      
*                                                                               
READ070  GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,WORKEY,(R2),0                        
         CLI   DMCB+8,X'10'         IF RECORD IS NOT FOUND, ASSUME              
         BE    READ040              IT'S A CONTRA PROBLEM.                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
READ080  MVC   KEY,WORKEY                                                       
         CLI   MODE,SBAC           NO NEED TO SET A(NEXT REC) AT SBAC           
         BE    POST010             TIME AS IT'S ADTRANS. SEE A38REQL.           
*                                                                               
POST010  ZIC   RF,MODE             POST ADDRESS OF ELEMENTS FOR THIS            
         SLL   RF,2                RECORD TYPE.                                 
         L     RF,APOSTABS(RF)     RF = A(POSTAB) FOR THIS TYPE.                
         USING POSTABD,RF                                                       
*                                                                               
POST020  CLI   PSTLCOD,0                                                        
         BE    POSTEND             NO MORE ELEMENT CODES.                       
         L     R8,AARECORD                                                      
         L     R8,0(R8)            R8 = A(RECORD).                              
         AH    R8,DATADISP         R8 = A(1ST ELEMENT)                          
POST030  ZICM  RE,PSTAEL,2                                                      
         LA    RE,ACWORKD(RE)      RE = A(ADDR FIELD FOR THIS EL).              
         XC    0(4,RE),0(RE)       CLEAR IT.                                    
*                                                                               
POST040  CLI   0(R8),0                                                          
         BE    POST070             END OF REC.                                  
         CLC   0(1,R8),PSTLCOD                                                  
         BE    POST060             FOUND THIS ONE.                              
         ZIC   R1,1(R8)                                                         
         AR    R8,R1                                                            
         B     POST040                                                          
*                                                                               
POST060  ST    R8,0(RE)            SAVE A(THIS ELMNT).                          
*                                                                               
POST070  LA    RF,PSTBLNQ(RF)      GET NEXT ELEMENT TYPE.                       
         B     POST020                                                          
*                                                                               
POSTEND  ZIC   RE,MODE             DO ANY SPECIAL HOUSEKEEPING FOR              
         SLL   RE,2                THIS TYPE.                                   
         L     RF,POSCRPTS(RE)                                                  
         LTR   RF,RF                                                            
         BZ    READ085                                                          
         BASR  RE,RF               DO SPECIFIC RECORD ROUTINE.                  
*                                                                               
READ085  CLI   MODE,SBAC           NO NEED TO SET A(NEXT REC) AT SBAC           
         BE    EXIT                TIME AS IT'S ADTRANS. SEE A38REQL.           
         LH    RF,ACLENGTH         SET A(NEXT RECORD TYPE).                     
         LA    RF,0(R2,RF)                                                      
         ZICM  RE,KEYAREC+KEYTBLNQ,2                                            
         BNZ   READ090             A(A(REC)) FOR THIS TYPE.                     
         ZICM  RE,KEYAREC+2*KEYTBLNQ,2 NO A(A(REC)), TRY THE NEXT.              
         BNZ   *+6                 GOT ONE HERE.                                
         DC    H'0'                TABLE LOOKS DAMAGED.                         
READ090  LA    RE,ACWORKD(RE)      RE = A(FIELD OF A(NEXT REC)).                
         ST    RF,0(RE)            STORE A(NEXT REC).                           
         B     EXIT                                                             
         EJECT                                                                  
POSCRPTS DC    A(0)                UNIT                                         
         DC    A(POSLEDG)          LEDGER                                       
         DC    A(0)                LEVEL A                                      
         DC    A(0)                LEVEL B                                      
         DC    A(0)                LEVEL C                                      
         DC    A(0)                LEVEL D                                      
         DC    A(0)                ACCOUNT                                      
         DC    A(0)                ANALYSIS CODE                                
         DC    A(0)                SUB-ACCT                                     
         EJECT                                                                  
*----------------------------------------------------------------               
*              ANALYZE HIERARCHY TABLE                                          
*----------------------------------------------------------------               
*                                                                               
POSLEDG  NTR1                                                                   
         L     R8,ADLEDGER         FOR LEDGER, ANALYZE HIERARCHY                
         AH    R8,DATADISP                                                      
         USING ACHEIRD,R8          MODIFYING KEYTAB TO REFLECT CBS FOR          
         LA    R7,KEYTBAC1         THIS LEDGER.                                 
         USING KEYTABD,R7                                                       
*                                                                               
POSL020  CLI   ACHREL,0                                                         
         BNE   POSL025                                                          
         LA    RE,LGHIRDEF         DUMMY LEDGER, POINT TO BASIC                 
         ST    RE,ADLDGHIR         HIERARCHY.                                   
         B     POSL100                                                          
*                                                                               
POSL025  CLI   ACHREL,X'16'                                                     
         BE    POSL060                                                          
POSL030  ZIC   R1,ACHRLEN                                                       
         AR    R8,R1                                                            
         B     POSL020                                                          
*                                                                               
POSL060  LA    R1,3                                                             
         LA    R4,HIERLEVA                                                      
         LA    R2,4                                                             
         LA    R5,ACHRLEVA                                                      
*                                                                               
POSL080  CLI   0(R5),0                                                          
         BNE   *+12                GET NEXT LEVEL SIZE.                         
         S     R7,=A(KEYTBLNQ)     LAST LEVEL WAS LOWEST.                       
         B     POSL100             IT'S ACC LEVEL.                              
         MVC   0(KEYTBLNQ,R7),0(R4) GET TABLE ENTRY FOR LEVEL.                  
         STC   R1,KEYDISP                                                       
         ZIC   R0,0(R5)                                                         
         SR    R0,R1               R0 = L'ACCT SO FAR LESS DISPLACEMENT         
         AH    R0,=H'3'            R0 = L'THIS ACCT LEVEL ONLY.                 
         AR    R1,R0               R1 = DISPLACEMENT OF NEXT LEVEL.             
         BCTR  R0,0                R0 = LNTH IN EXECUTABLE FORM.                
         STC   R0,KEYLEN                                                        
         LA    R5,16(R5)                                                        
         LA    R7,KEYTBLNQ(R7)                                                  
         LA    R4,KEYTBLNQ(R4)                                                  
         BCT   R2,POSL080                                                       
*                                                                               
POSL100  MVC   0(KEYTBLNQ,R7),HIERACC                                           
         LA    R7,KEYTBLNQ(R7)                                                  
POSL110  MVC   0(KEYTBLNQ,R7),HIERANAL                                          
         LA    R7,KEYTBLNQ(R7)                                                  
         MVC   0(KEYTBLNQ,R7),HIERSBAC                                          
         ST    R7,ALSTLVL                                                       
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------               
*              PROCESS TRANS/CHECK MODES                                        
*----------------------------------------------------------------               
*                                                                               
STATER   NTR1                                                                   
         MVC   INMODE,MODE         HOLD ENTRY MODE                              
         GOTO1 =A(WRITER),DMCB,(RA),0  PROCESS THE TRANS.                       
         CLC   INMODE,MODE                                                      
         BE    EXIT                MODE DIDN'T CHANGE.                          
         DC    H'0'                                                             
         DROP  RF                                                               
         EJECT                                                                  
*----------------------------------------------------------------               
*              LITERAL DECLARATIONS                                             
*----------------------------------------------------------------               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------               
*              CONSTANT DECLARATIONS                                            
*----------------------------------------------------------------               
*                                                                               
VALID    DS    XL1                 FLAG FOR ERROR                               
RECCARD  DC    C'RECORD TYPE=V,LENGTH=600 '                                     
SORTCARD DC    CL80' '                                                          
*                                                                               
DEFAULT  DC    C'NYY             '                                              
*                                                                               
PROFLIST DS    0CL5                                                             
         DC    C'SEST='                                                         
         DC    C'PEST='                                                         
         DC    C'JJOB='                                                         
         DC    C'MCMP='                                                         
         DC    X'FF',C'NUM='                                                    
*                                                                               
APOSTABS DS    0A                                                               
         DC    A(UNPOST)                                                        
         DC    A(LGPOST)                                                        
         DC    A(LVAPOST)                                                       
         DC    A(LVBPOST)                                                       
         DC    A(LVCPOST)                                                       
         DC    A(LVDPOST)                                                       
         DC    A(ACCPOST)                                                       
         DC    A(ANALPOST)                                                      
         DC    A(SUBPOST)                                                       
*                                                                               
ULTBLE   DS    0F                  CONVERSION TABLE FOR INPUT U/L               
         DC    C'D',C'2D'                                                       
         DC    C'P',C'2P'                                                       
         DC    C'T',C'1P'                                                       
         DC    C'9',C'29'                                                       
         DC    C'C',C'1C'                                                       
         DC    C'8',C'28'                                                       
         DC    C'E',C'SE'                                                       
         DC    C'V',C'SX'                                                       
         DC    C'V',C'SV'                                                       
         DC    C'V',C'2C'                                                       
         DC    X'00',C'SC'                                                      
         DC    X'00',C'SJ'                                                      
ULLENQ   EQU   (*-ULTBLE)/3        LENGTH OF TABLE/3 = # ROWS                   
         DC    X'FF'                                                            
*                                                                               
POSTABS  DS    0A                                                               
UNPOST   DC    X'20',AL2(ADUNTNAM-ACWORKD)                                      
         DC    AL1(0)                                                           
*                                                                               
LGPOST   DC    X'14',AL2(ADLDGEL-ACWORKD)                                       
         DC    X'16',AL2(ADLDGHIR-ACWORKD)                                      
         DC    X'20',AL2(ADLDGNAM-ACWORKD)                                      
         DC    AL1(0)                                                           
*                                                                               
LVAPOST  DC    X'20',AL2(ADLVANAM-ACWORKD)                                      
         DC    AL1(0)                                                           
*                                                                               
LVBPOST  DC    X'20',AL2(ADLVBNAM-ACWORKD)                                      
         DC    AL1(0)                                                           
*                                                                               
LVCPOST  DC    X'20',AL2(ADLVCNAM-ACWORKD)                                      
         DC    AL1(0)                                                           
*                                                                               
LVDPOST  DC    X'20',AL2(ADLVDNAM-ACWORKD)                                      
         DC    AL1(0)                                                           
*                                                                               
ACCPOST  DC    X'20',AL2(ADACCNAM-ACWORKD)                                      
         DC    X'22',AL2(ADACCADD-ACWORKD)                                      
         DC    X'26',AL2(ADACCJOB-ACWORKD)                                      
         DC    X'30',AL2(ADACCSTA-ACWORKD)                                      
         DC    X'32',AL2(ADACCBAL-ACWORKD)                                      
         DC    AL1(0)                                                           
*                                                                               
ANALPOST DC    A(0)                                                             
*                                                                               
SUBPOST  DC    A(0)                                                             
ACCOUNT  DC    C'ACCOUNT'                                                       
*                                                                               
HIERTAB  DS    0CL8                                                             
         DC    AL1(0,0,1,UNITFRST,0,UNITLAST),AL2(ADUNIT-ACWORKD)               
         DC    AL1(1,0,2,LEDGFRST,0,LEDGLAST),AL2(ADLEDGER-ACWORKD)             
HIERLEVA DC    AL1(2,0,3,LEVAFRST,PROCLEVA,LEVALAST)                            
         DC    AL2(ADHEIRA-ACWORKD)                                             
         DC    AL1(3,0,0,LEVBFRST,PROCLEVB,LEVBLAST)                            
         DC    AL2(ADHEIRB-ACWORKD)                                             
         DC    AL1(4,0,0,LEVCFRST,PROCLEVC,LEVCLAST)                            
         DC    AL2(ADHEIRC-ACWORKD)                                             
         DC    AL1(5,0,0,ACCFRST,PROCLEVD,0)                                    
         DC    AL2(ADHEIRD-ACWORKD)                                             
HIERACC  DC    AL1(6,11,3,ACCFRST,PROCACC,ACCLAST),AL2(ADACC-ACWORKD)           
*                                                                               
HIERANAL DC    AL1(7,1,15,0,0,0),AL2(0)                                         
*                                                                               
HIERSBAC DC    AL1(8,14,17,SBACFRST,PROCSBAC,SBACLAST)                          
         DC    AL2(ADSUBAC-ACWORKD)                                             
*                                                                               
LGHIRDEF DS    0CL67                                                            
         DC    X'1642'                                                          
         DC    X'0C',CL15' '                                                    
         DC    XL49'00'                                                         
*                                                                               
SBAC     EQU   8                                                                
ANAL     EQU   7                                                                
ACCT     EQU   6                                                                
LEVD     EQU   5                                                                
LEVC     EQU   4                                                                
LEVB     EQU   3                                                                
LEVA     EQU   2                                                                
LEDG     EQU   1                                                                
UNIT     EQU   0                                                                
         EJECT                                                                  
*----------------------------------------------------------------               
*              PRINT THE REPORT                                                 
*----------------------------------------------------------------               
*                                                                               
WRITER   DS    0D                                                               
         NMOD1 0,*WRITER*,RA,R9                                                 
*                                                                               
         L     R8,0(R1)                                                         
         USING ACWORKD,R8                                                       
         L     RC,=A(WORKC)                                                     
         USING WORKD,RC                                                         
         LA    RC,MYEND                                                         
         USING WORK1D,RC                                                        
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   MODE,0                                                           
         BE    XIT                                                              
         CLI   MODE,RUNFRST                                                     
         BE    AC6                                                              
         CLI   MODE,REQFRST                                                     
         BE    AC10                                                             
         CLI   MODE,UNITFRST                                                    
         BE    AC20                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    AC28                                                             
         CLI   MODE,PROCACC                                                     
         BE    AC40                                                             
         CLI   MODE,ANALFRST                                                    
         BE    AC70                                                             
         CLI   MODE,SBACFRST                                                    
         BE    AC90                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    AC100                                                            
         CLI   MODE,SBACLAST                                                    
         BE    AC130                                                            
         CLI   MODE,ANALLAST                                                    
         BE    AC140                                                            
         CLI   MODE,ACCLAST                                                     
         BE    AC150                                                            
         CLI   MODE,LEVCLAST                                                    
         BE    AC160                                                            
         CLI   MODE,LEVBLAST                                                    
         BE    AC160                                                            
         CLI   MODE,LEVALAST                                                    
         BE    AC160                                                            
         CLI   MODE,LEDGLAST                                                    
         BE    AC160                                                            
         CLI   MODE,UNITLAST                                                    
         BE    AC160                                                            
         CLI   MODE,REQLAST                                                     
         BE    AC160                                                            
*                                                                               
XIT      XMOD1 1                                                                
         EJECT                                                                  
*----------------------------------------------------------------               
*              MAIN PRINT PROCESSING                                            
*----------------------------------------------------------------               
*                                                                               
AC6      XC    CHOPSAVE,CHOPSAVE                                                
         L     RE,VOFFBUFF                                                      
         XC    0(4,RE),0(RE)                                                    
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------               
*              FIRST FOR REQUEST                                                
*----------------------------------------------------------------               
*                                                                               
AC10     MVI   SWITCHES,C'N'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   SWITCHES+1(L'SWITCHES-1),SWITCHES                                
         GOTO1 BUFFALO,DMCB,=C'SET',VBUFF                                       
         GOTO1 PROLLER,DMCB,0,ACCUMS,9,3                                        
         GOTO1 PROLLER,DMCB,0,TOTALS,6,3  INITIALIZE TOTALS ARRAY               
         XC    ELEMCNTR,ELEMCNTR          CLEAR TABLE COUNTER                   
         ZAP   LDGBLFWD,=P'0'                                                   
         ZAP   ACCCNTR,=P'0'                                                    
         ZAP   ACCCNTU,=P'0'                                                    
         ZAP   ACCCNT,=P'0'                                                     
         ZAP   ACCUNM,=P'0'                                                     
         ZAP   LDGCNT,=P'0'                                                     
         ZAP   UNTCNT,=P'0'                                                     
*                                  SET OFFICE SWITCH                            
         L     R1,ADCMPEL                                                       
         USING ACCOMPD,R1                                                       
         TM    ACMPSTAT,X'20'                                                   
         BZ    *+8                                                              
         MVI   OFFSW,C'Y'                                                       
         DROP  R1                                                               
         L     R3,VNAMETBL                                                      
         MVC   0(5,R3),=C'FFFFF'   SET FLAG FOR ACCOUNT TABLE                   
*                                  SET FILTER SWITCH                            
         CLC   QSTART(12),SPACES                                                
         BNE   *+14                                                             
         CLC   QTRNSFLT,SPACES                                                  
         BE    XIT                                                              
         MVI   FLTSW,C'Y'                                                       
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------               
*              FIRST FOR UNIT                                                   
*----------------------------------------------------------------               
*                                                                               
AC20     MVC   THISHED2(60),SPACES FIRST FOR UNIT.                              
         MVI   SAVELVL,3           PRINT OUTSTANDING UNIT SUMMARY.              
         CP    ACCCNTU,=P'1'                                                    
         BL    AC22                NO ACCTS FOR C/UNIT.                         
         BAS   RE,PRNTSUM                                                       
AC21     BAS   RE,CLEARBUF                                                      
         AP    ACCCNTR,ACCCNTU                                                  
         ZAP   ACCCNTU,=P'0'                                                    
*                                                                               
AC22     MVC   THISHED1(60),SPACES                                              
         MVC   THISHED1(6),=C'C/UNIT'                                           
         L     R1,ADUNIT                                                        
         MVC   THISHED1+10(1),1(R1)                                             
         BAS   RE,GETNAME                                                       
         MVC   THISHED1+12(36),WORK                                             
         ZAP   LDGCNT,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------               
*              FIRST FOR LEDGER                                                 
*----------------------------------------------------------------               
*                                                                               
AC28     MVI   SAVELVL,2           FIRST FOR LEDGER.                            
         XC    LASTACCS,LASTACCS                                                
         CP    ACCUNM,=P'1'        PRINT OUTSTANDING LEDGER SUMMARY.            
         BL    AC30                NO ACCTS FOR C/LEDGER.                       
         BAS   RE,PRNTSUM                                                       
AC29     BAS   RE,CLEARBUF                                                      
         AP    ACCCNTU,ACCUNM                                                   
         ZAP   ACCUNM,=P'0'                                                     
*                                                                               
AC30     L     R2,ADLDGEL                                                       
         USING ACLEDGD,R2                                                       
         MVC   PRTSW,ACLTPRNT                                                   
         MVI   SUBPROG,1                                                        
         CLI   PRTSW,C'+'                                                       
         BE    AC35                                                             
         CLI   PRTSW,C'-'                                                       
         BE    AC35                                                             
         MVI   PRTSW,C'N'                                                       
         MVI   SUBPROG,0                                                        
*                                                                               
AC35     CLI   QOPT1,C'N'                                                       
         BNE   *+8                                                              
         OI    SUBPROG,X'02'                                                    
         MVC   THISHED2(60),SPACES                                              
         MVC   THISHED3(60),SPACES                                              
         MVC   THISHED2(8),=C'C/LEDGER'                                         
         L     R1,ADLEDGER                                                      
         MVC   THISHED2+10(1),2(R1)                                             
         BAS   RE,GETNAME                                                       
         MVC   THISHED2+12(36),WORK                                             
*                                                                               
         LA    R2,7                CLEAR PROLLER FOR LEVD,C,B,A.                
         GOTO1 PROLLER,DMCB,2,ACCUMS,(R2)                                       
         BCTR  R2,0                                                             
         GOTO1 (RF),(R1),,,(R2)                                                 
         BCTR  R2,0                                                             
         GOTO1 (RF),(R1),,,(R2)                                                 
         BCTR  R2,0                                                             
         GOTO1 (RF),(R1),,,(R2)                                                 
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------------------               
*              FIRST FOR ACCOUNT                                                
*----------------------------------------------------------------               
*                                                                               
AC40     MVI   ACCSW,C'N'                                                       
         MVI   ANALSW,C'N'                                                      
         MVI   SAVESW,C'N'                                                      
         ZAP   TRNSCNT,=P'0'                                                    
         ZAP   ANALCNT,=P'0'                                                    
         L     R1,ADACC                                                         
         MVC   THATACC,0(R1)       GET A/C CODE & NAME                          
         BAS   RE,GETNAME                                                       
         MVC   THISNAME,WORK                                                    
         MVC   THISHED4,SPACES                                                  
         MVC   THISHED5,SPACES                                                  
         MVC   THISHED6,SPACES                                                  
         MVC   THISHED7,SPACES                                                  
         MVC   THISACCT,SPACES                                                  
         GOTO1 VACCEDIT,DMCB,(0,ADACC),ADLDGHIR,THISACCT                        
         MVI   ACCSW,C'F'                                                       
*                                                                               
AC60     L     RF,ADACC                                                         
         AH    RF,DATADISP                                                      
AC62     CLI   0(RF),0                                                          
         BE    XIT                                                              
         CLI   0(RF),X'33'         FIND PEEL ELEMENT                            
         BE    AC64                                                             
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     AC62                                                             
         USING ACPEELD,RF                                                       
AC64     CLC   TODAY3,ACPEPLDT     IF PEELED TODAY FIX UP ACCOUNT               
         BNE   XIT                 SO THAT IT LOOKS AS IF                       
         L     R2,ADACCBAL         HASN'T HAPPENED                              
         LTR   R2,R2                                                            
         BZ    XIT                                                              
         USING ACBALD,R2                                                        
         SP    ACBLFRWD,ACPEDR                                                  
         AP    ACBLFRWD,ACPECR                                                  
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         MVC   ACSTBFDT,ACPELBDT                                                
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------               
*              FIRST FOR ANALYSIS CODE                                          
*----------------------------------------------------------------               
*                                                                               
AC70     MVI   ANALSW,C'N'                                                      
         L     R2,ADTRANS                                                       
         USING SORTRECD,R2                                                      
         MVC   THISANAL,SORTANAL   FORMAT CODE/NAME INTO WORK                   
         OC    THISANAL,SPACES                                                  
         CLC   THISANAL,SPACES                                                  
         BE    XIT                                                              
         BAS   RE,GETANAL                                                       
         MVI   ANALSW,C'F'         SET FT SWITCH                                
         CLI   QOPT1,C'N'                                                       
         BNE   *+8                                                              
         MVI   ANALSW,C'Y'                                                      
         CLI   SORTSW,C'N'                                                      
         BE    *+8                                                              
         MVI   ANALSW,C'Y'                                                      
AC72     B     XIT                                                              
         EJECT                                                                  
         DROP  R2                                                               
*----------------------------------------------------------------               
*              FIRST FOR SUB-ACCOUNT                                            
*----------------------------------------------------------------               
*                                                                               
AC90     MVI   SBACSW,C'N'                                                      
         MVC   THISSBNM,SPACES                                                  
         MVI   TRNSSW,C'N'                                                      
         USING ACKEYD,RF                                                        
         LA    RF,KEY                                                           
         MVC   THISSBAC,ACKEYACC                                                
         L     R1,ADSUBAC                                                       
         BAS   RE,GETNAME                                                       
         MVC   THISSBNM,WORK                                                    
         MVI   SBACSW,C'F'         FIRST TIME FOR SUB-ACCT.                     
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*----------------------------------------------------------------               
*              TRANSACTION HANDLING                                             
*----------------------------------------------------------------               
*                                                                               
         USING SORTRECD,R6                                                      
AC100    L     R6,ADTRANS                                                       
         LA    R0,1                                                             
         TM    SORTSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    R0,2                                                             
         GOTO1 PROLLER,DMCB,3,ACCUMS,SORTAMNT,1,(R0)                            
         GOTO1 (RF),(R1),6         CROSS CAST & ROLL                            
         GOTO1 PROLLER,DMCB,3,TOTALS,SORTAMNT,1,(R0)                            
         GOTO1 (RF),(R1),3,OTHERTOT,SORTAMNT,2,(R0)                             
*                                  HANDLE OFFICE/ANALYSIS POSTING               
         CLI   ANALSW,C'N'                                                      
         BNE   AC120                                                            
         CLC   SORTANAL,SPACES                                                  
         BE    AC120                                                            
         MVC   THISANAL,SORTANAL                                                
*                                  PRINT TRANSACTIONS                           
AC120    BAS   RE,PRNTTRNS                                                      
*&&US                                                                           
         CLI   QOPT3,C'Y'          TAPE OUTPUT                                  
         BNE   AC122                                                            
         L     RF,=A(WRTAPE)                                                    
         BASR  RE,RF                                                            
*&&                                                                             
AC122    MVI   TRNSSW,C'Y'                                                      
         AP    TRNSCNT,=P'1'                                                    
AC124    GOTO1 PROLLER,DMCB,2,ACCUMS,1                                          
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------               
*              LAST FOR SUB-ACCOUNT                                             
*----------------------------------------------------------------               
*                                                                               
AC130    GOTO1 PROLLER,DMCB,1,ACCUMS,2                                          
         L     R2,DMCB                                                          
         CLI   TRNSSW,C'Y'         ANY ACTIVITY FOR THIS SUB-ACCOUNT            
         BNE   AC132                                                            
         BAS   RE,PRNTTRNS         NO - PRINT LAST TRANSACTION                  
AC131    CP    0(6,R2),=P'0'                                                    
         BNE   *+14                                                             
         CP    6(6,R2),=P'0'                                                    
         BE    AC132                                                            
         BAS   RE,POSTSBAC                                                      
AC132    MVC   0(18,R2),=3PL6'0'                                                
         MVI   TRNSSW,C'N'                                                      
         B     XIT                                                              
*                                                                               
*              LAST FOR ANALYSIS CODE                                           
*                                                                               
AC140    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------               
*              LAST FOR ACCOUNT                                                 
*----------------------------------------------------------------               
*                                                                               
AC150    CP    TRNSCNT,=P'0'       ANY TRANSACTIONS ?                           
         BNE   AC154                                                            
         BAS   RE,PRNTTRNS                                                      
         B     AC158                                                            
AC154    DS    0H                                                               
*                                                                               
         BAS   RE,PRNTTRNS         PRINT BALANCE FORWARD                        
*                                                                               
AC155    CLI   QOPT1,C' '          ARE WE PRINTING SUMMARIES                    
         BE    AC156                                                            
         CP    ACCCNT,=P'1'                                                     
         BH    *+12                                                             
         CLI   QOPT1,C'N'                                                       
         BNE   AC156                                                            
         CLI   LEDGPROF,C'Y'                                                    
         BNE   *+12                                                             
         MVI   SAVELVL,1                                                        
         BAS   RE,PRNTSUM          PRINT CONTRA-ACCOUNT SUMMARY                 
*                                                                               
AC156    GOTO1 ,DMCB,=C'ADD',VBUFF,1,2,3,(X'80',4)                              
         L     RF,BUFFALO                                                       
         BASR  RE,RF                                                            
         GOTO1 (RF),(R1),=C'CLEAR',,(X'80',1)                                   
AC157    GOTO1 PROLLER,DMCB,2,ACCUMS,3                                          
AC158    ZAP   ACCCNT,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------               
*              LAST FOR HIGH LEVEL ACCOUNTS/LEDGER                              
*----------------------------------------------------------------               
*                                                                               
AC160    L     R2,ADLDGHIR         R2=A(HEIRARCHY ELEMENT)                      
         USING ACHEIRD,R2                                                       
         SR    R5,R5                                                            
         LA    R3,4                R3=ACCUM LINE NUMBER                         
         L     R4,ADLVCNAM         R4=A(NAME ELEMENT)                           
         L     R7,ADACC                                                         
         LA    R7,3(R7)            R7 = A(ACCOUNT CODE START)                   
         ZIC   R0,ACHRLEVC         R0 = L'THIS LEVEL'S CODE                     
         CLI   MODE,LEVCLAST                                                    
         BE    AC162                                                            
         LA    R3,5                                                             
         L     R4,ADLVBNAM                                                      
         ZIC   R0,ACHRLEVB         R0 = L'THIS LEVEL'S CODE                     
         CLI   MODE,LEVBLAST                                                    
         BE    AC162                                                            
         LA    R3,6                                                             
         L     R4,ADLVANAM                                                      
         ZIC   R0,ACHRLEVA         R0 = L'THIS LEVEL'S CODE                     
         CLI   MODE,LEVALAST                                                    
         BE    AC162                                                            
         LA    R3,7                                                             
         L     R4,ADLDGNAM                                                      
         LA    R5,=CL15'C/LEDGER'                                               
         L     R7,ADACC                                                         
         LA    R7,1(R7)            R7 = A(UNIT/LEDGER CODE)                     
         LA    R0,2                R0 = L'UNIT/LEDGER CODES                     
         CLI   MODE,LEDGLAST                                                    
         BE    AC162                                                            
*                                                                               
         LA    R3,8                                                             
         L     R4,ADUNTNAM                                                      
         LA    R5,=CL15'C/UNIT'                                                 
         LA    R0,1                R0 = L'UNIT CODE                             
         CLI   MODE,UNITLAST                                                    
         BE    AC162                                                            
*                                                                               
         CLI   QOPT1,C' '                                                       
         BNE   AC166               SUMMARY WIL PRINT REQ TOTS.                  
         LA    R3,9                                                             
         SR    R4,R4                                                            
         LA    R5,=CL15'REQUEST'                                                
         SR    R7,R7                                                            
         SR    R0,R0                                                            
*                                                                               
AC162    GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)                                       
         L     R2,DMCB                                                          
         ZAP   FORMACCS,0(6,R2)                                                 
         ZAP   FORMACCS+8(8),6(6,R2)                                            
         MVC   0(18,R2),=3PL6'0'   CLEAR THIS LEVEL ACCUMS                      
         CLI   QOPT1,C'N'                                                       
         BE    AC166                                                            
         CLI   QOPT2,C'S'          OPTION TO SUPPRESS OTHER TOTALS              
         BE    AC166                                                            
         CLC   FORMACCS(8),LASTACCS                                             
         BE    AC166               DON'T PRINT DUPLICATE TOTALS.                
         MVC   LASTACCS,FORMACCS                                                
*                                                                               
         CLI   MODE,LEDGLAST                                                    
         BE    AC162B                                                           
         CP    FORMACCS,=P'0'      ANYTHING TO PRINT                            
         BNE   AC162B                                                           
         CP    FORMACCS+8(8),=P'0'                                              
         BNE   AC162B              NO - XIT                                     
         MVC   P(110),SPACES                                                    
         B     AC164                                                            
*                                                                               
AC162B   MVC   P+24(10),=C'TOTALS FOR'                                          
         LTR   R5,R5                                                            
         BZ    *+10                                                             
         MVC   P+35(15),0(R5)                                                   
         MVC   WORK,SPACES                                                      
         LR    R1,R0               L'CODE TO R1                                 
         LTR   R7,R7                                                            
         BZ    AC162B2             NO CODES                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R7)       CODE TO WORK                                 
AC162B2  LA    R1,WORK(R1)         FIND LAST CHAR OF CODE                       
         LTR   R4,R4                                                            
         BZ    AC162C              NO NAME                                      
         ZIC   R5,1(R4)                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),2(R4)       MOVE NAME TO WORK                            
*                                                                               
AC162C   CLC   WORK,SPACES                                                      
         BE    AC162C2             NO CODE OR NAME                              
         LA    R4,P+50                                                          
         CLI   0(R4),C' '          FIND END OF DESCRIPTION                      
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         LA    R5,P+69                                                          
         CLI   PRTSW,C'N'                                                       
         BE    *+8                                                              
         LA    R5,P+81                                                          
         SR    R5,R4                                                            
         GOTO1 CHOPPER,DMCB,(50,WORK),((R5),2(R4)),(C'P',2)                     
*                                                                               
AC162C2  MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         MVC   PTHIRD,PSECOND                                                   
         MVC   PSECOND,P                                                        
         MVC   P(110),SPACES                                                    
AC164    BAS   RE,REPORT                                                        
AC166    CLI   MODE,LEDGLAST                                                    
         BE    AC170               PRINT SUMMARIES AT LEDGLAST                  
         CLI   MODE,UNITLAST                                                    
         BE    AC180                                                            
         CLI   MODE,REQLAST                                                     
         BE    AC200                                                            
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------               
*              LAST FOR LEDGER                                                  
*----------------------------------------------------------------               
*                                                                               
AC170    MVI   SAVELVL,2                                                        
         MVC   HALF2,PAGE                                                       
         MVC   THISHED4,SPACES                                                  
         MVC   THISHED5,SPACES                                                  
         MVC   THISHED6,SPACES                                                  
         MVC   THISHED7,SPACES                                                  
         AP    LDGCNT,=P'1'                                                     
         B     XIT                                                              
         SPACE 1                                                                
*AC180    MVC   PAGE,=H'1'          LAST FOR UNIT.                              
AC180    MVI   SAVELVL,2           PRINT OUTSTANDING LEDGER SUMMARY.            
         CP    ACCUNM,=P'1'                                                     
         BL    AC184               NO ACCTS FOR C/LEDGER.       .               
         BE    AC182               ONE ACCT FOR C/LEDGER.                       
         BAS   RE,PRNTSUM                                                       
AC182    BAS   RE,CLEARBUF                                                      
AC184    ZAP   ACCCNT,=P'0'                                                     
         AP    ACCCNTU,ACCUNM      ACCUM UNIQUE ACC CODES FOR UNIT.             
         ZAP   ACCUNM,=P'0'                                                     
         AP    UNTCNT,=P'1'                                                     
         B     XIT                                                              
*                                                                               
AC200    CLC   QOPT4(3),SPACES                                                  
         BE    AC204                                                            
         MVI   XFLAG,X'CC'                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   ANFLAG(1),RCSUBPRG       SAVE THE HEADING IN USE                 
         MVI   RCSUBPRG,7                                                       
         GOTO1 PROLLER,DMCB,1,OTHERTOT,1                                        
         L     R2,0(R1)                                                         
         ZAP   FORMACCS,0(6,R2)                                                 
         ZAP   FORMACCS+8(8),6(6,R2)                                            
         ZAP   FORMACCS+16(8),12(6,R2)                                          
         MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         OC    P(110),SPACES                                                    
         CLC   P(110),SPACES                                                    
         BE    AC202                                                            
         MVC   P+24(25),=C'TOTALS FOR OTHER ACCOUNTS'                           
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
AC202    GOTO1 PROLLER,DMCB,1,OTHERTOT,2                                        
         L     R2,0(R1)                                                         
         ZAP   FORMACCS,0(6,R2)                                                 
         ZAP   FORMACCS+8(8),6(6,R2)                                            
         ZAP   FORMACCS+16(8),12(6,R2)                                          
         MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         OC    P(110),SPACES                                                    
         CLC   P(110),SPACES                                                    
         BE    AC203                                                            
         MVC   P+24(18),=C'TOTALS FOR REQUEST'                                  
         BAS   RE,REPORT                                                        
         XC    XFLAG,XFLAG                                                      
AC203    MVI   FORCEHED,C'Y'                                                    
         MVC   RCSUBPRG(1),ANFLAG       RESTORE ORIGINAL HEADING                
*AC204    MVC   PAGE,=H'1'          LAST FOR REQUEST.                           
AC204    MVC   THISHED2(60),SPACES                                              
         MVI   SAVELVL,3           PRINT OUTSTANDING UNIT SUMMARY.              
         CP    ACCCNTU,=P'1'                                                    
         BL    AC220               NO UNITS ACTIVE FOR REQUEST                  
         BE    AC210               ONLY ONE UNIT ACTIVE.                        
         CP    LDGCNT,=P'1'                                                     
         BNH   AC210               SUMMARY SAME AS PREVIOUS.                    
         BAS   RE,PRNTSUM                                                       
AC210    BAS   RE,CLEARBUF                                                      
         AP    ACCCNTR,ACCCNTU                                                  
*                                                                               
AC220    MVC   THISHED1(60),SPACES PRINT REQUEST SUMMARY.                       
         MVI   SAVELVL,4                                                        
         CP    ACCCNTR,=P'1'                                                    
         BNH   XIT                                                              
         CP    UNTCNT,=P'1'                                                     
         BNH   XIT                 SUMMARY SAME AS PREVIOUS.                    
         BAS   RE,PRNTSUM                                                       
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*----------------------------------------------------------------               
*              POST CONTRA-ACCOUNT TOTAL TO BUFFER                              
*----------------------------------------------------------------               
*                                                                               
POSTSBAC NTR1                                                                   
         XC    BUFFKEY,BUFFKEY                                                  
         MVC   BUFFCODE,THISSBAC                                                
         MVI   BUFFTYPE,1                                                       
         CP    ACCUNM,=P'1'        TESTING FOR MORE THAN ONE UNIQUE             
         BL    PSB040              ACC CODE FOR THIS SBAC.                      
         BH    PSB045                                                           
         GOTO1 BUFFALO,DMCB,=C'GET',VBUFF,BUFFREC,1                             
         CLI   DMCB+8,0                                                         
         BE    PSB045              ACC CODE ALREADY IN BUFFER.                  
PSB040   AP    ACCUNM,=P'1'        UNIQUE SO FAR, ACCUM IT.                     
PSB045   MVC   BUFFNAME,THISSBNM   NAME SUPPLIED                                
         OC    BUFFNAME,BUFFNAME                                                
         BNZ   *+10                                                             
         MVC   BUFFNAME,SPACES                                                  
         ZAP   BUFFDR,0(6,R2)                                                   
         ZAP   BUFFCR,6(6,R2)                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',VBUFF,BUFFREC                               
         AP    ACCCNT,=P'1'        ACCUM SBACS IN LEDGER.                       
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------------                    
*              ROUTINE TO PRINT A TRANSACTION                                   
*                      R6=SORTREC                                               
*-----------------------------------------------------------                    
*                                                                               
         USING SORTRECD,R6                                                      
PRNTTRNS NTR1                                                                   
         MVC   RCSUBPRG,SUBPROG                                                 
         ST    R6,SAVEASRT         SAVE A(SORT RECORD)                          
         CLI   SAVESW,C'Y'         DO I HAVE TO PRINT THE PREVIOUS              
         BNE   PRNTTRNC            TRANSACTION                                  
*                                                                               
         L     R6,=A(SAVESORT)     A(SAVED TRANSACTION RECORD)                  
         BAS   RE,CLEARBLK         CLEAR OUT THE PRINT BLOCK                    
         LA    RE,SORTLEN          FIGURE OUT THE LENGTH OF                     
         LH    R0,SORTRLEN         THE NARRATIVE FOR THE TRANS                  
         SR    R0,RE               R0=L'NARRATIVE                               
         BZ    PRNTTRN2            BRANCH IF NO NARRATIVE                       
         LA    R2,22               WILL NARRATIVE PRINT ON 1 OR 2               
         CLI   PRTSW,C'N'          SHORT LINES                                  
         BE    *+8                                                              
         LA    R2,34                                                            
         GOTO1 CHOPPER,DMCB,(0,SORTNARR),((R2),PRNTBLOC+50),(C'P',20),C*        
               'LEN=',(R0)                                                      
         CLC   8(4,R1),=F'3'                                                    
         BL    PRNTTRN2                                                         
         CLI   LEDGPROF+2,C'Y'                                                  
         BE    PRNTTRN2                                                         
         BAS   RE,CLEARBLK         NO - PRINT LONG NARRATIVE UNDERNEATH         
         LA    R2,41               TRANSACTION DETAILS                          
         CLI   PRTSW,C'N'                                                       
         BE    *+8                                                              
         LA    R2,53                                                            
         LA    R3,PRNTBLOC                                                      
         LA    R3,163(R3)                                                       
         GOTO1 (RF),DMCB,,((R2),(R3)),(C'P',20)                                 
*                                    FORMAT TRANSACTION DETAILS                 
PRNTTRN2 CLC   QOPT4(3),SPACES       USING ATTRIBUTE FIELDS?                    
         BE    PRNT2D                NO, THEN BRANCH                            
*                                                                               
         CLC   LEV2ACCT(15),SORTACC  DID 2ND SRTFLD CHANGE?                     
         BE    PRNTTR2               NO? DONT PRINT SRTFLD AGAIN                
*                                                                               
         CLI   LINE,43               CHECK TO SEE IF NEAR END OF PAGE           
         BNH   *+8                   BRANCH IF ENOUGH ROOM LEFT ON PG           
         MVI   LINE,99               FORCE NEW PAGE                             
         MVC   LEV2ACCT(15),SORTACC  STORE NEW ACCOUNT                          
         MVC   LEV3ACCT,SPACES       FORCE NAME TO PRINT                        
*        MVC   TMP3ACCT,SORTACC2                                                
         CLC   QOPT6(1),SPACES       USING THE 3RD SRT OPTION?                  
         BE    *+20                  BRANCH IF NOT USING IT                     
         MVC   THISHED7+12(14),SORTACC+1    PUT THE NAME IN HEADING             
         MVC   THISHED7(11),=C'ANALYSIS OF' GIVE IT A PREFIX                    
         B     *+10                  BRANCH OVER MOVE INSTRUCTION               
*                                                                               
         MVC   P+1(14),SORTACC+1     PRINT 2ND SORTED FIELD                     
         MVC   READKEY,SPACES        CLEAR OUT THE KEY                          
         MVC   READKEY(15),SORTACC   LOAD KEY FOR DATAMGR READHI                
         MVC   SAVACCNT(15),SORTACC  SAVE NAME WHEN PRINTING TOTALS             
         BAS   RE,READNAME           GET THE NAME                               
         MVC   SAVNAME(40),WORK      SAVE ACCOUNT NAME FOR BELOW                
         CLC   QOPT6(1),SPACES       CHECK IF USING 3RD SRT OPTION              
         BE    *+12                  BRANCH IF NOT USING IT                     
         LA    R3,THISHED7+12        POINT TO THE HEADING                       
         B     *+8                   ALWAYS BRANCH OVER NEXT INSTR              
         LA    R3,P+1                POINT TO START OF PRINTLINE                
         BAS   RE,CONCAT             LINK NAME AND ACCT IN PRINTLINE            
         MVC   1(40,R3),WORK                                                    
         CLC   QOPT6(1),SPACES       USING 3RD SRT OPTION?                      
         BNE   *+8                   ONLY PUT IN A BLANK LINE SOMETIMES         
         BAS   RE,REPORT             PRINT THE BLANK LINE                       
         MVI   TEMPCODE,X'AA'        JUST A FLAG FOR BELOW                      
*                                                                               
PRNTTR2  CLC   QOPT6(1),SPACES       ANY INPUT IN 3RD SRTFLD?                   
         BE    PRNT2D                DONT PROCESS 3RD SRTFLD.                   
*                                                                               
         CLC   LEV3ACCT,SORTACC2     HAS 3RD SRTFLD CHANGED?                    
         BE    PRNT2D                NO CHANGE                                  
*                                                                               
         CLI   LINE,44               ENOUGH ROOM LEFT ON PAGE?                  
         BNH   *+8                   YES, PLENTY OF ROOM LEFT                   
         MVI   LINE,99               CAUSE A NEW PAGE TO EJECT                  
         CLI   TEMPCODE,X'AA'        DONT SKIP A LINE IF CHANGE IN              
         BE    *+8                   2ND SORT ACCT FROM ABOVE                   
         BAS   RE,REPORT             PRINT A BLANK LINE                         
         XC    TEMPCODE,TEMPCODE     CLEAR OUT TEMP STORAGE                     
         OI    TOTFLAG,X'F0'         DO NORMAL CHECKING FOR TOTALS              
*                                                                               
         MVC   LEV3ACCT(15),SORTACC2 STORE ACCOUNT FOR TOTALS CHECK             
         MVC   P+3(14),SORTACC2+1    PUT ACCOUNT IN PRINT FIELD                 
         MVC   READKEY,SPACES        CLEAR OUT THE KEY                          
         MVC   READKEY(15),SORTACC2  LOAD KEY FOR DATAMGR READHI                
         BAS   RE,READNAME           GET THE NAME OF THE ACCT                   
         LA    R3,P+3                POINT TO START OF ACCOUNT NUM              
         BAS   RE,CONCAT             BRING THEM TOGETHER                        
         MVC   1(50,R3),WORK         MOVE IN NAME                               
         BAS   RE,REPORT             PRINT THE LINE                             
*                                                                               
PRNT2D   CLI   LINE,47               ENOUGH ROOM LEFT ON PAGE?                  
         BNH   *+8                   BRANCH IF THERE IS ENOUGH                  
         MVI   LINE,99               ELSE PUSH OUT A NEW PAGE                   
         MVC   PRNTBLOC+24(6),SORTMOS PUT THE MONTH INTO PRINTLINE              
         ZIC   R1,PRNTBLOC+25        READ MONTH FROM PRINTLINE                  
         CLI   PRNTBLOC+25,X'FA'     CONVERT X'FA/FB/FC' TO C'A/B/C'            
         BL    *+8                   DO WE NEED TO CONVERT?                     
         SH    R1,=H'57'             YES, CONVERT TO PRINTABLE FORM             
         STC   R1,PRNTBLOC+25        PUT IT BACK OUT TO PRINTLINE               
         GOTO1 DATCON,DMCB,(1,SORTDATE),(8,PRNTBLOC+31)                         
         MVC   PRNTBLOC+40(6),SORTREF  MOVE IN THE REFERENCE NUMBER             
         MVC   PRNTBLOC+47(2),SORTANAL MOVE IN THE ANALYSIS POSTINGS            
*                                                                               
         MVC   FORMACCS(32),=4PL8'0' CLEAR OUT ACCUMS USED IN PRINT             
         LA    R1,FORMACCS           POINT TO THE ACCUMULATORS                  
         TM    SORTSTAT,X'80'        CHECK IF A DEBIT OR CREDIT                 
         BO    *+8                   BRANCH IF A DEBIT                          
         LA    R1,8(R1)              ELSE BUMP TO CREDIT COLUMN                 
         AP    0(8,R1),SORTAMNT      ADD IN THE AMOUNT OF TRANSACTION           
         MVC   P,PRNTBLOC            MOVE THE BLOCK FOR PRINTING                
         LA    R2,PRNTBLOC           POINT TO PRINT BLOCK                       
         LA    R2,132(R2)            BUMP TO NEXT BLOCK IN LINE                 
         MVC   PSECOND,0(R2)         MOVE OUT THE SECOND LINE                   
         MVI   FORMSW,C'D'           FORMAT AMOUNT                              
         BAS   RE,FORMACC            FORMAT THE AMOUNT INTO PRINT               
         CLC   QOPT6(1),SPACES       IF USING 3RD ATTRIBUTE FIELD               
         BNE   PRNTTRN4              DONT PRINT BALANCE                         
         CLI   MODE,SBACLAST         IS THIS THE LAST TRANSACTION FOR           
         BNE   PRNTTRN4              THIS CONTRA-ACCOUNT                        
         GOTO1 PROLLER,DMCB,1,ACCUMS,2  GET ADDRESS OF BALANCE                  
         L     R2,DMCB               YES - PRINT CONTRA-ACC BALANCE             
         ZAP   FORMACCS+24(8),0(6,R2)   LOAD IN DEBITS                          
         SP    FORMACCS+24(8),6(6,R2)   SUBTRACT THE CREDITS                    
         MVI   FORMSW,C'B'           SET PRINT BALANCE SWITCH                   
         BAS   RE,FORMACC            FORMAT BALANCE TO PRINT LINE               
PRNTTRN4 MVC   PRNTBLOC,P                                                       
         LA    R2,PRNTBLOC                                                      
         LA    R2,132(R2)                                                       
         MVC   0(L'PSECOND,R2),PSECOND                                          
         MVC   PSECOND,SPACES                                                   
         LA    R2,PRNTBLOC                                                      
         CLI   SORTSW,C'N'                                                      
         BE    PRNTTRN6                                                         
         MVC   SAVESBAC(110),SPACES                                             
         MVC   SAVESBAC(L'SORTSBAC-1),SORTSBAC+1                                
*                                  PRINT THE TRANSACTION                        
PRNTTRN6 CLC   0(132,R2),SPACES                                                 
         BE    PRNTTRN8                                                         
         MVC   P,0(R2)                                                          
         CLC   QOPT4(3),SPACES     USING ATTRIBUTE FIELD?                       
         BNE   *+10                IF SPACES THEN FALL THROUGH                  
         MVC   P+1(22),SAVESBAC    LOAD IN ACCOUNT                              
         MVC   SAVESBAC(88),SAVESBAC+22                                         
         MVC   RCSUBPRG,SUBPROG                                                 
         BAS   RE,REPORT                                                        
         LA    R2,132(R2)                                                       
         B     PRNTTRN6                                                         
*                                                                               
PRNTTRN8 MVI   SAVESW,C'N'                                                      
         CLI   MODE,SBACLAST                                                    
         BNE   PRNTTRNC                                                         
         CLC   QOPT4(3),SPACES     DONT PRINT NAME DOWN LEFT COL.               
         BE    *+10                                                             
         MVC   SAVESBAC,SPACES                                                  
         CLC   SAVESBAC,SPACES     ENSURE ALL THE CONTRA-ACCOUNT NAME           
         BE    PRNTTRNA            IS PRINTED                                   
         MVC   P+1(22),SAVESBAC                                                 
         MVC   PSECOND+1(22),SAVESBAC+22                                        
         MVI   SPACING,2                                                        
PRNTTRNA BAS   RE,REPORT                                                        
         B     PRNTTRNX                                                         
*                                  FIRST TRANSACTION FOR ACCOUNT                
PRNTTRNC CLI   ACCSW,C'F'                                                       
         BNE   PRNTTRNR                                                         
         MVI   ACCSW,C'Y'                                                       
         MVC   THISHED6,SPACES     FORMAT NAME INTO HEADLINES                   
         MVC   THISHED7,SPACES     OR PRINTLINES                                
         CLC   QOPT4(3),SPACES                                                  
         BNE   PRNTC1                                                           
         CLI   LEDGPROF+1,C'Y'                                                  
         BNE   PRNTTRNE                                                         
PRNTC1   MVI   FORCEHED,C'Y'                                                    
         MVC   THISHED6(9),=C'C/ACCOUNT'                                        
         MVC   THISHED6+12(L'THISACCT),THISACCT                                 
         MVC   SAV2ACCT(15),THISACCT                                            
         MVC   SAV2NAME(L'THISNAME),THISNAME                                    
         CLC   QOPT6(1),SPACES                                                  
         BE    PRNTD1                                                           
         LA    R3,THISHED6+12                                                   
         BAS   RE,CONCAT                                                        
         MVC   0(36,R3),THISNAME                                                
*                                                                               
         MVI   TOTFLAG,X'F0'                                                    
         B     *+10                                                             
PRNTD1   MVC   THISHED7+12(L'THISNAME),THISNAME                                 
PRNTE1   MVC   LEV2ACCT,SPACES     FORCE 2ND SRT ACCOUNT TO PRINT               
         MVC   LEV3ACCT,SPACES     FORCE 3RD SRT ACCOUNT TO PRINT               
         MVC   TMP3ACCT,SORTACC2   SET ACCOUNT HOLD TO FIRST ACCOUNT            
         MVC   TMP2ACCT,SORTACC                                                 
PRNTTRND B     PRNTTRNN                                                         
PRNTTRNE BAS   RE,REPORT           SPACE LINE BEFORE NAME                       
         MVC   P+1(9),=C'C/ACCOUNT'                                             
         MVI   P+10,C'-'                                                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'THISACCT),THISACCT                                        
         MVC   WORK+20(L'THISNAME),THISNAME                                     
         GOTO1 VSQUASH,DMCB,WORK,64                                             
         L     R0,4(R1)                                                         
         GOTO1 CHOPPER,DMCB,((R0),WORK),(37,P+12),(C'P',2)                      
PRNTTRNN CLI   LEDGPROF+1,C'Y'     IF NOT SKIPING PAGES PRINT ACCOUNT           
         BE    PRNTTRNR            NAME                                         
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         B     PRNTTRNR                                                         
*                                  PRINT TOTALS/BALANCE CARRIED FORWARD         
PRNTTRNP CLI   QOPT1,C'N'                                                       
         BE    PRNTTRNX            SUMMARIES ONLY.                              
         MVI   CONTRFLG,X'FF'      PRINT CONTRA SUMMARIES AT END                
         B     PRNTTRNX                                                         
*                                  FIRST TRANSACTION FOR WORK-CODE              
PRNTTRNR CLI   MODE,ACCLAST                                                     
         BE    PRNTTRNP                                                         
         CLI   ANALSW,C'F'                                                      
         BNE   PRNTTRNT                                                         
         CLI   QOPT1,C'N'                                                       
         BE    PRNTTRNT                                                         
         MVI   ANALSW,C'Y'                                                      
         MVC   P+1(L'THISANL1),THISANL1                                         
         MVC   PSECOND+1(L'THISANL2),THISANL2                                   
         MVI   SPACING,2                                                        
         BAS   RE,REPORT           PRINT WORK-CODE NAME                         
*                                  FIRST TRANSACTION FOR SUB-ACCOUNT            
PRNTTRNT CLI   SBACSW,C'F'                                                      
         BNE   PRNTTRNV                                                         
         MVI   SBACSW,C'Y'                                                      
         MVC   WORK,SPACES         FORMAT CODE/NAME INTO WORK                   
         MVC   WORK(14),THISSBAC+1                                              
         MVC   WORK+16(L'THISSBNM),THISSBNM                                     
         GOTO1 VSQUASH,DMCB,WORK,64                                             
         L     R0,4(R1)                                                         
         MVC   SAVESBAC(110),SPACES                                             
         GOTO1 CHOPPER,DMCB,((R0),WORK),(22,SAVESBAC),4                         
PRNTTRNV CLI   QOPT1,C'N'                                                       
         BE    PRNTTRNX                                                         
         L     R0,=A(SAVESORT)                                                  
         L     RE,SAVEASRT                                                      
         LH    R1,0(RE)                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVI   SAVESW,C'Y'                                                      
*                                                                               
PRNTTRNX CLC   TMP2ACCT,SORTACC                                                 
         BE    PRNTTRNY              CHECK TO SEE IF NEW ACCOUNT                
         MVI   TOTFLAG,X'0F'         FORCE ALL TOTALS TO PRINT                  
         MVI   ACCTFLG,X'FF'         PRINT ACCOUNT TOTALS                       
         MVC   TMP2ACCT,SORTACC      SAVE NEW ACCOUNT                           
PRNTTRNY BAS   RE,PRTOTAL            PRINT THE TOTALS                           
         MVI   TOTFLAG,X'F0'         TURN OFF FIRST TIME THROUGH FLAG           
         MVI   ACCTFLG,0             RESET ACCOUNT TOTAL PRINT FLAG             
         GOTO1 PROLLER,DMCB,4,TOTALS,1,2  4TH LEVEL TOTALS                      
         GOTO1 PROLLER,DMCB,4,TOTALS,1,3  3RDLEVEL TOTALS                       
         GOTO1 PROLLER,DMCB,4,TOTALS,1,4  2ND LEVEL TOTALS                      
         GOTO1 PROLLER,DMCB,4,TOTALS,1,5  1ST LEVEL TOTALS                      
         GOTO1 PROLLER,DMCB,4,TOTALS,1,6  ACCOUNT TOTALS                        
         GOTO1 PROLLER,DMCB,2,TOTALS,1    CLEAR TEMP TOTAL LINE                 
*                                                                               
         CLI   CONTRFLG,X'FF'                                                   
         BNE   XIT                                                              
         GOTO1 PROLLER,DMCB,1,ACCUMS,3                                          
         L     R2,DMCB                                                          
         ZAP   FORMACCS,0(6,R2)                                                 
         ZAP   FORMACCS+8(8),6(6,R2)                                            
         CP    TRNSCNT,=P'0'                                                    
         BE    XIT                                                              
         MVC   P+24(20),=C'TOTALS FOR C/ACCOUNT'                                
         CLC   QOPT5(2),SPACES     PRINT NAME FOR TOTAL                         
         BE    PRNTTRY                                                          
         MVC   P+35(10),SPACES                                                  
         MVC   P+35(14),SAV2ACCT                                                
         LA    R3,P+35                                                          
         BAS   RE,CONCAT                                                        
         LA    R1,P+71                                                          
         SR    R1,R3                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SAV2NAME                                                 
PRNTTRY  MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC          FORMAT TOTALS LINE                           
         BAS   RE,REPORT                                                        
         MVI   CONTRFLG,0          RESET COUNTER                                
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*----------------------------------------------------------------               
*        PRINT OUT TOTALS FOR 3RD SRTFLD                                        
*----------------------------------------------------------------               
*                                                                               
         USING SORTRECD,R6                                                      
PRTOTAL  NTR1                                                                   
         CLI   QOPT6,C' '          ANY INPUT IN 3RD SRTFLD?                     
         BE    XIT                 LEAVE IF 3RD ATTRIBUTE NOT USED              
         MVI   ZEROTOTS,0                                                       
         CLI   TOTFLAG,0           SHOULD WE PRINT TOTALS?                      
         BE    PRTOTALY            NO                                           
*                                                                               
         CLC   SORTACC2+1(2),TMP3ACCT+1                                         
         BE    *+8                 CHECK FOR CHANGE OF U/L                      
         MVI   TOTFLAG,X'0F'       FORCE BOTTOM TOTAL TO PRINT                  
         BAS   RE,GETLEV           GET THE LEVEL STRUCTURE FOR U/L              
         DROP  R6                                                               
*                                                                               
         USING SORTRECD,R3                                                      
         L     R3,=A(SAVESORT)                                                  
*                                                                               
         CLI   LEVEL+3,0           IS LEVEL 4 USED??                            
         BE    PRTOTAL1            NO, CHECK NEXT HIGHER LEVEL                  
         TM    TOTFLAG,X'01'       0001 BIT ON?  TO PRINT W/O CHECK             
         BNZ   PRTOTALA                                                         
         ZIC   R2,LEVEL+3          LOAD IN LENGTH OF A+B+C+D LEVELS             
         LA    R2,1(R2)                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   SORTACC2+1(0),TMP3ACCT+1  SEE IF CHANGE IN 4TH LEVEL             
         BE    PRTOTAL1                  NO CHANGE                              
*                                                                               
PRTOTALA GOTO1 PROLLER,DMCB,1,TOTALS,2   GET ADDRESS OF D TOTALS                
         L     R2,0(R1)                  R2=A(D TOTALS)                         
         ZAP   FORMACCS,0(6,R2)          LOAD IN THE DEBITS                     
         ZAP   FORMACCS+8(8),6(6,R2)        LOAD IN THE CREDITS                 
         ZAP   FORMACCS+16(8),12(6,R2)      LOAD IN THE BALANCE                 
         ZAP   LASTTOTS,0(6,R2)                                                 
         ZAP   LASTTOTS+8(8),6(6,R2)                                            
         ZAP   LASTTOTS+16(8),12(6,R2)                                          
         MVI   FORMSW,C'A'               SET SWITCHES                           
         BAS   RE,FORMACC                FORMAT THE TOTALS                      
         OI    ZEROTOTS,X'01'                                                   
         OC    P(110),SPACES             PAD WITH BLANKS                        
         CLC   P(110),SPACES                                                    
         BE    PRTOTAL1                                                         
         NI    ZEROTOTS,X'FE'                                                   
         MVC   P+24(10),=C'TOTALS FOR'                                          
         LA    R3,P+35                                                          
         ZIC   R2,LEVEL+3                GET THE LENGTH OF LEVELS               
         BAS   RE,PRINTIT                                                       
         BAS   RE,REPORT                 PRINT THE LINE                         
PRTOTA1  GOTO1 PROLLER,DMCB,2,TOTALS,2   CLEAR LINE OF TOTALS                   
*                                                                               
PRTOTAL1 CLI   LEVEL+2,0                 IS LEVEL C USED??                      
         BE    PRTOTAL2                  NO, CHECK NEXT HIGHER LEVEL            
         TM    TOTFLAG,X'02'             0010 BIT ON?                           
         BNZ   PRTOTALB                                                         
         ZIC   R2,LEVEL+2                LOAD IN LENGTH OF A+B+C LEVELS         
         LA    R2,1(R2)                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   SORTACC2+1(0),TMP3ACCT+1  SEE IF CHANGE IN 3RD LEVEL             
         BE    PRTOTAL2                  NO CHANGE                              
*                                                                               
PRTOTALB GOTO1 PROLLER,DMCB,1,TOTALS,3   GET ADDRESS OF C TOTALS                
         L     R2,0(R1)                  R2=A(C TOTALS)                         
         ZAP   FORMACCS,0(6,R2)          LOAD IN THE DEBITS                     
         ZAP   FORMACCS+8(8),6(6,R2)        LOAD IN THE CREDITS                 
         ZAP   FORMACCS+16(8),12(6,R2)      LOAD IN THE BALANCE                 
         BAS   RE,CMPTOT                                                        
         CLI   XFLAG,0                                                          
         BE    PRTOTB1                                                          
         ZAP   LASTTOTS,0(6,R2)                                                 
         ZAP   LASTTOTS+8(8),6(6,R2)                                            
         ZAP   LASTTOTS+16(8),12(6,R2)                                          
         MVI   FORMSW,C'A'               SET SWITCHES                           
         BAS   RE,FORMACC                FORMAT THE TOTALS                      
         OC    P(110),SPACES                                                    
         OI    ZEROTOTS,X'02'                                                   
         CLC   P(110),SPACES                                                    
         BE    PRTOTAL2                                                         
         NI    ZEROTOTS,X'FD'                                                   
         MVC   P+24(10),=C'TOTALS FOR'                                          
         ZIC   R2,LEVEL+2          LOAD IN LENGTH OF A+B+C+D LEVELS             
         BAS   RE,PRINTIT                                                       
         BAS   RE,REPORT                 PRINT THE LINE                         
PRTOTB1  GOTO1 PROLLER,DMCB,2,TOTALS,3   CLEAR LINE OF TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,2   CLEAR LINE OF TOTALS                   
*                                                                               
PRTOTAL2 CLI   LEVEL+1,0                 IS LEVEL B USED??                      
         BE    PRTOTAL3                  NO, CHECK NEXT HIGHER LEVEL            
         TM    TOTFLAG,X'04'             0100 BIT ON?                           
         BNZ   PRTOTALC                                                         
         ZIC   R2,LEVEL+1                LOAD IN LENGTH OF A+B LEVELS           
         LA    R2,1(R2)                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   SORTACC2+1(0),TMP3ACCT+1  SEE IF CHANGE IN 2ND LEVEL             
         BE    PRTOTAL3                  NO CHANGE                              
*                                                                               
PRTOTALC GOTO1 PROLLER,DMCB,1,TOTALS,4   GET ADDRESS OF B TOTALS                
         L     R2,0(R1)                  R2=A(B TOTALS)                         
         ZAP   FORMACCS,0(6,R2)          LOAD IN THE DEBITS                     
         ZAP   FORMACCS+8(8),6(6,R2)        LOAD IN THE CREDITS                 
         ZAP   FORMACCS+16(8),12(6,R2)      LOAD IN THE BALANCE                 
         BAS   RE,CMPTOT                                                        
         CLI   XFLAG,0                                                          
         BE    PRTOTC1                                                          
         ZAP   LASTTOTS,0(6,R2)                                                 
         ZAP   LASTTOTS+8(8),6(6,R2)                                            
         ZAP   LASTTOTS+16(8),12(6,R2)                                          
         MVI   FORMSW,C'A'               SET SWITCHES                           
         OI    ZEROTOTS,X'04'                                                   
         BAS   RE,FORMACC                FORMAT THE TOTALS                      
         CLC   P(110),SPACES                                                    
         BE    PRTOTAL3                                                         
         NI    ZEROTOTS,X'FB'                                                   
         MVC   P+24(10),=C'TOTALS FOR'                                          
         ZIC   R2,LEVEL+1          LOAD IN LENGTH OF A+B+C+D LEVELS             
         MVI   SAVEFLG,1           PUT INTO TABLE                               
         BAS   RE,PRINTIT                                                       
         BAS   RE,REPORT                 PRINT THE LINE                         
PRTOTC1  GOTO1 PROLLER,DMCB,2,TOTALS,4   CLEAR LINE OF TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,3   CLEAR LINE OF TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,2   CLEAR LINE OF TOTALS                   
*                                                                               
PRTOTAL3 TM    TOTFLAG,X'08'             IS 00001000 BIT ON                     
         BNZ   PRTOTALD                  CHECK FOR PRINT W/O CHECK              
         ZIC   R2,LEVEL                  LOAD IN LENGTH OF A LEVEL              
         LA    R2,1(R2)                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   SORTACC2+1(0),TMP3ACCT+1                                         
         BE    PRTOTALX                  NO CHANGE                              
*                                                                               
PRTOTALD GOTO1 PROLLER,DMCB,1,TOTALS,5   GET ADDRESS OF A TOTALS                
         L     R2,0(R1)                  R2=A(A TOTALS)                         
         ZAP   FORMACCS,0(6,R2)          LOAD IN THE DEBITS                     
         ZAP   FORMACCS+8(8),6(6,R2)        LOAD IN THE CREDITS                 
         ZAP   FORMACCS+16(8),12(6,R2)      LOAD IN THE BALANCE                 
         BAS   RE,CMPTOT                                                        
         CLI   XFLAG,0                                                          
         BE    PRTOTD1                                                          
         ZAP   LASTTOTS,0(6,R2)                                                 
         ZAP   LASTTOTS+8(8),6(6,R2)                                            
         ZAP   LASTTOTS+16(8),12(6,R2)                                          
         MVI   FORMSW,C'A'               SET SWITCHES                           
         BAS   RE,FORMACC                FORMAT THE TOTALS                      
         OC    P(110),SPACES             PAD WITH BLANKS                        
         OI    ZEROTOTS,X'08'                                                   
         CLC   P(110),SPACES             IS LINE EMPTY?                         
         BE    PRTOTALX                                                         
         NI    ZEROTOTS,X'F7'                                                   
         MVC   P+24(10),=C'TOTALS FOR'                                          
         ZIC   R2,LEVEL                                                         
         MVI   SAVEFLG,1                                                        
         BAS   RE,PRINTIT                                                       
         BAS   RE,REPORT                 PRINT THE LINE                         
PRTOTD1  GOTO1 PROLLER,DMCB,2,TOTALS,5   CLEAR LINE OF TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,4   CLEAR LINE OF TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,3   CLEAR LINE OF TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,2   CLEAR LINE OF TOTALS                   
*                                                                               
PRTOTALX CLI   ACCTFLG,X'FF'             IS FLAG SET?                           
         BNE   PRTOTALY                  EXIT                                   
         GOTO1 PROLLER,DMCB,1,TOTALS,6   GET ADDRESS OF TOTALS                  
         L     R2,0(R1)                                                         
         ZAP   FORMACCS,0(6,R2)                                                 
         ZAP   FORMACCS+8(8),6(6,R2)                                            
         ZAP   FORMACCS+16(8),12(6,R2)                                          
         MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         OC    P(110),SPACES                                                    
         CLC   P(110),SPACES                                                    
         BE    PRTOTALY                                                         
         LA    R3,P+24               POINT TO START OF PRINT                    
         MVC   0(10,R3),=C'TOTALS FOR'                                          
         LA    R3,P+35                                                          
         MVC   0(14,R3),SAVACCNT+1                                              
         LA    R3,P+35                                                          
         BAS   RE,CONCAT             RETURNS ADDRESS OF FIRST BLANK-R3          
         LA    R1,P+71               PRINT-LINE LIMIT                           
         SR    R1,R3                 FIND LENGTH                                
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SAVNAME                                                  
         BAS   RE,REPORT                                                        
PRTOTX   GOTO1 PROLLER,DMCB,2,TOTALS,6   CLEAR ACCOUNT TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,5   CLEAR LINE OF TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,4   CLEAR LINE OF TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,3   CLEAR LINE OF TOTALS                   
         GOTO1 PROLLER,DMCB,2,TOTALS,2   CLEAR LINE OF TOTALS                   
         MVI   CLEARCNT,X'0F'            CLEAR ALL TOTALS                       
         CLI   CONTRFLG,X'FF'                                                   
         BE    PRTOTALY                                                         
         MVI   LINE,99                  FORCE ONTO NEXT PAGE                    
*                                                                               
PRTOTALY MVC   TMP3ACCT(15),SORTACC2                                            
         ZAP   LASTTOTS(8),=P'0'                                                
         ZAP   LASTTOTS+8(8),=P'0'                                              
         ZAP   LASTTOTS+16(8),=P'0'                                             
         MVI   TOTFLAG,X'F0'                                                    
         CLI   ZEROTOTS,0                                                       
         BE    XIT                                                              
         MVI   LINE,99                                                          
         BAS   RE,REPORT                 PRINT THE LINE                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*------------------------------------------------------------------             
*              PRINTS NAME FOR AN ACCOUNT IN TOTAL FIELD                        
*------------------------------------------------------------------             
*                                                                               
PRINTIT  NTR1                                                                   
         MVC   READKEY,SPACES                                                   
         LA    R2,1(R2)                  ALLLOW FOR U/L LENGTH                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+35(0),TMP3ACCT+1                                               
         LA    R2,1(R2)                  ALLOW FOR U/L LENGTH                   
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   READKEY(0),TMP3ACCT                                              
         BAS   RE,READNAME                                                      
         LA    R3,P+35                                                          
         BAS   RE,CONCAT                                                        
         LA    R1,P+71               PRINT-LINE LIMIT                           
         SR    R1,R3                 FIND LENGTH                                
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              COMPARE TOTALS FOR 3RD SRTFIELD                                  
*------------------------------------------------------------------             
*                                                                               
CMPTOT   NTR1                                                                   
         MVI   XFLAG,X'FF'         SET TO INVALID WHEN STARTING                 
         LA    R1,FORMACCS                                                      
         LA    R2,LASTTOTS                                                      
         LA    R3,3                                                             
CMPTOT1  CP    0(8,R1),0(8,R2)                                                  
         BNE   XIT                                                              
         LA    R1,8(R1)                                                         
         LA    R2,8(R2)                                                         
         BCT   R3,CMPTOT1                                                       
         XC    XFLAG,XFLAG                                                      
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              GET LEDGER STRUCTURE                                             
*------------------------------------------------------------------             
*                                                                               
GETLEV   NTR1                                                                   
         LA    R3,HIERTBLE                                                      
         LA    R1,ULLENQ           NUMBER OF ELEMENTS IN TABLE                  
GETLEV1  CLC   0(2,R3),TMP3ACCT+1                                               
         BNE   *+14                                                             
         MVC   LEVEL(4),2(R3)                                                   
         B     XIT                                                              
         LA    R3,6(R3)                                                         
         BCT   R1,GETLEV1                                                       
*                                                                               
         MVC   READKEY,SPACES      GO READ HIERCHY ELEMENT                      
         MVC   READKEY(3),TMP3ACCT LOAD IN UL                                   
         L     R2,VIOAREA          POINT TO WHERE RECORD WILL GO                
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',READKEY,(R2),0                   
         CLI   8(R1),0             WAS THE READ SUCESSFUL?                      
         BE    *+14                                                             
         MVC   LEVEL(4),=X'0C000000' SET THE DEFAULT LEDGER LENGTH              
         B     XIT                                                              
*                                                                               
         L     R2,VIOAREA                                                       
         AH    R2,DATADISP         R2=A(RECORD)                                 
GETLEV2  CLI   0(R2),0             DID WE HIT END OF RECORD??                   
         BNE   *+14                                                             
         MVC   LEVEL(4),=X'0C000000' ERE MUST BE HIERCHY ELEMENT                
         B     XIT                                                              
         CLI   0(R2),X'16'         CHECK FOR HIERCHY ELEMENT                    
         BE    GETLEV3             WE FOUND THE RIGHT ELEMENT                   
         ZIC   R1,1(R2)            RETRIEVE LENGTH OF ELEMENT                   
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         B     GETLEV2             LOOP BACK UP AGAIN                           
*                                                                               
         USING ACHEIRD,R2                                                       
GETLEV3  MVC   LEVEL,ACHRLEVA                                                   
*======================================================================         
*** SOMEONE FIX THIS: IMPLICIT LENGTH OF 4                                      
*** SHOULD PROBABLY BE AN EXPLICIT LENGTH OF 1                                  
         MVC   LEVEL+1,ACHRLEVB    LOAD IN LENGTH LEVEL A                       
         MVC   LEVEL+2,ACHRLEVC    STORE LENGTH OF LEVEL B                      
         MVC   LEVEL+3,ACHRLEVD    STORE THE LENGTH                             
*======================================================================         
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              READ THE NAME OF AN ACCOUNT                                      
*------------------------------------------------------------------             
*                                                                               
READNAME NTR1                                                                   
         MVC   NEWKEY,SPACES                                                    
         MVC   WORK,SPACES                                                      
         MVC   NEWKEY(15),READKEY    SAVE KEY                                   
         L     R2,VIOAREA            POINT TO THE IO AREA                       
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',READKEY,(R2),0                   
         CLC   NEWKEY(15),READKEY                                               
         BNE   XIT                                                              
         L     R1,VIOAREA            POINT TO RECORD READ                       
         BAS   RE,GETNAME            RETURNS NAME OF ACCT IN 'WORK'             
         OC    WORK,SPACES           PAD WITH BLANKS                            
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              PRINT CONTRA-ACCOUNT SUMMARY                                     
*------------------------------------------------------------------             
*                                                                               
PRNTSUM  NTR1                                                                   
         CLI   QOPT1,C' '                                                       
         BE    PRNTSUMA            NO SUMMARIES, EXIT INSTANTER.                
         CLI   SUBMODE,RUNLAST                                                  
         BE    XIT                 REQUEST TOTS PRINTED ALREADY.                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2          SET SUB-PROGRAM                              
         CLI   PRTSW,C'N'                                                       
         BE    *+8                                                              
         MVI   RCSUBPRG,3                                                       
         ZIC   R0,SAVELVL          R0=1 FOR ACCOUNT, 2 FOR LEDGER               
         XC    BUFFKEY,BUFFKEY                                                  
         MVI   BUFFTYPE,1                                                       
         ZAP   TOTDR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
PRNTSUM1 GOTO1 BUFFALO,DMCB,=C'HIGH',VBUFF,BUFFREC,(R0)                         
         B     PRNTSUM4                                                         
PRNTSUM2 GOTO1 BUFFALO,DMCB,=C'SEQ',VBUFF,BUFFREC,(R0)                          
*                                                                               
PRNTSUM4 CLI   8(R1),0                                                          
         BNE   PRNTSUM6                                                         
         CLI   BUFFTYPE,1                                                       
         BNE   PRNTSUM6                                                         
         CP    BUFFDR,=P'0'        CHECK FOR ACTIVITY                           
         BNE   *+14                                                             
         CP    BUFFCR,=P'0'                                                     
         BE    PRNTSUM2                                                         
         AP    TOTDR,BUFFDR                                                     
         AP    TOTCR,BUFFCR                                                     
*                                  FORMAT LINE & PRINT                          
PRNTSUM5 ZAP   FORMACCS,BUFFDR                                                  
         ZAP   FORMACCS+8(8),BUFFCR                                             
         MVC   P+1(14),BUFFCODE+1                                               
         MVC   P+17(36),BUFFNAME                                                
         MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         BAS   RE,REPORT                                                        
         B     PRNTSUM2                                                         
PRNTSUM6 ZAP   FORMACCS,TOTDR                                                   
         ZAP   FORMACCS+8(8),TOTCR                                              
         CLC   FORMACCS(16),=4PL8'0'                                            
         BE    PRNTSUM8            NOTHING TO PRINT.                            
         BAS   RE,REPORT                                                        
         MVC   P+50(12),=C'** TOTALS **'                                        
*                                                                               
         CLI   SUBMODE,REQLAST                                                  
         BNE   PRNTSUMQ            NO REQ TOTS IF NOT EOF ON BUFFALO.           
         CLI   SAVELVL,4                                                        
         BE    PRNTSUM7            REQLAST CALL, PRINT REQ TOTS.                
         CP    ACCCNTR,=P'1'       NOT YET REQLAST.                             
         BH    PRNTSUMQ            REQLAST CALL WILL PRINT SUMMARY.             
         CLI   SAVELVL,3           NO NEED FOR REQLAST CALL.                    
         BE    PRNTSUM7            UNIT LAST = REQLAST                          
         CP    ACCCNTU,=P'1'       NOT YET UNIT LAST.                           
         BH    PRNTSUMQ            UNITLAST CALL WILL PRINT SUMMARY.            
PRNTSUM7 MVC   P+42(20),=C'** REQUEST TOTALS **'                                
         MVI   SUBMODE,RUNLAST     MARK REQ TOTS AS PRINTED.                    
PRNTSUMQ MVI   FORMSW,C'A'                                                      
         BAS   RE,FORMACC                                                       
         BAS   RE,REPORT                                                        
PRNTSUM8 MVC   RCSUBPRG,SUBPROG    RESET SUB PROGRAM                            
PRNTSUMA CLI   LEDGPROF+1,C'Y'                                                  
         BE    XIT                 PAGE THROW ON ACCT BREAK.                    
         MVI   FORCEHED,C'Y'       NO PAGE THROW, FORCE ONE HERE.               
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              CLEAR BUFFER (BUFFALO)                                           
*------------------------------------------------------------------             
*                                                                               
CLEARBUF NTR1                                                                   
         ZIC   R0,SAVELVL                                                       
         GOTO1 BUFFALO,DMCB,=C'CLEAR',VBUFF,(X'80',(R0))                        
         B     XIT                                                              
         SPACE 3                                                                
*------------------------------------------------------------------             
*              ROUTINE TO EXTRACT NAME FROM A RECORD                            
*------------------------------------------------------------------             
*                                                                               
GETNAME  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         AH    R1,DATADISP         R1=A(RECORD)                                 
         SR    RE,RE                                                            
GETNAME2 CLI   0(R1),0                                                          
         BE    GETNAME4                                                         
         CLI   0(R1),X'20'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     GETNAME2                                                         
         IC    RE,1(R1)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),2(R1)       WORK NOW CONTAINS NAME                       
GETNAME4 B     XIT                                                              
         SPACE 3                                                                
*------------------------------------------------------------------             
*              ROUTINE TO LOOK-UP OFFICE NAME IN BUFFER                         
*------------------------------------------------------------------             
*                                                                               
GETOFFC  NTR1                                                                   
         L     R1,VOFFBUFF                                                      
         MVC   WORK,SPACES                                                      
         MVC   WORK(15),=C'**NOT ON FILE**'                                     
GETOFFC2 CLI   0(R1),X'FF'                                                      
         BE    GETOFFC4                                                         
         CLC   0(1,R1),THISANAL                                                 
         BE    *+12                                                             
         LA    R1,37(R1)                                                        
         B     GETOFFC2                                                         
         MVC   WORK(36),1(R1)                                                   
GETOFFC4 B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              REMOVE SPACES BTWN ACCT NAME AND ACCT NUMBER                     
*------------------------------------------------------------------             
*                                                                               
*        R3 MUST POINT TO START OF AREA WHERE SEARCH FOR A BLANK                
*        IS TO TAKE PLACE.                                                      
*                                                                               
CONCAT   CLC   0(3,R3),SPACES      IS THIS CHARACTER A BLANK?                   
         BE    *+12                BUMP TO NEXT CHAR                            
         LA    R3,1(R3)                                                         
         B     *-14                LOOP BACK UP FOR NEXT COMPARE                
         LA    R3,1(R3)            LEAVE A BLANK BETWEEN ACCTS                  
         BR    RE                  RETURN TO CALLER                             
*                                                                               
*------------------------------------------------------------------             
*              ROUTINE TO LOOK-UP ANALYSIS CODE IN LEDGER                       
*------------------------------------------------------------------             
*                                                                               
GETANAL  NTR1                                                                   
         MVC   THISANL1,SPACES                                                  
         MVC   THISANL2,SPACES                                                  
         CLC   THISANAL,=C'99'                                                  
         BNE   *+14                                                             
         MVC   THISANL1(14),=C'PREVIOUS BILLS'                                  
         B     GETAN8                                                           
         MVC   THISANL1(10),=C'WORK CODE='                                      
         MVC   THISANL1+10(2),THISANAL                                          
         L     R2,ADLEDGER                                                      
         AH    R2,DATADISP                                                      
         SR    R1,R1                                                            
GETAN2   CLI   0(R2),0                                                          
         BE    GETAN8                                                           
         CLI   0(R2),X'12'                                                      
         BE    GETAN6                                                           
GETAN4   IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETAN2                                                           
         USING ACANALD,R2                                                       
GETAN6   MVC   WORK(2),ACANCODE                                                 
         OC    WORK(2),SPACES                                                   
         CLC   WORK(2),THISANAL                                                 
         BNE   GETAN4                                                           
         MVC   THISANL1(L'ACANDESC),ACANDESC                                    
*                                  UNDERLINE IN THISANL2                        
GETAN8   MVI   THISANL2,C'-'                                                    
         MVC   THISANL2+1(L'THISANL2-1),THISANL2                                
         LA    R1,THISANL2+L'THISANL2-1                                         
         LA    RE,THISANL1+L'THISANL1-1                                         
         CLI   0(RE),C' '                                                       
         BNE   *+14                                                             
         MVI   0(R1),C' '                                                       
         BCTR  RE,0                                                             
         BCT   R1,*-14                                                          
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              CLEAR DOWN TRANSACTION PRINT BLOCK                               
*------------------------------------------------------------------             
*                                                                               
CLEARBLK NTR1                                                                   
         LA    R1,PRNTBLOC                                                      
         LA    RE,20                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   RE,*-10                                                          
         B     XIT                                                              
*                                                                               
*------------------------------------------------------------------             
*              ROUTINE TO PRINT A LINE OF REPORT                                
*------------------------------------------------------------------             
*                                                                               
REPORT   NTR1                                                                   
         MVC   HEAD5+1(L'THISHED1),THISHED1                                     
         MVC   HEAD6+1(L'THISHED1),THISHED2                                     
         MVC   HEAD8+60(L'THISHED4),THISHED5                                    
         MVC   HEAD4+85(13),=C'REQUESTED ON-'                                   
         CLI   QOPT1,C'N'                                                       
         BE    REP020                                                           
         MVC   HEAD7+1(L'THISHED6),THISHED6                                     
*        MVC   HEAD7+85(L'THISHEDM),THISHEDM                                    
         MVC   HEAD8+1(L'THISHED7),THISHED7                                     
REP020   CLI   XFLAG,X'CC'                                                      
         BNE   REP030                                                           
         MVC   HEAD4(80),SPACES                                                 
         MVC   HEAD5(110),SPACES                                                
         MVC   HEAD6(110),SPACES                                                
         MVC   HEAD7(80),SPACES                                                 
         MVC   HEAD8(80),SPACES                                                 
         MVC   HEAD9(110),SPACES                                                
REP030   GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              ROUTINE TO FORMAT ACCUMULATORS                                   
*------------------------------------------------------------------             
*                                                                               
FORMACC  NTR1                                                                   
         CLI   PRTSW,C'N'                                                       
         BNE   FORMACCA                                                         
*                                  NORMAL EDITS                                 
*                                  BALANCE (B/F)                                
         CLI   FORMSW,C'B'                                                      
         BNE   FORMACC2                                                         
         ZAP   DUB,FORMACCS+24(8)                                               
         LA    R2,P+97                                                          
         BAS   RE,EDIT1                                                         
         B     FORMACCX                                                         
*                                  AMOUNT (DR AND/OR CR)                        
FORMACC2 CLI   FORMSW,C'C'                                                      
         BE    FORMACC8                                                         
         CP    FORMACCS,=P'0'                                                   
         BE    FORMACC4                                                         
         ZAP   DUB,FORMACCS                                                     
         LA    R2,P+72                                                          
         BAS   RE,EDIT2                                                         
FORMACC4 CP    FORMACCS+8(8),=P'0'                                              
         BE    FORMACC6                                                         
         ZAP   DUB,FORMACCS+8(8)                                                
         LA    R2,P+84                                                          
         BAS   RE,EDIT3                                                         
*                                  DIFFERENCE                                   
FORMACC6 CLI   FORMSW,C'D'                                                      
         BE    FORMACCX                                                         
         ZAP   DUB,FORMACCS                                                     
         SP    DUB,FORMACCS+8(8)                                                
         CP    DUB,=P'0'                                                        
         BE    FORMACCX                                                         
         LA    R2,P+97                                                          
         BAS   RE,EDIT1                                                         
         B     FORMACCX                                                         
*                                  BALANCE C/F                                  
FORMACC8 ZAP   DUB,FORMACCS+24(8)                                               
         AP    DUB,FORMACCS                                                     
         SP    DUB,FORMACCS+8(8)                                                
         LA    R2,P+97                                                          
         CP    DUB,=P'0'                                                        
         BNE   *+14                                                             
         MVC   P+106(3),=C'NIL'                                                 
         B     FORMACCX                                                         
         BAS   RE,EDIT1                                                         
         B     FORMACCX                                                         
*                                  SPECIAL EDITS                                
*                                  BALANCE (B/F)                                
FORMACCA CLI   FORMSW,C'B'                                                      
         BNE   FORMACCD                                                         
         ZAP   DUB,FORMACCS+24(8)                                               
         CLI   PRTSW,C'+'                                                       
         BE    *+10                                                             
         MP    DUB,=P'-1'                                                       
         LA    R2,P+97                                                          
         BAS   RE,EDIT2                                                         
         B     FORMACCX                                                         
*                                  AMOUNT/DIFFERENCE                            
FORMACCD CLI   FORMSW,C'C'                                                      
         BE    FORMACCF                                                         
         ZAP   DUB,FORMACCS                                                     
         SP    DUB,FORMACCS+8(8)                                                
         CLI   PRTSW,C'+'                                                       
         BE    *+10                                                             
         MP    DUB,=P'-1'                                                       
         ZAP   DOUBLE,DUB                                                       
         LA    R2,P+84                                                          
         BAS   RE,EDIT2                                                         
         CLI   FORMSW,C'D'                                                      
         BE    FORMACCX                                                         
         ZAP   DOUBLE,DUB                                                       
         LA    R2,P+97                                                          
         BAS   RE,EDIT2                                                         
         B     FORMACCX                                                         
*                                  BALANCE C/F                                  
FORMACCF ZAP   DUB,FORMACCS+24(8)                                               
         AP    DUB,FORMACCS                                                     
         SP    DUB,FORMACCS+8(8)                                                
         CLI   PRTSW,C'+'                                                       
         BE    *+10                                                             
         MP    DUB,=P'-1'                                                       
         CP    DUB,=P'0'                                                        
         BNE   *+14                                                             
         MVC   P+106(3),=C'NIL'                                                 
         B     FORMACCX                                                         
         LA    R2,P+97                                                          
         BAS   RE,EDIT2                                                         
FORMACCX B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------               
*              EDITING AIDS                                                     
*----------------------------------------------------------------               
*                                                                               
EDIT1    NTR1                                                                   
         ZAP   THISNUM,DUB                                                      
         EDIT  (P8,THISNUM),(14,THISWORK),2,CR=YES,ZERO=BLANK                   
         B     EDITOUT                                                          
*                                                                               
EDIT2    NTR1                                                                   
         ZAP   THISNUM,DUB                                                      
         EDIT  (P8,THISNUM),(13,THISWORK),2,MINUS=YES,ZERO=BLANK                
         MVI   THISWORK+13,C' '                                                 
         B     EDITOUT                                                          
*                                                                               
EDIT3    NTR1                                                                   
         ZAP   THISNUM,DUB                                                      
         EDIT  (P8,THISNUM),(13,THISWORK),2,MINUS=YES,ZERO=BLANK                
         MVI   THISWORK+13,C' '                                                 
         CLI   THISWORK+12,C'-'                                                 
         BNE   *+8                                                              
         MVI   THISWORK+12,C'+'                                                 
         B     EDITOUT                                                          
*                                                                               
EDITOUT  CLC   THISWORK,SPACES                                                  
         BE    EDITX                                                            
         BCTR  R2,0                R2=A(OUTPUT-1)                               
         LA    R1,THISWORK         R1=A(NUMBER)                                 
         LA    RE,13               RE=L'NUMBER-1                                
*                                                                               
         CLI   0(R1),C' '          FIND FIRST DIGIT POSITION                    
         BNE   *+16                                                             
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   RE,*-16                                                          
         LA    RE,1(RE)            SET LNTH TO INCLUDE PREVIOUS BYTE.           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SPACES      WILL THE NUMBER FIT                          
         BE    *+8                                                              
         LA    R2,132(R2)          NO - BUMP TO NEXT LINE                       
         BCTR  RE,0                RESET LNTH FOR MVC.                          
         EX    RE,*+8                                                           
         B     EDITX                                                            
         MVC   1(0,R2),0(R1)       MOVE NUMBER TO OUTPUT LINE                   
EDITX    B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              LITERALS ETC.                                                    
*------------------------------------------------------------------             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------------------------             
*              TAPE  OUTPUT ROUTINE FOR O&M                                     
*------------------------------------------------------------------             
*                                                                               
         SPACE 1                                                                
WRTAPE   DS    0D                                                               
         NMOD1 0,**TAPE**                                                       
         USING ACWORKD,R8                                                       
         L     RC,=A(WORKC)                                                     
         USING WORKD,RC                                                         
         LA    RC,MYEND                                                         
         USING WORK1D,RC                                                        
         LA    R7,TAPEIO                                                        
         USING TAPD,R7                                                          
         USING SORTRECD,R6                                                      
         MVI   TAPREC,C' '                                                      
         MVC   TAPREC+1(L'TAPREC-1),TAPREC                                      
         MVC   TAPACCT,SORTACC+1   ACCOUNT                                      
         MVC   TAPACCN,THISSBNM    ACCOUNT NAME                                 
         MVC   TAPCACCT,SORTSBAC+1 CONTRA                                       
         MVC   TAPCACCN,THISNAME   CONTRA NAME                                  
         MVC   TAPOFFIC,SORTOFFC   OFFICE                                       
         MVC   TAPBATCH,SORTMOS    BATCH REFERENCE                              
         GOTO1 DATCON,DMCB,(1,SORTDATE),(0,TAPDATE)                             
         MVC   TAPREF,SORTREF      ITEM REFERENCE                               
         MVC   TAPWORK,SORTANAL    WORK CODE                                    
         ZAP   DUB,SORTAMNT        AMOUNT                                       
         TM    SORTSTAT,X'80'                                                   
         BO    *+10                                                             
         MP    DUB,=P'-1'          REVERSE SIGN FOR CREDITS                     
         UNPK  TAPAMNT,DUB                                                      
         PUT   TAPE,TAPEIO                                                      
         XIT1                                                                   
         SPACE 2                                                                
TAPE     DCB   DSORG=PS,MACRF=PM,DDNAME=TAPE,BLKSIZE=1500,LRECL=150             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------------------------             
*              STORAGE AREAS USED                                               
*------------------------------------------------------------------             
*                                                                               
         BUFF  LINES=700,ROWS=4,COLUMNS=2,COMMENT=36,FLAVOR=PACKED,    *        
               KEYLIST=(17,A)                                                   
SORTREC  DS    0D                                                               
         DS    1000C                                                            
*                                                                               
SAVESORT DS    0D                                                               
         DS    CL1000                                                           
*                                                                               
WORKC    DS    0D                                                               
         DS    10000C                                                           
*                                                                               
OFFBUFF  DS    0D                                                               
         DS    2000C                                                            
*                                                                               
IOAREA   DS    0D                                                               
         DS    2000C                                                            
*                                                                               
MAXNMBR  EQU   2000              MAXIMUM NUMBER OF NAMES SAVED IN TBLE          
NAMETBL  DS    0D                TABLE CONTAINS ACCTS AND THEIR NAMES           
         DS    102051C           FOR HIGH LEVEL ACCTS. USES'NAMEELEM'           
*                                                                               
SORTAREA DS    0D                                                               
         DS    10C                                                              
         EJECT                                                                  
*------------------------------------------------------------------             
*              DSECT TO COVER SAVE W/S                                          
*------------------------------------------------------------------             
*                                                                               
WORK1D   DSECT                                                                  
VTYPES   DS    0A                                                               
VBUFF    DS    A                                                                
SORTAR   DS    A                                                                
SORTER   DS    A                                                                
VSQUASH  DS    A                                                                
VACCEDIT DS    A                                                                
VCHOPCON DS    A                                                                
VOFFBUFF DS    A                                                                
VACLIST  DS    A                                                                
VIOAREA  DS    A                                                                
VNAMETBL DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
WORK1ST  DS    0C                                                               
SWITCHES DS    0CL16               ACTIVITY SWITCHES                            
READSW   DS    C                                                                
OFFSW    DS    C                                                                
FLTSW    DS    C                                                                
SORTSW   DS    C                                                                
PRTSW    DS    C                                                                
ACCSW    DS    C                                                                
ANALSW   DS    C                                                                
SBACSW   DS    C                                                                
TRNSSW   DS    C                                                                
FORMSW   DS    C                                                                
SUBMODE  DS    C                                                                
SAVESW   DS    C                                                                
         DS    CL5                                                              
*                                                                               
ACCCNT   DS    PL4                                                              
ACCCNTU  DS    PL4                                                              
ACCCNTR  DS    PL4                                                              
ACCUNM   DS    PL6                                                              
LDGCNT   DS    PL2                                                              
UNTCNT   DS    PL2                                                              
TRNSCNT  DS    PL4                                                              
ANALCNT  DS    PL4                                                              
*                                                                               
READKEY  DS    CL49                                                             
NEWKEY   DS    CL49                                                             
UNITLEDG DS    CL2                 CONTAINS UNIT/LEDGER ANALYSIS FIELD          
XFLAG    DS    XL1                 FLAG FOR LEVEL CHECK CODE                    
LEDGPROF DS    CL16                                                             
PSTART   DS    PL3                 PACKED START DATE (OR ZEROES)                
PEND     DS    PL3                 PACKED END DATE (OR X'FF')                   
TODAY3   DS    CL3                                                              
TODAY2   DS    CL2                                                              
LDGBLFWD DS    PL6                                                              
INACTOT  DS    PL6                                                              
TOTDR    DS    PL6                                                              
TOTCR    DS    PL6                                                              
SUBPROG  DS    X                                                                
SAVEMODE DS    X                                                                
SAVELVL  DS    X                                                                
TAPECNT  DS    X                                                                
TAPEIO   DS    CL(TAPLNQ)                                                       
DSNAME   DS    CL20                                                             
FORMACCS DS    4PL8                                                             
LASTACCS DS    CL16                                                             
LASTTOTS DS    4PL8                                                             
*                                  THIS TIME SAVE VALUES                        
THATACC  DS    CL15                                                             
THISACCT DS    CL20                                                             
THISNAME DS    CL36                                                             
THISANAL DS    CL2                                                              
THISANL1 DS    CL15                                                             
THISANL2 DS    CL15                                                             
THISSBAC DS    CL15                                                             
THISSBNM DS    CL36                                                             
THISACC2 DS    CL15                                                             
THISHEDM DS    CL31                                                             
THISHEDA DS    CL30                                                             
THISHED1 DS    CL109                                                            
THISHED2 DS    CL109                                                            
THISHED3 DS    CL109                                                            
THISHED4 DS    CL50                                                             
THISHED5 DS    CL64                                                             
THISHED6 DS    CL50                                                             
THISHED7 DS    CL50                                                             
EXTRA    DS    CL20                SPARE                                        
THATKEY  DS    CL42                                                             
THISNUM  DS    PL8                                                              
THISWORK DS    CL14                                                             
*                                  BUFFER RECORD AREA                           
BUFFREC  DS    0CL69                                                            
BUFFKEY  DS    0CL17                                                            
BUFFTYPE DS    X                                                                
BUFFCODE DS    CL15                                                             
BUFFLVL  DS    X                                                                
BUFFNAME DS    CL36                                                             
BUFFDR   DS    PL8                                                              
BUFFCR   DS    PL8                                                              
         DS    (3*2)PL8            SPARE FOR READ OF ALL ROWS.                  
*                                  CHOPCON W/S                                  
CHOPSAVE DS    CL256                                                            
CHOPSAV2 DS    CL256                                                            
CHOPBLOC DS    CL256                                                            
LEDGFILT DS    CL250                                                            
CHOPNUM  DS    X                                                                
*                                                                               
SAVESBAC DS    5CL22                                                            
SAVEASRT DS    A                                                                
ELEMCNTR DS    H                                                                
*                                                                               
ACCUMS   DS    CL172               8X3 6BYTE PACKED ACCUMS                      
*                                                                               
LEV1ACCT DS    CL15                TEMPORARY STORAGE FOR ACCOUNT                
LEV2ACCT DS    CL15                TEMPORARY STORAGE FOR ACCOUNT                
LEV3ACCT DS    CL15                TEMPORARY STORAGE 3RD SRTFLD                 
TMP2ACCT DS    CL15                                                             
TMP3ACCT DS    CL15                                                             
TEMPCODE DS    CL1                 TEMPORARY VARIABLE                           
CONTRFLG DS    XL1                 CONTRA ACCOUNT FLAG TO PRINT TOTALS          
ANALTBLE DS    0D                  TABLE HOLDING ANALYSIS ELEMENTS              
         DS    11CL15              MAXIMUM NUMBER TABLE ENTRIES = 11            
ANALTLEN EQU   *-ANALTBLE          TRUE LENGTH OF TABLE                         
ANLNTH   EQU   15                  LENGTH OF EACH TABLE ENTRY                   
         DS    XL1                 SPACE FOR END OF TABLE MARKER                
ANFLAG   DS    XL1                 FLAGS WHETHER TO USE 'C0' ELEM               
TOTFLAG  DS    XL1                 INDICATES TO PRINT TOTALS                    
ACCTFLG  DS    XL1                 INDICATES TO PRINT TOTALS                    
SAVEFLG  DS    XL1                 SAVE ELEMENT IN TABLE?                       
LEVEL    DS    CL4                 HOLD LENGTHS OF HIERCHY 3RD SRTFLD           
ZEROTOTS DS    XL1                                                              
SPARE    DS    CL15                                                             
SAVACCNT DS    CL15                SAVE THE ACCOUNT NUMBER                      
SAVNAME  DS    CL40                SAVE ACCOUNT NAME                            
SAV2ACCT DS    CL15                SAVE THE ACCOUNT NUMBER                      
SAV2NAME DS    CL36                SAVE THE NAME                                
CLEARCNT DS    XL1                 FLAG TO CLEAR TOTAL COUNTERS                 
OTHERTOT DS    CL44                "OTHER" TOTAL ACCUMULATOR                    
TOTALS   DS    CL155               3RD SRTFLD ACCUMULATORS                      
HIERTBLE DS    0F                                                               
         DS    12CL6               HOLDS U/L LEVEL LENGTHS                      
*                                                                               
PRNTBLOC DS    20CL132                                                          
WRK1LNQ  EQU   *-WORK1ST                                                        
         EJECT                                                                  
*------------------------------------------------------------------             
*              DSECT TO COVER TRANSACTION SORT RECORD                           
*------------------------------------------------------------------             
*                                                                               
SORTRECD DSECT                                                                  
SORTRLEN DS    H                                                                
         DS    H                                                                
SORTKEY  DS    0CL66                                                            
SORTSBAC DS    CL15                                                             
SORTANAL DS    CL2                                                              
SORTACC  DS    CL15                                                             
SORTDATE DS    CL3                                                              
SORTREF  DS    CL6                                                              
SORTMOS  DS    CL6                                                              
SORTACC2 DS    CL15                                                             
SORTDATA DS    0C                                                               
SORTTYPE DS    XL1                                                              
SORTSTAT DS    XL1                                                              
SORTOFFC DS    CL1                                                              
SORTAMNT DS    PL6                                                              
SORTLEN  EQU   *-SORTRECD                                                       
SORTNARR DS    0C                                                               
         EJECT                                                                  
*------------------------------------------------------------------             
*              DSECT TO COVER WORK AREA                                         
*------------------------------------------------------------------             
*                                                                               
WORKD    DSECT                                                                  
PARM     DS    0F                                                               
PARA1    DS    F                                                                
PARA2    DS    F                                                                
PARA3    DS    F                                                                
PARA4    DS    F                                                                
PARA5    DS    F                                                                
PARA6    DS    F                                                                
*                                                                               
ALSTLVL  DS    A                                                                
ACBLVL   DS    A                                                                
AARECORD DS    A                                                                
*                                                                               
KEYTAB   DS    CL(9*KEYTBLNQ+1)                                                 
         ORG   KEYTAB+(2*KEYTBLNQ)                                              
KEYTBAC1 DS    CL8                                                              
         ORG                                                                    
*                                                                               
INMODE   DS    C                                                                
SAVEKEY  DS    0CL(L'SORTKEY)                                                   
SAVSBAC  DS    CL(L'SORTSBAC)                                                   
SAVANAL  DS    CL(L'SORTANAL)                                                   
SAVACCT  DS    CL(L'SORTACC)                                                    
SAVDATE  DS    CL(L'SORTDATE)                                                   
SAVREF   DS    CL(L'SORTREF)                                                    
SAVMOS   DS    CL(L'SORTMOS)                                                    
SAVACCT2 DS    CL(L'SORTACC2)                                                   
*                                                                               
WORKEY   DS    CL66                                                             
*                                                                               
*                                                                               
MYEND    DS    0C                                                               
         EJECT                                                                  
*------------------------------------------------------------------             
*              DSECT FOR OPTIONAL TAPE OUTPUT(O&M)                              
*------------------------------------------------------------------             
*                                                                               
TAPD     DSECT                                                                  
TAPREC   DS    0CL(TAPLNQ)                                                      
TAPACCT  DS    CL14                ACCOUNT CODE                                 
TAPACCN  DS    CL36                ACCOUNT NAME                                 
TAPCACCT DS    CL14                CONTRA-ACCOUNT CODE                          
TAPCACCN DS    CL36                CONTRA-ACCOUNT NAME                          
TAPOFFIC DS    CL1                 OFFICE CODE                                  
TAPBATCH DS    CL6                 BATCH REFERENCE                              
TAPDATE  DS    CL6                 DATE (YYMMDD)                                
TAPREF   DS    CL6                 ITEM REFERENCE                               
TAPWORK  DS    CL2                 WORK CODE                                    
TAPAMNT  DS    CL10                AMOUNT                                       
         DS    CL19                SPARE                                        
TAPLNQ   EQU   *-TAPACCT                                                        
         EJECT                                                                  
*------------------------------------------------------------------             
*              OTHER DSECTS USED                                                
*------------------------------------------------------------------             
*                                                                               
POSTABD  DSECT                                                                  
PSTLCOD  DS    AL1                                                              
PSTAEL   DS    AL2                                                              
PSTBLNQ  EQU   *-POSTABD                                                        
*                                                                               
KEYTABD  DSECT                                                                  
KEYTYP   DS    AL1                                                              
KEYLEN   DS    AL1                                                              
KEYDISP  DS    AL1                                                              
KEYFRST  DS    AL1                                                              
KEYPROC  DS    AL1                                                              
KEYLAST  DS    AL1                                                              
KEYAREC  DS    AL2                                                              
KEYTBLNQ EQU   *-KEYTABD                                                        
         EJECT                                                                  
*                                                                               
NAMEELEM DSECT                                                                  
NAMEACCT DS    CL15                                                             
NAMEDESC DS    CL36                DESCRIPTION OF ACCOUNT                       
NAMEELMQ EQU   *-NAMEELEM                                                       
*                                                                               
         EJECT                                                                  
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREP3802 04/15/16'                                      
         END                                                                    
