*          DATA SET ACREPTT02S AT LEVEL 110 AS OF 03/10/03                      
*PHASE ACTT02A,*                                                                
*INCLUDE SORTER                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE CONVMOS                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE ADSCAN                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'ACTT - 1099 TAPE/FORMS GENERATOR FOR ACCT'                      
***********************************************************************         
*              OPT1:'N' OR ' '= TEST RUN (WITH REGISTER AT END).      *         
*                   'Y'= USE LIVE FORMS (NO REGISTER).                *         
*                   'R'= TEST RUN BUT SUPRESS FORMS PRINTING.         *         
*              OPT2:'Y'= SUPPRESS AMOUNTS < $600                      *         
*              OPT3:'C'= CORRECTION RQST. (PRODUCE 'G' RECORDS)       *         
*                   'T'= TEST RUN.                                    *         
*                   'R'= REPLACEMENT RUN.                             *         
*                   ' '= REGULAR RUN.                                 *         
*              OPT4:'R'= PRODUCE REGISTER ONLY. (READ A TAPE)         *         
*                   'T'= PRODUCE TAPE.     (1ST PASS)                 *         
*              OPT6:' '= DDS IS TRANSMITTER FOR THE TAPE.             *         
*                   'C'= COMPANY IS ITS OWN TRANSMITTER.              *         
*              QSEL:   = REPLACEMENT ALPHA REQUIRED FOR ALL REP TAPES *         
***********************************************************************         
         SPACE 1                                                                
ACTT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACTT**,R8,R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACTTD,RC                                                         
         LA    R9,SRECA                                                         
         USING SRECD,R9                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                INITIALIZATIONS FOR THE RUN.                 
         CLI   MODE,REQFRST                                                     
         BE    REQF                LOOKUP COMPANY INFO                          
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                LOOKUP LEDGER ELEMENT                        
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,ACCLAST                                                     
         BE    OTHRLDGR                                                         
*                                                                               
         CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         CLI   FOUNDIT,C'Y'                                                     
         BE    REQL                                                             
         MVC   P(40),=C'*** AGENCY NOT SET UP FOR 1099 FORMS ***'               
         GOTO1 ACREPORT                                                         
         MVC   P(40),=C'**** PLEASE CONTACT CLIENT SERVICE *****'               
         GOTO1 ACREPORT                                                         
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         L     R2,=A(REGHEAD)      SET UP HEADLINE ADDRESSABILTY                
         ST    R2,HEADHOOK                                                      
         L     R2,=A(HDRC)                                                      
         ST    RC,0(R2)                                                         
*                                                                               
         L     RE,ACPYTAB                                                       
         LA    RF,CTLENQ                                                        
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0               CLEAR CPYTAB TO SPACES                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   STATUS,0                                                         
         MVI   DWNSTAT,0                                                        
         L     R1,AINVTBL                                                       
         ST    R1,AINVENT                                                       
         ZAP   INVCNT,=P'0'                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,SVSDAT)                                
         GOTO1 DATCON,DMCB,(0,QEND),(1,SVEDAT)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(20,SVCDTE)                                 
         MVC   D1YEAR,SVCDTE                                                    
         MVI   SKIPACCT,C'N'                                                    
         MVI   FCRDACC,C'Y'                                                     
         MVI   FCRDTRNS,C'Y'                                                    
         MVI   FCRDHIST,C'Y'                                                    
*                                                                               
         GOTO1 ABLDCPY,DMCB,(RC)   BUILD COMPANY TABLE                          
*                                                                               
         CLI   QOPT4,C'R'          IF REGISTER FROM INPUT TAPE THEN             
         BNE   REQF10              GO DIRECTLY TO PRINT REGISTER.               
         BAS   RE,DOREG                                                         
         B     EXIT                                                             
*                                                                               
         USING CPYELD,R2                                                        
REQF10   L     R2,ADCMPEL          THE COMPANY ELEMENT                          
         CLI   0(R2),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    OFFSTAT,OFFSTAT     CLEAR OFFICE STATUS FIELD                    
         TM    CPYSTAT4,CPYSOFF2   TEST IF NEW OFFICE IS IN EFFECT              
         BNO   *+8                                                              
         MVI   OFFSTAT,STASOFF2    SET NEW OFFICE STATUS                        
         MVC   PRINCID,CPYUID      PRINCIPLE ID NUMBER                          
         MVI   READIT,C'T'         THESE AGENCIES READ TRANSACTIONS             
*                                                                               
         MVI   FOUNDIT,C'Y'        MARK HARDCODE AS 'FOUND'                     
         CLC   ALPHAID,=C'SS'                                                   
         BE    REQFX                ****HARDCODED FOR NOW********               
*                                                                               
         USING CPYTABD,R3                                                       
REQF30   L     R3,ACPYTAB          COMPANY TABLE                                
*                                                                               
         MVI   FOUNDIT,C'N'                                                     
REQF40   CLC   CALPHA,=C'**'       END OF TABLE                                 
         BE    REQF110                                                          
         CLC   CALPHA,ALPHAID      MATCH ON AGENCY ALPHA                        
         BNE   REQF50                                                           
         OC    CORIGIN,CORIGIN                                                  
         BZ    REQF60                                                           
         CLC   CORIGIN,ORIGINUM                                                 
         BE    REQF60                                                           
REQF50   LA    R3,CTLENQ(R3)       NEXT TABLE ENTRY                             
         B     REQF40                                                           
*                                                                               
REQF60   MVI   FOUNDIT,C'Y'        FOUND AGENCY IN TABLE                        
         TM    CSTATUS,CSTDWNL     COMPANY DOWNLOADING?                         
         BNO   *+8                                                              
         OI    DWNSTAT,DWNCMPY                                                  
*                                                                               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   SVOFFL,ACMROFL      SAVE OFF OFFICE LIST FROM MONACC             
         DROP  RF                                                               
*                                                                               
         XC    SVLIM,SVLIM         INITIALIZE LIMITED ACCESS TO ZEROES          
         USING CTIREC,R2                                                        
         LA    R2,SVKEY            R2=A(CONTROL ID RECORD)                      
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,PRINCID     PRINCIPLE ID                                 
         OC    CORIGIN,CORIGIN     IS THERE ID SPECIFIED IN TABLE?              
         BZ    *+10                                                             
         MVC   CTIKNUM,CORIGIN     YES - USE THAT INSTEAD                       
*                                                                               
         GOTO1 =A(DMCTFIL),DMCB,(RC)   READ HIGH                                
         CLC   IOKEY(CT5LEN-CT5KEY),SVKEY                                       
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         LA    R2,IO                                                            
*                                                                               
         LA    R1,CTIDATA                                                       
         SR    R0,R0                                                            
REQF70   CLI   0(R1),0             THE END                                      
         BE    REQF110                                                          
         USING CTAGYD,R1                                                        
         CLI   0(R1),X'06'         AGENCY ALPHA ELEMENT                         
         BNE   REQF80                                                           
         CLC   CTAGYID,ALPHAID     MAKE SURE THIS IS RIGHT AGENCY               
         BE    REQF100             YES - READ NEXT EL                           
         DC    H'0'                                                             
*                                                                               
         USING CTSYSD,R1                                                        
REQF80   DS    0H                                                               
         CLI   0(R1),X'21'         SYSTEM AUTH ELEMENT                          
         BNE   REQF90                                                           
         CLI   CTSYSNUM,X'06'      X'06' IS ACCOUNTING                          
         BNE   REQF100             NO - GET NEXTEL                              
         CLI   CTSYSLMT,0                                                       
         BE    REQF100             IF EQUAL - NO LIMIT ACCESS                   
         MVC   SVLIM,CTSYSLMT      SAVE OFF LIMITED ACCESS CODE                 
*                                                                               
         LA    R0,L'COFFLIST                                                    
         LA    RE,COFFLIST                                                      
         CLI   0(RE),X'40'         IS THERE AN OFFICE LIST?                     
         BH    REQF85                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,*-16                                                          
         MVC   COFFLIST,SVOFFL     LIMITED ACCESS/REQUEST OFFICE LIST           
         B     *+14                                                             
REQF85   MVC   SVOFFL,COFFLIST     SAVE OFF OFFICE LST FOR LATER                
         B     REQF100                                                          
*                                                                               
         USING CTDSTD,R1                                                        
         SR    RF,RF                                                            
REQF90   CLI   0(R1),X'30'         DESTINATION DETAIL ELEMENT                   
         BNE   REQF100                                                          
         CLC   CNAME,SPACES        IF NAME IS HARDCODED SKIP                    
         BNE   *+10                                                             
         MVC   CNAME,CTDSTNAM                                                   
         CLC   CADDR1,SPACES       IF  HARDCODED SKIP                           
         BNE   REQF100                                                          
         MVC   CADDR1(L'CTDSTADD),CTDSTADD                                      
         MVC   CADDR2,SPACES                                                    
         MVC   CADDR3,SPACES                                                    
         CLI   CTDSTLEN,166                                                     
         BL    REQF100                                                          
         MVC   CADDR2(L'CTDSTAD2),CTDSTAD2                                      
*                                                                               
REQF100  SR    R0,R0                                                            
         IC    R0,1(R1)            BUMP TO NEXT EL                              
         AR    R1,R0                                                            
         B     REQF70                                                           
*                                                                               
REQF110  CLI   FOUNDIT,C'Y'        AGENCY SETUP FOR 1099'S?                     
         BE    REQFX               YES - CONTINUE                               
         MVI   FCRDACC,C'N'        CAUSE REQLAST                                
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDHIST,C'N'                                                    
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
LDGF     DS    0H                                                               
         USING ACLEDGD,R2                                                       
         L     R2,ADLDGEL          THE LEDGER ELEMENT                           
         CLI   0(R2),X'14'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   OFPOS,ACLTOFF       OFFICE POSITION                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCACC                                                            *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         MVI   SORTCD,C'2'         SET SEQ  2C  COME BEFORE OTHER U/L'S         
         MVC   SVACCT,SPACES       CLEAR SAVED AREA FOR ACCOUNT                 
*                                                                               
         CLI   OFPOS,C'1'          IS OFFPOS IN ACCT FILTS (F1-F4)              
         BL    PACC10              NO - SKIP FILTER LOOKUP                      
         MVC   FILTS,SPACES                                                     
         L     R1,ADLVASTA         HIGH LEVEL FILTERING                         
         BAS   RE,SETFILTR                                                      
         L     R1,ADLVBSTA                                                      
         BAS   RE,SETFILTR                                                      
         L     R1,ADLVCSTA                                                      
         BAS   RE,SETFILTR                                                      
         L     R1,ADACCSTA                                                      
         BAS   RE,SETFILTR                                                      
*                                                                               
PACC10   CLI   READIT,C'T'         READ TRANSACTIONS OR BUCKETS                 
         BE    *+8                                                              
         BAS   RE,LOOKUP            LIMIT ACCESS                                
         XC    D1RNM(CLRLEN),D1RNM  CLEAR PAYER PART OF RECORD                  
         XC    SRECRACT,SRECRACT                                                
         ZAP   D1RAMT,=P'0'                                                     
*                                                                               
*        L     R2,ADACC             ACCOUNT RECORD                              
*        MVC   SVKEY,0(R2)                                                      
*                                                                               
         USING NAMELD,R2                                                        
         L     R2,ADACCNAM          ACCOUNT NAME ELEMENT X'20'                  
         SR    R5,R5                                                            
         IC    R5,NAMLN                                                         
         MVC   D1RNM,SPACES                                                     
         SH    R5,=Y(NAMLN1Q+1)                                                 
         BM    *+14                                                             
         EX    R5,*+4                                                           
         MVC   D1RNM(0),NAMEREC                                                 
*                                                                               
         MVI   D1RADNL,0                                                        
         MVC   D1RADLN1,SPACES     CLEAR OUT ADDRESS LINES                      
         MVC   D1RADLN2,SPACES                                                  
         MVC   D1RADLN3,SPACES                                                  
         MVC   D1RADLN4,SPACES                                                  
         L     R2,ADACCADD         ADDRESS ELEMENT X'22' ELEM                   
         LTR   R2,R2                                                            
         BZ    PACC30                                                           
         USING ACADDD,R2                                                        
         CLI   ACADLNES,1          CHECK FOR BAD #LINES                         
         BL    PACC30                                                           
         CLI   ACADLNES,4                                                       
         BH    PACC30                                                           
         MVC   D1RADNL,ACADLNES    GET #LINES IN ADDRESS                        
*                                                                               
         SR    R5,R5                                                            
         IC    R5,ACADLNES                                                      
         LA    R3,D1RADLN1                                                      
         LA    R2,ACADADD                                                       
PACC20   MVC   0(L'ACADADD,R3),0(R2)                                            
         LA    R3,L'D1RADLN1(R3)                                                
         LA    R2,L'ACADADD(R2)                                                 
         BCT   R5,PACC20                                                        
*                                                                               
PACC30   DS    0H                                                               
         MVC   SRECRID,SPACES                                                   
         MVI   D1SSORID,C' '                                                    
         MVC   D1MIN6,QOPT2                                                     
         MVI   ELCODE,X'23'                                                     
         L     R2,ADACC                                                         
         BAS   RE,GETEL                                                         
         BE    PACC40              IF NO SS# THEN                               
         MVC   SRECRID,=X'FFFFFFFFFFFFFFFFFF'   DO NOT PUT ON TAPE.             
         MVI   SORTCD,C'3'         IN THIS CASE, LUMP ACCTS TOGETHER.           
         B     PACC50                                                           
*                                                                               
         USING ACOTHERD,R2                                                      
PACC40   MVC   SRECRID,ACOTNUM     RECIP SS NO.                                 
         MVC   D1SSORID,ACOTPROF   ID OR SS NO. INDICATOR                       
         MVC   D1MIN6,QOPT2                                                     
*                                                                               
PACC50   DS    0H                                                               
         L     R2,ADACC                                                         
         MVC   SRECRACT,3(R2)                                                   
         MVC   D1ACCT,1(R2)                                                     
         MVI   D1FORGN,C'N'        NOT A FOREIGN ADDRESS                        
         MVI   ELCODE,X'3F'        ONLINE MEMO ELEMENT                          
         L     R2,ADACC                                                         
         BAS   RE,GETEL                                                         
         BNE   PACC60                                                           
         USING ACOLMD,R2                                                        
         CLC   ACOLMEM(9),=C'FOREIGN=Y'                                         
         BNE   PACC60                                                           
         MVI   D1FORGN,C'Y'                                                     
*                                                                               
PACC60   DS    0H                                                               
         MVI   D1PTYPE,RSTXNCMP    DEFAULT TO NON EMPLOYEE COMP                 
         L     R2,ADACC                                                         
         MVI   ELCODE,RSTELQ       X'30' - STATUS ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   PACC70                                                           
         USING RSTELD,R2                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         BL    *+10                                                             
         MVC   D1PTYPE,RSTX1099    PAYMENT TYPE FOR THIS ACCOUNT                
*                                                                               
PACC70   MVI   SCANSW,SCANRECP     VALIDATE RECIPIENTS ADDRESS                  
         BAS   RE,FIXADDR          FIX ADDRESS                                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCTRNS                                                           *          
**********************************************************************          
         SPACE 1                                                                
PTRN     DS    0H                                                               
         CLI   READIT,C'T'         READ TRANSACTIONS OR BUCKETS                 
         BNE   PTRNX                                                            
         L     R2,ADTRANS                                                       
         CLI   0(R2),X'44'                                                      
         BNE   PTRNX                                                            
         USING TRANSD,R2                                                        
         GOTO1 CONVMOS,DMCB,(X'FE',(R2)),MOS    MOS FROM TRANSACTION            
         CLC   MOS,SVSDAT               IS MOS LOWER THAN START                 
         BL    PTRNX                    YES- NEXT TRANSACTION                   
         CLC   MOS,SVEDAT               OR HIGHER THAN END DATE                 
         BH    PTRNX                    YES- GET NEXT                           
         BAS   RE,LOOKUP                CHECKLIMIT ACCESS                       
*                                                                               
***********************************************************************         
*        CLC   ALPHAID,=C'SU'           SAFFER HARDCODE                         
*        BNE   PTRN10                                                           
*        MVC   SRECCID,=CL9'980123040'  JAN/93-AUG/93                           
*        MVC   D1CNM,=CL36'SAFFER COMMUNICATIONS USA, LTD'                      
*        CLC   MOS,=X'9308'                                                     
*        BL    PTRN50                                                           
*        MVC   SRECCID,=CL9'363863346'  SEP/93-DEC/93                           
*        MVC   D1CNM,=CL36'DAVID CRAVIT && ASSOC., LTD'                         
*        B     PTRN50                                                           
***********************************************************************         
*                                                                               
PTRN10   CLC   ALPHAID,=C'JW'      IF J WALTER IT'S RUNNING OFF SV              
         BNE   PTRN50              NOT 2C SO..............                      
         CLI   TRNSTYPE,3          LEAVE OUT MANUAL CHECKS                      
         BE    PTRNX                                                            
         CLI   TRNSTYPE,37         SUBTRACT OUT TYPE 37 VOIDS                   
         BNE   PTRN30                                                           
         ZAP   D1RAMT,TRNSAMNT                                                  
         MP    D1RAMT,=P'-1'                                                    
*                                                                               
         L     RF,ADTRANS                                                       
PTRN20   ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),0                                                          
         BE    PTRN30                                                           
         CLI   0(RF),X'60'                                                      
         BNE   PTRN20                                                           
*                                                                               
         USING TRSELD,RF                                                        
         TM    TRSSTAT,TRSSVOID    VOIDED ITEM IN MARKER?                       
         BZ    *+10                                                             
         ZAP   D1RAMT,TRNSAMNT     IF VOIDED IN MARKER THEN DONT                
         B     PTRN20              SWITCH SIGN OF ITEM.                         
         DROP  RF                                                               
*                                                                               
PTRN30   L     RF,ADTRANS                                                       
PTRN40   ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),0                                                          
         BE    PTRN50                                                           
         CLI   0(RF),X'60'                                                      
         BNE   PTRN40                                                           
         USING TRSELD,RF                                                        
         CLC   TRSDATE,=X'BB9F'    SKIP ITEMS < 12/31/93                        
         BL    PTRNX                                                            
         B     PTRN40                                                           
         DROP  RF                                                               
*                                                                               
PTRN50   TM    TRNSSTAT,X'80'                                                   
         BZ    PTRNX                                                            
         ZAP   D1RAMT,TRNSAMNT                                                  
*                                                                               
         BAS   RE,PUTSORT               PUT TO SORT                             
*                                                                               
PTRNX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ACCLAST - READ OTHER LEDGERS                                       *          
**********************************************************************          
         SPACE 1                                                                
OTHRLDGR DS    0H                                                               
         CLI   READIT,C'T'      IF READING TRANSACTIONS DONT PUT HERE           
         BE    *+8                                                              
         BAS   RE,PUTSORT       ELSE PUT OUT THE REC FROM THE 2C LEDGER         
*                               EVEN IF THERE ARE NO $ WE NEED THE              
*                               2C NAME AND ADDRESS ETC.                        
*                                                                               
***********************************************************************         
         CLC   ALPHAID,=C'JW'                                                   
         BE    OLX                                                              
***********************************************************************         
*                                                                               
         MVI   SORTCD,C'3'                                                      
         LA    R1,READLIST         INITIALIZE CONTROL LEDGER                    
         MVC   CLGR,0(R1)                                                       
         ST    R1,SV1                                                           
*                                                                               
OL10     ZAP   D1RAMT,=P'0'                                                     
*                                                                               
         L     R2,ADACC                                                         
         USING TRNRECD,R4                                                       
         LA    R4,SVKEY                                                         
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,0(R2)                                                   
         MVI   TRNKUNT,C'S'                                                     
         MVC   TRNKLDG,CLGR                                                     
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   SVKEY(ACTKEND),IOKEY                                             
         BNE   OL130               DO NEXT LEDGER                               
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         LA    R4,IO               SET R4 TO POINT TO THE RECORD                
*                                                                               
*                                                                               
* PROCESS HIGH LEVEL ACCOUNTS - (GET NAME/ADDRESS)                              
*                                                                               
         MVI   ELCODE,X'32'        BALANCE EL ABSENT ON HI LVLS.                
         LR    R2,R4               SO MAKE SURE WE HAVE A LOW LEVEL.            
         BAS   RE,GETEL2                                                        
         BNE   OL40                                                             
         USING ACNAMED,R2                                                       
         L     R2,ADACCNAM                                                      
         SR    R1,R1                                                            
         IC    R1,ACNMLEN          LENGTH OF NAME EL                            
         MVC   D1RNM,SPACES                                                     
         SH    R1,=H'3'            TAKE OFF FOR L'CODE+L'LNTH+1                 
         BM    OL20                                                             
         EX    R1,*+4                                                           
         MVC   D1RNM(0),ACNMNAME                                                
*                                                                               
OL20     MVI   D1RADNL,0                                                        
         MVC   D1RADLN1,SPACES     CLEAR OUT ADDRESS LINES                      
         MVC   D1RADLN2,SPACES                                                  
         MVC   D1RADLN3,SPACES                                                  
         MVC   D1RADLN4,SPACES                                                  
         L     R2,ADACCADD         ADDRESS ELEMENT X'22' ELEM                   
         LTR   R2,R2                                                            
         BZ    OL40                                                             
         USING ACADDD,R2                                                        
         CLI   ACADLNES,1          CHECK FOR BAD #LINES                         
         BL    OL40                                                             
         CLI   ACADLNES,4                                                       
         BH    OL40                                                             
         MVC   D1RADNL,ACADLNES    GET #LINES IN ADDRESS                        
*                                                                               
         ZIC   R0,ACADLNES                                                      
         LA    R1,D1RADLN1                                                      
         LA    R2,ACADADD                                                       
OL30     MVC   0(L'ACADADD,R1),0(R2)                                            
         LA    R1,L'D1RADLN1(R1)                                                
         LA    R2,L'ACADADD(R2)                                                 
         BCT   R0,OL30                                                          
         MVI   SCANSW,SCANRECP     VALIDATE RECIPIENTS ADDRESS                  
         BAS   RE,FIXADDR                                                       
*                                                                               
OL40     DS    0H                                                               
         GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
         CLC   SVKEY(ACTKEND),IOKEY  SAME ACCT?                                 
         BNE   OL120               B IF NO.                                     
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         CLI   READIT,C'T'         ARE WE READING TRANSACTIONS                  
         BE    OL70                                                             
*                                                                               
* PROCESS HISTORY ELEMENTS                                                      
*                                                                               
         MVI   ELCODE,X'45'        ACCUMULATE HISTORY ELTS.                     
         LR    R2,R4                                                            
         BAS   RE,GETEL2                                                        
         BNE   OL40                                                             
*                                                                               
         USING TRHISTD,R2                                                       
OL50     CLC   TRHSYEAR(2),SVSDAT  LOWER THAN START YYMM                        
         BL    OL60                                                             
         CLC   TRHSYEAR(2),SVEDAT  HIGHER THAN END YYMM                         
         BH    OL60                                                             
         AP    D1RAMT,TRHSDR                                                    
         CLC   TRNKCUNT(2),=C'SC'  TAKE AWAY SUB SC CREDITS                     
         BNE   OL60                                                             
         SP    D1RAMT,TRHSCR                                                    
*                                                                               
OL60     BAS   RE,NEXTEL                                                        
         BE    OL50                LOOP BACK FOR NEXT 45 EL                     
         B     OL40                OR GET NEXT RECORD                           
*                                                                               
* PROCESS TRANSACTIONS                                                          
*                                                                               
OL70     MVI   ELCODE,X'44'             ACCUMULATE TRANS ELTS.                  
         LR    R2,R4                                                            
         BAS   RE,GETEL2                                                        
         BNE   OL40                                                             
         USING TRANSD,R2                                                        
         GOTO1 CONVMOS,DMCB,(X'FE',(R2)),MOS    MOS FROM TRANSACTION            
         CLC   MOS,SVSDAT               IS MOS LOWER THAN START                 
         BL    OL40                     YES- NEXT TRANSACTION                   
         CLC   MOS,SVEDAT               OR HIGHER THAN END DATE                 
         BH    OL40                     YES- GET NEXT                           
*                                                                               
         MVI   SKIPACCT,C'N'                                                    
         BAS   RE,LOOKUP                CHECKLIMIT ACCESS                       
         CLI   SKIPACCT,C'Y'            SKIP THIS TRANSACTION                   
         MVI   SKIPACCT,C'N'                                                    
         BE    OL40                     YES                                     
*                                                                               
* HANDLE VOIDS FROM BATCH/MARKER                                                
*                                                                               
         CLI   TRNSTYPE,37         SUBTRACT OUT TYPE 37 VOIDS                   
         BNE   OL90                                                             
         ZAP   D1RAMT,TRNSAMNT                                                  
         MP    D1RAMT,=P'-1'                                                    
*                                                                               
         LR    RF,R2                                                            
OL80     ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),0                                                          
         BE    OL100                                                            
         CLI   0(RF),X'60'                                                      
         BNE   OL80                                                             
*                                                                               
         USING TRSELD,RF                                                        
         TM    TRSSTAT,TRSSVOID    VOIDED ITEM IN MARKER?                       
         BZ    *+10                                                             
         ZAP   D1RAMT,TRNSAMNT     IF VOIDED IN MARKER THEN DONT                
         B     OL100               SWITCH SIGN OF ITEM.                         
         DROP  RF                                                               
*                                                                               
OL90     TM    TRNSSTAT,X'80'                                                   
         BZ    OL40                                                             
         ZAP   D1RAMT,TRNSAMNT                                                  
*                                                                               
OL100    DS    0H                                                               
         MVC   SRECRACT,IOKEY+3         MOVE IN ACCOUNT NO.                     
         MVC   D1ACCT,IOKEY+1                                                   
***********************************************************************         
*        CLC   ALPHAID,=C'SU'           SAFFER HARDCODE                         
*        BNE   OL110                                                            
*        MVC   SRECCID,=CL9'980123040'  JAN/93-AUG/93                           
*        MVC   D1CNM,=CL36'SAFFER COMMUNICATIONS USA, LTD'                      
*        CLC   MOS,=X'9308'                                                     
*        BL    OL110                                                            
*        MVC   SRECCID,=CL9'363863346'  SEP/93-DEC/93                           
*        MVC   D1CNM,=CL36'DAVID CRAVIT && ASSOC., LTD'                         
***********************************************************************         
OL110    BAS   RE,PUTSORT               PUT TO SORT                             
         B     OL40                                                             
*                                                                               
OL120    CLI   READIT,C'T'              ARE WE READING TRANSACTIONS             
         BE    OL130                                                            
         CP    D1RAMT,=P'0'                                                     
         BE    OL130                                                            
         BAS   RE,LOOKUP                CHECKLIMIT ACCESS                       
         MVC   SRECRACT,IOKEY+3         MOVE IN ACCOUNT NO.                     
         MVC   D1ACCT,IOKEY+1                                                   
         BAS   RE,PUTSORT                                                       
*                                                                               
OL130    L     R1,SV1              DO NEXT LEDGER                               
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    OL140                                                            
         MVC   CLGR,0(R1)                                                       
         ST    R1,SV1                                                           
         B     OL10                                                             
*                                                                               
OL140    LA    R1,READLIST         EXITING THE OTHRLDGR RTNE.                   
         ST    R1,SV1                                                           
         MVC   CLGR,0(R1)                                                       
*                                                                               
         LA    R4,SVKEY                                                         
         XC    TRNKEY,TRNKEY                                                    
         L     R2,ADACC                                                         
         MVC   TRNKEY(32),0(R2)         READ ACCOUNT RECORD AGAIN               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   NOT TO SCREW UP MONACC                  
         MVI   SORTCD,C'2'              RE-ESTABLISH 2C FIRST                   
*                                                                               
OLX      B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
READLIST DC    C'VWXY',X'FF'                                                    
SV1      DS    F                                                                
         EJECT                                                                  
**********************************************************************          
* PUT RECORD TO SORTER                                               *          
**********************************************************************          
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         TM    STATUS,STATSRT      IS SORTER ALREADY OPEN?                      
         BO    PUTSRT10                                                         
         OI    STATUS,STATSRT      INDICATE SORTER IS OPEN                      
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
PUTSRT10 MVC   SRECLGR,SORTCD           FORCE LGR SEQUENCE                      
         GOTO1 SORTER,DMCB,=C'PUT',SRECA                                        
*        MVC   MSG,=CL10'PUT'                                                   
*        GOTO1 ADUMP,DMCB,(RC),SRECA,L'SRECA                                    
PUTSX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         TM    STATUS,STATSRT      WAS SORTER OPENED?                           
         BZ    REQLXX                                                           
*                                                                               
         MVI   FIRSTPRT,C'Y'       SET FOR LINE UP PHASE.                       
         XC    SVCID,SVCID                                                      
         ZAP   PAYERAMT,=P'0'      CLEAR PAYER BUCKET.                          
         ZAP   TOTACNT,=P'0'       CLEAR PAYER RECORD COUNT.                    
*                                                                               
REQL10   GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
         MVC   MSG,=CL10'GET1'                                                  
         GOTO1 ADUMP,DMCB,(RC),SRECA,750                                        
*                                                                               
         LTR   R2,R2               END OF SORT?                                 
         BZ    REQL130                                                          
         LA    R1,LSREC                                                         
         MOVE  (SRECCO,(R1)),(R2)  MOVE REC INTO MY AREA.                       
         CLI   SVCID,0             FIRST RECORD?                                
         BNE   REQL20              IF YES THEN OPEN.                            
         BAS   RE,OPEN             OPEN TAPE FILE AND WORK FILE.                
         CLI   SRECCID,0                                                        
         BE    REQL40                                                           
         BAS   RE,DOTREC                                                        
         B     REQL40                                                           
*                                                                               
REQL20   CLC   SVCID,SRECCID       SEE IF COMPANY BREAK.                        
         BE    REQL50              B IF NO                                      
REQL30   BAS   RE,DOCREC    IF NEW COMPANY, CREATE 'C' REC FOR OLD ONE          
*                                                                               
REQL40   MVC   SVCID,SRECCID                                                    
         XC    SVRID,SVRID                                                      
         CLI   SRECCID,0                                                        
         BE    *+8                                                              
         BAS   RE,DOAREC                                                        
*                                                                               
REQL50   CLI   SVRID,0             FIRST PAYER FOR THIS COMPANY?                
         BE    REQL60              B IF YES                                     
         CLI   SRECRID,X'FF'       IF START MISSING SS NOS.                     
         BE    REQL100             THEN HANDLE THEM.                            
         CLC   SVRID,SRECRID       PAYER BREAK?                                 
         BE    REQL70              B IF NO                                      
         BAS   RE,WRITBREC         WRITE PRESENT B REC WITH BUCKET.             
*                                                                               
REQL60   BAS   RE,DOBREC           INITIALIZE 'B' REC FOR NEW PAYEE.            
         MVC   SVRID,SRECRID                                                    
*                                                                               
REQL70   MVC   SVPTYPE,D1PTYPE     SAVE CURRENT PAY TYPE                        
         AP    PAYERAMT,D1RAMT     BUCKET AMT, AND                              
         CP    D1RAMT,=P'0'        DONT TRACK ACCTS W/ZERO BALANCE              
         BE    REQL10                                                           
*                                                                               
         USING BREC,R6                                                          
         LA    R6,WKREC            ADD ENTRY TO SOURCE LIST                     
         LA    R6,BSRCLIST                                                      
         USING SRCED,R6                                                         
         LA    R0,SRCEMAX          MAX NUMBER OF ACCTS TO SAVE                  
*                                                                               
REQL80   CLI   0(R6),0                                                          
         BE    REQL90                                                           
         CLC   SRCEACCT,D1ACCT     SUM UP TRANSACTIONS                          
         BNE   *+14                                                             
         AP    SRCEAMNT,D1RAMT                                                  
         B     REQL10                                                           
         LA    R6,SRCELNQ(R6)      FIND NEXT AVAILABLE ENTRY                    
         BCT   R0,REQL80                                                        
         B     REQL10              NO MORE ROOM TO SAVE ACCTS                   
*                                                                               
REQL90   MVC   SRCEACCT,D1ACCT     SAVE OFF UNIT                                
         ZAP   SRCEAMNT,D1RAMT     SAVE OFF AMOUNT                              
         MVI   SRCELNQ(R6),0                                                    
         B     REQL10              GET NEXT SORT RECORD.                        
         DROP  R6                                                               
*                                                                               
* FOR FF RECS, BREAK ON ACCT NOT ID.                                            
*                                                                               
REQL100  BAS   RE,WRITBREC                                                      
         MVC   SVACCT,SRECRACT                                                  
         BAS   RE,DOBREC                                                        
*                                                                               
         USING BREC,R6                                                          
         LA    R6,WKREC                                                         
         LA    R6,BSRCLIST                                                      
         USING SRCED,R6                                                         
         MVC   SRCEACCT,D1ACCT                                                  
         ZAP   SRCEAMNT,D1RAMT                                                  
*                                                                               
REQL110  CLC   SVACCT,SRECRACT                                                  
         BE    REQL120                                                          
         BAS   RE,WRITBREC                                                      
         MVC   SVACCT,SRECRACT                                                  
         BAS   RE,DOBREC                                                        
*                                                                               
REQL120  AP    PAYERAMT,D1RAMT                                                  
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
*        MVC   MSG,=CL10'GET2'                                                  
*        GOTO1 ADUMP,DMCB,(RC),SRECA,750                                        
*                                                                               
         LTR   R2,R2                                                            
         BZ    REQL130             END OF SORT?                                 
         LA    R1,LSREC                                                         
         MOVE  (SRECCO,(R1)),(R2)  MOVE REC INTO MY AREA.                       
         CLI   SRECRID,X'FF'                                                    
         BE    REQL110             DO ANOTHER FF REC                            
         B     REQL30              ELSE WE ARE STARTING A NEW COMPANY.          
*                                                                               
REQL130  DS    0H                                                               
         BAS   RE,DOFREC           CREATE FINAL RECORD ON TAPE.                 
         MVI   BYTE,C'W'                                                        
         BAS   RE,CLOSE            CLOSE  WORK FILE.                            
         GOTO1 SORTER,DMCB,=C'END' CLOSE THE SORT.                              
         CLI   QOPT1,C'Y'                                                       
         BE    REQLX               NO REGISTER IF PRINTING LIVE FORMS           
         BAS   RE,DOREG            DO REGISTER                                  
         MVI   BYTE,C'T'                                                        
         BAS   RE,CLOSE            CLOSE TAPE FILE.                             
*                                                                               
REQLX    CLI   QOPT1,C'Y'          IF LIVE RUN - MARK T99 RECORD                
         BNE   REQLXX                                                           
         GOTO1 AMRKT99,DMCB,(RC)   MARK 1099 RECORD WITH RUN INFO               
*                                                                               
REQLXX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LIMIT ACCESS                                                       *          
**********************************************************************          
         SPACE 1                                                                
LIMIT    NTR1                                                                   
         XC    OFFLAG,OFFLAG                                                    
         LA    R1,COFFLIST         POINT TO OFFICE LIST                         
         LA    R0,COFFLNQ          R0=NUMBER OF OFFICES IN LIST                 
*                                                                               
         TM    OFFSTAT,STASOFF2    TEST IF NEW OFFICE CODE IN EFFECT            
         BO    LIMIT70                                                          
*                                                                               
         CLI   0(R1),C' '                                                       
         BE    LIMITYES            NO LIMIT ACCESS                              
         CLI   READIT,C'B'         READING BUCKETS                              
         BE    LIMIT20                                                          
         CLI   OFPOS,C'T'          FOR TRANSACTIONS                             
         BE    LIMIT10                                                          
         CLI   OFPOS,0             OR IF NOT SPECIFIED                          
         BE    LIMIT10                                                          
         B     LIMITYES                                                         
LIMIT10  MVC   OFFLAG(1),TRNSANAL-TRANSD(R2)                                    
         B     LIMIT50                                                          
*                                                                               
LIMIT20  CLI   OFPOS,C'T'          IF OFFICE ON TRANSACTIONS IT'S OK            
         BE    LIMITYES                                                         
         CLI   OFPOS,0             TEST OFFICE IN ACCOUNT KEY                   
         BE    LIMITYES                                                         
         CLI   OFPOS,12                                                         
         BH    LIMIT30                                                          
         ZIC   RF,OFPOS            POSITIONAL OFFICE CODE                       
         A     RF,ADACC                                                         
         LA    RF,2(RF)            POINT RF TO OFFICE CODE IN ACCOUNT           
         MVC   OFFLAG(1),0(RF)                                                  
         CLI   OFFLAG,C' '                                                      
         BE    LIMITYES            IF HIGH LEVEL IT'S OK                        
         B     LIMIT50                                                          
*                                                                               
LIMIT30  CLI   OFPOS,C'1'          TEST OFFICE IN ACCOUNT FILTERS               
         BL    LIMIT40                                                          
         MVC   OFFLAG(1),OFPOS        ADJUST OFPOS                              
         NI    OFFLAG,X'07'                                                     
         ZIC   RF,OFFLAG                                                        
         IC    RF,FILTS-1(RF)                                                   
         STC   RF,OFFLAG           SET FILTER VALUE                             
         B     LIMIT50                                                          
*                                                                               
LIMIT40  CLI   OFPOS,C'C'          TEST CLIENT OFFICE (ACPRUNIT)                
         BNE   LIMITYES                                                         
         L     RF,ADPROFIL                                                      
         MVC   OFFLAG(1),ACPRUNIT-ACPROFD(RF)                                   
*                                                                               
LIMIT50  CLI   0(R1),C'0'          SKIP OFFICE ZERO                             
         BE    LIMIT60                                                          
         CLI   0(R1),0             SKIP ZERO ENTRIES                            
         BE    LIMIT60                                                          
         CLC   OFFLAG(1),0(R1)     MATCH OFFICE TO LIST                         
         BE    LIMITYES                                                         
LIMIT60  LA    R1,1(R1)            BUMP TO NEXT LIST ENTRY                      
         BCT   R0,LIMIT50                                                       
         B     LIMITNO                                                          
*                                                                               
LIMIT70  SRL   R0,1                DIVIDE OFFICE LIST BY TWO                    
LIMIT75  MVC   OFFLAG,TRNOFFC-TRNELD(R2)                                        
         CLC   OFFLAG,0(R1)                                                     
         BE    LIMITYES                                                         
         LA    R1,2(R1)                                                         
         BCT   R0,LIMIT75                                                       
*                                                                               
LIMITNO  LTR   RB,RB               NO MATCH SET CC=NE                           
         B     EXIT                                                             
LIMITYES CR    RB,RB               MATCHED SET CC=EQ                            
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* VALIDATE ADDRESS                                                   *          
**********************************************************************          
         SPACE 1                                                                
FIXADDR  NTR1                                                                   
         MVC   ADDRST,SPACES       CLEAR OUT WORK AREA                          
         MVC   ADDRST2,SPACES                                                   
         MVC   ADDRST3,SPACES                                                   
         MVC   ADDRCSZ,SPACES                                                   
         CLI   SCANSW,SCANCMP      VALIDATE A COMPANY ADDRESS?                  
         BE    FIXCMP                                                           
         CLI   SCANSW,SCANRECP     VALIDATE A RECIPIENTS ADDRESS                
         BE    FIXRCP                                                           
         DC    H'0'                HAS TO BE ONE OR THE OTHER                   
*                                                                               
* COMPANY ADDRESS VALIDATION                                                    
*                                                                               
FIXCMP   DS    0H                                                               
         CLI   D1CADNL,0           MISSING COMPANY ADDRESS?                     
         BE    FIXERR1                                                          
         LA    R3,D1CADLN2                                                      
         LA    R2,2                                                             
*                                                                               
FIXCMP20 GOTO1 ADSCAN,DMCB,(40,(R3)),(20,SCANCY),SCANST,(9,SCANZIP)             
         CLI   DMCB+3,0                                                         
         BE    FIXCMP40                                                         
         LA    R3,L'D1CADLN2(R3)   NEXT LINE                                    
         BCT   R2,FIXCMP20                                                      
         B     FIXERR1                                                          
*                                                                               
FIXCMP40 CLC   SCANCY,SPACES                                                    
         BE    FIXERR1                                                          
         CLC   SCANST,SPACES                                                    
         BE    FIXERR1                                                          
         CLC   SCANZIP,SPACES                                                   
         BE    FIXERR1                                                          
         MVC   ADDRST,D1CADLN1     MOVE IN STREET ADDRESS                       
         GOTO1 SQUASHER,DMCB,SCANCY,L'SCANCY                                    
         L     R1,DMCB+4           LENGTH OF SQUASHED STRING                    
         BCTR  R1,0                                                             
         LA    R3,D1CADLN2                                                      
         LA    R0,L'D1CADLNS-L'D1CADLN1                                         
*                                                                               
FIXCMP70 EXCLC R1,0(R3),SCANCY     FIND WHERE CITY IS IN ADDRESS BLK            
         BE    FIXCMP80                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,FIXCMP70                                                      
         B     FIXERR1             TOO MANY TIMES TO LOOP                       
*                                                                               
FIXCMP80 LA    R0,D1CADLN2         POINT TO START OF BLOCK                      
         SR    R3,R0               GET #CHARS INTO BLOCK                        
         CH    R3,=Y(L'D1CADLN2)   WAS CITY IN 2ND LINE OF ADDRESS?             
         BL    FIXCMP90                                                         
         MVC   ADDRST2,D1CADLN2    IF CITY NOT IN FIRST LINE THEN MUST          
*                                  BE EXTRA ADDRESS INFO.                       
FIXCMP90 DS    0H                                                               
         BAS   RE,SCANIT                                                        
         MVI   D1CADNL,3           SET TO MAX # LINES                           
         MVC   D1CADLN1,ADDRST                                                  
         MVC   D1CADLN2,ADDRST2                                                 
         MVC   D1CADLN3,ADDRCSZ                                                 
         B     EXIT                                                             
*                                                                               
FIXERR1  DS    0H                                                               
         MVI   D1CADNL,3           SET TO MAX # LINES                           
         MVC   D1CADLN3,=CL40'**** INVALID ADDRESS ****'                        
         B     EXIT                                                             
*                                                                               
* RECIPIENT ADDRESS VALIDATION                                                  
*                                                                               
FIXRCP   DS    0H                                                               
         CLI   D1RADNL,0           MISSING RECIPIENT ADDRESS?                   
         BE    FIXERR3                                                          
         CLI   D1FORGN,C'Y'        DONT SCAN FOREIGN ADDRESSES                  
         BE    FIXERR3                                                          
         LA    R3,D1RADLN2                                                      
         LA    R2,3                #LINES TO SCAN                               
*                                                                               
FIXRCP5  GOTO1 ADSCAN,DMCB,(40,(R3)),(20,SCANCY),SCANST,(9,SCANZIP)             
         CLI   DMCB+3,0                                                         
         BE    FIXRCP10                                                         
         LA    R3,L'D1RADLN2(R3)   NEXT LINE                                    
         BCT   R2,FIXRCP5                                                       
         B     FIXERR3             ERROR IN SCANNING ADDRESS                    
*                                                                               
FIXRCP10 DS    0H                                                               
         CLC   SCANCY,SPACES                                                    
         BE    FIXERR3                                                          
         CLC   SCANST,SPACES                                                    
         BE    FIXERR3                                                          
         CLC   SCANZIP,SPACES                                                   
         BE    FIXERR3                                                          
         MVC   ADDRST,D1RADLN1     MOVE IN STREET ADDRESS                       
         GOTO1 SQUASHER,DMCB,SCANCY,L'SCANCY                                    
         L     R1,DMCB+4           LENGTH OF SQUASHED STRING                    
         BCTR  R1,0                                                             
         LA    R3,D1RADLN2                                                      
         LA    R0,L'D1RADLNS-L'D1RADLN1                                         
*                                                                               
FIXRCP50 EXCLC R1,0(R3),SCANCY     FIND WHERE CITY IS IN ADDRESS BLK            
         BNE   FIXRCP60                                                         
         LA    R5,D1RADLN2         CHECK THAT CITY IS NOT SOMEPLACE             
         CR    R3,R5                 IN THE STREET ADDRESS AS IN                
         BE    FIXRCP80              JWT - SVP01327                             
         LA    R5,D1RADLN3           OR SOMEPLACE ELSE IN THE                   
         CR    R3,R5                 ADDRESS AS IN JWT - SVP25187               
         BNL   FIXRCP80                                                         
FIXRCP60 LA    R3,1(R3)                                                         
         BCT   R0,FIXRCP50                                                      
         B     FIXERR3             TOO MANY TIMES TO LOOP                       
*                                                                               
FIXRCP80 LA    R0,D1RADLN2         POINT TO START OF BLOCK                      
         SR    R3,R0               GET #CHARS INTO BLOCK                        
         CH    R3,=Y(L'D1RADLN2)   WAS CITY IN 2ND LINE OF ADDRESS?             
         BL    FIXRCP90              AS IN JWT CASE, SVP01327                   
         MVC   ADDRST2,D1RADLN2                                                 
         CH    R3,=Y(L'D1RADLN2+L'D1RADLN3) ON 3RD LINE OF ADDRESS?             
         BL    FIXRCP90              AS IN JWT CASE, SVP25187                   
         MVC   ADDRST3,D1RADLN3                                                 
*                                                                               
FIXRCP90 DS    0H                                                               
         BAS   RE,SCANIT                                                        
         MVI   D1RADNL,4           SET TO MAX # LINES                           
         MVC   D1RADLN1,ADDRST                                                  
         MVC   D1RADLN2,ADDRST2                                                 
         MVC   D1RADLN3,ADDRST3                                                 
         MVC   D1RADLN4,ADDRCSZ                                                 
         B     EXIT                                                             
*                                                                               
FIXERR3  DS    0H                  THIS CODE IS ONLY IF THE GUY HAS             
         CLI   D1FORGN,C'Y'        A FOREIGN ADDRESS                            
         BNE   FIXERR7                                                          
         CLC   D1RADLN4,SPACES                                                  
         BNE   EXIT                                                             
         LA    R1,D1RADLN3                                                      
         LA    R0,2                # TIMES TO LOOP                              
*                                                                               
FIXERR4  CLC   0(L'D1RADLN2,R1),SPACES                                          
         BNE   FIXERR5                                                          
         SH    R1,=Y(L'D1RADLN2)                                                
         BCT   R0,FIXERR4                                                       
         B     FIXERR7                                                          
*                                                                               
FIXERR5  MVC   D1RADLN4,0(R1)      MOVE CITY INTO LAST LINE                     
         MVC   0(L'D1RADLN2,R1),SPACES                                          
         MVI   D1RADNL,4           SET TO MAX # LINES                           
         B     EXIT                                                             
*                                                                               
FIXERR7  MVI   D1RADNL,4           SET TO MAX # LINES                           
         MVC   D1RADLN4,=CL40'*** INVALID ADDRESS ***'                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ELIMINATE CHARS NOT ALLOWED IN ADDRESS                             *          
**********************************************************************          
         SPACE 1                                                                
SCANIT   NTR1                                                                   
         LA    R3,ADDRWORK                                                      
         LA    R0,L'ADDRWORK                                                    
*                                                                               
SCANIT10 CLI   0(R3),C'#'          # NOT ALLOWED                                
         BNE   *+8                                                              
         MVI   0(R3),C' '          REPLACE WITH A BLANK                         
         LA    R3,1(R3)                                                         
         BCT   R0,SCANIT10                                                      
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* BUILD 'T' RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
DOTREC   NTR1                      CLEAR OUT RECORD AREA                        
         LA    R0,SPACES           A(SOURCE)                                    
         SR    R1,R1               L'SOURCE                                     
         ICM   R1,8,SPACES         PAD CHAR = C' '                              
         LA    RE,WKREC            A(DESTINATION)                               
         LA    RF,L'WKREC          L'DESTINATION                                
         MVCL  RE,R0                                                            
         ZAP   RECSEQ,=P'1'        START SEQUENCE NUMBER AT 1                   
*                                                                               
         USING TRECD,R6                                                         
         LA    R6,WKREC            BUILD "A" REC                                
         MVI   TREC,C'T'           RECORD TYPE                                  
         MVC   TYEAR,D1YEAR                                                     
         UNPK  TRECSEQ,RECSEQ      RECORD SEQUENCE NUMBER                       
         OI    TRECSEQ+7,X'F0'                                                  
         MVC   TMTFIND(2),=C'LS'   MAGNETIC TAPE FILE INDICATOR                 
         MVC   TTRID,D1CTRAN       TRANSMITTER CODE                             
         MVC   TCPYNM,D1CNM        COMPANY NAME                                 
         MVC   TTRNNM,TCPYNM       COMPANY NAME AND TRANSMITTER NAME            
         MVC   TCPYADD1,D1CADLN1   STREET ADDRESS                               
         CLI   QOPT3,C'T'          IS THIS A TEST FILE?                         
         BNE   *+8                                                              
         MVI   TTEST,C'T'          MARK AS A TEST TRANSMISSION.                 
         CLI   QOPT3,C'R'                                                       
         BNE   *+10                                                             
         MVC   TRPALPHA,QSELECT                                                 
*                                                                               
         MVC   TCPYCTY(L'D1CADCTY),D1CADCTY    CITY                             
         MVC   TCPYST,D1CADST                      STATE                        
         MVC   TCPYZIP,D1CADZIP                         ZIP                     
*                                                                               
         MVC   TCONNM,SRECCNM      AGENCY CONTACT NAME                          
         MVC   TCPHONE,SRECPHON    AGENCY PHONE NUMBER                          
         MVC   TCEMAIL,SRECCEM     AGENCY EMAIL ADDRESS                         
         MVC   TTIN,SRECCID        EIN NUMBER                                   
*                                                                               
         ZAP   TOTBCNT,=P'0'                                                    
*                                                                               
         MVI   TVNDIND,C'V'        OUTSIDE VENDOR                               
         MVC   TVNDNM(26),=C'DONOVAN DATA SYSTEMS, INC.'                        
         MVC   TVNDADD(20),=C'115 WEST 18TH STREET'                             
         MVC   TVNDCTY(8),=C'NEW YORK'                                          
         MVC   TVNDST,=C'NY'                                                    
         MVC   TVNDZIP(5),=C'10011'                                             
         MVC   TVNDCNM(11),=C'RAJIV GUPTA'                                      
         MVC   TVNDCPH(10),=C'2126335198'                                       
         MVC   TVNDCEM,=CL35'RAJIV.GUPTA@DONOVANDATA.COM'                       
*                                                                               
         CLC   D1CTRAN,DDSTRANS    ONLY PUT IN TRANSMITTER NAME                 
         BNE   DOT10               IF DIFFERENT FROM PAYER.                     
         MVC   TCONNM(55),SPACES   CLEAR AGY CONTACT NAME/NUM                   
         MVC   TCONNM(13),=C'JOHN ACAMPORA'                                     
         MVC   TCPHONE(10),=C'2126335146'                                       
         MVC   TCEMAIL,=CL35'JOHN.ACAMPORA@DONOVANDATA.COM'                     
         MVC   TTRNNM,SPACES                                                    
         MVC   TTRNNM(26),=C'DONOVAN DATA SYSTEMS, INC.'                        
         MVC   TCPYNM,TTRNNM       TRANSMITTER NAME AND COMPANY NAME            
         MVC   TCPYADD1(20),=C'115 WEST 18TH STREET'                            
         MVC   TCPYCTY(10),=CL10'NEW YORK'                                      
         MVC   TCPYST,=C'NY'                                                    
         MVC   TCPYZIP(5),=C'10011'                                             
*                                                                               
* WRITE RECORD TO DISK/TAPE                                                     
*                                                                               
DOT10    LA    R5,TREC                                                          
         MVI   BYTE,C'W'           INDICATOR TO WRITE WORK FILE REC             
         BAS   RE,WTREC                                                         
*                                                                               
*        MVC   MSG,=CL10'TREC'                                                  
*        GOTO1 ADUMP,DMCB,(RC),TREC,750                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* BUILD 'A' RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
DOAREC   NTR1                      CLEAR OUT RECORD AREA                        
         LA    R0,SPACES           A(SOURCE)                                    
         SR    R1,R1               L'SOURCE                                     
         ICM   R1,8,SPACES         PAD CHAR = C' '                              
         LA    RE,WKREC            A(DESTINATION)                               
         LA    RF,L'WKREC          L'DESTINATION                                
         MVCL  RE,R0                                                            
         AP    RECSEQ,=P'1'        INCREMENT RECORD SEQUENCE NUMBER             
*                                                                               
         USING AREC,R6                                                          
         LA    R6,WKREC            BUILD "A" REC                                
         MVI   AREC,C'A'           RECORD TYPE                                  
         MVC   AYEAR,D1YEAR        1993                                         
         MVC   ACCOID,SRECCID      EIN NUMBER                                   
         MVI   AFRMTYP,C'A'        1099-MISC                                    
         MVC   AAMTCD,=CL12'12367' AMOUNT CODES                                 
*                                                                               
         MVI   AORIG,C'1'          ASSUME ITS ORIGINAL                          
         CLI   QOPT3,C' '          ITS AN ORIG IF BLANK OR TEST                 
         BE    DOA10                                                            
         CLI   QOPT3,C'T'                                                       
         BE    DOA10                                                            
         MVI   AORIG,C' '          CLEAR ORIG INDICATOR                         
         CLI   QOPT3,C'R'                                                       
         BNE   *+8                                                              
         MVI   AREPL,C'1'          MARK TAPE AS A REPLACEMENT                   
         CLI   QOPT3,C'C'                                                       
         BNE   *+8                                                              
         MVI   ACORRECT,C'1'       MARK TAPE AS A CORRECTION                    
*                                                                               
DOA10    MVC   ACOMPNM,D1CNM       COMPANY NAME                                 
         MVI   AZER,C'0'           NOT A TRANSFER AGENT                         
         MVC   ACADD1,D1CADLN1     STREET ADDRESS                               
         MVC   ACADCTY(L'D1CADCTY),D1CADCTY    CITY                             
         MVC   ACADST,D1CADST                      STATE                        
         MVC   ACADZIP,D1CADZIP                         ZIP                     
         MVC   ACPHONE,SRECPHON    PHONE NUMBER                                 
         UNPK  ARECSEQ,RECSEQ      RECORD SEQUENCE NUMBER                       
         OI    ARECSEQ+7,X'F0'                                                  
*                                                                               
* WRITE RECORD TO DISK/TAPE                                                     
*                                                                               
         LA    R5,AREC                                                          
         MVI   BYTE,C'W'           INDICATOR TO WRITE WORK FILE REC             
         BAS   RE,WTREC                                                         
*                                                                               
         ZAP   BRECOUNT,=P'0'                                                   
         AP    TOTACNT,=P'1'       INCREMENT PAYER RECORD COUNT.                
*                                                                               
         LA    R0,BTOT#                                                         
         LA    R1,BTOTALS                                                       
         ZAP   0(L'BTOTALS,R1),=P'0'                                            
         LA    R1,L'BTOTALS(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
*        MVC   MSG,=CL10'AREC'                                                  
*        GOTO1 ADUMP,DMCB,(RC),AREC,750                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* BUILD 'B' RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
DOBREC   NTR1                                                                   
         LA    R0,SPACES           A(SOURCE)                                    
         SR    R1,R1               L'SOURCE                                     
         ICM   R1,8,SPACES         PAD CHAR = C' '                              
         LA    RE,WKREC            A(DESTINATION)                               
         LA    RF,L'WKREC          L'DESTINATION                                
         MVCL  RE,R0                                                            
*                                                                               
         USING BREC,R6             CLEAR RECORD BUFFER                          
         LA    R6,WKREC                                                         
         MVI   BREC,C'B'           B TYPE RECORD                                
         MVC   BYEAR,D1YEAR        1993                                         
         CLI   QOPT3,C'C'                                                       
         BNE   *+8                                                              
         MVI   BCORR,C'G'          SET CORRECTED RETURN INDICATOR               
         MVC   BNMCTL,SPACES       NAME CONTROL                                 
         MVI   BTINTYPE,C'1'       SET FOR EIN NO.                              
         CLI   D1SSORID,C'I'       IS IT ID NO                                  
         BE    *+8                                                              
         MVI   BTINTYPE,C'2'       SET FOR SS NO.                               
         CLI   D1FORGN,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BFORGN,C'1'         SET FOREIGN ADDRESS INDICATOR                
         MVC   BSSNO,SRECRID       EIN OR SS #                                  
         MVC   BACCT(12),SRECRACT  DDS ACCT                                     
         MVC   BAMT1,ZEROS                                                      
         MVC   BAMT2,ZEROS                                                      
         MVC   BAMT3,ZEROS                                                      
         MVC   BAMT4,ZEROS                                                      
         MVC   BAMT5,ZEROS                                                      
         MVC   BAMT6,ZEROS                                                      
         MVC   BAMT7,ZEROS                                                      
         MVC   BAMT8,ZEROS                                                      
         MVC   BAMT9,ZEROS                                                      
         MVC   BAMTA,ZEROS                                                      
         MVC   BAMTB,ZEROS                                                      
         MVC   BAMTC,ZEROS                                                      
         MVC   BNAME,D1RNM                                                      
         MVC   BNAME2,SPACES                                                    
         BAS   RE,CONDENSE         IRS WANTS ONLY 40 CHARS SO ELIMINATE         
         MVC   BADDR,ADDRST        ATTN, C/O AND OTHER NONESSENTIALS            
*                                                                               
         MVC   BCTY(L'D1RADCTY),D1RADCTY     CITY                               
         MVC   BST,D1RADST                       STATE                          
         MVC   BZIP,D1RADZIP                          ZIP                       
*                                                                               
         XC    BSRCLIST,BSRCLIST   CLEAR OUT SOURCE LIST AREA                   
         LA    R1,LSREC                                                         
         MOVE  (SVSREC,(R1)),SRECA   SAVE SRT REC FOR FORMS.                    
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONDENSE 3 ADDRESS LINES INTO 1 40 CHAR LINE FOR B RECS            *          
**********************************************************************          
         SPACE 1                                                                
* ADDRESS SHOULD BE RECIPIENTS IN D1RADLN1-3                                    
* 1 LINE ADDRESS IS RETURNED IN ADDRST (D1RADLN1-3) IS UNTOUCHED                
*                                                                               
CONDENSE NTR1                                                                   
         MVC   ADDRST,D1RADLN1                                                  
         MVC   ADDRST2,D1RADLN2                                                 
         MVC   ADDRST3,D1RADLN3                                                 
         MVC   ADDRCSZ,SPACES                                                   
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         LA    R0,3                                                             
         LA    R5,ADDRST                                                        
CONDEN10 MVC   WORK2(L'D1RADLN1),0(R5)                                          
         BAS   RE,CONSCAN                                                       
         MVC   0(L'D1RADLN1,R5),WORK2                                           
         LA    R5,L'ADDRST(R5)                                                  
         BCT   R0,CONDEN10                                                      
         MVC   WORK2,ADDRST        CRUNCH ENTIRE ADDRESS                        
         GOTO1 SQUASHER,DMCB,WORK2,L'WORK2                                      
         MVC   ADDRST,WORK2                                                     
         B     EXIT                                                             
*                                                                               
* SCAN FOR ANY MATCH IN EXCLUSION TABLE - RETURN BLANKS IF MATCH                
*                                                                               
         USING EXTTBLD,R2                                                       
CONSCAN  NTR1                                                                   
         L     R2,AEXTTBL          TABLE OF ITEMS TO EXCLUDE                    
*                                                                               
CON50    LA    R4,L'D1RADLN1       MAX #CHARS                                   
         LA    R3,WORK2                                                         
*                                                                               
CON100   CLI   0(R2),X'FF'         END OF EXCLUSION TABLE                       
         BE    EXIT                                                             
         ZIC   R1,EXTLEN                                                        
         SH    R1,=H'1'                                                         
         EXCLC R1,EXTENT,0(R3)                                                  
         BE    CON200                                                           
         LA    R3,1(R3)            BUMP TO NEXT CHAR                            
         BCT   R4,CON100                                                        
         ZIC   R1,EXTLEN                                                        
         LA    R1,1(R1)                                                         
         AR    R2,R1               BUMP TO NEXT TABLE ENTRY                     
         B     CON50                                                            
*                                                                               
CON200   DS    0H                  IF MATCH WAS FOUND THEN CLEAR OUT            
         MVI   WORK2,C' '          ENTIRE LINE                                  
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* WRITE 'B' RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
WRITBREC NTR1                                                                   
         USING BREC,R6                                                          
         LA    R6,WKREC                                                         
*                                                                               
         CLI   QOPT2,C'Y'           SUPPRESS UNDER $600                         
         BNE   *+14                 OPTION NOT ON,CONTINUE                      
         CP    PAYERAMT,=P'60000'                                               
         BL    WB200                **SUPPRESS**                                
*                                                                               
         CLI   BSSNO,X'FF'          IF NO ID, THEN DON'T TOTAL                  
         BE    WB5                                                              
         CP    PAYERAMT,=P'0'                                                   
         BNL   WB10                                                             
         MVI   BCORR,X'FF'         FLAG NEGATIVE AMTS                           
WB5      UNPK  WORK(12),PAYERAMT                                                
         OI    WORK+11,X'F0'                                                    
         MVC   BAMT7,WORK                                                       
         B     WB30                                                             
*                                                                               
WB10     AP    BTOTALS,PAYERAMT                                                 
*                                                                               
WB20     UNPK  WORK(12),PAYERAMT                                                
         OI    WORK+11,X'F0'                                                    
*                                                                               
         TM    SVPTYPE,RSTXRENT                                                 
         BNO   *+20                                                             
         MVC   BAMT1,WORK          RENT                                         
         AP    BTOT1,PAYERAMT                                                   
         B     WB30                                                             
*                                                                               
         TM    SVPTYPE,RSTXROYL                                                 
         BNO   *+20                                                             
         MVC   BAMT2,WORK          ROYALTIES                                    
         AP    BTOT2,PAYERAMT                                                   
         B     WB30                                                             
*                                                                               
         TM    SVPTYPE,RSTXPRIZ                                                 
         BNO   *+20                                                             
         MVC   BAMT3,WORK          PRIZES                                       
         AP    BTOT3,PAYERAMT                                                   
         B     WB30                                                             
*                                                                               
         TM    SVPTYPE,RSTXMEDC                                                 
         BNO   *+20                                                             
         MVC   BAMT6,WORK          MEDICAL EXPENSES                             
         AP    BTOT6,PAYERAMT                                                   
         B     WB30                                                             
*                                                                               
         MVC   BAMT7,WORK          NON EMPLOYEE COMPENSATION (DEFAULT)          
         AP    BTOT7,PAYERAMT                                                   
*                                                                               
WB30     CLC   WORK(10),ZEROS                                                   
         BE    EXIT                IF $0 SKIP RECORD                            
*                                                                               
         CLI   BSSNO,X'FF'          IF NO ID, THEN DON'T COUNT                  
         BE    WB50                                                             
         CLI   BCORR,X'FF'          IF NEG, THEN DON'T COUNT                    
         BE    WB50                                                             
         AP    BRECOUNT,=P'1'      INCREMENT AGENCY RECORD COUNT.               
         AP    TOTBCNT,=P'1'       INCREMENT TOTAL  RECORD COUNT.               
         AP    RECSEQ,=P'1'        INCREMENT RECORD SEQUENCE NUMBER             
         UNPK  BRECSEQ,RECSEQ      RECORD SEQUENCE NUMBER                       
         OI    BRECSEQ+7,X'F0'                                                  
*                                                                               
* WRITE RECORD TO DISK.                                                         
*                                                                               
WB50     LA    R5,BREC                                                          
         MVI   BYTE,C'W'            INDICATOR TO WRITE WORK FILE REC            
         BAS   RE,WTREC                                                         
*                                                                               
* IF TAPE REQUESTED THEN WRITE RECORD TO TAPE.                                  
*                                                                               
         CLI   BSSNO,X'FF'                                                      
         BE    WB200               IF NO SSNO THEN DON'T WRITE TO TAPE.         
         CLI   BCORR,X'FF'                                                      
         BE    WB200               IF NEG THEN DON'T WRITE TO TAPE.             
*                                                                               
*  IF FORMS SUPRESSED THEN DON'T PRINT IT.                                      
*                                                                               
WB100    BAS   RE,DOFORM           PRINT OUT 1099 FORM.                         
*                                                                               
WB200    ZAP   PAYERAMT,=P'0'      CLEAR PAYER BUCKET.                          
*        MVC   MSG,=CL10'BREC'                                                  
*        GOTO1 ADUMP,DMCB,(RC),BREC,750                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* BUILD 'C' RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
DOCREC   NTR1                                                                   
         BAS   RE,WRITBREC         FIRST, WRITE LAST BREC FOR THIS COMP         
         LA    R0,SPACES           A(SOURCE)                                    
         SR    R1,R1               L'SOURCE                                     
         ICM   R1,8,SPACES         PAD CHAR = C' '                              
         LA    RE,WKREC            A(DESTINATION)                               
         LA    RF,L'WKREC          L'DESTINATION                                
         MVCL  RE,R0                                                            
*                                                                               
         USING CREC,R6                                                          
         LA    R6,WKREC                                                         
*                                                                               
         LA    R1,CAMTFLD1         CLEAR FIELDS TO ZERO                         
         LA    R0,CAMTQ                                                         
         MVC   0(L'CAMTFLD1,R1),ZEROS                                           
         LA    R1,L'CAMTFLD1(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
         UNPK  CAMT1(16),BTOT1                                                  
         OI    CAMT1+15,X'F0'                                                   
         UNPK  CAMT2(16),BTOT2                                                  
         OI    CAMT2+15,X'F0'                                                   
         UNPK  CAMT3(16),BTOT3                                                  
         OI    CAMT3+15,X'F0'                                                   
         UNPK  CAMT4(16),BTOT4                                                  
         OI    CAMT4+15,X'F0'                                                   
         UNPK  CAMT5(16),BTOT5                                                  
         OI    CAMT5+15,X'F0'                                                   
         UNPK  CAMT6(16),BTOT6                                                  
         OI    CAMT6+15,X'F0'                                                   
         UNPK  CAMT7(16),BTOT7                                                  
         OI    CAMT7+15,X'F0'                                                   
         UNPK  CAMT8(16),BTOT8                                                  
         OI    CAMT8+15,X'F0'                                                   
         UNPK  CAMT9(16),BTOT9                                                  
         OI    CAMT9+15,X'F0'                                                   
         UNPK  CAMTA(16),BTOTA                                                  
         OI    CAMTA+15,X'F0'                                                   
         UNPK  CAMTB(16),BTOTB                                                  
         OI    CAMTB+15,X'F0'                                                   
         UNPK  CAMTC(16),BTOTC                                                  
         OI    CAMTC+15,X'F0'                                                   
*                                                                               
         AP    RECSEQ,=P'1'        INCREMENT RECORD SEQUENCE NUMBER             
         UNPK  CRECSEQ,RECSEQ      RECORD SEQUENCE NUMBER                       
         OI    CRECSEQ+7,X'F0'                                                  
*                                                                               
         MVI   CREC,C'C'                                                        
         UNPK  CBCOUNT,BRECOUNT                                                 
         OI    CBCOUNT+7,X'F0'                                                  
*                                                                               
* WRITE RECORD TO DISK.                                                         
*                                                                               
         LA    R5,CREC                                                          
         MVI   BYTE,C'W'           INDICATOR TO WRITE WORK FILE REC             
         BAS   RE,WTREC                                                         
*                                                                               
DC100    MVI   FIRSTPRT,C'Y'          IF NEW COMPANY, REALIGN FORMS.            
*        MVC   MSG,=CL10'CREC'                                                  
*        GOTO1 ADUMP,DMCB,(RC),CREC,750                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* BUILD 'F' RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
DOFREC   NTR1                                                                   
         BAS   RE,DOCREC           FIRST, WRITE LAST CREC FOR THIS TAPE         
         LA    R0,SPACES           A(SOURCE)                                    
         SR    R1,R1               L'SOURCE                                     
         ICM   R1,8,SPACES         PAD CHAR = C' '                              
         LA    RE,WKREC            A(DESTINATION)                               
         LA    RF,L'WKREC          L'DESTINATION                                
         MVCL  RE,R0                                                            
*                                                                               
         USING FREC,R6                                                          
         LA    R6,WKREC                                                         
         MVI   FREC,C'F'                                                        
         MVI   FREC+1,C'0'                                                      
         MVC   FREC+2(28),FREC+1                                                
*                                                                               
         AP    RECSEQ,=P'1'        INCREMENT RECORD SEQUENCE NUMBER             
         UNPK  FRECSEQ,RECSEQ      RECORD SEQUENCE NUMBER                       
         OI    FRECSEQ+7,X'F0'                                                  
*                                                                               
         UNPK  FACOUNT,TOTACNT                                                  
         OI    FACOUNT+7,X'F0'                                                  
*                                                                               
* WRITE RECORD TO DISK.                                                         
*                                                                               
         LA    R5,FREC                                                          
         MVI   BYTE,C'W'           INDICATOR TO WRITE WORK FILE REC             
         BAS   RE,WTREC                                                         
*                                                                               
DF100    DS    0H                                                               
*        MVC   MSG,=CL10'FREC'                                                  
*        GOTO1 ADUMP,DMCB,(RC),FREC,750                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT LINEUP FORM/1099 FORM                                        *          
**********************************************************************          
         SPACE 1                                                                
DOFORM   NTR1                                                                   
         CLI   QOPT7,C'Y'          DOWNLOADING - THEN NO FORMS                  
         BE    EXIT                                                             
         CLI   QOPT1,C'R'                                                       
         BE    DFM200                                                           
         CLI   FIRSTPRT,C'Y'       LINE-UP PATTERN FIRST-TIME                   
         BNE   DFM200                                                           
         L     R9,ASTARS                                                        
*                                                                               
         LA    R2,6                PRINT SIX TEST PATTERNS                      
DFM100   DS    0H                                                               
         BAS   RE,PRNT                                                          
         BCT   R2,DFM100                                                        
         MVI   FIRSTPRT,C'N'       TURN OFF FIRST TIME SWITCH                   
*                                                                               
DFM200   LA    R9,SVSREC           REAL PRINT                                   
         ZAP   D1RAMT,PAYERAMT                                                  
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT 1099 FORM                                                    *          
**********************************************************************          
         SPACE 1                                                                
PRNT     NTR1                      CLEAR OUT BUFFER                             
         LA    R0,SPACES           A(SOURCE)                                    
         SR    R1,R1               L'SOURCE                                     
         ICM   R1,8,SPACES         PAD CHAR = C' '                              
         L     RE,ABUF1099         A(DESTINATION)                               
         LHI   RF,BFLNQ            L'DESTINATION                                
         MVCL  RE,R0                                                            
*                                                                               
         USING BUFFERD,R6                                                       
         L     R6,ABUF1099                                                      
         CLI   QOPT3,C'C'          OPTION FOR CORRECTED FORM                    
         BNE   *+8                                                              
         MVI   BFCRRCTD,C'X'       MARK PRINTED FORM 'CORRECTED'                
*                                                                               
         MVC   BFCMPNM,D1CNM       COMPANY NAME                                 
         MVC   ADDRST,D1CADLN1     1ST LINE OF COMPANY ADDRESS                  
         MVC   ADDRST2,D1CADLN2    2ND LINE OF COMPANY ADDRESS                  
         MVC   ADDRST3,SPACES                                                   
         MVC   ADDRCSZ,D1CADLN3    3RD LINE OF COMPANY ADDRESS                  
         LA    R4,ADDRCSZ                                                       
         BAS   RE,COMMAS           INSERT COMMA/SPACE CITY OUT                  
*                                                                               
         LA    R3,ADDRST           FIRST LINE OF COMPANY ADDRESS                
         LA    R0,4                # INPUT LINES                                
         LA    R2,BFCMPAD1         FIRST LINE OF PRINT BUFFER                   
PRNT10   MVC   0(L'BFCMPAD1,R2),0(R3)                                           
         CLC   0(L'BFCMPAD1,R2),SPACES                                          
         BE    *+8                 SKIP A LINE ONLY IF SMTHNG PRNTED            
         LA    R2,132(R2)                                                       
         LA    R3,L'ADDRST(R3)                                                  
         BCT   R0,PRNT10                                                        
*                                                                               
         C     R9,ASTARS           DON'T PUT PHONE NUMBER FOR STARS             
         BNE   *+12                                                             
         MVI   BFCRRCTD,C'*'                                                    
         B     PRNT20                                                           
         LA    R1,BFPHON2          ASSUME 3 LINES OF ADDRESS                    
         CLI   D1CADNL,3                                                        
         BE    *+8                                                              
         LA    R1,BFPHON1                                                       
         MVC   0(L'BFPHON1,R1),SRECPHON                                         
*                                                                               
PRNT20   MVC   BFCIDNO(2),SRECCID  COMPANY ID #  EX/ 99-9999999                 
         MVI   BFCIDNO+2,C'-'                                                   
         MVC   BFCIDNO+3(7),SRECCID+2                                           
*                                                                               
         CLI   D1SSORID,C'I'       IS IT A FED ID NO                            
         BNE   PRNT30                                                           
         MVC   BFSSNO(2),SRECRID   ID NO.                                       
         MVI   BFSSNO+2,C'-'                                                    
         MVC   BFSSNO+3(7),SRECRID+2                                            
         B     PRNT40                                                           
*                                                                               
PRNT30   MVC   BFSSNO(3),SRECRID   SOCIAL SECURITY NUMBER                       
         MVI   BFSSNO+3,C'-'                                                    
         MVC   BFSSNO+4(2),SRECRID+3                                            
         MVI   BFSSNO+6,C'-'                                                    
         MVC   BFSSNO+7(4),SRECRID+5                                            
*                                                                               
PRNT40   DS    0H                                                               
         MVC   BFYEAR,SVCDTE       YYYY                                         
*                                                                               
         CLI   FIRSTPRT,C'Y'       LINE-UP PATTERN FIRST-TIME                   
         BNE   *+10                                                             
         MVC   BFSINBOX(14),=CL14'PUT IN BOX--*'                                
         MVC   BFRNAME,D1RNM       RECIPIENTS NAME / ADDRESS                    
*                                                                               
         LA    R4,BFNONEMP         NON EMPLOYEE COMPENSATION (DEFAULT)          
         TM    SVPTYPE,RSTXRENT                                                 
         BZ    *+8                                                              
         LA    R4,BFRENT           RENT                                         
         TM    SVPTYPE,RSTXROYL                                                 
         BZ    *+8                                                              
         LA    R4,BFROYAL          ROYALTIES                                    
         TM    SVPTYPE,RSTXPRIZ                                                 
         BZ    *+8                                                              
         LA    R4,BFPRIZES         PRIZES                                       
         TM    SVPTYPE,RSTXMEDC                                                 
         BZ    *+8                                                              
         LA    R4,BFMEDIC          MEDICAL EXPENSES                             
         EDIT  (P6,D1RAMT),(12,(R4)),2                                          
*                                                                               
         CLC   D1RADLN1,SPACES     REMOVE BLANK LINES                           
         BNE   PRNT50                                                           
         MVC   D1RADLN1,D1RADLN2   SHIFT UP ALL LINES                           
         MVC   D1RADLN2,D1RADLN3                                                
         MVC   D1RADLN3,SPACES                                                  
PRNT50   CLC   D1RADLN2,SPACES                                                  
         BNE   *+16                                                             
         MVC   D1RADLN2,D1RADLN3   SHIFT UP 3RD LINE                            
         MVC   D1RADLN3,SPACES                                                  
*                                                                               
         LA    R0,3                                                             
         LA    R1,D1RADLN1         COUNT THE NUMBER OF LINES USED               
         LA    R2,0                                                             
PRNT60   CLC   0(L'D1RADLN1,R1),SPACES                                          
         BE    PRNT70                                                           
         LA    R2,1(R2)            INCREMENT COUNTER                            
         LA    R1,L'D1RADLN1(R1)   NEXT LINE                                    
         BCT   R0,PRNT60                                                        
*                                                                               
PRNT70   DS    0H                  R2 HAS #LINES                                
         OR    R2,R2                                                            
         BZ    PRNT100                                                          
         LA    R1,BFRADD2          FOR 1 OR 2 LINES OF ADDRESS                  
         C     R2,=F'1'            START ON 2ND LINE                            
         BE    PRNT80                                                           
         C     R2,=F'2'                                                         
         BE    PRNT80                                                           
         LA    R1,BFRADD1          ELSE START ON 1ST LINE                       
*                                                                               
PRNT80   DS    0H                                                               
         LA    R3,D1RADLN1                                                      
*                                                                               
PRNT90   MVC   0(L'BFRADD1,R1),0(R3)                                            
         LA    R1,132(R1)                                                       
         LA    R3,L'D1RADLN1(R3)                                                
         BCT   R2,PRNT90                                                        
*                                                                               
PRNT100  MVC   ADDRCSZ,D1RADLN4    CITY, STATE ZIP ALWAYS ON 4TH LINE           
         LA    R4,ADDRCSZ                                                       
         CLI   D1FORGN,C'Y'        NO EXTRA FORMATING FOR FOREIGN ADDRS         
         BE    *+8                                                              
         BAS   RE,COMMAS           INSERT COMMA/SPACE CITY OUT                  
         MVC   BFRADD4,ADDRCSZ                                                  
*                                                                               
         CLI   PROGPROF,C'Y'       PRINT ACCOUNT ON 1099 FORM                   
         BNE   *+10                                                             
         MVC   BFACCT(12),SRECRACT                                              
*                                                                               
         CLI   QOPT1,C'R'          DONT PRINT FORM IF ONLY WANT REGSTER         
         BE    PRNT120                                                          
*                                                                               
         USING BUFFERD,R6                                                       
         L     R6,ABUF1099         PRINT OUT 1099 FORM                          
         LA    R0,BFEQU            NUMBER OF LINES                              
PRNT110  MVC   P,0(R6)             MOVE IN PRINT LINE                           
         BAS   RE,WRITE                                                         
         LA    R6,132(R6)          BUMP TO NEXT LINE                            
         BCT   R0,PRNT110                                                       
*                                                                               
PRNT120  CLI   QOPT2,C'Y'           IF ITEMS<$600 = Y THEN DONT                 
         BNE   *+14                 PRINT ON INVALID ADDRESS REPORT             
         CP    D1RAMT,=P'60000'                                                 
         BL    PTRNX                                                            
*                                                                               
         CLC   D1RADLN4,=CL40'*** INVALID ADDRESS ***'                          
         BNE   PTRNX                                                            
         CP    INVCNT,=P'100'      MAX NUMBER OF ERRORS TO PRINT                
         BNL   PTRNX                                                            
         AP    INVCNT,=P'1'        BUMP COUNT OF ERRORS                         
         USING INVADDD,R1                                                       
         L     R1,AINVENT                                                       
         MVC   INVACCT,SRECRACT    SAVE ACCT WHERE ERROR IS                     
         MVC   INVNAME,D1RNM       SAVE NAME OF ACCOUNT                         
         LA    R1,INVEQU(R1)                                                    
         ST    R1,AINVENT                                                       
*                                                                               
PRNTX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT LINE                                                         *          
**********************************************************************          
         SPACE 1                                                                
WRITE    NTR1                                                                   
         MVC   PATTERN,=C'BL01'    SINGLE SPACE                                 
         GOTO1 PRINT,DMCB,P,PATTERN                                             
         MVC   P,SPACES                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* FORMAT CITY, STATE ZIP                                             *          
**********************************************************************          
         SPACE 1                                                                
* INPUT = 40 CHAR STRING WITH CITY(29),STATE(2),ZIP(9)                          
* OUTPUT= 40 CHAR STRING WITH CITY, STATE ZIP                                   
*                                                                               
COMMAS   NTR1                      R4 POINTS AT INPUT                           
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         MVC   WORK2(29),0(R4)                                                  
         CLI   WORK2,C'*'          ERROR MESSAGE?                               
         BE    EXIT                                                             
         LA    R1,WORK2+28                                                      
         LA    R0,29                                                            
COMMA10  CLI   0(R1),C' '                                                       
         BNE   COMMA20                                                          
         SH    R1,=H'1'                                                         
         BCT   R0,COMMA10                                                       
         B     EXIT                                                             
*                                                                               
COMMA20  MVI   1(R1),C','                                                       
         MVC   WORK2+30(2),29(R4) FORMAT ZIP TO 10965                           
         MVC   WORK2+40(5),31(R4)                                               
         CLC   36(4,R4),SPACES     CHECK FOR EXTENDED ZIPCODE                   
         BE    COMMA30                                                          
         MVI   WORK2+45,C'-'       OR FORMAT TO 10965-1076                      
         MVC   WORK2+46(4),36(R4)                                               
*                                                                               
COMMA30  GOTO1 SQUASHER,DMCB,WORK2,50                                           
         MVC   0(40,R4),WORK2                                                   
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* FILE/TAPE ROUTINES (OPEN/CLOSE)                                    *          
**********************************************************************          
         SPACE 1                                                                
OPEN     NTR1                   OPEN WORK FILE (AND TAPE IF NECESSARY)          
         OPEN  (ACT99WK,(OUTPUT))                                               
         CLI   QOPT4,C'T'                                                       
         BNE   OPX                                                              
         MVI   DSPARM+15,C'1'                                                   
         MVC   DSPARM+12(2),D1COMPCD                                            
         MVC   DSPARM+14(1),QOPT5                                               
         CLC   ALPHAID,=C'*B'      IF DDSB FORCE JW TAPE                        
         BNE   *+10                                                             
         MVC   DSPARM+12(3),=C'YNX'                                             
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),(0,DSPARM)                              
         OPEN  (ACT99TP,(OUTPUT))                                               
OPX      B     EXIT                                                             
*                                                                               
CLOSE    NTR1                                                                   
         CLI   BYTE,C'W'                                                        
         BNE   CLOSE05                                                          
         CLOSE ACT99WK                                                          
CLOSE05  CLI   QOPT7,C'Y'                                                       
         BNE   CLOSE10                                                          
         TM    DWNSTAT,DWNINIT                                                  
         BZ    CLOSE10                                                          
         GOTO1 ADWNL,DMCB,(RC),(R9),,DWNEOR                                     
*                                                                               
CLOSE10  CLI   BYTE,C'T'                                                        
         BNE   CLX                                                              
         CLI   QOPT4,C'T'                                                       
         BNE   CLX                                                              
         CLOSE ACT99TP                                                          
CLX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* GETEL#2                                                             *         
***********************************************************************         
         SPACE 1                                                                
         GETELN R2,DISP2,ELCODE,2                                               
         EJECT                                                                  
***********************************************************************         
* WRITE WORK RECORD                                                   *         
***********************************************************************         
         SPACE 1                                                                
WTREC    NTR1                                                                   
         CLI   BYTE,C'W'                                                        
         BNE   WT10                                                             
*        MVC   MSG,=CL10'WRKREC'                                                
*        GOTO1 ADUMP,DMCB,(RC),(R5),LSREC                                       
         PUT   ACT99WK,(R5)        WRITE WORK RECORD                            
         B     EXIT                                                             
*                                                                               
WT10     CLI   BYTE,C'T'                                                        
         BNE   WT20                                                             
*        MVC   MSG,=CL10'PUTTPE'                                                
*        GOTO1 ADUMP,DMCB,(RC),(R5),750                                         
         PUT   ACT99TP,(R5)        WRITE TAPE RECORD                            
         B     EXIT                                                             
*                                                                               
WT20     CLI   BYTE,C'D'           DOWNLOADING                                  
         BNE   WT30                                                             
         TM    DWNSTAT,DWNCMPY     DOWNLOADING                                  
         BNO   EXIT                                                             
         GOTO1 ADWNL,DMCB,(RC),(R9),(R5),DWNREC                                 
         B     EXIT                                                             
*                                                                               
WT30     DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT REGISTER & ERROR REPORTS                                     *          
**********************************************************************          
         SPACE 1                                                                
DOREG    NTR1                                                                   
*        CLI   QOPT7,C'Y'          DOWNLOADING - THEN NO REGISTER               
*        BE    EXIT                                                             
         OPEN  (ACT99WK,(INPUT))   OPEN WORK OR TAPE FILE.                      
         LA    R5,WKREC                                                         
*                                                                               
DOREG20  DS    0H                                                               
         GET   ACT99WK,(R5)        READ RECORD.                                 
         CLI   0(R5),C'T'                                                       
         BE    PTREC                                                            
         CLI   0(R5),C'A'                                                       
         BE    PAREC                                                            
         CLI   0(R5),C'B'                                                       
         BE    PBREC                                                            
         CLI   0(R5),C'C'                                                       
         BE    PCREC                                                            
         CLI   0(R5),C'F'                                                       
         BE    PFREC                                                            
         DC    H'0'                MUST BE A,B,C OR F                           
         EJECT                                                                  
**********************************************************************          
* PROCESS AND PRINT 'T' RECORD                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING TRECD,R5                                                         
PTREC    DS    0H                  PROCESS 'T' RECORD.                          
         UNPK  TBCOUNT,TOTBCNT                                                  
         OI    TBCOUNT+7,X'F0'                                                  
*                                                                               
         LA    RE,SRECA            SAVE OFF T REC IN SRECA                      
         LR    R0,R5                                                            
         LA    RF,L'SRECA                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   QOPT4,C'T'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'T'           INDICATOR TO WRITE TAPE REC                  
         BAS   RE,WTREC                                                         
         CLI   QOPT7,C'Y'          DOWNLOADING                                  
         BNE   DOREG20                                                          
         MVI   BYTE,C'D'                                                        
         BAS   RE,WTREC                                                         
         B     DOREG20                                                          
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS AND PRINT 'A' RECORD                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING TRECD,R4                                                         
         USING ARECD,R5                                                         
PAREC    DS    0H                  PRINT 'A' RECORD.                            
         CLI   QOPT7,C'Y'          DOWNLOADING - THEN NO REGISTER               
         BE    PAREC10                                                          
*                                                                               
         LA    R4,SRECA                                                         
         MVI   RCSUBPRG,4                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   SVCNM,ACOMPNM       SAVE IT FOR HEADLINE.                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+1(17),=C'TRANSMITTER CODE='                                    
         MVC   P+18(5),TTRID                                                    
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+66(40),TTRNNM                                                  
         MVC   P+25(40),ACOMPNM                                                 
         MVC   P+12(9),ACCOID                                                   
         MVC   P+1(10),=C'CMPNY ID#='                                           
         MVC   PSECOND+66(40),TCPYADD1                                          
         MVC   PSECOND+25(40),ACADD1                                            
         MVC   PSECOND+12(1),AFRMTYP                                            
         MVC   PSECOND+1(10),=C'FORM TYPE='                                     
*                                                                               
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(L'TCPYADD2),TCPYADD2                                       
         GOTO1 SQUASHER,DMCB,WORK2,L'WORK2                                      
         MVC   PTHIRD+66(40),WORK2                                              
*                                                                               
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(L'ACADD2),ACADD2                                           
         GOTO1 SQUASHER,DMCB,WORK2,L'WORK2                                      
         MVC   PTHIRD+25(40),WORK2                                              
*                                                                               
         MVC   PTHIRD+1(10),=C'AMT CODES='                                      
         MVC   PTHIRD+12(L'AAMTCD),AAMTCD                                       
         GOTO1 ACREPORT                                                         
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
PAREC10  LA    R0,BTOT#                                                         
         LA    R1,BTOTALS                                                       
         ZAP   0(L'BTOTALS,R1),=P'0'                                            
         LA    R1,L'BTOTALS(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         CLI   QOPT4,C'T'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'T'           INDICATOR TO WRITE TAPE REC                  
         BAS   RE,WTREC                                                         
         CLI   QOPT7,C'Y'          DOWNLOADING                                  
         BNE   DOREG20                                                          
         MVI   BYTE,C'D'                                                        
         BAS   RE,WTREC                                                         
         B     DOREG20                                                          
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS AND PRINT 'B' RECORD                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING BRECD,R5                                                         
PBREC    DS    0H                  PRINT 'B' RECORD.                            
         CLI   QOPT7,C'Y'          DOWNLOADING - THEN NO REGISTER               
         BE    PBREC120                                                         
*                                                                               
         CLI   BSSNO,X'FF'                                                      
         BNE   PBREC10                                                          
         CLI   RCSUBPRG,2                                                       
         BE    PBREC10                                                          
         MVC   P+1(25),=CL25'*** COMPANY TOTALS ***'                            
         EDIT  (P8,BTOTALS),(14,P+55),2,COMMAS=NO                               
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   RCSUBPRG,2          FOR FIRST FF RECORD SKIP PAGE.               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
PBREC10  LA    R0,SPACES           CLEAR OUT PRINT BUFFER                       
         SR    R1,R1               L'SOURCE                                     
         ICM   R1,8,SPACES         PAD CHAR = C' '                              
         L     RE,ABUF1099         A(DESTINATION)                               
         LA    RF,2000             L'DESTINATION                                
         MVCL  RE,R0                                                            
*                                                                               
         USING PRBRECD,R6                                                       
         L     R6,ABUF1099                                                      
         MVC   PRBYR,BYEAR         PRINT YEAR                                   
         MVC   PRBMCTL,BNMCTL                                                   
         MVC   PRBTTYPE,BTINTYPE   TIN TYPE                                     
         MVC   PRBNAME,BNAME                                                    
         MVC   PRBADDR,BADDR       ADDRESS                                      
         CLI   BSSNO,X'FF'                                                      
         BE    PBREC20                                                          
         MVC   PRBSSNO(3),BSSNO                                                 
         MVI   PRBSSNO+3,C'-'                                                   
         MVC   PRBSSNO+4(2),BSSNO+3                                             
         MVI   PRBSSNO+6,C'-'                                                   
         MVC   WORK(9),=9X'F0'                                                  
         MVZ   WORK(9),BSSNO                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BE    PBREC30                                                          
         MVC   PRBERROR,=CL30'*** INVALID TAX ID NUMBER ***'                    
         B     PBREC40                                                          
*                                                                               
PBREC20  MVC   PRBERROR,=CL30'*** MISSING TAX ID NUMBER ***'                    
         CLI   BSSNO,X'FF'                                                      
         BE    PBREC40                                                          
*                                                                               
PBREC30  MVZ   BSSNO+8(1),=X'F0'                                                
         MVC   PRBSSNO+7(4),BSSNO+5                                             
*                                                                               
PBREC40  MVC   WORK2,SPACES                                                     
         MVC   WORK2(L'BCSZ),BCSZ                                               
         GOTO1 SQUASHER,DMCB,WORK2,L'WORK2                                      
         MVC   PRBCSZ,WORK2                                                     
         CLI   BSSNO,X'FF'                                                      
         BE    PBREC50                                                          
         CLI   BCORR,X'FF'                                                      
         BNE   PBREC60                                                          
         MVC   PRBERROR,=CL30'*** NEGATIVE AMOUNT ***'                          
         OI    STATUS,STATNEG                                                   
PBREC50  PACK  WORK(6),BAMT7       NON EMPLOYEE COMPENSATION                    
         ZAP   AMOUNT,WORK(6)                                                   
         B     PBREC70                                                          
*                                                                               
PBREC60  ZAP   AMOUNT,=P'0'                                                     
         PACK  WORK(6),BAMT1       RENT                                         
         AP    AMOUNT,WORK(6)                                                   
         AP    BTOT1,WORK(6)                                                    
         PACK  WORK(6),BAMT2       ROYALTY ADDED JAN/2001                       
         AP    AMOUNT,WORK(6)                                                   
         AP    BTOT2,WORK(6)                                                    
         PACK  WORK(6),BAMT3       PRIZES                                       
         AP    AMOUNT,WORK(6)                                                   
         AP    BTOT3,WORK(6)                                                    
         PACK  WORK(6),BAMT6       MEDICAL                                      
         AP    AMOUNT,WORK(6)                                                   
         AP    BTOT6,WORK(6)                                                    
         PACK  WORK(6),BAMT7       NON EMPLOYEE COMPENSATION                    
         AP    AMOUNT,WORK(6)                                                   
         AP    BTOT7,WORK(6)                                                    
         AP    BTOTALS,AMOUNT                                                   
PBREC70  EDIT  AMOUNT,PRBAMT,2,COMMAS=NO                                        
*                                                                               
         USING SRCED,R4                                                         
         LA    R4,BSRCLIST                                                      
         USING PRBRECD,R6                                                       
         L     R6,ABUF1099                                                      
         MVC   PRBSRCE,BACCT                                                    
         CLI   BSSNO,X'FF'                                                      
         BE    PBREC90                                                          
*                                                                               
         LA    R3,SRCEMAX          MAX NUMBER                                   
PBREC80  CLI   0(R4),0                                                          
         BE    PBREC90                                                          
         MVC   PRBSRCE,SRCEACCT                                                 
         EDIT  SRCEAMNT,PRBAMT2,2,COMMAS=NO,FLOAT=-                             
         LA    R4,SRCELNQ(R4)      NEXT ACCT                                    
         LA    R6,PRBLNQ1(R6)      NEXT LINE                                    
         BCT   R3,PBREC80                                                       
*                                                                               
PBREC90  L     R6,ABUF1099                                                      
         LA    R3,SRCEMAX          MAX NUMBER PRINT LINES                       
*                                                                               
PBREC100 CLC   0(L'P,R6),SPACES    ANYTHING TO PRINT?                           
         BE    PBREC110                                                         
         MVC   P,0(R6)             MOVE IN PRINT LINE                           
         GOTO1 ACREPORT                                                         
         LA    R6,132(R6)          BUMP TO NEXT LINE                            
         BCT   R3,PBREC100                                                      
*                                                                               
PBREC110 DS    0H                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
PBREC120 CLI   BSSNO,X'FF'                                                      
         BE    DOREG20             IF NO SSNO THEN DON'T WRITE TO TAPE.         
         CLI   BCORR,X'FF'                                                      
         BE    DOREG20             IF NEG THEN DON'T WRITE TO TAPE.             
*                                                                               
         CLI   QOPT4,C'T'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'T'           INDICATOR TO WRITE TAPE REC                  
         BAS   RE,WTREC                                                         
         CLI   QOPT7,C'Y'          DOWNLOADING                                  
         BNE   DOREG20                                                          
         MVI   BYTE,C'D'                                                        
         BAS   RE,WTREC                                                         
         B     DOREG20                                                          
         EJECT                                                                  
**********************************************************************          
* PROCESS AND PRINT 'C' RECORD                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING CRECD,R5                                                         
PCREC    DS    0H                  PRINT 'C' RECORD.                            
         CLI   QOPT7,C'Y'          DOWNLOADING - THEN NO REGISTER               
         BE    PCREC30                                                          
*                                                                               
         CLI   RCSUBPRG,2                                                       
         BE    PCREC10             B IF WE PRINTED COMP TOTALS ALREADY.         
         MVC   P+1(25),=CL25'*** COMPANY TOTALS ***'                            
         EDIT  (P8,BTOTALS),(14,P+55),2,COMMAS=NO                               
         GOTO1 ACREPORT                                                         
*                                                                               
PCREC10  GOTO1 ACREPORT                                                         
         CLI   QOPT8,C'A'                                                       
         BNE   PCREC20                                                          
         MVC   P+1(9),=CL9'BTOT1 = '                                            
         EDIT  (P8,BTOT1),(14,P+10),2,COMMAS=NO                                 
         MVC   P+31(9),=CL9'BTOT3 = '                                           
         EDIT  (P8,BTOT3),(14,P+40),2,COMMAS=NO                                 
         MVC   P+61(9),=CL9'BTOT6 = '                                           
         EDIT  (P8,BTOT6),(14,P+70),2,COMMAS=NO                                 
         MVC   P+91(9),=CL9'BTOT7 = '                                           
         EDIT  (P8,BTOT7),(14,P+100),2,COMMAS=NO                                
         GOTO1 ACREPORT                                                         
*                                                                               
         PACK  WORK(8),CAMT1(16)                                                
         ZAP   AMOUNT,WORK(8)                                                   
         MVC   P+1(9),=CL9'CAMT1 = '                                            
         EDIT  (P8,AMOUNT),(14,P+10),2,COMMAS=NO                                
         PACK  WORK(8),CAMT3(16)                                                
         ZAP   AMOUNT,WORK(8)                                                   
         MVC   P+31(9),=CL9'CAMT3 = '                                           
         EDIT  (P8,AMOUNT),(14,P+40),2,COMMAS=NO                                
         PACK  WORK(8),CAMT6(16)                                                
         ZAP   AMOUNT,WORK(8)                                                   
         MVC   P+61(9),=CL9'CAMT6 = '                                           
         EDIT  (P8,AMOUNT),(14,P+70),2,COMMAS=NO                                
         PACK  WORK(8),CAMT7(16)                                                
         ZAP   AMOUNT,WORK(8)                                                   
         MVC   P+91(9),=CL9'CAMT7 = '                                           
         EDIT  (P8,AMOUNT),(14,P+100),2,COMMAS=NO                               
         GOTO1 ACREPORT                                                         
*                                                                               
PCREC20  MVC   P(34),=C'NUMBER OF PAYEES FOR THIS COMPANY='                     
         MVC   P+35(8),CBCOUNT                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
PCREC30  CLI   QOPT4,C'T'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'T'           INDICATOR TO WRITE TAPE REC                  
         BAS   RE,WTREC                                                         
         CLI   QOPT7,C'Y'          DOWNLOADING                                  
         BNE   DOREG20                                                          
         MVI   BYTE,C'D'                                                        
         BAS   RE,WTREC                                                         
         B     DOREG20                                                          
         EJECT                                                                  
**********************************************************************          
* PROCESS AND PRINT 'F' RECORD                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING FREC,R5                                                          
PFREC    DS    0H                  PRINT 'F' RECORD.                            
         CLI   QOPT7,C'Y'          DOWNLOADING - THEN NO REGISTER               
         BE    PFREC20                                                          
*                                                                               
         MVC   P(16),=C'** "F" RECORD **'                                       
         GOTO1 ACREPORT                                                         
         CLOSE ACT99WK                                                          
*                                                                               
         TM    STATUS,STATNEG                                                   
         BZ    PFREC10                                                          
         GOTO1 ACREPORT                                                         
         MVC   P(60),=CL60'*** NEGATIVE AMOUNT IN CURRENT RUN ***'              
         GOTO1 ACREPORT                                                         
*                                                                               
PFREC10  BAS   RE,FORMINV          PRINT INVALID ADDRESS LIST                   
         BAS   RE,FORM4804         PRINT 4804 FORM                              
*                                                                               
PFREC20  CLI   QOPT4,C'T'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'T'           INDICATOR TO WRITE TAPE REC                  
         BAS   RE,WTREC                                                         
         CLI   QOPT7,C'Y'          DOWNLOADING                                  
         BNE   EXIT                                                             
         MVI   BYTE,C'D'                                                        
         BAS   RE,WTREC                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* FILTER RECORDS                                                     *          
**********************************************************************          
         SPACE 1                                                                
SETFILTR LTR   R1,R1                                                            
         BZR   RE                                                               
         USING ACSTATD,R1                                                       
         CLI   ACSTFILT,X'41'                                                   
         BL    *+10                                                             
         MVC   FILT1,ACSTFILT                                                   
         CLI   ACSTFILT+1,X'41'                                                 
         BL    *+10                                                             
         MVC   FILT2,ACSTFILT+1                                                 
         CLI   ACSTANAL,X'41'                                                   
         BL    *+10                                                             
         MVC   FILT3,ACSTANAL                                                   
         CLI   ACSTSUB,X'41'                                                    
         BL    *+10                                                             
         MVC   FILT4,ACSTSUB                                                    
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* LOOKUP ITEM IN COMPANY TABLE                                       *          
**********************************************************************          
         SPACE 1                                                                
LOOKUP   NTR1                                                                   
         USING CPYTABD,R3                                                       
         L     R3,ACPYTAB          COMPANY TABLE                                
*                                                                               
LOOK10   CLC   CALPHA,=C'**'       END OF TABLE                                 
         BNE   LOOK20                                                           
         MVI   SKIPACCT,C'Y'       SKIP THIS ACCOUNT                            
         B     LOOKX                                                            
*                                                                               
LOOK20   CLC   CALPHA,ALPHAID      MATCH ON AGENCY ALPHA                        
         BE    LOOK40                                                           
LOOK30   LA    R3,CTLENQ(R3)       NEXT TABLE ENTRY                             
         B     LOOK10                                                           
*                                                                               
LOOK40   DS    0H                                                               
*        MVC   MSG,=CL10'CALPHA'                                                
*        GOTO1 ADUMP,DMCB,(RC),CALPHA,L'CALPHA                                  
*        MVC   MSG,=CL10'ALPHAID'                                               
*        GOTO1 ADUMP,DMCB,(RC),ALPHAID,L'ALPHAID                                
         OC    CORIGIN,CORIGIN                                                  
         BZ    LOOK50                                                           
*        MVC   MSG,=CL10'CORIGIN'                                               
*        GOTO1 ADUMP,DMCB,(RC),CORIGIN,L'CORIGIN                                
*        MVC   MSG,=CL10'ORIGINUM'                                              
*        GOTO1 ADUMP,DMCB,(RC),ORIGINUM,L'ORIGINUM                              
         CLC   CORIGIN,ORIGINUM                                                 
         BNE   LOOK30                                                           
*                                                                               
LOOK50   LA    R0,L'COFFLIST                                                    
         LA    R1,COFFLIST                                                      
         CLI   0(R1),X'40'         IS THERE AN OFFICE LIST?                     
         BH    LOOK55                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-16                                                          
         B     LOOK60                                                           
*        CLC   COFFLIST,SPACES     IS THERE AN OFFICE LIST                      
*        BE    LOOK60              NO- PRINCIPLE ID INFO IS DEFAULT             
LOOK55   BAS   RE,LIMIT            CHECK LIMIT ACCESS                           
         BNE   LOOK30              OFFICE NOT IN THIS OFFICE LIST               
         CLI   READIT,C'T'         OR IF NOT READING TRANSACTION                
         BNE   LOOK60                                                           
         CLC   CORIGIN,ORIGINUM    CHECK ORIGIN NUMB AGAINST TAB ENTRY          
         BE    LOOK60                                                           
         MVI   SKIPACCT,C'Y'       SKIP THIS ACCOUNT                            
         B     LOOKX                                                            
*                                                                               
LOOK60   DS    0H                                                               
         MVC   SRECCO,SPACES                                                    
         MVC   D1CNM,SPACES                                                     
         MVC   D1CNM,CNAME         COMPANY NAME                                 
         MVC   D1CADLNS,SPACES                                                  
         MVC   D1CADLN1,CADDR1     MOVE IN ADDRESS FROM CMP TBLE                
         MVC   D1CADLN2,CADDR2                                                  
         MVC   D1CADLN3,CADDR3                                                  
*                                                                               
         LA    R0,3                DETERMINE NUMBER OF ADDR LINES               
         LA    R1,D1CADLN3         THIRD ADDR LINE                              
LOOK70   CLC   0(L'D1CADLN3,R1),SPACES                                          
         BNE   LOOK80                                                           
         SH    R1,=Y(L'D1CADLN3)                                                
         BCT   R0,LOOK70                                                        
         DC    H'0'                NO ADDRESS LINE                              
*                                                                               
LOOK80   STC   R0,D1CADNL          NUMBER OF ADDR LINES                         
         MVI   SCANSW,SCANCMP      VALIDATE COMPANY ADDRESS                     
         BAS   RE,FIXADDR                                                       
*                                                                               
         MVC   SRECCNM,CCONTNM     COMPANY CONTACT NAME                         
         MVC   SRECPHON,CPHONE     COMPANY PHONE NUMBER                         
         MVC   SRECCEM,CEMAIL      COMPANY EMAIL ADDRESS                        
         TM    DWNSTAT,DWNCMPY                                                  
         BO    LOOK90                                                           
         CLI   QOPT6,C'C'          IS COMPANY IT'S OWN TRANSMITTER              
         BE    LOOK90              YES                                          
         MVC   D1CTRAN,DDSTRANS    DEFAULT IS DDS AS THE TRANSMITTER            
         MVC   D1COMPCD,DDSDDN     DDS 99 FOR TAPE DDN                          
         B     LOOK110                                                          
*                                                                               
LOOK90   CLI   QOPT7,C'Y'           DOWNLOADING?                                
         BNE   LOOK100                                                          
         CLC   CTRANS,ZEROS         IS COMPANY IT'S OWN TRANSMITTER?            
         BNE   *+6                                                              
         DC    H'0'                                                             
LOOK100  MVC   D1CTRAN,CTRANS       AGENCY'S OWN TRANS NUMBER                   
         MVC   D1COMPCD,CALPHA      AGENCY ALPHA FOR TAPE DDN                   
*                                                                               
LOOK110  DS    0H                                                               
         MVC   SRECCID,CNUMBER      COMPANY'S EIN NUMBER                        
         CLI   QOPT5,C' '                                                       
         BNE   LOOKX                                                            
         MVC   D1COMPCD,CALPHA      IF COMP TAPE, THEN USE COMPANY CD           
*                                                                               
LOOKX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT 4804 FORM                                                    *          
**********************************************************************          
         SPACE 1                                                                
FORM4804 NTR1                                                                   
         OPEN  (ACT99WK,(INPUT))    OPEN WORK OR TAPE FILE.                     
         LA    R5,WKREC                                                         
*                                                                               
FO10     GET   ACT99WK,(R5)        READ RECORD.                                 
         CLI   0(R5),C'T'                                                       
         BE    FTREC                                                            
         CLI   0(R5),C'A'                                                       
         BE    FAREC                                                            
         CLI   0(R5),C'C'                                                       
         BE    FCREC                                                            
         CLI   0(R5),C'F'                                                       
         BE    FEXIT                                                            
         B     FO10                                                             
*                                                                               
*  PRINT PAYER TRANSMITTER INFO FROM 'T' RECORD                                 
*                                                                               
FTREC    DS    0H                                                               
         USING TREC,R5                                                          
         CLC   SVTRID,TTRID                                                     
         BE    FO10                IF SAME TRANSMITTER, NO NEW HEADING          
         MVC   SVTRID,TTRID                                                     
*                                                                               
         MVI   RCSUBPRG,3          FOR FORM 4804, DIFFERENT HEADING.            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         CLC   ALPHAID,=C'FC'                                                   
         BE    FO10                                                             
         MVI   P,C'-'                                                           
         MVC   P+1(80-1),P                                                      
         MVC   PSECOND(LMEDTYP),MEDTYP                                          
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     FO10                                                             
*                                                                               
*  PRINT PAYER NAME AND ADDRESS FROM 'A' RECORD                                 
*                                                                               
         USING AREC,R5                                                          
FAREC    DS    0H                                                               
         MVC   P+8(40),ACOMPNM                                                  
         MVC   P+50(10),=C'PAYER EIN='                                          
         MVC   P+61(L'ACCOID),ACCOID                                            
         MVC   PSECOND+8(40),ACADD1                                             
*                                                                               
         MVC   WORK2,SPACES                                                     
         MVC   WORK2(L'ACADD2),ACADD2                                           
         GOTO1 SQUASHER,DMCB,WORK2,L'WORK2                                      
         MVC   PTHIRD+8(40),WORK2                                               
*                                                                               
         MVC   PTHIRD+50(L'DOCTYP),DOCTYP                                       
         MVC   PFOURTH+50(9),=C'AMT CODE='                                      
         MVC   PFOURTH+60(3),=C'137'                                            
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     FO10                                                             
         DROP  R5                                                               
*                                                                               
FCREC    DS    0H                                                               
         USING CREC,R5                                                          
         MVC   P+8(24),=C'NUMBER OF PAYEE RECORDS='                             
         EDIT  (C8,CBCOUNT),(6,P+33),COMMAS=YES,ALIGN=LEFT                      
*                                                                               
         ZAP   AMOUNT,=P'0'                                                     
         PACK  WORK(8),CAMT1       RENT                                         
         AP    AMOUNT,WORK(8)                                                   
         PACK  WORK(8),CAMT2       ROYALTY                                      
         AP    AMOUNT,WORK(8)                                                   
         PACK  WORK(8),CAMT3       PRIZES                                       
         AP    AMOUNT,WORK(8)                                                   
         PACK  WORK(8),CAMT6       MEDICAL                                      
         AP    AMOUNT,WORK(8)                                                   
         PACK  WORK(8),CAMT7       NON EMPLOYEE COMPENSATION                    
         AP    AMOUNT,WORK(8)                                                   
         EDIT  (P8,AMOUNT),(17,P+50),2,COMMAS=YES,ALIGN=LEFT                    
         MVI   PSECOND,C'-'                                                     
         MVC   PSECOND+1(80-1),PSECOND                                          
         GOTO1 ACREPORT                                                         
         B     FO10                                                             
         DROP  R5                                                               
*                                                                               
FEXIT    GOTO1 ACREPORT                                                         
         MVC   P(LAFF1),AFFIDAV1                                                
         MVC   PSECOND(LAFF2),AFFIDAV2                                          
         MVC   PTHIRD(LAFF3),AFFIDAV3                                           
         MVC   PFOURTH(LAFF4),AFFIDAV4                                          
         GOTO1 ACREPORT                                                         
         MVC   P(LAFF5),AFFIDAV5                                                
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P(L'SIGNTURE),SIGNTURE                                           
         MVI   P+L'SIGNTURE,C'.'                                                
         MVC   P+L'SIGNTURE+1(20-1),P+L'SIGNTURE                                
         MVC   P+30(L'TITL-1),TITL                                              
         MVI   P+30+L'TITL-1,C'.'                                               
         MVC   P+30+L'TITL-1+1(19-1),P+30+L'TITL-1                              
         MVC   P+55(L'DAT),DAT                                                  
         MVI   P+55+L'DAT,C'.'                                                  
         MVC   P+55+L'DAT+1(16-1),P+55+L'DAT                                    
         GOTO1 ACREPORT                                                         
         MVI   SVTRID,0                                                         
         CLOSE ACT99WK                                                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT INVALID ADDRESS LIST                                         *          
**********************************************************************          
         SPACE 1                                                                
FORMINV  NTR1                                                                   
         MVI   RCSUBPRG,5                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
FORMINV2 CP    INVCNT,=P'0'                                                     
         BNE   FORMINV4                                                         
         MVC   P+1(40),=CL40'ALL ADDRESSES ARE CORRECT'                         
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
*                                                                               
FORMINV4 CP    INVCNT,=P'100'                                                   
         BNE   FORMINV6                                                         
         MVC   P(40),=CL40'*** MORE THAN 100 ERRORS EXIST ***'                  
         GOTO1 ACREPORT                                                         
*                                                                               
         USING INVADDD,R1                                                       
FORMINV6 L     R1,AINVTBL                                                       
*                                                                               
FORMINV8 MVC   P+1(12),INVACCT                                                  
         MVC   P+19(40),INVNAME                                                 
         GOTO1 ACREPORT                                                         
         LA    R1,INVEQU(R1)                                                    
         SP    INVCNT,=P'1'                                                     
         BP    FORMINV8                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* TAPE NAMES & ADDRESS CONSTANTS                                     *          
**********************************************************************          
         SPACE 1                                                                
DDPARM   DC    CL8'ACT99TP'                                                     
DSPARM   DC    CL20'ACCTAPE.ACTTXXN1'       XX=CO. CODE., N=ACFILE NO.          
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    V(DLFLD)                                                         
         DC    V(CONVMOS)                                                       
         DC    V(SQUASHER)                                                      
         DC    V(ADSCAN)                                                        
         DC    V(PRNTBL)                                                        
         DC    V(HELLO)                                                         
         DC    A(DWNL)                                                          
         DC    A(DWNBUF)                                                        
         DC    A(CPYTAB)                                                        
         DC    A(BUF1099)                                                       
         DC    A(EXTTBL)                                                        
         DC    A(STARS)                                                         
         DC    A(INVTBL)                                                        
         DC    A(DUMP)             DUMP ROUTINE                                 
         DC    A(MRKT99)           MARK 1099 RECORD WITH LIVE INFO              
         DC    A(BLDCPY)           READ 1099 RECS AND BLD COMPANY TABLE         
*                                                                               
DDSDDN   DC    C'99'                                                            
DDSTRANS DC    C'19868'   ----***  DDS'S TRANSMITTER NO.  ***--                 
FCDDN    DC    C'FC'                                                            
FCTRANS  DC    C'99999'                                                         
ZEROS    DC    C'00000000000000000000'                                          
STAR1    DC    60C'*'                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SORTER DEFINITIONS                                                 *          
**********************************************************************          
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,33,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=750'                                   
         EJECT                                                                  
**********************************************************************          
* 4804 FORM CONSTANTS                                                *          
**********************************************************************          
         SPACE 1                                                                
MEDTYP   DC    C'MEDIUM NUMBER AND TYPE = 1 MAGNETIC TAPE, LABELED, '           
         DC    C'9-TRACK, 6250 BPI, EBCDIC.'                                    
LMEDTYP  EQU   *-MEDTYP                                                         
DOCTYP   DC    C'1099-MISC'                                                     
*                                                                               
AFFIDAV1 DC    C'UNDER PENALTIES OF PERJURY, I DECLARE THAT I HAVE '            
         DC    C'EXAMINED THIS TRANSMITTAL,'                                    
LAFF1    EQU   *-AFFIDAV1                                                       
AFFIDAV2 DC    C'INCLUDING ACCOMPANYING DOCUMENTS, AND TO THE BEST '            
         DC    C'OF MY KNOWLEDGE AND BELIEF'                                    
LAFF2    EQU   *-AFFIDAV2                                                       
AFFIDAV3 DC    C'IT IS CORRECT AND COMPLETE.  IN THE CASE OF DOCUME'            
         DC    C'NTS WITHOUT RECIPIENT''S'                                      
LAFF3    EQU   *-AFFIDAV3                                                       
AFFIDAV4 DC    C'IDENTIFIYING NUMBERS, I HAVE COMPLIED WITH THE '               
         DC    C'REQUIREMENTS OF THE LAW IN '                                   
LAFF4    EQU   *-AFFIDAV4                                                       
AFFIDAV5 DC    C'ATTEMPTING TO SECURE SUCH NUMBERS FROM THE RECIPIENT.'         
LAFF5    EQU   *-AFFIDAV5                                                       
SIGNTURE DC    C'SIGNATURE'                                                     
TITL     DC    C'TITLE:'                                                        
DAT      DC    C'DATE'                                                          
         EJECT                                                                  
**********************************************************************          
* TAPE DEFINITIONS                                                   *          
**********************************************************************          
         SPACE 1                                                                
ACT99TP  DCB   DSORG=PS,MACRF=PM,DDNAME=ACT99TP,RECFM=F,LRECL=750,     X        
               BLKSIZE=750                                                      
*                                                                               
ACT99WK  DCB   DSORG=PS,MACRF=(PM,GM),DDNAME=ACT99WK,RECFM=F,          X        
               LRECL=930,BLKSIZE=930                                            
         EJECT                                                                  
**********************************************************************          
* DUMMY LINEUP                                                       *          
**********************************************************************          
         SPACE 1                                                                
* THE FOLLOWING MUST MATCH SREC DSECT BYTE FOR BYTE.                            
*                                                                               
STARS    DS    0D                                                               
         DC    C'**'                                                            
         DC    18C'9'              ID NOS.                                      
         DC    C' '                                                             
         DC    12C'*'                                                           
         DC    CL40' '                                                          
         DC    15C'*'                                                           
         DC    CL35' '                                                          
         DC    40C'*'                                                           
         DC    X'04'               3 ADDR LNS                                   
         DC    120C'*'                                                          
         DC    2C'*'                                                            
         DC    5C'9'                                                            
         DC    4C'*'                                                            
         DC    40C'*'                                                           
         DC    X'04'               4 ADDR LNS                                   
         DC    160C'*'                                                          
         DC    PL6'999999'                                                      
         DC    C'*'                                                             
         DC    247C' '                                                          
         EJECT                                                                  
**********************************************************************          
* ADDRESS EXCEPTION TABLE                                            *          
**********************************************************************          
         SPACE 1                                                                
EXTTBL   DC    AL1(3),C'C/O'                                                    
         DC    AL1(4),C'ATTN'                                                   
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
* COMPANY TABLE                                                      *          
**********************************************************************          
         SPACE 1                                                                
CPYTAB   DS    CL(CTLENQ)                                                       
         EJECT                                                                  
**********************************************************************          
* BUFFERS                                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNBUF   DS    0D                                                               
         DS    500C                                                             
*                                                                               
INVTBL   DS    0D                                                               
         DS    (100*48)C           INVALID ADDRESS TABLE                        
*                                                                               
         DS    0D                                                               
         DC    C'*BF1099*'                                                      
BUF1099  DS    (132*33)C                                                        
         EJECT                                                                  
**********************************************************************          
* READ 1099 RECORDS AND BUILD COMPANY TABLE                          *          
**********************************************************************          
         SPACE 1                                                                
BLDCPY   NMOD1 0,*BLDCP*                                                        
         L     RC,0(R1)                                                         
*                                                                               
         USING CPYTABD,R3                                                       
         L     R3,ACPYTAB          WORK AREA FOR COMPANY TABLE                  
         MVC   0(2,R3),=C'**'      INITIALIZE WITH STARS                        
*                                                                               
         MVC   SVKEY,SPACES                                                     
         USING T99RECD,R4                                                       
         LA    R4,SVKEY                                                         
         MVI   T99KTYP,T99KTYPQ    X'3E' - 1099 TAX INFO TYPE                   
         MVI   T99KSUB,T99KSUBQ    X'15' - 1099 TAX INFO SUB TYPE               
         MVC   T99KCPY,RCCOMPFL    COMPANY                                      
         MVC   T99KYEAR,SVCDTE     CURRENT YEAR                                 
         MVC   T99KOID,ORIGINUM    READ FOR SPECIFIC ORIGIN NUMBER              
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   SVKEY(T99KEND),IOKEY  SAME ACCT?                                 
         BNE   BLDCPYX                                                          
*        BE    BLDCPY10                                                         
*                                                                               
*        XC    T99KOID,T99KOID     READ FOR ALL RECORD                          
*                                                                               
*        GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
*        CLC   SVKEY(T99KEND),IOKEY  SAME ACCT?                                 
*        BNE   BLDCPYX                                                          
*                                                                               
BLDCPY10 GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         LA    R4,IO               SET R4 TO POINT TO THE RECORD                
*                                                                               
         MVC   CALPHA,ALPHAID      MOVE IN 2 BYTE ID CODE                       
         MVC   CORIGIN,T99KOID     ORIGIN ID (IF ANY)                           
         LA    RE,COFFLIST         SET OFFICE LIST TO SPACES                    
         LA    RF,L'COFFLIST                                                    
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*        MVC   COFFLIST,SPACES     SET OFFICE LIST TO SPACES                    
*                                                                               
         LA    R2,T99RFST          POINT TO FIRST ELEMENT                       
BLDCPY20 CLI   0(R2),0             END OF RECORD?                               
         BE    BLDCPY99                                                         
         CLI   0(R2),X'20'         NAME ELEMENT                                 
         BE    BLDCPY30                                                         
         CLI   0(R2),X'22'         ADDRESS ELEMENT                              
         BE    BLDCPY40                                                         
         CLI   0(R2),X'D2'         OFFICE LIST                                  
         BE    BLDCPY50                                                         
         CLI   0(R2),X'DB'         FREE FORM TEXT ELEMENT                       
         BE    BLDCPY60                                                         
BLDCPY25 SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     BLDCPY20                                                         
*                                                                               
         USING NAMELD,R2                                                        
BLDCPY30 SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   CNAME(0),NAMEREC    SAVE COMPANY NAME                            
         B     BLDCPY25                                                         
*                                                                               
         USING ADRELD,R2                                                        
BLDCPY40 CLI   ADRLN,ADRLNQ                                                     
         BNE   BLDCPY47                                                         
         MVC   CADDR,ADRLINE1      MOVE IN ALL 3 ADDRESSES                      
*        TM    ADRSTAT,ADRCSZ      ARE WE USING NEW FORMAT?                     
*        BNO   BLDCPY25                                                         
         MVC   CADDR3,SPACES                                                    
         LA    R1,L'ADRCITY                                                     
         LA    R0,ADRCITY+L'ADRCITY-1                                           
         CLI   0(R0),X'40'         FIND LAST SIGNIFICANT CHARACTER              
         BNE   BLDCPY45                                                         
         AHI   R1,-1                                                            
         BCT   R0,*-12                                                          
         B     BLDCPY25                                                         
*                                                                               
BLDCPY45 AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   CADDR3(0),ADRCITY                                                
         LA    RE,CADDR3                                                        
         LA    RE,1(R1,RE)         BUMP TO FIRST AVAILABLE BYTE                 
         MVI   0(RE),C','                        ADD COMMA                      
         LA    RE,2(RE)                          SKIP TWO SPACES                
         MVC   0(L'ADRSTATE,RE),ADRSTATE         MOVE IN STATE                  
         MVC   L'ADRSTATE+1(L'ADRZIP,RE),ADRZIP  AND ZIP CODE                   
         B     BLDCPY25                                                         
*                                                                               
         USING OADRELD,R2                                                       
BLDCPY47 MVC   CADDR1(L'OADRLN1),OADRLN1                                        
         MVC   CADDR2(L'OADRLN2),OADRLN2                                        
         MVC   CADDR3(L'OADRCSZP),OADRCSZP                                      
*        TM    OADRSTAT,OADRCSZ    ARE WE USING NEW FORMAT?                     
*        BNO   BLDCPY25                                                         
         MVC   CADDR3,SPACES                                                    
         LA    R1,L'OADRCITY                                                    
         LA    R0,OADRCITY+L'OADRCITY-1                                         
         CLI   0(R0),X'40'         FIND LAST SIGNIFICANT CHARACTER              
         BNE   BLDCPY48                                                         
         AHI   R1,-1                                                            
         BCT   R0,*-12                                                          
         B     BLDCPY25                                                         
*                                                                               
BLDCPY48 AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   CADDR3(0),OADRCITY                                               
         LA    RE,CADDR3                                                        
         LA    RE,1(R1,RE)         BUMP TO FIRST AVAILABLE BYTE                 
         MVI   0(RE),C','                        ADD COMMA                      
         LA    RE,2(RE)                          SKIP TWO SPACES                
         MVC   0(L'OADRST,RE),OADRST             MOVE IN STATE                  
         MVC   L'OADRST+1(L'OADRZIP,RE),OADRZIP  AND ZIP CODE                   
         B     BLDCPY25                                                         
*                                                                               
         USING OFLELD,R2                                                        
BLDCPY50 CLI   QOPT3,C'R'          IS THIS A REPLACEMENT RUN                    
         BNE   BLDCPY25                                                         
         SR    R1,R1                                                            
         IC    R1,OFLLN                                                         
         SH    R1,=Y(OFLLN1Q+1)    SUBTRACT OVERHEAD                            
         CLI   R1,99                                                            
         BL    *+8                                                              
         LA    R1,99               MAXIMUM LENGTH OF FIELD IS 100               
         EX    R1,*+4                                                           
         MVC   COFFLIST(0),OFLNTRY                                              
         B     BLDCPY25                                                         
*                                                                               
         USING FFTELD,R2                                                        
BLDCPY60 CLI   FFTTYPE,FFTTCNAM    NAME ELEMENT?                                
         BNE   BLDCPY65                                                         
         MVC   CCONTNM(L'CCONTNM+L'CPHONE),FFTTNAME  CONTACT NAME/PHONE         
         CLI   FFTDLEN,FFTT75LN    IS THERE AN EMAIL ADDRESS?                   
         BNH   BLDCPY25                                                         
         SR    R1,R1                                                            
         IC    R1,FFTDLEN          GET LENGTH OF TEXT                           
         SH    R1,=Y(FFTT75LN+1)   SUBTRACT OVERHEAD                            
         CLI   R1,34                                                            
         BL    *+8                                                              
         LA    R1,34               MAXIMUM LENGTH OF FIELD IS 35                
         EX    R1,*+4                                                           
         MVC   CEMAIL(0),FFTTCEM   MOVE IN EMAIL ADDRESS                        
         B     BLDCPY25                                                         
*                                                                               
BLDCPY65 CLI   FFTTYPE,FFTTTNNI    1099 TAX INFO                                
         BNE   BLDCPY25                                                         
         MVC   CTRANS,FFTTTCC      TCC CODE                                     
         MVC   CNUMBER,FFTTTIN     TIN NUMBER                                   
         CLI   FFTTDWNL,C'Y'       ARE WE DOWNLOADING                           
         BNE   *+8                                                              
         OI    CSTATUS,CSTDWNL     SET DOWNLOAD BIT                             
         B     BLDCPY25                                                         
*                                                                               
BLDCPY99 LA    R3,CTLENQ(R3)                                                    
         MVC   0(2,R3),=C'**'      END WITH STARS                               
*                                                                               
BLDCPYX  XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* LTERALS                                                            *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT10,C'Y'                                                      
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
         EJECT                                                                  
**********************************************************************          
* LTERALS                                                            *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* MARK 1099 RECORDS FOR LIVE RUNS ONLY                               *          
**********************************************************************          
         SPACE 1                                                                
MRKT99   NMOD1 0,*MRKT*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         USING T99RECD,R4                                                       
         LA    R4,SVKEY                                                         
         MVC   T99KEY,SPACES                                                    
         MVI   T99KTYP,T99KTYPQ    X'3E' - 1099 TAX INFO TYPE                   
         MVI   T99KSUB,T99KSUBQ    X'15' - 1099 TAX INFO SUB TYPE               
         MVC   T99KCPY,RCCOMPFL    COMPANY                                      
         MVC   T99KYEAR,SVCDTE     CURRENT YEAR                                 
         MVC   T99KOID,ORIGINUM    READ FOR SPECIFIC ORIGIN NUMBER              
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   SVKEY(T99KEND),IOKEY  SAME ACCT?                                 
         BNE   MRKT99X                                                          
*        BE    MRKT9910                                                         
*                                                                               
*        XC    T99KOID,T99KOID     READ FOR ALL RECORD                          
*                                                                               
*        GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
*        CLC   SVKEY(T99KEND),IOKEY  SAME ACCT?                                 
*        BNE   MRKT99X                                                          
*                                                                               
MRKT9910 GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         LA    R4,IO               SET R4 TO POINT TO THE RECORD                
*                                                                               
         LA    R2,T99RFST          POINT TO FIRST ELEMENT                       
MRKT9920 CLI   0(R2),0             END OF RECORD?                               
         BE    MRKT9940                                                         
         CLI   0(R2),X'DB'         FREE FORM TEXT ELEMENT                       
         BE    MRKT9930                                                         
MRKT9925 SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     MRKT9920                                                         
*                                                                               
         USING FFTELD,R2                                                        
MRKT9930 CLI   FFTTYPE,FFTTTNNI    1099 TAX INFO                                
         BNE   MRKT9925                                                         
         GOTO1 DATCON,DMCB,(5,0),(21,FFTTDLLR)    MARK WITH TODAYS DATE         
         MVC   FFTTLAOC,SVLIM      LIMIT ACCESS                                 
         ZAP   DUB,TOTBCNT         # OF FORMS COUNTER                           
         CVB   R1,DUB                                                           
         STCM  R1,3,FFTTNOF        STORE NUMBER OF FORMS                        
         B     MRKT9925                                                         
*                                                                               
MRKT9940 BAS   RE,BLDD2            BUILD OFFICE ELEMENT                         
*        MVC   MSG,=CL10'1099 REC'                                              
*        SR    R6,R6                                                            
*        ICM   R6,3,T99RLEN                                                     
*        GOTO1 ADUMP,DMCB,(RC),(R4),(R6)                                        
         CLI   RCWRITE,C'N'                                                     
         BE    MRKT99X                                                          
         GOTO1 =A(DMPUTREC),DMCB,(RC)                                           
*                                                                               
MRKT99X  XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD OR REBUILD THE OFFICE ELEMENT (D2)                            *         
***********************************************************************         
         SPACE 1                                                                
         USING T99RECD,R4                                                       
BLDD2    NTR1                                                                   
         LA    R4,IO                                                            
*                                                                               
         LA    R2,T99RFST                                                       
BLDD210  CLI   0(R2),0             END OF RECORD?                               
         BE    BLDD230             NO D2 ELEM-ADD IT                            
         CLI   0(R2),X'D2'         FOUND D2-DEL IT AND RE-ADD IT                
         BE    BLDD220                                                          
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     BLDD210                                                          
*                                                                               
         USING OFLELD,R2                                                        
BLDD220  MVI   0(R2),X'FF'         DELETE OFFICE ELEMENT AND REBUILD            
         BAS   RE,DELIT                                                         
*                                                                               
BLDD230  LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   OFLEL,OFLELQ        X'D2'                                        
*                                                                               
         LA    R1,SVOFFL           POINT TO OFFICE LIST                         
         LA    R0,COFFLNQ          R0=NUMBER OF OFFICES IN LIST                 
*                                                                               
         SR    RE,RE                                                            
         TM    OFFSTAT,STASOFF2    TEST IF NEW OFFICE CODE IN EFFECT            
         BO    BLDD250                                                          
*                                                                               
BLDD240  CLI   0(R1),C' '          CHECK FOR SPACES                             
         BNH   BLDD260                                                          
         LA    R1,1(R1)            BUMP TO NEXT LIST ENTRY                      
         AHI   RE,1                                                             
         BCT   R0,BLDD240                                                       
         B     BLDD260                                                          
*                                                                               
BLDD250  SRL   R0,1                DIVIDE OFFICE LIST BY TWO                    
BLDD255  CLI   0(R1),C' '          CHECK FOR SPACES                             
         BNH   BLDD260                                                          
         LA    R1,2(R1)                                                         
         AHI   RE,2                                                             
         BCT   R0,BLDD255                                                       
*                                                                               
BLDD260  LR    R1,RE                                                            
         SHI   R1,1                                                             
         EX    R1,*+4                                                           
         MVC   OFLNTRY(0),SVOFFL                                                
         AH    R1,=Y(OFLLN1Q+1)    ADD OVERHEAD AND 1 FOR EX DECREMENT          
         STC   R1,OFLLN            PUT IN ELEMENT LENGTH                        
         BAS   RE,ADDL             ADD ELEMENT TO RECORD                        
*                                                                               
BLDD2X   XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DELTE X'FF' ELEMENT                                                 *         
***********************************************************************         
         SPACE 1                                                                
DELIT    NTR1                                                                   
         LA    R5,IO                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'ACCMST'),(X'FF',(R5)),0,0                    
DELX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT                                                         *         
***********************************************************************         
         SPACE 1                                                                
ADDL     NTR1                                                                   
         LA    R5,IO                                                            
         LA    R4,ELEM                                                          
         GOTO1 HELLO,DMCB,(C'P',=C'ACCMST'),(R5),(R4)                           
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* HEADLINE ROUTINES                                                  *          
**********************************************************************          
         SPACE 1                                                                
REGHEAD  DS    0H                                                               
         NMOD1 0,*RGHED                                                         
         L     RC,HDRC                                                          
         CLI   QOPT7,C'Y'          DOWNLOADING                                  
         BE    REGHEADX                                                         
         CLI   RCSUBPRG,3                                                       
         BE    *+14                                                             
         MVC   HEAD3+9(36),SVCNM                                                
         B     REGHEADX                                                         
*                                                                               
         MVC   HEAD1+75(5),SVTRID                                               
         MVC   HEAD2(15),=CL15'ORIGINAL'                                        
         CLI   QOPT3,C'C'                                                       
         BNE   *+10                                                             
         MVC   HEAD2(15),=CL15'CORRECTION'                                      
*                                                                               
REGHEADX XMOD1 1                                                                
*                                                                               
HDRC     DC    A(0)                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     R5,8(R1)            R5 = A(RECORD)                               
         L     RF,12(R1)                                                        
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         L     R6,ADWNBUF                                                       
         USING DLCBD,R6                                                         
*                                                                               
* INITIALIZATION                                                                
*                                                                               
         TM    DWNSTAT,DWNINIT     HAS IT BEEN INITIALIZED?                     
         BO    DWNL100                                                          
         SR    R0,R0                                                            
         SR    R1,R1               L'SOURCE                                     
         L     RE,ADWNBUF          A(DESTINATION)                               
         LA    RF,DLCBXLX          L'DESTINATION                                
         MVCL  RE,R0                                                            
*                                                                               
         MVI   DLCBACT,DLCBINIT    START OF REPORT                              
         OI    DLCBFLG1,DLCBFXTN                                                
         LA    RE,P                                                             
         ST    RE,DLCBAPL          PRINT LINE AREA                              
         LA    RE,DLPRINT                                                       
         ST    RE,DLCBAPR          PRINT ROUTINE                                
         MVI   DLCXMAXL+1,L'P      MAX LINE LENGTH                              
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT                                         
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT                               
         MVI   DLCXEOLC,X'5E'      END OF LINE ,SEMI-COLON                      
         MVI   DLCXEORC,C':'       END OF REPORT                                
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLX,SPACES                                                   
         GOTO1 DLFLD,DLCBD                                                      
         OI    DWNSTAT,DWNINIT                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,X'FF'      NO HEADINGS FOR DOWNLOADING                  
*                                                                               
* DOWNLOAD RECORD                                                               
*                                                                               
DWNL100  DS    0H                                                               
         CLI   DWNMODE,DWNREC      DOWNLOAD RECORD                              
         BNE   DWNL200                                                          
         LA    R3,10                                                            
DWNL110  MVC   DLCBFLX,SPACES                                                   
         MVC   DLCBFLX(75),0(R5)                                                
         MVI   DLCBLEN,75          LENGTH                                       
         MVI   DLCBACT,DLCBPUT     PUT                                          
         MVI   DLCBTYP,DLCBTXT     TEXT                                         
         OI    DLCBFLG1,DLCBFXFL   USING EXTENDED FIELD                         
         GOTO1 DLFLD,DLCBD                                                      
         LA    R5,75(R5)                                                        
         BCT   R3,DWNL110                                                       
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 DLFLD,DLCBD                                                      
         B     DWNX                                                             
*                                                                               
* SET END OF REPORT                                                             
*                                                                               
DWNL200  CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 DLFLD,DLCBD                                                      
*                                                                               
DWNX     XMOD1 1                                                                
*                                                                               
* PRINT ROUTINE                                                                 
*                                                                               
DLPRINT  NTR1                                                                   
         TM    DWNSTAT,DWNINIT                                                  
         BO    *+8                                                              
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMCTFIL  NMOD1 0,CTF               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',SVKEY,IOKEY                  
         B     DMX                                                              
*                                                                               
DMWRTDR  NMOD1 0,WRT               WRITE BACK TO DIR                            
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMADDDR  NMOD1 0,ADD               ADD KEY TO DIR                               
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
         DROP  R3                                                               
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
*                                                                               
DMX      DS    0H                                                               
*        CLI   DMCB+8,0            MAKE SURE THERE IS NO ERROR                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* FILE DCB                                                           *          
**********************************************************************          
         SPACE 1                                                                
         DCBD  DSORG=PS,DEVD=TA                                                 
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACTTD    DSECT                                                                  
VTYPES   DS    0A                                                               
SORTER   DS    A                                                                
DLFLD    DS    A                                                                
CONVMOS  DS    A                                                                
SQUASHER DS    A                                                                
ADSCAN   DS    A                                                                
PRNTBL   DS    A                                                                
HELLO    DS    A                                                                
ADWNL    DS    A                                                                
ADWNBUF  DS    A                                                                
ACPYTAB  DS    A                                                                
ABUF1099 DS    A                                                                
AEXTTBL  DS    A                                                                
ASTARS   DS    A                                                                
AINVTBL  DS    A                                                                
ADUMP    DS    A                                                                
AMRKT99  DS    A                                                                
ABLDCPY  DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
AINVENT  DS    A                   CURRENT ADDRESS OF NEXT ERROR ENTRY          
SVDA     DS    F                   SAVED AREA FOR DISK ADDRESS                  
DISP2    DS    H                   DISPLACEMENT TO ELEMENTS                     
*                                                                               
ADDRWORK DS    0CL160              WORK AREA FOR ADDRESS MANIPULATION           
ADDRST   DS    CL40                STREET NUMBER                                
ADDRST2  DS    CL40                2ND LINE FOR STREET                          
ADDRST3  DS    CL40                3RD LINE FOR STREET                          
ADDRCSZ  DS    CL40                CITY STATE ZIP                               
         ORG ADDRCSZ                                                            
SCANCY   DS    CL29                CITY                                         
SCANST   DS    CL2                 STATE                                        
SCANZIP  DS    CL9                 ZIP                                          
*                                                                               
MSG      DS    CL10                MESSAGE FOR PRINTABLE                        
ELCODE   DS    CL1                                                              
FOUNDIT  DS    CL1                 AGENCY CODE FOUND IN TABLE                   
SCANSW   DS    CL1                 TYPE OF ADDRESS TO FORMAT                    
SCANRECP EQU   X'01'               -SCAN A RECIEPIENTS ADDRESS                  
SCANCMP  EQU   X'02'               -SCAN A COMPANY ADDRESS                      
PAYERAMT DS    PL8                 TOTAL FOR THIS PAYER.                        
*                                                                               
BTOTALS  DS    PL8                                                              
BTOT1    DS    PL8                                                              
BTOT2    DS    PL8                                                              
BTOT3    DS    PL8                                                              
BTOT4    DS    PL8                                                              
BTOT5    DS    PL8                                                              
BTOT6    DS    PL8                                                              
BTOT7    DS    PL8                                                              
BTOT8    DS    PL8                                                              
BTOT9    DS    PL8                                                              
BTOTA    DS    PL8                                                              
BTOTB    DS    PL8                                                              
BTOTC    DS    PL8                                                              
BTOT#    EQU   (*-BTOTALS)/L'BTOTALS                                            
*                                                                               
COTOTAL  DS    PL6                 COMP TOTAL FOR TOTAL PAGE                    
BRECOUNT DS    PL8                                                              
TOTACNT  DS    PL8                 TOTAL A COUNT                                
TOTBCNT  DS    PL8                 TOTAL B COUNT                                
RECSEQ   DS    PL8                 RECORD SEQUENCE NUMBER                       
INVCNT   DS    PL6                 # OF INVALID ADDRESSES IN INVTBL             
CLGR     DS    CL1                 CURRENT LGR INDICATOR                        
SORTCD   DS    CL1             FORCES LGR 2C TO COME FIRST IN SORT GET.         
*                                                                               
STATUS   DS    CL1                                                              
STATSRT  EQU   X'01'               SORTER IS OPEN                               
STATNEG  EQU   X'02'               RUN HAS NEGATIVE AMOUNTS                     
*                                                                               
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNCMPY  EQU   X'01'               DOWNLOAD FOR THIS COMPANY                    
DWNINIT  EQU   X'02'               DOWNLOAD INITIALIZED                         
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNREC   EQU   1                   DOWNLOAD RECORD                              
DWNEOR   EQU   2                   DOWNLOAD END OF REPORT                       
*                                                                               
OFFSTAT  DS    XL1                 OFFICE STATUS BYTE                           
STASOFF2 EQU   X'01'               TWO CHARACTER OFFICE CODE                    
*                                                                               
AMOUNT   DS    PL8                                                              
FIRSTPRT DS    CL1                 FIRST TIME TO PRINT.                         
ACTRD    DS    CL1                                                              
SKIPACCT DS    CL1                 'Y' FORCES AN ACCOUNT TO BE SKIPPED          
PATTERN  DS    CL4                                                              
PRINCID  DS    CL2                 SAVE PRINCIPLE ID                            
OFFLAG   DS    XL2                 SAVED OFFICE FOR LIMITED ACCESS TEST         
OFPOS    DS    CL1                 OFFICE POSITION FROM LEDGER RECORD           
MOS      DS    CL2                 MOS FROM TRANSACTION                         
READIT   DS    CL1                 READ BUCKETS OR TRANSACTION                  
FILTS    DS    0CL4                                                             
FILT1    DS    CL1                 ACCOUNT FILTER 1                             
FILT2    DS    CL1                 ACCOUNT FILTER 2                             
FILT3    DS    CL1                 ACCOUNT FILTER 3                             
FILT4    DS    CL1                 ACCOUNT FILTER 4                             
SVSDAT   DS    PL3                 PACKED START DATE                            
SVEDAT   DS    PL3                 PCAKED END DATE                              
SVCOMP   DS    XL1                                                              
         DS    XL1                                                              
SVCID    DS    XL9                                                              
SVRID    DS    XL9                                                              
SVACCT   DS    CL12                                                             
SVKEY    DS    CL42                                                             
SVCNM    DS    CL36                                                             
SVCADNL  DS    XL1                                                              
SVPTYPE  DS    XL1                                                              
SVCDTE   DS    CL8                 SAVED AREA FOR DATE (INCLD CENTURY)          
*                                                                               
SVTRID   DS    CL5                 FOR USE BY FORM4804                          
         DS    CL1                 UNUSED                                       
WORK2    DS    CL120               FOR ADDRESS CRUNCHING                        
*                                                                               
SVLIM    DS    CL4                 SAVED AREA FR LIMITED ACCESS                 
SVOFFL   DS    CL(L'COFFLIST)      SAVED AREA FOR OFFICE LIST                   
*                                                                               
ELEM     DS    XL255               ELEMENT                                      
*                                                                               
SRECA    DS    CL750                                                            
SVSREC   DS    CL750               SAVE SRT REC FOR FORMS.                      
WKREC    DS    CL750               SPACE TO BUILD IRS RECORDS.                  
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                                                             
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
**********************************************************************          
* COMPANY TABLE DSECT                                                *          
**********************************************************************          
         SPACE 1                                                                
CPYTABD  DSECT                                                                  
CALPHA   DS    CL2                 AGENCY ALPHA                                 
CORIGIN  DS    XL2                 ORIGIN ID                                    
CTRANS   DS    CL5                 TRANSMITTER NUMBER                           
CNUMBER  DS    CL9                 COMPANY EIN NUMBER                           
CSTATUS  DS    XL1                 STATUS BYTE                                  
CSTDWNL  EQU   X'01'               COMPANY DOWNLOADS INSTEAD OF TAPE            
CNAME    DS    CL40                COMPANY NAME FROM ID RECORD                  
CADDR    DS    0CL120                                                           
CADDR1   DS    CL40                COMPANY ADDR LINE 1                          
CADDR2   DS    CL40                COMPANY ADDR LINE 2                          
CADDR3   DS    CL40                COMPANY ADDR LINE 3                          
COFFLIST DS    CL240               LIMITED ACCESS OFFICE LIST                   
COFFLNQ  EQU   (*-COFFLIST)        LENGTH OF OFFICE LIST                        
CCONTNM  DS    CL40                AGENCY CONTACT NAME                          
CPHONE   DS    CL15                AGENCY PHONE NUMBER                          
CEMAIL   DS    CL35                AGENCY CONTACT EMAIL                         
CTLENQ   EQU   (*-CPYTABD)                                                      
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
**********************************************************************          
* 1099 FORM DSECT                                                    *          
**********************************************************************          
         SPACE 1                                                                
BUFFERD  DSECT                                                                  
*                                                                               
BF1      DS    0CL132             **** LINE #1 ****                             
         DS    CL32                                                             
BFCRRCTD DS    CL1                 (CORRECTED)                                  
         ORG   BF1+132                                                          
*                                                                               
BF2      DS    CL132              **** LINE #2 ****                             
*                                                                               
BF3      DS    0CL132             **** LINE #3 ****                             
         DS    CL5                                                              
BFCMPNM  DS    CL33                (COMPANY NAME)                               
         DS    CL2                                                              
         DS    CL12                                                             
         ORG   BF3+132                                                          
*                                                                               
BF4      DS    0CL132             **** LINE #4 ****                             
         DS    CL5                                                              
BFCMPAD1 DS    CL33                (1ST LINE OF COMPANY ADDRESS)                
         DS    CL2                                                              
BFRENT   DS    CL12                (RENT)                                       
         ORG   BF4+132                                                          
*                                                                               
BF5      DS    0CL132             **** LINE #5 ****                             
         DS    CL5                                                              
BFCMPAD2 DS    CL33                (2ND LINE OF COMPANY ADDRESS)                
         DS    CL2                                                              
         DS    CL12                                                             
         ORG   BF5+132                                                          
*                                                                               
BF6      DS    0CL132             **** LINE #6 ****                             
         DS    CL5                                                              
BFCMPAD3 DS    0CL33               (3RD LINE OF COMPANY ADDRESS)                
BFPHON1  DS    CL15                (CPY PHONE # IF 2 LINES OF ADDRESS)          
         DS    CL18                                                             
         DS    CL2                                                              
         DS    CL12                                                             
         ORG   BF6+132                                                          
*                                                                               
BF7      DS    0CL132             **** LINE #7 ****                             
         DS    CL5                                                              
BFPHON2  DS    CL15                (CPY PHONE # IF 3 LINES OF ADDRESS)          
         DS    CL18                                                             
         DS    CL2                                                              
BFROYAL  DS    CL12                ROYALTIES                                    
         ORG   BF7+132                                                          
*                                                                               
BF8      DS    CL132              **** LINE #8 ****                             
*                                                                               
BF9      DS    0CL132             **** LINE #9 ****                             
         DS    CL40                                                             
BFPRIZES DS    CL12                (PRIZES,AWARDS)                              
         DS    CL1                                                              
BFFINCTX DS    CL12                (FEDERAL INCOME TAX)                         
         DS    CL3                                                              
BFYEAR   DS    CL4                 (CALENDAR YEAR)                              
         ORG   BF9+132                                                          
*                                                                               
BF10     DS    CL132              **** LINE #10 ****                            
*                                                                               
BF11     DS    CL132              **** LINE #11 ****                            
*                                                                               
BF12     DS    0CL132             **** LINE #12 ****                            
         DS    CL9                                                              
BFCIDNO  DS    CL10                (COMPANY ID NUMBER)                          
         DS    CL6                                                              
BFSSNO   DS    CL11                (RECIPIENT'S SS#)                            
         DS    CL4                                                              
BFFISH   DS    CL12                FISHING BOAT PROCEEDS                        
         DS    CL1                                                              
BFMEDIC  DS    CL12                (MEDICAL)                                    
         ORG   BF12+132                                                         
*                                                                               
BF13     DS    CL132              **** LINE #13 ****                            
*                                                                               
BF14     DS    CL132              **** LINE #14 ****                            
*                                                                               
BF15     DS    0CL132             **** LINE #15 ****                            
         DS    CL5                                                              
BFRNAME  DS    CL33                (RECIPIENTS NAME)                            
         ORG   BF15+132                                                         
*                                                                               
BF16     DS    0CL132             **** LINE #16 ****                            
         DS    CL5                                                              
*FRADD1  DS    CL33                (RECIPIENT'S ADDRESS #1)                     
         ORG   BF16+132                                                         
*                                                                               
BF17     DS    0CL132             **** LINE #17 ****                            
         DS    CL40                                                             
BFNONEMP DS    CL12                (NONEMPLOYEE COMPENSATION)                   
         ORG   BF17+132                                                         
*                                                                               
BF18     DS    0CL132             **** LINE #18 ****                            
         DS    CL5                                                              
BFRADD1  DS    CL33                (RECIPIENT'S ADDRESS #1)                     
         ORG   BF18+132                                                         
*                                                                               
BF19     DS    0CL132             **** LINE #19 ****                            
         DS    CL5                                                              
BFRADD2  DS    CL33                (RECIPIENT'S ADDRESS #2)                     
         ORG   BF19+132                                                         
*                                                                               
BF20     DS    0CL132             **** LINE #20 ****                            
         DS    CL5                                                              
BFRADD3  DS    CL33                (RECIPIENT'S ADDRESS #3)                     
         DS    CL1                                                              
BFSINBOX DS    CL20                (PUT STAR IN BOX)                            
         ORG   BF20+132                                                         
*                                                                               
BF21     DS    CL132              **** LINE #21 ****                            
*                                                                               
BF22     DS    0CL132             **** LINE #22 ****                            
         DS    CL5                                                              
BFRADD4  DS    CL33                (RECIPIENT'S ADDRESS #4)                     
         ORG   BF22+132                                                         
*                                                                               
BF23     DS    CL132              **** LINE #23 ****                            
*                                                                               
BF24     DS    0CL132             **** LINE #24 ****                            
         DS    CL9                                                              
BFACCT   DS    CL12                (ACCOUNT NUMBER -OPTIONAL)                   
         ORG   BF24+132                                                         
*                                                                               
BF25     DS    CL132              **** LINE #25 ****                            
BF26     DS    CL132              **** LINE #26 ****                            
BF27     DS    CL132              **** LINE #27 ****                            
BF28     DS    CL132              **** LINE #28 ****                            
BF29     DS    CL132              **** LINE #29 ****                            
BF30     DS    CL132              **** LINE #30 ****                            
BF31     DS    CL132              **** LINE #31 ****                            
BF32     DS    CL132              **** LINE #32 ****                            
* 32 IS THE MAX NO. OF LINES ALLOWED *                                          
*                                                                               
BFEQU    EQU   33               CHANGE BUF1099 ALSO,  #LINES IN BUFFER          
BFLNQ    EQU   BFEQU*132                                                        
         EJECT                                                                  
***********************************************************************         
* 'T' RECORD DSECT - TRANSMITTER RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
TRECD    DSECT                                                                  
TREC     DS    0CL750              PAYER RECORD                                 
TTYPE    DS    CL1                 RECORD TYPE - 'T'                            
TYEAR    DS    CL4                 YEAR                                         
         DS    CL1                                                              
TTIN     DS    CL9                 TRANSMITTER TIN                              
TTRID    DS    CL5                 TRANSMITTER CODE                             
TRPALPHA DS    CL2                 REPLACEMENT ALPHA                            
         DS    CL5                                                              
TTEST    DS    CL1                 TEST FILE INDICATOR                          
TFORENT  DS    CL1                 FOREIGN ENTITY INDICATOR                     
TTRNNM   DS    CL40                TRANSMITTER NAME                             
         DS    CL40                TRANSMITTER NAME (CONTINUATION)              
TCPYNM   DS    CL40                COMPANY NAME                                 
         DS    CL40                COMPANY NAME     (CONTINUATION)              
TCPYADD1 DS    CL40                STREET ADDRESS                               
TCPYADD2 DS    0CL51               CITY-STATE-ZIP                               
TCPYCTY  DS    CL40                CITY                                         
TCPYST   DS    CL2                      STATE                                   
TCPYZIP  DS    CL9                            ZIP                               
         DS    CL15                                                             
TBCOUNT  DS    CL8                 TOTAL NUMBER OF PAYEES                       
TCONNM   DS    CL40                CONTACT NAME                                 
TCPHONE  DS    CL15                CONTACT PHONE NUMBER                         
TCEMAIL  DS    CL35                CONTACT E-MAIL ADDRESS                       
TMTFIND  DS    CL2                 MAGNETIC TAPE FILER INDICATOR                
TFILNM   DS    CL15                ELECTRONIC FILE NAME                         
TMEDNUM  DS    CL6                 TRANSMITTER MEDIA NUMBER                     
         DS    CL83                                                             
TRECSEQ  DS    CL8                 RECORD SEQUENCE NUMBER                       
         DS    CL10                                                             
TVNDIND  DS    CL1                 VENDOR INDICATOR V OR I                      
TVNDNM   DS    CL40                VENDOR NAME                                  
TVNDADD  DS    CL40                VENDOR MAILING ADDRESS                       
TVNDCTY  DS    CL40                VENDOR CITY                                  
TVNDST   DS    CL2                 VENDOR STATE                                 
TVNDZIP  DS    CL9                 VENDOR ZIP CODE                              
TVNDCNM  DS    CL40                VENDOR CONTACT NAME                          
TVNDCPH  DS    CL15                VENDORS CONTACT PHONE AND EXTN               
TVNDCEM  DS    CL35                VENDOR'S CONTACT'S E-MAIL ADDRESS            
         DS    CL(750-(*-TREC))                                                 
****     DS    CL375                                                            
TLENQ    EQU   (*-TREC)                                                         
         EJECT                                                                  
***********************************************************************         
*               'A' RECORD DSECT                                      *         
***********************************************************************         
AREELSEQ DS    CL3                 REEL SEQ NUMBER                              
         SPACE 1                                                                
ARECD    DSECT                                                                  
AREC     DS    0CL750              PAYER RECORD                                 
         DS    CL1                 RECORD TYPE - 'A'                            
AYEAR    DS    CL4                 YEAR                                         
         DS    CL6                                                              
ACCOID   DS    CL9                 EIN  BLANK ONLY FOR FOREIGN CORP             
         DS    CL4                 PAYER NAME CONTROL - BLANK                   
         DS    CL1                 BLANK                                        
         DS    CL1                 COMBINED FED/STATE FILER - BLANK             
AFRMTYP  DS    CL1                 FORM TYPE (CONSTANT) 1099-MISC               
AAMTCD   DS    CL12                AMOUNT CODES (CONSTANT)                      
         DS    CL8                 BLANK                                        
AORIG    DS    CL1                 1 - ORIGINAL                                 
AREPL    DS    CL1                 1 - REPLACEMENT                              
ACORRECT DS    CL1                 TEST/CORRECTION INDICATOR                    
         DS    CL1                 BLANK                                        
         DS    CL1                 FOREIGN CORP INDICATOR                       
ACOMPNM  DS    CL40                COMPANY NAME                                 
         DS    CL40                                                             
AZER     DS    CL1                 INDICATES 'NOT' A TRANSFER AGENT             
ACADD1   DS    CL40                STREET ADDRESS                               
ACADD2   DS    0CL51                                                            
ACADCTY  DS    CL40                CITY                                         
ACADST   DS    CL2                     STATE                                    
ACADZIP  DS    CL9                          ZIP                                 
ACPHONE  DS    CL15                AGENCY PHONE NUMBER                          
         DS    CL260               BLANK                                        
ARECSEQ  DS    CL8                 RECORD SEQUENCE NUMBER                       
         DS    CL243               BLANK                                        
ALENQ    EQU   (*-AREC)                                                         
         EJECT                                                                  
***********************************************************************         
*               'B' RECORD DSECT                                      *         
***********************************************************************         
*                                                                               
BRECD    DSECT                                                                  
BREC     DS    0CL750              PAYEE RECORD                                 
         DS    CL1                 RECORD TYPE - 'B'                            
BYEAR    DS    CL4                 YEAR                                         
BCORR    DS    CL1                 HAS A 'G' IF A CORRECTED RETURN.             
BNMCTL   DS    CL4                 NAME CONTROL (1ST 4 LETTERS SURNAME)         
BTINTYPE DS    CL1                 EIN OR SSN (1 OR 2)                          
BSSNO    DS    CL9                 ID NUMBER TIN                                
BACCT    DS    CL20                DDS ACCOUNT NUMBER                           
         DS    CL4                 OFFICE CODE - BLANKS                         
         DS    CL10                BLANKS                                       
BAMT1    DS    CL12                                                             
BAMT2    DS    CL12                                                             
BAMT3    DS    CL12                                                             
BAMT4    DS    CL12                                                             
BAMT5    DS    CL12                                                             
BAMT6    DS    CL12                                                             
BAMT7    DS    CL12                DOLLAR AMOUNT OF AMT 7                       
BAMT8    DS    CL12                                                             
BAMT9    DS    CL12                DOLLAR AMOUNT OF AMT 9                       
BAMTA    DS    CL12                DOLLAR AMOUNT OF AMT A                       
BAMTB    DS    CL12                DOLLAR AMOUNT OF AMT B                       
BAMTC    DS    CL12                DOLLAR AMOUNT OF AMT C                       
         DS    CL48                BLANK                                        
BFORGN   DS    CL1                 FOREIGNCOUNTRY INDICATOR                     
BNAME    DS    CL40                PAYEE NAME                                   
BNAME2   DS    CL40                                                             
         DS    CL40                BLANKS                                       
BADDR    DS    CL40                STREET ADDRESS                               
         DS    CL40                BLANKS                                       
BCSZ     DS    0CL51                                                            
BCTY     DS    CL40                CITY                                         
BST      DS    CL2                       STATE                                  
BZIP     DS    CL9                              ZIP                             
         DS    CL1                 BLANK                                        
BRECSEQ  DS    CL8                 RECORD SEQUENCE NUMBER                       
         DS    CL36                BLANKS                                       
         DS    CL1                 SECOND TIN NOTICE-BLANK                      
         DS    CL2                 BLANKS                                       
         DS    CL1                 DIRECT SALES INDICATOR-BLANK                 
         DS    CL115               BLANKS                                       
         DS    CL60                SPECIAL DATA INDICATOR-BLANK                 
         DS    CL12                STATE INC TAX WITHHELD-BLANK                 
         DS    CL12                LOCAL IC TAX WITHHELD-BLANK                  
         DS    CL2                 COMB FED/STATE CODE-BLANK                    
         DS    CL2                 BLANKS                                       
BLENQ    EQU   (*-BREC)                                                         
BSRCLIST DS    CL180               SOURCE LIST  (LENGTH/ACCT/AMOUNT)            
         EJECT                                                                  
***********************************************************************         
*               'C' RECORD DSECT                                      *         
***********************************************************************         
*                                                                               
CRECD    DSECT                                                                  
CREC     DS    0CL750              PAYER SUMMARY RECORD                         
         DS    CL1                 RECORD TYPE - 'C'                            
CBCOUNT  DS    CL8                 NUMBER OF B RECS FOR THIS COMPANY            
         DS    CL6                 BLANK                                        
CAMTFLD1 DS    0CL18                                                            
         DS    CL2                                                              
CAMT1    DS    CL16                                                             
CAMTFLD2 DS    0CL18                                                            
         DS    CL2                                                              
CAMT2    DS    CL16                ROYALITIES                                   
CAMTFLD3 DS    0CL18                                                            
         DS    CL2                                                              
CAMT3    DS    CL16                PRIZES/AWARDS                                
CAMTFLD4 DS    0CL18                                                            
         DS    CL2                                                              
CAMT4    DS    CL16                                                             
CAMTFLD5 DS    0CL18                                                            
         DS    CL2                                                              
CAMT5    DS    CL16                                                             
CAMTFLD6 DS    0CL18                                                            
         DS    CL2                                                              
CAMT6    DS    CL16                                                             
CAMTFLD7 DS    0CL18                                                            
         DS    CL2                                                              
CAMT7    DS    CL16                NON EMPL COMP                                
CAMTFLD8 DS    0CL18                                                            
         DS    CL2                                                              
CAMT8    DS    CL16                                                             
CAMTFLD9 DS    0CL18                                                            
         DS    CL2                                                              
CAMT9    DS    CL16                                                             
CAMTFLDA DS    0CL18                                                            
         DS    CL2                                                              
CAMTA    DS    CL16                                                             
CAMTFLDB DS    0CL18                                                            
         DS    CL2                                                              
CAMTB    DS    CL16                                                             
CAMTFLDC DS    0CL18                                                            
         DS    CL2                                                              
CAMTC    DS    CL16                                                             
CAMTQ    EQU   (*-CAMTFLD1)/L'CAMTFLD1                                          
         DS    CL268               BLANKS                                       
CRECSEQ  DS    CL8                 RECORD SEQUENCE NUMBER                       
         DS    CL243               BLANKS                                       
CLENQ    EQU   (*-CREC)                                                         
         EJECT                                                                  
***********************************************************************         
*               'F' RECORD DSECT                                      *         
***********************************************************************         
*                                                                               
FRECD    DSECT                                                                  
FREC     DS    0CL750              TAPE SUMMARY RECORD                          
         DS    CL1                 RECORD TYPE                                  
FACOUNT  DS    CL8                 NUMBER OF A RECS IN FILE                     
         DS    CL21                ZEROS                                        
         DS    CL19                BLANKS                                       
FBCOUNT  DS    CL8                 NUMBER OF B RECS IN FILE                     
         DS    CL442               BLANKS                                       
FRECSEQ  DS    CL8                 RECORD SEQUENCE NUMBER                       
         DS    CL243               BLANKS                                       
FLENQ    EQU   (*-FREC)                                                         
         EJECT                                                                  
***********************************************************************         
*               ADDRESS EXCLUSION TABLE                               *         
***********************************************************************         
*                                                                               
EXTTBLD  DSECT                                                                  
EXTLEN   DS    AL1                 LENGTH OF FIELD                              
EXTENT   DS    0CL1                VARIABLE LENGTH ENTRY                        
         SPACE 5                                                                
***********************************************************************         
*               INVALID ADDRESS TABLE                                 *         
***********************************************************************         
*                                                                               
INVADDD  DSECT                                                                  
INVACCT  DS    CL12                ACCOUNT CODE                                 
INVNAME  DS    CL40                ACCOUNT NAME                                 
INVEQU   EQU   *-INVADDD                                                        
         SPACE 5                                                                
***********************************************************************         
*              SOURCE ACCOUNT DSECT                                   *         
***********************************************************************         
*                                                                               
SRCED    DSECT                                                                  
SRCEACCT DS    CL14                ACCOUNT CODE                                 
SRCEAMNT DS    PL6                 AMOUNT                                       
SRCELNQ  EQU   *-SRCED                                                          
SRCEMAX  EQU   8                   MAXIMUM NUMBER OF ACCTS TO SAVE              
         EJECT                                                                  
***********************************************************************         
*              B RECORD PRINT DSECT                                   *         
***********************************************************************         
*                                                                               
PRBRECD  DSECT                                                                  
PRBLINE1 DS    0CL132                                                           
         DS    CL1                                                              
PRBSSNO  DS    CL9                 SS NUMBER                                    
         DS    CL7                                                              
PRBNAME  DS    CL36                                                             
         DS    CL6                                                              
PRBAMT   DS    CL12                                                             
         DS    CL15                                                             
PRBSRCE  DS    CL14                SOURCE ACCT                                  
         DS    CL2                                                              
PRBAMT2  DS    CL12                AMOUNT BROKEN DOWN                           
PRBLNQ1  EQU   132                 LENGTH OF 1 PRINT LINE                       
         ORG PRBLINE1+132                                                       
PRBLINE2 DS    0H                                                               
         DS    CL1                                                              
PRBYR    DS    CL4                 YEAR                                         
         DS    CL1                                                              
PRBMCTL  DS    CL4                                                              
         DS    CL1                                                              
PRBTTYPE DS    CL1                                                              
         DS    CL5                                                              
PRBADDR  DS    CL40                                                             
         ORG PRBLINE2+132                                                       
PRBLINE3 DS    0H                                                               
         DS    CL17                                                             
PRBCSZ   DS    CL40                                                             
         ORG PRBLINE3+132                                                       
         DS    CL17                                                             
PRBERROR DS    CL30                                                             
         EJECT                                                                  
***********************************************************************         
*              SORT RECORD                                            *         
***********************************************************************         
*                                                                               
SRECD    DSECT                                                                  
SRECCO   DS    CL2       X         COMPANY ALPHA ID                             
SRECCID  DS    CL9       X         COMPANY ID NO.                               
SRECRID  DS    CL9       X         RECIPIENT'S ID NO.                           
SRECLGR  DS    CL1       C         FORCE LEDGER SEQUENCE                        
SRECRACT DS    CL12      X         RECIPIENTS ACCOUNT.                          
SRECCNM  DS    CL40      X         COMPANY CONTACT PERSON                       
SRECPHON DS    CL15      X         COMPANY CONTACT NUMBER                       
SRECCEM  DS    CL35                COMPANY CONTACT EMAIL                        
*                                                                               
D1CNM    DS    CL40      A         COMPANY NAME                                 
D1CADNL  DS    CL1       B         NO. OF CO ADDR LNS                           
D1CADLNS DS    0CL120    A         COMPANY ADDR LINES                           
D1CADLN1 DS    CL40      A         COMPANY ADDR LINES                           
D1CADLN2 DS    CL40      A         COMPANY ADDR LINES                           
D1CADLN3 DS    0CL40     A         COMPANY ADDR LINES                           
D1CADCTY DS    CL29      A         CITY                                         
D1CADST  DS    CL2       A             STATE                                    
D1CADZIP DS    CL9       A                  ZIP                                 
D1COMPCD DS    CL2       C         COMPANY CODE FOR TAPE DDN                    
D1CTRAN  DS    CL5       C         TRANSMITTER NUMBER                           
D1YEAR   DS    CL4       C         YEAR OF FORMS                                
*                                                                               
* FOR EACH NEW PAYEE, CLEAR FROM THIS POINT ON.                                 
*                                                                               
D1RNM    DS    CL40      A         RECIPIENTS NAME                              
D1RADNL  DS    CL1       B         NO. OF RECIP ADDR LNS                        
D1RADLNS DS    0CL160    A         RECIPIENTS ADDR LINES                        
D1RADLN1 DS    CL40      A                                                      
D1RADLN2 DS    CL40      A                                                      
D1RADLN3 DS    CL40      A                                                      
D1RADLN4 DS    0CL40     A                                                      
D1RADCTY DS    CL29      A         CITY                                         
D1RADST  DS    CL2       A             STATE                                    
D1RADZIP DS    CL9       A                  ZIP                                 
D1RAMT   DS    PL6       P         RECIPIENTS AMOUNT                            
D1SSORID DS    CL1       C         I=FED ID NO, ANYTHING ELSE IS SS NO          
D1MIN6   DS    CL1       C         600 DOLLAR MINIMUM REQESTED                  
D1FORGN  DS    CL1       C         FORIEGN ADDRESS?                             
D1PTYPE  DS    XL1                 PAYMENT TYPE                                 
D1ACCT   DS    CL14                UNIT/LDG/ACCT                                
CLRLEN   EQU   *-D1RNM             LENGTH TO CLEAR                              
         DS    CL230               SPARE                                        
LSREC    EQU   *-SRECCO            LENGTH OF SORT RECORD                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER OLD ADDRESS ELEMENT ADRELD, WITH CITY, STATE ZIP     *         
***********************************************************************         
OADRELD  DSECT                                                                  
OADREL   DS    X                                                                
OADRELQ  EQU   X'22'                                                            
OADRLN   DS    XL1                                                              
OADRSTAT DS    XL1                 STATUS                                       
OADRCSZ  EQU   X'80'                                                            
OADRLN1  DS    CL33                ADDRESS LINE 1                               
OADRLN2  DS    CL33                ADDRESS LINE 2                               
OADRCSZP DS    0CL33               CITY, STATE, ZIP, ZIP ROUTE                  
OADRCITY DS    CL22                                                             
OADRST   DS    CL2                 STATE                                        
OADRZIP  DS    CL5                                                              
OADRZRN  DS    CL4                 ZIP ROUTING NO.                              
OADRLNQ  EQU   *-OADRELD           LENGTH OF THIS ELEMENT                       
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
*ACREPWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENMODES                                                                     
*DDLOGOD                                                                        
*ACMASTD                                                                        
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110ACREPTT02S03/10/03'                                      
         END                                                                    
