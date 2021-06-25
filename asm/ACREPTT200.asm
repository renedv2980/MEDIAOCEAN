*          DATA SET ACREPTT200 AT LEVEL 095 AS OF 03/09/01                      
*PHASE ACTT02A,*                                                                
*INCLUDE SORTER                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE CONVMOS                                                                
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
         USING ACTT02D,RC                                                       
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
         CLI   MODE,PROCHIST                                                    
         BE    PHIST                                                            
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
         LA    RE,VTYPES           RELOCATE VTYPES                              
         LA    R0,ADCONS                                                        
         LA    RF,VTYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,=A(REGHEAD)      SET UP HEADLINE ADDRESSABILTY                
         ST    R2,HEADHOOK                                                      
         L     R2,=A(HDRC)                                                      
         ST    RC,0(R2)                                                         
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
         LA    R0,TRNTBLNQ         NUMBER OF ENTRIES IN TABLE                   
         L     R1,ATRNTAB          AGENCIES READING TRANSACTION TABLE           
REQF20   CLC   ALPHAID,0(R1)       FOR LIMITED ACCESS                           
         BE    REQF30                                                           
         LA    R1,L'TRNTAB(R1)                                                  
         BCT   R0,REQF20                                                        
*                                   ****HARDCODED FOR NOW********               
         MVI   FOUNDIT,C'Y'        MARK HARDCODE AS 'FOUND'                     
         CLC   ALPHAID,=C'SS'                                                   
         BE    REQFX                ****HARDCODED FOR NOW********               
         MVI   READIT,C'B'         READ BUCKETS                                 
*                                                                               
         USING CPYTABD,R3                                                       
REQF30   L     R3,ACPYTAB          COMPANY TABLE                                
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
         LA    R2,CTFILIO          R2=A(CONTROL ID RECORD)                      
         USING CTIREC,R2                                                        
         XC    LSTSAVE,LSTSAVE     CLEAR LIST NUMBER SAVE                       
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,PRINCID     PRINCIPLE ID                                 
         OC    CORIGIN,CORIGIN     IS THERE ID SPECIFIED IN TABLE?              
         BZ    *+10                                                             
         MVC   CTIKNUM,CORIGIN     YES - USE THAT INSTEAD                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=CL8'CTFILE',(R2),(R2),0               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                 ID NOT FOUND                                
*                                                                               
         LA    R1,CTIDATA                                                       
         SR    R0,R0                                                            
REQF70   CLI   0(R1),0              THE END                                     
         BE    REQF110                                                          
         USING CTAGYD,R1                                                        
         CLI   0(R1),X'06'          AGENCY ALPHA ELEMENT                        
         BNE   REQF80                                                           
         CLC   CTAGYID,ALPHAID      MAKE SURE THIS IS RIGHT AGENCY              
         BE    REQF100              YES - READ NEXT EL                          
         DC    H'0'                                                             
*                                                                               
         USING CTSYSD,R1                                                        
REQF80   DS    0H                                                               
         CLI   0(R1),X'21'          SYSTEM AUTH ELEMENT                         
         BNE   REQF90                                                           
         CLI   CTSYSNUM,X'06'       X'06' IS ACCOUNTING                         
         BNE   REQF100              NO - GET NEXTEL                             
         CLI   CTSYSLMT,0                                                       
         BE    REQF100             IF EQUAL - NO LIMIT ACCESS                   
         CLC   COFFLIST,SPACES     IS THERE AN OFFICE LIST?                     
         BH    REQF100                                                          
         XC    COFFLIST,COFFLIST                                                
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   COFFLIST,ACMROFL    LIMITED ACCESS/REQUEST OFFICE LIST           
         DROP  RF                                                               
*                                                                               
         USING CTDSTD,R1                                                        
         SR    RF,RF                                                            
REQF90   CLI   0(R1),X'30'          DESTINATION DETAIL ELEMENT                  
         BNE   REQF100                                                          
         CLC   CNAME,SPACES        IF NAME IS HARDCODED SKIP                    
         BNE   *+10                                                             
         MVC   CNAME,CTDSTNAM                                                   
         CLC   CADDR1,SPACES       IF  HARDCODED SKIP                           
         BNE   REQF100                                                          
         MVC   CADDR1,CTDSTADD                                                  
         MVC   CADDR2,SPACES                                                    
         MVC   CADDR3,SPACES                                                    
         CLI   CTDSTLEN,166                                                     
         BL    REQF100                                                          
         MVC   CADDR2,CTDSTAD2                                                  
*                                                                               
REQF100  SR    R0,R0                                                            
         IC    R0,1(R1)             BUMP TO NEXT EL                             
         AR    R1,R0                                                            
         B     REQF70                                                           
*                                                                               
REQF110  CLI   FOUNDIT,C'Y'         AGENCY SETUP FOR 1099'S?                    
         BE    REQFX                YES - CONTINUE                              
         MVI   FCRDACC,C'N'         CAUSE REQLAST                               
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
         L     R2,ADACC             ACCOUNT RECORD                              
         MVC   SVKEY,0(R2)                                                      
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
* PROCHIST                                                           *          
**********************************************************************          
         SPACE 1                                                                
PHIST    DS    0H                                                               
         CLI   READIT,C'T'          READ TRANSACTIONS OR BUCKETS                
         BE    PHSTX                                                            
         L     R2,ADTRANS                                                       
         CLI   0(R2),X'45'                                                      
         BNE   PHSTX                                                            
         USING TRHISTD,R2                                                       
         CLC   TRHSYEAR(2),SVSDAT  LOWER THAN START YYMM                        
         BL    PHSTX                                                            
         CLC   TRHSYEAR(2),SVEDAT  HIGHER THAN END YYMM                         
         BH    PHSTX                                                            
         AP    D1RAMT,TRHSDR                                                    
*                                                                               
PHSTX    B     EXIT                                                             
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
         L     R2,ADACC                                                         
         USING ACKEYD,R4                                                        
         L     R4,AIOAREA                                                       
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(15),0(R2)                                               
         MVI   ACKEYACC+1,C'S'                                                  
         MVC   ACKEYACC+2(1),CLGR                                               
         MVC   KEYSAVE,ACKEYACC                                                 
         BAS   RE,READ                                                          
         CLC   KEYSAVE(15),ACKEYACC                                             
         BNE   OL130               DO NEXT LEDGER                               
*                                                                               
* PROCESS HIGH LEVEL ACCOUNTS - (GET NAME/ADDRESS)                              
*                                                                               
         MVI   ELCODE,X'32'        BALANCE EL ABSENT ON HI LVLS.                
         LR    R2,R4               SO MAKE SURE WE HAVE A LOW LEVEL.            
         BAS   RE,GETEL                                                         
         BNE   OL40                                                             
         L     R2,ADACC             2C NAME                                     
         USING ACNAMED,R2                                                       
         L     R2,ADACCNAM                                                      
         ZIC   R1,ACNMLEN          LENGTH OF NAME EL                            
         MVC   D1RNM,SPACES                                                     
         SH    R1,=H'3'            TAKE OFF FOR L'CODE+L'LNTH+1                 
         BM    OL20                                                             
         EXMVC R1,D1RNM,ACNMNAME                                                
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
         MVC   SVKEY,0(R4)         NOW LOOK FOR SUB ACCOUNTS                    
         BAS   RE,SEQ                                                           
         CLC   SVKEY(15),0(R4)     SAME ACCT?                                   
         BNE   OL120               B IF NO.                                     
         CLI   READIT,C'T'         ARE WE READING TRANSACTIONS                  
         BE    OL70                                                             
*                                                                               
* PROCESS HISTORY ELEMENTS                                                      
*                                                                               
         MVI   ELCODE,X'45'        ACCUMULATE HISTORY ELTS.                     
         LR    R2,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   OL40                                                             
*                                                                               
         USING TRHISTD,R2                                                       
OL50     CLC   TRHSYEAR(2),SVSDAT  LOWER THAN START YYMM                        
         BL    OL60                                                             
         CLC   TRHSYEAR(2),SVEDAT  HIGHER THAN END YYMM                         
         BH    OL60                                                             
         AP    D1RAMT,TRHSDR                                                    
         CLC   ACKEYCON+1(2),=C'SC'  TAKE AWAY SUB SC CREDITS                   
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
         BAS   RE,GETEL                                                         
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
         MVC   SRECRACT,SVKEY+3         MOVE IN ACCOUNT NO.                     
         MVC   D1ACCT,SVKEY+1                                                   
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
         MVC   SRECRACT,SVKEY+3         MOVE IN ACCOUNT NO.                     
         MVC   D1ACCT,SVKEY+1                                                   
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
         XC    ACKEYACC(42),ACKEYACC                                            
         L     R2,ADACC                                                         
         MVC   ACKEYACC(32),0(R2)  READ ACCOUNT RECORD AGAIN                    
         BAS   RE,READ             TO NOT SCREW UP MONACC                       
         MVI   SORTCD,C'2'         REESTABLISH 2C FIRST                         
*                                                                               
OLX      B     EXIT                                                             
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
         MVC   MSG,=CL10'PUT'                                                   
         GOTO1 ADUMP,DMCB,(RC),SRECA,L'SRECA                                    
PUTSX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         TM    STATUS,STATSRT      WAS SORTER OPENED?                           
         BZ    EXIT                                                             
*                                                                               
RS10     MVI   FIRSTPRT,C'Y'       SET FOR LINE UP PHASE.                       
         XC    SVCID,SVCID                                                      
         ZAP   PAYERAMT,=P'0'      CLEAR PAYER BUCKET.                          
*                                                                               
RS20     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
*        MVC   MSG,=CL10'GET1'                                                  
*        GOTO1 ADUMP,DMCB,(RC),SRECA,750                                        
*                                                                               
RS21     LTR   R2,R2               END OF SORT?                                 
         BZ    RS500                                                            
         LA    R1,LSREC                                                         
         MOVE  (SRECCO,(R1)),(R2)  MOVE REC INTO MY AREA.                       
         CLI   SVCID,0             FIRST RECORD?                                
         BNE   RS30                IF YES THEN OPEN.                            
         BAS   RE,OPEN             OPEN TAPE FILE AND WORK FILE.                
         CLI   SRECCID,0                                                        
         BE    RS50                                                             
         BAS   RE,DOTREC                                                        
         B     RS50                                                             
*                                                                               
RS30     CLC   SVCID,SRECCID       SEE IF COMPANY BREAK.                        
         BE    RS100               B IF NO                                      
RS40     BAS   RE,DOCREC    IF NEW COMPANY, CREATE 'C' REC FOR OLD ONE          
*                                                                               
RS50     MVC   SVCID,SRECCID                                                    
         XC    SVRID,SVRID                                                      
         CLI   SRECCID,0                                                        
         BE    *+8                                                              
         BAS   RE,DOAREC                                                        
*                                                                               
RS100    CLI   SVRID,0             FIRST PAYER FOR THIS COMPANY?                
         BE    RS150               B IF YES                                     
         CLI   SRECRID,X'FF'       IF START MISSING SS NOS.                     
         BE    RS300               THEN HANDLE THEM.                            
         CLC   SVRID,SRECRID       PAYER BREAK?                                 
         BE    RS200               B IF NO                                      
         BAS   RE,WRITBREC         WRITE PRESENT B REC WITH BUCKET.             
*                                                                               
RS150    BAS   RE,DOBREC           INITIALIZE 'B' REC FOR NEW PAYEE.            
         MVC   SVRID,SRECRID                                                    
*                                                                               
RS200    MVC   SVPTYPE,D1PTYPE     SAVE CURRENT PAY TYPE                        
         AP    PAYERAMT,D1RAMT     BUCKET AMT, AND                              
         CP    D1RAMT,=P'0'        DONT TRACK ACCTS W/ZERO BALANCE              
         BE    RS20                                                             
*                                                                               
         USING BREC,R6                                                          
         LA    R6,WKREC            ADD ENTRY TO SOURCE LIST                     
         LA    R6,BSRCLIST                                                      
         USING SRCED,R6                                                         
         LA    R0,SRCEMAX          MAX NUMBER OF ACCTS TO SAVE                  
*                                                                               
RS230    CLI   0(R6),0                                                          
         BE    RS250                                                            
         CLC   SRCEACCT,D1ACCT     SUM UP TRANSACTIONS                          
         BNE   *+14                                                             
         AP    SRCEAMNT,D1RAMT                                                  
         B     RS20                                                             
         LA    R6,SRCELNQ(R6)      FIND NEXT AVAILABLE ENTRY                    
         BCT   R0,RS230                                                         
         B     RS20                NO MORE ROOM TO SAVE ACCTS                   
*                                                                               
RS250    MVC   SRCEACCT,D1ACCT     SAVE OFF UNIT                                
         ZAP   SRCEAMNT,D1RAMT     SAVE OFF AMOUNT                              
         MVI   SRCELNQ(R6),0                                                    
         B     RS20                GET NEXT SORT RECORD.                        
         DROP  R6                                                               
*                                                                               
* FOR FF RECS, BREAK ON ACCT NOT ID.                                            
*                                                                               
RS300    BAS   RE,WRITBREC                                                      
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
RS310    CLC   SVACCT,SRECRACT                                                  
         BE    RS320                                                            
         BAS   RE,WRITBREC                                                      
         MVC   SVACCT,SRECRACT                                                  
         BAS   RE,DOBREC                                                        
*                                                                               
RS320    AP    PAYERAMT,D1RAMT                                                  
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
*        MVC   MSG,=CL10'GET2'                                                  
*        GOTO1 ADUMP,DMCB,(RC),SRECA,750                                        
*                                                                               
RS321    LTR   R2,R2                                                            
         BZ    RS500               END OF SORT?                                 
         LA    R1,LSREC                                                         
         MOVE  (SRECCO,(R1)),(R2)  MOVE REC INTO MY AREA.                       
         CLI   SRECRID,X'FF'                                                    
         BE    RS310               DO ANOTHER FF REC                            
         B     RS40                ELSE WE ARE STARTING A NEW COMPANY.          
*                                                                               
RS500    DS    0H                                                               
         BAS   RE,DOFREC           CREATE FINAL RECORD ON TAPE.                 
         MVI   BYTE,C'W'                                                        
         BAS   RE,CLOSE            CLOSE  WORK FILE.                            
         GOTO1 SORTER,DMCB,=C'END' CLOSE THE SORT.                              
         CLI   QOPT1,C'Y'                                                       
         BE    EXIT                NO REGISTER IF PRINTING LIVE FORMS           
         BAS   RE,DOREG            DO REGISTER                                  
         MVI   BYTE,C'T'                                                        
         BAS   RE,CLOSE            CLOSE TAPE FILE.                             
         B     EXIT                                                             
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
*                                                                               
         USING TRECD,R6                                                         
         LA    R6,WKREC            BUILD "A" REC                                
         MVI   TREC,C'T'           RECORD TYPE                                  
         MVC   TYEAR,D1YEAR                                                     
         MVC   TMTFIND(2),=C'LS'   MAGNETIC TAPE FILE INDICATOR                 
         MVC   TTRID,D1CTRAN       TRANSMITTER CODE                             
         MVC   TCPYNM(36),D1CNM    COMPANY NAME                                 
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
         MVC   TTIN,SRECCID        EIN NUMBER                                   
*                                                                               
         ZAP   TOTBCNT,=P'0'                                                    
*                                                                               
         CLC   D1CTRAN,DDSTRANS    ONLY PUT IN TRANSMITTER NAME                 
         BNE   DOT10               IF DIFFERENT FROM PAYER.                     
         MVC   TCONNM(55),SPACES   CLEAR AGY CONTACT NAME/NUM                   
         MVC   TCONNM(13),=C'JOHN ACAMPORA'                                     
         MVC   TCPHONE(10),=C'2126335146'                                       
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
         MVC   MSG,=CL10'TREC'                                                  
         GOTO1 ADUMP,DMCB,(RC),TREC,750                                         
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
*                                                                               
         USING AREC,R6                                                          
         LA    R6,WKREC            BUILD "A" REC                                
         MVI   AREC,C'A'           RECORD TYPE                                  
         MVC   AYEAR,D1YEAR        1993                                         
         MVC   ACCOID,SRECCID      EIN NUMBER                                   
         MVI   AFRMTYP,C'A'        1099-MISC                                    
         MVC   AAMTCD,=CL12'1367'  AMOUNT CODES                                 
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
DOA10    MVC   ACOMPNM(36),D1CNM   COMPANY NAME                                 
         MVI   AZER,C'0'           NOT A TRANSFER AGENT                         
         MVC   ACADD1,D1CADLN1     STREET ADDRESS                               
         MVC   ACADCTY(L'D1CADCTY),D1CADCTY    CITY                             
         MVC   ACADST,D1CADST                      STATE                        
         MVC   ACADZIP,D1CADZIP                         ZIP                     
         MVC   ACPHONE,SRECPHON    PHONE NUMBER                                 
*                                                                               
* WRITE RECORD TO DISK/TAPE                                                     
*                                                                               
         LA    R5,AREC                                                          
         MVI   BYTE,C'W'           INDICATOR TO WRITE WORK FILE REC             
         BAS   RE,WTREC                                                         
*                                                                               
         ZAP   BRECOUNT,=P'0'                                                   
*                                                                               
         LA    R0,BTOT#                                                         
         LA    R1,BTOTALS                                                       
         ZAP   0(L'BTOTALS,R1),=P'0'                                            
         LA    R1,L'BTOTALS(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         MVC   MSG,=CL10'AREC'                                                  
         GOTO1 ADUMP,DMCB,(RC),AREC,750                                         
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
         MVC   BNAME(36),D1RNM                                                  
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
DFM100   BAS   RE,PRNT                                                          
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
         LA    RF,BFLNQ            L'DESTINATION                                
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
         BE    PRNT20                                                           
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
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* DATA MANAGER INTERFACE                                             *          
**********************************************************************          
         SPACE 1                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     GTFILE                                                           
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     GTFILE                                                           
*                                                                               
GTFILE   NTR1                                                                   
         L     R4,AIOAREA                                                       
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R4),(R4)                       
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* WRITE WORK RECORD                                                  *          
**********************************************************************          
         SPACE 1                                                                
WTREC    NTR1                                                                   
         CLI   BYTE,C'W'                                                        
         BNE   WT10                                                             
         PUT   ACT99WK,(R5)        WRITE WORK RECORD                            
         B     EXIT                                                             
*                                                                               
WT10     CLI   BYTE,C'T'                                                        
         BNE   WT20                                                             
         MVC   MSG,=CL10'PUTTPE'                                                
         GOTO1 ADUMP,DMCB,(RC),(R5),750                                         
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
LOOK40   OC    CORIGIN,CORIGIN                                                  
         BZ    LOOK50                                                           
         CLC   CORIGIN,ORIGINUM                                                 
         BNE   LOOK30                                                           
*                                                                               
LOOK50   CLC   COFFLIST,SPACES     IS THERE AN OFFICE LIST                      
         BE    LOOK60              NO- PRINCIPLE ID INFO IS DEFAULT             
         BAS   RE,LIMIT            CHECK LIMIT ACCESS                           
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
         MVC   D1CNM(L'CNAME),CNAME COMPANY NAME                                
         MVC   D1CADLNS,SPACES                                                  
         MVC   D1CADLN1(33),CADDR1 MOVE IN ADDRESS FROM CMP TBLE                
         MVC   D1CADLN2(33),CADDR2                                              
         MVC   D1CADLN3(33),CADDR3                                              
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
         MVC   P+19(36),INVNAME                                                 
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
         DC    A(DWNL)                                                          
         DC    A(IO1)                                                           
         DC    A(DWNBUF)                                                        
         DC    A(TRNTAB)                                                        
         DC    A(CPYTAB)                                                        
         DC    A(BUF1099)                                                       
         DC    A(EXTTBL)                                                        
         DC    A(STARS)                                                         
         DC    A(INVTBL)                                                        
         DC    A(DUMP)             DUMP ROUTINE                                 
*                                                                               
DDSDDN   DC    C'99'                                                            
DDSTRANS DC    C'19868'   ----***  DDS'S TRANSMITTER NO.  ***--                 
FCDDN    DC    C'FC'                                                            
FCTRANS  DC    C'99999'                                                         
ZEROS    DC    C'00000000000000000000'                                          
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
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
* SORTER / TAPE DEFINITIONS                                          *          
**********************************************************************          
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,33,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=750'                                   
*                                                                               
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
         DC    CL55' '                                                          
         DC    36C'*'                                                           
         DC    X'04'               3 ADDR LNS                                   
         DC    120C'*'                                                          
         DC    2C'*'                                                            
         DC    5C'9'                                                            
         DC    4C'*'                                                            
         DC    36C'*'                                                           
         DC    X'04'               4 ADDR LNS                                   
         DC    160C'*'                                                          
         DC    PL6'999999'                                                      
         DC    C'*'                                                             
         DC    290C' '                                                          
         EJECT                                                                  
**********************************************************************          
* ADDRESS EXCEPTION TABLE                                            *          
**********************************************************************          
         SPACE 1                                                                
EXTTBL   DC    AL1(3),C'C/O'                                                    
         DC    AL1(4),C'ATTN'                                                   
         DC    X'FF'                                                            
*                                                                               
TRNTAB   DS    0CL2                TO ENFORCE LIMITED ACCESS                    
         DC    C'BC'               READ TRANSACTIONS - NOT BUCKETS              
         DC    C'BD'               IDS IN THIS TABLE REFLECT                    
         DC    C'BP'               LIMITED ACCESS STATUS                        
         DC    C'BS'                                                            
         DC    C'CF'                                                            
         DC    C'DE'                                                            
         DC    C'DM'                                                            
         DC    C'DV'                                                            
         DC    C'DW'                                                            
         DC    C'D9'                                                            
         DC    C'FM'                                                            
         DC    C'H9'                                                            
         DC    C'ID'                                                            
         DC    C'I5'                                                            
         DC    C'MZ'                                                            
         DC    C'NE'                                                            
         DC    C'NW'                                                            
         DC    C'OA'                                                            
         DC    C'OM'                                                            
         DC    C'JW'                                                            
         DC    C'LC'                                                            
         DC    C'QJ'                                                            
         DC    C'SU'                                                            
         DC    C'TR'                                                            
TRNTBLNQ EQU   (*-TRNTAB)/L'TRNTAB    # OF ENTRIES IN TABLE                     
         EJECT                                                                  
**********************************************************************          
* COMPANY TABLE                                                      *          
**********************************************************************          
         SPACE 1                                                                
CPYTAB   DS    0F                                                               
*                                                                               
* AG - ARNOLD COMMUNICATIONS                                                    
*                                                                               
         DC    CL2'AG'             ACBOA                                        
         DC    AL2(6568)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'08548'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'043016107'      EIN#/TIN#                                    
         DC    X'01'                                                            
         DC    CL33'ARNOLD WORLDWIDE, INC.'  COMPANY NAME                       
         DC    CL33' '                       3X33 ADD LINES                     
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'GREGORY A. BATTY'     AGENCY CONTACT NAME                   
         DC    CL15'6175878506'           AGENCY PHONE NUMBER                   
*                                                                               
* AL - ANDERSON LEMBKE                                                          
*                                                                               
*        DC    CL2'AL'               ALNY                                       
*        DC    AL2(3735)             ORIGIN (DEFAULT IS PRINC ID)               
*        DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
*        DC    CL9'061340016'        EIN#/TIN#                                  
*        DC    X'00'                                                            
*        DC    CL33'ANDERSON AND LEMBKE, INC.'     COMPANY NAME                 
*        DC    CL33'320 WEST 13TH STREET'                                       
*        DC    CL33'NEW YORK, NY  10014'                                        
*        DC    CL33' '                                                          
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40'JOAN BURKHARDT'       AGENCY CONTACT NAME                   
*        DC    CL15'2128862118'           AGENCY PHONE NUMBER                   
*                                                                               
*        DC    CL2'AL'               ALSF                                       
*        DC    AL2(3737)             ORIGIN (DEFAULT IS PRINC ID)               
*        DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
*        DC    CL9'061340016'        EIN#/TIN#                                  
*        DC    X'00'                                                            
*        DC    CL33'ANDERSON AND LEMBKE, INC.'     COMPANY NAME                 
*        DC    CL33'320 WEST 13TH STREET'                                       
*        DC    CL33'NEW YORK, NY  10014'                                        
*        DC    CL33' '                                                          
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40'JOAN BURKHARDT'       AGENCY CONTACT NAME                   
*        DC    CL15'2128862118'           AGENCY PHONE NUMBER                   
*                                                                               
* AV - THE ADVERTISING COUNCIL, INC.                                            
*                                                                               
         DC    CL2'AV'               THE ADVERTISING COUNSEL                    
         DC    AL2(2111)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'130417693'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15'2129841946'    AGENCY PHONE NUMBER                          
*                                                                               
* BC - RAPP COLLINS MARCOA                                                      
*                                                                               
         DC    CL2'BC'               MDBO - RAPP, COLLINS, MARCOA               
         DC    AL2(1795)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'112679168'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'BC'               MDNY - RAPP, COLLINS, MARCOA               
         DC    AL2(1818)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'112679168'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33'RAPP COLLINS WORLDWIDE'                                     
         DC    CL33'11 MADISON AVE'             3 X 33 ADDR LINES               
         DC    CL33'NEW YORK, NY  10010'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'RALPH MARINO'  AGENCY CONTACT NAME                          
         DC    CL15'2125907024'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'BC'               MDCH - RAPP, COLLINS, MARCOA               
         DC    AL2(2105)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'364053944'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'CLIFF MARNICK'        AGENCY CONTACT NAME                   
         DC    CL15'3124544964'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'BC'               MDCP - RAPP, COLLINS, MARCOA               
         DC    AL2(2413)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'112679168'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33'RAPP COLLINS WORLDWIDE'     COMPANY NAME                    
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DICK HENDRIX'  AGENCY CONTACT NAME                          
         DC    CL15'2125907488'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'BC'             MDMIN- RAPP, COLLINS, MARCOA                 
         DC    AL2(2989)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'363698267'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33'RAPP COLLINS WORLDWIDE'  COMPANY NAME                       
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAN WILLETT'          AGENCY CONTACT NAME                   
         DC    CL15'6123733021'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'BC'               MDTS - JAN/98                              
         DC    AL2(6232)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133922303'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33'TEAM SOUTH, INC.'            COMPANY NAME                   
         DC    CL33'200 VARICK STREET'           3 X 33 ADDR LINES              
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '                  OFFICE LIST (SPACES - NO LIST)         
         DC    CL40'CLIFFORD J. MARNICK' AGENCY CONTACT NAME                    
         DC    CL15'2128868348'          AGENCY PHONE NUMBER                    
*                                                                               
* BD - BDDO                                                                     
*                                                                               
         DC    CL2'BD'               BDDE DETROIT                               
         DC    AL2(0163)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'35D71'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'382964249'        EIN#/TIN#                                  
         DC    X'01'                                                            
         DC    CL33'BBDO DETROIT'              COMPANY NAME                     
         DC    CL33'840 LONG LAKE ROAD'        3 X 33 ADDR LINES                
         DC    CL33'TROY,MI 48098'                                              
         DC    CL33' '                                                          
         DC    CL100' '                 OFFICE LIST (SPACES - NO LIST)          
         DC    CL40'RICK FORCE'         AGENCY CONTACT NAME                     
         DC    CL15'2128272000'         AGENCY PHONE NUMBER                     
*                                                                               
         DC    CL2'BD'               BBDO CHICAGO-BDCH                          
         DC    AL2(1049)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'35D71'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'223045170'        EIN#/TIN#                                  
         DC    X'01'                 CHANGED TO BD 12/96                        
         DC    CL33'BBDO CHICAGO'                                               
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'RICK FORCE'           AGENCY CONTACT NAME                   
         DC    CL15'2128272000'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'BD'             KENT                                         
         DC    AL2(1110)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'35D71'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'130473120'      EIN#/TIN#                                    
         DC    X'01'                                                            
         DC    CL33'BBDO WORLDWIDE'            COMPANY NAME                     
         DC    CL33'1285 AVE OF AMERICAS'      3 X 33 ADDR LINES                
         DC    CL33'NEW YORK, NY 10019'                                         
         DC    CL33' '                                                          
         DC    CL100' '             OFFICE LIST (SPACES - NO LIST)              
         DC    CL40'RICK FORCE'     AGENCY CONTACT NAME                         
         DC    CL15'2128272000'     AGENCY PHONE NUMBER                         
*                                                                               
         DC    CL2'BD'               BDA/BBDO -BDNAT - CHANGED 12/96            
         DC    AL2(3504)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'35D71'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'581597648'        EIN#/TIN#                                  
         DC    X'01'                                                            
         DC    CL33'BBDO SOUTH'                                                 
         DC    CL33'3414 PEACHTREE ROAD, SUITE 1600'                            
         DC    CL33'ATLANTA, GA 30326'                                          
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'RICK FORCE'           AGENCY CONTACT NAME                   
         DC    CL15'2128272000'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'BD'               BDATM ADDED FEB/2001                       
         DC    AL2(8083)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'35D71'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'134067883'        EIN#/TIN#                                  
         DC    X'01'                                                            
         DC    CL33'@TMOSPHERE INTERACTIVE'                                     
         DC    CL33'1375 BROADWAY'                                              
         DC    CL33'NEW YORK, NY 10018'                                         
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'RICK FORCE'           AGENCY CONTACT NAME                   
         DC    CL15'2128272000'           AGENCY PHONE NUMBER                   
*                                                                               
* BG - BWCNY                                                                    
*                                                                               
         DC    CL2'BG'               BWCNY - (3/95)                             
         DC    AL2(3282)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133767936'                                                   
         DC    X'00'                                                            
         DC    CL33'BERLIN CAMERON DOYLE'                                       
         DC    CL33'79 FIFTH AVENUE'       3 X 33 ADDR LINES                    
         DC    CL33'NEW YORK, NY  10003'                                        
         DC    CL33'ATTN: DIANE FELDMAN'                                        
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* BS - BATES USA                                                                
*                                                                               
         DC    CL2'BS'             BSNY - BACKER SPEILVOGEL BATES INC.          
         DC    AL2(0294)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19N35'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'132993871'      EIN#/TIN#                                    
         DC    X'01'                                                            
         DC    CL33'BATES ADVERTISING USA, INC.'    COMPANY NAME                
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DIANA SADOWSKI'       AGENCY CONTACT NAME                   
         DC    CL15'2122978129'           AGENCY PHONE NUMBER                   
*                                                                               
*        DC    CL2'BS'             BSNYGA-BACKER SPEILVOGEL BATES INC.          
*        DC    AL2(3665)           ORIGIN (DEFAULT IS PRINC ID)                 
*        DC    CL5'19N35'          TRANSMITTER#(00000 USE DDS#)-TCC             
*        DC    CL9'132545952'                                                   
*        DC    X'01'                                                            
*        DC    CL33'BATES ADVERTISING HOLDINGS, INC.'  COMPANY NAME             
*        DC    CL33'405 LEXINGTON AVENUE'        3 X 33 ADDR LINES              
*        DC    CL33'NEW YORK, NY  10174'                                        
*        DC    CL33' '                                                          
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40'DIANA SADOWSKI'       AGENCY CONTACT NAME                   
*        DC    CL15'2122978129'           AGENCY PHONE NUMBER                   
*                                                                               
* BW - BBDO INC.                                                                
*                                                                               
         DC    CL2'BW'               BBDO L.A. -BWLA                            
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'953308425'                                                   
         DC    X'00'                                                            
         DC    CL33'BBDO LOS ANGELES'                                           
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* CE - CAMPBELL MITHUN - ESTY                                                   
*                                                                               
         DC    CL2'CE'               CASH PLUS, INC. - CECP                     
         DC    AL2(2072)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'08K07'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'411267291'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'CE'               CAMPBELL-MITHUN ESTY CEND                  
         DC    AL2(2084)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'08K07'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'410985665'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'CE'               PROMOTION WORKS, INC. - CEPW               
         DC    AL2(2086)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'411333350'                                                   
         DC    X'00'                                                            
         DC    CL33'CME PROMOTION MARKETING'                                    
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'CE'               CECME                                      
         DC    AL2(2869)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'08K07'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'410985665'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'CE'               CEHO                                       
         DC    AL2(2818)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'741282588'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'CE'               CEMNRE                                     
         DC    AL2(3100)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'08K07'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'410985665'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'CE'               CEIPG                                      
         DC    AL2(4052)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'08K07'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'411807452'                                                   
         DC    X'01'                                                            
         DC    CL33'CAMPBELL-MITHUN-ESTY, L.L.C.'                               
         DC    CL33'222 SOUTH 9TH STREET'                                       
         DC    CL33'MINNEAPOLIS, MN  55402'                                     
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* CF - CARRAFIELLO, DIEHL AND ASSOCIATES                                        
*                                                                               
         DC    CL2'CF'               CACAR                                      
         DC    AL2(6049)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5' '                TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'132749326'        EIN                                        
         DC    X'00'                                                            
         DC    CL33'CARRAFIELLO, DIEHL AND ASSOCIATES'                          
         DC    CL33'90 NORTH BROADWAY'                                          
         DC    CL33'IRVINGTON, NY  10533'                                       
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'RICH PODLOVITS'  AGENCY CONTACT NAME                        
         DC    CL15'9146743995'      AGENCY PHONE NUMBER                        
*                                                                               
         DC    CL2'CF'               CAINK                                      
         DC    AL2(6050)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5' '                TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133082893'        EIN                                        
         DC    X'00'                                                            
         DC    CL33'INKWELL STUDIOS, INC.'                                      
         DC    CL33'90 NORTH BROADWAY'                                          
         DC    CL33'IRVINGTON, NY  10533'                                       
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'RICH PODLOVITS'  AGENCY CONTACT NAME                        
         DC    CL15'9146743995'      AGENCY PHONE NUMBER                        
*                                                                               
*        DC    CL2'CF'               CANYHN                                     
*        DC    AL2(6444)             ORIGIN (DEFAULT IS PRINC ID)               
*        DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
*        DC    CL9'133928399'        EIN                                        
*        DC    X'00'                                                            
*        DC    CL33'CDA HEALTH INC.'                                            
*        DC    CL33'90 NORTH BROADWAY'                                          
*        DC    CL33'IRVINGTON, NY  10533'                                       
*        DC    CL33' '                                                          
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40'RICH PODLOVITS'       AGENCY CONTACT NAME                   
*        DC    CL15'9146743995'           AGENCY PHONE NUMBER                   
*                                                                               
* CZ - CREATIVE MEDIA INCORPORATED                                              
*                                                                               
         DC    CL2'CZ'               CMINY                                      
         DC    AL2(6080)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133900116'        EIN                                        
         DC    X'00'                                                            
         DC    CL33'CREATIVE MEDIA INCORPORATED'                                
         DC    CL33'488 MADISON AVENUE'                                         
         DC    CL33'NEW YORK, NY  10022'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'LELAND SMITH'         AGENCY CONTACT NAME                   
         DC    CL15'2128402260'           AGENCY PHONE NUMBER                   
*                                                                               
* D9 - DEUTSCH, INC.                                                            
*                                                                               
         DC    CL2'D9'             DTNY - ADDED JAN/00                          
         DC    AL2(6486)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'132649664'      EIN                                          
         DC    X'00'                                                            
         DC    CL33' '                           COMPANY NAME                   
         DC    CL33' '                           3 X 33 ADDR LINES              
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '                                                         
         DC    CL40'RICHARD BRYAN'        AGENCY CONTACT NAME                   
         DC    CL15'2129957546'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'D9'             DTLA - DEUTSCH LA, INC.                      
         DC    AL2(6488)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'954545404'      EIN                                          
         DC    X'00'                                                            
         DC    CL33' '                           COMPANY NAME                   
         DC    CL33'111 8TH AVE. 14TH FLOOR'     3 X 33 ADDR LINES              
         DC    CL33'NEW YORK, NY 10011'                                         
         DC    CL33' '                                                          
         DC    CL100' '                                                         
         DC    CL40'BRIAN R. MAKWORO'     AGENCY CONTACT NAME                   
         DC    CL15'2129817887'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'D9'             DTED - DEUTSCH, INC.                         
         DC    AL2(6647)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133777133'      EIN                                          
         DC    X'00'                                                            
         DC    CL33' '                           COMPANY NAME                   
         DC    CL33' '                          3 X 33 ADDR LINES               
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '                                                         
         DC    CL40'BRIAN R. MAKWORO'     AGENCY CONTACT NAME                   
         DC    CL15'2129817887'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'D9'             DTJO - DEUTSCH, INC.                         
         DC    AL2(6648)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133506227'      EIN                                          
         DC    X'00'                                                            
         DC    CL33'DEUTSCH, INC.'               COMPANY NAME                   
         DC    CL33' '                           3 X 33 ADDR LINES              
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '                                                         
         DC    CL40'BRIAN MAKWORO'        AGENCY CONTACT NAME                   
         DC    CL15'2129817887'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'D9'             DTDNY - DEUTSCH, INC.                        
         DC    AL2(6649)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133592947'      EIN                                          
         DC    X'00'                                                            
         DC    CL33' '                           COMPANY NAME                   
         DC    CL33' '                           3 X 33 ADDR LINES              
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '                                                         
         DC    CL40'BRIAN R. MAKWORO'     AGENCY CONTACT NAME                   
         DC    CL15'2129817887'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'D9'             DTNYC - DEUTSCH, INC.                        
         DC    AL2(6789)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'132649664'      EIN                                          
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33' '                    3 X 33 ADDR LINES                     
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '                                                         
         DC    CL40'BRIAN R. MAKWORO'     AGENCY CONTACT NAME                   
         DC    CL15'2129817887'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'D9'             DTRU -ADDED JAN/00.                          
         DC    AL2(8112)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'134076292'      EIN                                          
         DC    X'00'                                                            
         DC    CL33'DRUSH, LLC'                   COMPANY NAME                  
         DC    CL33' '                            3 X 33 ADDR LINES             
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '                                                         
         DC    CL40'BRIAN R. MAKWORO'     AGENCY CONTACT NAME                   
         DC    CL15'2129817887'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'D9'             DTDD -ADDED JAN/2001                         
         DC    AL2(8342)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19X17'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'132649664'      EIN                                          
         DC    X'01'                                                            
         DC    CL33' '                            COMPANY NAME                  
         DC    CL33' '                            3 X 33 ADDR LINES             
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '                                                         
         DC    CL40'BRIAN R. MAKWORO'     AGENCY CONTACT NAME                   
         DC    CL15'2129817887'           AGENCY PHONE NUMBER                   
*                                                                               
* DB - CLIENT SERVICE                                                           
*                                                                               
         DC    CL2'*B'               DDSB                                       
         DC    AL2(2635)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'12345'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'123456789'        EIN                                        
         DC    X'01'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* DE - DELLA FEMINA MCNAMEE, INC.                                               
*                                                                               
         DC    CL2'DE'               DELLA FEMINA   DENYA                       
         DC    AL2(1737)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'132590823'        EIN                                        
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'DE'               DELLA FEMINA   DEWNY                       
         DC    AL2(2309)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133639857'        EIN                                        
         DC    X'00'                                                            
         DC    CL33'EUROCOM ADVERTISING N.A., INC.'                             
         DC    CL33'350 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'DE'               DELLA FEMINA   DEBO                        
         DC    AL2(2139)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133597470'        EIN                                        
         DC    X'00'                                                            
         DC    CL33'LAWNER REINGOLD BRITTON && PTRS'                            
         DC    CL33'C/O DELLA FEMINA MCNAMEE, INC'                              
         DC    CL33'350 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'DE'               DELLA FEMINA   DENYC                       
         DC    AL2(2380)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'132590823'        EIN                                        
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL99' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'DE'               DELLA FEMINA   DEDBO                       
         DC    AL2(2172)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133502994'        EIN                                        
         DC    X'00'                                                            
         DC    CL33'ORSATTI && PARRISH, INC.'                                   
         DC    CL33'C/O DELLA FEMINA MCNAMEE, INC'                              
         DC    CL33'350 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* DM - DOREMUS AND COMPANY                                                      
*                                                                               
*        DC    CL2'DM'               DOREMUS & COMPANY  DMNYO                   
*        DC    AL2(1440)             ORIGIN (DEFAULT IS PRINC ID)               
*        DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
*        DC    CL9'135033900'                                                   
*        DC    X'00'                                                            
*        DC    CL33' '             COMPANY NAME                                 
*        DC    CL99' '             3 X 33 ADDR LINES                            
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40' '             AGENCY CONTACT NAME                          
*        DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'DM'               DOREMUS & COMPANY  DMNYA                   
         DC    AL2(3023)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'35E66'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133712950'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33'200 VARICK STREET 12TH FLOOR' 3X33 ADD LINES                
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'ABRAHAM BARRETTO'     AGENCY CONTACT NAME                   
         DC    CL15'2123663000'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DM'               DOREMUS & COMPANY  DMNYD                   
         DC    AL2(3148)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'35E65'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'135033900'                                                   
         DC    X'01'                                                            
         DC    CL33'DOREMUS'       COMPANY NAME                                 
         DC    CL33'200 VARICK STREET 11TH FL.' 3X33 ADDR LINES                 
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'LYNN BASTA'           AGENCY CONTACT NAME                   
         DC    CL15'2123663010'           AGENCY PHONE NUMBER                   
*                                                                               
* DS - TALENT PARTNERS                                                          
*                                                                               
         DC    CL2'DS'               TALENT PARTNERS - TPCH                     
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'363800090'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'KEVIN BLACKMAN'        AGENCY CONTACT NAME                  
         DC    CL15'3129237918'            AGENCY PHONE NUMBER                  
*                                                                               
* DV - SPIER NEW YORK                                                           
*                                                                               
         DC    CL2'DV'             SDNY ADDED JAN/2001                          
         DC    AL2(7781)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133545478'      EIN                                          
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '                                                         
         DC    CL40'MARYBETH FICCARO'     AGENCY CONTACT NAME                   
         DC    CL15'2125619260'           AGENCY PHONE NUMBER                   
*                                                                               
* DW - SAATCHI AND SAATCHI NORTH AMERICA INC.                                   
*                                                                               
         DC    CL2'DW'             SAATCHI SAATCHI DFS, INC. - DWNYC            
         DC    AL2(1659)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'35D73'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133341336'      EIN                                          
         DC    X'01'                                                            
         DC    CL33' '                                                          
         DC    CL33'375 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '                                                         
         DC    CL40'ALAN CARR'            AGENCY CONTACT NAME                   
         DC    CL15'2124632456'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWCN                                         
         DC    AL2(2533)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133198075'      EIN                                          
         DC    X'00'                                                            
         DC    CL33'CONILL ADVERTISING, INC.'                                   
         DC    CL33'375 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014-3620'                                    
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'ALAN CARR'            AGENCY CONTACT NAME                   
         DC    CL15'2124632500'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             CONILL ADVERTISING - DWNYN                   
         DC    AL2(2534)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133198075'      EIN                                          
         DC    X'00'                                                            
         DC    CL33'CONILL ADVERTISING, INC.'                                   
         DC    CL33'375 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014-3620'                                    
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'DW'             DWKLC                                        
         DC    AL2(2783)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'132597353'      EIN                                          
         DC    X'00'                                                            
         DC    CL33'RESOURCE GROUP FOR HEALTH EDUCATI'                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124633462'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLM                                        
         DC    AL2(2784)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'132852539'      EIN                                          
         DC    X'00'                                                            
         DC    CL33'MEDICAL SERVICE PROJECTS, INC.'                             
         DC    CL33'375 HUDSON STREET 9 FLOOR'                                  
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124633400'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLT                                        
         DC    AL2(2978)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19686'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133888821'      EIN                                          
         DC    X'01'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124635099'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWVL                                         
         DC    AL2(3595)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133198075'      EIN                                          
         DC    X'00'                                                            
         DC    CL33'VISION LATINA COMMUNICATIONS'                               
         DC    CL33'375 HUDSON STREET, 8TH FLOOR'                               
         DC    CL33'NEW YORK, NY  10014-3660'                                   
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'DW'             DWKLMB - ADDED 12/97                         
         DC    AL2(4456)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133862182'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33'BESTMED, INC.'                                              
         DC    CL33'375 HUDSON STREET, 12TH FLOOR'                              
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124633457'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLE - ADDED 12/97                          
         DC    AL2(6455)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133939793'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL33'375 HUDSON STREET, 13TH FLOOR'                              
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124633651'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWCL - ADDED JAN/98                          
         DC    AL2(6457)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133920076'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33'CLIFF FREEMAN AND PARTNERS'                                 
         DC    CL33'375 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'PETER REGAN'          AGENCY CONTACT NAME                   
         DC    CL15'2124632303'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLF - ADDED DEC/98                         
         DC    AL2(6894)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133965952'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33'EVOLUTION HEALTH INITIATIVES, LLC'                          
         DC    CL33'485 A ROUTE 1 SOUTH SUITE 300'                              
         DC    CL33'ISELIN, NJ  08830'                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124633473'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLJ - ADDED JAN/00                         
         DC    AL2(7850)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133888821'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33'ADVANCED THERAPEUTICS GROUP'                                
         DC    CL33'375 HUDSON ST. - 9TH FLOOR'                                 
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124635099'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLN - ADDED JAN/00                         
         DC    AL2(7851)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133888821'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL33'375 HUDSON ST. - 9TH FLOOR'                                 
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124633469'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLG - ADDED JAN/00                         
         DC    AL2(7852)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133888821'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33'COLLEGEVILLE ADVERTISING ASSOC.'                            
         DC    CL33'416 EAST MAIN STREET, SUITE 201'                            
         DC    CL33'COLLEGEVILLE, PA 19426'                                     
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124633469'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLH - ADDED JAN/00                         
         DC    AL2(7853)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133888821'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33'HEALTHCARE RESOURCES GROUP'                                 
         DC    CL33'375 HUDSON ST. - 9TH FLOOR'                                 
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124633469'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLP - ADDED JAN/00                         
         DC    AL2(7959)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133888821'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33' '             3X33 ADDRESS LINES                           
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124635074'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'DW'             DWKLL - ADDED JAN/00                         
         DC    AL2(8052)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133888821'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33' '             3X33 ADDRESS LINES                           
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID HALPERN'        AGENCY CONTACT NAME                   
         DC    CL15'2124633469'           AGENCY PHONE NUMBER                   
*                                                                               
* DY - DYNAMIC PRODUCTIONS INC.                                                 
*                                                                               
         DC    CL2'DY'             DYTO - SAFFER                                
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'980123040'      EIN                                          
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '                                                         
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* FB - BBDO                                                                     
*                                                                               
         DC    CL2'FB'               TFB/BBDO, INC. - TFBPA                     
         DC    AL2(2793)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'942276267'                                                   
         DC    X'00'                                                            
         DC    CL33'TFB/BBDO, INC'                                              
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* FM - SFM MEDIA CORPORATION                                                    
*                                                                               
         DC    CL2'FM'               FMNY - SFM MEDIA CORPORATION               
         DC    AL2(0261)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133977932'                                                   
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'PREMA DESAI'          AGENCY CONTACT NAME                   
         DC    CL15'2127904892'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'FM'               FMNYE - SFM ENTERTAINMENT LLC              
         DC    AL2(6876)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133977929'                                                   
         DC    X'00'                                                            
         DC    CL33' '                           COMPANY NAME                   
         DC    CL33'1180 AVENUE OF THE AMERICAS' 3 X 33 ADDR LINES              
         DC    CL33'NEW YORK, NY 10018'                                         
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'PREMA DESAI'          AGENCY CONTACT NAME                   
         DC    CL15'2127904892'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'FM'               FMMPG - ADDED JAN/2001                     
         DC    AL2(8565)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133502994'                                                   
         DC    X'00'                                                            
         DC    CL33'MPG USA, INC.'             COMPANY NAME                     
         DC    CL33'1180 AVENUE OF THE AMERICAS' 3X33 ADDRESS LINES             
         DC    CL33'NEW YORK, NY 10018'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'PREMA DESAI'          AGENCY CONTACT NAME                   
         DC    CL15'2127904892'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'FM'               FMMC - ADDED JAN/2001                      
         DC    AL2(8566)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'522178035'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33'1180 AVENUE OF THE AMERICAS' 3X33 ADDRESS LINES             
         DC    CL33'NEW YORK, NY 10018'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'PREMA DESAI'          AGENCY CONTACT NAME                   
         DC    CL15'2127904892'           AGENCY PHONE NUMBER                   
*                                                                               
* GB - GRIFFIN BACAL, INC.                                                      
*                                                                               
         DC    CL2'GB'               GRIFFIN BACALL - GBNY                      
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'132941459'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15'2123376170'    AGENCY PHONE NUMBER                          
*                                                                               
* GE - HMS/RUHR, LTD.                                                           
*                                                                               
         DC    CL2'GE'               GENY - GEER DUBOIS INC.                    
         DC    AL2(2957)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'131555974'                                                   
         DC    X'00'                                                            
         DC    CL33'GEER DUBOIS INC.'                                           
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* GV - GAVIN ANDERSON AND COMPANY                                               
*                                                                               
         DC    CL2'GV'             GAVIN - DMGA                                 
         DC    AL2(2340)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133083151'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33'220 EAST 42ND STREET  SUITE 408' 3X33 ADDR LINES            
         DC    CL33'NEW YORK, NY 10017'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'WENDY OLIVA'          AGENCY CONTACT NAME                   
         DC    CL15'2123730251'    AGENCY PHONE NUMBER                          
*                                                                               
* HC - HILL HOLLIDAY CONNORS COSMOPULOS                                         
*                                                                               
         DC    CL2'HC'               HILL HOLIDAY HCLA                          
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'042437626'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* HH - HILL HOLLIDAY                                                            
*                                                                               
         DC    CL2'HH'               HILL HOLIDAY                               
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'042437626'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* HK - HILL AND KNOWLTON, INC.                                                  
*                                                                               
         DC    CL2'HK'               HILL & KNOWLTON - HKNY                     
         DC    AL2(1702)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19981'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133016062'        EIN#/TIN#                                  
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'JOEL SALMAN'          AGENCY CONTACT NAME                   
         DC    CL15'2128850408'    AGENCY PHONE NUMBER                          
*                                                                               
* HL - HILL AND KNOWLTON, INC.                                                  
*                                                                               
         DC    CL2'HL'               HLTOC                                      
         DC    AL2(2521)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'000000000'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* HR - HAL RINEY AND PARTNERS                                                   
*                                                                               
         DC    CL2'HR'             HAL RINEY & PARTNERS (87) - HRSF             
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'39482'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'942973839'                                                   
         DC    X'01'                                                            
         DC    CL33'PUBLICIS AND HAL RINEY'       COMPANY NAME                  
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'THERESA O''DONNELL'   AGENCY CONTACT NAME                   
         DC    CL15'4152932345'    AGENCY PHONE NUMBER                          
*                                                                               
* H3 - HOLLAND MARK MARTIN EDMUND                                               
*                                                                               
         DC    CL2'H3'             HOLLAND MARK MARTIN EDMUND - HMME            
         DC    AL2(6936)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'042903978'                                                   
         DC    X'00'                                                            
         DC    CL33'HOLLAND MARK MARTIN EDMUND'   COMPANY NAME                  
         DC    CL33' '            3X33 ADDRESS LINES                            
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DEBBIE SOURBORN'      AGENCY CONTACT NAME                   
         DC    CL15'6172957133'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'H3'             HOLLAND MARK MARTIN EDMUND - HMMEA           
         DC    AL2(6938)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'042903978'                                                   
         DC    X'00'                                                            
         DC    CL33'HOLLAND MARK MARTIN EDMUND'   COMPANY NAME                  
         DC    CL33'312 STUART STREET'                                          
         DC    CL33'BOSTON, MA  02116'                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'STEVEN A. REIDY'      AGENCY CONTACT NAME                   
         DC    CL15'6179603565'           AGENCY PHONE NUMBER                   
*                                                                               
* H9 - STARLINK                                                                 
*                                                                               
         DC    CL2'H9'             STACH STARLINK - ADDED JAN/2001              
         DC    AL2(8601)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'364347045'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33' '             3X33 ADDRESS LINES                           
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '               OFFICE LIST (SPACES - NO LIST)            
         DC    CL40'DEREK BENGTSON'   AGENCY CONTACT NAME                       
         DC    CL15'3122205735'       AGENCY PHONE NUMBER                       
*                                                                               
* ID - RAPP COLLINS - REPLACED FROM BC ON JAN/2001                              
*                                                                               
         DC    CL2'ID'               RCDA - ADDED JAN/2001                      
         DC    AL2(7557)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'752625096'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                3 X 33 ADDR LINES                         
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '             OFFICE LIST (SPACES - NO LIST)              
         DC    CL40'BRENDA MASSEY'  AGENCY CONTACT NAME                         
         DC    CL15'9725822028'     AGENCY PHONE NUMBER                         
*                                                                               
         DC    CL2'ID'               RCSCP - ADDED JAN/2001                     
         DC    AL2(8557)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133364184'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                3 X 33 ADDR LINES                         
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'RALPH MARINO'  AGENCY CONTACT NAME                          
         DC    CL15'2125907784'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'ID'               RCNY - ADDED JAN/2001                      
         DC    AL2(8558)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'112679168'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '            COMPANY NAME                                  
         DC    CL33'11 MADISON AVE'             3 X 33 ADDR LINES               
         DC    CL33'NEW YORK, NY  10010'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DOROTHY L FREDA'  AGENCY CONTACT NAME                       
         DC    CL15'2125907473'       AGENCY PHONE NUMBER                       
*                                                                               
         DC    CL2'ID'             RCMN - ADDED JAN/2001                        
         DC    AL2(8559)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'363698267'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33'901 MARQUETTE AVE S - 17TH FL' 3X33 ADDRESS LINES           
         DC    CL33'MINNEAPOLIS, MN 55429'                                      
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAN WILLETT'          AGENCY CONTACT NAME                   
         DC    CL15'6123733021'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'ID'               RCCH - ADDED JAN/2001                      
         DC    AL2(8561)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'364053944'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33'10 S. RIVERSIDE PLAZA SUITE 1920' 3X33 ADD LINES            
         DC    CL33'CHICAGO, IL 60606'                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'JOHN PACE'            AGENCY CONTACT NAME                   
         DC    CL15'3124668177'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'ID'             RCDS - ADDED JAN/2001                        
         DC    AL2(8588)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'364019827'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33'INFOWORKS, INC.'  COMPANY NAME                              
         DC    CL33'8410 W. BRYN MAWR SUITE 600'  3X33 ADDRESS LINES            
         DC    CL33'CHICAGO, IL 60631'                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'JOHN PACE'            AGENCY CONTACT NAME                   
         DC    CL15'3124668177'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'ID'             RCRD - ADDED JAN/2001                        
         DC    AL2(8589)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'134088029'      EIN#/TIN#                                    
         DC    X'00'                                                            
         DC    CL33'RAPP DIGITAL DIRECT, INC.' COMPANY NAME                     
         DC    CL33'11 MADISON AVENUE'         3X33 ADDRESS LINES               
         DC    CL33'NEW YORK, NY 10010'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'CYNTHIA RATTNER'      AGENCY CONTACT NAME                   
         DC    CL15'2125907580'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'ID'               RCAC - ADDED JAN/2001                      
         DC    AL2(8647)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'134052294'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33'ACUITY HEALTH GROUP'   COMPANY NAME                         
         DC    CL99' '                  3 X 33 ADDR LINES                       
         DC    CL100' '                 OFFICE LIST (SPACES - NO LIST)          
         DC    CL40'RICHARD P. HENDRIX' AGENCY CONTACT NAME                     
         DC    CL15'2125907488'         AGENCY PHONE NUMBER                     
*                                                                               
         DC    CL2'ID'               RCCO - ADDED JAN/2001                      
         DC    AL2(8648)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'112679168'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33'RAPP COLLINS'          COMPANY NAME                         
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '                 OFFICE LIST (SPACES - NO LIST)          
         DC    CL40'RICHARD P. HENDRIX' AGENCY CONTACT NAME                     
         DC    CL15'2125907488'         AGENCY PHONE NUMBER                     
*                                                                               
* I5 - INTEGRATED COMMUNICATIONS CORP.                                          
*                                                                               
         DC    CL2'I5'             ICCNJ - INTEGRATED COMMUNICATIONS            
         DC    AL2(7348)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19R35'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'222619437'                                                   
         DC    X'01'                                                            
         DC    CL33' '                            COMPANY NAME                  
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'MICHELE EVELAND'      AGENCY CONTACT NAME                   
         DC    CL15'9734512404'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'I5'             ICC01 - ADDED JAN/2001                       
         DC    AL2(7446)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19R35'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'222619437'                                                   
         DC    X'01'                                                            
         DC    CL33' '                            COMPANY NAME                  
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'MICHELE EVELAND'      AGENCY CONTACT NAME                   
         DC    CL15'9734512404'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'I5'             ICC02 - ADDED JAN/2001                       
         DC    AL2(7447)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19R35'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'222619437'                                                   
         DC    X'01'                                                            
         DC    CL33' '            COMPANY NAME                                  
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'MICHELE EVELAND'      AGENCY CONTACT NAME                   
         DC    CL15'9734512404'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'I5'             ICC03 - ADDED JAN/2001                       
         DC    AL2(7448)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19R35'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'222619437'                                                   
         DC    X'01'                                                            
         DC    CL33' '                            COMPANY NAME                  
         DC    CL33'5 SYLVAN WAY'                                               
         DC    CL33'PARSIPPANY, NJ 07054'                                       
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'MICHELE EVELAND'      AGENCY CONTACT NAME                   
         DC    CL15'9734512404'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'I5'             ICC04 - ADDED JAN/2001                       
         DC    AL2(7967)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19R35'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'222619437'                                                   
         DC    X'01'                                                            
         DC    CL33'INTERLINK HEALTHCARE COMMUNICATIONS' COMPANY NAME           
         DC    CL33'5 SYLVAN WAY'  3X33 ADDRESS LINES                           
         DC    CL33'PARSIPPANY, NJ 07054'                                       
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'MICHELE EVELAND'      AGENCY CONTACT NAME                   
         DC    CL15'9734512404'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'I5'             ICC02S - ADDED JAN/2001                      
         DC    AL2(8555)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19R35'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'222619437'                                                   
         DC    X'01'                                                            
         DC    CL33'SENIOR CARE SEMINAR'         COMPANY NAME                   
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'MICHELE EVELAND'      AGENCY CONTACT NAME                   
         DC    CL15'9734512404'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'I5'             ICC05 - ADDED JAN/2001                       
         DC    AL2(9169)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19R35'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'222619437'                                                   
         DC    X'01'                                                            
         DC    CL33' '              COMPANY NAME                                
         DC    CL33' '              3X33 ADDRESS LINES                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'MICHELE EVELAND'      AGENCY CONTACT NAME                   
         DC    CL15'9734512404'           AGENCY PHONE NUMBER                   
*                                                                               
* JW - J. WALTER THOMPSON                                                       
*                                                                               
         DC    CL2'JW'             J WALTER JWNYA - CHANGED FROM JWNYME         
         DC    AL2(0460)           ORIGIN (DEFAULT IS PRINC ID)   12/95         
         DC    CL5'19984'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133016052'                                                   
         DC    X'01'                                                            
         DC    CL33'J.WALTER THOMPSON, U.S.A.'   COMPANY NAME                   
         DC    CL33'466 LEXINGTON AVENUE'                                       
         DC    CL33'NEW YORK, NY  10017'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'GAYA RACHANSKI'       AGENCY CONTACT NAME                   
         DC    CL15'2122107718'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'JW'             J WALTER - JWBC                              
         DC    AL2(1773)           ORIGIN (DEFAULT IS PRINC ID)   12/99         
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133040332'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'GAYA RACHANSKI'       AGENCY CONTACT NAME                   
         DC    CL15'2122107718'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'JW'             J WALTER - JWTIG                             
         DC    AL2(6803)           ORIGIN (DEFAULT IS PRINC ID)   12/99         
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133974921'      TIN NUMBER                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'GAYA RACHANSKI'       AGENCY CONTACT NAME                   
         DC    CL15'2122107718'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'JW'             J WALTER - JWFR                              
         DC    AL2(7379)           ORIGIN (DEFAULT IS PRINC ID)   12/99         
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'943307791'                                                   
         DC    X'01'                                                            
         DC    CL33'FORESTERIA, INC.'     COMPANY NAME                          
         DC    CL33'4 EMBARCADERO CENTER'                                       
         DC    CL33'SAN FRANCISCO, CA  94111'                                   
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'GAYA RACHANSKI'       AGENCY CONTACT NAME                   
         DC    CL15'2122107718'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'JW'             J WALTER - JWT99                             
         DC    AL2(8239)           ORIGIN (DEFAULT IS PRINC ID)   12/99         
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133016052'                                                   
         DC    X'01'                                                            
         DC    CL33' '                    COMPANY NAME                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'GAYA RACHANSKI'       AGENCY CONTACT NAME                   
         DC    CL15'2122107718'    AGENCY PHONE NUMBER                          
*                                                                               
* KP - KALLIR, PHILIPS, ROSS INC.                                               
*                                                                               
         DC    CL2'KP'               KPNY                                       
         DC    AL2(2061)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19868'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133169392'        TIN/EIN #                                  
         DC    X'01'               CHANGED TO DWNLD JAN/98 AS PER REQ           
         DC    CL33'KALLIR, PHILIPS, ROSS, INC.'                                
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'SHARI NADELL'         AGENCY CONTACT NAME                   
         DC    CL15'2128568843'           AGENCY PHONE NUMBER                   
*                                                                               
*        DC    CL2'KP'               KPSF-KALLIR, PHILIPS ROSS INC              
*        DC    AL2(4589)             ORIGIN (DEFAULT IS PRINC ID)               
*        DC    CL5'19868'            TRANSMITTER#(00000 USE DDS#)-TCC           
*        DC    CL9'942554387'        TIN/EIN #                                  
*        DC    X'00'                                                            
*        DC    CL33'RAINOLDI KERZNER RADCLIFFE'                                 
*        DC    CL33'343 SANSOME STREET'                                         
*        DC    CL33'SAN FRANCISCO, CA 94104-1309'                               
*        DC    CL33' '                                                          
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40' '             AGENCY CONTACT NAME                          
*        DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
*        DC    CL2'KP'               KPNYC - JAN/98                             
*        DC    AL2(4591)             ORIGIN (DEFAULT IS PRINC ID)               
*        DC    CL5'19868'            TRANSMITTER#(00000 USE DDS#)-TCC           
*        DC    CL9'133169392'        TIN/EIN #                                  
*        DC    X'01'                                                            
*        DC    CL33'KALLIR, PHILIPS, ROSS, INC.'                                
*        DC    CL99' '             3 X 33 ADDR LINES                            
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40'NEVEEN MIGALY'        AGENCY CONTACT NAME                   
*        DC    CL15'2128568830'           AGENCY PHONE NUMBER                   
*                                                                               
*        DC    CL2'KP'               KPGM-KALLIR, PHILIPS ROSS INC              
*        DC    AL2(4770)             ORIGIN (DEFAULT IS PRINC ID)               
*        DC    CL5'19868'            TRANSMITTER#(00000 USE DDS#)-TCC           
*        DC    CL9'232722341'        TIN/EIN#                                   
*        DC    X'00'                                                            
*        DC    CL33'THE GMR GROUP'                                              
*        DC    CL33'1300 VIRGINIA DR. - SUITE 210'                              
*        DC    CL33'FT. WASHINGTON, PA  19034'                                  
*        DC    CL33' '                                                          
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40'SHARI NADELL'  AGENCY CONTACT NAME                          
*        DC    CL15'2128568830'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'KP'             KPKT - ADDED 12/97                           
         DC    AL2(6203)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19868'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'134038808'      TIN/EIN#                                     
         DC    X'00'                                                            
         DC    CL33'ACCEL HEALTHCARE COMMUNICATIONS L'                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'SHARI NADELL'  AGENCY CONTACT NAME                          
         DC    CL15'2128568830'    AGENCY PHONE NUMBER                          
*                                                                               
* LA - LARSON, COLBY                                                            
*                                                                               
         DC    CL2'LA'             LARSON, COLBY - LALA                         
         DC    AL2(1558)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'942597272'      TIN/EIN#                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* LC - GOTHAM                                                                   
*                                                                               
         DC    CL2'LC'             LAURENCE,CHARLES,FREE & LAWSON LCNYA         
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133753098'                                                   
         DC    X'00'                                                            
         DC    CL33'GOTHAM, INC.'              COMPANY NAME                     
         DC    CL33'100 FIFTH AVENUE'          3 X 33 ADDR LINES                
         DC    CL33'NEW YORK, NY 10011'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'PAM WALSH'            AGENCY CONTACT NAME                   
         DC    CL15'2124147033'    AGENCY PHONE NUMBER                          
*                                                                               
* LE - LEAGAS DELANEY                                                           
*                                                                               
         DC    CL2'LE'               LDSF                                       
         DC    AL2(6637)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'39J40'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133832066'                                                   
         DC    X'01'                                                            
         DC    CL33'LEAGAS DELANEY, INC.'           COMPANY NAME                
         DC    CL33'840 BATTERY STREET, 2ND FLOOR'  ADDRESS                     
         DC    CL33'SAN FRANCISCO, CA  94111'                                   
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'PANKAJ SEWAL'  AGENCY CONTACT NAME                          
         DC    CL15'4154395800'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'LE'               LDNOSH - ADDED FEB/00                      
         DC    AL2(6687)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'39J40'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133832066'                                                   
         DC    X'01'                                                            
         DC    CL33'LEAGAS DELANEY, INC.'           COMPANY NAME                
         DC    CL33' '                              ADDRESS                     
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'PANKAJ SEWAL'  AGENCY CONTACT NAME                          
         DC    CL15'4156771392'    AGENCY PHONE NUMBER                          
*                                                                               
* LM - LOWE AND PARTNERS/SMS                                                    
*                                                                               
         DC    CL2'LM'               LOWE - LMNYA                               
         DC    AL2(2365)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'131550675'                                                   
         DC    X'00'                                                            
         DC    CL33'LOWE && PARTNERS/INC.'          COMPANY NAME                
         DC    CL33'ONE DAG HAMMARSKJOLD PLAZA'     ADDRESS                     
         DC    CL33'NEW YORK, NY  10017-2203'                                   
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'SIM SIMSIMAN'         AGENCY CONTACT NAME                   
         DC    CL15'2126058979'    AGENCY PHONE NUMBER                          
*                                                                               
* LP - LOWE DIRECT                                                              
*                                                                               
*        DC    CL2'LP'               LOWE - LPNYD                               
*        DC    AL2(2482)             ORIGIN (DEFAULT IS PRINC ID)               
*        DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
*        DC    CL9'133900345'                                                   
*        DC    X'00'                                                            
*        DC    CL33'LOWE FOX PAVLIKA'                                           
*        DC    CL33'1114 AVENUE OF THE AMERICAS'                                
*        DC    CL33'NEW YORK, NY 10036-7796'                                    
*        DC    CL33' '                                                          
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40'SUSAN MOZDIERZ'       AGENCY CONTACT NAME                   
*        DC    CL15'2127437842'    AGENCY PHONE NUMBER                          
*                                                                               
*        DC    CL2'LP'               LOWE - LPNYDT                              
*        DC    AL2(6169)             ORIGIN (DEFAULT IS PRINC ID)               
*        DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
*        DC    CL9'133900345'                                                   
*        DC    X'00'                                                            
*        DC    CL33'LOWE DIRECT, INC.'                                          
*        DC    CL33'1114 AVENUE OF THE AMERICAS'                                
*        DC    CL33'NEW YORK, NY 10036-7796'                                    
*        DC    CL33' '                                                          
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40' '             AGENCY CONTACT NAME                          
*        DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* MC - MCCANN-ERICKSON, INC                                                     
*                                                                               
         DC    CL2'MC'               MCANN ERICKSON                             
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19A05'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'131938691'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* MK - MOXON DOLPHIN KERBY                                                      
*                                                                               
         DC    C'MK'               MOXON DOLPHIN KERBY ADVERTISING              
         DC    AL2(1911)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    C'521593746'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* MP - THE MINGO GROUP                                                          
*                                                                               
         DC    CL2'MP'             THE MINGO GROUP - TMGT                       
         DC    AL2(2979)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'132887300'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'CHARITO RIEL'    AGENCY CONTACT NAME                        
         DC    CL15'2126974515'      AGENCY PHONE NUMBER                        
*                                                                               
* MZ - MEZZINA/BROWN INC.                                                       
*                                                                               
         DC    CL2'MZ'             MZNY - MEZZINA BROWN                         
         DC    AL2(2736)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'134085826'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID FRUTKO'         AGENCY CONTACT NAME                   
         DC    CL15'2122517198'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'MZ'             MZIN - MEZZINA BROWN                         
         DC    AL2(3366)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133753385'                                                   
         DC    X'00'                                                            
         DC    CL33'M/B INTERACTIVE INC.'      COMPANY NAME                     
         DC    CL33'C/O MEZZINA/BROWN, INC.'                                    
         DC    CL33'401 PARK AVE. SOUTH - 7TH FL'    ADDRESS                    
         DC    CL33'NEW YORK, NY 10016'                                         
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'DAVID FRUTKO'  AGENCY CONTACT NAME                          
         DC    CL15'2122517716'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'MZ'             MZNYA - MEZZINA BROWN                        
         DC    AL2(8495)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'113077473'                                                   
         DC    X'00'                                                            
         DC    CL33' '                         COMPANY NAME                     
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                              ADDRESS                     
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'MARILYN BECK'  AGENCY CONTACT NAME                          
         DC    CL15'2122517719'    AGENCY PHONE NUMBER                          
*                                                                               
* NC - ROTANDO PARTNERS, INC.                                                   
*                                                                               
         DC    CL2'NC'             NCNY - NORTH CASTLE (86)                     
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'061470186'                                                   
         DC    X'00'                                                            
         DC    CL33'NORTH CASTLE COMMUNICATIONS, INC.'                          
         DC    CL33'15 BANK STREET'                                             
         DC    CL33'STAMFORD, CT 06901'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'CAROL DEMAIO'  AGENCY CONTACT NAME                          
         DC    CL15'2033582135'    AGENCY PHONE NUMBER                          
*                                                                               
* NE - DDB NEEDHAM WORLDWIDE INC.                                               
*                                                                               
         DC    CL2'NE'             DNNYA - DDB NEEDHAM WORLDWIDE                
         DC    AL2(1314)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19E33'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133355855'      TIN/EIN #                                    
         DC    X'01'                                                            
         DC    CL33'DDB WORLDWIDE COMMUNICATIONS GROU'                          
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'ANDREW J. KEREKES'  AGENCY CONTACT NAME                     
         DC    CL15'2124153170'         AGENCY PHONE NUMBER                     
*                                                                               
         DC    CL2'NE'             DNCHC - DDB NEEDHAM WORLDWIDE                
         DC    AL2(3839)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19E33'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'363711029'      TIN/EIN#                                     
         DC    X'01'                                                            
         DC    CL33'DDB CHICAGO INC.'     AGENCY NAME                           
         DC    CL33'437 MADISON AVENUE'   AGENCY ADDRESS                        
         DC    CL33'NEW YORK, NY  10022'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'ANDY KEREKES'  AGENCY CONTACT NAME                          
         DC    CL15'2124153170'    AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'NE'             DNGBA - DDB NEEDHAM WORLDWIDE INC.           
         DC    AL2(6812)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'132941459'      TIN/EIN#                                     
         DC    X'00'                                                            
         DC    CL33'GRIFFIN BACAL INC.'                                         
         DC    CL33' '             3X33 ADDRESS LINES                           
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'LEONARD RAMAUTAR'     AGENCY CONTACT NAME                   
         DC    CL15'2124153231'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'NE'             DNSE - DDB WORLDWIDE INC.                    
         DC    AL2(7023)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'911140296'      TIN/EIN#                                     
         DC    X'00'                                                            
         DC    CL33'ELGIN DDB'                                                  
         DC    CL33'1008 WESTERN AVE. SUITE #601'                               
         DC    CL33'SEATTLE, WA  98104'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'TRACY MIRABELLA'     AGENCY CONTACT NAME                    
         DC    CL15'2062236302'          AGENCY PHONE NUMBER                    
*                                                                               
         DC    CL2'NE'             DNSENS - ADDED JAN/2001                      
         DC    AL2(7659)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'911140296'      TIN/EIN#                                     
         DC    X'00'                                                            
         DC    CL33'DDB SEATTLE'                                                
         DC    CL33'1008 WESTERN AVE. SUITE 601'                                
         DC    CL33'SEATTLE, WA  98104'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'TRACY MIRABELLA'     AGENCY CONTACT NAME                    
         DC    CL15'2062236302'          AGENCY PHONE NUMBER                    
*                                                                               
* NR - DDBN RETAIL                                                              
*                                                                               
         DC    CL2'NR'             DDB NEEDHAM - RETAIL                         
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19E33'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133355855'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* NW - NW AYER INC                                                              
*                                                                               
         DC    CL2'NW'             N.W.AYER - AYNYR                             
         DC    AL2(2628)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19579'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'230376910'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'NW'               N.W.AYER - AYDR                            
         DC    AL2(3127)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19579'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133710113'                                                   
         DC    X'01'                                                            
         DC    CL33'DIRECT PRO, INC.'                                           
         DC    CL33'825 EIGHTH AVENUE'                                          
         DC    CL33'NY, NY  10019'                                              
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'NW'               N.W.AYER - AYWD                            
         DC    AL2(3140)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19M97'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133710114'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'NW'               N.W.AYER - AYME                            
         DC    AL2(3470)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19579'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133764500'                                                   
         DC    X'01'                                                            
         DC    CL33'MEDIA EDGE, INC.'           COMPANY NAME                    
         DC    CL33'825 EIGTH AVENUE'           COMPANY ADDRESS                 
         DC    CL33'NEW YORK, NY  10019'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'NW'               N.W.AYER - AYBM                            
         DC    AL2(3835)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19579'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133804028'                                                   
         DC    X'01'                                                            
         DC    CL33'BLUE MARBLE ACG, LTD.'      COMPANY NAME                    
         DC    CL33'825 EIGTH AVENUE'           COMPANY ADDRESS                 
         DC    CL33'NEW YORK, NY  10019'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* NV - DDS                                                                      
*                                                                               
         DC    CL2'NV'               NVNY                                       
         DC    AL2(2886)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133669079'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* OA - OGILVY AND MATHER                                                        
*                                                                               
         DC    CL2'OA'               OGILVY MATHER - OAEY                       
         DC    AL2(2801)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19939'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'132867398'                                                   
         DC    X'01'                                                            
         DC    CL33'EYEPATCH PRODUCTIONS, INC.'                                 
         DC    CL33'309 WEST 49TH STREET'                                       
         DC    CL33'NEW YORK, NY 10019-7399'                                    
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'HELEN RIPKA'          AGENCY CONTACT NAME                   
         DC    CL15'2122375685'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'OA'               OGILVY MATHER - OANYS                      
         DC    AL2(3319)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19939'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'132677706'                                                   
         DC    X'01'                                                            
         DC    CL33'GRAPHIC SERVICES, INC.'                                     
         DC    CL33'309 WEST 49TH STREET'                                       
         DC    CL33'NEW YORK, NY 10019-7399'                                    
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'HELEN RIPKA'          AGENCY CONTACT NAME                   
         DC    CL15'2122375685'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'OA'               OGILVY MATHER - OACOTT                     
         DC    AL2(3778)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19939'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'132555496'                                                   
         DC    X'01'                                                            
         DC    CL33'THE OGILVY GROUP, INC.'                                     
         DC    CL33'309 WEST 49TH STREET'                                       
         DC    CL33'NEW YORK, NY 10019-7399'                                    
         DC    CL33' '                                                          
         DC    CL100' '           OFFICE LIST                                   
         DC    CL40'HELEN RIPKA'          AGENCY CONTACT NAME                   
         DC    CL15'2122375685'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    CL2'OA'               OGILVY MATHER - OALAS                      
         DC    AL2(4057)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19939'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'953938806'                                                   
         DC    X'01'                                                            
         DC    CL33'SILVER BULLET INC.'                                         
         DC    CL33'3530 HAYDEN AVE.'      3X33 ADDRESS LINES                   
         DC    CL33'CULVER CITY, CA 90232'                                      
         DC    CL33' '                                                          
         DC    CL100' '                    OFFICE LIST                          
         DC    CL40'HELEN RIPKA'           AGENCY CONTACT NAME                  
         DC    CL15'2122375685'            AGENCY PHONE NUMBER                  
*                                                                               
         DC    CL2'OA'               CQNYP ADDED JAN/2001                       
         DC    AL2(6215)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'19939'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133916072'                                                   
         DC    X'01'                                                            
         DC    CL33' '                                                          
         DC    CL33'P.O. BOX 722'          3X33 ADDRESS LINES                   
         DC    CL33'NEW YORK, NY 10101'                                         
         DC    CL33' '                                                          
         DC    CL100' '                    OFFICE LIST                          
         DC    CL40'HELEN RIPKA'           AGENCY CONTACT NAME                  
         DC    CL15'2122375685'            AGENCY PHONE NUMBER                  
*                                                                               
* OH - DONINO WHITE & COMPANY                                                   
*                                                                               
         DC    CL2'OH'             DWPAT                                        
         DC    AL2(7554)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'943370942'                                                   
         DC    X'00'                                                            
         DC    CL33'DWP/BATES TECHNOLOGY'       COMPANY NAME                    
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'TIJUANA V. MESIDOR'   AGENCY CONTACT NAME                   
         DC    CL15'7706685700'           AGENCY PHONE NUMBER                   
*                                                                               
* ON - BBDO                                                                     
*                                                                               
         DC    CL2'ON'             OMNICOM-ONNY                                 
         DC    AL2(1252)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'00000'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'133463642'                                                   
         DC    X'00'                                                            
         DC    CL33'OMNICOM MANAGEMENT, INC.'       COMPANY NAME                
         DC    CL33'437 MADISON AVENUE'                                         
         DC    CL33'NEW YORK, NY 10022'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'STEVEN AZZOPARDI'     AGENCY CONTACT NAME                   
         DC    CL15'2124153677'           AGENCY PHONE NUMBER                   
*                                                                               
* PN - PORTER NOVELLI INC.                                                      
*                                                                               
         DC    CL2'PN'               PORTER NOVELLI- NY (DOREMUS) PNNY          
         DC    AL2(1513)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'35D68'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133461427'                                                   
         DC    X'01'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                       3 X 33 ADDR LINES                  
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* QJ - INGALLS, QUINN AND JOHNSON                                               
*                                                                               
         DC    CL2'QJ'               QJBO-INGALLS, QUINN & JOHNSON              
         DC    AL2(0656)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'043041769'        EIN#/TIN#                                  
         DC    X'01'                                                            
         DC    CL33'INGALLS'                                                    
         DC    CL33'ONE DESIGN CENTER PLACE'                                    
         DC    CL33'BOSTON, MA 02110'                                           
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'STEVEN DOHERTY'       AGENCY CONTACT NAME                   
         DC    CL15'6172957777'           AGENCY PHONE NUMBER                   
*                                                                               
* RB - RJ PALMER MEDIA, INC.                                                    
*                                                                               
         DC    CL2'RB'               RJNY                                       
         DC    AL2(3479)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133071688'                                                   
         DC    X'00'                                                            
         DC    CL33'R.J. PALMER, INC.'  COMPANY NAME                            
         DC    CL33' '                  3 X 33 ADDR LINES                       
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* RP - RUBIN POSTAER AND ASSOCIATES                                             
*                                                                               
         DC    CL2'RP'               RUBIN POSTAER ASSOCIATES                   
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'954062405'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* RU - RUHR/PARAGON INC                                                         
*                                                                               
         DC    CL2'RU'               RUMN                                       
         DC    AL2(2926)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'32P33'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'311441282'                                                   
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* SC - LOWE AND PARTNERS/SMS                                                    
*                                                                               
         DC    CL2'SC'               SCALI (86)                                 
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'132882093'                                                   
         DC    X'00'                                                            
         DC    CL33'SCALI, MCCABE, SLOVES INC.'                                 
         DC    CL33'1345 AVENUE OF THE AMERICAS'                                
         DC    CL33'NEW YORK, NY  10105'                                        
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* SG - LOWE AND PARTNERS/SMS                                                    
*                                                                               
         DC    CL2'SG'               MARIA                                      
         DC    AL2(2858)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'540789050'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* SH - SAFFER ADVERTISING                                                       
*                                                                               
         DC    CL2'SH'               SACH                                       
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'363216533'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* SJ - DDS                                                                      
*                                                                               
         DC    CL2'SJ'               SJR TEST                                   
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'77777'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'99SJRTEST'                                                   
         DC    X'01'               DOWNLOADING                                  
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* SP - SAATCHI AND SAATCHI ADVERTISING                                          
*                                                                               
         DC    CL2'SP'               SAATCHI && SAATCHI PROMOTIONS              
         DC    AL2(1730)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133451963'                                                   
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* ST - SGTSEC                                                                   
*                                                                               
         DC    CL2'ST'               SGTO - SAFFER                              
         DC    AL2(3223)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'980123040'        EIN                                        
         DC    X'00'                                                            
         DC    CL33'SAFFER GROUP, INC.'        COMPANY NAME                     
         DC    CL33'156 DUNCAN MILLS RD. UNIT 1'                                
         DC    CL33'DON MILLS, ONTARIO  M3B 3N2'                                
         DC    CL33' '                                                          
         DC    CL100' '                                                         
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* SU - SUSEC - DAVID CRAVIT AND ASSOCIATES, LTD                                 
*                                                                               
         DC    CL2'SU'               SAFFER - SUCH                              
         DC    AL2(2842)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'363863346'                                                   
         DC    X'00'                                                            
         DC    CL33'DAVID CRAVIT AND ASSOCIATES, LTD'                           
         DC    CL33'737 NORTH MICHIGAN AVE.  16TH FL'                           
         DC    CL33'CHICAGO, IL  60611'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
         DC    CL2'SU'               SAFFER - SUTO                              
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'980123040'                                                   
         DC    X'00'                                                            
         DC    CL33'SAFFER COMMUNICATIONS USA LTD'                              
         DC    CL33'54 WEST HUBBARD ST., SUITE 600'                             
         DC    CL33'CHICAGO, IL  60610'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* TH - ZENITH MEDIA SERVICES                                                    
*                                                                               
         DC    CL2'TH'               ZENY                                       
         DC    AL2(3816)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133736241'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '                                COMPANY NAME              
         DC    CL33'299 WEST HOUSTON STREET 10TH FL.' 3 X 33 ADDR LINES         
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '              OFFICE LIST (SPACES - NO LIST)             
         DC    CL40'JOHN F. KEANNA'  AGENCY CONTACT NAME                        
         DC    CL15'2128595160'      AGENCY PHONE NUMBER                        
*                                                                               
* TR - OMNICOM MANAGEMENT SERVICES                                              
*                                                                               
         DC    CL2'TR'               TLDA ADDED JAN/2001                        
         DC    AL2(0031)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'54430'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'751372103'        EIN#/TIN#                                  
         DC    X'01'                                                            
         DC    CL33'DDB DALLAS, INC.'                 COMPANY NAME              
         DC    CL33'1999 BRYAN STREET, SUITE 3200'    3 X 33 ADDR LINES         
         DC    CL33'DALLAS, TX 75201'                                           
         DC    CL33' '                                                          
         DC    CL100' '              OFFICE LIST (SPACES - NO LIST)             
         DC    CL40'TAMMY PIGOTT'    AGENCY CONTACT NAME                        
         DC    CL15'2142592542'      AGENCY PHONE NUMBER                        
*                                                                               
         DC    CL2'TR'               TLSF ADDED JAN/2001                        
         DC    AL2(0239)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'752625370'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33'THE FOCUS AGENCY'                 COMPANY NAME              
         DC    CL33'1999 BRYAN STREET, SUITE 3200'    3 X 33 ADDR LINES         
         DC    CL33'DALLAS, TX 75201'                                           
         DC    CL33' '                                                          
         DC    CL100' '              OFFICE LIST (SPACES - NO LIST)             
         DC    CL40'TAMMY PIGOTT'    AGENCY CONTACT NAME                        
         DC    CL15'2142592542'      AGENCY PHONE NUMBER                        
*                                                                               
         DC    CL2'TR'               TLDP ADDED JAN/2001                        
         DC    AL2(3258)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'54430'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'752498121'        EIN#/TIN#                                  
         DC    X'01'                                                            
         DC    CL33'PGC ADVERTISING, INC.'            COMPANY NAME              
         DC    CL33'1999 BRYAN STREET, SUITE 3200'    3 X 33 ADDR LINES         
         DC    CL33'DALLAS, TX 75201'                                           
         DC    CL33' '                                                          
         DC    CL100' '              OFFICE LIST (SPACES - NO LIST)             
         DC    CL40'TAMMY PIGOTT'    AGENCY CONTACT NAME                        
         DC    CL15'2142592542'      AGENCY PHONE NUMBER                        
*                                                                               
         DC    CL2'TR'               TLPP ADDED JAN/2001                        
         DC    AL2(5879)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'54430'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'752958035'        EIN#/TIN#                                  
         DC    X'01'                                                            
         DC    CL33'TLP L.P.'                         COMPANY NAME              
         DC    CL33'1999 BRYAN STREET, SUITE 3200'    3 X 33 ADDR LINES         
         DC    CL33'DALLAS, TX 75201'                                           
         DC    CL33' '                                                          
         DC    CL100' '              OFFICE LIST (SPACES - NO LIST)             
         DC    CL40'TAMMY PIGOTT'    AGENCY CONTACT NAME                        
         DC    CL15'2142592542'      AGENCY PHONE NUMBER                        
*                                                                               
         DC    CL2'TR'               TLDSP ADDED JAN/2001                       
         DC    AL2(7139)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'752267897'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33'TRACEY-LOCKE, INC DBA GROUP MANAG' COMPANY NAME             
         DC    CL33'1999 BRYAN STREET, SUITE 3200'    3 X 33 ADDR LINES         
         DC    CL33'DALLAS, TX 75201'                                           
         DC    CL33' '                                                          
         DC    CL100' '              OFFICE LIST (SPACES - NO LIST)             
         DC    CL40'TAMMY PIGOTT'    AGENCY CONTACT NAME                        
         DC    CL15'2142592542'      AGENCY PHONE NUMBER                        
*                                                                               
* UB - CARAT                                                                    
*                                                                               
         DC    CL2'UB'               CARAT ADDED JAN/00                         
         DC    AL2(7307)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'953408346'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '                          COMPANY NAME                    
         DC    CL33'3 PARK AVENUE - 16TH FLOOR' 3 X 33 ADDR LINES               
         DC    CL33'NEW YORK, NY  10016'                                        
         DC    CL33'ATTN: RONALD IOZZIO'                                        
         DC    CL100' '              OFFICE LIST (SPACES - NO LIST)             
         DC    CL40'RONALD IOZZIO'            AGENCY CONTACT NAME               
         DC    CL15'2125919134'               AGENCY PHONE NUMBER               
*                                                                               
* WL - ??????????????????                                                       
*                                                                               
         DC    CL2'WL'               WARING & LAROSA (86)                       
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'00000'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'133050248'        EIN#/TIN#                                  
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40' '             AGENCY CONTACT NAME                          
         DC    CL15' '             AGENCY PHONE NUMBER                          
*                                                                               
* WD - WIEDEN AND KENNEDY                                                       
*                                                                               
         DC    CL2'WD'               WDPO - WIEDEN & KENNEDY                    
         DC    AL2(0000)             ORIGIN (DEFAULT IS PRINC ID)               
         DC    CL5'29A70'            TRANSMITTER#(00000 USE DDS#)-TCC           
         DC    CL9'930832855'        EIN#/TIN#                                  
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'JANET BLACK'          AGENCY CONTACT NAME                   
         DC    CL15'5039377840'           AGENCY PHONE NUMBER                   
*                                                                               
* WW - WUNDERMAN WORLDWIDE INC.                                                 
*                                                                               
         DC    CL2'WW'             WWSFS - WUNDERMAN WORLDWIDE INC.             
         DC    AL2(3364)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19728'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'131493710'      EIN#/TIN#                                    
         DC    X'01'                                                            
         DC    CL33'Y && R, INC.  DBA  IMPIRIC'                                 
         DC    CL33'675 AVENUE OF AMERICAS'                                     
         DC    CL33'NEW YORK, NY  10010'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'STEVEN MANDELL'       AGENCY CONTACT NAME                   
         DC    CL15'2126144576'    AGENCY PHONE NUMBER                          
*                                                                               
* YE - CHAPMAN DIRECT ADVERTISING                                               
*                                                                               
*        DC    CL2'YE'             YCHNY - CHAPMAN DIRECT ADVERTISING           
*        DC    AL2(3698)           ORIGIN (DEFAULT IS PRINC ID)                 
*        DC    CL5'19728'          TRANSMITTER#(00000 USE DDS#)-TCC             
*        DC    CL9'131493710'      EIN#/TIN#                                    
*        DC    X'01'                                                            
*        DC    CL33'Y && R, INC.  DBA CHAPMAN'                                  
*        DC    CL33'230 PARK AVENUE SOUTH - 6TH FLOOR'                          
*        DC    CL33'NEW YORK, NY  10003'                                        
*        DC    CL33' '                                                          
*        DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*        DC    CL40'STEVEN MANDELL'       AGENCY CONTACT NAME                   
*        DC    CL15'2126144576'    AGENCY PHONE NUMBER                          
*                                                                               
* YF - BURSON MARSTELLER                                                        
*                                                                               
         DC    CL2'YF'             YBMSFS - BURSON                              
         DC    AL2(3354)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19728'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'131493710'      EIN#/TIN#                                    
         DC    X'01'                                                            
         DC    CL33'Y && R, INC. DBA BURSON MARSTELLER'                         
         DC    CL33'230 PARK AVENUE SOUTH'                                      
         DC    CL33'NEW YORK, NY  10003'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'STEVEN MANDELL'       AGENCY CONTACT NAME                   
         DC    CL15'2126144576'    AGENCY PHONE NUMBER                          
*                                                                               
* YN - BM/YR/SFS                                                                
*                                                                               
         DC    CL2'YN'             YNRO - YOUNG & RUBICAM INC.INC.              
         DC    AL2(2463)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19728'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'131493710'      EIN#/TIN#                                    
         DC    X'01'                                                            
         DC    CL33'YOUNG && RUBICAM, INC.' COMPANY NAME                        
         DC    CL33'285 MADISON AVENUE'     3X33 ADDRESS LINES                  
         DC    CL33'NEW YORK, NY  10017'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'STEVEN MANDELL'       AGENCY CONTACT NAME                   
         DC    CL15'2126144576'    AGENCY PHONE NUMBER                          
*                                                                               
* YP - SUDLER AND HENNESEY/SFS                                                  
*                                                                               
         DC    CL2'YP'             YSHNY - SUDLER AND HENNESEY/SFS              
         DC    AL2(3655)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    CL5'19728'          TRANSMITTER#(00000 USE DDS#)-TCC             
         DC    CL9'131493710'      EIN#/TIN#                                    
         DC    X'01'                                                            
         DC    CL33'Y && R, INC. DBA SUDLER && HENESSY'                         
         DC    CL33'230 PARK AVENUE SOUTH' 3X33 ADDRESS LINES                   
         DC    CL33'NEW YORK, NY  10003'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
         DC    CL40'STEVEN MANDELL'       AGENCY CONTACT NAME                   
         DC    CL15'2126144576'           AGENCY PHONE NUMBER                   
*                                                                               
         DC    C'**'                                                            
         EJECT                                                                  
**********************************************************************          
* BUFFERS                                                            *          
**********************************************************************          
         SPACE 1                                                                
IO1      DS    0D                                                               
         DS    2000C                                                            
*                                                                               
DWNBUF   DS    0D                                                               
         DS    500C                                                             
*                                                                               
INVTBL   DS    0D                                                               
         DS    (100*48)C           INVALID ADDRESS TABLE                        
*                                                                               
         DS    0D                                                               
         DC    C'*BF1099*'                                                      
BUF1099  DS    (132*22)C                                                        
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT8,C'Y'                                                       
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
*                                                                               
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
* FILE DCB                                                           *          
**********************************************************************          
         SPACE 1                                                                
         DCBD  DSORG=PS,DEVD=TA                                                 
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACTT02D  DSECT                                                                  
VTYPES   DS    0A                                                               
SORTER   DS    A                                                                
DLFLD    DS    A                                                                
CONVMOS  DS    A                                                                
SQUASHER DS    A                                                                
ADSCAN   DS    A                                                                
PRNTBL   DS    A                                                                
ADWNL    DS    A                                                                
AIOAREA  DS    A                                                                
ADWNBUF  DS    A                                                                
ATRNTAB  DS    A                                                                
ACPYTAB  DS    A                                                                
ABUF1099 DS    A                                                                
AEXTTBL  DS    A                                                                
ASTARS   DS    A                                                                
AINVTBL  DS    A                                                                
ADUMP    DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
AINVENT  DS    A                   CURRENT ADDRESS OF NEXT ERROR ENTRY          
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
TOTBCNT  DS    PL8                 TOTAL B COUNT                                
INVCNT   DS    PL6                 # OF INVALID ADDRESSES IN INVTBL             
COMMAND  DS    CL6                                                              
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
LSTSAVE  DS    CL3                 SAVE FOR $LIST OF OFFICES                    
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
SVKEY    DS    CL32                                                             
SVCNM    DS    CL36                                                             
SVCADNL  DS    XL1                                                              
SVPTYPE  DS    XL1                                                              
SVCDTE   DS    CL8                 SAVED AREA FOR DATE (INCLD CENTURY)          
*                                                                               
SVTRID   DS    CL5                 FOR USE BY FORM4804                          
         DS    CL1                 UNUSED                                       
WORK2    DS    CL120               FOR ADDRESS CRUNCHING                        
*                                                                               
SRECA    DS    CL750                                                            
SVSREC   DS    CL750               SAVE SRT REC FOR FORMS.                      
WKREC    DS    CL750               SPACE TO BUILD IRS RECORDS.                  
CTFILIO  DS    CL2048              IO AREA FOR CONTROL FILE READ                
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
CNAME    DS    CL33                COMPANY NAME FROM ID RECORD                  
CADDR    DS    0CL99                                                            
CADDR1   DS    CL33                COMPANY ADDR LINE 1                          
CADDR2   DS    CL33                COMPANY ADDR LINE 2                          
CADDR3   DS    CL33                COMPANY ADDR LINE 3                          
COFFLIST DS    CL100               LIMITED ACCESS OFFICE LIST                   
COFFLNQ  EQU   (*-COFFLIST)        LENGTH OF OFFICE LIST                        
CCONTNM  DS    CL40                AGENCY CONTACT NAME                          
CPHONE   DS    CL15                AGENCY PHONE NUMBER                          
CTLENQ   EQU   (*-CPYTABD)                                                      
         EJECT                                                                  
**********************************************************************          
* 1099 FORM DSECT                                                    *          
**********************************************************************          
         SPACE 1                                                                
BUFFERD  DSECT                                                                  
*                                                                               
BF1      DS    0CL132              **** LINE #1 ****                            
         DS    CL43                                                             
BFCRRCTD DS    CL1                 (CORRECTED)                                  
         ORG BF1+132                                                            
*                                                                               
BF2      DS    0CL132              **** LINE #2 ****                            
         ORG BF2+132                                                            
*                                                                               
BF3      DS    0CL132              **** LINE #3 ****                            
         DS    CL5                                                              
BFCMPNM  DS    CL33                (COMPANY NAME)                               
         DS    CL2                                                              
BFRENT   DS    CL12                (RENT)                                       
         ORG BF3+132                                                            
*                                                                               
BF4      DS    0CL132              **** LINE #4 ****                            
         DS    CL5                                                              
BFCMPAD1 DS    CL33                (1ST LINE OF COMPANY ADDRESS)                
         ORG BF4+132                                                            
*                                                                               
BF5      DS    0CL132              **** LINE #5 ****                            
         DS    CL5                                                              
BFCMPAD2 DS    CL33                (2ND LINE OF COMPANY ADDRESS)                
         DS    CL2                                                              
BFROYAL  DS    CL12                (ROYALTIES)                                  
         ORG BF5+132                                                            
*                                                                               
BF6      DS    0CL132              **** LINE #6 ****                            
         DS    CL5                                                              
BFCMPAD3 DS    0CL33               (3RD LINE OF COMPANY ADDRESS)                
BFPHON1  DS    CL15                (CPY PHONE # IF 2 LINES OF ADDRESS)          
         DS    CL18                                                             
         ORG BF6+132                                                            
*                                                                               
BF7      DS    0CL132              **** LINE #7 ****                            
         DS    CL5                                                              
BFPHON2  DS    CL15                (CPY PHONE # IF 3 LINES OF ADDRESS)          
         DS    CL20                                                             
BFPRIZES DS    CL12                (PRIZES,AWARDS)                              
         ORG BF7+132                                                            
*                                                                               
BF8      DS    0CL132              **** LINE #8 ****                            
         ORG BF8+132                                                            
*                                                                               
BF9      DS    0CL132              **** LINE #9 ****                            
         DS    CL9                                                              
BFCIDNO  DS    CL10                (COMPANY ID NUMBER)                          
         DS    CL6                                                              
BFSSNO   DS    CL11                (RECIPIENT'S SS#)                            
         ORG BF8+132                                                            
*                                                                               
BF9A     DS    0CL132              **** LINE #10 ****                           
         DS    CL68                                                             
**T      DS    CL67                                                             
BFYEAR   DS    CL4                 (CALENDAR YEAR)                              
         ORG BF9+132                                                            
*                                                                               
BF10     DS    0CL132              **** LINE #10 ****                           
         DS    CL68                                                             
**T      DS    CL67                                                             
**T      DS    CL4                 (CALENDAR YEAR)                              
         ORG BF10+132                                                           
*                                                                               
BF11     DS    0CL132              **** LINE #11 ****                           
         DS    CL5                                                              
BFRNAME  DS    CL33                (RECIPIENTS NAME)                            
         DS    CL2                                                              
BFMEDIC  DS    CL12                (MEDICAL)                                    
         DS    CL1                                                              
BFNONEMP DS    CL12                (NONEMPLOYEE COMPENSATION)                   
         ORG BF11+132                                                           
*                                                                               
BF12     DS    0CL132              **** LINE #12 ****                           
         ORG BF12+132                                                           
*                                                                               
BF13     DS    0CL132              **** LINE #13 ****                           
         DS    CL5                                                              
BFRADD1  DS    CL33                (RECIPIENT'S ADDRESS #1)                     
         ORG BF13+132                                                           
*                                                                               
BF14     DS    0CL132              **** LINE #14 ****                           
         DS    CL5                                                              
BFRADD2  DS    CL33                (RECIPIENT'S ADDRESS #2)                     
         DS    CL14                                                             
BFSINBOX DS    CL20                (PUT STAR IN BOX)                            
         ORG BF14+132                                                           
*                                                                               
BF15     DS    0CL132              **** LINE #15 ****                           
         DS    CL5                                                              
BFRADD3  DS    CL33                (RECIPIENT'S ADDRESS #3)                     
         ORG BF15+132                                                           
*                                                                               
BF16     DS    0CL132              **** LINE #16 ****                           
         DS    CL5                                                              
BFRADD4  DS    CL33                (RECIPIENT'S ADDRESS #4)                     
         ORG BF16+132                                                           
*                                                                               
BF17     DS    0CL132              **** LINE #17 ****                           
         ORG BF17+132                                                           
*                                                                               
BF18     DS    0CL132              **** LINE #18 ****                           
         DS    CL9                                                              
BFACCT   DS    CL12                (ACCOUNT NUMBER -OPTIONAL)                   
         ORG BF18+132                                                           
*                                                                               
BF19     DS    CL132                                                            
BF20     DS    CL132                                                            
BF21     DS    CL132                                                            
BF22     DS    CL132                                                            
BFEQU    EQU   22                  #LINES IN BUFFER                             
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
TMTFIND  DS    CL2                 MAGNETIC TAPE FILER INDICATOR                
TFILNM   DS    CL15                ELECTRONIC FILE NAME                         
         DS    CL375                                                            
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
         DS    CL511               BLANK                                        
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
         DS    CL45                BLANKS                                       
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
         DS    CL519                                                            
CLENQ    EQU   (*-CREC)                                                         
         EJECT                                                                  
***********************************************************************         
*               'F' RECORD DSECT                                      *         
***********************************************************************         
*                                                                               
FRECD    DSECT                                                                  
FREC     DS    0CL750              TAPE SUMMARY RECORD                          
         DS    CL1                 RECORD TYPE                                  
         DS    CL4                 MISTER IRS MAGNETIC MEDIA DIRECTOR           
*                                  SAID THAT WE DON'T HAVE TO USE THIS.         
         DS    CL25                                                             
         DS    CL200                                                            
         DS    CL190                                                            
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
INVNAME  DS    CL36                ACCOUNT NAME                                 
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
*                                                                               
D1CNM    DS    CL36      A         COMPANY NAME                                 
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
D1RNM    DS    CL36      A         RECIPIENTS NAME                              
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
         DS    CL273               SPARE                                        
LSREC    EQU   *-SRECCO            LENGTH OF SORT RECORD                        
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
**PAN#1  DC    CL21'095ACREPTT20003/09/01'                                      
         END                                                                    
