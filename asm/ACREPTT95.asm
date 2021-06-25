*          DATA SET ACREPTT95  AT LEVEL 040 AS OF 05/01/02                      
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
*              OPT3:'R'= PRODUCE REGISTER ONLY. (READ A TAPE)         *         
*                   'T'= PRODUCE TAPE.     (1ST PASS)                 *         
*              OPT4:'G'= CORRECTION RQST. (PRODUCE 'G' RECORDS)       *         
*                   ' '= REGULAR RUN.                                 *         
*              OPT6:' '= DDS IS TRANSMITTER FOR THE TAPE.             *         
*                   'C'= COMPANY IS ITS OWN TRANSMITTER.              *         
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
         BE    PRACC                                                            
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
         GOTO1 DATCON,DMCB,(0,QEND),(20,SVCDAT)                                 
         MVC   D1YEAR,QEND                                                      
         MVI   SKIPACCT,C'N'                                                    
         MVI   FCRDACC,C'Y'                                                     
         MVI   FCRDTRNS,C'Y'                                                    
         MVI   FCRDHIST,C'Y'                                                    
*                                                                               
         CLI   QOPT3,C'R'          IF REGISTER FROM INPUT TAPE THEN             
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
*                                                                               
         USING ACCOMPD,R2                                                       
         CLI   0(R2),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRINCID,ACMPID      PRINCIPLE ID NUMBER                          
         MVI   READIT,C'T'         THESE AGENCIES READ TRANSACTIONS             
*                                                                               
         LA    R0,TRNTBLNQ         NUMBER OF ENTRIES IN TABLE                   
         LA    R1,TRNTAB           AGENCIES READING TRANSACTION TABLE           
REQF20   CLC   ALPHAID,0(R1)                                                    
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
PRACC    DS    0H                                                               
         MVI   SORTCD,C'2'         SET SEQ  2C  COME BEFORE OTHER U/L'S         
*                                                                               
         CLI   OFPOS,C'1'          IS OFFPOS IN ACCT FILTS (F1-F4)              
         BL    PACC200             NO - SKIP FILTER LOOKUP                      
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
PACC200  CLI   READIT,C'T'         READ TRANSACTIONS OR BUCKETS                 
         BE    *+8                                                              
         BAS   RE,LOOKUP            LIMIT ACCESS                                
         XC    D1RNM(CLRLEN),D1RNM  CLEAR PAYER PART OF RECORD                  
         XC    SRECRACT,SRECRACT                                                
         ZAP   D1RAMT,=P'0'                                                     
*                                                                               
         L     R2,ADACC             ACCOUNT RECORD                              
         MVC   SVKEY,0(R2)                                                      
*                                                                               
         USING ACNAMED,R2                                                       
         L     R2,ADACCNAM          ACCOUNT NAME ELEMENT X'20'                  
         ZIC   R5,ACNMLEN                                                       
         MVC   D1RNM,SPACES                                                     
         SH    R5,=H'3'                                                         
         BM    PACC300                                                          
         EXMVC R5,D1RNM,ACNMNAME                                                
*                                                                               
PACC300  MVI   D1RADNL,0                                                        
         MVC   D1RADLN1,SPACES     CLEAR OUT ADDRESS LINES                      
         MVC   D1RADLN2,SPACES                                                  
         MVC   D1RADLN3,SPACES                                                  
         MVC   D1RADLN4,SPACES                                                  
         L     R2,ADACCADD         ADDRESS ELEMENT X'22' ELEM                   
         LTR   R2,R2                                                            
         BZ    PACC500                                                          
         USING ACADDD,R2                                                        
         CLI   ACADLNES,1          CHECK FOR BAD #LINES                         
         BL    PACC500                                                          
         CLI   ACADLNES,4                                                       
         BH    PACC500                                                          
         MVC   D1RADNL,ACADLNES    GET #LINES IN ADDRESS                        
*                                                                               
         ZIC   R5,ACADLNES                                                      
         LA    R3,D1RADLN1                                                      
         LA    R2,ACADADD                                                       
PACC400  MVC   0(L'ACADADD,R3),0(R2)                                            
         LA    R3,L'D1RADLN1(R3)                                                
         LA    R2,L'ACADADD(R2)                                                 
         BCT   R5,PACC400                                                       
*                                                                               
PACC500  DS    0H                                                               
         MVC   SRECRID,SPACES                                                   
         MVI   D1SSORID,C' '                                                    
         MVC   D1MIN6,QOPT2                                                     
         MVI   ELCODE,X'23'                                                     
         L     R2,ADACC                                                         
         BAS   RE,GETEL                                                         
         BE    PACC600             IF NO SS# THEN                               
         MVC   SRECRID,=X'FFFFFFFFFFFFFFFFFF'   DO NOT PUT ON TAPE.             
         MVI   SORTCD,C'3'         IN THIS CASE, LUMP ACCTS TOGETHER.           
         B     PACC700                                                          
*                                                                               
         USING ACOTHERD,R2                                                      
PACC600  MVC   SRECRID,ACOTNUM     RECIP SS NO.                                 
         MVC   D1SSORID,ACOTPROF   ID OR SS NO. INDICATOR                       
         MVC   D1MIN6,QOPT2                                                     
*                                                                               
PACC700  DS    0H                                                               
         L     R2,ADACC                                                         
         MVC   SRECRACT,3(R2)                                                   
         MVC   D1ACCT,1(R2)                                                     
         MVI   D1FORGN,C'N'        NOT A FOREIGN ADDRESS                        
         MVI   ELCODE,X'3F'        ONLINE MEMO ELEMENT                          
         L     R2,ADACC                                                         
         BAS   RE,GETEL                                                         
         BNE   PACC800                                                          
         USING ACOLMD,R2                                                        
         CLC   ACOLMEM(9),=C'FOREIGN=Y'                                         
         BNE   PACC800                                                          
         MVI   D1FORGN,C'Y'                                                     
*                                                                               
PACC800  DS    0H                                                               
         MVI   D1PTYPE,RSTXNCMP    DEFAULT TO NON EMPLOYEE COMP                 
         L     R2,ADACC                                                         
         MVI   ELCODE,RSTELQ       X'30' - STATUS ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   PACC900                                                          
         USING RSTELD,R2                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         BL    *+10                                                             
         MVC   D1PTYPE,RSTX1099    PAYMENT TYPE FOR THIS ACCOUNT                
*                                                                               
PACC900  MVI   SCANSW,SCANRECP     VALIDATE RECIPIENTS ADDRESS                  
         BAS   RE,FIXADDR          FIX ADDRESS                                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCHIST                                                           *          
**********************************************************************          
         SPACE 1                                                                
PHIST    DS    0H                                                               
         CLI   READIT,C'T'          READ TRANSACTIONS OR BUCKETS                
         BE    EXIT                                                             
         L     R2,ADTRANS                                                       
         CLI   0(R2),X'45'                                                      
         BNE   EXIT                                                             
         USING TRHISTD,R2                                                       
         CLC   TRHSYEAR(2),SVSDAT  LOWER THAN START YYMM                        
         BL    EXIT                                                             
         CLC   TRHSYEAR(2),SVEDAT  HIGHER THAN END YYMM                         
         BH    EXIT                                                             
         AP    D1RAMT,TRHSDR                                                    
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCTRNS                                                           *          
**********************************************************************          
         SPACE 1                                                                
PTRN     DS    0H                                                               
         CLI   READIT,C'T'         READ TRANSACTIONS OR BUCKETS                 
         BNE   EXIT                                                             
         L     R2,ADTRANS                                                       
         CLI   0(R2),X'44'                                                      
         BNE   EXIT                                                             
         USING TRANSD,R2                                                        
         GOTO1 CONVMOS,DMCB,(X'FE',(R2)),MOS    MOS FROM TRANSACTION            
         CLC   MOS,SVSDAT               IS MOS LOWER THAN START                 
         BL    EXIT                     YES- NEXT TRANSACTION                   
         CLC   MOS,SVEDAT               OR HIGHER THAN END DATE                 
         BH    EXIT                     YES- GET NEXT                           
         BAS   RE,LOOKUP                CHECKLIMIT ACCESS                       
*                                                                               
***********************************************************************         
*        CLC   ALPHAID,=C'SU'           SAFFER HARDCODE                         
*        BNE   PTRN50                                                           
*        MVC   SRECCID,=CL9'980123040'  JAN/93-AUG/93                           
*        MVC   D1CNM,=CL36'SAFFER COMMUNICATIONS USA, LTD'                      
*        CLC   MOS,=X'9308'                                                     
*        BL    PTRN175                                                          
*        MVC   SRECCID,=CL9'363863346'  SEP/93-DEC/93                           
*        MVC   D1CNM,=CL36'DAVID CRAVIT && ASSOC., LTD'                         
*        B     PTRN175                                                          
***********************************************************************         
*                                                                               
PTRN50   CLC   ALPHAID,=C'JW'      IF J WALTER IT'S RUNNING OFF SV              
         BNE   PTRN175             NOT 2C SO..............                      
         CLI   TRNSTYPE,3          LEAVE OUT MANUAL CHECKS                      
         BE    EXIT                                                             
         CLI   TRNSTYPE,37         SUBTRACT OUT TYPE 37 VOIDS                   
         BNE   PTRN160                                                          
         ZAP   D1RAMT,TRNSAMNT                                                  
         MP    D1RAMT,=P'-1'                                                    
*                                                                               
         L     RF,ADTRANS                                                       
PTRN150  ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),0                                                          
         BE    PTRN160                                                          
         CLI   0(RF),X'60'                                                      
         BNE   PTRN150                                                          
*                                                                               
         USING TRSELD,RF                                                        
         TM    TRSSTAT,TRSSVOID    VOIDED ITEM IN MARKER?                       
         BZ    *+10                                                             
         ZAP   D1RAMT,TRNSAMNT     IF VOIDED IN MARKER THEN DONT                
         B     PTRN150             SWITCH SIGN OF ITEM.                         
         DROP  RF                                                               
*                                                                               
PTRN160  L     RF,ADTRANS                                                       
PTRN165  ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),0                                                          
         BE    PTRN175                                                          
         CLI   0(RF),X'60'                                                      
         BNE   PTRN165                                                          
         USING TRSELD,RF                                                        
         CLC   TRSDATE,=X'BB9F'    SKIP ITEMS < 12/31/93                        
         BL    EXIT                                                             
         B     PTRN165                                                          
         DROP  RF                                                               
*                                                                               
PTRN175  TM    TRNSSTAT,X'80'                                                   
         BZ    EXIT                                                             
         ZAP   D1RAMT,TRNSAMNT                                                  
*                                                                               
PTRN200  BAS   RE,PUTSORT               PUT TO SORT                             
         B     EXIT                                                             
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
         BE    EXIT                                                             
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
         BNE   OL400               DO NEXT LEDGER                               
*                                                                               
* PROCESS HIGH LEVEL ACCOUNTS - (GET NAME/ADDRESS)                              
*                                                                               
         MVI   ELCODE,X'32'        BALANCE EL ABSENT ON HI LVLS.                
         LR    R2,R4               SO MAKE SURE WE HAVE A LOW LEVEL.            
         BAS   RE,GETEL                                                         
         BNE   OL100                                                            
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
         BZ    OL100                                                            
         USING ACADDD,R2                                                        
         CLI   ACADLNES,1          CHECK FOR BAD #LINES                         
         BL    OL100                                                            
         CLI   ACADLNES,4                                                       
         BH    OL100                                                            
         MVC   D1RADNL,ACADLNES    GET #LINES IN ADDRESS                        
*                                                                               
         ZIC   R0,ACADLNES                                                      
         LA    R1,D1RADLN1                                                      
         LA    R2,ACADADD                                                       
OL50     MVC   0(L'ACADADD,R1),0(R2)                                            
         LA    R1,L'D1RADLN1(R1)                                                
         LA    R2,L'ACADADD(R2)                                                 
         BCT   R0,OL50                                                          
         MVI   SCANSW,SCANRECP     VALIDATE RECIPIENTS ADDRESS                  
         BAS   RE,FIXADDR                                                       
*                                                                               
OL100    DS    0H                                                               
         MVC   SVKEY,0(R4)         NOW LOOK FOR SUB ACCOUNTS                    
         BAS   RE,SEQ                                                           
         CLC   SVKEY(15),0(R4)     SAME ACCT?                                   
         BNE   OL300               B IF NO.                                     
         CLI   READIT,C'T'         ARE WE READING TRANSACTIONS                  
         BE    OL190                                                            
*                                                                               
* PROCESS HISTORY ELEMENTS                                                      
*                                                                               
         MVI   ELCODE,X'45'        ACCUMULATE HISTORY ELTS.                     
         LR    R2,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   OL100                                                            
*                                                                               
         USING TRHISTD,R2                                                       
OL150    CLC   TRHSYEAR(2),SVSDAT  LOWER THAN START YYMM                        
         BL    OL180                                                            
         CLC   TRHSYEAR(2),SVEDAT  HIGHER THAN END YYMM                         
         BH    OL180                                                            
         AP    D1RAMT,TRHSDR                                                    
         CLC   ACKEYCON+1(2),=C'SC'  TAKE AWAY SUB SC CREDITS                   
         BNE   OL180                                                            
         SP    D1RAMT,TRHSCR                                                    
*                                                                               
OL180    BAS   RE,NEXTEL                                                        
         BE    OL150               LOOP BACK FOR NEXT 45 EL                     
         B     OL100               OR GET NEXT RECORD                           
*                                                                               
* PROCESS TRANSACTIONS                                                          
*                                                                               
OL190    MVI   ELCODE,X'44'             ACCUMULATE TRANS ELTS.                  
         LR    R2,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   OL100                                                            
         USING TRANSD,R2                                                        
         GOTO1 CONVMOS,DMCB,(X'FE',(R2)),MOS    MOS FROM TRANSACTION            
         CLC   MOS,SVSDAT               IS MOS LOWER THAN START                 
         BL    OL100                    YES- NEXT TRANSACTION                   
         CLC   MOS,SVEDAT               OR HIGHER THAN END DATE                 
         BH    OL100                    YES- GET NEXT                           
*                                                                               
         MVI   SKIPACCT,C'N'                                                    
         BAS   RE,LOOKUP                CHECKLIMIT ACCESS                       
         CLI   SKIPACCT,C'Y'            SKIP THIS TRANSACTION                   
         MVI   SKIPACCT,C'N'                                                    
         BE    OL100                    YES                                     
*                                                                               
* HANDLE VOIDS FROM BATCH/MARKER                                                
*                                                                               
         CLI   TRNSTYPE,37         SUBTRACT OUT TYPE 37 VOIDS                   
         BNE   OL250                                                            
         ZAP   D1RAMT,TRNSAMNT                                                  
         MP    D1RAMT,=P'-1'                                                    
*                                                                               
         LR    RF,R2                                                            
OL200    ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),0                                                          
         BE    OL275                                                            
         CLI   0(RF),X'60'                                                      
         BNE   OL200                                                            
*                                                                               
         USING TRSELD,RF                                                        
         TM    TRSSTAT,TRSSVOID    VOIDED ITEM IN MARKER?                       
         BZ    *+10                                                             
         ZAP   D1RAMT,TRNSAMNT     IF VOIDED IN MARKER THEN DONT                
         B     OL275               SWITCH SIGN OF ITEM.                         
         DROP  RF                                                               
*                                                                               
OL250    TM    TRNSSTAT,X'80'                                                   
         BZ    OL100                                                            
         ZAP   D1RAMT,TRNSAMNT                                                  
*                                                                               
OL275    DS    0H                                                               
         MVC   SRECRACT,SVKEY+3         MOVE IN ACCOUNT NO.                     
         MVC   D1ACCT,SVKEY+1                                                   
***********************************************************************         
*        CLC   ALPHAID,=C'SU'           SAFFER HARDCODE                         
*        BNE   OL280                                                            
*        MVC   SRECCID,=CL9'980123040'  JAN/93-AUG/93                           
*        MVC   D1CNM,=CL36'SAFFER COMMUNICATIONS USA, LTD'                      
*        CLC   MOS,=X'9308'                                                     
*        BL    OL280                                                            
*        MVC   SRECCID,=CL9'363863346'  SEP/93-DEC/93                           
*        MVC   D1CNM,=CL36'DAVID CRAVIT && ASSOC., LTD'                         
***********************************************************************         
OL280    BAS   RE,PUTSORT               PUT TO SORT                             
         B     OL100                                                            
*                                                                               
OL300    CLI   READIT,C'T'              ARE WE READING TRANSACTIONS             
         BE    OL400                                                            
         CP    D1RAMT,=P'0'                                                     
         BE    OL400                                                            
         BAS   RE,LOOKUP                CHECKLIMIT ACCESS                       
         MVC   SRECRACT,SVKEY+3         MOVE IN ACCOUNT NO.                     
         MVC   D1ACCT,SVKEY+1                                                   
         BAS   RE,PUTSORT                                                       
*                                                                               
OL400    L     R1,SV1              DO NEXT LEDGER                               
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    OL500                                                            
         MVC   CLGR,0(R1)                                                       
         ST    R1,SV1                                                           
         B     OL10                                                             
*                                                                               
OL500    LA    R1,READLIST         EXITING THE OTHRLDGR RTNE.                   
         ST    R1,SV1                                                           
         MVC   CLGR,0(R1)                                                       
         XC    ACKEYACC(42),ACKEYACC                                            
         L     R2,ADACC                                                         
         MVC   ACKEYACC(32),0(R2)  READ ACCOUNT RECORD AGAIN                    
         BAS   RE,READ             TO NOT SCREW UP MONACC                       
         MVI   SORTCD,C'2'         REESTABLISH 2C FIRST                         
         B     EXIT                                                             
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
         GOTO1 =A(PRNTIT),DMCB,(RC),=C'PUT',SRECA,420                           
         B     EXIT                                                             
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
         GOTO1 =A(PRNTIT),DMCB,(RC),=C'GET1',SRECA,420                          
*                                                                               
RS21     LTR   R2,R2               END OF SORT?                                 
         BZ    RS500                                                            
         LA    R1,LSREC                                                         
         MOVE  (SRECCO,(R1)),(R2)  MOVE REC INTO MY AREA.                       
         CLI   SVCID,0             FIRST RECORD?                                
         BNE   RS30                IF YES THEN OPEN.                            
         BAS   RE,OPEN             OPEN TAPE FILE AND WORK FILE.                
         B     RS50                                                             
*                                                                               
RS30     CLC   SVCID,SRECCID       SEE IF COMPANY BREAK.                        
         BE    RS100               B IF NO                                      
RS40     BAS   RE,DOCREC    IF NEW COMPANY, CREATE 'C' REC FOR OLD ONE          
*                                                                               
RS50     MVC   SVCID,SRECCID                                                    
         XC    SVRID,SVRID                                                      
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
         GOTO1 =A(PRNTIT),DMCB,(RC),=C'GET2',SRECA,420                          
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
         BAS   RE,CLOSE            CLOSE TAPE FILE AND WORK FILE.               
         GOTO1 SORTER,DMCB,=C'END' CLOSE THE SORT.                              
         CLI   QOPT1,C'Y'                                                       
         BE    EXIT                NO REGISTER IF PRINTING LIVE FORMS           
         BAS   RE,DOREG            DO REGISTER                                  
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
* BUILD 'A' RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
DOAREC   NTR1                      CLEAR OUT RECORD AREA                        
         USING AREC,R6                                                          
         LA    R6,WKREC            BUILD "A" REC                                
         MVI   AREC,C'A'           RECORD TYPE                                  
         MVC   AYEAR,D1YEAR        1993                                         
         MVC   AREELSEQ(3),=C'001' SEQUENCE NUMBER                              
         MVC   ACCOID,SRECCID      EIN NUMBER                                   
         MVI   AFRMTYP,C'A'        1099-MISC                                    
         MVC   AFRMTYP+1(4),=C'1367'    AMOUNT CODES                            
         CLI   QOPT4,C'G'                                                       
         BNE   *+8                                                              
         MVI   ACORRECT,C'C'       MARK TAPE AS A CORRECTION                    
         TM    DWNSTAT,DWNCMPY                                                  
         BO    *+8                                                              
         MVI   ASERVICE,C'1'       RETURN PROCESSED BY SERVICE BUREAU           
         MVC   AMTFIND(2),=C'LS'   MAGNETIC TAPE FILE INDICATOR                 
         MVC   ATRID,D1CTRAN       TRANSMITTER CODE                             
         MVC   ACOMPNM(36),D1CNM   COMPANY NAME                                 
         MVI   AZER,C'0'           NOT A TRANSFER AGENT                         
         MVC   ACADD1,D1CADLN1     STREET ADDRESS                               
         MVC   ACADD2,D1CADLN3     CITY,STATE ZIP                               
*                                                                               
         CLC   D1CTRAN,DDSTRANS    ONLY PUT IN TRANSMITTER NAME                 
         BNE   DA250               IF DIFFERENT FROM PAYER.                     
         MVC   ATRANNM(26),=C'DONOVAN DATA SYSTEMS, INC.'                       
         MVC   ATADDR1(20),=C'115 WEST 18TH STREET'                             
         MVC   ATADDR2(20),=C'NEW YORK,  NY  10011'                             
*                                                                               
* WRITE RECORD TO DISK/TAPE                                                     
*                                                                               
DA250    LA    R5,AREC                                                          
         MVI   BYTE,C'W'           INDICATOR TO WRITE WORK FILE REC             
         BAS   RE,WTREC                                                         
         CLI   QOPT3,C'T'          WRITE RECORD TO TAPE?                        
         BNE   *+12                                                             
         MVI   BYTE,C'T'           INDICATOR TO WRITE TAPE REC                  
         BAS   RE,WTREC                                                         
         CLI   QOPT7,C'Y'          DOWNLOADING                                  
         BNE   *+12                                                             
         MVI   BYTE,C'D'                                                        
         BAS   RE,WTREC                                                         
*                                                                               
DA300    ZAP   BRECOUNT,=P'0'                                                   
*                                                                               
         LA    R0,BTOT#                                                         
         LA    R1,BTOTALS                                                       
         ZAP   0(L'BTOTALS,R1),=P'0'                                            
         LA    R1,L'BTOTALS(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         GOTO1 =A(PRNTIT),DMCB,(RC),=C'AREC',AREC,420                           
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
         CLI   QOPT4,C'G'                                                       
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
         MVC   BNAME(36),D1RNM                                                  
         MVC   BNAME2,SPACES                                                    
         BAS   RE,CONDENSE         IRS WANTS ONLY 40 CHARS SO ELIMINATE         
         MVC   BADDR,ADDRST        ATTN, C/O AND OTHER NONESSENTIALS            
         MVC   BCSZ,D1RADLN4       CITY, STATE ZIP                              
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
WB5      UNPK  WORK(10),PAYERAMT                                                
         OI    WORK+9,X'F0'                                                     
         MVC   BAMT7,WORK                                                       
         B     WB30                                                             
*                                                                               
WB10     AP    BTOTALS,PAYERAMT                                                 
*                                                                               
WB20     UNPK  WORK(10),PAYERAMT                                                
         OI    WORK+9,X'F0'                                                     
*                                                                               
         TM    SVPTYPE,RSTXRENT                                                 
         BNO   *+20                                                             
         MVC   BAMT1,WORK          RENT                                         
         AP    BTOT1,PAYERAMT                                                   
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
         AP    BRECOUNT,=P'1'      INCREMENT RECORD COUNT.                      
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
         CLI   QOPT3,C'T'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'T'           INDICATOR TO WRITE TAPE REC                  
         BAS   RE,WTREC                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'D'           DOWNLOADING                                  
         BAS   RE,WTREC                                                         
*                                                                               
*  IF FORMS SUPRESSED THEN DON'T PRINT IT.                                      
*                                                                               
WB100    BAS   RE,DOFORM           PRINT OUT 1099 FORM.                         
*                                                                               
WB200    ZAP   PAYERAMT,=P'0'      CLEAR PAYER BUCKET.                          
         GOTO1 =A(PRNTIT),DMCB,(RC),=C'BREC',BREC,420                           
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
         UNPK  CAMT1(15),BTOT1                                                  
         OI    CAMT1+14,X'F0'                                                   
         UNPK  CAMT2(15),BTOT2                                                  
         OI    CAMT2+14,X'F0'                                                   
         UNPK  CAMT3(15),BTOT3                                                  
         OI    CAMT3+14,X'F0'                                                   
         UNPK  CAMT4(15),BTOT4                                                  
         OI    CAMT4+14,X'F0'                                                   
         UNPK  CAMT5(15),BTOT5                                                  
         OI    CAMT5+14,X'F0'                                                   
         UNPK  CAMT6(15),BTOT6                                                  
         OI    CAMT6+14,X'F0'                                                   
         UNPK  CAMT7(15),BTOT7                                                  
         OI    CAMT7+14,X'F0'                                                   
         UNPK  CAMT8(15),BTOT8                                                  
         OI    CAMT8+14,X'F0'                                                   
         UNPK  CAMT9(15),BTOT9                                                  
         OI    CAMT9+14,X'F0'                                                   
*                                                                               
         MVI   CREC,C'C'                                                        
         UNPK  CBCOUNT,BRECOUNT                                                 
         OI    CBCOUNT+5,X'F0'                                                  
*                                                                               
* WRITE RECORD TO DISK.                                                         
*                                                                               
         LA    R5,CREC                                                          
         MVI   BYTE,C'W'           INDICATOR TO WRITE WORK FILE REC             
         BAS   RE,WTREC                                                         
*                                                                               
* IF TAPE REQUESTED THEN WRITE RECORD TO TAPE.                                  
*                                                                               
         CLI   QOPT3,C'T'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'T'           INDICATOR TO WRITE TAPE REC                  
         BAS   RE,WTREC                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'D'           DOWNLOADING                                  
         BAS   RE,WTREC                                                         
*                                                                               
DC100    MVI   FIRSTPRT,C'Y'          IF NEW COMPANY, REALIGN FORMS.            
         GOTO1 =A(PRNTIT),DMCB,(RC),=C'CREC',CREC,420                           
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
*  IF TAPE REQUESTED THEN WRITE RECORD TO TAPE.                                 
*                                                                               
         CLI   QOPT3,C'T'                                                       
         BNE   *+12                                                             
         MVI   BYTE,C'T'           INDICATOR TO WRITE TAPE REC                  
         BAS   RE,WTREC                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DF100                                                            
         MVI   BYTE,C'D'           DOWNLOADING                                  
         BAS   RE,WTREC                                                         
*                                                                               
DF100    DS    0H                                                               
         GOTO1 =A(PRNTIT),DMCB,(RC),=C'FREC',FREC,420                           
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
DFM100   BAS   RE,SPRINT                                                        
         BCT   R2,DFM100                                                        
         MVI   FIRSTPRT,C'N'       TURN OFF FIRST TIME SWITCH                   
*                                                                               
DFM200   LA    R9,SVSREC           REAL PRINT                                   
         ZAP   D1RAMT,PAYERAMT                                                  
         BAS   RE,SPRINT                                                        
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT 1099 FORM                                                    *          
**********************************************************************          
         SPACE 1                                                                
SPRINT   NTR1                      CLEAR OUT BUFFER                             
         LA    R0,SPACES           A(SOURCE)                                    
         SR    R1,R1               L'SOURCE                                     
         ICM   R1,8,SPACES         PAD CHAR = C' '                              
         L     RE,ABUF1099         A(DESTINATION)                               
         LA    RF,BFLNQ            L'DESTINATION                                
         MVCL  RE,R0                                                            
*                                                                               
         USING BUFFERD,R6                                                       
         L     R6,ABUF1099                                                      
         CLI   QOPT4,C'G'          OPTION FOR CORRECTED FORM                    
         BNE   SP10                                                             
         MVI   BFCRRCTD,C'X'       MARK PRINTED FORM 'CORRECTED'                
*                                                                               
SP10     DS    0H                  DO WORK IN WORK AREA                         
         MVC   BFCMPNM,D1CNM       COMPANY NAME                                 
*                                                                               
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
SP15     MVC   0(L'BFCMPAD1,R2),0(R3)                                           
         CLC   0(L'BFCMPAD1,R2),SPACES                                          
         BE    *+8                 SKIP A LINE ONLY IF SMTHNG PRNTED            
         LA    R2,132(R2)                                                       
         LA    R3,L'ADDRST(R3)                                                  
         BCT   R0,SP15                                                          
*                                                                               
         MVC   BFCIDNO(2),SRECCID  COMPANY ID #  EX/ 99-9999999                 
         MVI   BFCIDNO+2,C'-'                                                   
         MVC   BFCIDNO+3(7),SRECCID+2                                           
*                                                                               
         CLI   D1SSORID,C'I'       IS IT A FED ID NO                            
         BNE   SP20                                                             
         MVC   BFSSNO(2),SRECRID   ID NO.                                       
         MVI   BFSSNO+2,C'-'                                                    
         MVC   BFSSNO+3(7),SRECRID+2                                            
         B     SP30                                                             
*                                                                               
SP20     MVC   BFSSNO(3),SRECRID   SOCIAL SECURITY NUMBER                       
         MVI   BFSSNO+3,C'-'                                                    
         MVC   BFSSNO+4(2),SRECRID+3                                            
         MVI   BFSSNO+6,C'-'                                                    
         MVC   BFSSNO+7(4),SRECRID+5                                            
*                                                                               
SP30     DS    0H                                                               
         MVC   BFYEAR,SVCDTE       YYYY                                         
*        MVC   BFYEAR(2),=C'19'                                                 
*        MVC   BFYEAR+2(2),D1YEAR                                               
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
         TM    SVPTYPE,RSTXPRIZ                                                 
         BZ    *+8                                                              
         LA    R4,BFPRIZES         PRIZES                                       
         TM    SVPTYPE,RSTXMEDC                                                 
         BZ    *+8                                                              
         LA    R4,BFMEDIC          MEDICAL EXPENSES                             
         EDIT  (P6,D1RAMT),(12,(R4)),2,COMMAS=YES                               
*                                                                               
SP38     DS    0H                                                               
         CLC   D1RADLN1,SPACES     REMOVE BLANK LINES                           
         BNE   SP40                                                             
         MVC   D1RADLN1,D1RADLN2   SHIFT UP ALL LINES                           
         MVC   D1RADLN2,D1RADLN3                                                
         MVC   D1RADLN3,SPACES                                                  
SP40     CLC   D1RADLN2,SPACES                                                  
         BNE   SP45                                                             
         MVC   D1RADLN2,D1RADLN3   SHIFT UP 3RD LINE                            
         MVC   D1RADLN3,SPACES                                                  
*                                                                               
SP45     DS    0H                                                               
         LA    R0,3                                                             
         LA    R1,D1RADLN1         COUNT THE NUMBER OF LINES USED               
         LA    R2,0                                                             
SP50     CLC   0(L'D1RADLN1,R1),SPACES                                          
         BE    SP80                                                             
         LA    R2,1(R2)            INCREMENT COUNTER                            
         LA    R1,L'D1RADLN1(R1)   NEXT LINE                                    
         BCT   R0,SP50                                                          
*                                                                               
SP80     DS    0H                  R2 HAS #LINES                                
         OR    R2,R2                                                            
         BZ    SP150                                                            
         LA    R1,BFRADD2          FOR 1 OR 2 LINES OF ADDRESS                  
         C     R2,=F'1'            START ON 2ND LINE                            
         BE    SP100                                                            
         C     R2,=F'2'                                                         
         BE    SP100                                                            
         LA    R1,BFRADD1          ELSE START ON 1ST LINE                       
*                                                                               
SP100    DS    0H                                                               
         LA    R3,D1RADLN1                                                      
*                                                                               
SP125    MVC   0(L'BFRADD1,R1),0(R3)                                            
         LA    R1,132(R1)                                                       
         LA    R3,L'D1RADLN1(R3)                                                
         BCT   R2,SP125                                                         
*                                                                               
SP150    MVC   ADDRCSZ,D1RADLN4    CITY, STATE ZIP ALWAYS ON 4TH LINE           
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
         BE    SP300                                                            
*                                                                               
         USING BUFFERD,R6                                                       
         L     R6,ABUF1099         PRINT OUT 1099 FORM                          
         LA    R0,BFEQU            NUMBER OF LINES                              
SP200    MVC   P,0(R6)             MOVE IN PRINT LINE                           
         BAS   RE,WRITE                                                         
         LA    R6,132(R6)          BUMP TO NEXT LINE                            
         BCT   R0,SP200                                                         
*                                                                               
SP300    CLI   QOPT2,C'Y'           IF ITEMS<$600 = Y THEN DONT                 
         BNE   *+14                 PRINT ON INVALID ADDRESS REPORT             
         CP    D1RAMT,=P'60000'                                                 
         BL    EXIT                                                             
*                                                                               
         CLC   D1RADLN4,=CL40'*** INVALID ADDRESS ***'                          
         BNE   EXIT                                                             
         CP    INVCNT,=P'100'      MAX NUMBER OF ERRORS TO PRINT                
         BNL   EXIT                                                             
         AP    INVCNT,=P'1'        BUMP COUNT OF ERRORS                         
         USING INVADDD,R1                                                       
         L     R1,AINVENT                                                       
         MVC   INVACCT,SRECRACT    SAVE ACCT WHERE ERROR IS                     
         MVC   INVNAME,D1RNM       SAVE NAME OF ACCOUNT                         
         LA    R1,INVEQU(R1)                                                    
         ST    R1,AINVENT                                                       
         B     EXIT                                                             
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
         CLI   QOPT3,C'T'                                                       
         BNE   OPX                                                              
         MVI   DSPARM+15,C'1'                                                   
         MVC   DSPARM+12(2),D1COMPCD                                            
         MVC   DSPARM+14(1),QOPT5                                               
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),(0,DSPARM)                              
         OPEN  (ACT99TP,(OUTPUT))                                               
OPX      B     EXIT                                                             
*                                                                               
CLOSE    NTR1                                                                   
         CLOSE ACT99WK                                                          
         CLI   QOPT7,C'Y'                                                       
         BNE   CLOSE10                                                          
         TM    DWNSTAT,DWNINIT                                                  
         BZ    CLOSE10                                                          
         GOTO1 ADWNL,DMCB,(RC),(R9),,DWNEOR                                     
*                                                                               
CLOSE10  CLI   QOPT3,C'T'                                                       
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
         CLI   QOPT7,C'Y'          DOWNLOADING - THEN NO REGISTER               
         BE    EXIT                                                             
         OPEN  (ACT99WK,(INPUT))   OPEN WORK OR TAPE FILE.                      
         LA    R5,WKREC                                                         
*                                                                               
DOREG20  DS    0H                                                               
         GET   ACT99WK,(R5)        READ RECORD.                                 
         CLI   0(R5),C'A'                                                       
         BE    PAREC                                                            
         CLI   0(R5),C'B'                                                       
         BE    PBREC                                                            
         CLI   0(R5),C'C'                                                       
         BE    PCREC                                                            
         CLI   0(R5),C'F'                                                       
         BE    PFREC                                                            
         DC    H'0'                MUST BE A,B,C OR F                           
*                                                                               
PAREC    DS    0H                  PRINT 'A' RECORD.                            
         USING AREC,R5                                                          
         MVI   RCSUBPRG,4                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   SVCNM,ACOMPNM       SAVE IT FOR HEADLINE.                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+1(17),=C'TRANSMITTER CODE='                                    
         MVC   P+18(6),ATRID                                                    
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+63(40),ATRANNM                                                 
         MVC   P+22(40),ACOMPNM                                                 
         MVC   P+12(9),ACCOID                                                   
         MVC   P+1(10),=C'CMPNY ID#='                                           
         MVC   PSECOND+63(40),ATADDR1                                           
         MVC   PSECOND+22(40),ACADD1                                            
         MVC   PSECOND+12(1),AFRMTYP                                            
         MVC   PSECOND+1(10),=C'FORM TYPE='                                     
         MVC   PTHIRD+63(40),ATADDR2                                            
         MVC   PTHIRD+22(40),ACADD2                                             
         MVC   PTHIRD+1(10),=C'AMT CODES='                                      
         MVC   PTHIRD+12(9),AFRMTYP+1                                           
         GOTO1 ACREPORT                                                         
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         LA    R0,BTOT#                                                         
         LA    R1,BTOTALS                                                       
         ZAP   0(L'BTOTALS,R1),=P'0'                                            
         LA    R1,L'BTOTALS(R1)                                                 
         BCT   R0,*-10                                                          
         B     DOREG20             GET NEXT RECORD.                             
         EJECT                                                                  
**********************************************************************          
* PRINT 'B' RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
         USING BREC,R5                                                          
PBREC    DS    0H                  PRINT 'B' RECORD.                            
         CLI   BSSNO,X'FF'                                                      
         BNE   PB05                                                             
         CLI   RCSUBPRG,2                                                       
         BE    PB05                                                             
         MVC   P+1(25),=CL25'*** COMPANY TOTALS ***'                            
         EDIT  (P8,BTOTALS),(14,P+55),2,COMMAS=NO                               
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   RCSUBPRG,2          FOR FIRST FF RECORD SKIP PAGE.               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
PB05     LA    R0,SPACES           CLEAR OUT PRINT BUFFER                       
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
         BE    PB10                                                             
         MVC   PRBSSNO(3),BSSNO                                                 
         MVI   PRBSSNO+3,C'-'                                                   
         MVC   PRBSSNO+4(2),BSSNO+3                                             
         MVI   PRBSSNO+6,C'-'                                                   
         MVC   WORK(9),=9X'F0'                                                  
         MVZ   WORK(9),BSSNO                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BE    PB15                                                             
         MVC   PRBERROR,=CL30'*** INVALID TAX ID NUMBER ***'                    
         B     PB20                                                             
*                                                                               
PB10     MVC   PRBERROR,=CL30'*** MISSING TAX ID NUMBER ***'                    
         CLI   BSSNO,X'FF'                                                      
         BE    PB20                                                             
*                                                                               
PB15     MVZ   BSSNO+8(1),=X'F0'                                                
         MVC   PRBSSNO+7(4),BSSNO+5                                             
*                                                                               
PB20     MVC   WORK2,SPACES                                                     
         MVC   WORK2(L'BCSZ),BCSZ                                               
         GOTO1 SQUASHER,DMCB,WORK2,L'WORK2                                      
         MVC   PRBCSZ,WORK2                                                     
         CLI   BSSNO,X'FF'                                                      
         BE    PB22                                                             
         CLI   BCORR,X'FF'                                                      
         BNE   PB30                                                             
         MVC   PRBERROR,=CL30'*** NEGATIVE AMOUNT ***'                          
         OI    STATUS,STATNEG                                                   
PB22     PACK  WORK(5),BAMT7(10)   NON EMPLOYEE COMPENSATION                    
         ZAP   AMOUNT,WORK(5)                                                   
         B     PB35                                                             
*                                                                               
PB30     ZAP   AMOUNT,=P'0'                                                     
         PACK  WORK(5),BAMT1(10)   RENT                                         
         AP    AMOUNT,WORK(5)                                                   
         AP    BTOT1,WORK(5)                                                    
         PACK  WORK(5),BAMT3(10)   PRIZES                                       
         AP    AMOUNT,WORK(5)                                                   
         AP    BTOT3,WORK(5)                                                    
         PACK  WORK(5),BAMT6(10)   MEDICAL                                      
         AP    AMOUNT,WORK(5)                                                   
         AP    BTOT6,WORK(5)                                                    
         PACK  WORK(5),BAMT7(10)   NON EMPLOYEE COMPENSATION                    
         AP    AMOUNT,WORK(5)                                                   
         AP    BTOT7,WORK(5)                                                    
         AP    BTOTALS,AMOUNT                                                   
PB35     EDIT  (P8,AMOUNT),(10,PRBAMT),2,COMMAS=NO                              
*                                                                               
         USING SRCED,R4                                                         
         LA    R4,BSRCLIST                                                      
         USING PRBRECD,R6                                                       
         L     R6,ABUF1099                                                      
         MVC   PRBSRCE,BACCT                                                    
         CLI   BSSNO,X'FF'                                                      
         BE    PB60                                                             
*                                                                               
         LA    R3,SRCEMAX          MAX NUMBER                                   
PB55     CLI   0(R4),0                                                          
         BE    PB60                                                             
         MVC   PRBSRCE,SRCEACCT                                                 
         EDIT  (P6,SRCEAMNT),(10,PRBAMT2),2,COMMAS=NO,FLOAT=-                   
         LA    R4,SRCELNQ(R4)      NEXT ACCT                                    
         LA    R6,PRBLNQ1(R6)      NEXT LINE                                    
         BCT   R3,PB55                                                          
*                                                                               
PB60     L     R6,ABUF1099                                                      
         LA    R3,SRCEMAX          MAX NUMBER PRINT LINES                       
*                                                                               
PB65     CLC   0(L'P,R6),SPACES    ANYTHING TO PRINT?                           
         BE    PB70                                                             
         MVC   P,0(R6)             MOVE IN PRINT LINE                           
         GOTO1 ACREPORT                                                         
         LA    R6,132(R6)          BUMP TO NEXT LINE                            
         BCT   R3,PB65                                                          
*                                                                               
PB70     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         B     DOREG20                                                          
         EJECT                                                                  
**********************************************************************          
* PRINT 'C' RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
PCREC    DS    0H                  PRINT 'C' RECORD.                            
         USING CREC,R5                                                          
         CLI   RCSUBPRG,2                                                       
         BE    PC20                B IF WE PRINTED COMP TOTALS ALREADY.         
         MVC   P+1(25),=CL25'*** COMPANY TOTALS ***'                            
         EDIT  (P8,BTOTALS),(14,P+55),2,COMMAS=NO                               
         GOTO1 ACREPORT                                                         
*                                                                               
PC20     GOTO1 ACREPORT                                                         
         CLI   QOPT8,C'A'                                                       
         BNE   PC30                                                             
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
         PACK  WORK(8),CAMT1(15)                                                
         ZAP   AMOUNT,WORK(8)                                                   
         MVC   P+1(9),=CL9'CAMT1 = '                                            
         EDIT  (P8,AMOUNT),(14,P+10),2,COMMAS=NO                                
         PACK  WORK(8),CAMT3(15)                                                
         ZAP   AMOUNT,WORK(8)                                                   
         MVC   P+31(9),=CL9'CAMT3 = '                                           
         EDIT  (P8,AMOUNT),(14,P+40),2,COMMAS=NO                                
         PACK  WORK(8),CAMT6(15)                                                
         ZAP   AMOUNT,WORK(8)                                                   
         MVC   P+61(9),=CL9'CAMT6 = '                                           
         EDIT  (P8,AMOUNT),(14,P+70),2,COMMAS=NO                                
         PACK  WORK(8),CAMT7(15)                                                
         ZAP   AMOUNT,WORK(8)                                                   
         MVC   P+91(9),=CL9'CAMT7 = '                                           
         EDIT  (P8,AMOUNT),(14,P+100),2,COMMAS=NO                               
         GOTO1 ACREPORT                                                         
*                                                                               
PC30     MVC   P(34),=C'NUMBER OF PAYEES FOR THIS COMPANY='                     
         MVC   P+35(6),CBCOUNT                                                  
         GOTO1 ACREPORT                                                         
         B     DOREG20                                                          
*                                                                               
PFREC    DS    0H                  PRINT 'F' RECORD.                            
         USING FREC,R5                                                          
         MVC   P(16),=C'** "F" RECORD **'                                       
         GOTO1 ACREPORT                                                         
         CLOSE ACT99WK                                                          
*                                                                               
         TM    STATUS,STATNEG                                                   
         BZ    PFREC2                                                           
         GOTO1 ACREPORT                                                         
         MVC   P(60),=CL60'*** NEGATIVE AMOUNT IN CURRENT RUN ***'              
         GOTO1 ACREPORT                                                         
*                                                                               
PFREC2   BAS   RE,FORMINV          PRINT INVALID ADDRESS LIST                   
         BAS   RE,FORM4804         PRINT 4804 FORM                              
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
LOOK01   CLC   CALPHA,=C'**'       END OF TABLE                                 
         BNE   LOOK01A                                                          
         MVI   SKIPACCT,C'Y'       SKIP THIS ACCOUNT                            
         B     LOOK99                                                           
*                                                                               
LOOK01A  CLC   CALPHA,ALPHAID      MATCH ON AGENCY ALPHA                        
         BE    LOOK04                                                           
LOOK02   LA    R3,CTLENQ(R3)       NEXT TABLE ENTRY                             
         B     LOOK01                                                           
*                                                                               
LOOK04   OC    CORIGIN,CORIGIN                                                  
         BZ    LOOK04A                                                          
         CLC   CORIGIN,ORIGINUM                                                 
         BNE   LOOK02                                                           
*                                                                               
LOOK04A  CLC   COFFLIST,SPACES     IS THERE AN OFFICE LIST                      
         BE    LOOK06              NO- PRINCIPLE ID INFO IS DEFAULT             
         BAS   RE,LIMIT            CHECK LIMIT ACCESS                           
         BNE   LOOK02              OFFICE NOT IN THIS OFFICE LIST               
         CLI   READIT,C'T'         OR IF NOT READING TRANSACTION                
         BNE   LOOK06                                                           
         CLC   CORIGIN,ORIGINUM    CHECK ORIGIN NUMB AGAINST TAB ENTRY          
         BE    LOOK06                                                           
         MVI   SKIPACCT,C'Y'       SKIP THIS ACCOUNT                            
         B     LOOK99                                                           
*                                                                               
LOOK06   DS    0H                                                               
         MVC   SRECCO,SPACES                                                    
         MVC   D1CNM,SPACES                                                     
         MVC   D1CNM(L'CNAME),CNAME COMPANY NAME                                
         MVC   D1CADLN1,SPACES                                                  
         MVC   D1CADLN2,SPACES                                                  
         MVC   D1CADLN3,SPACES                                                  
         MVC   D1CADLN1(33),CADDR1 MOVE IN ADDRESS FROM CMP TBLE                
         MVC   D1CADLN2(33),CADDR2                                              
         MVC   D1CADLN3(33),CADDR3                                              
*                                                                               
         LA    R0,3                DETERMINE NUMBER OF ADDR LINES               
         LA    R1,D1CADLN3         THIRD ADDR LINE                              
LOOK08   CLC   0(L'D1CADLN3,R1),SPACES                                          
         BNE   LOOK10                                                           
         SH    R1,=Y(L'D1CADLN3)                                                
         BCT   R0,LOOK08                                                        
         DC    H'0'                NO ADDRESS LINE                              
*                                                                               
LOOK10   STC   R0,D1CADNL          NUMBER OF ADDR LINES                         
         MVI   SCANSW,SCANCMP      VALIDATE COMPANY ADDRESS                     
         BAS   RE,FIXADDR                                                       
*                                                                               
         TM    DWNSTAT,DWNCMPY                                                  
         BO    LOOK12                                                           
         CLI   QOPT6,C'C'          IS COMPANY IT'S OWN TRANSMITTER              
         BE    LOOK12              YES                                          
         MVC   D1CTRAN,DDSTRANS    DEFAULT IS DDS AS THE TRANSMITTER            
         MVC   D1COMPCD,DDSDDN     DDS 99 FOR TAPE DDN                          
         B     LOOK14                                                           
*                                                                               
LOOK12   CLI   QOPT7,C'Y'           DOWNLOADING?                                
         BNE   LOOK13                                                           
         CLC   CTRANS,ZEROS         IS COMPANY IT'S OWN TRANSMITTER?            
         BNE   *+6                                                              
         DC    H'0'                                                             
LOOK13   MVC   D1CTRAN,CTRANS       AGENCY'S OWN TRANS NUMBER                   
         MVC   D1COMPCD,CALPHA      AGENCY ALPHA FOR TAPE DDN                   
*                                                                               
LOOK14   DS    0H                                                               
         MVC   SRECCID,CNUMBER      COMPANY'S EIN NUMBER                        
         CLI   QOPT5,C' '                                                       
         BNE   LOOK99                                                           
         MVC   D1COMPCD,CALPHA      IF COMP TAPE, THEN USE COMPANY CD           
*                                                                               
LOOK99   B     EXIT                                                             
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
         CLI   0(R5),C'A'                                                       
         BE    FAREC                                                            
         CLI   0(R5),C'C'                                                       
         BE    FCREC                                                            
         CLI   0(R5),C'F'                                                       
         BE    FEXIT                                                            
         B     FO10                                                             
*                                                                               
FAREC    DS    0H                                                               
         USING AREC,R5                                                          
         CLC   SVTRID,ATRID                                                     
         BE    FA100               IF SAME TRANSMITTER, NO NEW HEADING          
         MVC   SVTRID,ATRID                                                     
*                                                                               
         MVI   RCSUBPRG,3          FOR FORM 4804, DIFFERENT HEADING.            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         CLC   ALPHAID,=C'FC'                                                   
         BE    FA100                                                            
         MVI   P,C'-'                                                           
         MVC   P+1(80-1),P                                                      
         MVC   PSECOND(LMEDTYP),MEDTYP                                          
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
*  PRINT PAYER NAME AND ADDRESS FROM 'A' RECORD                                 
*                                                                               
FA100    MVC   P+8(40),ACOMPNM                                                  
         MVC   P+50(10),=C'PAYER EIN='                                          
         MVC   P+61(L'ACCOID),ACCOID                                            
         MVC   PSECOND+8(40),ACADD1                                             
         MVC   PTHIRD+8(40),ACADD2                                              
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
         EDIT  (C6,CBCOUNT),(6,P+33),COMMAS=YES,ALIGN=LEFT                      
*                                                                               
         ZAP   AMOUNT,=P'0'                                                     
         PACK  WORK(8),CAMT1(15)   RENT                                         
         AP    AMOUNT,WORK(8)                                                   
         PACK  WORK(8),CAMT3(15)   PRIZES                                       
         AP    AMOUNT,WORK(8)                                                   
         PACK  WORK(8),CAMT6(15)   MEDICAL                                      
         AP    AMOUNT,WORK(8)                                                   
         PACK  WORK(8),CAMT7(15)   NON EMPLOYEE COMPENSATION                    
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
         DC    A(CPYTAB)                                                        
         DC    A(BUF1099)                                                       
         DC    A(EXTTBL)                                                        
         DC    A(STARS)                                                         
         DC    A(INVTBL)                                                        
*                                                                               
DDSDDN   DC    C'99'                                                            
DDSTRANS DC    C'19868'   ----***  DDS'S TRANSMITTER NO.  ***--                 
FCDDN    DC    C'FC'                                                            
FCTRANS  DC    C'99999'                                                         
ZEROS    DC    C'000000000000000'                                               
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
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=420'                                   
*                                                                               
ACT99TP  DCB   DSORG=PS,MACRF=PM,DDNAME=ACT99TP,RECFM=F,LRECL=420,     X        
               BLKSIZE=420                                                      
*                                                                               
ACT99WK  DCB   DSORG=PS,MACRF=(PM,GM),DDNAME=ACT99WK,RECFM=F,          X        
               LRECL=600,BLKSIZE=600                                            
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
         DC    36C'*'                                                           
         DC    X'03'               3 ADDR LNS                                   
         DC    120C'*'                                                          
         DC    2C'*'                                                            
         DC    5C'9'                                                            
         DC    2C'*'                                                            
         DC    12C'*'                                                           
         DC    36C'*'                                                           
         DC    X'04'               4 ADDR LNS                                   
         DC    160C'*'                                                          
         DC    PL6'999999'                                                      
         DC    C'*'                                                             
         DC    20C' '                                                           
         EJECT                                                                  
**********************************************************************          
* ADDRESS EXCEPTION TABLE                                            *          
**********************************************************************          
         SPACE 1                                                                
EXTTBL   DC    AL1(3),C'C/O'                                                    
         DC    AL1(4),C'ATTN'                                                   
         DC    X'FF'                                                            
*                                                                               
TRNTAB   DS    0CL2                AGENCIES READING TRANSACTIONS                
         DC    C'BC'                                                            
         DC    C'BD'                                                            
         DC    C'BP'                                                            
         DC    C'BS'                                                            
         DC    C'DE'                                                            
         DC    C'DM'                                                            
         DC    C'DW'                                                            
         DC    C'MZ'                                                            
         DC    C'NW'                                                            
         DC    C'OA'                                                            
         DC    C'OM'                                                            
         DC    C'JW'                                                            
         DC    C'LC'                                                            
         DC    C'QJ'                                                            
         DC    C'SU'                                                            
TRNTBLNQ EQU   (*-TRNTAB)/L'TRNTAB    # OF ENTRIES IN TABLE                     
         EJECT                                                                  
**********************************************************************          
* COMPANY TABLE                                                      *          
**********************************************************************          
         SPACE 1                                                                
CPYTAB   DS    0F                                                               
*                                                                               
* AL - ANDERSON LEMBKE                                                          
*                                                                               
         DC    C'AL'               ALNY                                         
         DC    AL2(3735)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'061340016'                                                     
         DC    X'00'                                                            
         DC    CL33'ANDERSON AND LEMBKE, INC.'     COMPANY NAME                 
         DC    CL33'320 WEST 13TH STREET'                                       
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'AL'               ALSF                                         
         DC    AL2(3737)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'061340016'                                                     
         DC    X'00'                                                            
         DC    CL33'ANDERSON AND LEMBKE, INC.'     COMPANY NAME                 
         DC    CL33'320 WEST 13TH STREET'                                       
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* AV - THE ADVERTISING COUNCIL, INC.                                            
*                                                                               
         DC    C'AV'               THE ADVERTISING COUNSEL                      
         DC    AL2(2111)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'130417693'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* BB - BBDO                                                                     
*                                                                               
         DC    C'BB'               BBDO CHICAGO -BBCH                           
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'223045170'                                                     
         DC    X'00'                                                            
         DC    CL33'BBDO CHICAGO, INC.'                                         
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* BC - RAPP COLLINS MARCOA                                                      
*                                                                               
         DC    C'BC'               MDBO - RAPP, COLLINS, MARCOA                 
         DC    AL2(1795)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'112679168'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'BC'               MDCH - RAPP, COLLINS, MARCOA                 
         DC    AL2(2105)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'362777652'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'BC'               MDNY - RAPP, COLLINS, MARCOA                 
         DC    AL2(1818)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'112679168'                                                     
         DC    X'00'                                                            
         DC    CL33'RAPP COLLINS WORLDWIDE'                                     
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'BC'               MDCP - RAPP, COLLINS, MARCOA                 
         DC    AL2(2413)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'112679168'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'BC'               MDMIN- RAPP, COLLINS, MARCOA                 
         DC    AL2(2989)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'362951128'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* BD - BDDO                                                                     
*                                                                               
         DC    C'BD'               KENT                                         
         DC    AL2(1110)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'35D71'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'130473120'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL33'1285 AVE OF AMERICAS'      3 X 33 ADDR LINES                
         DC    CL33'NEW YORK, NY 10019'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'BD'               BDDE DETROIT                                 
         DC    AL2(0163)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'35D71'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'382964249'                                                     
         DC    X'01'                                                            
         DC    CL33'BBDO DETROIT'              COMPANY NAME                     
         DC    CL33'26261 EVERGREEN ROAD'      3 X 33 ADDR LINES                
         DC    CL33'SUITE 300'                                                  
         DC    CL33'SOUTHFIELD, MI  48076'                                      
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* BP - BBDO SOUTH                                                               
*                                                                               
         DC    C'BP'               BDA/BBDO -BDAT                               
         DC    AL2(1023)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'30870'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'581597648'                                                     
         DC    X'01'                                                            
         DC    CL33'BBDO ATLANTA, INC.'                                         
         DC    CL33'3414 PEACHTREE RD, SUITE 1600'                              
         DC    CL33'ATLANTA, GA 30326'                                          
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* BS - BATES USA                                                                
*                                                                               
         DC    C'BS'               BSNYG - BACKER SPEILVOGEL BATES INC.         
         DC    AL2(1566)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19N35'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132993871'                                                     
         DC    X'01'                                                            
         DC    CL33'BATES ADVERTISING USA, INC.'    COMPANY NAME                
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'BS'               BSNYGA-BACKER SPEILVOGEL BATES INC.          
         DC    AL2(3665)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19N35'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132545952'                                                     
         DC    X'01'                                                            
         DC    CL33'BATES ADVERTISING HOLDINGS, INC.'  COMPANY NAME             
         DC    CL33'405 LEXINGTON AVENUE'        3 X 33 ADDR LINES              
         DC    CL33'NEW YORK, NY  10174'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* BW - BBDO INC.                                                                
*                                                                               
         DC    C'BW'               BBDO L.A. -BWLA                              
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'953308425'                                                     
         DC    X'00'                                                            
         DC    CL33'BBDO LOS ANGELES'                                           
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* BWCNY                                                                         
*                                                                               
         DC    C'BG'               BWCNY - (3/95)                               
         DC    AL2(3282)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133767936'                                                     
         DC    X'00'                                                            
         DC    CL33'BERLIN CAMERON DOYLE'                                       
         DC    CL33'79 FIFTH AVENUE'       3 X 33 ADDR LINES                    
         DC    CL33'NEW YORK, NY  10003'                                        
         DC    CL33'ATTN: DIANE FELDMAN'                                        
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* CE - CAMPBELL MITHUN - ESTY                                                   
*                                                                               
         DC    C'CE'               CASH PLUS, INC. - CECP                       
         DC    AL2(2072)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'08K07'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'411267291'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'CE'               CAMPBELL-MITHUN ESTY CEND                    
         DC    AL2(2084)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'08K07'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'410985665'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'CE'               PROMOTION WORKS, INC. - CEPW                 
         DC    AL2(2086)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'411333350'                                                     
         DC    X'00'                                                            
         DC    CL33'CME PROMOTION MARKETING'                                    
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'CE'               CECME                                        
         DC    AL2(2869)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'08K07'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'410985665'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'CE'               CEHO                                         
         DC    AL2(2818)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'741282588'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'CE'               CEMNRE                                       
         DC    AL2(3100)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'08K07'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'410985665'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'CE'               CEIPG                                        
         DC    AL2(4052)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'08K07'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'411807452'                                                     
         DC    X'01'                                                            
         DC    CL33'CAMPBELL-MITHUN-ESTY, L.L.C.'                               
         DC    CL33'222 SOUTH 9TH STREET'                                       
         DC    CL33'MINNEAPOLIS, MN  55402'                                     
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* DB - D'ARCY MASIUS BENTON AND BOWLES, INC.                                    
*                                                                               
         DC    C'DB'               DDSB                                         
         DC    AL2(2635)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'123456789'        EIN                                          
         DC    X'01'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* DE - DELLA FEMINA MCNAMEE, INC.                                               
*                                                                               
         DC    C'DE'               DELLA FEMINA   DENYA                         
         DC    AL2(1737)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132590823'        EIN                                          
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DE'               DELLA FEMINA   DEWNY                         
         DC    AL2(2309)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133639857'        EIN                                          
         DC    X'00'                                                            
         DC    CL33'EUROCOM ADVERTISING N.A., INC.'                             
         DC    CL33'350 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DE'               DELLA FEMINA   DEBO                          
         DC    AL2(2139)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133597470'        EIN                                          
         DC    X'00'                                                            
         DC    CL33'LAWNER REINGOLD BRITTON && PTRS'                            
         DC    CL33'C/O DELLA FEMINA MCNAMEE, INC'                              
         DC    CL33'350 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DE'               DELLA FEMINA   DENYC                         
         DC    AL2(2380)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132590823'        EIN                                          
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL99' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DE'               DELLA FEMINA   DEDBO                         
         DC    AL2(2172)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133502994'        EIN                                          
         DC    X'00'                                                            
         DC    CL33'ORSATTI && PARRISH, INC.'                                   
         DC    CL33'C/O DELLA FEMINA MCNAMEE, INC'                              
         DC    CL33'350 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014'                                         
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* DM - DOREMUS AND COMPANY                                                      
*                                                                               
         DC    C'DM'               DOREMUS & COMPANY  DMNYO                     
         DC    AL2(1440)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'135033900'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DM'               DOREMUS & COMPANY  DMNYA                     
         DC    AL2(3023)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'35E66'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133712950'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DM'               DOREMUS & COMPANY  DMNYD                     
         DC    AL2(3148)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'35E65'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'135033900'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* DS - TALENT PARTNERS                                                          
*                                                                               
         DC    C'DS'               TALENT PARTNERS - TPCH                       
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'363800090'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* DW - SAATCHI AND SAATCHI NORTH AMERICA INC.                                   
*                                                                               
         DC    C'DW'               SAATCHI SAATCHI DFS, INC. - DWNYC            
         DC    AL2(1659)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'35D73'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133341336'        EIN                                          
         DC    X'01'                                                            
         DC    CL33'SAATCHI && SAATCHI N.A., INC.'                              
         DC    CL33'375 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10014-3620'                                    
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '                                                         
*                                                                               
         DC    C'DW'               DWCN                                         
         DC    AL2(2533)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133198075'        EIN                                          
         DC    X'00'                                                            
         DC    CL33'CONILL ADVERTISING, INC.'                                   
         DC    CL33'375 HUDSON STREET'                                          
         DC    CL33'NEW YORK, NY 10956'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DW'               CONILL ADVERTISING - DWNYN                   
         DC    AL2(2534)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133198075'        EIN                                          
         DC    X'00'                                                            
         DC    CL33'CONILL ADVERTISING, INC.'                                   
         DC    CL33'100 AVENUE OF THE AMERICAS'                                 
         DC    CL33'NEW YORK, NY 10013'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DW'               DWKLC                                        
         DC    AL2(2783)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132597353'        EIN                                          
         DC    X'00'                                                            
         DC    CL33'COMTACT CORPORATION'                                        
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DW'               DWKLT                                        
         DC    AL2(2978)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132975889'        EIN                                          
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'DW'               DWVL                                         
         DC    AL2(3595)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133198075'        EIN                                          
         DC    X'00'                                                            
         DC    CL33'VISION LATINA COMMUNICATIONS'                               
         DC    CL33'375 HUDSON STREET, 8TH FLOOR'                               
         DC    CL33'NEW YORK, NY  10014-3660'                                   
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* DY - DYNAMIC PRODUCTIONS INC.                                                 
*                                                                               
         DC    C'DY'               DYTO - SAFFER                                
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'980123040'        EIN                                          
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '                                                         
*                                                                               
* FB - BBDO                                                                     
*                                                                               
         DC    C'FB'               TFB/BBDO, INC. - TFBPA                       
         DC    AL2(2793)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'942276267'                                                     
         DC    X'00'                                                            
         DC    CL33'TFB/BBDO, INC'                                              
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* FM - SFM MEDIA CORPORATION                                                    
*                                                                               
         DC    C'FM'               FMNY - SFM MEDIA CORPORATION                 
         DC    AL2(0261)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132643170'                                                     
         DC    X'00'                                                            
         DC    CL33' '                                                          
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* GB - GRIFFIN BACAL, INC.                                                      
*                                                                               
         DC    C'GB'               GRIFFIN BACALL                               
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132941459'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* GE - HMS/RUHR, LTD.                                                           
*                                                                               
         DC    C'GE'               GENY - GEER DUBOIS INC.                      
         DC    AL2(2957)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'131555974'                                                     
         DC    X'00'                                                            
         DC    CL33'GEER DUBOIS INC.'                                           
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* GV - GAVIN ANDERSON AND COMPANY                                               
*                                                                               
         DC    C'GV'               GAVIN - DMGA                                 
         DC    AL2(2340)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133083151'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* HC - HILL HOLLIDAY CONNORS COSMOPULOS                                         
*                                                                               
         DC    C'HC'               HILL HOLIDAY HCLA                            
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'042437626'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* HH - HILL HOLLIDAY                                                            
*                                                                               
         DC    C'HH'               HILL HOLIDAY                                 
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'042437626'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* HK - HILL AND KNOWLTON, INC.                                                  
*                                                                               
         DC    C'HK'               HILL & KNOWLTON - HKNY                       
         DC    AL2(1702)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'35D93'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133016062'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* HL - HILL AND KNOWLTON, INC.                                                  
*                                                                               
         DC    C'HL'               HLTOC                                        
         DC    AL2(2521)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'000000000'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* HR - HAL RINEY AND PARTNERS                                                   
*                                                                               
         DC    C'HR'               HAL RINEY & PARTNERS (87)                    
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'39482'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'942973839'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* JW - J. WALTER THOMPSON                                                       
*                                                                               
         DC    C'JW'               J WALTER JWNYA - CHANGED FROM JWNYME         
         DC    AL2(0460)           ORIGIN (DEFAULT IS PRINC ID)   12/95         
         DC    C'19984'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133016052'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* KP - KALLIR, PHILIPS, ROSS INC.                                               
*                                                                               
         DC    C'KP'               KALLIR, PHILIPS ROSS INC                     
         DC    AL2(2061)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133169392'                                                     
         DC    X'00'                                                            
         DC    CL33'KALLIR, PHILIPS, ROSS, INC.'                                
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* LA - LARSON, COLBY                                                            
*                                                                               
         DC    C'LA'               LARSON, COLBY - LALA                         
         DC    AL2(1558)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'942597272'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* LC - GOTHAM                                                                   
*                                                                               
         DC    C'LC'               LAURENCE,CHARLES,FREE & LAWSON LCNYA         
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133397664'                                                     
         DC    X'00'                                                            
         DC    CL33'GOTHAM, INC.'              COMPANY NAME                     
         DC    CL33'260 MADISON AVENUE'        3 X 33 ADDR LINES                
         DC    CL33'NEW YORK, NY 10016'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* LM - LOWE AND PARTNERS/SMS                                                    
*                                                                               
         DC    C'LM'               LOWE - LMNYA                                 
         DC    AL2(2365)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'131550675'                                                     
         DC    X'00'                                                            
         DC    CL33'LOWE && PARTNERS/SMS'           COMPANY NAME                
         DC    CL33'1114 AVENUE OF THE AMERICAS'    ADDRESS                     
         DC    CL33'NEW YORK, NY  10036'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* LP - LOWE DIRECT                                                              
*                                                                               
         DC    C'LP'               LOWE - LPNYD                                 
         DC    AL2(2482)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'131550675'                                                     
         DC    X'00'                                                            
         DC    CL33'LOWE && PARTNERS, INC.'                                     
         DC    CL33'(LOWE DIRECT, DIVISION)'                                    
         DC    CL33'1114 AVENUE OF THE AMERICAS'                                
         DC    CL33'NEW YORK, NY 10036'                                         
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* MC - MCCANN-ERICKSON, INC                                                     
*                                                                               
         DC    C'MC'               MCANN ERICKSON                               
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19A05'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'131938691'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* MK - MOXON DOLPHIN KERBY                                                      
*                                                                               
         DC    C'MK'               MOXON DOLPHIN KERBY ADVERTISING              
         DC    AL2(1911)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'521593746'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* MP - THE MINGO GROUP                                                          
*                                                                               
         DC    C'MP'               THE MINGO GROUP - TMGT                       
         DC    AL2(2979)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132887300'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* MZ - MEZZINA/BROWN INC.                                                       
*                                                                               
         DC    C'MZ'               MZNY - MEZZINA BROWN                         
         DC    AL2(2736)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'113077473'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'MZ'               MZIN - MEZZINA BROWN                         
         DC    AL2(3366)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133753385'                                                     
         DC    X'00'                                                            
         DC    CL33'M/B INTERACTIVE INC.'      COMPANY NAME                     
         DC    CL33'401 PARK AVE. SOUTH - 7TH FL'    ADDRESS                    
         DC    CL33'NEW YORK, NY 10016'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* NC - ROTANDO PARTNERS, INC.                                                   
*                                                                               
         DC    C'NC'               NCNY - NORTH CASTLE (86)                     
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'061190424'                                                     
         DC    X'00'                                                            
         DC    CL33'NORTH CASTLE COMMUNICATIONS, INC.'                          
         DC    CL33'300 FIRST STAMFORD PLACE'                                   
         DC    CL33'STAMFORD, CT 06902'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* NE - DDB NEEDHAM WORLDWIDE INC.                                               
*                                                                               
         DC    C'NE'               DDB NEEDHAM WORLDWIDE                        
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19E33'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133355855'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* NR - DDBN RETAIL                                                              
*                                                                               
         DC    C'NR'               DDB NEEDHAM - RETAIL                         
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19E33'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133355855'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* NW - NW AYER INC                                                              
*                                                                               
         DC    C'NW'               N.W.AYER - AYNYR                             
         DC    AL2(2628)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19M97'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'230376910'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'NW'               N.W.AYER - AYDR                              
         DC    AL2(3127)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19M97'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133710113'                                                     
         DC    X'01'                                                            
         DC    CL33'DIRECT PRO, INC.'                                           
         DC    CL33'825 EIGHTH AVENUE'                                          
         DC    CL33'NY, NY  10019'                                              
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'NW'               N.W.AYER - AYWD                              
         DC    AL2(3140)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19M97'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133710114'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'NW'               N.W.AYER - AYME                              
         DC    AL2(3470)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19579'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133764500'                                                     
         DC    X'01'                                                            
         DC    CL33'MEDIA EDGE, INC.'           COMPANY NAME                    
         DC    CL33'825 EIGTH AVENUE'           COMPANY ADDRESS                 
         DC    CL33'NEW YORK, NY  10019'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'NW'               N.W.AYER - AYBM                              
         DC    AL2(3835)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19579'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133764500'                                                     
         DC    X'01'                                                            
         DC    CL33'BLUE MARBLE ACG, LTD.'      COMPANY NAME                    
         DC    CL33'825 EIGTH AVENUE'           COMPANY ADDRESS                 
         DC    CL33'NEW YORK, NY  10019'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* NV - DDS                                                                      
*                                                                               
         DC    C'NV'               NVNY                                         
         DC    AL2(2886)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133669079'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* OA - OGILVY AND MATHER                                                        
*                                                                               
         DC    C'OA'               OGILVY MATHER - OACO                         
         DC    AL2(2174)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19939'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132555496'                                                     
         DC    X'01'                                                            
         DC    CL33'OGILVY && MATHER'                                           
         DC    CL33'309 WEST 49TH STREET-4TH FLOOR'                             
         DC    CL33'NEW YORK, NY 10019-7399'                                    
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST                                  
*                                                                               
         DC    C'OA'               OGILVY MATHER - OAEY                         
         DC    AL2(2801)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19939'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132867395'                                                     
         DC    X'01'                                                            
         DC    CL33'EYEPATCH PRODUCTIONS, INC.'                                 
         DC    CL33'309 WEST 49TH STREET'                                       
         DC    CL33'NEW YORK, NY 10019-7399'                                    
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'OA'               OGILVY MATHER - OANYS                        
         DC    AL2(3319)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19939'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132677706'                                                     
         DC    X'01'                                                            
         DC    CL33'GRAPHIC SERVICES, INC.'                                     
         DC    CL33'309 WEST 49TH STREET'                                       
         DC    CL33'NEW YORK, NY 10019-7399'                                    
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'OA'               OGILVY MATHER - OACOTT                       
         DC    AL2(3778)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19939'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132555496'                                                     
         DC    X'01'                                                            
         DC    CL33'THE OGILVY GROUP, INC.'                                     
         DC    CL33'309 WEST 49TH STREET'                                       
         DC    CL33'NEW YORK, NY 10019-7399'                                    
         DC    CL33' '                                                          
         DC    CL100' '           OFFICE LIST                                   
*                                                                               
         DC    C'OA'               OGILVY MATHER - OALAS                        
         DC    AL2(4057)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19939'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'953938806'                                                     
         DC    X'01'                                                            
         DC    CL33'SILVER BULLET PRODUCTIONS'                                  
         DC    CL33'11766 WILSHIRE BLVD - 9TH FL.'                              
         DC    CL33'LOS ANGELES, CA 90025'                                      
         DC    CL33' '                                                          
         DC    CL100' '           OFFICE LIST                                   
*                                                                               
* OD - OGILVY AND MATHER DIRECT                                                 
*                                                                               
         DC    C'OD'               OGILVY MATHER (DIRECT RESPONSE)              
         DC    AL2(2175)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19939'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132877533'                                                     
         DC    X'01'                                                            
         DC    CL33'OGILVY && MATHER DIRECT RESPONSE'                           
         DC    CL33'309 WEST 49TH STREET-15TH FLOOR'                            
         DC    CL33'NEW YORK, NY 10019-7399'                                    
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'OD'               OGILVY MATHER ODRNY                          
         DC    AL2(2304)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19939'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132877533'                                                     
         DC    X'01'                                                            
         DC    CL33'OGILVY && MATHER DIRECT RESPONSE'                           
         DC    CL33'309 WEST 49TH STREET'                                       
         DC    CL33'NEW YORK, NY 10019'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* ON - BBDO                                                                     
*                                                                               
         DC    C'ON'               OMNICOM(87)                                  
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'131514814'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* PN - PORTER NOVELLI INC.                                                      
*                                                                               
         DC    C'PN'               PORTER NOVELLI- NY (DOREMUS) PNNY            
         DC    AL2(1513)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'35D68'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133461427'                                                     
         DC    X'01'                                                            
         DC    CL33' '                                                          
         DC    CL33' '                       3 X 33 ADDR LINES                  
         DC    CL33' '                                                          
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* QJ - INGALLS, QUINN AND JOHNSON                                               
*                                                                               
         DC    C'QJ'               INGALLS, QUINN & JOHNSON                     
         DC    AL2(0656)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'31G60'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'043041769'                                                     
         DC    X'01'                                                            
         DC    CL33'INGALLS,QUINN && JOHNSON,INC.'                              
         DC    CL33'855 BOYLSTON STREET'                                        
         DC    CL33'BOSTON, MA 02116'                                           
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* RB - RJ PALMER MEDIA, INC.                                                    
*                                                                               
         DC    C'RB'               RJNY                                         
         DC    AL2(3479)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133071688'                                                     
         DC    X'00'                                                            
         DC    CL33'RJ PALMER, INC.'                                            
         DC    CL33'156 WEST 56TH STREET SUITE 1801'                            
         DC    CL33'NEW YORK, NY  10019'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* RP - RUBIN POSTAER AND ASSOCIATES                                             
*                                                                               
         DC    C'RP'               RUBIN POSTAER ASSOCIATES                     
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'954062405'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* RU - RUHR/PARAGON INC                                                         
*                                                                               
         DC    C'RU'               RUMN                                         
         DC    AL2(2926)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'32P33'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'311441282'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* SC - LOWE AND PARTNERS/SMS                                                    
*                                                                               
         DC    C'SC'               SCALI (86)                                   
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'132882093'                                                     
         DC    X'00'                                                            
         DC    CL33'SCALI, MCCABE, SLOVES INC.'                                 
         DC    CL33'1345 AVENUE OF THE AMERICAS'                                
         DC    CL33'NEW YORK, NY  10105'                                        
         DC    CL33' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* SG - LOWE AND PARTNERS/SMS                                                    
*                                                                               
         DC    C'SG'               MARIA                                        
         DC    AL2(2858)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'540789050'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* SH - SAFFER ADVERTISING                                                       
*                                                                               
         DC    C'SH'               SACH                                         
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'363216533'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* SJ - DDS                                                                      
*                                                                               
         DC    C'SJ'               SJR TEST                                     
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'77777'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'99SJRTEST'                                                     
         DC    X'01'               DOWNLOADING                                  
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* SP - SAATCHI AND SAATCHI ADVERTISING                                          
*                                                                               
         DC    C'SP'               SAATCHI && SAATCHI PROMOTIONS                
         DC    AL2(1730)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133451963'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* ST - SGTSEC                                                                   
*                                                                               
         DC    C'ST'               SGTO - SAFFER                                
         DC    AL2(3223)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'980123040'        EIN                                          
         DC    X'00'                                                            
         DC    CL33'SAFFER GROUP, INC.'        COMPANY NAME                     
         DC    CL33'156 DUNCAN MILLS RD. UNIT 1'                                
         DC    CL33'DON MILLS, ONTARIO  M3B 3N2'                                
         DC    CL33' '                                                          
         DC    CL100' '                                                         
*                                                                               
* SU - SUSEC - DAVID CRAVIT AND ASSOCIATES, LTD                                 
*                                                                               
         DC    C'SU'               SAFFER - SUCH                                
         DC    AL2(2842)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'363863346'                                                     
         DC    X'00'                                                            
         DC    CL33'DAVID CRAVIT AND ASSOCIATES, LTD'                           
         DC    CL33'737 NORTH MICHIGAN AVE.  16TH FL'                           
         DC    CL33'CHICAGO, IL  60611'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
         DC    C'SU'               SAFFER - SUTO                                
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'980123040'                                                     
         DC    X'00'                                                            
         DC    CL33'SAFFER COMMUNICATIONS USA LTD'                              
         DC    CL33'54 WEST HUBBARD ST., SUITE 600'                             
         DC    CL33'CHICAGO, IL  60610'                                         
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* TH - ZENITH                                                                   
*                                                                               
         DC    C'TH'               ZENY                                         
         DC    AL2(3816)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133736241'                                                     
         DC    X'00'                                                            
         DC    CL33'ZENITH MEDIA SERVICES'       COMPANY NAME                   
         DC    CL33'299 WEST HOUSTON STREET'     3 X 33 ADDR LINES              
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* WL - ??????????????????                                                       
*                                                                               
         DC    C'WL'               WARING & LAROSA (86)                         
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'00000'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133050248'                                                     
         DC    X'00'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* WD - WIEDEN AND KENNEDY                                                       
*                                                                               
         DC    C'WD'               WIEDEN & KENNEDY                             
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'29870'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'930832855'                                                     
         DC    X'01'                                                            
         DC    CL33' '             COMPANY NAME                                 
         DC    CL99' '             3 X 33 ADDR LINES                            
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* WW - WUNDERMAN WORLDWIDE INC.                                                 
*                                                                               
         DC    C'WW'               WUNDERMAN WORLDWIDE INC.                     
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19728'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133493149'                                                     
         DC    X'01'                                                            
         DC    CL33'YOUNG&&RUBICAM,L.P. DBA WUNDERMAN'                          
         DC    CL33'675 AVENUE OF AMERICAS'                                     
         DC    CL33'NEW YORK, NY  10010'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* YE - CHAPMAN DIRECT ADVERTISING                                               
*                                                                               
         DC    C'YE'               YCHNY - CHAPMAN DIRECT ADVERTISING           
         DC    AL2(3698)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19728'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133493149'                                                     
         DC    X'01'                                                            
         DC    CL33'Y && R LP DBA CHAPMAN'                                      
         DC    CL33'230 PARK AVENUE SOUTH - 6TH FLOOR'                          
         DC    CL33'NEW YORK, NY  10003'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* YF - BURSON MARSTELLER                                                        
*                                                                               
         DC    C'YF'               YBMSFS - BURSON                              
         DC    AL2(3354)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19728'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133493149'                                                     
         DC    X'01'                                                            
         DC    CL33'Y && R,LP DBA BURSON MARSTELLER'                            
         DC    CL33'230 PARK AVENUE SOUTH'                                      
         DC    CL33'NEW YORK, NY  10003'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* YN - BM/YR/SFS                                                                
*                                                                               
         DC    C'YN'               YOUNG & RUBICAM INC.INC.                     
         DC    AL2(0000)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19728'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133493149'                                                     
         DC    X'01'                                                            
         DC    CL33'YOUNG && RUBICAM LP'                                        
         DC    CL33'285 MADISON AVENUE'                                         
         DC    CL33'NEW YORK, NY  10017'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
*                                                                               
* YP - SUDLER AND HENNESEY/SFS                                                  
*                                                                               
         DC    C'YP'               YSHNY - SUDLER AND HENNESEY/SFS              
         DC    AL2(3655)           ORIGIN (DEFAULT IS PRINC ID)                 
         DC    C'19728'            TRANSMITTER NUM(00000 USE DDS NUMB)          
         DC    C'133493149'                                                     
         DC    X'01'                                                            
         DC    CL33'Y && R LP DBA SUDLER && HENNESSY'                           
         DC    CL33'230 PARK AVENUE SOUTH - 6TH FLOOR'                          
         DC    CL33'NEW YORK, NY  10014'                                        
         DC    CL33' '                                                          
         DC    CL100' '            OFFICE LIST (SPACES - NO LIST)               
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
**********************************************************************          
* PRNTABLE                                                           *          
*    PARM1 = RC                                                      *          
*    PARM2 = LITERAL                                                 *          
*    PARM3 = ADDRESS OF WHAT TO PRINT                                *          
*    PARM4 = LENGTH                                                  *          
**********************************************************************          
         SPACE 1                                                                
PRNTIT   DS    0H                                                               
         NMOD1 0,*PRN*                                                          
         L     RC,0(R1)            RESTORE RC                                   
         L     R2,4(R1)            MOVE IN LITERAL                              
         L     R3,8(R1)            MOVE IN ADDRESS                              
         L     R4,12(R1)           MOVE IN LENGTH                               
*                                                                               
         CLI   QOPT8,C'Y'          DO WE WANT TO DO A PRINTABLE                 
         BNE   PRNTITX             NO - EXIT                                    
         GOTO1 PRNTBL,DMCB,(R2),(R3),C'DUMP',(R4),=C'2D'                        
PRNTITX  XIT1                                                                   
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
         CLI   QOPT4,C'G'                                                       
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
         LA    R3,5                                                             
DWNL110  MVC   DLCBFLX,SPACES                                                   
         MVC   DLCBFLX(84),0(R5)                                                
         MVI   DLCBLEN,84          LENGTH                                       
         MVI   DLCBACT,DLCBPUT     PUT                                          
         MVI   DLCBTYP,DLCBTXT     TEXT                                         
         OI    DLCBFLG1,DLCBFXFL   USING EXTENDED FIELD                         
         GOTO1 DLFLD,DLCBD                                                      
         LA    R5,84(R5)                                                        
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
ACPYTAB  DS    A                                                                
ABUF1099 DS    A                                                                
AEXTTBL  DS    A                                                                
ASTARS   DS    A                                                                
AINVTBL  DS    A                                                                
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
BTOT#    EQU   (*-BTOTALS)/L'BTOTALS                                            
*                                                                               
COTOTAL  DS    PL6                 COMP TOTAL FOR TOTAL PAGE                    
BRECOUNT DS    PL8                                                              
INVCNT   DS    PL6                 # OF INVALID ADDRESSES IN INVTBL             
COMMAND  DS    CL6                                                              
CLGR     DS    CL1                 CURRENT LGR INDICATOR                        
SRECA    DS    CL420                                                            
SVSREC   DS    CL420               SAVE SRT REC FOR FORMS.                      
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
WKREC    DS    CL600               SPACE TO BUILD IRS RECORDS.                  
CTFILIO  DS    CL2048              IO AREA FOR CONTROL FILE READ                
*                                                                               
SVTRID   DS    CL5                 FOR USE BY FORM4804                          
         DS    CL1                 UNUSED                                       
WORK2    DS    CL120               FOR ADDRESS CRUNCHING                        
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
BF4      DS    0CL132              **** LINE #5 ****                            
         DS    CL5                                                              
BFCMPAD1 DS    CL33                (1ST LINE OF COMPANY ADDRESS)                
         ORG BF4+132                                                            
*                                                                               
BF5      DS    0CL132              **** LINE #6 ****                            
         DS    CL5                                                              
BFCMPAD2 DS    CL33                (2ND LINE OF COMPANY ADDRESS)                
         ORG BF5+132                                                            
*                                                                               
BF6      DS    0CL132              **** LINE #6 ****                            
         DS    CL5                                                              
BFCMPAD3 DS    CL33                (3RD LINE OF COMPANY ADDRESS)                
         ORG BF6+132                                                            
*                                                                               
BF7      DS    0CL132              **** LINE #7 ****                            
         DS    CL40                                                             
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
*               'A' RECORD DSECT                                      *         
***********************************************************************         
*                                                                               
ARECD    DSECT                                                                  
AREC     DS    0CL420              PAYER RECORD                                 
         DS    CL1                 RECORD TYPE                                  
AYEAR    DS    CL2                 LAST 2 DIGITS OF YEAR.                       
AREELSEQ DS    CL3                 REEL SEQ NUMBER                              
ACCOID   DS    CL9                 EIN  BLANK ONLY FOR FOREIGN CORP             
         DS    CL4                 PAYER NAME CONTROL - BLANK                   
         DS    CL1                 BLANK                                        
         DS    CL1                 COMBINED FED/STATE FILER - BLANK             
AFRMTYP  DS    CL1                 FORM TYPE (CONSTANT) 1099-MISC               
         DS    CL9                 AMOUNT CODES (CONSTANT)                      
ACORRECT DS    CL1                 TEST/CORRECTION INDICATOR                    
ASERVICE DS    CL1                 SERVICE BUREAU INDICATOR                     
         DS    CL8                 BLANK                                        
AMTFIND  DS    CL2                 MAGNETIC TAPE FILER INDICATOR                
ATRID    DS    CL5                 TRANSMITTER CODE                             
         DS    CL1                 FOREIGN CORP INDICATOR                       
ACOMPNM  DS    CL40                COMPANY NAME                                 
         DS    CL40                                                             
AZER     DS    CL1                 INDICATES 'NOT' A TRANSFER AGENT             
ACADD1   DS    CL40                STREET ADDRESS                               
ACADD2   DS    CL40                CITY, STATE, ZIP.                            
ATRANNM  DS    CL80                TRANSMITTER NAME                             
ATADDR1  DS    CL40                            STREET ADDRESS                   
ATADDR2  DS    CL40                            CITY, STATE, ZIP                 
         DS    CL50                BLANK                                        
ALENQ    EQU   (*-AREC)                                                         
         EJECT                                                                  
***********************************************************************         
*               'B' RECORD DSECT                                      *         
***********************************************************************         
*                                                                               
BRECD    DSECT                                                                  
BREC     DS    0CL420              PAYEE RECORD                                 
         DS    CL1                 RECORD TYPE                                  
BYEAR    DS    CL2                 LAST 2 DIGITS OF YEAR                        
         DS    CL2                 DOCUMENT SPECIFIC CODE - BLANK               
         DS    CL1                 BLANK                                        
BCORR    DS    CL1                 HAS A 'G' IF A CORRECTED RETURN.             
BNMCTL   DS    CL4                 NAME CONTROL (1ST 4 LETTERS SURNAME)         
         DS    CL2                 BLANKS                                       
BTINTYPE DS    CL1                 EIN OR SSN (1 OR 2)                          
BSSNO    DS    CL9                 ID NUMBER TIN                                
BACCT    DS    CL20                DDS ACCOUNT NUMBER                           
         DS    CL7                 BLANKS                                       
BAMT1    DS    CL10                                                             
BAMT2    DS    CL10                                                             
BAMT3    DS    CL10                                                             
BAMT4    DS    CL10                                                             
BAMT5    DS    CL10                                                             
BAMT6    DS    CL10                                                             
BAMT7    DS    CL10                DOLLAR AMOUNT OF AMT 7                       
BAMT8    DS    CL10                                                             
BAMT9    DS    CL10                DOLLAR AMOUNT OF AMT 9                       
         DS    CL20                BLANKS                                       
BFORGN   DS    CL1                 FOREIGNCOUNTRY INDICATOR                     
BNAME    DS    CL40                PAYEE NAME                                   
BNAME2   DS    CL40                                                             
BADDR    DS    CL40                STREET ADDRESS                               
BCSZ     DS    CL40                CITY, STATE, ZIP 29-2-9                      
         DS    CL28                BLANKS                                       
         DS    CL67                BLANKS                                       
         DS    CL2                 BLANKS                                       
         DS    CL2                 BLANKS                                       
BLENQ    EQU   (*-BREC)                                                         
BSRCLIST DS    CL180               SOURCE LIST  (LENGTH/ACCT/AMOUNT)            
         EJECT                                                                  
***********************************************************************         
*               'C' RECORD DSECT                                      *         
***********************************************************************         
*                                                                               
CRECD    DSECT                                                                  
CREC     DS    0CL420              PAYER SUMMARY RECORD                         
         DS    CL1                 RECORD TYPE                                  
CBCOUNT  DS    CL6                 NUMBER OF B RECS FOR THIS COMPANY            
         DS    CL3                 BLANK                                        
CAMT1    DS    CL15                                                             
CAMT2    DS    CL15                                                             
CAMT3    DS    CL15                PRIZES/AWARDS                                
CAMT4    DS    CL15                                                             
CAMT5    DS    CL15                                                             
CAMT6    DS    CL15                                                             
CAMT7    DS    CL15                NON EMPL COMP                                
CAMT8    DS    CL15                                                             
CAMT9    DS    CL15                                                             
         DS    CL200                                                            
         DS    CL75                                                             
CLENQ    EQU   (*-CREC)                                                         
         EJECT                                                                  
***********************************************************************         
*               'F' RECORD DSECT                                      *         
***********************************************************************         
*                                                                               
FRECD    DSECT                                                                  
FREC     DS    0CL420              TAPE SUMMARY RECORD                          
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
PRBAMT   DS    CL10                                                             
         DS    CL15                                                             
PRBSRCE  DS    CL14                SOURCE ACCT                                  
         DS    CL2                                                              
PRBAMT2  DS    CL10                AMOUNT BROKEN DOWN                           
PRBLNQ1  EQU   132                 LENGTH OF 1 PRINT LINE                       
         ORG PRBLINE1+132                                                       
PRBLINE2 DS    0H                                                               
         DS    CL1                                                              
PRBYR    DS    CL2                 YEAR                                         
         DS    CL1                                                              
PRBMCTL  DS    CL4                                                              
         DS    CL1                                                              
PRBTTYPE DS    CL1                                                              
         DS    CL7                                                              
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
*                                                                               
D1CNM    DS    CL36      A         COMPANY NAME                                 
D1CADNL  DS    CL1       B         NO. OF CO ADDR LNS                           
D1CADLNS DS    0CL120    A         COMPANY ADDR LINES                           
D1CADLN1 DS    CL40      A         COMPANY ADDR LINES                           
D1CADLN2 DS    CL40      A         COMPANY ADDR LINES                           
D1CADLN3 DS    CL40      A         COMPANY ADDR LINES                           
D1COMPCD DS    CL2       C         COMPANY CODE FOR TAPE DDN                    
D1CTRAN  DS    CL5       C         TRANSMITTER NUMBER                           
D1YEAR   DS    CL2       C         YEAR OF FORMS                                
*                                                                               
* FOR EACH NEW PAYEE, CLEAR FROM THIS POINT ON.                                 
*                                                                               
D1RNM    DS    CL36      A         RECIPIENTS NAME                              
D1RADNL  DS    CL1       B         NO. OF RECIP ADDR LNS                        
D1RADLNS DS    0CL160    A         RECIPIENTS ADDR LINES                        
D1RADLN1 DS    CL40      A                                                      
D1RADLN2 DS    CL40      A                                                      
D1RADLN3 DS    CL40      A                                                      
D1RADLN4 DS    CL40      A                                                      
D1RAMT   DS    PL6       P         RECIPIENTS AMOUNT                            
D1SSORID DS    CL1       C         I=FED ID NO, ANYTHING ELSE IS SS NO          
D1MIN6   DS    CL1       C         600 DOLLAR MINIMUM REQESTED                  
D1FORGN  DS    CL1       C         FORIEGN ADDRESS?                             
D1PTYPE  DS    XL1                 PAYMENT TYPE                                 
D1ACCT   DS    CL14                UNIT/LDG/ACCT                                
CLRLEN   EQU   *-D1RNM             LENGTH TO CLEAR                              
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
**PAN#1  DC    CL21'040ACREPTT95 05/01/02'                                      
         END                                                                    
