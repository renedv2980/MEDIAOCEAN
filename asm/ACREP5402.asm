*          DATA SET ACREP5402  AT LEVEL 045 AS OF 06/13/18                      
*PHASE AC5402A                                                                  
*INCLUDE CENTER                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE GETLOGO                                                                
*INCLUDE PPGETADR                                                               
*INCLUDE PQPROF                                                                 
*INCLUDE PUBED                                                                  
*INCLUDE PUBVAL                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE VATICAN                                                                
         TITLE 'AC5402 - PRINT CLEARANCES'                                      
*RGUP 045 21MAY18  <SPEC-20692> NEED ADDITIONAL MEDIA FOR DIGIAL AUDIO          
AC5402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**5402*,R9,R8                                                  
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         L     RC,=A(AC54C)                                                     
         USING AC54C,RC                                                         
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
*                                                                               
         L     RF,ADMASTC                                                       
         MVC   AUTL,MCUTL-MASTD(RF)  GET ADDRESS OF UTL                         
         L     RF,AUTL                                                          
         MVC   SYSTSEN,4(RF)       SAVE SYSTEM UTL                              
         XC    ACCTAB,ACCTAB       CLEAR TABLE OF OPEN ACC SYSTEMS              
         MVC   ACCTAB(1),SYSTSEN                                                
                                                                                
***********************************************************************         
*                                                                     *         
* QSELECT FIELD :  MEDIA FILE (PRINTX)                                *         
* QOPT1         :  N= NO REMOTE OUTPUT                                *         
* OPTION 2-3    :  ALPHA AGENCY FILTER                                *         
*                  FIRST CHARACTER LOWER CASE TO EXCLUDE              *         
* OPTION 4      :  R= READ FROM RECOVER FILE (RECVIN)                 *         
* OPTION 5      :  D= ALLOW DUPLICATE POSTINGS FILES                  *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* MANAGER ROUTINE                                                     *         
***********************************************************************         
                                                                                
         BAS   RE,INIT             INITIALIZE RUN                               
         BAS   RE,PUTS             READ REQUEST - PUT TO SORT                   
         BAS   RE,GETS             GET FIRST SORTED RECORD                      
         CLI   SRTOID,X'FF'                                                     
         BE    MAINX               END OF JOB IF NO DATA FOUND/SORTED           
         MVC   SAVBRK,SRTK         SAVE FIRST CONTROL KEY                       
*                                                                               
MAIN3    BAS   RE,GCPYR            GET ACC COMPANY RECORD VALUES                
         BAS   RE,INDXR            BUILD INDEX RECORDS                          
         BAS   RE,BLDOF            BUILD OFFICE CODES AND NAMES                 
         BAS   RE,BLDMI            BUILD MI TABLE                               
*                                                                               
MAIN5    BAS   RE,REMCK            CHECK REMOTE                                 
         GOTO1 =A(OUTTRNTO),DMCB,AIO2,AGID                                      
                                                                                
         GOTO1 DIVIDE,SLOGO        START LOGO                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
MAIN6    CLC   COMREP,=C'0000'     TEST ANY COMMISSION ONLY REP                 
         BE    MAIN7                                                            
         CLC   SRTREP,COMREP       IS THIS THE REP?                             
         BNE   MAIN7                                                            
         OI    ITEMS,ITMCOMO       SET COMMISSION ONLY REP                      
*                                                                               
MAIN7    CLI   PROFIL14,C'Y'      SEE IF GETTING NAME FOR AGY HEADER            
         BE    MAIN7X                                                           
         BAS   RE,MEDNM            MEDIA CODE/NAME LOOK UP                      
*                                                                               
MAIN7X   DS    0H                  MEDIA CODE/NAME NOW GOTTEN                   
*                                  FROM AGENCY HEADER                           
         BAS   RE,PAGYH            READ PRINT AGENCY HEADER                     
         BAS   RE,CLIR             GET CLIENT RECORD                            
         BAS   RE,PRDR             GET PRODUCT RECORD                           
         BAS   RE,XCSH             EXTRACT THE CASH                             
         BAS   RE,FORM             FORMAT AND PRINT A LINE                      
         BAS   RE,SMFOX            SMF OUTPUT FOR CROSSFILE                     
         BAS   RE,ADDOF            ADD OFFICE TOTALS                            
         MVC   LASTOFF,SPACES                                                   
         TM    FLGTOT,FLGIDS       TEST SECOND PASS                             
         BO    MAIN8               YES, SKIP POSTINGS                           
         TM    ITEMS,ITMCOMO       TEST COMMISSION ONLY REP                     
         BO    MAIN8               YES, SKIP POSTINGS                           
         GOTO1 APOST,PSTDTL        MAKE THE POSTINGS                            
*                                                                               
MAIN8    BAS   RE,GETS             GET NEXT SORT RECORD                         
         CLC   SAVBRK,SRTK                                                      
         BE    MAIN6                                                            
*                                                                               
         LA    R2,CTLTBL           POINTS TO CONTROL BREAK TABLE                
         SR    R1,R1                                                            
MAIN9    IC    R1,0(R2)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAVBRK(0),SRTK                                                   
         BNE   MAIN11                                                           
         LA    R2,L'CTLTBL(R2)                                                  
         B     MAIN9                                                            
*                                                                               
MAIN11   MVC   BREAK,1(R2)         SET CONTROL TOTALS                           
         TM    BREAK,CLITQ                                                      
         BNO   *+8                                                              
         BAS   RE,CLITOT           CLIENT TOTAL                                 
         TM    BREAK,PUBTQ                                                      
         BNO   *+8                                                              
         BAS   RE,PUBTOT           PUBFILE TOTAL                                
         TM    BREAK,REPTQ                                                      
         BNO   *+8                                                              
         BAS   RE,REPTOT           REP TOTAL                                    
         TM    BREAK,MEDTQ                                                      
         BNO   *+8                                                              
         BAS   RE,MEDTOT           MEDIA TOTAL                                  
         TM    BREAK,AGYTQ                                                      
         BNO   MAIN13                                                           
         BAS   RE,AGYTOT           AGENCY                                       
         GOTO1 DIVIDE,ELOGO        END LOGO                                     
*                                                                               
MAIN13   TM    BREAK,CPYTQ         TEST BREAK ON ACC COMPANY                    
         BNO   *+8                                                              
         BAS   RE,CPYPST           COMPANY POSTINGS                             
         CLI   SRTOID,X'FF'        TEST EOF                                     
         BE    MAINX                                                            
         TM    FLGTOT,FLGIDS       ALREADY ON SECOND PASS                       
         BO    MAIN17                                                           
         OC    SRTOID,SRTOID       TEST STILL FIRST PASS                        
         BZ    MAIN17                                                           
         OI    FLGTOT,FLGIDS       SET FOR SECOND PASS                          
*                                                                               
MAIN17   TM    BREAK,CPYTQ         TEST BREAK ON ACC COMPANY                    
         BO    MAIN3               GET NEW ACC AGENCY                           
         TM    BREAK,AGYTQ         TEST BREAK ON PRINT AGENCY                   
         BO    MAIN5               GET NEW PRINT AGENCY HEADER                  
         B     MAIN6               SOME OTHER BREAK                             
*                                                                               
MAINX    XC    SAVOID,SAVOID       NEEDED BY FILE RTN---CLOSWRK/CLOSODD         
         XC    SRTOID,SRTOID       NEEDED BY DIVIDE RTN                         
         BAS   RE,RUNTOT                                                        
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         GOTO1 ADMGR,ODDCLO          CLOSE ODDS FILES                           
         L     RF,AUTL                                                          
         MVC   4(1,RF),SYSTSEN       RESTORE SYSTEM UTL                         
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION ROUTINE                                              *         
***********************************************************************         
                                                                                
INIT     NTR1  ,                                                                
         LA    R2,SRTKLEN           SET SORT CARDS                              
         LA    R4,SORTCARD+15                                                   
         EDIT  (R2),(3,0(R4)),FILL=0                                            
         LA    R2,L'SRTWRK                                                      
         LA    R4,RECDCARD+21                                                   
         EDIT  (R2),(3,0(R4)),FILL=0                                            
         GOTO1 ADSORTER,DMCB,SORTCARD,RECDCARD,0                                
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(X'20',TODAY6)                            
         GOTO1 DATCON,DMCB,(0,TODAY6),(1,TODAY)                                 
         MVC   MOS(1),TODAY6+1     BUILD MOS                                    
         MVC   MOS+1(1),TODAY6+3                                                
         CLC   TODAY6+2(2),=C'10'                                               
         BNE   *+8                                                              
         MVI   MOS+1,C'A'                                                       
         CLC   TODAY6+2(2),=C'11'                                               
         BNE   *+8                                                              
         MVI   MOS+1,C'B'                                                       
         CLC   TODAY6+2(2),=C'12'                                               
         BNE   *+8                                                              
         MVI   MOS+1,C'C'                                                       
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUF                                       
*                                                                               
         MVI   RCSUBPRG,0                                                       
         XC    XDATE,XDATE         ADDITIONAL DATE                              
         CLC   QSTART,SPACES                                                    
         BE    INIT3                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(X'20',XDATE)                             
*                                                                               
INIT3    LA    RF,OPNP                                                          
         CLC   QSELECT(4),=C'PRNT'                                              
         BE    INIT5                                                            
         DC    H'0'                INVALID REQUEST                              
INIT5    BASR  RE,RF                                                            
         XR    R4,R4                                                            
         LA    R5,MIOLNQ           GETMAIN AREA FOR SORTING ELEMENT             
         L     RE,=A(MIMAX)        GROUPS WITHIN A PERSON CODE                  
         MR    R4,RE                                                            
         ST    R5,MIBUFLN                                                       
         L     R0,MIBUFLN                                                       
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,AMIBUFF          START OF AREA                                
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* PRINT? (?=PRINT SYSTEM NUMBER...PRINT1... PRINT2)                   *         
*  OPEN MEDIA FILES                                                   *         
***********************************************************************         
                                                                                
OPNP     NTR1  ,                                                                
         MVC   REQFIL+5(2),QSELECT+4     NPREQ?                                 
         MVC   SYSNUMB,QSELECT                                                  
         MVC   PRTNUM,QSELECT                                                   
         MVI   SEN,SENPRNT              SE 4 FOR PRINT                          
*                                                                               
         L     R2,ACONIO                                                        
         USING CTWREC,R2                                                        
         XC    CTWKEY,CTWKEY       GET SYSTEM LIST RECORD                       
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         MVI   CTWKSYSN,CTWKPRNT   FOR PRINT                                    
         MVC   DKEY,0(R2)                                                       
         GOTO1 ADMGR,CONRD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,CTWDATA                                                       
OPNP3    CLI   0(R1),X'A4'         SYSTEM LIST ELEMENT                          
         BE    OPNP9                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   *+6                 NO SE NUMBER ?                               
         DC    H'0'                                                             
*                                                                               
OPNP5    ZIC   R2,1(R1)                                                         
         AR    R1,R2                                                            
         B     OPNP3                                                            
*                                                                               
         USING SYSELD,R1                                                        
OPNP9    CLC   SYSSYS,SEN          BASE SYSTEM NUMBER                           
         BNE   OPNP5               SKIP IT                                      
         CLC   PRTNUM,SYSNAME      MATCH LOGICAL SYSTEM NUMBER                  
         BNE   OPNP5               GET ANOTHER                                  
         MVC   PRNTSEN,SYSSEN      SET SE NUMBER                                
         GOTO1 ADMGR,PRTOPN                                                     
         B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* PUT REQUEST TO SORT                                                 *         
***********************************************************************         
                                                                                
PUTS     NTR1  ,                                                                
         CLI   QOPT4,C'R'          READ RECOVERY FILE                           
         BNE   PUTS3                                                            
         L     R2,=A(RECVIN)                                                    
         OPEN  ((R2),(INPUT))      OPEN FILE                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUTS1    L     R3,AREQIO                                                        
         CLI   QOPT4,C'R'          READ RECOVERY FILE                           
         BNE   PUTS3                                                            
         GET   (R2),(R3)                                                        
         LA    R3,4(R3)                                                         
         USING RECVD,R3                                                         
         CLI   RFILTY,PRTREQQ      TEST PRINT REQUEST                           
         BNE   PUTS1                                                            
         CLI   RPRG,PRTPAYQ        PAY PROGRAM                                  
         BNE   PUTS1                                                            
         LA    R3,L'RECVHDR(R3)                                                 
         USING RQHHDRD,R3          NEW 80 BYTE HEADER                           
         MVC   ORIGNUM,RQHORIG     SAVE ORIGIN ID                               
         LA    R3,L'RQHHDR(R3)     R3=A(REQUEST CARD)                           
         B     PUTS5                                                            
*                                                                               
PUTS3    GOTO1 ADMGR,PRTREQ                                                     
         BNE   PUTSX               EOF                                          
         L     R3,AREQIO                                                        
         USING REQOFFC,R3                                                       
         MVC   ORIGNUM,REQORIG     ORIGIN - FROM OLD HEADER                     
         LA    R3,L'RQHIUSR(R3)    ADD LENGTH OF OLD HEADER                     
*                                                                               
         USING REQD,R3                                                          
PUTS5    CLC   REQPROG(2),=C'30'   FILTER OUT THE NON-CHECK ELEMENTS            
         BNE   PUTS1                                                            
         TM    REQCOMM,X'20'       NEW STYLE FORMAT                             
         BO    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,FILTR                                                         
         BNE   PUTS1                                                            
*                                                                               
PUTS9    CLC   REQTODAY,SPACES     DATE FIELD SPACES OR NULL ?                  
         BNH   PUTS1               YES, SKIP  CARD                              
         SR    R0,R0                                                            
         GOTO1 DATCON,DMCB,(3,REQTODAY),(X'20',WORK)                            
         CLC   TODAY6,WORK                                                      
         BE    PUTS11                                                           
         OC    XDATE,XDATE         ADDITIONAL CLEARANCE DATE                    
         BZ    PUTS1                                                            
         CLC   XDATE,WORK                                                       
         BNE   PUTS1                                                            
*                                                                               
PUTS11   CLC   REQSTRT(L'REQSTRT+L'REQEND),SPACES                               
         BE    PUTS1                                                            
*                                                                               
PUTS15   LA    R0,SPACES           CLEAR SRT AREA TO SPACES                     
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         LA    RE,SRTWRK                                                        
         LH    RF,=Y(L'SRTWRK)                                                  
         MVCL  RE,R0                                                            
*                                                                               
         XC    SRTOID,SRTOID                                                    
         MVC   SRTCPY,CPYCODE      ACC COMPANY CODE                             
         MVC   SRTAGY,REQAGY       PRINT AGENCY CODE                            
         MVC   SRTMED,REQMED       MEDIA                                        
         MVC   SRTREP,REQREP       REP                                          
         MVC   SRTTYPE,REQTYPE     TYPE                                         
         MVC   SRTPUB,REQPUB       PUBFILE                                      
         MVC   SRTCLI,REQCLI       CLIENT                                       
         MVC   SRTPROD,REQPROD     PRODUCT                                      
         AP    SEQUENCE,=P'1'                                                   
         ZAP   SRTSEQ,SEQUENCE                                                  
*                                                                               
         MVC   SRTORIG,ORIGNUM                                                  
         MVC   SRTESTN,REQESTN     ESTIMATE NUMBER                              
         MVC   SRTINVDC,REQINVDC   INVOICE DATE                                 
         MVC   SRTOAGY,REQOAGY     OVERRIDE AGENCY                              
         MVC   SRTCOMM,REQCOMM     # OF COMMENTS IN REQNARR (0-5)               
         NI    SRTCOMM,X'FF'-X'20'                                              
         MVC   SRTSEQN,REQSEQN     CLEARANCE SEQUENCE NUMBER                    
*                                                                               
         MVC   WORK(6),REQSTRT     SET START DATE                               
         CLC   WORK+4(2),=C'00'                                                 
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',SRTSTRT)                             
         MVC   SRTSTRT+4(2),REQSTRT+4                                           
*                                                                               
         MVC   SRTEND,REQEND                                                    
         CLI   SRTEND+2,C' '       END MONTH SPACES                             
         BNH   PUTS15A                                                          
         GOTO1 DATCON,DMCB,(0,SRTEND),(X'20',SRTEND)                            
*                                                                               
PUTS15A  GOTO1 DATCON,DMCB,(3,REQTODAY),(1,SRTPDATE)                            
         CLC   REQCDTE,REQTODAY    CONTROL DATE = TODAY                         
         BE    PUTS15B             YES, SKIP IT                                 
         CLC   REQCDTE,SPACES                                                   
         BE    PUTS15B                                                          
         MVC   SRTCDTE,REQCDTE                                                  
         GOTO1 DATCON,DMCB,(3,SRTCDTE),(1,SRTPDATE)                             
*                                                                               
PUTS15B  MVC   SRTTYPE,REQTYPE                                                  
         MVC   SRTNET,REQNET        NET                                         
         MVC   SRTCSHD,REQCD        CD                                          
         MVC   SRTCDI,REQCDIND      CD INDICATOR                                
         MVC   SRTGSTYP,REQGSTYP    GST TAX CODE TYPE                           
         MVC   SRTGST,REQGST        GST TAX AMOUNT                              
*                                                                               
PUTS16   MVC   SRTOFF,REQOFF        OFFICE                                      
         MVC   SRTINV,REQINV        INVOICE NUMBER                              
         MVC   SRTCARD2,REQCARD2    PST BLOCK/CTA BLOCK                         
         OC    SRTCARD2,SRTCARD2                                                
         BNZ   *+10                                                             
         MVC   SRTCARD2,SPACES                                                  
         MVI   SRTNARL,0                                                        
         CLI   SRTCOMM,0           *** COMMENTS NEW WAY ***                     
         BE    PUTS17                                                           
         CLI   SRTCOMM,5           REQCOMM > 5 = SOMETHING WRONG                
         BH    PUTS17                                                           
         SR    RF,RF                                                            
         IC    RF,SRTCOMM          # COMMENTS                                   
         MH    RF,=H'40'           L'COMMENT                                    
         SH    RF,=H'1'                                                         
         BM    PUTS17                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRTNARR(0),REQNARR                                               
         LA    RF,L'SRTNARR                                                     
         GOTO1 ADSQUASH,DMCB,SRTNARR,(RF)                                       
         MVC   SRTNARL,7(R1)         SET LENGTH OF NARRATIVE                    
*                                                                               
PUTS17   MVC   SRTASE,ACCTSEN       SAVE ACC SE NUMBER                          
         MVC   SRTPROFL,PROFILE     AND PROFILES                                
         MVC   SRTCTRY,CTRY         COUNTRY CODE                                
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTWRK                                     
         CLI   PROFIL8,C'Y'         SEPARATE REPORTS FOR PRINT                  
         BNE   PUTS1                                                            
         MVC   SRTOID,ORIGNUM      ORIGIN NUMBER                                
         BASR  RE,RF               2ND PUT SORT                                 
         B     PUTS1                                                            
*                                                                               
PUTSX    CLI   QOPT4,C'R'                                                       
         BNE   EXIT                                                             
         CLOSE ((R2))                                                           
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FIND AGENCY                                                         *         
***********************************************************************         
                                                                                
         USING REQD,R3                                                          
FILTR    NTR1  ,                                                                
         CLC   QOPT2(2),SPACES     TEST AGENCY FILTER OPTION                    
         BE    FILT1                                                            
         CLC   REQAGY,QOPT2                                                     
         BE    FILT1                                                            
         TM    QOPT2,X'40'         TEST EXCLUDE OPTION                          
         BO    FILTNO              NOT EXCLUDE, SKIP THIS AGENCY                
         MVC   HALF,QOPT2                                                       
         OI    HALF,X'40'                                                       
         CLC   REQAGY,HALF         TEST EXCLUDE THIS AGENCY                     
         BE    FILTNO                                                           
*                                                                               
FILT1    L     R6,AAGYTAB                                                       
         USING CND,R6                                                           
         CLC   REQAGY,=C'BP'       FORCE BDAT                                   
         BNE   *+10                                                             
         MVC   REQOAGY,=C'BD'                                                   
         CLC   REQAGY,=C'BB'       AND BBCH                                     
         BNE   *+10                                                             
         MVC   REQOAGY,=C'BD'      TO BDNY                                      
         LA    RF,REQAGY                                                        
         CLI   REQOAGY,X'41'                                                    
         BL    *+8                                                              
         LA    RF,REQOAGY                                                       
         MVC   HALF,0(RF)          SAVE AGENCY CODE                             
*                                                                               
FILT3    CLI   CNAGY,0             EOT                                          
         BE    FILT5                                                            
         CLC   HALF,CNAGY          TEST AGENCY MATCH                            
         BE    FILT6                                                            
         LA    R6,CNLNQ(R6)                                                     
         B     FILT3                                                            
*                                                                               
FILT5    BAS   RE,ACCESS           READ ACCESS RECORD                           
         CLI   CNAGY,0             NO MATCH                                     
         BE    FILTNO                                                           
*                                                                               
FILT6    MVC   ACCTSEN,CNASE       CURRENT ACC SE NUMBER                        
         MVC   CTRY,CNCTRY         COUNTRY CODE                                 
         LA    RF,ACCTAB                                                        
*                                                                               
FILT7    CLC   0(1,RF),CNASE       TEST ACCFILE ALREADY OPEN                    
         BE    FILT11                                                           
         CLI   0(RF),0             EOT                                          
         BE    FILT9                                                            
         LA    RF,1(RF)                                                         
         B     FILT7                                                            
*                                                                               
FILT9    MVC   0(1,RF),CNASE       SAVE SE NUMBERS FOR OPEN FILES               
         GOTO1 ADMGR,ACCOPN        OPEN THIS ACC FILE                           
*                                                                               
FILT11   MVC   CPYCODE,CNACD       ACC COMPANY CODE                             
         MVC   CRACCT,=CL12'C006'  CASH RECEIPT CONTROL                         
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'A054'                                                 
         MVC   WORK+4(1),CPYCODE                                                
         MVC   WORK+12(2),CNAGY    ALPHA ID                                     
         L     RE,AUTL                                                          
         MVC   4(1,RE),SYSTSEN                                                  
         L     RF,APROFBUF                                                      
         GOTO1 GETPROF,DMCB,WORK,PROFILE,DATAMGR,(4,(RF))                       
         CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
FILTNO   LTR   RB,RB               SKIP IT                                      
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* READ ACCESS RECORDS - ADD ENTRY TO TABLE                            *         
***********************************************************************         
                                                                                
         USING CND,R6                                                           
ACCESS   NTR1  ,                                                                
         XC    CND(CNLNQ),CND      CLEAR LAST ENTRY                             
         L     R2,ACONIO                                                        
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,HALF                                                    
         MVC   DKEY,0(R2)                                                       
         GOTO1 ADMGR,CONRD                                                      
         BE    *+6                                                              
         DC    H'0'                CAN'T READ ACCESS RECORD                     
*                                                                               
         SR    R0,R0                                                            
         LA    R4,CT5DATA                                                       
*                                                                               
ACCESS5  CLI   0(R4),0             TEST EOR                                     
         BE    EXIT                GET NEXT RECORD                              
         CLI   0(R4),CTSYSELQ      SYSTEM ELEMENT                               
         BE    ACCESS7                                                          
         CLI   0(R4),CTAGDELQ                                                   
         BE    ACCESS8                                                          
ACCESS6  IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ACCESS5                                                          
*                                                                               
         USING CTSYSD,R4                                                        
ACCESS7  CLI   CTSYSNUM,6          TEST ACCOUNT FILE                            
         BNE   ACCESS6                                                          
         MVC   CNACD,CTSYSAGB      ACC AGENCY BINARY                            
         MVC   CNASE,CTSYSSE       ACC SE                                       
         MVC   CNAGY,CT5KALPH      AGENCY ID                                    
         B     ACCESS6                                                          
*                                  FIND COUNTRY CODE                            
         USING CTAGDD,R4                                                        
ACCESS8  CLI   CTAGDLEN,CTAGDCTY-CTAGDD                                         
         BL    ACCESS6                                                          
         MVC   CNCTRY,CTAGDCTY   SET AGENCY COUNTRY CODE                        
         B     ACCESS6                                                          
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* GET RECORD FROM SORTER                                              *         
***********************************************************************         
                                                                                
GETS     NTR1  ,                                                                
         MVC   SAVBRK,SRTK           SAVE OLD CONTROL BREAK                     
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BNZ   GETS3                                                            
         MVI   SRTK,X'FF'                                                       
         MVC   SRTK+1(SRTBKLNQ-1),SRTK                                          
         B     EXIT                                                             
*                                                                               
GETS3    LA    R3,L'SRTWRK         MOVE IT WHERE I CAN SEE IT                   
         LR    R5,R3                                                            
         LA    R4,SRTWRK                                                        
         MVCL  R4,R2               MOVE CARD REQUEST TO SRTWRK                  
         MVI   ITEMS,0             CLEAR ITEM STATUS                            
         CLC   SRTREP,SPACES       TEST REP                                     
         BE    GETS4                                                            
         OI    ITEMS,ITMREP        SET ITEM REP STATUS                          
*                                                                               
GETS4    DS    0H                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ PRINT AGENCY HEADER                                            *         
***********************************************************************         
                                                                                
PAGYH    NTR1  ,                                                                
         XC    DKEY,DKEY                                                        
         LA    R4,DKEY                                                          
         USING PAGYREC,R4                                                       
         MVC   PAGYKAGY,SRTAGY     AGENCY                                       
         MVC   PAGYKMED,SRTMED     MEDIA                                        
         MVI   PAGYKRCD,X'01'      RECORD TYPE                                  
         GOTO1 ADMGR,PRTHI                                                      
         CLC   DIR(L'PAGYKEY),DKEY                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ADMGR,PRTGET                                                     
         L     R4,AIO1                                                          
         OC    PAGYNAME,SPACES                                                  
         MVC   AGYNAME,PAGYNAME                                                 
         MVC   AGYPROF,PAGYPROF                                                 
*                                                                               
         CLI   PROFIL14,C'Y'                                                    
         BNE   EXIT                                                             
*                                                                               
         MVC   MEDNAME,SPACES                                                   
         MVC   MEDNAME(10),PAGYMED                                              
         OC    MEDNAME(10),SPACES                                               
         MVC   MEDCODE(1),PAGYKMED                                              
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET ACC COMPANY RECORD                                              *         
***********************************************************************         
                                                                                
GCPYR    NTR1  ,                                                                
         MVC   ACCTSEN,SRTASE      SET ACC SE NUMBER                            
         MVC   PROFILE,SRTPROFL    RESTORE PROFILE                              
         L     RF,ADMASTC                                                       
         MVC   MCCTRY-MASTD(L'MCCTRY,RF),SRTCTRY RESTORE COUNTRY CODE           
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(1),SRTCPY      READ COMPANY RECORD                          
         GOTO1 ADMGR,ACCRD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADMGR,ACCGET                                                     
         L     R4,AIO1                                                          
         AH    R4,FRSTL                                                         
         SR    R1,R1                                                            
*                                                                               
GCPYR5   CLI   0(R4),CPYELQ                                                     
         BE    GCPYR7                                                           
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GCPYR5                                                           
*                                                                               
         USING CPYELD,R4                                                        
GCPYR7   SR    R5,R5                                                            
         ICM   R5,3,CPYREPC       REP CODE FOR COMMISSION ONLY                  
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB+5(3)                                                 
         MVC   COMREP,WORK+1                                                    
         MVC   CMPSTA4,CPYSTAT4    SAVE STATUS BYTES                            
         MVC   CMPSTA8,CPYSTAT8                                                 
         MVC   AGID,CPYUID         GET ORIGIN ID NUMBER                         
         OC    SRTOID,SRTOID       TEST SECOND PASS                             
         BZ    *+10                                                             
         MVC   AGID,SRTOID         USE ORIGINAL ID FOR REMOTE                   
*                                                                               
         MVC   TAXUL,SPACES        ASSUME NO GST                                
         CLI   CPYLN,CPYLN2Q                                                    
         BL    *+10                                                             
         MVC   TAXUL,CPYTAX        SAVE U/L FOR GST                             
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD INDEX RECORD FOR POSTING AND ODD FILE                         *         
***********************************************************************         
                                                                                
INDXR    NTR1  ,                                                                
         TM    FLGTOT,FLGIDS       TEST SECOND PASS                             
         BO    EXIT                YES, SKIP WORKER FILES                       
*                                                                               
         LA    R5,POSTKEY          BUILD INDEX FOR POSTING FILE                 
         USING UKRECD,R5                                                        
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKUSRID,AGID                                                     
         MVC   UKSYSPRG,=C'P54'                                                 
         L     RF,=A(REQFIL)                                                    
         MVC   UKSUBPRG,5(RF)                                                   
         MVC   UKEXTRA,6(RF)                                                    
         CLI   UKEXTRA,C' '                                                     
         BH    *+8                                                              
         MVI   UKEXTRA,0                                                        
         MVC   UKDAY,TODAY+2                                                    
         MVI   UKCLASS,C'P'                                                     
         CLI   QOPT5,C'D'          ALLOW DUPLICATE FILES                        
         BNE   *+8                                                              
         MVI   UKFLAG,X'01'                                                     
         GOTO1 ADMGR,WRKOPN        OPEN POSTING FILE                            
*                                                                               
         OC    ODDKEY,ODDKEY       TEST FIRST TIME                              
         BNZ   EXIT                                                             
         MVC   ODDKEY,POSTKEY      BUILD INDEX FOR ODDS FILE                    
         LA    R5,ODDKEY                                                        
         MVI   UKCLASS,C'O'                                                     
         GOTO1 ADMGR,ODDOPN        OPEN ODDS FILE                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD OFFICE CODE AND OFFICE NAME TABLE                             *         
***********************************************************************         
                                                                                
BLDOF    NTR1  ,                                                                
         CLI   PROFIL12,C'Y'       INCLUDE OFFICE TABLE?                        
         BNE   EXIT                NO - SKIP                                    
         LA    R4,DKEY                                                          
         USING OFFRECD,R4                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ    X'01' - OFFICE RECORD TYPE                   
         MVC   OFFKCPY,SRTCPY      COMPANY CODE                                 
         GOTO1 ADMGR,ACCHI         READ HIGH                                    
         B     BLDOF5                                                           
*                                                                               
BLDOF3   GOTO1 ADMGR,ACCSEQ                                                     
BLDOF5   LA    R4,DIR                                                           
         CLC   OFFKEY(OFFKOFF-OFFKEY),DKEY                                      
         BNE   EXIT                                                             
         CLC   OFFKOFF-OFFKEY(L'OFFKOFF,R4),SPACES     ANY OFFICE?              
         BE    BLDOF3                               NO - SKIP                   
         GOTO1 ADMGR,ACCGET                                                     
         L     R4,AIO1                                                          
         LA    R2,OFNWRK                                                        
         USING OFND,R2                                                          
         MVC   OFNWRK,SPACES       CLEAR CODE TABLE WORK AREA                   
         MVC   OFNCDE,OFFKOFF-OFFKEY(R4)            YES - SAVE IT               
*                                                                               
BLDOF7   LA    R4,OFFRFST          BUMP TO ELEMENTS                             
         USING NAMELD,R4                                                        
BLDOF9   CLI   0(R4),0                                                          
         BE    BLDOF13             IF NO NAME ELEMENT DON'T ADD NAME            
         CLI   NAMEL,NAMELQ        X'20' - NAME ELEMENT                         
         BE    BLDOF11                                                          
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         AR    R4,R1                                                            
         B     BLDOF9                                                           
*                                                                               
BLDOF11  SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFNNME(0),NAMEREC                                                
*                                                                               
BLDOF13  GOTO1 BINADD,DMCB,OFNWRK,AOFNTAB    ADD TO OFF CDE TABLE               
         B     BLDOF3                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF MEDIA INTERFACE RECORDS                              *         
***********************************************************************         
                                                                                
BLDMI    NTR1  ,                                                                
                                                                                
         USING MIOELD,R5                                                        
         L     R5,AMIBUFF       R5=BUFFER OF MI ELEMENTS                        
         LHI   R0,MIMAX                                                         
         MVI   0(R5),X'FF'                                                      
         TM    CMPSTA4,CPYSMINT  MEDIA INTERFACE RECORDS                        
         BNO   EXIT                                                             
         USING MINRECD,R2                                                       
BLDM00   LA    R2,DKEY                                                          
         MVC   DKEY,SPACES                                                      
         MVI   MINKTYP,MINKTYPQ  READ MEDIA INTERFACE RECORDS                   
         MVC   MINKCPY,SRTCPY                                                   
         GOTO1 ADMGR,ACCHI                                                      
*                                                                               
BLDM01   CLC   DIR(2),DKEY                                                      
         BNE   EXIT                                                             
         LA    R2,DIR              RESET R2 TO POINT TO KEY                     
         GOTO1 ADMGR,ACCGET                                                     
         L     R4,AIO1                                                          
         AH    R4,FRSTL                                                         
         SR    R1,R1                                                            
*                                                                               
BLDM03   CLI   0(R4),0             GET THE MI (19) ELEMENT                      
         BE    BLDM07                                                           
         CLI   0(R4),MDIELQ                                                     
         BE    BLDM05                                                           
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     BLDM03                                                           
*                                                                               
         USING MDIELD,R4                                                        
BLDM05   MVC   MIOCODE,MDICODE     MI CODE                                      
         MVC   MIOOFF,MINKOFF      MI OFFICE                                    
         MVC   MIODATA,MDIDESC     REST OF ELEMENT                              
         LA    R5,MIOLNQ(R5)                                                    
         BCT   R0,*+6                                                           
         DC    H'0'                MI BUFFER IS FULL                            
         MVI   0(R5),X'FF'         NEW END OF BUFFER                            
*                                                                               
BLDM07   GOTO1 ADMGR,ACCSEQ        NEXT MI RECORD                               
         B     BLDM01                                                           
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* CLIENT NAME ROUTINE                                                 *         
***********************************************************************         
                                                                                
CLIR     NTR1  ,                                                                
         CLC   CLINAME,SPACES                                                   
         BNE   EXIT                                                             
         XC    DKEY,DKEY                                                        
         LA    R4,DKEY                                                          
         USING PCLTREC,R4                                                       
         MVC   PCLTKAGY,SRTAGY     AGENCY                                       
         MVC   PCLTKMED,SRTMED     MEDIA                                        
         MVC   PCLTKCLT,SRTCLI     CLIENT                                       
         MVI   PCLTKRCD,X'02'                                                   
         GOTO1 ADMGR,PRTHI                                                      
         CLC   DIR(L'PCLTKEY),DKEY                                              
         BE    CLIR3                                                            
         BAS   RE,NOPRNT           CAN'T READ PRINT FILE                        
         MVC   CLINAME(7),=CL10'MISSING'                                        
         B     EXIT                                                             
*                                                                               
CLIR3    GOTO1 ADMGR,PRTGET                                                     
         L     R4,AIO1                                                          
         OC    PCLTNAME,SPACES                                                  
         MVC   CLINAME,PCLTNAME                                                 
         MVC   POFC(1),PCLTOFF     SAVE PRINT OFFICE CODE                       
         MVC   OFFC(1),PCLTOFF     DEFAULT OFFICE FOR ACC                       
         MVI   OFFC+1,C' '                                                      
         TM    CMPSTA4,CPYSOFF2    NEW 2 BYTE OFFICES                           
         BNO   EXIT                                                             
         CLI   PCLTAOFC,C' '      2 BYTE OFFICE                                 
         BNH   EXIT                                                             
         MVC   OFFC,PCLTAOFC                                                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PRODUCT NAME ROUTINE                                                *         
***********************************************************************         
                                                                                
PRDR     NTR1  ,                                                                
         CLC   SRTPROD,SAVPRD      TEST SAME PRODUCT                            
         BE    EXIT                                                             
         MVC   SAVPRD,SRTPROD      SAVE THE PRODUCT CODE                        
         MVC   PRDNAME,SPACES                                                   
         MVC   PRDNAME(3),=C'ALL'                                               
         CLC   SRTPROD,=C'ALL'                                                  
         BE    EXIT                                                             
*                                                                               
PRDR1    XC    DKEY,DKEY                                                        
         LA    R4,DKEY                                                          
         USING PPRDREC,R4                                                       
         MVC   PPRDKAGY,SRTAGY     AGENCY                                       
         MVC   PPRDKMED,SRTMED     MEDIA                                        
         MVI   PPRDKRCD,X'06'      RECORD TYPE                                  
         MVC   PPRDKCLT,SRTCLI     CLIENT                                       
         MVC   PPRDKPRD,SRTPROD    PRODUCT                                      
*                                                                               
         GOTO1 ADMGR,PRTHI                                                      
         CLC   DIR(L'PPRDKEY),DKEY                                              
         BE    PRDR5                                                            
         BAS   RE,NOPRNT           CAN'T READ PRINT FILE                        
         B     EXIT                                                             
*                                                                               
PRDR5    GOTO1 ADMGR,PRTGET                                                     
         L     R4,AIO1                                                          
         OC    PPRDNAME,SPACES                                                  
         MVC   PRDNAME,PPRDNAME    SAVE PRODUCT NAME                            
         OC    PRDNAME,SPACES                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT CASH VALUES \ ADD TO HIGHER LEVELS                          *         
***********************************************************************         
                                                                                
XCSH     NTR1  ,                                                                
         LA    R1,CSHCTL                                                        
         LHI   R0,CSHC                                                          
         ZAP   0(8,R1),=P'0'       CLEAR ACCUMS                                 
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
XCSH10   ICM   R1,15,SRTNET        NET                                          
         CVD   R1,DUB                                                           
         ZAP   NET,DUB                                                          
*                                                                               
         ICM   R1,15,SRTCSHD       CASH DISCOUNT                                
         CVD   R1,DUB                                                           
         LA    RF,CASHD                                                         
         CLI   SRTCDI,SRTCDIL      LOST CD                                      
         BNE   *+8                                                              
         LA    RF,LCASHD                                                        
         ZAP   0(L'CASHD,RF),DUB                                                
*                                                                               
         CLI   SRTGSTYP,C' '       TEST GST TYPE                                
         BNH   *+14                                                             
         CLC   SRTGST,SPACES       TEST GST AMOUNT                              
         BNE   *+10                                                             
         XC    SRTGST,SRTGST                                                    
         ICM   R1,15,SRTGST                                                     
         CVD   R1,DUB                                                           
         ZAP   GST,DUB                                                          
*                                                                               
         CLC   SRTPST,SPACES       PROVINCE SALES TAX (PST)                     
         BE    XCSH35                    NO PST PRESENT                         
         MVC   BYTE,SRTGIND              SAVE OFF INDICATOR BYTE                
         LA    RF,L'SRTPST/SRTPSTLQ      MAX NUMBER OF PST'S                    
         LA    RE,SRTPST                 START OF PST BLOCK                     
*                                                                               
         USING SRTPST,RE                                                        
XCSH15   CLI   SRTPSTYP,C' '       AT END OF  PST LIST ?                        
         BNH   XCSH35              YES, NO MORE PST TO ADD TO                   
         CLC   SRTPSTBS,SPACES                                                  
         BE    *+14                                                             
         OC    SRTPSTBS,SRTPSTBS                                                
         BNZ   XCSH20                                                           
         TM    BYTE,X'40'          ARE WE RUNNING AS BINARY?                    
         BNO   *+14                                                             
         XC    SRTPSTBS,SRTPSTBS                                                
         B     XCSH20                                                           
         ZAP   SRTPSTBS,=P'0'                                                   
*                                                                               
XCSH20   CLI   SRTTYPE,SRTTCHK     CASH RECEIPT                                 
         BNE   XCSH30                                                           
*                                                                               
         TM    BYTE,X'20'          PST IN BINARY                                
         BO    *+14                                                             
         ZAP   DUB,SRTPSTAM                                                     
         B     *+12                                                             
         ICM   R1,15,SRTPSTAM                                                   
         CVD   R1,DUB                                                           
         MP    DUB,=P'-1'                                                       
         TM    BYTE,X'20'          PST IN BINARY                                
         BO    *+14                                                             
         ZAP   SRTPSTAM,DUB                                                     
         B     *+12                                                             
         CVB   R1,DUB                                                           
         STCM  R1,15,SRTPSTAM                                                   
*                                                                               
         TM    BYTE,X'40'          ARE WE RUNNING AS BINARY?                    
         BNO   XCSH25                                                           
         ICM   R1,15,SRTPSTBS                                                   
         LNR   R0,R1                                                            
         STCM  R0,15,SRTPSTBS                                                   
         B     XCSH30                                                           
*                                                                               
XCSH25   ZAP   DUB,SRTPSTBS                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   SRTPSTBS,DUB                                                     
*                                                                               
XCSH30   DS    0H                                                               
         TM    BYTE,X'20'          PST IN BINARY                                
         BO    *+14                                                             
         ZAP   DUB,SRTPSTAM                                                     
         B     *+12                                                             
         ICM   R1,15,SRTPSTAM                                                   
         CVD   R1,DUB                                                           
         ZAP   PST,DUB             ADD  UP ALL THE PST                          
         LA    RE,SRTPSTLQ(RE)     BUMP TO NEXT PST AMOUNT                      
         BCT   RF,XCSH15                                                        
         DROP  RE                                                               
*                                                                               
XCSH35   SP    NET,CASHD           NET INCLUDES CD, GST, PST                    
         SP    NET,GST                                                          
         SP    NET,PST                                                          
*                                                                               
         CLI   SRTTYPE,SRTTCHK     CASH RECEIPT(COMES IN NEGATIVE)              
         BNE   XCSH40                                                           
         ZAP   CASHR,NET           KEEP POSITIVE CR                             
         AP    CASHR,GST                                                        
         AP    CASHR,PST                                                        
         MP    CASHR,=P'-1'                                                     
*                                                                               
XCSH40   TM    ITEMS,ITMCOMO       TEST ANY COMMISSION ONLY REP                 
         BNO   XCSH50                                                           
         CLI   SRTTYPE,SRTTCHK     IS IT A COMMISSION CASH RECEIPT              
         BNE   XCSH45                                                           
         AP    NET,GST             COMMISSION ONLY                              
         AP    NET,PST                                                          
         ZAP   GST,=P'0'           WITH GST - ADD GST TO NET                    
         ZAP   PST,=P'0'                                                        
*                                                                               
XCSH45   AP    CONET,NET           COMMISSION ONLY                              
         AP    COGST,GST           GST SEPARATE                                 
         AP    COGST,PST           ADD PST IN WITH GST                          
*                                                                               
XCSH50   ZAP   TOT,NET             TOT =NET + GST + PST                         
         AP    TOT,GST                                                          
         AP    TOT,PST                                                          
*                                                                               
         LHI   R1,OFFLINE          SET FOR OFFLINE                              
         MH    R1,=Y(CSHTLNQ)      X WIDTH OF EACH TABLE                        
         L     RF,AACCUMS          RF=A(ACCUMULATORS)                           
         AR    RF,R1               RF=A(SPECIFIC TABLE IN ACCUMS)               
*                                                                               
         LA    R1,NROW             R1=NUMBER OF ROWS                            
         TM    FLGTOT,FLGIDS       TEST SECOND PASS(BY ID)                      
         BNO   *+6                                                              
         BCTR  R1,0                DON'T ADD TO RUN TOTALS                      
*                                                                               
XCSH55   LA    RE,CSHCTL           RE=A(CASH BUCKETS FOR THIS REQUEST)          
         LHI   R0,CSHC             R0=NUMBER OF COLUMNS                         
*                                                                               
         AP    0(8,RF),0(8,RE)     ADD TO HIGHER LEVELS                         
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14             ADD ALL COLUMNS                              
         BCT   R1,XCSH55           ADD TO EACH HIGHER ROW                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FORMAT A LINE OF PRINT                                              *         
***********************************************************************         
                                                                                
FORM     NTR1  ,                                                                
         GOTO1 APUBL,DMCB,3        GET PUBNAME                                  
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVC   PLINE+1(36),PUBNML                                               
         CLC   SRTPUB+8(3),=C'ZZZ'                                              
         BNE   FORM1                                                            
         MVC   PLINE+38(12),=C'ALL EDITIONS'                                    
         GOTO1 ADSQUASH,DMCB,PANAL,55                                           
*                                                                               
FORM1    BAS   RE,REPRT                                                         
         MVC   PREP,SRTREP                                                      
         MVC   WORK(11),SRTPUB                                                  
         CLC   SRTPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZON                       
         BNE   *+10                                                             
         MVC   WORK+8(3),SPACES      USE 'BASE' PUB NUMBER                      
         GOTO1 PUBVAL,DMCB,WORK,WORK+12                                         
         GOTO1 PUBED,DMCB,WORK+12,PPUB                                          
*                                                                               
         MVC   PCLIENT,SRTCLI                                                   
         MVC   PINVOICE,SRTINV                                                  
         MVC   PSTART,SRTSTRT                                                   
         CLC   PSTART+4(2),=C'00'                                               
         BNE   *+8                                                              
         MVI   PSTART+5,C'1'                                                    
         CLI   SRTEND+2,C' '                                                    
         BE    FORM2                                                            
         MVI   PSTART+6,C'-'                                                    
         MVC   PEND,SRTEND                                                      
*                                                                               
FORM2    MVC   PPRODNM,PRDNAME                                                  
         MVC   PTYPE,=C'RG'                                                     
         CLI   SRTTYPE,C'1'        1=PAYMENT (REGULAR)                          
         BE    FORM4                                                            
         MVC   PTYPE,=C'CR'                                                     
         CLI   SRTTYPE,C'2'        2=CREDIT MEMO                                
         BE    FORM4                                                            
         MVC   PTYPE,=C'CK'        3=CASH RECEIPT                               
*                                                                               
FORM4    EDIT  CASHD,(12,PTOTCD),2,MINUS=YES                                    
         EDIT  TOT,(12,PTOTNET),2,MINUS=YES                                     
         CP    LCASHD,=P'0'                                                     
         BE    FORM5                                                            
         EDIT  (P8,LCASHD),(12,PTOTCD),2,MINUS=YES,TRAIL=C'*'                   
*                                                                               
FORM5    CLI   TAXUL,C' '                                                       
         BNH   FORM7                                                            
         EDIT  GST,(12,PTOTGST),2,MINUS=YES                                     
         EDIT  PST,(12,PTOTPST),2,MINUS=YES                                     
*                                                                               
FORM7    CLC   SRTCDTE,SPACES      TEST CONTROL DATE                            
         BE    FORM9                                                            
         LA    R7,PSECOND                                                       
         MVC   PDATE,=CL13'CONTROL DATE='                                       
         GOTO1 DATCON,DMCB,(3,SRTCDTE),(8,PDATE1)                               
*                                                                               
FORM9    BAS   RE,REPRT                                                         
         LA    R7,P                                                             
         OC    SRTINVDC,SRTINVDC                                                
         BZ    FORM10                                                           
         MVC   PDATE,=CL13'INVOICE DATE='                                       
         GOTO1 DATCON,DMCB,(2,SRTINVDC),(8,PDATE1)                              
         BAS   RE,REPRT                                                         
*                                                                               
FORM10   CLI   SRTNARL,0                                                        
         BE    EXIT                                                             
         SR    R4,R4                                                            
         IC    R4,SRTNARL                                                       
         GOTO1 CHOPPER,DMCB,((R4),SRTNARR),(80,WORKA),3                         
         LA    R2,WORKA                                                         
         L     R3,8(R1)            NUMBER OF LINES USED                         
         LTR   R3,R3                                                            
         BNP   EXIT                                                             
*                                                                               
FORM11   MVC   PANAL(80),0(R2)     UP TO 3 LINES OF NARRATIVE                   
         BAS   RE,REPRT                                                         
         LA    R2,80(R2)                                                        
         BCT   R3,FORM11                                                        
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT SMF RECORD FOR CROSSFILE CLEARANCES                                    
***********************************************************************         
         USING CROSD,SMFREC                                                     
SMFOX    NTR1  ,                                                                
         CLI   SRTOAGY,C' '          ANY OVERRIDE ACC AGENCY?                   
         BNH   SMFOXX                NO: NO NEED TO LOG                         
         CLC   SRTAGY,SRTOAGY        SAME AS THE MEDIA AGENCY?                  
         BE    SMFOXX                YES: NO NEED TO LOG                        
         XC    SMFREC,SMFREC                                                    
         MVC   CROSLN,=AL2(CROSLNQ)  RECORD LENGTH                              
         MVI   CROSTYP,CROSTYPQ      RECORD TYPE           X                    
         MVI   CROSSUB,CROSSUB1      SUB RECORD TYPE       1                    
         L     R1,ADMASTC                                                       
         L     R1,MCSSB-MCBLOCK(R1)                                             
         MVC   CROSDSP,SSODSPAC-SSOOFF(R1) DATA SPACE      C                    
         MVI   CROSYSN,X'06'         SYSTEM                X                    
         MVI   CROSYSA,C'A'          SYSTEM                A                    
         MVI   CROSPGM+0,C'5'        PROGRAM               5                    
         MVI   CROSPGM+1,C'4'        PROGRAM               4                    
         MVC   CROSUID,SRTOID        COMPANY ID            XX                   
         MVC   CROSPID,SRTPID        PERSON ID             XX                   
*        MVC   CROSSAGY,             SECURITY AGENCY                            
         MVC   CROSAAGY,SRTOAGY      ACCOUNTING AGENCY     AA                   
         MVC   CROSMAGY,SRTAGY       MEDIA AGENCY          AA                   
         MVC   CROSASE,SRTASE        ACCOUNTING SE NUMBER  N                    
         MVC   CROSMSE,PRNTSEN       MEDIA SE NUMBER       N                    
         MVC   CROSOFF,SRTOFF        OFFICE                AA                   
         MVC   CROSMED,SRTMED        MEDIA CODE            M                    
         MVC   CROSCLI,SRTCLI        CLIENT CODE           CCC                  
         MVC   CROSPRD,SRTPROD       PRODUCT CODE          PPP                  
         L     R2,ADCOMFAC                                                      
         ICM   R2,15,CSMFOUT-COMFACSD(R2)                                       
         GOTO1 (R2),DMCB,13,SMFREC                                              
SMFOXX   B     EXIT                                                             
SMFREC   DS    XL(CROSLNQ)           SMFREC                                     
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT CLIENT TOTALS                                                 *         
***********************************************************************         
                                                                                
CLITOT   NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVI   ROW,ROWCLI                                                       
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW                  GET CLIENT TOTALS                       
         BAS   RE,REPRT                 BLANK LINE                              
         MVC   PTOTLINE(18),=CL18'CLIENT TOTALS'                                
         BAS   RE,EDIT                                                          
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                 BLANK LINE                              
*                                                                               
CLITOT3  MVC   BUFCLI,SAVCLI            BUILD A BUFFALO RECORD                  
         MVC   BUFCLIN,CLINAME                                                  
         MVC   BUFOFF,OFFC                                                      
         CLI   LASTOFF,C' '                                                     
         BNH   CLITOT5                                                          
         MVC   BUFOFF,LASTOFF                                                   
*                                                                               
CLITOT5  MVI   BUFTYPE,BUFTBOTH         POST CLIENT OFF/ONLINE TOTALS           
         MVC   BUFTOTAL(CSHLNQ),CSHCTL                                          
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFLINE                               
*                                                                               
         MVI   TYPE,TYPOFF              GET OFFLINE TOTALS                      
         BAS   RE,GROW                                                          
         MVI   BUFTYPE,BUFTOFFL         AMOUNT W/O ONLINE CLEARANCES            
         MVC   BUFTOTAL(CSHLNQ),CSHCTL                                          
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUF,BUFLINE                               
*                                                                               
CLITOT7  BAS   RE,CROW                  ZERO CLIENT TOTALS                      
         MVC   SAVPRD,SPACES            CLEAR PRODUCT                           
         MVC   CLINAME,SPACES           CLIENT NAME                             
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT PUBFILE TOTALS                                                *         
***********************************************************************         
                                                                                
PUBTOT   NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVI   ROW,ROWPUB                                                       
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW             GET PUBFILE TOTALS                           
         MVC   PTOTLINE,=CL25'PUBLICATION TOTALS'                               
         BAS   RE,EDIT                                                          
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
*                                                                               
         BAS   RE,CROW                   CLEAR TOTALS                           
         MVC   PUBNM,SPACES        FORCE PUBFILE READ                           
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT REP TOTALS                                                    *         
***********************************************************************         
                                                                                
REPTOT   NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVI   ROW,ROWREP                                                       
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW             GET REP TOTALS                               
         CLC   SAVREP,SPACES       DONT PRINT REP TOTALS WHEN                   
         BE    REPTOT3             THERE IS NO REP                              
         MVC   PTOTLINE,=CL25'REP TOTALS'                                       
         BAS   RE,EDIT                                                          
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
*                                                                               
REPTOT3  BAS   RE,CROW             CLEAR TOTALS                                 
         MVC   REPNAME,SPACES                                                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT MEDIA TOTALS                                                  *         
***********************************************************************         
                                                                                
MEDTOT   NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVI   ROW,ROWMED                                                       
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW             GET MEDIA TOTALS                             
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
*                                                                               
         OI    FLGTOT,FLGSUM        SUMMARY HEADS                               
         LA    R7,MID1                                                          
         MVC   PANAL,=C'ANALYSIS BY CLIENT'                                     
         LA    R7,L'MID1(R7)                                                    
         MVC   PANAL,=C'------------------'                                     
         MVI   FORCEMID,C'Y'                                                    
         LA    R7,P                                                             
         BAS   RE,REPRT                                                         
         XC    BUFLINE,BUFLINE     CLEAR BUFFALO RECORD                         
         MVI   BUFTYPE,BUFTBOTH    GET ON & OFFLINE TOTALS                      
         LA    R1,BUFTOTAL                                                      
         LHI   R0,BUFTOT#                                                       
         ZAP   0(L'BUFTOTAL,R1),=P'0'                                           
         LA    R1,L'BUFTOTAL(R1)                                                
         BCT   R0,*-10                                                          
         MVC   COMMAND,=CL8'HIGH'                                               
         B     *+10                                                             
*                                                                               
MEDTOT5  MVC   COMMAND,=CL8'SEQ'                                                
         GOTO1 BUFFALO,DMCB,COMMAND,ADBUF,BUFLINE,1                             
         TM    DMCB+8,X'80'                                                     
         BO    MEDTOT9                                                          
         CLI   BUFTYPE,BUFTBOTH                                                 
         BNE   MEDTOT5                                                          
         MVC   PCLIENT,BUFCLI      CLIENT CODE                                  
         MVC   PCLINAME,BUFCLIN    CLIENT NAME                                  
*                                                                               
         AP    BUFTOT,BUFCASHR                                                  
         EDIT  BUFTOT,(12,PTOTNET),2,MINUS=YES                                  
         MP    BUFCASHR,=P'-1'                                                  
         EDIT  BUFCASHR,(12,PTOTCR),2,MINUS=YES                                 
         EDIT  BUFCASHD,(12,PTOTCD),2,MINUS=YES                                 
         CLI   TAXUL,C' '          TEST GST/PST                                 
         BNH   MEDTOT7                                                          
         EDIT  BUFGST,(12,PTOTGST),2,MINUS=YES,ZERO=BLANK                       
         EDIT  BUFPST,(12,PTOTPST),2,MINUS=YES,ZERO=BLANK                       
*                                                                               
MEDTOT7  MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
         B     MEDTOT5                                                          
*                                                                               
MEDTOT9  TM    FLGTOT,FLGAGY       ARE WE GOING AGENCY OFFICE TOTALS            
         BO    *+8                                                              
         BAS   RE,OFFTOT           DO OFFICE TOTALS                             
         MVC   PTOTLINE(12),=CL12'MEDIA TOTALS'                                 
         BAS   RE,EDTOT                                                         
*                                                                               
         MVC   PTOTLINE,=CL25'REGULAR LESS CASH RECEIPT'                        
         EDIT  TOT,(12,PTOTNET),2,MINUS=YES                                     
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
         BAS   RE,CROW             CLEAR MEDIA TOTALS                           
*                                                                               
         XC    BUFLINE,BUFLINE     *** CREATE MEDIA CONTROL POSTINGS **         
         MVI   BUFTYPE,BUFTOFFL                                                 
         LA    R1,BUFTOTAL                                                      
         LHI   R0,BUFTOT#                                                       
         ZAP   0(L'BUFTOTAL,R1),=P'0'                                           
         LA    R1,L'BUFTOTAL(R1)                                                
         BCT   R0,*-10                                                          
         MVC   COMMAND,=CL8'HIGH'                                               
         B     *+10                                                             
*                                                                               
MEDTOT13 MVC   COMMAND,=CL8'SEQ'                                                
         GOTO1 BUFFALO,DMCB,COMMAND,ADBUF,BUFLINE,1                             
         TM    DMCB+8,X'80'                                                     
         BO    MEDTOT15                                                         
         CLI   BUFTYPE,BUFTOFFL                                                 
         BNE   MEDTOT13                                                         
         TM    FLGTOT,FLGIDS       TEST SECOND PASS                             
         BO    MEDTOT13            YES, SKIP POSTINGS                           
         GOTO1 APOST,PSTMED        MAKE 'SZ' POSTINGS                           
         B     MEDTOT13                                                         
*                                                                               
MEDTOT15 GOTO1 BUFFALO,DMCB,=C'RESET',ADBUF                                     
         NI    FLGTOT,X'FF'-(FLGSUM) RESET PRINT SUMMARY FLAG                   
         TM    BREAK,AGYTQ         TEST AGENCY TOTAL NEXT                       
         BO    *+8                 PRINT AGENCT TOTAL ON SAME PAGE              
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT AGENCY TOTALS                                                 *         
***********************************************************************         
                                                                                
AGYTOT   NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVI   ROW,ROWAGY                                                       
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW                                                          
         OI    FLGTOT,FLGAGY       OFFICE TABLE BY AGENCY                       
         BAS   RE,OFFTOT           AND OFFICE TOTALS                            
*                                                                               
         OI    FLGTOT,FLGSUM       SET PRINT SUMMARY HEADS                      
         MVC   PTOTLINE(13),=CL13'AGENCY TOTALS'                                
         BAS   RE,EDTOT                                                         
*                                                                               
         MVI   TYPE,TYPONL                                                      
         BAS   RE,GROW                                                          
         CP    TOT,=P'0'                                                        
         BE    AGYTOT5                                                          
         MVC   PTOTLINE,=CL25'AGENCY ON-LINE TOTAL'                             
         EDIT  TOT,(12,PTOTNET),2,MINUS=YES                                     
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
         BAS   RE,PONL             POST ON LINE TOTAL                           
*                                                                               
AGYTOT5  MVC   PTOTLINE,=CL25'REGULAR LESS CASH RECEIPT'                        
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW                                                          
         EDIT  TOT,(12,PTOTNET),2,MINUS=YES                                     
         BAS   RE,REPRT                                                         
*                                                                               
AGYTOT7  NI    FLGTOT,X'FF'-(FLGSUM+FLGAGY)                                     
         MVI   ROW,ROWAGY          CLEAR TOTALS                                 
         BAS   RE,CROW                                                          
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* COMPANY POSTINGS                                                    *         
***********************************************************************         
                                                                                
CPYPST   NTR1  ,                                                                
         TM    FLGTOT,FLGIDS       TEST SECOND PASS                             
         BNO   CPYPST1             NO, OK TO POST                               
         BAS   RE,BODDS            BUILD ODDS FILE RECORD                       
         GOTO1 ADMGR,ODDADD        ADD DUMMY RECORD FOR EOD(PACKING)            
         B     EXIT                                                             
*                                                                               
CPYPST1  GOTO1 APOST,PSTTOT        POST THE TOTAL RECORD                        
         GOTO1 ADMGR,WRKCLO        CLOSE THE WORKER FILE                        
*                                                                               
*                                  ADD AGENCY TOTALS TO ODDS FILES              
         MVI   ROW,ROWCPY          GET AGENCY                                   
         MVI   TYPE,TYPBOTH        ONLINE & OFFLINE TOTALS                      
         BAS   RE,GROW                                                          
*                                                                               
         BAS   RE,BODDS            BUILD ODDS RECORD                            
         L     R5,ATWORKL                                                       
         USING ODDRECD,R5                                                       
         ZAP   ODDAMT,TOT          ADD REAL ODDS RECORD                         
         ZAP   ODDCOMO,CONET                                                    
         AP    ODDCOMO,COGST                                                    
         ZAP   ODDCR,CASHR                                                      
         ZAP   DUB,ODDCR                                                        
         MP    DUB,=P'-1'                                                       
         ZAP   ODDCR,DUB                                                        
         GOTO1 ADMGR,ODDADD                                                     
*                                                                               
*                                                                               
CPYPST3  MVI   ROW,ROWCPY          SET AGENCY TOTALS                            
         BAS   RE,CROW             CLEAR TOTALS                                 
         ZAP   COUNTER,=P'0'                                                    
         ZAP   CASHTOT,=P'0'                                                    
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD ODDS FILE RECORD                                              *         
***********************************************************************         
                                                                                
BODDS    NTR1  ,                                                                
         L     R5,ATWORKL          BUILD ODDS RECORD                            
         USING ODDRECD,R5                                                       
         XC    ODDREC(ODDLNQ),ODDREC                                            
         LA    R1,ODDLNQ            SET RECORD LENGTH                           
         STCM  R1,3,ODDLEN                                                      
         MVC   ODDACC,SAVCPY       ACC COMPANY CODE                             
         MVC   ODDID,AGID          CONTROL FILE ID NUMBER                       
         MVC   ODDUTL,PRNTSEN      PRINT SE NUMBER                              
         MVC   ODDAGY,SAVAGY       AGENCY ALPHA                                 
         MVC   ODDSE,ACCTSEN       ACC SE NUMBER                                
         MVC   ODDNAM,AGYNAME      AGENCY NAME                                  
         ZAP   ODDAMT,=P'0'        AMOUNT                                       
         ZAP   ODDCOMO,=P'0'       COMMISSION ONLY                              
         ZAP   ODDCR,=P'0'         CASH RECEIPT                                 
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* POST ON-LINE TOTALS FOR AGENCY                                      *         
***********************************************************************         
                                                                                
PONL     NTR1  ,                                                                
         TM    FLGTOT,FLGIDS       TEST SECOND PASS                             
         BO    EXIT                YES, SKIP WORKER FILES                       
         XC    ONLNKEY,ONLNKEY                                                  
         LA    R5,ONLNKEY                                                       
         USING UKRECD,R5                                                        
         MVC   UKUSRID,AGID                                                     
         MVC   UKSYSPRG,=C'A54'                                                 
         MVI   UKSUBPRG,C'S'                                                    
         MVC   UKDAY,TODAY+2                                                    
         MVI   UKCLASS,C'P'                                                     
         CLI   QOPT5,C'D'          ALLOW DUPLICATE FILES                        
         BNE   *+8                                                              
         MVI   UKFLAG,X'01'                                                     
         GOTO1 ADMGR,ONLOPN        OPEN ONLINE FILE                             
*                                                                               
         L     R5,ATWORKL                                                       
         XC    0(4,R5),0(R5)                                                    
         LA    R1,PSSUBFL+5        SET RECORD LENGTH                            
         STCM  R1,3,0(R5)                                                       
         L     R5,ATWORK                                                        
         USING PSSUBFD,R5                                                       
         MVI   PSSBEL,PSSBELQ      X'52' ELEMENT                                
         MVI   PSSBLEN,PSSUBFL                                                  
         MVC   PSSBDESC,=CL15'ONLINE PRINT CLR'                                 
         ZAP   PSSBRECS,TOT        DR'S                                         
         ZAP   PSSBCASH,TOT        ONLINE TOTAL                                 
         MVI   PSSBCASH+6,0        EOR                                          
         GOTO1 ADMGR,ONLADD        ADD ONLINE TOTAL                             
         GOTO1 ADMGR,ONLCLO        CLOSE ONLINE TOTAL                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* OFFICE CONTROL BREAK                                                *         
***********************************************************************         
         SPACE 1                                                                
OFFTOT   NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         L     R4,AOFFATAB         ASSUME IT IS AGENCY TOTS FOR OFFICE          
         TM    FLGTOT,FLGAGY       ARE WE GOING AGENCY OFFICE TOTALS            
         BO    *+8                                                              
         L     R4,AOFFMTAB         NO - IT IS MEDIA TOTS FOR OFFICE             
         USING BIND,R4                                                          
         ICM   R0,15,BININ                                                      
         BZ    OFFTOTX                                                          
         LA    R2,BINTAB                                                        
         USING OFFD,R2                                                          
         TM    FLGTOT,FLGAGY       ARE WE GOING AGENCY OFFICE TOTALS            
         BZ    *+8                                                              
         MVI   FORCEHED,C'Y'       YES - FORCE NEW PAGE                         
*                                                                               
         MVC   PLINE,SPACES                                                     
         OI    FLGTOT,FLGSUM       SET FOR SUMMARY REPORT                       
         LA    R7,MID1                                                          
         MVC   PANAL,=CL18'ANALYSIS BY OFFICE'                                  
         LA    R7,L'MID1(R7)                                                    
         MVC   PANAL,=CL18'------------------'                                  
         MVI   FORCEMID,C'Y'                                                    
         LA    R7,P                                                             
         BAS   RE,REPRT                                                         
*                                                                               
OFFTOT3  MVC   POFFCDE,OFFCDE      OFFICE CODE                                  
         MVC   POFFNME,OFFNME      OFFICE NAME                                  
         EDIT  (P8,OFFCD),(12,PTOTCD),2,MINUS=YES                               
         EDIT  (P8,OFFCR),(12,PTOTCR),2,MINUS=YES                               
         EDIT  (P8,OFFNET),(12,PTOTNET),2,MINUS=YES                             
         CLI   TAXUL,C' '                                                       
         BNH   OFFTOT5                                                          
         EDIT  (P8,OFFGST),(12,PTOTGST),2,MINUS=YES                             
         EDIT  (P8,OFFPST),(12,PTOTPST),2,MINUS=YES                             
*                                                                               
OFFTOT5  MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
         LA    R2,OFFLNQ(R2)                                                    
         BCT   R0,OFFTOT3                                                       
*                                                                               
OFFTOTX  XC    BININ,BININ         CLEAR TABLE FOR NEXT SET OF ENTRIES          
         B     EXIT                                                             
         DROP  R2,R4,R7                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT END OF RUN TOTALS                                             *         
***********************************************************************         
                                                                                
RUNTOT   NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVI   ROW,ROWRUN                                                       
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW                                                          
         OI    FLGTOT,FLGRUN                                                    
         MVC   AGID,DDST1                                                       
         BAS   RE,REMCK                                                         
         GOTO1 =A(OUTTRNTO),DMCB,AIO2,AGID                                      
                                                                                
         GOTO1 DIVIDE,SLOGO        START LOGO PAGE FOR CONTROL                  
         MVC   AGYNAME,SPACES                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         OI    FLGTOT,FLGSUM       SUMMARY HEADS                                
         MVC   PTOTLINE(10),=C'TOTALS FOR'                                      
         MVC   PTOTLINE+11(6),SYSNUMB                                           
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
         MVC   PTOTLINE(10),=C'RUN TOTALS'                                      
         BAS   RE,EDTOT                                                         
         MVC   PTOTLINE,=CL25'REGULAR LESS CASH RECEIPT'                        
         EDIT  TOT,(12,PTOTNET),2,MINUS=YES                                     
         BAS   RE,REPRT                                                         
         GOTO1 DIVIDE,ELOGO        END PAGE FOR CONTROL                         
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT GST, PST, CD AND NET                                           *         
***********************************************************************         
                                                                                
EDIT     NTR1 ,                                                                 
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         EDIT  GST,(12,PTOTGST),2,MINUS=YES,ZERO=BLANK                          
         EDIT  PST,(12,PTOTPST),2,MINUS=YES,ZERO=BLANK                          
         EDIT  CASHD,(12,PTOTCD),2,MINUS=YES                                    
         EDIT  (P8,TOT),(12,PTOTNET),2,MINUS=YES                                
         CP    LCASHD,=P'0'        ANY LOST C.D.                                
         BE    EXIT                                                             
         LA    R7,L'P(R7)                                                       
         MVC   PTOTLIN2,=CL18'LOST CASH DISCOUNT'                               
         EDIT  LCASHD,(12,PTOTCD),2,MINUS=YES                                   
         B     EXIT                                                             
*                                                                               
EDTOT    NTR1  ,                        MEDIA & AGENCY TOTALS                   
         LA    R7,P                                                             
         ZAP   DUB,TOT             TOTAL + CR                                   
         AP    DUB,CASHR                                                        
         EDIT  (P8,DUB),(12,PTOTNET),2,MINUS=YES                                
         ZAP   DUB,CASHR           CR                                           
         MP    DUB,=P'-1'                                                       
         EDIT  (P8,DUB),(12,PTOTCR),2,MINUS=YES                                 
         EDIT  CASHD,(12,PTOTCD),2,MINUS=YES                                    
         EDIT  GST,(12,PTOTGST),2,MINUS=YES,ZERO=BLANK                          
         EDIT  PST,(12,PTOTPST),2,MINUS=YES,ZERO=BLANK                          
*                                                                               
EDTOT3   MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*  MEDIA NAME ROUTINES                                                *         
***********************************************************************         
                                                                                
MEDNM    DS    0H                                                               
         LA    R1,MEDTAB                                                        
MEDNM1   CLC   0(1,R1),SRTMED      MATCH MEDIA CODE                             
         BE    MEDNM3                                                           
         LA    R1,L'MEDTAB(R1)                                                  
         CLI   0(R1),C' '          DEFAULT                                      
         BNE   MEDNM1                                                           
*                                                                               
MEDNM3   MVC   MEDCODE,0(R1)       SAVE CODE AND NAME                           
         MVC   MEDNAME,2(R1)                                                    
         BER   RE                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                        *         
***********************************************************************         
         SPACE 1                                                                
REPRT    NTR1  ,                                                                
         MVC   HEAD1,SPACES                                                     
         MVC   HEAD2,SPACES                                                     
         LA    RF,HEAD1+40                                                      
         LR    R3,RF                                                            
         LA    R4,L'HEAD1(R3)                                                   
         MVC   0(L'RPTIT,RF),RPTIT      REPORT TITLE                            
         LA    RF,L'RPTIT+2(RF)                                                 
         TM    FLGTOT,FLGRUN            CONTROL TOTALS                          
         BO    REPRT3                                                           
         TM    FLGTOT,FLGIDS            TEST REPORT BY ID                       
         BNO   REPRT3                                                           
         L     R5,LOGOC                                                         
         USING LOGOD,R5                                                         
         MVC   0(L'LOGO1,RF),LOGO1      ADD OFFICE LOGO                         
         LA    RF,L'LOGO1+2(RF)                                                 
         MVC   0(L'LOGO2,RF),LOGO2                                              
*                                                                               
REPRT3   LA    R0,L'RPTIT+L'LOGO1+L'LOGO2+5                                     
         GOTO1 ADSQUASH,DMCB,(R3),(R0)                                          
         ICM   R0,15,4(R1)                                                      
         GOTO1 UNDERLIN,DMCB,((R0),(R3)),(R4)                                   
         LA    R0,HEAD1+107                                                     
         SR    R0,R3                                                            
         GOTO1 CENTER,DMCB,(R3),(R0)                                            
         GOTO1 CENTER,DMCB,(R4),(R0)                                            
*                                                                               
         MVI   RCSUBPRG,5                                                       
         TM    FLGTOT,FLGRUN               CONTROL TOTALS                       
         BO    REPRT5                                                           
*                                                                               
         MVC   HEAD4+08(33),AGYNAME        AGENCY NAME                          
         MVC   HEAD4+110(6),=CL6'MEDIA'                                         
         CLI   PROFIL14,C'Y'               NAME FOR AGY HEADER?                 
         BNE   REPRT4                                                           
         MVC   HEAD4+117(1),MEDCODE                                             
         MVC   HEAD4+119(10),MEDNAME       MEDIA NAME                           
         B     REPRT4X                                                          
*                                                                               
REPRT4   MVC   HEAD4+117(15),MEDNAME      OLD PLACE + LENGTH                    
*                                                                               
REPRT4X  TM    FLGTOT,FLGAGY               TEST AGENCY TOTALS                   
         BZ    *+10                                                             
         MVC   HEAD4+110(25),=CL25'AGENCY TOTALS'                               
*                                                                               
         MVI   RCSUBPRG,2                  DETAILS                              
         CLI   TAXUL,C' '                                                       
         BNH   *+8                                                              
         MVI   RCSUBPRG,3                  DETAILS & GST                        
*                                                                               
         TM    FLGTOT,FLGSUM               TEST SUMMARY                         
         BZ    REPRT5                                                           
*                                                                               
         MVI   RCSUBPRG,4                  SUMMARY                              
         CLI   TAXUL,C' '                                                       
         BNH   *+8                                                              
         MVI   RCSUBPRG,5                  SUMMARY & GST                        
*                                                                               
REPRT5   GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RETRIEVE A ROW OF ACCUMULATORS                           *         
***********************************************************************         
                                                                                
GROW     NTR1  ,                                                                
         LA    RF,CSHCTL           CLEAR CASH CONTROL BLOCK                     
         LHI   R0,CSHC             R0=# OF COLUMNS IN A ROW                     
         ZAP   0(8,RF),=P'0'       CLEAR ROW                                    
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LHI   R5,OFFLINE          OFFLINE OR BOTH                              
         TM    TYPE,TYPOFF                                                      
         BO    GROW3                                                            
         LHI   R5,ONLINE           ONLINE                                       
*                                                                               
GROW3    LR    R1,R5                                                            
         MH    R1,=Y(CSHTLNQ)      X WIDTH OF EACH TABLE                        
         L     RF,AACCUMS          RF=A(ACCUMULATORS)                           
         AR    RF,R1               ADD WIDTH OF TABLE                           
         SR    RE,RE                                                            
         IC    RE,ROW                                                           
         MH    RE,=Y(CSHLNQ)       RE=DISPLACEMENT TO ROW                       
         AR    RF,RE               ADD DISPLACEMENT TO ROW                      
         LA    R3,CSHCTL                                                        
         BAS   RE,GROW9                                                         
         TM    TYPE,TYPBOTH        TEST ADD BOTH                                
         BNO   EXIT                                                             
         LTR   R5,R5               TEST ALREADY ADDED ONLINE                    
         BNZ   EXIT                                                             
         LA    R5,1(R5)                                                         
         B     GROW3                                                            
*                                                                               
GROW9    LHI   R0,CSHC             R0=# OF COLUMNS IN A ROW                     
         AP    0(8,R3),0(8,RF)     ADD TABLE ENTRY TO CASH BLOCK                
         LA    R3,8(R3)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLEAR ROW OF ACCUMULATORS                                *         
***********************************************************************         
                                                                                
CROW     NTR1  ,                                                                
         SR    RE,RE                                                            
         IC    RE,ROW                                                           
         MH    RE,=Y(CSHLNQ)       RE=DISPLACEMENT TO ROW                       
*                                                                               
         L     RF,AACCUMS          RF=A(ACCUMULATORS)                           
         AR    RF,RE               RF=ONLINE ACCUMS                             
         LR    RE,RF                                                            
         AH    RE,=Y(CSHTLNQ)      RE=OFFLINE ACCUMS                            
         LHI   R0,CSHC             R0=NUMBER OF COLUMNS IN TABLE                
*                                                                               
CROW3    ZAP   0(8,RF),=P'0'       CLEAR ROW                                    
         ZAP   0(8,RE),=P'0'                                                    
         LA    RF,8(RF)                                                         
         LA    RE,8(RE)                                                         
         BCT   R0,CROW3                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINES                                                      *         
***********************************************************************         
                                                                                
NOPRNT   NTR1  ,                  CAN'T READ PRINT FILE                         
         AP    NOPRNTF,=P'1'                                                    
         CP    NOPRNTF,=P'10'                                                   
         BL    EXIT                                                             
         GOTO1 LOGIO,DMCB,1,(50,NOPRNTM)                                        
         DC    H'0'                                                             
*                                                                               
NOSTAT   NTR1  ,                   CAN'T READ PUBFILE FILE                      
         AP    NOSTATF,=P'1'                                                    
         CP    NOSTATF,=P'20'                                                   
         BL    EXIT                                                             
         GOTO1 LOGIO,DMCB,1,(50,NOPUBLM)                                        
         DC    H'0'                                                             
*                                                                               
NOPRNTM  DC    CL50'CAN''T READ PRINT FILE - ** RUN ABORTED **'                 
NOPUBLM  DC    CL50'CAN''T READ PUBFILE FILE - ** RUN ABORTED **'               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD AMOUNTS TO OFFICE TABLE                                         *         
***********************************************************************         
         SPACE 1                                                                
ADDOF    NTR1  ,                                                                
         CLI   PROFIL12,C'Y'       INCLUDE OFFICE TABLE?                        
         BNE   EXIT                NO - SKIP                                    
         LA    R2,OFFWRK                                                        
         USING OFFD,R2                                                          
         XC    OFFWRK,OFFWRK                                                    
         MVC   OFFCDE,OFFC         OFFICE CODE                                  
         MVC   OFFNME,SPACES       CLEAR NAME FIELD WITH SPACES                 
*                                                                               
         L     R5,AOFNTAB          R5=A(OFFICE CODE TABLE)                      
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         GOTO1 BINSRCH,DMCB,(X'00',OFFCDE),(R6)                                 
         CLI   DMCB,1              RECORD WAS NOT FOUND                         
         BE    ADDOF3                                                           
*                                                                               
         L     R4,0(R1)            ADDRESS OF FOUND RECORD                      
         USING OFND,R4                                                          
         MVC   OFFNME,OFNNME       PLUG IN OFFICE NAME FROM TABLE               
*                                                                               
ADDOF3   ZAP   OFFCR,CASHR         CASH RECEIPTS                                
         MP    OFFCR,=P'-1'                                                     
         ZAP   OFFGST,GST          GST                                          
         ZAP   OFFPST,PST          PST                                          
         ZAP   OFFCD,CASHD         CASH DISCOUNTS                               
         ZAP   OFFNET,NET          NET AMOUNT                                   
         AP    OFFNET,GST          PLUS GST                                     
         AP    OFFNET,PST          PLUS PST                                     
*                                                                               
         GOTO1 BINADD,DMCB,OFFWRK,AOFFMTAB  ADD TO MEDIA TABLE                  
         GOTO1 BINADD,DMCB,OFFWRK,AOFFATAB  ADD TO AGENCY TABLE                 
         B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* CHECK PROFILE RECORDS FOR REMOTE PRINTING                           *         
***********************************************************************         
                                                                                
REMCK    NTR1  ,                                                                
         MVI   REMOPT,C'N'         ASSUME NOT                                   
         CLI   QOPT1,C'N'          OPTION FOR "NO REMOTE PRINTING"              
         BE    EXIT                                                             
         L     R5,REMOTEC                                                       
         USING REMOTED,R5                                                       
         XC    REMOTKEY,REMOTKEY                                                
*                                                                               
         XC    SVREMOTE,SVREMOTE                                                
         LA    R4,SVREMOTE                                                      
SAVE     USING REMOTED,R4                                                       
         XC    WORK,WORK                                                        
         MVI   WORK,C'A'             SYSTEM CODE IS 'A' ACCOUNTING              
         MVC   WORK+1(2),RCPROG      PROGRAM                                    
         MVC   WORK+3(2),AGID                                                   
         L     RF,AUTL                                                          
         MVC   SVSEN,4(RF)           SAVE SYSTEM UTL                            
         MVI   4(RF),X'0A'           HARD CODE TO READ CONTROL FILE             
         GOTO1 PQPROF,DMCB,(0,WORK),(0,SVREMOTE),ADCOMFAC                       
         L     RF,AUTL                                                          
         MVC   4(1,RF),SVSEN         RESTORE SYSTEM UTL                         
         MVC   REMOTTYP,SAVE.REMOTTYP                                           
         MVC   REMOTARC,SAVE.REMOTARC                                           
         MVC   REMOTTY1,SAVE.REMOTTY1                                           
         DROP  SAVE                                                             
*                                                                               
         L     R2,ACONIO                                                        
         USING CTPREC,R2                                                        
         XC    CTPKEY,CTPKEY                                                    
         XC    OUTTYPE,OUTTYPE                                                  
REMCK10  MVI   CTPKTYP,C'P'                                                     
         MVC   CTPKSYS(3),=C'A54'                                               
         MVC   DKEY,0(R2)                                                       
         GOTO1 ADMGR,CONRD         READ CONTROL FILE                            
         BNE   REMCK60                                                          
         LA    R4,CTPDATA                                                       
*                                                                               
REMCK20  CLI   0(R4),0                                                          
         BE    REMCK50                                                          
         CLI   0(R4),X'42'                                                      
         BNE   REMCK30                                                          
         USING CTOCOD,R4                                                        
         MVC   OUTTYPE,CTOCODE     OUTPUT TYPE REMOTE ETC                       
*                                                                               
REMCK30  CLI   0(R4),X'43'                                                      
         BNE   REMCK40                                                          
         MVC   OUTTYPE,CTOCODE     SERIES 1                                     
*                                                                               
REMCK40  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     REMCK20                                                          
                                                                                
REMCK50  OC    CTPKORIG,CTPKORIG                                                
         BNZ   REMCK60                                                          
         XC    CTPKEY,CTPKEY                                                    
         MVC   CTPKORIG,AGID                                                    
         B     REMCK10                                                          
*                                                                               
REMCK60  CLC   =C'REMOTE',OUTTYPE                                               
         BNE   REMCK70                                                          
         MVI   REMOPT,C'Y'                                                      
         MVI   REMOTFLG,X'FA'                                                   
         MVC   REMOTSYS(3),=C'A54'                                              
         MVC   REMOTJID(3),REMOTSYS                                             
         MVC   REMOTDSC(8),=C'PR CLEAR'                                         
         MVC   REMOTDST(2),AGID                                                 
         MVI   REMOTCLS,C'M'                                                    
         MVC   REMOTADM,DATAMGR                                                 
         B     REMCKX                                                           
REMCK70  CLC   =C'S1',OUTTYPE      DOES WANT LOGOS                              
         BNE   REMCKX                                                           
         MVI   REMOPT,C'N'         DOES WANT LOGOS                              
REMCKX   B     EXIT                                                             
         DROP  R2,R4,R5                                                         
*                                                                               
SVREMOTE DS    CL(REMOTEDL)        SAVED REMOTE BLOCK                           
SVSEN    DS    XL1                                                              
OUTTYPE  DS    CL(L'CTOCODE)                                                    
         EJECT                                                                  
***********************************************************************         
* PRINT LOGOS                                                         *         
***********************************************************************         
                                                                                
DIVIDE   NTR1  ,                                                                
         L     RE,AUTL                                                          
         MVC   4(1,RE),SYSTSEN                                                  
         LTR   R1,R1                                                            
         BZ    DIV200              BRANCH IF START LOGO                         
*                                                                               
DIV100   L     R5,LOGOC           NOT REMOTE                                    
         USING LOGOD,R5                                                         
         MVI   LOGOTYPE,C'E'                                                    
         CLI   LOGOS,C'N'                                                       
         BE    DIV103              DO END LOGO                                  
         GOTO1 LOGO,DMCB,(R5)                                                   
         USING REMOTED,R5                                                       
DIV103   L     R5,REMOTEC                                                       
         OC    REMOTKEY,REMOTKEY   CLOSE PRINT IF LAST WENT REMOTE              
         BZ    EXIT                                                             
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         XC    REMOTKEY,REMOTKEY                                                
         B     EXIT                                                             
*                                                                               
         USING LOGOD,R5                                                         
DIV200   L     R5,LOGOC                                                         
         GOTO1 GETLOGO,DMCB,(1,AGID),(R5),DATAMGR,=C'ACC'                       
         MVI   LOGOTYPE,C'S'                                                    
         MVC   LOGOJOB(4),DMCB                                                  
         MVC   LOGOJOB+4(4),=C'A54P'                                            
         MVC   HEAD1+44(15),SPACES                                              
*                                                                               
         MVI   LOGOS,C'N'                                                       
         TM    CMPSTA8,CPYSRLOG    REMOTE LOGO                                  
         BO    *+12                                                             
         CLI   REMOPT,C'Y'                                                      
         BE    EXIT                NO LOGO FOR REMOTE                           
         MVI   LOGOS,C'Y'                                                       
         GOTO1 LOGO,DMCB,(R5)                                                   
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
BINADD   NTR1  ,                                                                
         L     R5,4(R1)                                                         
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         SR    R6,R6                                                            
         IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
BINA10   AP    0(OFFBKLN,R4),0(OFFBKLN,R3)   ADD TO BUCKET                      
         LA    R3,OFFBKLN(R3)      BUMP TO NEXT ENTRY IN NEW ITEM               
         LA    R4,OFFBKLN(R4)      BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,BINA10                                                        
*                                                                               
BINXIT   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         DROP  RB,R9,R8                                                         
         EJECT                                                                  
***********************************************************************         
* PUBFILE NAME/ADDRESS ROUTINE                                        *         
***********************************************************************         
                                                                                
PUBL     NMOD1 0,PUBL,R9                                                        
         L     RF,4(RD)                                                         
         L     RC,68(RF)           RESTORE RC                                   
*                                                                               
         MVC   PUBPARAM,3(R1)                                                   
*                                                                               
         XC    DKEY,DKEY                                                        
         LA    R4,DKEY                                                          
         USING PUBREC,R4                                                        
         MVC   PUBKMED,SRTMED                                                   
         PACK  PUBKPUB(5),SRTPUB(9)                                             
         MVI   PUBKPUB+4,0                                                      
         CLI   SRTPUB+8,C' '                                                    
         BE    PUBL1                                                            
         GOTO1 HEXIN,DMCB,SRTPUB+8,PUBKPUB+4,2                                  
*                                                                               
PUBL1    MVC   PUBKED,SRTPUB+10                                                 
         CLI   PUBKED,X'40'                                                     
         BNE   *+8                                                              
         MVI   PUBKED,0                                                         
         MVC   PUBKAGY,SRTAGY                                                   
         MVI   PUBKCOD,X'81'                                                    
         CLI   PUBPARAM,2          LOOK FOR BASE PUB                            
         BE    PUBL3                                                            
         CLC   SRTPUB+8(3),=C'ZZZ'                                              
         BE    PUBL3                                                            
         LA    R3,PUBKAGY-PUBREC-1 ZONE & EDITION                               
         B     PUBL5                                                            
*                                                                               
PUBL3    LA    R3,PUBKZON-PUBREC-1  BASE PUB                                    
         XC    PUBKZON(L'PUBKZON+L'PUBKED),PUBKZON                              
*                                                                               
PUBL5    BAS   RE,PUBD             GET PUB DIRECTORY                            
         BAS   RE,PUBR             GET NAME/ADDRESS                             
         B     PUBX                                                             
         EJECT                                                                  
***********************************************************************         
* GET PUB DIRECTORY RECORD                                            *         
***********************************************************************         
                                                                                
PUBD     NTR1  ,                                                                
         GOTO1 ADMGR,PUBHI                                                      
         B     PUBD5                                                            
*                                                                               
PUBD3    GOTO1 ADMGR,PUBSEQ                                                     
PUBD5    EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   DIR(0),DKEY                                                      
         BE    *+6                                                              
         DC    H'0'                PUB NOT FOUND                                
*                                                                               
         LA    R4,DIR                                                           
         CLC   PUBKAGY,SRTAGY                                                   
         BE    PUBD9                                                            
         CLI   AGYPROF+16,C'0'     TEST OK TO USE SRDS DEFAULT                  
         BE    PUBD3               DON'T USE SRDS                               
         CLC   PUBKAGY,=C'ZZ'      TEST DEFAULT                                 
         BNE   PUBD3                                                            
*                                                                               
PUBD9    CLI   PUBKCOD,X'81'       TEST BIG REC                                 
         BNE   PUBD3                                                            
         B     PUBX                                                             
         EJECT                                                                  
***********************************************************************         
* GET PUB RECORD - EXTRACT NAME/ADDRESS                               *         
***********************************************************************         
         SPACE 1                                                                
PUBR     NTR1  ,                                                                
         GOTO1 ADMGR,PUBGET                                                     
         L     R4,AIO1                                                          
         LA    R4,33(,R4)          A(X'10' NAME/ADR ELEMENT)                    
*                                                                               
         USING PUBNAMEL,R4                                                      
         OC    PUBNAME,SPACES      ELEMENT HAS BINARY ZEROS                     
         OC    PUBCITY,SPACES                                                   
         OC    PUBSTATE,SPACES                                                  
         OC    PUBZNAME,SPACES                                                  
         OC    PUBLINE1,SPACES                                                  
         OC    PUBLINE2,SPACES                                                  
*                                                                               
         MVI   COUNTRY,C'U'        USA                                          
         CLC   PUBSTACD,=C'90'                                                  
         BNE   *+8                                                              
         MVI   COUNTRY,C'C'        CANADIAN                                     
*                                                                               
         MVC   PUBNM,PUBNAME       NAME                                         
         MVC   PUBNML,SPACES       LONG NAME (CITY/PUB)                         
         MVC   PUBNML(20),PUBNM                                                 
         MVC   PUBST,PUBSTATE      STATE                                        
         MVC   ZONE,PUBZNAME       ZONE                                         
*                                                                               
         CLI   SRTMED,C'N'         THATS ENOUGH FOR NON-NEWSPAPERS              
         BNE   *+16                                                             
         CLI   PROFIL13,C'N'       GET CITY NAME FOR NEWSPAPER UNLESS           
         BE    *+8                 PROFILE SET NOT TO INCLUDE CITY              
         BAS   RE,GETCITY          BEFORE THE NAME                              
*                                                                               
         CLI   PUBPARAM,3          EXIT IF WE ONLY WANT NAME                    
         BE    PUBX                                                             
*                                                                               
         MVC   PUBAD,SPACES        NOW BUILD ADDRESS                            
         MVC   PUBAD+1(20),PUBZNAME   LINE 1                                    
         SR    R6,R6               LINE COUNTER                                 
         LA    R5,PUBAD+1                                                       
         CLI   0(R5),C' '                                                       
         BE    *+12                NO ZONE                                      
         LA    R5,26(,R5)                                                       
         LA    R6,1(,R6)                                                        
*                                                                               
         GOTO1 CHOPPER,DMCB,(0,PUBLINE1),(26,0(R5)),2,C'DLM=',30,DELIML         
         A     R6,DMCB+8           LINES USED                                   
         CLI   0(R5),C' '                                                       
         BE    PUBR10                                                           
         LA    R5,26(,R5)                                                       
         CLI   0(R5),C' '                                                       
         BE    *+8                                                              
         LA    R5,26(,R5)                                                       
*                                                                               
PUBR10   GOTO1 (RF),(R1),(0,PUBLINE2),(26,0(R5)),2,C'DLM=',30,DELIML            
         A     R6,DMCB+8           LINES USED                                   
         STC   R6,PUBAD                                                         
         C     R6,=F'4'                                                         
         BNH   PUBR20                                                           
*                                  MORE THAN 4 LINES                            
*                                  STRING PUB LINES 1 AND 2                     
*                                  TOGETHER AND TRY TO CHOP THE WHOLE           
*                                  INTO THREE LINES                             
         MVC   PUBAD+27(78),SPACES                                              
         MVI   WORKA,C' '                                                       
         MVC   WORKA+1(L'WORKA-1),WORKA                                         
         MVC   WORKA+30(30),PUBLINE1                                            
         MVC   WORKA+62(30),PUBLINE2                                            
         GOTO1 (RF),(R1),(0,WORKA+30),(26,PUBAD+27),4,C'DLM=',62,DELIML         
         MVI   PUBAD,4                                                          
         CLI   DMCB+11,3                                                        
         BNH   PUBR20                                                           
*                                  STILL NO GOOD - GIVE UP AND                  
*                                  JUST MOVE THE LINES IN                       
         MVC   PUBAD+27(78),SPACES                                              
         MVC   PUBAD+27(30),PUBLINE1                                            
         MVC   PUBAD+62(30),PUBLINE2                                            
*                                                                               
PUBR20   BAS   RE,GETADR           INTERFACE TO PGETADR                         
         B     PUBX                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET CITY ROUTINE                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING PUBNAMEL,R4                                                      
GETCITY  LR    R0,RE                                                            
         LA    R6,PUBCITY+(L'PUBCITY-1)   GET LENGTH                            
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
*                                                                               
         LA    R5,PUBCITY          CHECK FOR CITY NAME DUPLICATION              
         SR    R6,R5                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   PUBNAME(0),PUBCITY                                               
         BE    GETCITYX                                                         
         MVC   PUBNML(16),PUBCITY                                               
         MVI   PUBNML+16,C' '                                                   
         MVC   PUBNML+17(20),PUBNAME                                            
         LA    R6,L'PUBNML                                                      
         GOTO1 ADSQUASH,DMCB,PUBNML,(R6)                                        
GETCITYX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO PPGETADR                                               *         
***********************************************************************         
                                                                                
GETADR   NTR1  ,                                                                
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         USING GETPD,R3                                                         
         MVC   GETPAGY,SRTAGY      AGENCY                                       
         MVC   GETPMED,SRTMED      MEDIA                                        
         MVC   GETPCLI,SRTCLI      CLIENT                                       
         MVC   GETPOFC,POFC        OFFICE CODE                                  
         L     R4,AIO1                                                          
         GOTO1 PPGETADR,DMCB,(C'P',(R3)),(R4),DATAMGR,0                         
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                SOMETHING IS WRONG                           
*                                                                               
         CLI   0(R1),0             ANY ADDRESS  ?                               
         BE    PUBX                NO OVERRIDE, SO FINISHED                     
*                                                                               
         USING PGADREC,R4                                                       
         L     R4,4(,R1)           R4=A(OUTPUT BLOCK)                           
         CLC   1(L'GETPAGM,R1),FFS  TEST DEFAULT AGENCY/MEDIA                   
         BE    GETADR3                                                          
         MVI   OFFPAYR,C'*'                                                     
         MVC   OFFPAYR+1(2),OFFC      SET PAY ADDRESS BY OFFICE                 
*                                                                               
GETADR3  GOTO1 ADSQUASH,DMCB,PGADNAME,30                                        
         GOTO1 ADSQUASH,DMCB,PGADLIN1,30                                        
         GOTO1 ADSQUASH,DMCB,PGADLIN2,30                                        
         MVC   PUBNML,SPACES                                                    
         MVC   PUBNML(30),PGADNAME                                              
         MVC   PUBNM,PUBNML                                                     
         MVC   PUBAD,SPACES                                                     
         MVI   PUBAD,3                                                          
         CLC   PGADLIN1+26(4),SPACES                                            
         BE    GETADR5                                                          
         GOTO1 CHOPPER,DMCB,(0,PGADLIN1),(26,PUBAD+1),2,C'DLM=',30,    *        
               DELIML                                                           
         MVC   PUBAD+53(26),PGADLIN2                                            
         MVI   PUBAD,4                                                          
         B     GETADR7                                                          
*                                                                               
GETADR5  CLC   PGADLIN2+26(4),SPACES                                            
         BE    GETADR9                                                          
         MVC   PUBAD+1(26),PGADLIN1                                             
         GOTO1 CHOPPER,DMCB,(0,PGADLIN2),(26,PUBAD+27),2,C'DLM=',30,   *        
               DELIML                                                           
         MVI   PUBAD,4                                                          
*                                                                               
GETADR7  MVC   PUBAD+79(20),PGADATTN                                            
         B     PUBX                                                             
*                                                                               
GETADR9  MVC   PUBAD+1(26),PGADLIN1     BOTH 1&2 ARE SHORT                      
         MVC   PUBAD+27(26),PGADLIN2                                            
         MVC   PUBAD+53(20),PGADATTN                                            
*                                                                               
PUBX     XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REP NAME/ADDRESS ROUTINE                                            *         
***********************************************************************         
                                                                                
REPL     NMOD1 0,REPL,R9                                                        
         L     RF,4(RD)                                                         
         L     RC,68(RF)           RESTORE RC                                   
*                                                                               
*****    CLC   REPNAME,SPACES                                                   
*****    BNE   REPX                                                             
         MVI   COUNTRY,C'U'        USA                                          
         XC    DKEY,DKEY                                                        
*                                                                               
         LA    R4,DKEY                                                          
         USING PREPREC,R4                                                       
         MVC   PREPKAGY,SRTAGY     AGENCY                                       
         MVC   PREPKMED,SRTMED     MEDIA                                        
         MVC   PREPKREP,SRTREP     REP                                          
         MVI   PREPKRCD,X'11'                                                   
         GOTO1 ADMGR,PRTHI                                                      
         CLC   DIR(PREPLEN-PREPREC),DKEY                                        
         BE    REPL1                                                            
*                                                                               
         MVC   PREPKAGY,=C'ZZ'     COMMON REPS                                  
         GOTO1 ADMGR,PRTHI                                                      
         CLC   DIR(PREPLEN-PREPREC),DKEY                                        
         BE    REPL1                                                            
         DC    H'0'                                                             
*                                                                               
REPL1    GOTO1 ADMGR,PRTGET                                                     
         L     R4,AIO1                                                          
         OC    PREPNAME,SPACES                                                  
         OC    PREPLIN1,SPACES                                                  
         OC    PREPLIN2,SPACES                                                  
         OC    PREPATTN,SPACES                                                  
*                                                                               
         MVC   REPNAME,PREPNAME                                                 
         CLC   PREPSTAC,=C'90'                                                  
         BNE   *+8                                                              
         MVI   COUNTRY,C'C'        CANADIAN                                     
*                                                                               
         MVC   PUBAD,SPACES        NOW BUILD ADDRESS                            
         MVI   PUBAD,3             ASSUME 3 LINES                               
         CLC   PREPLIN1+26(4),SPACES                                            
         BE    REPL2                                                            
         GOTO1 CHOPPER,DMCB,(0,PREPLIN1),(26,PUBAD+01),2,C'DLM=',30,   *        
               DELIML                                                           
         MVC   PUBAD+53(26),PREPLIN2     CANT HELP LINE2 IF 1 IS BIG            
         MVI   PUBAD,4                                                          
         B     REPL3                                                            
*                                                                               
REPL2    CLC   PREPLIN2+26(4),SPACES                                            
         BE    REPL4                                                            
         MVC   PUBAD+01(26),PREPLIN1                                            
         GOTO1 CHOPPER,DMCB,(0,PREPLIN2),(26,PUBAD+27),2,C'DLM=',30,   *        
               DELIML                                                           
         MVI   PUBAD,4                                                          
*                                                                               
REPL3    MVC   PUBAD+79(20),PREPATTN                                            
         B     REPX                                                             
*                                                                               
REPL4    MVC   PUBAD+01(26),PREPLIN1  IF 1 AND 2 ARE BOTH SHORT                 
         MVC   PUBAD+27(26),PREPLIN2  WE ARE OK WITH 3 LINES                    
         MVC   PUBAD+53(20),PREPATTN                                            
*                                                                               
REPX     XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD POSTING FILE RECORD (CREDITS ONLY)                            *         
***********************************************************************         
                                                                                
POST     NMOD1 0,*POST*,R9,R8                                                   
         L     RF,4(RD)                                                         
         L     RC,68(RF)           RESTORE RC                                   
*                                                                               
         STC   R1,POSTC            SAVE POSTING CONTROL                         
         CLI   POSTC,PSTMED        MEDIA TOTAL                                  
         BE    PMEDC               POST MEDIA CONTROL DEBIT TO SZ               
         CLI   POSTC,PSTTOT        MEDIA TOTAL                                  
         BE    PTOTAL              POSTING TOTAL RECORD                         
*                                                                               
         L     R5,ATWORK           POST DETAIL ITEM                             
         USING PSHEADD,R5                                                       
         MVI   PSHDEL,PSHDELQ      POSTING HEADER                               
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC(1),SRTCPY                                                
         MVC   PSHDACC+1(2),=C'SP' ASSUME US                                    
*****    CLI   COUNTRY,C'C'        CANADIAN IS LEDGER Q                         
*****    BNE   *+8                                                              
*****    MVI   PSHDACC+2,C'Q'                                                   
         CLC   SRTAGY,=C'YN'       CLEAR ACROSS FILES?                          
         BNE   POSTD3                                                           
         CLC   SRTOAGY,=C'H7'      FROM YN TO H7?                               
         BNE   POSTD3                                                           
         MVI   PSHDACC+2,C'Q'      FAKE IT TO CANADIAN LEDGER                   
POSTD3   MVC   PSHDACC+3(1),SRTMED                                              
         MVC   PSHDACC+4(11),SPACES                                             
*                                                                               
         CLI   SRTMED,C'O'                                                      
         BNE   POSTD7                                                           
         CLC   SRTAGY,=C'MX'       HARD CODE FOR OUTDOOR                        
         BE    *+14                                                             
         CLC   SRTAGY,=C'CX'                                                    
         BNE   POSTD7                                                           
         MVI   PSHDACC+3,C'I'                                                   
*                                                                               
POSTD7   MVC   PSHDANAL(53),SPACES                                              
         MVC   PSHDSBAC+12(3),SRTCLI                                            
         XC    OFFPAYR,OFFPAYR     CLEAR PAY BY OFFICE                          
         TM    ITEMS,ITMREP          TEST REP                                   
         BNO   *+14                                                             
         MVC   PSHDACC+4(4),SRTREP   REP IS PAYEE                               
         B     POSTD11                                                          
*                                                                               
         CLI   AGYPROF+13,C'1'                                                  
         BE    POSTD9                                                           
         GOTO1 APUBL,DMCB,1             PUB-ONLY                                
         MVC   PSHDACC+4(11),SRTPUB                                             
         CLC   SRTPUB+8(3),=C'ZZZ'     'ALL' MUST SPACE OUT ZONE/EDITN          
         BNE   POSTD19                                                          
         MVC   PSHDACC+12(3),SPACES                                             
         B     POSTD19                                                          
*                                                                               
POSTD9   MVC   PSHDACC+4(8),SRTPUB  COMBINED PUBS                               
*                                                                               
POSTD11  GOTO1 APUBL,DMCB,3          FOR NAME/STATE ONLY                        
         MVC   PSHDSBAC(1),SRTMED                                               
         MVC   PSHDSBAC+1(11),SRTPUB                                            
         CLI   SRTMED,C'N'                                                      
         BE    POSTD13                                                          
         MVC   PSHDSBNM,SPACES                                                  
         MVC   WORK(20),PUBNML     USE ZONE LINE FOR NON-NEWSPAPERS             
         MVI   WORK+20,C' '                                                     
         MVC   WORK+21(20),ZONE                                                 
         GOTO1 ADSQUASH,DMCB,WORK,41                                            
         L     R4,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,(0,WORK),(36,PSHDSBNM),1,C'DLM=',(R4),     *        
               DELIML                                                           
         B     POSTD15                                                          
*                                                                               
POSTD13  MVC   PSHDSBNM,PUBNML                                                  
         CLC   PSHDSBNM+32(4),SPACES                                            
         BNE   POSTD15                                                          
         LA    R1,PSHDSBNM+35      PUT IN PUB STATE FOR REPS                    
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(2,R1),PUBST                                                    
*                                                                               
POSTD15  TM    ITEMS,ITMREP          TEST REP                                   
         BNO   POSTD17                                                          
         GOTO1 AREPL                                                            
         B     POSTD19                                                          
*                                                                               
POSTD17  GOTO1 APUBL,DMCB,2                                                     
         B     POSTD21                                                          
*                                                                               
POSTD19  OC    OFFPAYR,OFFPAYR     OFFICE CODE FOR SPECIAL PAY ADDRS            
         BZ    POSTD21                                                          
         MVC   PSHDACC+12(3),OFFPAYR                                            
*                                                                               
POSTD21  CLI   COUNTRY,C'C'        CANADIAN IS LEDGER Q                         
         BNE   *+8                                                              
         MVI   PSHDACC+2,C'Q'                                                   
         TM    CMPSTA4,CPYSOFF2    IF ON NEW OFFICE SKIP OVERRIDE               
         BO    POSTD23                                                          
         CLI   SRTOFF,C' '                                                      
         BNH   *+10                                                             
         MVC   OFFC,SRTOFF                                                      
*                                                                               
         USING MIOELD,R4                                                        
POSTD23  L     R4,AMIBUFF          R4=BUFFER OF MI ELEMENTS                     
         XC    MIELM,MIELM                                                      
POSTD25  CLI   0(R4),X'FF'         END-OF-BUFFER                                
         BE    POSTD29             NO MATCH MEDIA RECORD                        
         CLC   MIOOFF,SPACES       ANY OFFICE ATTACHED?                         
         BE    *+14                                                             
         CLC   MIOOFF,OFFC         MATCH ON OFFICE?                             
         BNE   POSTD26                                                          
         CLC   MIOCODE,PSHDACC+2   MATCH SYSTEM/MEDIA                           
         BE    POSTD27                                                          
POSTD26  LA    R4,MIOLNQ(R4)                                                    
         B     POSTD25                                                          
*                                                                               
POSTD27  MVC   MIELM,MIOEL        SAVE MI ELEMENT FOR THIS MEDIA                
         B     POSTD26                                                          
*                                                                               
POSTD29  L     R6,ATWORKT                                                       
         USING TRNELD,R6                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
         MVI   TRNSUB,0                                                         
         MVC   TRNDATE,SRTPDATE        POSTING DATE                             
         MVC   TRNREF(3),SRTPROD        REFERENCE NUMBER                        
         MVC   TRNREF+3(3),SRTINV                                               
         MVI   TRNSTAT,0                                                        
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNBTCH(2),MOS           BATCH                                   
         MVC   TRNOFFC,OFFC             OFFICE                                  
         TM    CMPSTA4,CPYSOFF2                                                 
         BO    POSTD31                                                          
         CLI   SRTOFF,C' '                                                      
         BNH   *+10                                                             
         MVC   TRNOFFC,SRTOFF                                                   
*                                                                               
POSTD31  OC    TRNOFFC,SPACES                                                   
         MVC   LASTOFF,TRNOFFC                                                  
         MVI   TRNNARR,C' '                                                     
         ZAP   TRNAMNT,TOT       PAYABLE IS NET + GST & PST                     
         MVI   TRNTYPE,X'31'     TRANSACTION TYPE - PUB                         
         TM    ITEMS,ITMREP      TEST REP                                       
         BNO   *+8                                                              
         MVI   TRNTYPE,X'32'     TRANSACTION TYPE - REP                         
*                                                                               
         SR    R2,R2             L'44 ELEMENT IF NO NARRATIVE                   
         IC    R2,TRNLN                                                         
         SR    R1,R1                                                            
         CLI   SRTCOMM,0         GET TRANSACTION NARRATIVE                      
         BE    POSTD33                                                          
         CLI   SRTCOMM,5                                                        
         BH    POSTD33                                                          
         LLC   R1,SRTNARL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRNNARR(0),SRTNARR                                               
         AR    R2,R1                                                            
*                                                                               
POSTD33  STC   R2,TRNLN          LENGTH OF ELEMENT WITH NARRATIVE               
         CLI   SRTTYPE,SRTTCHK   DIFFERENT POSTING FOR CASH RECEIPT             
         BNE   POSTD35                                                          
         BAS   RE,PCASH            POST CASH RECEIPT                            
         B     POSTD37                                                          
*                                                                               
POSTD35  GOTO1 CHKACT,PSHDACC      CHECK ACCT TO SEE WHICH ELMS TO ADD          
         BAS   RE,PSTAT            POST PUBFILE/REP                             
*                                                                               
POSTD37  L     R6,ATWORKT                                                       
         CP    GST,=P'0'                                                        
         BE    POSTD39                                                          
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BNE   *-12                                                             
         USING SCIELD,R6                                                        
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        X'50' ELEMENT                                
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTAXP    GST TAX - TYPE 'T'                           
         ZAP   SCIAMNT,GST         GST AMOUNT                                   
         ZAP   SCIBASE,NET                                                      
         AP    SCIBASE,CASHD       BASIS = NET + CD                             
         MVC   SCISUBTY,SPACES     SUB TYPE                                     
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
*                                                                               
POSTD39  CP    PST,=P'0'           ANY PST PRESENT AT ALL ?                     
         BE    POSTD43             NO                                           
         MVC   BYTE,SRTGIND        SAVE OFF INDICATOR BYTE                      
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0             AT END YET ?                                 
         BNE   *-12                NO                                           
*                                                                               
         USING SCIELD,R6                                                        
         LA    RE,SRTPST           POINT TO BLOCK OF PST DATA                   
         LA    RF,L'SRTPST/SRTPSTLQ                                             
*                                                                               
         USING SRTPST,RE                                                        
POSTD41  CLI   SRTPSTYP,C' '       ANY PST TYPE ?                               
         BE    POSTD43             NO, FINISHED                                 
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        X'50' ELEMENT                                
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTQST    PST TAX - TYPE 'Q'                           
         TM    BYTE,X'20'          IS PST RUNNING AS BINARY?                    
         BO    *+14                                                             
         ZAP   DUB,SRTPSTAM                                                     
         B     *+12                                                             
         ICM   R1,15,SRTPSTAM                                                   
         CVD   R1,DUB                                                           
         ZAP   SCIAMNT,DUB                                                      
         TM    BYTE,X'40'          ARE WE RUNNING AS BINARY?                    
         BNO   POSTD42                                                          
         ICM   R1,15,SRTPSTBS                                                   
         CVD   R1,DUB                                                           
         ZAP   SCIBASE,DUB         BASIS                                        
         B     *+10                                                             
POSTD42  ZAP   SCIBASE,SRTPSTBS    BASIS                                        
         MVC   SCISUBTY,SPACES     SUB TYPE                                     
         MVC   SCISUBPR,SRTPSTPR   PROVINCE CODE                                
         MVC   SCISUBPT,SRTPSTYP   PRIVINCE TAX TYPE                            
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
         LA    RE,SRTPSTLQ(RE)     BUMP TO NEXT PST ENTRY                       
         BCT   RF,POSTD41          LOOP TO BUILD ANOTHER X'50'                  
         DROP  RE                                                               
*                                                                               
         USING SCIELD,R6                                                        
POSTD43  CLC   SRTPGRS,SPACES                                                   
         BE    POSTD44                                                          
         CP    SRTPGRS,=P'0'                                                    
         BE    POSTD44                                                          
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0             AT END YET ?                                 
         BNE   *-12                NO                                           
*                                                                               
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        X'50' ELEMENT                                
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITGRSS    GROSS                                        
         ZAP   SCIAMNT,SRTPGRS                                                  
         ZAP   SCINET,=P'0'                                                     
         TM    SRTGIND,X'80'       SET X'80' IF NEEDED                          
         BZ    *+8                                                              
         OI    SCIINDR,SCIICAL                                                  
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
*                                                                               
         USING PIDEL,R6                                                         
POSTD44  OC    SRTPID,SRTPID                                                    
         BZ    POSTD45                                                          
         CLC   SRTPID,SPACES                                                    
         BE    POSTD45                                                          
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0             AT END YET ?                                 
         BNE   *-12                NO                                           
*                                                                               
         XC    PIDEL(PIDLNQ),PIDEL                                              
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,SRTPID                                                     
         MVC   PIDAGY,SRTAGY                                                    
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0                                                          
*                                                                               
POSTD45  BAS   RE,PUTAPE                                                        
         USING PSHEADD,R5                                                       
         L     R5,ATWORK                                                        
         MVC   PAYACCT,PSHDACC     SAVE PAYABLE ACCOUNT CODE                    
         CP    GST,=P'0'                                                        
         BE    *+8                                                              
         BAS   RE,PGST             POST THE GST                                 
*                                                                               
         CP    PST,=P'0'                                                        
         BE    POSTX                                                            
         LA    R0,L'SRTPST/SRTPSTLQ     NUMBER OF POSSIBLE ENTRIES              
         LA    R1,SRTPST                                                        
*                                                                               
         USING SRTPST,R1                                                        
POSTD46  CLI   SRTPSTYP,C' '       ANY PST ENTRY ?                              
         BNH   POSTX               NO, SO FINISHED HERE                         
         GOTO1 PPSTT               POST THE PST PASS R1 (PST ENTRY)             
         LA    R1,SRTPSTLQ(R1)     NEXT ENTRY                                   
         BCT   R0,POSTD46          TRY AGIAN                                    
*                                                                               
POSTX    XIT1                                                                   
         DROP  R1,R4,R5,R6                                                      
         EJECT                                                                  
***********************************************************************         
* CHECK ACCOUNT STATUS ELEMENT ON WHAT ELEMENTS TO ADD                *         
*       INITIALIZE ACTFLAG TO X'00' AS DEFAULT SO ALL ELMS ARE ADDED  *         
*       READ ACCOUNTS RSTELD (30) ELEMENT AND TURN ON BITS ACCORDINGLY*         
*       OF WHICH ELMS TO NOT ADD                                      *         
***********************************************************************         
         SPACE 1                                                                
CHKACT   NTR1                                                                   
         MVI   ACTFLAG,0           CLEAR ACCOUNT FLAG                           
         MVC   SVCULA,0(R1)                                                     
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(3),SVCULA      READ LEDGER                                  
         GOTO1 ADMGR,ACCRD                                                      
         BNE   CHKAX                                                            
*                                                                               
         GOTO1 ADMGR,ACCGET                                                     
*                                                                               
         USING LDGRECD,R2                                                       
         L     R2,AIO1                                                          
         LA    R3,LDGRFST          POINT TO FIRST ELEMENT                       
CHKA10   CLI   0(R3),0             END OF REC?                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),ACLELQ        X'16' - ACCOUNTS LENGTHS ELEM                
         BE    CHKA20                                                           
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CHKA10                                                           
*                                                                               
         USING ACLELD,R3                                                        
CHKA20   LA    R0,4                ASSUME 4 LEVELS                              
         SR    R1,R1                                                            
         LA    RE,LEVELS                                                        
         LA    RF,ACLVALS                                                       
CHKA30   ICM   R1,1,0(RF)                                                       
         BZ    CHKA40                                                           
         STC   R1,0(RE)                                                         
         LA    RF,L'ACLVALS(RF)                                                 
         LA    RE,1(RE)                                                         
         BCT   R0,CHKA30                                                        
         DROP  R3                                                               
*                                                                               
CHKA40   SR    R1,R1                                                            
         LA    R0,4                ASSUME 4 LEVELS                              
         LA    RE,LEVELS                                                        
         ST    RE,FULL                                                          
CHKA50   L     RE,FULL                                                          
         ICM   R1,1,0(RE)                                                       
         BZ    CHKAX                                                            
         AHI   R1,2                ADD C/U/L AND THEN -1 FOR EXMVC              
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(0),SVCULA      READ LEDGER                                  
         EX    R1,*-6                                                           
         GOTO1 ADMGR,ACCRD                                                      
         BNE   CHKAX                                                            
*                                                                               
         GOTO1 ADMGR,ACCGET                                                     
*                                                                               
         USING ACTRECD,R2                                                       
         L     R2,AIO1                                                          
         LA    R3,ACTRFST          POINT TO FIRST ELEMENT                       
CHKA60   CLI   0(R3),0             END OF REC?                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),RSTELQ        X'30' - RECORD STATUS ELEM                   
         BE    CHKA70                                                           
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CHKA60                                                           
*                                                                               
         USING RSTELD,R3                                                        
CHKA70   DS    0H                                                               
         CLI   RSTLN,RSTLN3Q                                                    
         BL    CHKA80                                                           
         TM    RSTSTAT6,RSTLNAM+RSTLADDR     SOMETHING TO PREVENT?              
         BZ    CHKA80                                                           
         MVI   ACTFLAG,0                                                        
         TM    RSTSTAT6,RSTLNAM    SHOULD WE ADD THE NAME ELM                   
         BNO   *+8                                                              
         OI    ACTFLAG,ACT20                                                    
         TM    RSTSTAT6,RSTLADDR   SHOULD WE ADD THE ADDR ELM                   
         BNO   *+8                                                              
         OI    ACTFLAG,ACT22                                                    
         DROP  R3                                                               
*                                                                               
CHKA80   L     RE,FULL                                                          
         LA    RE,1(RE)                                                         
         ST    RE,FULL                                                          
         BCT   R0,CHKA50                                                        
*                                                                               
CHKAX    B     POSTX                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* POSTINGS FOR PUBFILE/REP                                            *         
***********************************************************************         
                                                                                
         USING TRNELD,R6                                                        
PSTAT    NTR1  ,                                                                
         SR    R2,R2                                                            
         IC    R2,TRNLN          ADD CASH DISCOUNT FOR REP&STA                  
         AR    R6,R2                                                            
*                                                                               
         USING XPYELD,R6                                                        
         XC    XPYEL(XPYLN3Q),XPYEL                                             
         MVI   XPYEL,XPYELQ        EXTRA PAYMENT ELEMENT                        
         MVI   XPYLN,XPYLN2Q                                                    
         ZAP   XPYCD,CASHD         CASH DISCOUNT                                
         MVC   XPYCLI,CLINAME                                                   
         MVC   XPYPRO,PRDNAME                                                   
         MVC   XPYAGY,SRTAGY       MEDIA AGENCY                                 
         MVC   XPYSEQ,SRTSEQN      SEQUENCE NUMBER                              
         MVC   XPYDATE,SRTINVDC                                                 
*                                                                               
         MVC   XPYINV,SPACES                                                    
         MVC   XPYINV(11),SRTINV                                                
         XC    XPYPER,XPYPER                                                    
         MVC   XPYPER(6),SRTSTRT                                                
         CLI   SRTEND+2,X'40'                                                   
         BE    *+10                                                             
         MVC   XPYPER+6(6),SRTEND                                               
         MVC   XPYTYPE,SRTTYPE                                                  
         XC    XPYEST,XPYEST                                                    
         CLC   SRTESTN,SPACES                                                   
         BE    *+10                                                             
         MVC   XPYEST,SRTESTN                                                   
*                                                                               
         CLC   SRTBLDTE,SPACES     ANY BILLABLE DATE INFO?                      
         BNH   PSTAT3                                                           
         GOTO1 DATCON,DMCB,(3,SRTBLDTE),(1,XPYBLDTE)                            
         MVI   XPYLN,XPYLN3Q       SET BIGGER LENGTH                            
*                                                                               
PSTAT3   IC    R2,XPYLN                                                         
         AR    R6,R2                                                            
*                                                                               
         USING GDAELD,R6           GENERAL DATE ELEMENT                         
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATCLR                                                  
****     MVC   GDADATE,SRTPDATE                                                 
         MVC   GDADATE,TODAY                                                    
         SR    R2,R2                                                            
         IC    R2,1(,R6)                                                        
         AR    R6,R2                                                            
*                                                                               
         XC    GDAEL(GDALNQ),GDAEL                                              
         MVI   GDAEL,GDAELQ        ADD MEDIA MOS                                
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDAMMOS     MMOS TYPE                                    
         LA    R4,SRTSTRT          START DATE (DEFAULT)                         
         CLI   SRTEND+2,X'40'      ANY END DATE ?                               
         BNH   *+8                 NO                                           
         LA    R4,SRTEND           YES, SO USE THAT INSTEAD                     
         GOTO1 DATCON,DMCB,(0,(R4)),(1,GDAYYMM)                                 
         MVI   GDAYYMM+L'GDAYYMM,0             CLEAR DAY PART                   
         SR    R2,R2                                                            
         IC    R2,1(,R6)                                                        
         AR    R6,R2                                                            
*                                                                               
         MVC   PAYNAME,SPACES                                                   
         MVC   PAYNAME,PUBNML                                                   
         TM    ITEMS,ITMREP                                                     
         BNO   *+10                                                             
         MVC   PAYNAME(L'REPNAME),REPNAME                                       
*                                                                               
         TM    ACTFLAG,ACT20       IS THE NAMELD LOCKED?                        
         BO    PSTAT8                                                           
         USING NAMELD,R6                                                        
         MVI   NAMEL,NAMELQ         PUBFILE/REP NAME                            
         MVI   NAMLN,NAMEREC-NAMELD+L'NAMEREC                                   
         MVC   NAMEREC(36),PUBNML                                               
         TM    ITEMS,ITMREP                                                     
         BNO   PSTAT7                                                           
         MVI   NAMLN,L'REPNAME+2                                                
         MVC   NAMEREC(L'REPNAME),REPNAME                                       
*                                                                               
PSTAT7   IC    R2,NAMLN                                                         
         AR    R6,R2                                                            
         MVI   0(R6),0                                                          
*                                                                               
PSTAT8   TM    ACTFLAG,ACT22       IS THE ADRELD LOCKED?                        
         BO    PSTAT15                                                          
         BAS   RE,CHKPAD           FIX ADDRESS IN PUBAD IF NEEDED               
*                                                                               
         USING ADRELD,R6           ADDRESS ELEMENT                              
         CLI   PUBAD,0             TEST ANY ADDRESS                             
         BE    PSTAT15                                                          
         MVI   ADREL,ADRELQ                                                     
         MVC   ADRNUM,PUBAD        NO OF LINES (WORKED OUT BY SUBRTN)           
         LA    R4,PUBAD+1                                                       
         MVC   ADRNUM,PUBAD        PUBFILE ADDRESS                              
         LA    R4,PUBAD+1                                                       
*                                                                               
         IC    R2,ADRNUM                                                        
         MVI   ADRLN,3             IN CASE WE FIND NO ADDRESS                   
         MVI   ADRLN+2,0                                                        
*                                                                               
         LA    R5,ADRADD1                                                       
PSTAT11  MVC   0(26,R5),0(R4)                                                   
         OC    0(26,R5),SPACES                                                  
         LA    R4,26(R4)                                                        
         LA    R5,26(R5)                                                        
         BCT   R2,PSTAT11                                                       
*                                                                               
PSTAT13  IC    R2,ADRNUM                                                        
         MHI   R2,26                                                            
         AHI   R2,3                                                             
         STC   R2,ADRLN                                                         
         AR    R6,R2                                                            
*                                                                               
         USING RSTELD,R6                                                        
PSTAT15  XC    RSTEL(RSTLN3Q),RSTEL                                             
         MVI   RSTEL,RSTELQ          STATUS ELEMENT                             
         MVI   RSTLN,RSTLN3Q                                                    
         IC    R2,RSTLN                                                         
         AR    R6,R2                                                            
         MVI   0(R6),0                                                          
         B     POSTX                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK PUBAD AND FIX IN NEEDED                                       *         
***********************************************************************         
         SPACE 1                                                                
CHKPAD   NTR1                                                                   
         LA    R5,PUBAD+1                                                       
         LA    R0,3                4 ADR LINES BUT ONLY 3 CHANGEABLE            
CHKP10   STC   R0,BYTE                                                          
         CLC   0(26,R5),SPACES     ANY SIGNIFICANT DATA?                        
         BNH   CHKP70                                                           
         LA    RE,26(R5)                                                        
         CLI   0(RE),C'1'          ONLY CONCERNED WITH SPLIT ZIPS SO IF         
         BL    CHKP70              1ST BYTE OF NEXT LINE ISN'T A #-SKIP         
         LA    R3,25(R5)           LAST BYTE OF CURRENT LINE                    
         LA    R0,26                                                            
         LA    RE,CHKP20                                                        
CHKP20   CLI   0(R3),C' '          DID WE FIND A SIGNIFICANT CHAR?              
         BE    CHKP40                                                           
         CLI   0(R3),C'-'          DID WE HIT A HYPHEN?                         
         BNE   CHKP70                                                           
         LR    R1,R3                                                            
         AHI   R1,-1               CHECK IF PREVIOUS CHAR IS A NUMBER           
         CLI   0(R1),C'1'          IS IT A NUMBER?                              
         BL    CHKP70              NO-SKIP IT.                                  
         SR    R1,R1                                                            
         LA    RE,CHKP30                                                        
CHKP30   CLI   0(R3),C' '          DID WE FIND A SIGNIFICANT CHAR?              
         BE    CHKP50                                                           
         AHI   R1,1                                                             
CHKP40   AHI   R3,-1                                                            
         BCTR  R0,RE                                                            
         B     CHKP70                                                           
*                                                                               
CHKP50   AHI   R3,1                                                             
         LA    RE,WORK                                                          
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R3)                                                    
         LA    RE,1(R1,RE)                                                      
         MVC   0(26,RE),26(R5)                                                  
         LA    RE,25(RE)           LAST SIGNIFICANT FIELD                       
         LA    R0,1(R1)            LENGTH OF FIELD TO MOVE                      
CHKP60   CLI   0(RE),X'40'         ARE WE AT A SIGNIFICANT CHARACTER?           
         BNE   CHKP70              NOT ENOUGH ROOM-CAN'T CHANGE                 
         AHI   RE,-1                                                            
         BCT   R0,CHKP60                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES                                                   
         MVC   26(26,R5),WORK                                                   
*                                                                               
CHKP70   LA    R5,26(R5)                                                        
         SR    R0,R0                                                            
         IC    R0,BYTE                                                          
         BCT   R0,CHKP10                                                        
*                                                                               
CHKPX    B     POSTX                                                            
         EJECT                                                                  
***********************************************************************         
* POSTINGS FOR CASH RECEIPT                                           *         
***********************************************************************         
                                                                                
         USING PSHEADD,R5                                                       
         USING TRNELD,R6                                                        
PCASH    NTR1  ,                                                                
         CLI   MIELM,0             ANY MEDIA INTERFACE ELEMENT                  
         BE    PCASH3              DO IT THE OLD WAY                            
         LA    R4,MIELM                                                         
         USING MIOELD,R4                                                        
         LA    RF,MIOCSHR         U.S. CASH RECEIPT ACCOUNT                     
         CLI   COUNTRY,C'C'                                                     
         BNE   *+8                                                              
         LA    RF,MIOCCSR         CANADIAN CASH RECEIPT                         
         CLI   0(RF),C' '          HAVE THEY SPECIFIED AN ACCOUNT               
         BNH   PCASH3                                                           
         MVC   PSHDACC+1(14),0(RF) CASH RECEIPT ACCOUNT                         
         B     PCASH5                                                           
*                                                                               
PCASH3   MVC   PSHDACC+1(2),=C'SC' HARD FOR CASH RECEIPT CONTROL                
         MVC   PSHDACC+3(12),CRACCT                                             
         CLI   COUNTRY,C'C'                                                     
         BNE   *+8                                                              
         MVI   PSHDACC+4,C'C'      CANADIAN CASH RECEIPT                        
         CLI   PROFIL4,C'Y'        CASH REC BY MEDIA WANTED                     
         BNE   PCASH5                                                           
         MVC   PSHDACC+7(1),SRTMED                                              
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(15),PSHDACC    READ FOR CASH RECEIPT BY MEDIA               
         GOTO1 ADMGR,ACCRD                                                      
         BE    *+8                 YES                                          
         MVI   PSHDACC+7,X'40'     NO, FORCE TO NON-MEDIA C/R ACCT              
*                                                                               
PCASH5   MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC+1(1),SRTMED                                             
         MVI   PSHDSBAC+2,C'/'                                                  
         MVC   PSHDSBAC+3(3),SRTCLI                                             
         MVI   PSHDSBAC+6,C'/'                                                  
         MVC   PSHDSBAC+7(3),SRTPROD                                            
*                                                                               
PCASH7   ZAP   TRNAMNT,TOT        NET                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(1),SRTMED                                                   
         MVC   WORK+1(11),SRTPUB                                                
*                                                                               
PCASH9   GOTO1 DATCON,DMCB,(1,TODAY),(8,WORK+13)                                
         GOTO1 ADSQUASH,DMCB,WORK,25                                            
         L     R4,DMCB+4                                                        
         SR    R2,R2                                                            
         IC    R2,TRNLN                                                         
         LA    R1,0(R2,R6)                                                      
         MVI   0(R1),C' '          END OF NARRATIVE                             
         SH    R4,=H'1'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),WORK        PUBFILE AND DATE TO NARRATIVE                
         LA    R2,2(R4,R2)         GET NEW LENGTH                               
         STC   R2,TRNLN                                                         
         AR    R6,R2                                                            
         MVI   0(R6),0                                                          
         B     POSTX                                                            
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* POSTINGS FOR GST                                                    *         
***********************************************************************         
                                                                                
PGST     NTR1  ,                                                                
         L     R5,ATWORK                                                        
         USING PSHEADD,R5                                                       
         L     R6,ATWORKT                                                       
         USING TRNELD,R6                                                        
         MVI   TRNSTAT,X'80'      DEBIT TO SG                                   
         MVC   TRNREF,SRTINV                                                    
         ZAP   TRNAMNT,GST        FOR THE GST                                   
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R6)            AND MEMO                                     
         AR    R6,R1                                                            
         USING SCIELD,R6                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,C'N'       NET TYPE                                      
         ZAP   SCIAMNT,NET        NET AMOUNT                                    
         AP    SCIAMNT,CASHD      PLUS C.D.                                     
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
         MVC   PSHDSBAC,PAYACCT    PAYABLE ACCOUNT IS C/A                       
         MVC   PSHDSBNM,PAYNAME    PAYABLE ACCOUNT NAME                         
         MVC   PSHDACC+1(14),=CL14'SG GST MISSING'                              
         CLI   SRTGSTYP,C' '                                                    
         BNH   PGSTX               NOT VALID TYPE USE DEFAULT                   
         XC    WORK(10),WORK       DUMMY FIELD HEADER                           
         MVI   WORK,9              HEADER + FIELD LENGTH                        
         MVI   WORK+5,1            INPUT LENGTH                                 
         MVC   WORK+8(1),SRTGSTYP  GST TYPE                                     
         CLI   SRTGSTYP,C'T'       TREAT T LIKE S                               
         BNE   *+8                                                              
         MVI   WORK+8,C'S'                                                      
*                                                                               
         L     R6,ATWORKT                                                       
         USING TRNELD,R6                                                        
         LA    R1,VTC                                                           
         USING VTCD,R1                                                          
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCAIVAL    VALIDATE INPUT TYPE                          
         MVC   VTCCPY,PSHDACC      COMPANY CODE                                 
         MVC   VTCOFFC,TRNOFFC    OFFICE=CLI/PRO OFFICE                         
         MVC   VTCCOMF,ADCOMFAC    COMFACS                                      
         LA    R2,WORK                                                          
         ST    R2,VTCAFLDH         FIELD HEADER FOR TYPE                        
         MVC   VTCINVD,TRNDATE                                                  
         L     RE,AUTL                                                          
         MVC   4(1,RE),ACCTSEN     SET ACC SE NUMBER                            
         GOTO1 VATICAN                                                          
         BNE   PGSTX               NOT OK, USE DEFAULT                          
         MVC   PSHDACC,VTCACT      VAT ACCOUNT                                  
*                                                                               
PGSTX    BAS   RE,PUTAPE       ADD TO POSTING FILE                              
         B     POSTX                                                            
         DROP  R1,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* POSTINGS FOR PST                                                    *         
***********************************************************************         
                                                                                
PPSTT    NTR1  ,                                                                
         LR    R4,R1               R1 = PST ENTRY                               
         USING SRTPST,R4                                                        
         L     R5,ATWORK                                                        
         USING PSHEADD,R5                                                       
         MVC   PSHDSBAC,PAYACCT    PAYABLE ACCOUNT IS C/A                       
         MVC   PSHDSBNM,PAYNAME    PAYABLE ACCOUNT NAME                         
*                                                                               
         L     R6,ATWORKT                                                       
         USING TRNELD,R6                                                        
         MVI   TRNSTAT,X'80'      DEBIT TO SG                                   
         MVC   TRNREF,SRTINV                                                    
         TM    BYTE,X'20'         IS PST RUNNING AS BINARY?                     
         BO    *+14                                                             
         ZAP   DUB,SRTPSTAM       POST THE PST AMOUNT                           
         B     *+12                                                             
         ICM   R1,15,SRTPSTAM                                                   
         CVD   R1,DUB                                                           
         ZAP   TRNAMNT,DUB                                                      
         LLC   R1,1(R6)           ADD MEMO                                      
         AR    R6,R1              POINT TO NEW LOCATION TO BUILD                
*                                                                               
         USING SCIELD,R6                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,C'N'       NET TYPE                                      
         ZAP   SCIAMNT,NET        NET AMOUNT                                    
*        AP    SCIAMNT,GST        + GST       (REMOVED FOR DSFTK-158)           
         AP    SCIAMNT,CASHD      + CD                                          
         LLC   R1,1(R6)                                                         
         AR    R6,R1               POINT TO NEW LOCATION                        
         MVI   0(R6),0             NEW END OF RECORD                            
*                                                                               
         MVC   PSHDACC+1(14),=CL14'SG PST MISSING'                              
         XC    WORK(10),WORK       DUMMY FIELD HEADER                           
         MVI   WORK,9              HEADER + FIELD LENGTH                        
         MVI   WORK+5,1            INPUT LENGTH                                 
         MVC   WORK+8(1),SRTPSTYP  PST TYPE                                     
*                                                                               
         USING TRNELD,R6                                                        
         L     R6,ATWORKT         POINT AT X'44' ELEMENT                        
*                                                                               
         USING VTCD,R1                                                          
         LA    R1,VTC              VATICAN BLOCK                                
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCAIVAL    VALIDATE INPUT TYPE                          
         MVC   VTCCPY,PSHDACC      COMPANY CODE                                 
         MVC   VTCOFFC,TRNOFFC    OFFICE=CLI/PRO OFFICE                         
         MVC   VTCCOMF,ADCOMFAC    COMFACS                                      
         LA    R2,WORK                                                          
         ST    R2,VTCAFLDH         FIELD HEADER FOR TYPE                        
         MVC   VTCPRV,SRTPSTPR     PROVINCE CODE                                
         MVC   VTCINVD,TRNDATE                                                  
         L     RE,AUTL                                                          
         MVC   4(1,RE),ACCTSEN     SET ACC UTL                                  
         GOTO1 VATICAN                                                          
         BNE   PPSTX               NOT OK, USE DEFAULT                          
         MVC   PSHDACC,VTCACT      REPLACE WITH GOOD VAT ACCOUNT                
*                                                                               
PPSTX    BAS   RE,PUTAPE       ADD TO POSTING FILE                              
         B     POSTX                                                            
         DROP  R1,R4,R5,R6                                                      
         EJECT                                                                  
***********************************************************************         
* BUILD MEDIA CONTROL POSTING TO SZ                                   *         
***********************************************************************         
                                                                                
PMEDC    L     R5,ATWORK                                                        
         USING PSHEADD,R5                                                       
         L     R6,ATWORKT                                                       
         USING TRNELD,R6                                                        
         SR    R2,R2                                                            
         MVI   PSHDEL,PSHDELQ           POSTING HEADER                          
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,SPACES                                                   
         MVC   PSHDACC(1),SAVCPY                                                
         MVC   PSHDACC+1(3),=C'SZP'     HARD-MEDIA CONTROL ACCOUNTS             
         MVC   PSHDACC+4(1),SAVMED                                              
*                                                                               
PMEDC3   MVC   PSHDANAL(53),SPACES                                              
         MVC   PSHDSBAC(1),SAVCPY                                               
         MVC   PSHDSBAC+1(2),=C'SJ'                                             
         MVC   PSHDSBAC+3(3),BUFCLI                                             
         MVC   PSHDSBNM(20),BUFCLIN                                             
*                                                                               
         XC    MIELM,MIELM         CLEAR CURRENT MI ELEMENT                     
         L     R4,AMIBUFF                                                       
         USING MIOELD,R4                                                        
PMEDC5   CLI   0(R4),X'FF'         END-OF-BUFFER                                
         BE    PMEDC9              NO MATCH MEDIA RECORD                        
         CLC   MIOCODE,PSHDACC+3   MATCH MEDIA                                  
         BE    PMEDC7                                                           
         LA    R4,MIOLNQ(R4)                                                    
         B     PMEDC5                                                           
*                                                                               
PMEDC7   CLI   MIOCNTL,C' '       HAS MEDIA ACCOUNT BEEN SPECIFIED              
         BNH   *+10                DON'T REPLACE IF NOT USED                    
         MVC   PSHDACC+1(14),MIOCNTL                                            
         MVC   MIELM,MIOEL        SAVE MI ELEMENT FOR THIS MEDIA                
*                                                                               
PMEDC9   LA    R4,MIELM                                                         
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
         MVC   TRNDATE,TODAY                                                    
         MVC   TRNREF,SPACES                                                    
         XC    TRNSUB(2),TRNSUB                                                 
         MVI   TRNSTAT,X'80'                                                    
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNBTCH(2),MOS                                                   
         ZAP   TRNAMNT,BUFNET                                                   
         SP    TRNAMNT,BUFCONET   LESS COMMISSION ONLY AMOUNT                   
         MVC   TRNOFFC(4),=X'40404000'                                          
         MVC   TRNOFFC,BUFOFF                                                   
         CP    TRNAMNT,=P'0'                                                    
         BE    PMEDC11             NO ZERO POSTINGS                             
         BAS   RE,PUTAPE                                                        
*                                                                               
PMEDC11  CP    BUFCONET,=P'0'                                                   
         BE    POSTX               NO COMMISSION                                
         MVC   WORK(12),PSHDACC+3  CHANGE SZST TO SZCST                         
         MVI   PSHDACC+3,C'C'                                                   
         MVC   PSHDACC+4(11),WORK  COMMISSION ONLY FROM                         
         CLI   MIOCONL,0           MI RECORD                                    
         BE    *+10                                                             
         MVC   PSHDACC+1(14),MIOCONL                                            
         MVI   TRNSTAT,0           CREDIT TO SZ                                 
         ZAP   TRNAMNT,BUFCONET    COMMISSION ONLY                              
         AP    TRNAMNT,BUFCOGST    PLUS GST ON IT                               
         CP    TRNAMNT,=P'0'       NET IS ZERO                                  
         BE    POSTX               SKIP IT                                      
         BAS   RE,PUTAPE                                                        
         MVI   TRNSTAT,X'80'       DEBIT TO SZ                                  
         BAS   RE,PUTAPE                                                        
         B     POSTX                                                            
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* POST THE TOTAL RECORD                                       *                 
***********************************************************************         
                                                                                
PTOTAL   L     R5,ATWORK                                                        
         USING PSSUBFD,R5                                                       
         MVI   PSSBEL,PSSBELQ                                                   
         MVI   PSSBLEN,PSSUBFL                                                  
         MVC   PSSBDESC(8),=C'CLEARED-'                                         
         MVC   PSSBDESC+8(6),SYSNUMB                                            
         ZAP   PSSBRECS,COUNTER                                                 
         ZAP   PSSBCASH,CASHTOT                                                 
         CP    PSSBRECS,=P'0'                                                   
         BE    POSTX                                                            
         MVI   PSSBCASH+6,0                                                     
         BAS   RE,PUTAPE                                                        
         B     POSTX                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE AN ACPOST RECORD                                              *         
***********************************************************************         
                                                                                
PUTAPE   NTR1  ,                                                                
         L     R4,ATWORK                                                        
         SR    R2,R2                                                            
PUT2     CLI   0(R4),0                                                          
         BE    PUT4                                                             
         CLI   0(R4),TRNELQ        GET TOTAL DEBITS                             
         BNE   PUT3                                                             
         TM    TRNSTAT-TRNELD(R4),X'80'                                         
         BNO   *+10                                                             
         AP    CASHTOT,TRNAMNT-TRNELD(6,R4)                                     
*                                                                               
PUT3     IC    R2,1(R4)                                                         
         AR    R4,R2                                                            
         B     PUT2                                                             
*                                                                               
PUT4     L     R6,ATWORKL               WORK OUT LENGTH FOR WORKER              
         XC    0(4,R6),0(R6)                                                    
         LA    R4,1(R4)                                                         
         SR    R4,R6                                                            
         STH   R4,0(R6)                                                         
         GOTO1 ADMGR,WRKADD                                                     
         B     POSTX                                                            
         EJECT                                                                  
***********************************************************************         
* GET NAME                                                            *         
***********************************************************************         
                                                                                
GETNME   NTR1  ,                                                                
         MVC   WORK,SPACES                                                      
         USING NAMELD,R5                                                        
         L     R5,AIO1             GET RECORD NAME                              
         AH    R5,FRSTL                                                         
         SR    R0,R0                                                            
*                                                                               
GETNME3  CLI   0(R5),0                                                          
         BE    POSTX                                                            
         CLI   NAMEL,NAMELQ                                                     
         BE    GETNME5                                                          
         IC    R0,NAMLN                                                         
         AR    R5,R0                                                            
         B     GETNME3                                                          
*                                                                               
GETNME5  SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),NAMEREC                                                  
         B     POSTX                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL CONSTANTS                                                     *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         DROP  RB,R9,R8                                                         
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER AND WORKER FILE ROUTINES                               *         
***********************************************************************         
                                                                                
DMGR     NMOD1 0,*DMGR*                                                         
         L     RF,4(RD)                                                         
         L     RC,68(RF)           RESTORE RC                                   
*                                                                               
         STC   R1,DMFLG            SAVE COMMAND CODE                            
         LA    RE,SYSTSEN          SET SYSTEM SE NUMBER                         
         CLI   DMFLG,CONRD                                                      
         BNH   DMGR3                                                            
         LA    RE,ACCTSEN          SET ACC SE NUMBER                            
         CLI   DMFLG,ACCGET                                                     
         BNH   DMGR3                                                            
         LA    RE,PRNTSEN          SET PRINT SE NUMBER                          
*                                                                               
DMGR3    L     RF,AUTL                                                          
         MVC   4(1,RF),0(RE)       SET SE NUMBER FOR FILE                       
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     DCONRD              READ CONTROL FILE                            
         B     DACCOPN             OPEN ACCOUNT FILE                            
         B     DACCRD              READ ACCDIR                                  
         B     DACCHI              READ HIGH ACCDIR                             
         B     DACCSEQ             READ SEQ ACCDIR                              
         B     DACCGET             GETREC ACCMST                                
         B     DPRTOPN             OPEN PRINT FILES                             
         B     DPRTRD              READ PRINT DIR                               
         B     DPRTHI              READ HIGH PRINT DIR                          
         B     DPRTSEQ             READ SEQ PRINT DIR                           
         B     DPRTGET             GETREC PRINT FILE                            
         B     DPUBRD              READ PUBFILE                                 
         B     DPUBHI              READ HIGH PUBFILE                            
         B     DPUBSEQ             READ SEQUENTIAL PUBFILE                      
         B     DPUBGET             GETREC PUBFILE                               
         B     DPRTREQ             READ PRINT REQUEST FILE                      
*                                                                               
         B     DWRKOPN             OPEN WORKER FILE                             
         B     DWRKADD             ADD WORKER FILE                              
         B     DWRKCLO             CLOSE WORKER FILE                            
         B     DODDOPN             OPEN ODD FILE                                
         B     DODDADD             ADD ODD FILE                                 
         B     DODDCLO             CLOSE ODD FILE                               
         B     DONLOPN             OPEN FOR ONLINE ADDS                         
         B     DONLADD             ADD FILE FOR ONLINE ADDS                     
         B     DONLCLO             CLOSE ONLINE FILE                            
*                                                                               
DCONRD   GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,DKEY,ACONIO READ CONTROL              
         B     DMERR                                                            
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES - ACCOUNT FILES                               *         
***********************************************************************         
                                                                                
DACCOPN  GOTO1 DATAMGR,DMCB,OPEN,ACCOUNT,ACFILEL     OPEN ACCOUNT               
         B     DMXIT                                                            
*                                                                               
DACCRD   GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR   READ ACC DIR               
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DACCHI   GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR   READ HI ACC DIR            
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DACCSEQ  GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR   READ SEQ ACC DIR           
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DACCGET  GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO1,DMWORK GETREC ACCMST          
         B     DMERR                                                            
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES - PRINT FILES                                 *         
***********************************************************************         
                                                                                
DPRTOPN  GOTO1 DATAMGR,DMCB,OPEN,PRINTF,PRFILEL      OPEN PRINT FILES           
         B     DMXIT                                                            
*                                                                               
DPRTRD   GOTO1 DATAMGR,DMCB,DMREAD,PRTDIR,DKEY,DIR   READ PRINT DIR             
         MVC   DA,DIR+(PAGYCNTL-PAGYKEY)                                        
         B     DMERR                                                            
*                                                                               
DPRTHI   GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,DKEY,DIR   READ HI PRINT DIR          
         MVC   DA,DIR+(PAGYCNTL-PAGYKEY)                                        
         B     DMERR                                                            
*                                                                               
DPRTSEQ  GOTO1 DATAMGR,DMCB,DMRSEQ,PRTDIR,DKEY,DIR   READ SEQ PRINT DIR         
         MVC   DA,DIR+(PAGYCNTL-PAGYKEY)                                        
         B     DMERR                                                            
*                                                                               
DPRTGET  GOTO1 DATAMGR,DMCB,GETREC,PRTFIL,DA,AIO1,DMWORK PRINT FILE             
         B     DMERR                                                            
*                                                                               
DPUBRD   GOTO1 DATAMGR,DMCB,DMREAD,PUBDIR,DKEY,DIR   READ PUBDIR                
         MVC   DA,DIR+(PAGYCNTL-PAGYKEY)                                        
         B     DMERR                                                            
*                                                                               
DPUBHI   GOTO1 DATAMGR,DMCB,DMRDHI,PUBDIR,DKEY,DIR   READ HI PUBDIR             
         MVC   DA,DIR+(PAGYCNTL-PAGYKEY)                                        
         B     DMERR                                                            
*                                                                               
DPUBSEQ  GOTO1 DATAMGR,DMCB,DMRSEQ,PUBDIR,DKEY,DIR   READ SEQ PUBDIR            
         MVC   DA,DIR+(PAGYCNTL-PAGYKEY)                                        
         B     DMERR                                                            
*                                                                               
DPUBGET  GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,DA,AIO1,DMWORK PUBFILE               
         B     DMERR                                                            
*                                                                               
DPRTREQ  GOTO1 ,DMCB,(X'10',DMRSEQ),REQUEST,DISKAD,AREQIO,ATRKBLK               
         GOTO1 DATAMGR                                                          
         TM    8(R1),X'80'         TEST EOF                                     
         BO    DMXITN                                                           
         TM    8(R1),X'40'         DISK ERROR                                   
         BNO   DMXITY                                                           
         LH    RE,DISKAD           GO TO NEXT TRACK                             
         LA    RE,1(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,DISKAD                                                        
         MVC   P(34),=C'** DISK ERROR-BUT STRUGGLING ON **'                     
         GOTO1 LOGIO,DMCB,1,(34,P)                                              
         B     DPRTREQ                                                          
*                                                                               
DMERR    MVC   DMBYTE,8(R1)                                                     
         CLI   DMBYTE,X'10'        TEST RECORD NOT FOUND                        
         BE    DMXITN              RETURN NOT EQUAL                             
         CLI   DMBYTE,0                                                         
         BE    DMXITY                                                           
         DC    H'0'                                                             
DMXITN   LTR   RB,RB                                                            
         B     *+6                                                              
DMXITY   CR    RB,RB                                                            
DMXIT    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* WORKER FILE ROUTINES                                                *         
***********************************************************************         
                                                                                
DWRKOPN  LA    R2,OPEN             WORKER FILE OPEN                             
         L     R3,APOSTBUF                                                      
         LA    R4,POSTKEY                                                       
         B     WRKALL                                                           
*                                                                               
DWRKADD  LA    R2,ADD              WORKER ADD                                   
         L     R3,APOSTBUF                                                      
         LA    R4,POSTKEY                                                       
         AP    COUNTER,=P'1'                                                    
         B     WRKALL                                                           
*                                                                               
DWRKCLO  LA    R2,CLOSE            WORKER FILE CLOSE                            
         L     R3,APOSTBUF                                                      
         LA    R4,POSTKEY                                                       
         B     WRKALL                                                           
*                                                                               
DODDOPN  LA    R2,OPEN             ODD FILE OPEN                                
         L     R3,AODDBUF                                                       
         LA    R4,ODDKEY                                                        
         B     WRKALL                                                           
*                                                                               
DODDADD  LA    R2,ADD              ODD FILE ADD                                 
         L     R3,AODDBUF                                                       
         LA    R4,ODDKEY                                                        
         B     WRKALL                                                           
*                                                                               
DODDCLO  LA    R2,CLOSE            ODD FILE CLOSE                               
         L     R3,AODDBUF                                                       
         LA    R4,ODDKEY                                                        
         B     WRKALL                                                           
*                                                                               
DONLOPN  LA    R2,OPEN             ONLINE FILE OPEN                             
         L     R3,AONLBUF                                                       
         LA    R4,ONLNKEY                                                       
         B     WRKALL                                                           
*                                                                               
DONLADD  LA    R2,ADD              ONLINE FILE ADD                              
         L     R3,AONLBUF                                                       
         LA    R4,ONLNKEY                                                       
         B     WRKALL                                                           
*                                                                               
DONLCLO  LA    R2,CLOSE            ONLINE FILE CLOSE                            
         L     R3,AONLBUF                                                       
         LA    R4,ONLNKEY                                                       
*                                                                               
WRKALL   CLI   RCPOSTNG,C'N'                                                    
         BE    DMXIT               NO POSTINGS                                  
         L     R5,ATWORKL                                                       
         GOTO1 WORKER,DMCB,(R2),(R3),(R4),(R5)                                  
         TM    DMCB+8,X'C0'                                                     
         BZ    DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
* ERROR ON ADD TO WORKER FILE                                         *         
***********************************************************************         
                                                                                
WKERR    NTR1  ,                                                                
         MVC   P,SPACES                                                         
         MVC   P(39),=C'*** WORKER FILE FULL - CANNOT ADD ID = '                
         TM    DMCB+8,X'80'                                                     
         BO    *+10                                                             
         MVC   P(39),=C'** WKFILE DISK ERROR - CANNOT ADD ID = '                
         L     R2,DMCB+8           ADDRESS OF KEY                               
         MVC   DUB(2),0(R2)                                                     
         LH    R3,DUB                                                           
         CVD   R3,DUB                                                           
         UNPK  DUB+2(6),DUB                                                     
         OI    DUB+7,X'F0'                                                      
         MVC   P+39(3),DUB+5                                                    
         MVC   P+42(23),=C', REPLY = ''OK'' FOR DUMP'                           
*                                                                               
WKERR2   GOTO1 LOGIO,WORK,1,(65,P)                                              
         GOTO1 (RF),(R1),0,(2,DUB)                                              
         CLC   DUB(2),=C'OK'                                                    
         BNE   WKERR2                                                           
         DC    H'0'                NOW DIE                                      
         EJECT                                                                  
***********************************************************************         
* LOCAL CONSTANTS                                                     *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GLOBAL WORKING STORAGE                                              *         
***********************************************************************         
         SPACE 1                                                                
         ORG   AC5402+(4096*5)                                                  
AC54C    CSECT                                                                  
*                                                                               
CENTER   DC    V(CENTER)                                                        
CLPACK   DC    V(CLPACK)                                                        
DATVAL   DC    V(DATVAL)                                                        
HEXIN    DC    V(HEXIN)                                                         
GETLOGO  DC    V(GETLOGO)                                                       
PPGETADR DC    V(PPGETADR)                                                      
PUBED    DC    V(PUBED)                                                         
UNDERLIN DC    V(UNDERLIN)                                                      
VATICAN  DC    V(VATICAN)                                                       
PUBVAL   DC    V(PUBVAL)                                                        
PQPROF   DC    V(PQPROF)                                                        
*                                                                               
APOST    DC    A(POST)             POSTING ROUTINE                              
APUBL    DC    A(PUBL)             PUB ROUTINE                                  
AREPL    DC    A(REPL)             REP ROUTINE                                  
*                                                                               
AOFNTAB  DC    A(OFNTAB)           OFFICE TABLE OF CODES AND NAMES              
AOFFMTAB DC    A(OFFMTAB)          OFFICE TABLE FOR MEDIAS                      
AOFFATAB DC    A(OFFATAB)          OFFICE TABLE FOR AGENCIES                    
AACCUMS  DC    A(ACCUMS)           LEVELS OF ACCUMULATORS                       
ATWORKL  DC    A(TWORKL)           A(LENGTH OF POSTING)                         
ATWORK   DC    A(TWORK)            A(POSTING FILE HEADER)                       
ATWORKT  DC    A(TWORKT)           A(POSTING FILE TRANSACTION)                  
AREQIO   DC    A(REQIO)            A(REQUEST INPUT AREA)                        
AIO1     DC    A(IO1)              A(ACC/PRINT FILE INPUT AREA)                 
AIO2     DC    A(IO2)                                                           
ACONIO   DC    A(CONIO)            A(CONTROL FILE INPUT AREA)                   
APOSTBUF DC    A(POSTBUF)          A(BUFFER FOR POSTING WRKFILE)                
AODDBUF  DC    A(ODDBUF)           A(BUFFER FOR ODDS WRKFILE)                   
AONLBUF  DC    A(ONLBUF)           A(BUFFER FOR ONLINE WRKFILE)                 
AAGYTAB  DC    A(AGYTAB)           A(AGENCY CONTROL TABLE)                      
APROFBUF DC    A(PROFBUF)          A(BUFFER FOR CONTROL PROFILES)               
ATRKBLK  DC    A(TRKBLK)           A(BLOCK FOR FULL TRACK READ)                 
ADBUF    DC    A(BUFFALOC)         A(BUFFALO CSECT)                             
*                                                                               
ADMGR    DC    A(DMGR)             A(COMMON DATAMGR ROUTINE)                    
*                                                                               
AMIBUFF  DC    F'0'                A(MEDIA INTERFACE ELEMENTS)                  
*                                                                               
CONRD    EQU   1                   CONTROL FILE - READ                          
ACCOPN   EQU   2                   ACCOUNT FILE - OPEN                          
ACCRD    EQU   3                   ACCDIR       - READ                          
ACCHI    EQU   4                                - HIGH                          
ACCSEQ   EQU   5                                - SEQUENTIAL                    
ACCGET   EQU   6                   ACCMST       - GETREC                        
PRTOPN   EQU   7                   PRINT FILE - OPEN                            
PRTRD    EQU   8                   PRINT DIR - READ                             
PRTHI    EQU   9                             - HIGH                             
PRTSEQ   EQU   10                            - SEQUENTIAL                       
PRTGET   EQU   11                  PRINT FILE - GETREC                          
PUBRD    EQU   12                  PUBFILE FILE - READ                          
PUBHI    EQU   13                               - HIGH                          
PUBSEQ   EQU   14                               - HIGH                          
PUBGET   EQU   15                               - HIGH                          
PRTREQ   EQU   16                               - HIGH                          
WRKOPN   EQU   17                  WORKER FILE  - OPEN                          
WRKADD   EQU   18                               - ADD                           
WRKCLO   EQU   19                               - CLOSE                         
ODDOPN   EQU   20                  ODD FILE     - OPEN                          
ODDADD   EQU   21                               - ADD                           
ODDCLO   EQU   22                               - CLOSE                         
ONLOPN   EQU   23                  ONLINE       - OPEN                          
ONLADD   EQU   24                               - ADD                           
ONLCLO   EQU   25                               - CLOSE                         
*                                                                               
DMFLG    DC    X'00'                                                            
DKEY     DC    CL42' '                                                          
DIR      DC    CL60' '                                                          
DMBYTE   DC    X'00'                                                            
DA       DC    F'0'                                                             
DISKAD   DC    F'0'                DISK ADDRESS FOR NEXT REQUEST                
*                                                                               
MIBUFLN  DC    F'0'                                                             
*                                                                               
LEVELS   DS    0XL1                                                             
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
SVCULA   DS    CL(L'PSHDACC)       COMPANY/UNIT/LEDGER/ACCOUNT                  
*                                                                               
AUTL     DC    A(0)                                                             
AKEY     DC    A(0)                                                             
*                                                                               
FRSTL    DC    Y(ACCRFST-ACCRECD)                                               
*                                                                               
OFFLINE  EQU   0                                                                
ONLINE   EQU   1                                                                
*                                                                               
TYPE     DS    XL1                                                              
TYPOFF   EQU   X'80'               OFFLINE                                      
TYPONL   EQU   X'40'               ONLINE                                       
TYPBOTH  EQU   TYPOFF+TYPONL       BOTH                                         
NTYP     EQU   2                   NUMBER OF TYPES                              
*                                                                               
ROW      DS    XL1                 ROW NUMBER                                   
ROWCLI   EQU   0                   CLIENT                                       
ROWPUB   EQU   1                   PUBFILE                                      
ROWREP   EQU   2                   REP                                          
ROWMED   EQU   3                   MEDIA                                        
ROWAGY   EQU   4                   SOURCE AGENCY                                
ROWCPY   EQU   5                   ACCPAK COMPANY CODE FOR POSTINGS             
ROWRUN   EQU   6                   RUN                                          
NROW     EQU   7                   NUMBER OF ROWS                               
*                                                                               
CSHCTL   DS    0X                                                               
NET      DC    PL8'0'              NET AMOUNT (NO GST OR CD)                    
GST      DC    PL8'0'              GST                                          
PST      DC    PL8'0'              PST                                          
TOT      DC    PL8'0'              NET + GST + PST                              
CASHR    DC    PL8'0'              CASH RECEIPT                                 
CASHD    DC    PL8'0'              CASH DISCOUNT                                
LCASHD   DC    PL8'0'              LOST CASH DISCOUNT                           
CONET    DC    PL8'0'              COMMISSION ONLY NET                          
COGST    DC    PL8'0'              COMMISSION ONLY GST                          
COPST    DC    PL8'0'              COMMISSION ONLY PST                          
CSHC     EQU   (*-NET)/8           NUMBER OF COLUMNS                            
CSHLNQ   EQU   *-CSHCTL                                                         
CSHTLNQ  EQU   (CSHLNQ*NROW)       TABLE LENGTH(LENGTH 0F ROW * # ROWS)         
*                                                                               
ALL      EQU   X'FF'                                                            
*                                                                               
COMMAND  DC    C'        '                                                      
OPEN     DC    C'OPEN    '                                                      
CLOSE    DC    C'CLOSE   '                                                      
ADD      DC    C'ADD     '                                                      
GETREC   DC    C'GETREC  '                                                      
*                                                                               
CONTROL  DC    C'CONTROL '                                                      
CTFILE   DC    C'CTFILE  '                                                      
*                                                                               
ACCOUNT  DC    C'ACCOUNT '                                                      
ACFILEL  DC    C'NACCDIR NACCMST NACCARC X '                                    
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
*                                                                               
PRINTF   DC    C'PRINT   '                                                      
PRFILEL  DC    C'NPRTDIR NPRTFILENPUBDIR NPUBFILE'                              
REQFIL   DC    C'NPREQ   X '                                                    
PRTDIR   DC    C'PRTDIR  '                                                      
PRTFIL   DC    C'PRTFILE '                                                      
PUBDIR   DC    C'PUBDIR  '                                                      
PUBFILE  DC    C'PUBFILE '                                                      
REQUEST  DC    C'REQUEST '                                                      
DELIML   DC    C'DELIMITERLIST=<, ',X'FF'                                       
*                                                                               
PRTREQQ  EQU   69                  PRINT REQUEST FILE                           
PRTPAYQ  EQU   03                  PRINT PAY PROGRAM                            
*                                                                               
SYSNUMB  DC    CL6' '              SYSTEM AND NUMBER (PRINT X)                  
PRTNUM   DS    CL6                 PRINT/NET LOGICAL NUMBER CHARACTER           
SEN      DS    XL1                 BASE SYSTEM NUMBER PRINT IS 2, NET 3         
SENPRNT  EQU   4                                                                
*                                                                               
PRNTSEN  DC    X'00'               PRINT SE NUMBER                              
ACCTSEN  DC    X'00'               ACC SE NUMBER                                
SYSTSEN  DC    X'00'               SYSTEM SE NUMBER                             
*                                                                               
ACCTAB   DS    XL20                TABLE OF OPEN ACC FILES                      
FFS      DC    20X'FF'                                                          
*                                                                               
BREAK    DC    X'00'                                                            
CLITQ    EQU   X'80'               CLIENT                                       
PUBTQ    EQU   X'40'               PUBFILE                                      
REPTQ    EQU   X'20'               REP                                          
MEDTQ    EQU   X'10'               MEDIA                                        
AGYTQ    EQU   X'08'               SOURCE AGENCY                                
CPYTQ    EQU   X'04'               POSTING COMPANY                              
*                                                                               
CLIQ     EQU   CLITQ                                                            
PUBQ     EQU   CLITQ+PUBTQ                                                      
REPQ     EQU   CLITQ+PUBTQ+REPTQ                                                
MEDQ     EQU   CLITQ+PUBTQ+REPTQ+MEDTQ                                          
AGYQ     EQU   CLITQ+PUBTQ+REPTQ+MEDTQ+AGYTQ                                    
CPYQ     EQU   CLITQ+PUBTQ+REPTQ+MEDTQ+AGYTQ+CPYTQ                              
*                                                                               
CTLTBL   DS    0XL2                                                             
         DC    AL1(SRTAGY-SRTK-1),AL1(CPYQ)     COMPANY POSTING                 
         DC    AL1(SRTMED-SRTK-1),AL1(AGYQ)     AGENCY TOTAL                    
         DC    AL1(SRTREP-SRTK-1),AL1(MEDQ)     MEDIA TOTAL                     
         DC    AL1(SRTPUB-SRTK-1),AL1(REPQ)     REP TOTAL                       
         DC    AL1(SRTCLI-SRTK-1),AL1(PUBQ)     PUB TOTAL                       
         DC    AL1(SRTLAST-SRTK-1),AL1(CLIQ)    CLIENT TOTAL                    
*                                                                               
FLGTOT   DC    X'00'               FLAG FOR TOTALS RUN                          
FLGAGY   EQU   X'80'               AGENCY TOTALS                                
FLGRUN   EQU   X'20'               RUN TOTALS                                   
FLGSUM   EQU   X'10'               SUMMARY REPORT                               
FLGIDS   EQU   X'08'               NOW PROCESSING ID REPORTS                    
*                                                                               
*  SORT SEQUENCE IS AGENCY/MEDIA/REP/PUBFILE/CLI/PROD                           
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,NNN,A),FORMAT=BI,WORK=1 '                    
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=NNN '                                  
*                                                                               
POSTC    DS    XL1                 POSTING CONTROL                              
PSTDTL   EQU   1                   POST CREDIT TO PAYABLE ACCOUNT               
PSTMED   EQU   2                   POST MEDIA TOTAL (DEBIT TO SZ)               
PSTTOT   EQU   3                   POST THE TOTAL RECORD                        
*                                                                               
ORIGNUM  DS    XL2                 ORIGIN NUMBER FROM PRINT PAY                 
*                                                                               
POSTKEY  DC    XL16'00'            INDEX KEY FOR POSTING FILE                   
ODDKEY   DC    XL16'00'            INDEX KEY FOR ODD FILE                       
ONLNKEY  DC    XL16'00'            INDEX KEY FOR ONLINE TOTAL                   
*                                                                               
RPTIT    DC    C'DAILY CLEARANCE REPORT'                                        
*                                                                               
MEDCODE  DS    CL1                 MEDIA CODE                                   
MEDNAME  DS    CL15                MEDIA NAME                                   
*                                                                               
MEDTAB   DS    0CL17                                                            
         DC    C'B',X'00',CL15'MOBILE'                                          
         DC    C'I',X'00',CL15'INTERACTIVE'                                     
         DC    C'L',X'00',CL15'SOCIAL'                                          
         DC    C'M',X'00',CL15'MAGAZINES'                                       
         DC    C'N',X'00',CL15'NEWSPAPERS'                                      
         DC    C'S',X'00',CL15'SUPPLEMENTS'                                     
         DC    C'T',X'00',CL15'TRADE'                                           
         DC    C'V',X'00',CL15'NATIONAL VIDEO'                                  
         DC    C'W',X'00',CL15'LOCAL VIDEO'                                     
         DC    C'D',X'00',CL15'DIGITAL AUDIO'                                   
         DC    C' ',X'00',CL15'OUTDOOR'                                         
*                                                                               
SAVAUTL  DS    XL1                                                              
SAVPROFL DS    CL(L'PROFILE)                                                    
*                                                                               
*                                                                               
ACTFLAG  DS    XL1                 ACCOUNT FLAG                                 
ACT20    EQU   X'80'               DO NOT ADD NAMELD                            
ACT22    EQU   X'40'               DO NOT ADD ADRELD                            
*                                                                               
TODAY    DS    XL3                 TODAY PWOS                                   
TODAY6   DS    CL6                 TODAY CHARACTER YYMMDD                       
XDATE    DS    CL6                 EXTRA CLEARANCE DATE                         
MOS      DS    XL2                 MONTH OF SERVICE                             
*                                                                               
CPYCODE  DS    XL1                 ACC COMPANY CODE                             
CMPSTA4  DC    X'00'               COMPANY STATUS 4                             
CMPSTA8  DC    X'00'               COMPANY STATUS 8                             
COMREP   DS    CL4                 CODE FOR COMMISSION ONLY REP                 
AGID     DS    XL2                 ACC PRINCIPAL ID                             
*DST1    DC    H'5'                ID FOR DATA CONTROL PAGE                     
DDST1    DC    H'498'              ID FOR DATA CONTROL PAGE                     
CTRY     DS    XL1                 COUNTRY CODE                                 
PAYACCT  DS    CL15                PAYABLE ACCOUNT                              
PAYNAME  DS    CL36                ACCOUNT NAME                                 
TAXUL    DS    CL2                 UNIT/LEDGER FOR GST                          
*                                                                               
AGYCODE  DC    XL1'00'             PRINT AGENCY CODE                            
AGYNAME  DC    CL(L'PAGYNAME)' '   AGENCY NAME                                  
AGYPROF  DC    CL(L'PAGYPROF)' '          PROFILE                               
CLINAME  DC    CL20' '             CLIENT NAME                                  
POFC     DC    CL1' '              PRINT OFFICE CODE                            
OFFC     DC    CL2' '              CLIENT OFFICE                                
OFFPAYR  DS    XL3                 OFFICE PAYEE                                 
SAVPRD   DC    CL3' '              PRODUCT CODE                                 
PRDNAME  DC    CL20' '             PRODUCT NAME                                 
*                                                                               
PUBPARAM DS    X'0'                                                             
*                                                                               
PUBNM    DC    CL20' '             PUB NAME                                     
PUBNML   DC    CL38' '             CITY + PUB NAME                              
PUBST    DC    CL2' '              STATE                                        
ZONE     DC    CL20' '             ZONE                                         
PUBAD    DC    CL105' '            PUB ADDRESS                                  
REPNAME  DC    CL30' '             REP NAME                                     
*                                                                               
WORKA    DC    CL255' '                                                         
*                                                                               
*                                                                               
CRACCT   DC    CL12' '                                                          
LASTOFF  DC    CL2' '                                                           
*                                                                               
ITEMS    DC    X'00'               ITEM STATUS                                  
ITMREP   EQU   X'80'               REP                                          
ITMUNW   EQU   X'40'               UNWIRED REP                                  
ITMCOMO  EQU   X'20'               COMMISSION ONLY REP                          
*                                                                               
BUFLINE  DS    0CL82               *** BUFFALO RECORD ***                       
BUFTYPE  DC    X'00'                                                            
BUFTBOTH EQU   C'1'                ONLINE AND OFFLINE                           
BUFTOFFL EQU   C'2'                OFFLINE ONLY     E                           
BUFCLI   DC    CL3' '                                                           
BUFCLIN  DC    CL20' '                                                          
BUFOFF   DC    CL2' '                                                           
BUFTOTAL DS    0PL8                                                             
BUFNET   DC    PL8'0'                                                           
BUFGST   DC    PL8'0'                                                           
BUFPST   DC    PL8'0'                                                           
BUFTOT   DC    PL8'0'                                                           
BUFCASHR DC    PL8'0'                                                           
BUFCASHD DC    PL8'0'                                                           
BUFLCASH DC    PL8'0'                                                           
BUFCONET DC    PL8'0'                                                           
BUFCOGST DC    PL8'0'                                                           
BUFCOPST DC    PL8'0'                                                           
BUFTOT#  EQU   (*-BUFTOTAL)/L'BUFTOTAL                                          
*                                                                               
OFNWRK   DC    CL(OFNLNQ)' '       WORK AREA FOR OFFICE CODE TABLE              
OFFWRK   DC    CL(OFFLNQ)' '       WORK AREA FOR OFFICE TABLE                   
*                                                                               
PLWRK    DC    CL240' '            PRINT WORK LINES                             
COUNTRY  DC    CL1' '              CANADA OR US                                 
LOGOS    DC    CL1' '                                                           
SLOGO    EQU   0                   START LOGO                                   
ELOGO    EQU   1                   END LOGO                                     
*                                                                               
SEQUENCE DC    PL3'0'                                                           
COUNTER  DC    PL3'0'                                                           
CASHTOT  DC    PL6'0'                                                           
NOSTATF  DC    PL2'0'                                                           
NOPRNTF  DC    PL2'0'                                                           
*                                                                               
MIELM    DC    CL(MIOLNQ)' '      CURRENT MI ELEMENT                            
REMOPT   DC    CL1' '                                                           
*                                                                               
PROFILE  DS    CL16                                                             
         ORG   PROFILE                                                          
PROFIL1  DS    CL1                 PRINT DETAILS FOR SPOT         Y,N           
PROFIL2  DS    CL1                 PRINT DETAILS FOR PRINT        Y,N           
PROFIL3  DS    CL1                 POST SPOT  CASH REC BY MEDIA   Y,N           
PROFIL4  DS    CL1                 POST PRINT CASH REC BY MEDIA   Y,N           
PROFIL5  DS    CL1                 USE PRODUCT NAME FOR PRINT     Y,N           
PROFIL6  DS    CL1                 USE LEDGER U FOR NETWORK       Y,N           
PROFIL7  DS    CL1                 ALTERNATE NETWORK CONTROL-SZ   U-Z,N         
PROFIL8  DS    CL1                 ADD'L PRINT REPORT BY ID       N,Y           
PROFIL9  DS    CL1                 ADD'L SPOT REPORT BY ID        N,Y           
PROFIL10 DS    CL1                 ADD'L NETWORK REPORT BY ID     N,Y           
PROFIL11 DS    CL1                 ALTERNATE CODE FOR LOST C.D.   A-Z           
PROFIL12 DS    CL1                 ANALYSIS BY OFFICE             Y,N           
PROFIL13 DS    CL1                 INCLUDE CITY BEFORE PUB NAME   Y,N           
PROFIL14 DS    CL1                 N/D                                          
PROFIL15 DS    CL1                 N/D                                          
PROFIL16 DS    CL1                 N/D                                          
*                                                                               
VTC      DC    CL(VTCLNQ)' '       VATICAN BLOCK                                
*                                                                               
SAVBRK   DS    0CL(SRTBKLNQ)       BREAK CONTROL                                
SAVOID   DS    XL(L'SRTOID)        ORIGIN ID                                    
SAVASE   DS    XL(L'SRTASE)        ACC SE NUMBER                                
SAVCPY   DS    XL(L'SRTCPY)        ACC COMPANY CODE                             
SAVAGY   DS    CL(L'SRTAGY)        AGENCY ALPHA                                 
SAVMED   DS    CL(L'SRTMED)        MEDIA                                        
SAVREP   DS    CL(L'SRTREP)        REP                                          
SAVPUB   DS    CL(L'SRTPUB)        PUBFILE                                      
SAVCLI   DS    CL(L'SRTCLI)        CLIENT                                       
*                                                                               
SRTWRK   DS    0CL(SRTLEN)                                                      
SRTK     DS    0C                  SORT KEY                                     
SRTOID   DS    XL2                 ORIGIN ID                                    
SRTASE   DS    XL1                 ACC SE NUMBER                                
SRTCPY   DS    XL1                 ACC COMPANY CODE                             
SRTAGY   DS    CL(L'REQAGY)        AGENCY ALPHA                                 
SRTMED   DS    CL(L'REQMED)        MEDIA                                        
SRTREP   DS    CL(L'REQREP)        REP                                          
SRTPUB   DS    CL(L'REQPUB)        PUBFILE                                      
SRTCLI   DS    CL(L'REQCLI)        CLIENT                                       
SRTLAST  EQU   *                                                                
SRTBKLNQ EQU   *-SRTK              CONTROL BREAK                                
SRTPROD  DS    CL(L'REQPROD)       PRODUCT                                      
SRTSEQ   DS    PL3                 SEQUENCE                                     
SRTKLEN  EQU   *-SRTK              LENGTH OF SORT KEY                           
SRTORIG  DS    XL2                 ORIGIN NUMBER                                
SRTESTN  DS    CL(L'REQESTN)       ESTIMATE NUMBER                              
SRTINVDC DS    CL(L'REQINVDC)      INVOICE DATE                                 
SRTPDATE DS    XL3                 TRANSACTION POSTING DATE                     
SRTCDTE  DS    XL3                 CONTROL DATE                                 
SRTOAGY  DS    CL(L'REQOAGY)       OVERRIDE AGENCY                              
SRTCOMM  DS    XL1                 # OF COMMENTS IN REQNARR (0-5)               
SRTSEQN  DS    XL1                 CLEARANCE SEQUENCE NUMBER                    
SRTSTRT  DS    CL(L'REQSTRT)                                                    
SRTEND   DS    CL(L'REQEND)                                                     
SRTTYPE  DS    CL1                                                              
SRTTPAY  EQU   C'1'                PAYMENT                                      
SRTTCRM  EQU   C'2'                CREDIT MEMO                                  
SRTTCHK  EQU   C'3'                CHECK - A CASH RECEIPT                       
SRTNET   DS    XL(L'REQNET)        NET                                          
SRTCSHD  DS    XL(L'REQCD)         CASH DISCOUNT                                
SRTCDI   DS    CL1                 CD INDICATOR                                 
SRTCDIC  EQU   C'C'                C=CASH DISCOUNT                              
SRTCDIL  EQU   C'L'                L=LOST CASH DISCOUNT                         
SRTGSTYP DS    CL1                 GST TAX CODE TYPE                            
SRTGST   DS    PL4                 GST TAX AMOUNT                               
SRTOFF   DS    CL2                 OFFICE                                       
SRTINV   DS    CL(L'REQINV)        INVOICE NUMBER                               
*                                                                               
SRTCARD2 DS    0CL80                                                            
SRTPST   DS    0CL66               PST BLOCK                                    
SRTPSTPR DS    CL2                 PST PROVINCE CODE                            
SRTPSTYP DS    CL1                 PST TYPE                                     
SRTPSTAM DS    XL4                 PST AMOUNT                                   
SRTPSTBS DS    PL4                 BASIS AMOUNT                                 
SRTPSTLQ EQU   *-SRTPST            PST ENTRY LENGTH                             
         DS    CL55                N/D                                          
SRTPID   DS    XL2                                                              
SRTPGRS  DS    PL6                                                              
SRTGIND  DS    XL1                 X'80' GROSS IS A CALCULATION                 
SRTBLDTE DS    XL3                 BILLABLE DATE - BINARY (PRINT)               
         DS    CL2                                                              
SRTNARL  DS    XL1                 LENGTH OF NARRATIVE                          
SRTNARR  DS    CL(L'REQNARR)       COMMENTS (5 COMMENTS CL40 EACH)              
*                                                                               
SRTCTRY  DS    CL1                 COUNTRY CODE                                 
SRTPROFL DS    XL16                PROFILES                                     
SRTLEN   EQU   *-SRTK              LENGTH OF KEY + EXTRA DATA                   
         EJECT                                                                  
**********************************************************************          
* OFFICE TABLE (BY MEDIA AND AGENCY)                                 *          
**********************************************************************          
         SPACE 1                                                                
OFNTAB   DS    0D                  OFFICE TABLE OF CODES AND NAMES              
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(OFNLNQ)         LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(OFNKLNQ)        KEY LENGTH                                   
         DC    AL4(OFNMAX)         MAX IN TABLE                                 
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISPLACEMENT TO FIRST BUCKET                 
         DS    (OFNMAX*OFNLNQ)XL1  TABLE                                        
*                                                                               
OFFMTAB  DS    0D                  OFFICE TABLE BY MEDIA                        
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(OFFLNQ)         LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(OFFKLNQ)        KEY LENGTH                                   
         DC    AL4(OFFMAX)         MAX IN TABLE                                 
         DC    AL1(OFFBKCT)        NUMBER OF BUCKETS                            
         DC    AL1(OFFBKT-OFFD)    DISPLACEMENT TO FIRST BUCKET                 
         DS    (OFFMAX*OFFLNQ)XL1  TABLE                                        
*                                                                               
OFFATAB  DS    0D                  OFFICE TABLE BY AGENCY                       
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(OFFLNQ)         LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(OFFKLNQ)        KEY LENGTH                                   
         DC    AL4(OFFMAX)         MAX IN TABLE                                 
         DC    AL1(OFFBKCT)        NUMBER OF BUCKETS                            
         DC    AL1(OFFBKT-OFFD)    DISPLACEMENT TO FIRST BUCKET                 
         DS    (OFFMAX*OFFLNQ)XL1  TABLE                                        
*                                                                               
OFNMAX   EQU   1000                                                             
OFFMAX   EQU   1000                                                             
         EJECT                                                                  
***********************************************************************         
*              I/O AREAS                                              *         
***********************************************************************         
                                                                                
         DS    0D                                                               
         DC    CL8'*ACCUMS*'                                                    
ACCUMS   DC    (NROW*CSHC*NTYP)PL8'0'                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*TWORK**'       TRANSACTON POSTING BLOCK                     
TWORKL   DC    F'0'                                                             
TWORK    DC    XL(PSHEADL)'00'                                                  
TWORKT   DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*REQIO**'       REQUEST INPUT AREA                           
REQIO    DC    10000X'00'                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO1** '       ACC/PRINT FILE INPUT AREA                    
IO1      DC    4000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO2** '                                                    
IO2      DC    4000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*CONIO**'       CONTROL FILE INPUT AREA                      
CONIO    DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'POSTBUF*'       BUFFER FOR POSTING WRKFILE                   
POSTBUF  DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*ODDBUF*'       BUFFER FOR ODDS WRKFILE                      
ODDBUF   DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*ONLBUF*'       BUFFER OF ONLINE WRKFILE                     
ONLBUF   DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*AGYTAB*'       AGENCY CONTROL TABLE                         
AGYTAB   DC    7000X'00'                                                        
*                                                                               
         DS    0D                                                               
MIMAX    EQU   20000                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*PROFBUF'       BUFFER FOR CONTROL PROFILES                  
PROFBUF  DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*TRKBLK*'       BUFFER FOR FULL TRACK READ                   
TRKBLK   DC    56000X'00'                                                       
*                                                                               
         BUFF  LINES=50,ROWS=1,COLUMNS=10,FLAVOR=PACKED,KEYLIST=(26,A)          
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,MACRF=GM,EODAD=PUTSX,            X        
               RECFM=VB,LRECL=8200                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACOTRNTO                                                       
***********************************************************************         
* REQUEST RECORD DSECT                                                *         
***********************************************************************         
                                                                                
REQD     DSECT                                                                  
REQREC   DS    0CL80                                                            
REQPROG  DS    CL2                 C'30' FOR CHECK ELEMENTS                     
REQAGY   DS    CL2                                                              
REQMED   DS    CL1                                                              
REQCLI   DS    CL3                                                              
REQOAGY  DS    CL2                                                              
REQCOMM  DS    CL1                X'80' = THERE IS A CONTINUATION CARD          
*                                         LOW 4 BITS SEQ#, 00 IS FIRST          
*                                 X'20' = COMMENTS ATTATCHED TO RECORD          
REQPROD  DS    CL3                                                              
REQREP   DS    CL4                                                              
REQTYPE  DS    CL1                                                              
REQTODAY DS    XL3                 TODAY'S DATE                                 
REQCDTE  DS    XL3                 CLEARANCE DATE (CONTROL DATRE)               
REQSEQN  DS    XL1                                                              
REQPUB   DS    CL11                                                             
REQSTRT  DS    CL6                                                              
REQEND   DS    CL6                                                              
REQNET   DS    CL4                                                              
REQCD    DS    CL4                                                              
REQGST   DS    CL4                                                              
REQCDIND DS    CL1                 CD INDICATOR                                 
REQCDIC  EQU   C'C'                C=CASH DISCOUNT                              
REQCDIL  EQU   C'L'                L=LOST CASH DISCOUNT                         
REQESTN  DS    CL2       B         EST. NUMBER IF PAID BY ESTIMATE              
REQINVDC DS    CL2                 INVOICE DATA COMPRESSED                      
REQGSTYP DS    CL1                 GST TYPE                                     
REQOFF   DS    CL2                 OFFICE                                       
REQINV   DS    CL11                                                             
         ORG   REQREC+L'REQREC                                                  
*                                                                               
REQCARD2 DS    0CL80                                                            
REQPST   DS    0CL66               PST BLOCK                                    
REQPSTPR DS    CL2                 PST PROVINCE CODE                            
REQPSTYP DS    CL1                 PST TYPE                                     
REQPSTAM DS    XL4                 PST AMOUNT                                   
REQPSTBS DS    XL4                 PST BASIS AMOUNT                             
         ORG   REQPST+L'REQPST                                                  
REQPID   DS    XL2                 PID NUMBER                                   
REQPGRS  DS    PL6                 GROSS AMOUNT                                 
REQGIND  DS    XL1                 X'80' GROSS IS A CALCULATION                 
*                                  X'40' REQPSTBS IS A XL4 VS PL4               
REQBLDTE DS    XL3                 BILLABLE DATE - BINARY (PRINT)               
         DS    CL2                 SPARE                                        
REQNARR  DS    CL200               CONCATENATED FROM COMMENT RECORD             
REQLEN   EQU   *-REQD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
                                                                                
PLINED   DSECT                                                                  
PLINE    DS    0CL132              ENTIRE PRINT LINE                            
         DS    CL1                                                              
PANAL    DS    0CL18               ANALYSIS LINE                                
PREP     DS    CL4                 REP                                          
         DS    CL1                                                              
PPUB     DS    CL5                 STATION                                      
         ORG   PLINE+15                                                         
PCLIENT  DS    CL3                                                              
         DS    CL2                                                              
PCLINAME DS    CL20                                                             
         ORG   PLINE+15                                                         
POFFCDE  DS    CL2                                                              
         DS    CL3                                                              
POFFNME  DS    CL20                                                             
         ORG   PLINE+22                                                         
PINVOICE DS    CL11                                                             
         ORG   PLINE+33                                                         
PTOTLINE DS    0CL25               TOTALS                                       
PDATE    DS    0CL13                                                            
PSTART   DS    CL6                 START DATE                                   
         DS    CL1                                                              
PEND     DS    CL6                 END DATE                                     
         DS    CL1                                                              
PDATE1   DS    CL8                                                              
         ORG   PLINE+47                                                         
PPRODNM  DS    0CL20               PRODUCT NAME                                 
PTOTLIN2 DS    CL18                TOTAL LINE                                   
         ORG   PLINE+68                                                         
PTOTCR   DS    0CL12               CASH RECIEPT TOTAL                           
         DS    CL7                                                              
PTYPE    DS    CL2                 TYPE (REGULAR, ETC...)                       
         ORG   PLINE+82                                                         
PTOTGST  DS    CL12                GST TOTAL                                    
         ORG   PLINE+94                                                         
PTOTPST  DS    CL12                PST TOTAL                                    
         ORG   PLINE+105                                                        
PTOTCD   DS    CL12                CASH DISCOUNT TOTAL                          
         ORG   PLINE+120                                                        
PTOTNET  DS    CL12                NET TOTAL                                    
         ORG   PLINE+L'PLINE                                                    
         EJECT                                                                  
**********************************************************************          
* DSECT FOR ENTRY IN OFFICE TABLE                                    *          
**********************************************************************          
                                                                                
OFFD     DSECT                                                                  
OFFCDE   DS    CL2                 OFFICE CODE                                  
OFFKLNQ  EQU   *-OFFD              LENGTH OF KEY                                
OFFNME   DS    CL36                OFFICE NAME                                  
OFFBKT   DS    0PL8                START OF BUCKETS                             
OFFCR    DS    PL8                 OFFICE BUCKET - CASH RECEIPTS                
OFFBKLN  EQU   *-OFFBKT            BUCKET LENGTH                                
OFFGST   DS    PL8                 OFFICE BUCKET - GST                          
OFFPST   DS    PL8                 OFFICE BUCKET - PST                          
OFFCD    DS    PL8                 OFFICE BUCKET - CASH DISCOUNTS               
OFFNET   DS    PL8                 OFFICE BUCKET - NET                          
OFFBKCT  EQU   (*-OFFBKT)/OFFBKLN  NUMBER OF BUCKETS                            
OFFLNQ   EQU   *-OFFD              LENGTH                                       
                                                                                
                                                                                
**********************************************************************          
* DSECT FOR OFFICE NAME TABLE                                        *          
**********************************************************************          
                                                                                
OFND     DSECT                                                                  
OFNCDE   DS    CL2                 OFFICE CODE                                  
OFNKLNQ  EQU   *-OFND              LENGTH OF KEY                                
OFNNME   DS    CL36                OFFICE NAME                                  
OFNLNQ   EQU   *-OFND              LENGTH                                       
                                                                                
                                                                                
**********************************************************************          
* DSECT FOR ENTRY IN OFFICE TABLE                                    *          
**********************************************************************          
                                                                                
MIOELD   DSECT                     ** MEDIA INTERFACE ELEMENT **                
MIOEL    DS    0C                                                               
MIOCODE  DS    CL2                 MEDIA CODE                                   
MIOOFF   DS    CL2                 OFFICE CODE OR SPACES                        
MIODATA  DS    0CL(MIODLNQ)        START OF DATA AREA                           
MIODESC  DS    CL12                  MEDIA DESCRIPTION                          
MIOCOMM  DS    CL14                  COMMISSION ACCOUNT                         
MIOCSHD  DS    CL14                  CASH DISCOUNT ACCOUNT                      
MIOCSHR  DS    CL14                  CASH RECEIPT ACCOUNT                       
MIOCCSR  DS    CL14                  CANADIAN CASH RECEIPT ACCOUNT              
MIOCNTL  DS    CL14                  MEDIA CONTROL ACCOUNT                      
MIOCOST  DS    CL12                  COST ANALYSIS (11 AND 12)                  
MIOLOST  DS    CL14                  LOST CASH DISCOUNT ACCOUNT                 
MIOCONL  DS    CL14                  COMMISSION ONLY CONTROL                    
         DS    XL12                  N/D                                        
MIODLNQ  EQU   *-MIODESC           LENGTH OF DATA AREA                          
MIOLNQ   EQU   *-MIOEL                                                          
**********************************************************************          
* DSECT FOR PPGETADDR CALL                                           *          
**********************************************************************          
                                                                                
GETPD    DSECT                                                                  
GETPAGM  DS    0CL3                AGENCY MEDIA                                 
GETPAGY  DS    CL2                 AGENCY                                       
GETPMED  DS    CL1                 MEDIA                                        
GETPCLI  DS    CL3                 CLIENT                                       
GETPOFC  DS    CL1                 OFFICE                                       
         EJECT                                                                  
**********************************************************************          
* DSECT FOR BINSEARCH PARAMETERS                                     *          
**********************************************************************          
                                                                                
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
                                                                                
                                                                                
***********************************************************************         
* DSECT FOR ID CONTROL TABLE                                          *         
***********************************************************************         
                                                                                
CND      DSECT                                                                  
CNAGY    DS    CL2       C         AGENCY ALPHA CODE                            
CNASE    DS    CL1       B         ACCOUNT SE NUMBER                            
CNACD    DS    CL1       B         ACCOUNT AGENCY CODE                          
CNCTRY   DS    CL1       B         AGENCY COUNTRY CODE                          
CNLNQ    EQU   *-CND                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT FOR OFFILE RECORD                                             *         
***********************************************************************         
                                                                                
ODDRECD  DSECT                                                                  
ODDREC   DS    0C                                                               
ODDLEN   DS    XL4                                                              
ODDACC   DS    CL1                 ACCOUNTING AGENCY CODE                       
ODDID    DS    CL2                 CONTL AGENCY ID                              
ODDUTL   DS    CL1                 UTL NUMBER                                   
ODDAGY   DS    CL2                 PRINT OR PRINT AGENCY CODE                   
ODDAMT   DS    PL6                 AMOUNT FROM CLEARANCE AC53                   
ODDNAM   DS    CL24                AGENCY NAME                                  
ODDCR    DS    PL5                 CASH RECEIPT                                 
ODDSE    DS    CL1                 ACCOUNT SE NUMBER                            
ODDCOMO  DS    PL6                 COMMISSION ONLY                              
ODDLNQ   EQU   *-ODDREC                                                         
         EJECT                                                                  
***********************************************************************         
* DSECTS FOR RECOVERY/REQUEST HEADERS                                 *         
***********************************************************************         
                                                                                
RECVD    DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACVATICAND                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBUFFALOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
* DDCOMFACSC                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* PRINTPAK                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRINTPAK                                                       
         PRINT ON                                                               
* PAGYREC                                                                       
         PRINT OFF                                                              
       ++INCLUDE PAGYREC                                                        
         PRINT ON                                                               
* PCLTREC                                                                       
         PRINT OFF                                                              
       ++INCLUDE PCLTREC                                                        
         PRINT ON                                                               
* PPRDREC                                                                       
         PRINT OFF                                                              
       ++INCLUDE PPRDREC                                                        
         PRINT ON                                                               
* PREPREC                                                                       
         PRINT OFF                                                              
       ++INCLUDE PREPREC                                                        
         PRINT ON                                                               
* PUBREC                                                                        
         PRINT OFF                                                              
       ++INCLUDE PUBREC                                                         
         PRINT ON                                                               
* PPGETADRD                                                                     
         PRINT OFF                                                              
       ++INCLUDE PPGETADRD                                                      
         PRINT ON                                                               
* PUBNAMEL                                                                      
         PRINT OFF                                                              
       ++INCLUDE PUBNAMEL                                                       
         PRINT ON                                                               
* PUBAOVEL                                                                      
         PRINT OFF                                                              
       ++INCLUDE PUBAOVEL                                                       
         PRINT ON                                                               
* DDCROSSD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCROSSD                                                       
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045ACREP5402 06/13/18'                                      
         END                                                                    
