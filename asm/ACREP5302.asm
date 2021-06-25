*          DATA SET ACREP5302  AT LEVEL 096 AS OF 11/03/16                      
*PHASE AC5302A                                                                  
*INCLUDE CENTER                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETLOGO                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE PQPROF                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE VATICAN                                                                
         TITLE 'AC5302 - SPOT CLEARANCES'                                       
AC5302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**5302**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         L     RC,=A(AC53C)                                                     
         USING AC53C,RC                                                         
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
* QSELECT FIELD :  MEDIA FILE (SPOT1, NET3, ETC)                      *         
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
         BAS   RE,REMCK            CHECK REMOTE                                 
         BAS   RE,INDXR            BUILD INDEX RECORDS                          
         BAS   RE,BLDOF            BUILD OFFICE CODES AND NAMES                 
         BAS   RE,BLDMI            BUILD MI TABLE                               
*                                                                               
MAIN5    GOTO1 DIVIDE,SLOGO        START LOGO                                   
         BAS   RE,SAGYH            READ SPOT AGENCY HEADER                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
MAIN6    CLC   COMREP,=C'000'      TEST ANY COMMISSION ONLY REP                 
         BE    MAIN7                                                            
         CLC   SRTREP,COMREP       IS THIS THE REP?                             
         BNE   MAIN7                                                            
         OI    ITEMS,ITMCOMO       SET COMMISSION ONLY REP                      
*                                                                               
MAIN7    BAS   RE,MEDNM            MEDIA CODE/NAME LOOK UP                      
         BAS   RE,CLIR             GET CLIENT RECORD                            
         BAS   RE,PRDR             GET PRODUCT RECORD                           
         BAS   RE,STAR             GET STATION/REP RECORD                       
         BAS   RE,GLDGR            GET ACC LEDGER  RECORD VALUES                
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
         TM    BREAK,STATQ                                                      
         BNO   *+8                                                              
         BAS   RE,STATOT           STATION TOTAL                                
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
         TM    BREAK,AGYTQ         TEST BREAK ON SPOT AGENCY                    
         BO    MAIN5               GET NEW SPOT AGENCY HEADER                   
         B     MAIN6               SOME OTHER BREAK                             
*                                                                               
MAINX    XC    SAVOID,SAVOID       NEEDED BY FILE RTN---CLOSWRK/CLOSODD         
         XC    SRTOID,SRTOID       NEEDED BY DIVIDE RTN                         
         BAS   RE,RUNTOT                                                        
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         GOTO1 ADMGR,ODDCLO        CLOSE ODDS FILES                             
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
INIT3    LA    RF,SPOT1ST                                                       
         CLC   QSELECT(4),=C'SPOT'                                              
         BE    INIT5                                                            
         LA    RF,NET1ST                                                        
         CLC   QSELECT(3),=C'NET'                                               
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
         EJECT                                                                  
***********************************************************************         
* NET? (?=NET SYSTEM NUMBER ...NET1 .... NET2)                        *         
***********************************************************************         
NET1ST   NTR1  ,                                                                
         MVC   SYSNMEL,QSELECT+3                                                
         GOTO1 DATAMGR,DMCB,(0,DDNAME),SYSNME,0                                 
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    *+2                                                              
         L     RE,8(,R1)           A(DDNADATA)                                  
*                                                                               
         USING DDNAMED,RE                                                       
NET300   MVC   REQFIL+4(2),DDNASENA+3 SPOT EQUIVELENT                           
         DROP  RE                                                               
*                                                                               
         MVC   SYSNUMB,QSELECT        SYSTEM (NET)                              
         MVC   SPTNUM,QSELECT                                                   
         MVI   SEN,SENNET            SE OVERLAY 3 FOR NET                       
         B     SPTOPNF                                                          
*                                                                               
DDNAME   DC    CL8'DDNAME'                                                      
SYSNME   DC    C'SYS=NET  '                                                     
         ORG   SYSNME+7                                                         
SYSNMEL  DS    CL2                                                              
         EJECT ,                                                                
***********************************************************************         
* SPOT? (?=SPOT SYSTEM NUMBER...SPOT1.... SPOT2)                      *         
*  OPEN MEDIA FILES                                                   *         
***********************************************************************         
                                                                                
SPOT1ST  NTR1  ,                                                                
         MVC   REQFIL+4(2),QSELECT+4     SPOT1-7 OR B-E...                      
         MVC   SYSNUMB,QSELECT                                                  
         MVC   SPTNUM,QSELECT                                                   
         MVI   SEN,SENSPOT              SE OVERLAY 2 FOR SPOT                   
*                                                                               
SPTOPNF  L     R2,ACONIO                                                        
         USING CTWREC,R2                                                        
         XC    CTWKEY,CTWKEY       GET SYSTEM LIST RECORD                       
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         MVI   CTWKSYSN,CTWKSPOT   FOR SPOT                                     
         CLI   SEN,SENSPOT                                                      
         BE    *+8                                                              
         MVI   CTWKSYSN,CTWKNET    OR NET                                       
         MVC   DKEY,0(R2)                                                       
         GOTO1 ADMGR,CONRD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,CTWDATA                                                       
SPTOPN3  CLI   0(R1),X'A4'         SYSTEM LIST ELEMENT                          
         BE    SPTOPN9                                                          
         CLI   0(R1),0             END OF RECORD                                
         BNE   *+6                 NO SE NUMBER ?                               
         DC    H'0'                                                             
*                                                                               
SPTOPN5  ZIC   R2,1(R1)                                                         
         AR    R1,R2                                                            
         B     SPTOPN3                                                          
*                                                                               
         USING SYSELD,R1                                                        
SPTOPN9  CLC   SYSSYS,SEN          BASE SYSTEM NUMBER                           
         BNE   SPTOPN5             SKIP IT                                      
         CLC   SPTNUM,SYSNAME      MATCH LOGICAL SYSTEM NUMBER                  
         BNE   SPTOPN5             GET ANOTHER                                  
         MVC   SPOTSEN,SYSSEN      SET SE NUMBER                                
         GOTO1 ADMGR,SPTOPN                                                     
         B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* PUT REQUEST TO SORT                                                 *         
***********************************************************************         
                                                                                
PUTS     NTR1  ,                                                                
         CLI   QOPT4,C'R'          READ RECOVERY FILE                           
         BNE   PUTS04                                                           
         L     R2,=A(RECVIN)                                                    
         OPEN  ((R2),(INPUT))      OPEN FILE                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUTS02   L     R3,AREQIO                                                        
         CLI   QOPT4,C'R'          READ RECOVERY FILE                           
         BNE   PUTS04                                                           
         GET   (R2),(R3)                                                        
         LA    R3,4(R3)                                                         
         USING RECVD,R3                                                         
         CLI   RFILTY,SPTREQQ      TEST SPOT REQUEST                            
         BNE   PUTS02                                                           
         CLI   RPRG,SPTPAYQ        PAY PROGRAM                                  
         BNE   PUTS02                                                           
         LA    R3,L'RECVHDR(R3)                                                 
         USING RQHHDRD,R3          NEW 80 BYTE HEADER                           
         MVC   ORIGNUM,RQHORIG     SAVE ORIGIN ID                               
         LA    R3,L'RQHHDR(R3)     R3=A(REQUEST CARD)                           
         B     PUTS06                                                           
*                                                                               
PUTS04   GOTO1 ADMGR,SPTREQ                                                     
         BNE   PUTSX               EOF                                          
         L     R3,AREQIO                                                        
         USING REQOFFC,R3                                                       
         MVC   ORIGNUM,REQORIG     ORIGIN - FROM OLD HEADER                     
         LA    R3,L'RQHIUSR(R3)    ADD LENGTH OF OLD HEADER                     
*                                                                               
         USING REQD,R3                                                          
PUTS06   CLC   REQPROG(2),=C'30BN' FILTER OUT THE NON-CHECK ELEMENTS            
         BNE   PUTS02                                                           
         BAS   RE,FILTR                                                         
         BNE   PUTS02                                                           
*                                                                               
         CLC   REQTODAY,SPACES     DATE FIELD SPACES OR NULL ?                  
         BNH   PUTS02              YES, SKIP  CARD                              
         GOTO1 DATCON,DMCB,(0,REQTODAY),(X'20',REQTODAY)                        
         CLC   REQTODAY,TODAY6                                                  
         BE    PUTS08                                                           
         OC    XDATE,XDATE         ADDITIONAL CLEARANCE DATE                    
         BZ    PUTS02                                                           
         CLC   REQTODAY,XDATE                                                   
         BNE   PUTS02                                                           
*                                                                               
PUTS08   GOTO1 GETDAY,DMCB,REQSTRT,WORK                                         
         CLC   WORK(3),SPACES                                                   
         BE    PUTS02                                                           
         CLC   REQSTRT,SPACES      START DATE  SPACES OR NULL ?                 
         BNH   PUTS10              YES,  SKIP                                   
         GOTO1 DATCON,DMCB,(0,REQSTRT),(X'20',REQSTRT)                          
*                                                                               
PUTS10   CLI   REQEND+2,X'40'      END   MONTH SPACES OR NULL ?                 
         BNH   PUTS12              YES,  SKIP                                   
         GOTO1 DATCON,DMCB,(0,REQEND),(X'20',REQEND)                            
*                                                                               
PUTS12   LA    R0,SPACES           CLEAR SRT AREA TO SPACES                     
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         LA    RE,SRTWRK                                                        
         LH    RF,=Y(L'SRTWRK)                                                  
         MVCL  RE,R0                                                            
*                                                                               
         XC    SRTOID,SRTOID                                                    
         MVC   SRTCPY,CPYCODE      ACC COMPANY CODE                             
         MVC   SRTAGY,REQAGY       SPOT AGENCY CODE                             
         MVC   SRTMED,REQMED       MEDIA                                        
         MVC   SRTREP,REQREP       REP                                          
         MVC   SRTREPT,REQREPT     TYPE                                         
         MVC   SRTSTA,REQSTA       STATION                                      
         MVC   SRTCLI,REQCLI       CLIENT                                       
         MVC   SRTPROD,REQPROD     PRODUCT                                      
         AP    SEQUENCE,=P'1'                                                   
         ZAP   SRTSEQ,SEQUENCE                                                  
*                                                                               
         MVC   SRTSMED,REQSMED     SUB-MEDIA (NET)                              
         OC    SRTSMED,SPACES                                                   
         MVC   SRTORIG,ORIGNUM                                                  
         MVC   SRTBMKT,REQBMKT     BINARY MARKET NUMBER                         
         MVC   SRTESTN,REQESTN     ESTIMATE NUMBER                              
         MVC   SRTSTAT,REQSTAT     STATUS                                       
         GOTO1 DATCON,DMCB,(0,REQTODAY),(1,SRTDAYP)                             
         MVC   SRTOAGY,REQOAGY     OVERRIDE AGENCY                              
         MVC   SRTCOMM,REQCOMM     # OF COMMENTS IN REQNARR (0-5)               
         MVC   SRTSEQN,REQSEQN     CLEARANCE SEQUENCE NUMBER                    
         MVC   SRTSTRT,REQSTRT                                                  
         MVC   SRTEND,REQEND                                                    
         MVC   SRTPROD2,REQPROD2                                                
         MVC   SRTPROD3,REQPROD3                                                
         MVC   SRTTYPE,REQTYPE                                                  
         XC    SRTPNET,SRTPNET     WE HAD TO CHANGE SRTPNET TO 6 BYTES          
         MVC   SRTPNET+1(L'REQPNET),REQPNET   TO HANDLE LARGE AMNTS             
         OC    REQPNET2,REQPNET2                                                
         BZ    PUTS14                                                           
         CLC   REQPNET2,SPACES                                                  
         BE    PUTS14                                                           
         ZAP   SRTPNET,REQPNET2    HANDLES 999,999,999.99                       
                                                                                
PUTS14   MVC   SRTGSTYP,REQGSTYP   GST TAX CODE TYPE                            
         MVC   SRTPGST,REQPGST     GST TAX AMOUNT                               
         OC    SRTPGST,SRTPGST     ANY GST AMOUNT ?     EXPECT BINARY           
         BZ    PUTS16              NO, SO LEAVE ALONE                           
         CLI   SRTGSTYP,0          GST TYPE SUPPLIED ?                          
         BNE   PUTS16                                                           
         MVI   SRTGSTYP,C'S'       IF NOT SUPPLIED THEN SET TO "S"              
*&&DO                                                                           
PUTS16   TM    REQNET,X'F0'        NET PAY PROGRAM STILL USES CHARACTER         
         BNO   PUTS16                                                           
         PACK  SRTPNET,REQNET      FITS 999,999,999.99                          
         MVC   SRTGSTYP,SPACES     THEN THERE IS NOT GST                        
*        MVC   SRTPGST,SPACES                                                   
         XC    SRTPGST,SRTPGST                                                  
*&&                                                                             
PUTS16   MVC   SRTOFF,REQOFF       OFFICE                                       
         MVC   SRTINV,REQINV       INVOICE NUMBER                               
         MVC   SRTCARD2,REQCARD2   PST BLOCK/CTA BLOCK                          
         OC    SRTCARD2,SRTCARD2                                                
         BNZ   *+10                                                             
         MVC   SRTCARD2,SPACES                                                  
         MVC   SRTPNET2(L'REQPNET),REQPNET    IN BOTH PLACES FOR NOW            
         ZAP   SRTSLUSH,=P'0'      ROUNDING DIFFERENCE INIT TO 0                
         ZAP   SRTINVAM,=P'0'      INVOICE AMOUNT INIT TO 0                     
         CLC   QSELECT(3),=C'NET'                                               
         BE    PUTS20                                                           
         CLC   REQSLUSH,SPACES     ANY ROUNDING DIFFERENCE?                     
         BE    PUTS18                                                           
         OC    REQSLUSH,REQSLUSH                                                
         BZ    PUTS18                                                           
         MVC   SRTSLUSH,REQSLUSH   ROUNDING DIFFERENCE                          
PUTS18   CLC   REQINVAM,SPACES     ANY INVOICE AMOUNT?                          
         BE    PUTS20                                                           
         OC    REQINVAM,REQINVAM                                                
         BZ    PUTS20                                                           
         MVC   SRTINVAM,REQINVAM   INVOINCE AMOUNT                              
PUTS20   CLC   SRTPGRS,SPACES      IS THERE A GROSS AMOUNT?                     
         BE    PUTS22              NO, SET TO ZERO                              
         OC    SRTPGRS,SRTPGRS                                                  
         BNZ   *+10                                                             
PUTS22   ZAP   SRTPGRS,=P'0'       NO INITIALIZE IT TO ZERO                     
                                                                                
         CLI   SRTCOMM,0           *** COMMENTS NEW WAY ***                     
         BE    PUTS24                                                           
         CLI   SRTCOMM,5           REQCOMM > 5 = SOMETHING WRONG                
         BH    PUTS24                                                           
         SR    R1,R1                                                            
         IC    R1,SRTCOMM          # COMMENTS                                   
         MHI   R1,40               L'COMMENT                                    
         SHI   R1,1                                                             
         BM    PUTS24                                                           
         EXMVC R1,SRTNARR,REQNARR                                               
*                                                                               
PUTS24   MVC   SRTASE,ACCTSEN       SAVE ACC SE NUMBER                          
         MVC   SRTPROFL,PROFILE     AND PROFILES                                
         MVC   SRTCTRY,CTRY         COUNTRY CODE                                
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTWRK                                     
         LA    R5,PROFIL9           SEPARATE REPORTS FOR SPOT                   
         CLI   SEN,SENSPOT                                                      
         BE    *+8                                                              
         LA    R5,PROFIL10          SEPARATE REPORTS FOR NET                    
         CLI   0(R5),C'Y'                                                       
         BNE   PUTS02                                                           
         MVC   SRTOID,ORIGNUM      ORIGIN NUMBER                                
         BASR  RE,RF               2ND PUT SORT                                 
         B     PUTS02                                                           
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
         MVC   CRACCT,=CL12'C003'  CASH RECEIPT CONTROL                         
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
         CLC   SRTREP,=3C'0'       TEST REP                                     
         BE    GETS4                                                            
         CLC   SRTREP,=3X'00'                                                   
         BE    GETS4                                                            
         OI    ITEMS,ITMREP        SET ITEM REP STATUS                          
*                                                                               
GETS4    DS    0H                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ SPOT AGENCY HEADER                                             *         
***********************************************************************         
                                                                                
SAGYH    NTR1  ,                                                                
         XC    DKEY,DKEY                                                        
         MVI   DKEY,X'06'                                                       
         MVC   DKEY+1(2),SRTAGY    AGENCY CODE                                  
*                                                                               
         GOTO1 ADMGR,SPTHI                                                      
         CLC   DIR(13),DKEY                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ADMGR,SPTGET                                                     
         L     R4,AIO1                                                          
         USING AGYHDR,R4                                                        
*                                                                               
         SR    R1,R1                                                            
         MVC   AGYNM,AGYNAME                                                    
         OC    AGYNM,SPACES                                                     
         IC    R1,AGYPROF+19        BINARY AGENCY CODE                          
         CLI   AGYPROF+19,X'F0'                                                 
         BNL   *+8                                                              
         LA    R1,9(R1)                                                         
         SLL   R1,4                                                             
         STC   R1,AGYCODE                                                       
         MVC   SAVPRD,SPACES            CLEAR PRODUCT                           
         MVC   CLINAME,SPACES           CLIENT NAME                             
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
         MVC   COMREP,WORK+2                                                    
         MVC   CMPALPHA,CPYALPHA   COMPANY ALPHA CODE                           
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
* GET ACC LEDGER RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
GLDGR    NTR1  ,                                                                
         CLC   SAVSLSHC,SPACES     DO WE HAVE A SLUSH ACCOUNT?                  
         BH    GLDGRX                                                           
*                                                                               
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(1),SRTCPY      READ COMPANY RECORD                          
         MVC   DKEY+1(2),=C'SS'                                                 
         CLI   COUNTRY,C'C'                                                     
         BNE   *+8                                                              
         MVI   DKEY+2,C'T'         CANADIAN                                     
         CLI   SRTMED,C'N'                                                      
         BNE   GLDGR2              IS MEDIA NETWORK                             
         CLI   PROFIL6,C'Y'        USE LEDGER "U" FOR NETWORK                   
         BNE   GLDGR2                                                           
         MVI   DKEY+2,C'U'         USE LEDGER U FOR NETWORK                     
GLDGR2   GOTO1 ADMGR,ACCRD                                                      
         BNE   GLDGR8              IF NOT EQUAL MUST BE WRONG SETUP             
         GOTO1 ADMGR,ACCGET                                                     
         L     R4,AIO1                                                          
         AH    R4,FRSTL                                                         
         SR    R1,R1                                                            
*                                                                               
GLDGR5   CLI   0(R4),0                                                          
         BE    GLDGR8                                                           
         CLI   0(R4),FFTELQ        X'DB' - FREEFORM TEXT ELEMENT                
         BE    GLDGR7                                                           
GLDGR6   IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GLDGR5                                                           
*                                                                               
         USING FFTELD,R4                                                        
GLDGR7   CLI   FFTTYPE,FFTTSLAC       DO WE HAVE A SLUSH ACCOUNT?               
         BNE   GLDGR6                                                           
         SR    R1,R1                                                            
         IC    R1,FFTDLEN             LENGTH OF U/L/A                           
         AHI   R1,-1                                                            
         MVC   SAVSLSHC(0),FFTTSLSH   SAVE OFF SLUSH ACCOUNT                    
         EX    R1,*-6                                                           
         B     GLDGR6                                                           
         DROP  R4                                                               
*                                                                               
GLDGR8   CLC   SAVSLSHC,SPACES                                                  
         BH    GLDGRX                                                           
         MVC   SAVSLSHC(2),DKEY+1                                               
         MVC   SAVSLSHC+2(12),=CL12'SLUSH'    SET DEFAULT ACCOUNT               
GLDGRX   B     EXIT                                                             
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
         MVC   UKSYSPRG,=C'S54'                                                 
         L     RF,=A(REQFIL)                                                    
         MVC   UKSUBPRG,4(RF)                                                   
         MVC   UKEXTRA,5(RF)                                                    
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
         MVC   OFNNME(0),NAMEREC                                                
         EX    R1,*-6                                                           
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
         MVC   DKEY+1(1),AGYCODE      AGENCY CODE                               
         OC    DKEY+1(1),MEDCODE      MEDIA CODE                                
         GOTO1 CLPACK,DMCB,SRTCLI,DKEY+2                                        
*                                                                               
CLIR2    GOTO1 ADMGR,SPTHI                                                      
         CLC   DIR(13),DKEY                                                     
         BE    CLIR3                                                            
         BAS   RE,NOSPOT           CAN'T READ SPOT FILE                         
         MVC   CLINAME(7),=CL10'MISSING'                                        
         B     EXIT                                                             
*                                                                               
CLIR3    GOTO1 ADMGR,SPTGET                                                     
         L     R4,AIO1                                                          
         USING CLTHDR,R4                                                        
         MVC   CLINAME,CNAME                                                    
         MVC   OFFICE,SPACES                                                    
         MVC   OFFICE(1),COFFICE                                                
         TM    CMPSTA4,CPYSOFF2    NEW 2 BYTE OFFICES                           
         BNO   CLIR4                                                            
         CLI   CACCOFC,C' '        2 BYTE OFFICE                                
         BNH   CLIR4                                                            
         MVC   OFFICE,CACCOFC                                                   
*                                                                               
CLIR4    OC    OFFICE,SPACES                                                    
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
         CLI   PROFIL5,C'Y'        USE PRODUCT NAME NOT CODE                    
         BE    PRDR1                                                            
         MVC   PRDNAME(3),=C'ALL'                                               
         CLC   SRTPROD,=C'ALL'                                                  
         BE    EXIT                                                             
*                                                                               
PRDR1    XC    DKEY,DKEY                                                        
         MVC   DKEY+1(1),AGYCODE   BUILD KEY FOR PRODUCT READ                   
         OC    DKEY+1(1),MEDCODE                                                
         GOTO1 CLPACK,DMCB,SRTCLI,DKEY+2                                        
         MVC   DKEY+4(3),SRTPROD                                                
         CLI   PROFIL5,C'Y'        USE PRODUCT NAME NOT CODE                    
         BNE   PRDR3                                                            
         CLC   SRTPROD,=C'POL'                                                  
         BNE   PRDR3                                                            
         CLC   SRTPROD2,SPACES                                                  
         BE    PRDR3                                                            
         MVC   DKEY+4(3),SRTPROD2                                               
*                                                                               
PRDR3    GOTO1 ADMGR,SPTHI                                                      
         CLC   DIR(13),DKEY                                                     
         BE    PRDR5                                                            
         BAS   RE,NOSPOT           CAN'T READ SPOT FILE                         
         B     EXIT                                                             
*                                                                               
PRDR5    GOTO1 ADMGR,SPTGET                                                     
         L     R4,AIO1                                                          
         USING PRDHDR,R4                                                        
         MVC   PRDNAME,PNAME       SAVE PRODUCT NAME                            
         OC    PRDNAME,SPACES                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* STATION NAME/ADDRESS ROUTINE                                        *         
***********************************************************************         
                                                                                
STAR     TM    ITEMS,ITMREP           TEST REP                                  
         BO    REPR                                                             
STAR1    NTR1  ,                                                                
         CLC   STANM,SPACES           TEST ALREADY HAVE NAME                    
         BNE   EXIT                                                             
         MVI   COUNTRY,C'U'           USA                                       
         LA    R4,DKEY                                                          
         USING ADDRREC,R4                                                       
         MVI   ADDRKEY,C'A'                                                     
         MVC   ADDRKEY+1(1),SRTMED    MEDIA                                     
         MVC   ADDRKEY+2(5),SRTSTA    STATION                                   
         MVC   ADDRKEY+7(2),SRTAGY    AGENCY                                    
         MVC   ADDRKEY+9(8),=8C'0'                                              
         CLI   SRTSTA+4,C' '                                                    
         BNE   *+10                                                             
         MVC   ADDRKEY+6(1),SRTMED                                              
*                                                                               
         GOTO1 ADMGR,STAHI                                                      
         L     R4,AIO1                                                          
         CLC   DKEY(15),0(R4)                                                   
         BE    STAR3                                                            
         BAS   RE,NOSTAT           CAN'T READ STATION FILE                      
         MVC   STANM(5),SRTSTA     USE CODE                                     
         MVI   STAAD,1             NO ADDRESS                                   
         MVC   STAAD+1(26),SPACES                                               
         B     EXIT                                                             
*                                                                               
STAR3    MVC   STANM,ANAME         SAVE STATION NAME                            
         OC    STANM,SPACES                                                     
         MVC   SAVSTAN,STANM       SAVE STAION NAME FOR SLUSH POSTING           
         CLC   AZIP,=C'CANAD'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   COUNTRY,C'C'                                                     
         MVI   STAAD,C' '          SAVE STATION ADDRESS                         
         MVC   STAAD+1(L'STAAD-1),STAAD                                         
         MVI   STAAD,3             NUMBER OF LINES                              
         MVC   STASTR(24),A1LINE   STATION STREET ADDRESS                       
         MVC   STACTY,A2LINE       CITY                                         
         MVC   STASTA,A3LINE       STATE                                        
         CLC   STACTST,SPACES      CITY /STATE                                  
         BE    STAR5                                                            
         GOTO1 ADSQUASH,DMCB,STACTST,(0,26)                                     
*                                                                               
STAR5    MVC   STAZIP(L'AZIP),AZIP                                              
         CLC   ABIGZIP,SPACES                                                   
         BNH   STAR8                                                            
         MVC   STAZIP,SPACES                                                    
         MVC   STAZIP(L'ABIGZIP),ABIGZIP                                        
*                                                                               
STAR8    EQU   *                                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'STACTST+L'STAZIP),STACTST                                 
         GOTO1 ADSQUASH,DMCB,WORK,(0,52)                                        
         CLC   WORK+L'STACTST(L'STAZIP),SPACES                                  
         BNE   STAR9                                                            
         MVC   STACTST,WORK                                                     
         MVC   STAZIP,SPACES                                                    
         MVI   STAAD,2                                                          
STAR9    EQU   *                                                                
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* REP NAME/ADDRESS ROUTINE                                            *         
***********************************************************************         
                                                                                
REPR     NTR1  ,                                                                
*        CLC   REPNAME,SPACES      TEST ALREADY HAVE REP                        
*        BNE   EXIT                                                             
         MVI   COUNTRY,C'U'                                                     
         LA    R4,DKEY                                                          
         USING X_REPREC,R4                                                      
         MVI   X_REPKEY,C'R'                                                    
         MVC   X_REPKEY+1(1),SRTMED                                             
         MVC   X_REPKEY+2(3),SRTREP                                             
         MVC   X_REPKEY+5(2),SRTAGY                                             
         MVC   X_REPKEY+7(10),=10C'0'                                           
         GOTO1 ADMGR,STAHI                                                      
         L     R4,AIO1                                                          
         CLC   DKEY(15),0(R4)                                                   
         BE    REPR3                                                            
         BAS   RE,NOSTAT           CAN'T READ STATION FILE                      
         MVC   REPNAME(7),=CL10'MISSING'                                        
         MVI   REPUBAD,0                                                        
         B     EXIT                                                             
*                                                                               
REPR3    MVC   REPNAME,X_RNAME                                                  
         OC    REPNAME,SPACES                                                   
         CLI   X_RUNWNET,C'Y'      UNWIRED REP                                  
         BNE   *+8                                                              
         OI    ITEMS,ITMUNW        SET UNWIRED FLAG                             
         MVC   REPUBAD,SPACES                                                   
         MVI   REPUBAD,3                                                        
         MVC   REPUBAD+1(24),X_R1LINE                                           
         MVC   REPUBAD+27(24),X_R2LINE                                          
         MVC   REPUBAD+53(03),X_R3LINE                                          
         MVC   REPUBAD+56(5),X_RZIP                                             
         MVC   REPUBAD+61(2),SPACES                                             
         OC    REPUBAD+1(78),SPACES                                             
         CLC   X_RZIP,=C'CANAD'                                                 
         BNE   *+8                                                              
         MVI   COUNTRY,C'C'                                                     
         CLC   X_RBIGZIP,SPACES                                                 
         BNH   REPR5                                                            
*                                                                               
         MVC   REPUBAD+56(10),X_RBIGZIP                                         
         OC    REPUBAD+53(26),SPACES                                            
                                                                                
REPR5    MVC   WORK,SPACES                                                      
         MVC   WORK(78),REPUBAD+27                                              
         GOTO1 ADSQUASH,DMCB,WORK,(0,78)                                        
         CLC   WORK+26(26),SPACES                                               
         BNE   EXIT                                                             
         MVC   REPUBAD+27(78),SPACES                                            
         MVC   REPUBAD+27(26),WORK                                              
         MVI   REPUBAD,2                                                        
*                                                                               
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
         MVI   FLAG,0                                                           
         CLI   SRTCTA,SRTCTAQ      CTA TRADE PAYMENT                            
         BNE   XCSH3                                                            
         SR    R0,R0                                                            
         IC    R0,SRTCTA#          MAX # CONTRACT/AMOUNT ENTRIES                
         LA    R2,SRTCTABK                                                      
         USING CTABLKD,R2                                                       
         AP    TRADE,CTAGROSS                                                   
         LA    R2,L'CTAINFO(R2)                                                 
         BCT   R0,*-10                                                          
         CLC   CMPALPHA,=C'TY'     TBSNY ONLY                                   
         BE    XCSH9                                                            
         ZAP   NET,SRTPNET         SAVE NET                                     
         B     XCSH9                                                            
         DROP  R2                                                               
*                                                                               
XCSH3    ZAP   NET,SRTPNET         SAVE NET                                     
         ZAP   SLUSH,SRTSLUSH      SAVE SLUSH AMOUNT                            
         ZAP   INVAMT,SRTINVAM     SAVE INVOICE AMOUNT                          
         BNZ   *+14                                                             
         ZAP   INVAMT,NET          SAVE NET IF NO INVOICE                       
         OI    FLAG,FLGNET                                                      
         CLI   SRTGSTYP,C' '       TEST GST TYPE                                
         BNH   XCSH5                                                            
         OC    SRTPGST,SRTPGST     TEST GST AMOUNT                              
         BZ    XCSH5                                                            
         ICM   R1,15,SRTPGST                                                    
         CVD   R1,DUB                                                           
         ZAP   GST,DUB                                                          
*                                                                               
XCSH5    CLC   SRTPST,SPACES       PROVINCE SALES TAX (PST)                     
         BE    XCSH9                     NO PST PRESENT                         
         MVC   BYTE,SRTGIND              SAVE OFF INDICATOR BYTE                
         LA    RF,L'SRTPST/SRTPSTLQ      MAX NUMBER OF PST'S                    
         LA    RE,SRTPST                 START OF PST BLOCK                     
*                                                                               
         USING SRTPST,RE                                                        
XCSH7    CLI   SRTPSTYP,C' '       AT END OF  PST LIST ?                        
         BE    XCSH9               YES, NO MORE PST TO ADD TO                   
         TM    BYTE,X'20'          IS PST RUNNING IN BINARY?                    
         BO    *+14                                                             
         ZAP   DUB,SRTPSTAM                                                     
         B     *+12                                                             
         ICM   R1,15,SRTPSTAM                                                   
         CVD   R1,DUB                                                           
         CLI   SRTTYPE,SRTTCHK     CASH RECEIPT                                 
         BNE   *+10                                                             
         MP    DUB,=P'-1'                                                       
         AP    PST,DUB             ADD  UP ALL THE PST                          
         TM    BYTE,X'20'          IS PST RUNNING IN BINARY?                    
         BO    *+14                                                             
         ZAP   SRTPSTAM,DUB                                                     
         B     *+12                                                             
         CVB   R1,DUB                                                           
         STCM  R1,15,SRTPSTAM                                                   
*                                                                               
         LA    RE,SRTPSTLQ(RE)     BUMP TO NEXT PST AMOUNT                      
         BCT   RF,XCSH7                                                         
         DROP  RE                                                               
*                                                                               
XCSH9    CLI   SRTTYPE,SRTTCHK     CASH RECEIPT                                 
         BNE   XCSH11                                                           
         ZAP   CASHRO,NET          KEEP POSITIVE CR                             
         AP    CASHRO,GST                                                       
         AP    CASHRO,PST                                                       
         ZAP   CASHR,INVAMT        KEEP POSITIVE CR                             
         TM    FLAG,FLGNET         ARE WE RUNNING ON NET AMOUNT?                
         BNO   XCSH10                                                           
         AP    CASHR,GST                                                        
         AP    CASHR,PST                                                        
*                                                                               
XCSH10   MP    NET,=P'-1'          NEGATIVE NET                                 
         MP    INVAMT,=P'-1'       NEGATIVE INVAMT                              
         MP    GST,=P'-1'                                                       
*                                                                               
XCSH11   TM    ITEMS,ITMCOMO       TEST ANY COMMISSION ONLY REP                 
         BNO   XCSH15                                                           
         CLI   SRTTYPE,SRTTCHK     IS IT A COMMISSION CASH RECEIPT              
         BNE   XCSH13                                                           
         TM    FLAG,FLGNET         ARE WE RUNNING ON NET AMOUNT?                
         BNO   *+16                                                             
         AP    NET,GST             COMMISSION ONLY                              
         AP    NET,PST                                                          
         ZAP   GST,=P'0'           WITH GST - ADD GST TO NET                    
         ZAP   PST,=P'0'                                                        
*                                                                               
XCSH13   ZAP   DUB,NET                                                          
         TM    FLAG,FLGNET         ARE WE RUNNING ON NET AMOUNT?                
         BO    *+10                                                             
         ZAP   DUB,INVAMT                                                       
         AP    CONET,DUB           COMMISSION ONLY                              
         AP    COGST,GST           GST SEPARATE                                 
         AP    COGST,PST           ADD PST IN WITH GST                          
*                                                                               
XCSH15   ZAP   TOT,INVAMT          TOT = INVAMT (SPOT)                          
         TM    FLAG,FLGNET         ARE WE RUNNING ON NET AMOUNT?                
         BNO   XCSH16                                                           
         AP    TOT,GST                          + GST + PST (NET)               
         AP    TOT,PST                                                          
*                                                                               
XCSH16   LHI   R1,OFFLINE          SET FOR OFFLINE                              
         TM    SRTSTAT,SRTONLNE    TEST ON LINE                                 
         BNO   *+8                                                              
         LHI   R1,ONLINE           SET FOR ONLINE                               
         MH    R1,=Y(CSHTLNQ)      X WIDTH OF EACH TABLE                        
         L     RF,AACCUMS          RF=A(ACCUMULATORS)                           
         AR    RF,R1               RF=A(SPECIFIC TABLE IN ACCUMS)               
*                                                                               
         LA    R1,NROW             R1=NUMBER OF ROWS                            
         TM    FLGTOT,FLGIDS       TEST SECOND PASS(BY ID)                      
         BNO   *+6                                                              
         BCTR  R1,0                DON'T ADD TO RUN TOTALS                      
*                                                                               
XCSH17   LA    RE,CSHCTL           RE=A(CASH BUCKETS FOR THIS REQUEST)          
         LHI   R0,CSHC             R0=NUMBER OF COLUMNS                         
*                                                                               
         AP    0(8,RF),0(8,RE)     ADD TO HIGHER LEVELS                         
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14             ADD ALL COLUMNS                              
         BCT   R1,XCSH17           ADD TO EACH HIGHER ROW                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FORMAT A LINE OF PRINT                                              *         
***********************************************************************         
                                                                                
FORM     NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         CLI   PROFIL1,C'N'        TEST "NO DETAIL"                             
         BE    EXIT                                                             
         MVC   PREP,SRTREP                                                      
         MVC   PSTA,SRTSTA                                                      
*                                                                               
         CLI   SRTSTA+4,C'A'                                                    
         BNE   *+10                                                             
         MVC   PSTA+4(3),=C'-AM'                                                
         CLI   SRTSTA+4,C'F'                                                    
         BNE   *+10                                                             
         MVC   PSTA+4(3),=C'-FM'                                                
*                                                                               
         MVC   PCLIENT,SRTCLI                                                   
         MVC   PINVOICE,SRTINV                                                  
         MVC   PSTART,SRTSTRT                                                   
         CLI   SRTEND+2,X'40'                                                   
         BNH   *+14                                                             
         MVI   PSTART+6,C'-'                                                    
         MVC   PEND,SRTEND                                                      
*                                                                               
         MVC   PPRODNM,PRDNAME                                                  
         CLC   SRTPROD,=C'POL'                                                  
         BNE   FORM3                                                            
         MVC   PPRODNM,SPACES                                                   
         MVC   PPROD1,=C'POL'                                                   
         CLC   SRTPROD2,SPACES                                                  
         BE    FORM3                                                            
         MVI   PPROD1#,C'/'                                                     
         MVC   PPROD2,SRTPROD2                                                  
         CLC   SRTPROD3,SPACES                                                  
         BE    FORM3                                                            
         MVI   PPROD2#,C'/'                                                     
         MVC   PPROD3,SRTPROD3                                                  
*                                                                               
FORM3    CLI   SRTCTA,SRTCTAQ      *** CTA TRADE PAYMENT ***                    
         BNE   FORM5                                                            
         MVC   PTYPE-1(5),=C'TRADE'                                             
         B     FORM7                                                            
*                                                                               
FORM5    CLI   SEN,SENNET          TEST NETWORK                                 
         BE    FORM6               STATUS BYTE NOT SET                          
         TM    SRTSTAT,SRTONLNE    WAS ITEM CLEARED ONLINE                      
         BZ    *+8                                                              
         MVI   PONLINE,C'*'        MARK ITEM AS CLEARED ONLINE                  
         TM    SRTSTAT,SRTAUTO     AUTO CLEARANCE                               
         BZ    *+8                                                              
         MVI   PONLINE,C'A'                                                     
*                                                                               
FORM6    CP    SLUSH,=P'0'         ANY SLUSH?                                   
         BE    *+8                                                              
         MVI   PIND,C'*'                                                        
         MVC   PTYPE,=C'RG'                                                     
         CLI   SRTTYPE,SRTTPAY     1=PAYMENT (REGULAR)                          
         BE    FORM7                                                            
         MVC   PTYPE,=C'CR'                                                     
         CLI   SRTTYPE,SRTTCRM     2=CREDIT MEMO                                
         BE    FORM7                                                            
         MVC   PTYPE,=C'CK'        3=CASH RECEIPT                               
*                                                                               
FORM7    BAS   RE,EDIT             EDIT THE CASH                                
         MVI   SPACING,1                                                        
         BAS   RE,REPRT                                                         
*                                                                               
FORM9    CLI   SRTCTA,SRTCTAQ      TRADE BUY                                    
         BNE   FORM15                                                           
         MVI   PLWRK,C' '                                                       
         MVC   PLWRK+1(L'PLWRK-1),PLWRK                                         
         MVC   PLWRK(13),=CL13'CONTRACT#:'                                      
*                                                                               
         SR    R4,R4                                                            
         IC    R4,SRTCTA#                                                       
         LA    R2,SRTCTABK                                                      
         USING CTABLKD,R2                                                       
         LA    R6,PLWRK+18                                                      
*                                                                               
FORM11   LA    R6,2(R6)                                                         
         EDIT  CTACTRCT,(5,0(R6)),ZERO=BLANK                                    
         MVI   5(R6),C'='                                                       
         LA    R6,6(R6)                                                         
         EDIT  CTAGROSS,(12,0(R6)),2,FLOAT=$,ALIGN=LEFT                         
         AR    R6,R0                                                            
         MVI   0(R6),C','                                                       
         LA    R2,L'CTAINFO(R2)                                                 
         BCT   R4,FORM11                                                        
*                                                                               
FORM13   MVI   0(R6),C')'                                                       
         GOTO1 ADSQUASH,DMCB,PLWRK,200                                          
         MVC   PLINE,PLWRK                                                      
         BAS   RE,REPRT                                                         
         DROP  R2                                                               
*                                                                               
FORM15   CLI   SRTCOMM,0                                                        
         BE    FORMX                                                            
         CLI   SRTCOMM,5                                                        
         BH    FORMX                                                            
         CLC   SRTNARR(120),SPACES                                              
         BNH   FORMX                                                            
*                                                                               
         LA    R5,SRTNARR                                                       
         GOTO1 ADSQUASH,DMCB,(R5),200                                           
         L     R4,DMCB+4                                                        
         MVI   PLWRK,C' '                                                       
         MVC   PLWRK+1(L'PLWRK-1),PLWRK                                         
         GOTO1 CHOPPER,DMCB,((R4),(R5)),(80,PLWRK),3                            
         LA    R2,PLWRK                                                         
         L     R4,DMCB+8           NUMBER OF LINES USED                         
*                                                                               
FORM17   MVC   PLINE(80),0(R2)                                                  
         BAS   RE,REPRT                                                         
         LA    R2,80(R2)                                                        
         BCT   R4,FORM17                                                        
*                                                                               
FORMX    B     EXIT                                                             
         DROP  R7                                                               
                                                                                
***********************************************************************         
* OUTPUT SMF RECORD FOR CROSSFILE CLEARANCES                                    
***********************************************************************         
         USING CROSD,SMFREC                                                     
SMFOX    NTR1  ,                                                                
         CLI   SRTOAGY,C' '          IS THERE AN OVERRIDE AGENCY                
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
         MVI   CROSPGM+0,C'5'        PROGRAM               M                    
         MVI   CROSPGM+1,C'3'        PROGRAM               X                    
         MVC   CROSUID,SRTOID        COMPANY ID            OO                   
         MVC   CROSPID,SRTPID        PERSON ID             PP                   
*        MVC   CROSSAGY,             SECURITY AGENCY                            
         MVC   CROSAAGY,SRTOAGY      ACCOUNTING AGENCY     AA                   
         MVC   CROSMAGY,SRTAGY       MEDIA AGENCY          AA                   
         MVC   CROSASE,SRTASE        ACCOUNTING SE NUMBER  N                    
         MVC   CROSMSE,SPOTSEN       MEDIA SE NUMBER       N                    
         MVC   CROSOFF,SRTOFF        OFFICE                OO                   
         MVC   CROSMED,SRTMED        MEDIA CODE            M                    
         MVC   CROSCLI,SRTCLI        CLIENT CODE           CCC                  
         MVC   CROSPRD,SRTPROD       PRODUCT CODE          PPP                  
         L     R2,ADCOMFAC                                                      
         ICM   R2,15,CSMFOUT-COMFACSD(R2)                                       
         BZ    SMFOXX                                                           
         GOTO1 (R2),DMCB,13,SMFREC                                              
SMFOXX   B     EXIT                                                             
SMFREC   DS    XL(CROSLNQ)                                                      
                                                                                
***********************************************************************         
* PRINT CLIENT TOTALS                                                 *         
***********************************************************************         
                                                                                
CLITOT   NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVI   ROW,ROWCLI                                                       
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW                  GET CLIENT TOTALS                       
         CLI   PROFIL1,C'N'             TEST "NO DETAIL"                        
         BE    CLITOT3                                                          
         BAS   RE,REPRT                 BLANK LINE                              
         MVC   PTOTLIN2,=CL14'CLIENT TOTALS'                                    
         BAS   RE,EDIT                                                          
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                 BLANK LINE                              
*                                                                               
CLITOT3  MVC   BUFCLI,SAVCLI            BUILD A BUFFALO RECORD                  
         MVC   BUFCLIN,CLINAME                                                  
         MVC   BUFOFF,OFFICE                                                    
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
* PRINT STATION TOTALS                                                *         
***********************************************************************         
                                                                                
STATOT   NTR1  ,                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVI   ROW,ROWSTA                                                       
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW             GET STATION TOTALS                           
         CLI   PROFIL1,C'N'        TEST "NO DETAIL"                             
         BE    STATOT3                                                          
         MVC   PTOTLIN2,=CL14'STATION TOTALS'                                   
         BAS   RE,EDIT                                                          
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
*                                                                               
STATOT3  BAS   RE,CROW                   CLEAR TOTALS                           
         MVC   STANM,SPACES        FORCE STATION READ                           
         MVC   SAVSLSHC,SPACES     FORCE SLUSH CODE READ                        
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
         CLC   SAVREP,=3C'0'       DONT PRINT REP TOTALS WHEN                   
         BE    REPTOT3             THERE IS NO REP                              
         CLI   PROFIL1,C'N'        TEST "NO DETAIL"                             
         BE    REPTOT3                                                          
         MVC   PTOTLIN2,=CL14'REP TOTALS'                                       
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
         CLI   PROFIL1,C'N'        IF  "NO DETAIL"                              
         BNE   MEDTOT3                                                          
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,2           ELSE, SKIP 2 LINES                           
         BAS   RE,REPRT                                                         
*                                                                               
MEDTOT3  OI    FLGTOT,FLGSUM        SUMMARY HEADS                               
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
         EDIT  BUFTOT,(12,PTOTNET),2,MINUS=YES,ZERO=BLANK                       
         MP    BUFCASHR,=P'-1'                                                  
         EDIT  BUFCASHR,(12,PTOTCR),2,MINUS=YES,ZERO=BLANK                      
         CLI   TAXUL,C' '          TEST GST/PST                                 
         BNH   MEDTOT7                                                          
         EDIT  BUFGST,(10,PTOTGST),2,MINUS=YES,ZERO=BLANK                       
         EDIT  BUFPST,(10,PTOTPST),2,MINUS=YES,ZERO=BLANK                       
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
         EDIT  TOT,(12,PTOTNET),2,MINUS=YES,ZERO=BLANK                          
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
         BO    MEDTOT13                                                         
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
         EDIT  TOT,(12,PTOTNET),2,MINUS=YES,ZERO=BLANK                          
         MVI   SPACING,2                                                        
         BAS   RE,REPRT                                                         
         BAS   RE,PONL             POST ON LINE TOTAL                           
*                                                                               
AGYTOT5  MVC   PTOTLINE,=CL25'REGULAR LESS CASH RECEIPT'                        
         MVI   TYPE,TYPBOTH                                                     
         BAS   RE,GROW                                                          
         EDIT  TOT,(12,PTOTNET),2,MINUS=YES,ZERO=BLANK                          
         BAS   RE,REPRT                                                         
*                                                                               
         CP    TRADE,=P'0'                                                      
         BE    AGYTOT7                                                          
         MVC   PTOTLINE,=CL25'TRADE BUYS'                                       
         EDIT  TRADE,(12,PTOTNET),2,MINUS=YES,ZERO=BLANK                        
         MVI   SPACING,2                                                        
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
         GOTO1 ADMGR,ODDADD        ADD DUMMY RECORD FOR EOD(PACKING             
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
         ZAP   ODDAMT,NET          ODDAMT = NET + GST + PST                     
         AP    ODDAMT,GST                                                       
         AP    ODDAMT,PST                                                       
*                                                                               
         ZAP   ODDCOMO,CONET                                                    
         AP    ODDCOMO,COGST                                                    
         ZAP   ODDCR,CASHRO                                                     
         ZAP   DUB,ODDCR                                                        
         MP    DUB,=P'-1'                                                       
         ZAP   ODDCR,DUB                                                        
         ZAP   ODDTRADE,TRADE                                                   
         BNZ   *+14                                                             
         OI    ODDSTAT,ODDSSL      SET TO SHOW SLUSH IS SET                     
         ZAP   ODDSLUSH,SLUSH                                                   
         GOTO1 ADMGR,ODDADD                                                     
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
         MVC   ODDUTL,SPOTSEN      SPOT SE NUMBER                               
         MVC   ODDAGY,SAVAGY       AGENCY ALPHA                                 
         MVC   ODDSE,ACCTSEN       ACC SE NUMBER                                
         MVC   ODDNAM,AGYNM        AGENCY NAME                                  
         MVI   ODDSTAT,0           STATUS                                       
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
         MVC   PSSBDESC,=CL15'ONLINE SPOT CLR'                                  
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
         GOTO1 DIVIDE,SLOGO        START LOGO PAGE FOR CONTROL                  
         MVC   AGYNM,SPACES                                                     
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
         EDIT  TOT,(12,PTOTNET),2,MINUS=YES,ZERO=BLANK                          
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
         EDIT  GST,(10,PTOTGST),2,MINUS=YES,ZERO=BLANK                          
         EDIT  PST,(10,PTOTPST),2,MINUS=YES,ZERO=BLANK                          
         EDIT  CASHD,(12,PTOTCD),2,MINUS=YES,ZERO=BLANK                         
         EDIT  (P8,TOT),(12,PTOTNET),2,MINUS=YES,ZERO=BLANK                     
         B     EXIT                                                             
*                                                                               
EDTOT    NTR1  ,                        MEDIA & AGENCY TOTALS                   
         LA    R7,P                                                             
         ZAP   DUB,TOT             TOTAL + CR                                   
         AP    DUB,CASHR                                                        
         EDIT  (P8,DUB),(12,PTOTNET),2,MINUS=YES,ZERO=BLANK                     
         ZAP   DUB,CASHR           CR                                           
         MP    DUB,=P'-1'                                                       
         EDIT  (P8,DUB),(12,PTOTCR),2,MINUS=YES,ZERO=BLANK                      
         EDIT  CASHD,(12,PTOTCD),2,MINUS=YES,ZERO=BLANK                         
         EDIT  GST,(10,PTOTGST),2,MINUS=YES,ZERO=BLANK                          
         EDIT  PST,(10,PTOTPST),2,MINUS=YES,ZERO=BLANK                          
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
MEDNM3   MVC   MEDCODE,1(R1)       SAVE HEX CODE AND NAME                       
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
         MVC   HEAD4+08(33),AGYNM          AGENCY NAME                          
         MVC   HEAD4+110(6),=CL6'MEDIA'                                         
         MVC   HEAD4+117(15),MEDNAME       MEDIA NAME                           
*                                                                               
         TM    FLGTOT,FLGAGY               TEST AGENCY TOTALS                   
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
                                                                                
NOSPOT   NTR1  ,                  CAN'T READ SPOT FILE                          
         AP    NOSPOTF,=P'1'                                                    
         CP    NOSPOTF,=P'10'                                                   
         BL    EXIT                                                             
         GOTO1 LOGIO,DMCB,1,(50,NOSPOTM)                                        
         DC    H'0'                                                             
*                                                                               
NOSTAT   NTR1  ,                   CAN'T READ STATION FILE                      
         CLC   SRTAGY,=C'CK'       TEST COKE                                    
         BE    EXIT                                                             
         AP    NOSTATF,=P'1'                                                    
         CP    NOSTATF,=P'20'                                                   
         BL    EXIT                                                             
         GOTO1 LOGIO,DMCB,1,(50,NOSTATM)                                        
         DC    H'0'                                                             
*                                                                               
NOSPOTM  DC    CL50'CAN''T READ SPOT FILE - ** RUN ABORTED **'                  
NOSTATM  DC    CL50'CAN''T READ STATION FILE - ** RUN ABORTED **'               
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
         MVC   OFFCDE,OFFICE       OFFICE CODE                                  
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
         MVC   WORK+1(2),=C'54'      PROGRAM                                    
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
         MVC   REMOTDSC(8),=C'SP CLEAR'                                         
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
         MVC   LOGOJOB+4(4),=C'A54S'                                            
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
         CLI   SRTCTA,SRTCTAQ      TRADE BUY                                    
         BNE   POSTD2              POST DETAIL ITEM                             
         BAS   RE,CTAPOST          MAKE TRADE BUY POSTINGS                      
         B     POSTX                                                            
*                                                                               
POSTD2   L     R5,ATWORK           POST DETAIL ITEM                             
         USING PSHEADD,R5                                                       
         MVI   PSHDEL,PSHDELQ      POSTING HEADER                               
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC(1),SRTCPY                                                
         MVC   PSHDACC+1(2),=C'SS'                                              
         CLI   COUNTRY,C'C'                                                     
         BNE   *+8                                                              
         MVI   PSHDACC+2,C'T'      CANADIAN                                     
         MVC   PSHDACC+3(12),SPACES                                             
         MVC   PSHDACC+3(1),SRTMED                                              
         MVC   PSHDANAL(53),SPACES                                              
         MVC   PSHDSBAC+12(3),SRTCLI                                            
*                                                                               
         MVC   PSHDACC+4(5),SRTSTA  ACCOUNT IS STATION                          
         MVC   SAVSTA,SRTSTA        SAVE STATION FOR SLUSH                      
         TM    ITEMS,ITMREP         TEST REP                                    
         BNO   POSTD4                                                           
         MVC   PSHDACC+4(5),SPACES                                              
         MVC   PSHDACC+4(4),SRTREP  ACCOUNT IS REP                              
         MVC   PSHDSBAC+4(5),SRTSTA CONTRA STATION                              
         MVC   PSHDSBAC+3(1),SRTMED                                             
*                                                                               
POSTD4   CLI   SRTMED,C'N'                                                      
         BNE   POSTD5              IS MEDIA NETWORK                             
         CLI   PROFIL6,C'Y'        USE LEDGER "U" FOR NETWORK                   
         BNE   POSTD5                                                           
         MVI   PSHDACC+2,C'U'      USE LEDGER U FOR NETWORK                     
*                                                                               
POSTD5   CLC   SRTAGY,=C'YN'       CLEAR ACROSS FILES?                          
         BNE   POSTD6                                                           
         CLC   SRTOAGY,=C'H7'      FROM YN TO H7?                               
         BNE   POSTD6                                                           
         MVI   PSHDACC+2,C'T'      FAKE IT TO CANADIAN LEDGER                   
*                                                                               
POSTD6   XC    MIELM,MIELM         CLEAR CURRENT MI ELEMENT                     
         L     R4,AMIBUFF                                                       
         USING MIOELD,R4                                                        
POSTD7   CLI   0(R4),X'FF'         END-OF-BUFFER                                
         BE    POSTD11             NO MATCH MEDIA RECORD                        
         CLC   MIOOFF,SPACES       ANY OFFICE ATTACHED?                         
         BE    POSTD7A                                                          
         LA    RE,OFFICE           OFFICE                                       
         TM    CMPSTA4,CPYSOFF2                                                 
         BO    *+8                                                              
         LA    RE,SRTOFF                                                        
         CLC   MIOOFF,0(RE)        MATCH ON OFFICE?                             
         BNE   POSTD8                                                           
POSTD7A  CLC   MIOCODE,PSHDACC+2   MATCH SYSTEM/MEDIA                           
         BE    POSTD9                                                           
POSTD8   LA    R4,MIOLNQ(R4)                                                    
         B     POSTD7                                                           
*                                                                               
POSTD9   MVC   MIELM,MIOEL        SAVE MI ELEMENT FOR THIS MEDIA                
         B     POSTD8                                                           
*                                                                               
POSTD11  L     R6,ATWORKT                                                       
         USING TRNELD,R6                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
         MVI   TRNSUB,0                                                         
         GOTO1 DATCON,DMCB,(0,SRTEND),(1,TRNDATE)                               
         MVC   TRNREF(3),SRTPROD        REFERENCE NUMBER                        
         MVC   TRNREF+3(3),SRTINV                                               
         MVI   TRNSTAT,0                                                        
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNBTCH(2),MOS           BATCH                                   
         MVC   TRNOFFC,OFFICE           OFFICE                                  
         TM    CMPSTA4,CPYSOFF2                                                 
         BO    POSTD13                                                          
         CLI   SRTOFF,C' '                                                      
         BNH   *+10                                                             
         MVC   TRNOFFC,SRTOFF                                                   
*                                                                               
POSTD13  OC    TRNOFFC,SPACES                                                   
         MVC   LASTOFF,TRNOFFC                                                  
         MVI   TRNNARR,C' '                                                     
         ZAP   TRNAMNT,TOT       PAYABLE IS TOT                                 
         SP    TRNAMNT,CASHD      - CD                                          
         MVI   TRNTYPE,X'21'     TRANSACTION TYPE - STATION                     
         TM    ITEMS,ITMREP      TEST REP                                       
         BNO   *+8                                                              
         MVI   TRNTYPE,X'22'     TRANSACTION TYPE - REP                         
         TM    ITEMS,ITMREP+ITMUNW                                              
         BNO   *+8                                                              
         MVI   TRNTYPE,X'23'     TRANSACTION TYPE - UNWIRED REP                 
*                                                                               
         SR    R2,R2             L'44 ELEMENT IF NO NARRATIVE                   
         IC    R2,TRNLN                                                         
         SR    R1,R1                                                            
         CLI   SRTCOMM,0         GET TRANSACTION NARRATIVE                      
         BE    POSTD15                                                          
         CLI   SRTCOMM,5                                                        
         BH    POSTD15                                                          
         IC    R1,SRTCOMM                                                       
         MHI   R1,40                                                            
         SHI   R1,1                                                             
         MVC   TRNNARR(0),SRTNARR                                               
         EX    R1,*-6                                                           
         AR    R2,R1                                                            
*                                                                               
POSTD15  STC   R2,TRNLN          LENGTH OF ELEMENT WITH NARRATIVE               
         CLI   SRTTYPE,SRTTCHK   DIFFERENT POSTING FOR CASH RECEIPT             
         BNE   POSTD17                                                          
         BAS   RE,PCASH            POST CASH RECEIPT                            
         B     POSTD19                                                          
*                                                                               
POSTD17  GOTO1 CHKACT,PSHDACC      CHECK ACCT TO SEE WHICH ELMS TO ADD          
         BAS   RE,PSTAT            POST STATION/REP                             
*                                                                               
POSTD19  TM    SRTSTAT,SRTONLNE    IS IT CLEARED ONLINE                         
         BNZ   POSTX                                                            
         L     R6,ATWORKT                                                       
         CP    GST,=P'0'                                                        
         BE    POSTD21                                                          
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
POSTD21  CP    PST,=P'0'           ANY PST PRESENT AT ALL ?                     
         BE    POSTD24             NO                                           
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
POSTD23  CLI   SRTPSTYP,C' '       ANY PST TYPE ?                               
         BE    POSTD24             NO, FINISHED                                 
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        X'50' ELEMENT                                
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTQST    PST TAX - TYPE 'Q'                           
         TM    BYTE,X'20'          IS PST RUNNING IN BINARY?                    
         BO    *+14                                                             
         ZAP   DUB,SRTPSTAM                                                     
         B     *+12                                                             
         ICM   R1,15,SRTPSTAM                                                   
         CVD   R1,DUB                                                           
         ZAP   SCIAMNT,DUB                                                      
         ZAP   SCIBASE,NET                                                      
         AP    SCIBASE,CASHD                                                    
         AP    SCIBASE,GST         BASIS = NET+CD+GST                           
         MVC   SCISUBTY,SPACES     SUB TYPE                                     
         MVC   SCISUBPR,SRTPSTPR   PROVINCE CODE                                
         MVC   SCISUBPT,SRTPSTYP   PRIVINCE TAX TYPE                            
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
         LA    RE,SRTPSTLQ(RE)     BUMP TO NEXT PST ENTRY                       
         BCT   RF,POSTD23          LOOP TO BUILD ANOTHER X'50'                  
         DROP  RE                                                               
*                                                                               
         USING SCIELD,R6                                                        
POSTD24  CP    SRTPGRS,=P'0'                                                    
         BE    POSTD25                                                          
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
POSTD25  OC    SRTPID,SRTPID                                                    
         BZ    POSTD26                                                          
         CLC   SRTPID,SPACES                                                    
         BE    POSTD26                                                          
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
POSTD26  BAS   RE,PUTAPE                                                        
                                                                                
         USING PSHEADD,R5                                                       
         L     R5,ATWORK                                                        
         MVC   PAYACCT,PSHDACC     SAVE PAYABLE ACCOUNT CODE                    
         CP    GST,=P'0'                                                        
         BE    *+8                                                              
         BAS   RE,PGST             POST THE GST                                 
*                                                                               
         CP    PST,=P'0'                                                        
         BE    POSTD99                                                          
         LA    R0,L'SRTPST/SRTPSTLQ     NUMBER OF POSSIBLE ENTRIES              
         LA    R1,SRTPST                                                        
*                                                                               
         USING SRTPST,R1                                                        
POSTD27  CLI   SRTPSTYP,C' '       ANY PST ENTRY ?                              
         BE    POSTD99             NO, SO FINISHED HERE                         
         GOTO1 PPSTT               POST THE PST PASS R1 (PST ENTRY)             
         LA    R1,SRTPSTLQ(R1)     NEXT ENTRY                                   
         BCT   R0,POSTD27          TRY AGIAN                                    
*                                                                               
POSTD99  CP    SLUSH,=P'0'                                                      
         BE    POSTX                                                            
         BAS   RE,PSLUSH           MAKE SLUSH POSTINGS                          
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
CHKA70   CLI   RSTLN,RSTLN3Q                                                    
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
* POSTINGS FOR STATION/REP                                            *         
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
         MVI   XPYLN,XPYLN3Q                                                    
         ZAP   XPYCD,CASHD         CASH DISCOUNT                                
         MVC   XPYCLI,CLINAME                                                   
         MVC   XPYPRO,PRDNAME                                                   
         MVC   XPYPRON,PRDNAME     SAVE PROD NAME IN EXCLUSIVE FIELD            
         MVC   XPYAGY,SRTAGY       MEDIA AGENCY                                 
         MVC   XPYSEQ,SRTSEQN      SEQUENCE NUMBER                              
         MVC   XPYSMED,SRTSMED     SUB-MEDIA (FROM NET)                         
         MVC   XPYBMKT,SRTBMKT     BINARY MARKET NUMBER                         
         CLI   PROFIL5,C'Y'        USE PRODUCT NAME NOT CODE                    
         BE    PSTAT3                                                           
         MVC   XPYPRO,SPACES                                                    
         MVC   XPYPRO(3),SRTPROD                                                
         CLC   SRTPROD2,SPACES                                                  
         BE    PSTAT3                                                           
         MVI   XPYPRO+3,C'/'                                                    
         MVC   XPYPRO+4(3),SRTPROD2                                             
         CLC   SRTPROD3,SPACES                                                  
         BE    PSTAT3                                                           
         CLC   SRTPROD3,=C'REG'                                                 
         BE    PSTAT3                                                           
         CLC   SRTPROD3,=C'PIG'                                                 
         BE    PSTAT3                                                           
         MVI   XPYPRO+7,C'/'                                                    
         MVC   XPYPRO+8(3),SRTPROD3                                             
*                                                                               
PSTAT3   MVC   XPYINV,SPACES                                                    
         MVC   XPYINV(11),SRTINV                                                
         XC    XPYPER,XPYPER                                                    
         MVC   XPYPER(6),SRTSTRT                                                
         CLI   SRTEND+2,X'40'                                                   
         BE    *+10                                                             
         MVC   XPYPER+6(6),SRTEND                                               
         MVC   XPYTYPE,SRTTYPE                                                  
         XC    XPYEST,XPYEST                                                    
         LA    R0,3                VALIDATE THE ESTIMATE NUMBER                 
         LA    R1,SRTESTN          MUST BE 3 NUMERIC                            
         CLI   0(R1),C'0'                                                       
         BL    PSTAT4                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
*                                                                               
         PACK  DUB,SRTESTN                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,XPYEST                                                      
*                                                                               
PSTAT4   CLC   SRTPROD2,SPACES                                                  
         BE    PSTAT5                                                           
         MVC   XPYPRO2,SRTPROD2                                                 
*                                                                               
PSTAT5   IC    R2,XPYLN                                                         
         AR    R6,R2                                                            
*                                                                               
         USING GDAELD,R6           GENERAL DATE ELEMENT                         
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATCLR                                                  
*        MVC   GDADATE,SRTDAYP                                                  
         MVC   GDADATE,TODAY                                                    
*                                                                               
         SR    R2,R2                                                            
         IC    R2,1(,R6)                                                        
         AR    R6,R2                                                            
*                                                                               
         XC    GDAEL(GDALNQ),GDAEL                                              
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDAMMOS     MEDIA MOS                                    
         LA    R4,SRTSTRT                                                       
         CLI   SRTEND+2,X'40'                                                   
         BE    *+8                                                              
         LA    R4,SRTEND                                                        
         CLI   SRTMED,C'N'         NETWORK ?                                    
         BE    PSTAT6              YES                                          
         GOTO1 =V(GETBROAD),DMCB,(1,(R4)),WORK,GETDAY,ADDAY                     
         LA    R4,WORK+6                                                        
*                                                                               
PSTAT6   GOTO1 DATCON,DMCB,(0,(R4)),(1,GDAYYMM)                                 
         MVI   GDAYYMM+L'GDAYYMM,0             CLEAR DAY FIELD                  
         SR    R2,R2                                                            
         IC    R2,1(,R6)                                                        
         AR    R6,R2                                                            
*                                                                               
         MVC   PAYNAME,SPACES                                                   
         MVC   PAYNAME(20),STANM                                                
         TM    ITEMS,ITMREP                                                     
         BNO   *+10                                                             
         MVC   PAYNAME(22),REPNAME                                              
*                                                                               
         TM    ACTFLAG,ACT20       IS THE NAMELD LOCKED?                        
         BO    PSTAT8                                                           
         USING NAMELD,R6                                                        
         MVI   NAMEL,NAMELQ         STATION/REP NAME                            
         MVI   NAMLN,L'STANM+2      ASSUME STA CHECK FOR NOW                    
         MVC   NAMEREC(20),STANM                                                
         TM    ITEMS,ITMREP                                                     
         BNO   PSTAT7                                                           
         MVI   NAMLN,L'REPNAME+2                                                
         MVC   NAMEREC(22),REPNAME                                              
*                                                                               
PSTAT7   IC    R2,NAMLN                                                         
         AR    R6,R2                                                            
         MVI   0(R6),0                                                          
*                                                                               
PSTAT8   TM    ACTFLAG,ACT22       IS THE ADRELD LOCKED?                        
         BO    PSTAT15                                                          
*                                                                               
         CLI   REPUBAD,0           NO ADDRESS                                   
         BE    PSTAT15                                                          
         USING ADRELD,R6           ADDRESS ELEMENT                              
         MVI   ADREL,ADRELQ                                                     
         MVC   ADRNUM,REPUBAD      NO OF LINES (WORKED OUT BY SUBRTN)           
         LA    R4,REPUBAD+1                                                     
         TM    ITEMS,ITMREP                                                     
         BO    PSTAT9                                                           
         MVC   ADRNUM,STAAD        STATION ADDRESS                              
         LA    R4,STAAD+1                                                       
*                                                                               
PSTAT9   IC    R2,ADRNUM                                                        
         MVI   ADRLN,3             IN CASE WE FIND NO ADDRESS                   
         MVI   ADRLN+2,0                                                        
*                                                                               
         LA    R5,ADRADD1                                                       
PSTAT11  MVC   0(26,R5),0(R4)                                                   
         OC    0(26,R5),SPACES                                                  
         LA    R4,26(,R4)                                                       
         LA    R5,26(,R5)                                                       
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
         CLI   PROFIL3,C'Y'        CASH REC BY MEDIA WANTED                     
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
         CLC   SRTPROD2,SPACES                                                  
         BE    PCASH7                                                           
         MVI   PSHDSBAC+10,C'/'                                                 
         MVC   PSHDSBAC+11(3),SRTPROD2                                          
*                                                                               
PCASH7   ZAP   TRNAMNT,TOT        TOT                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(1),SRTMED                                                   
         MVC   WORK+1(5),SRTSTA                                                 
         TM    ITEMS,ITMREP                                                     
         BNO   PCASH9                                                           
         MVC   WORK+1(5),SPACES                                                 
         MVC   WORK+1(4),SRTREP                                                 
*                                                                               
PCASH9   GOTO1 DATCON,DMCB,(1,TODAY),(8,WORK+13)                                
         GOTO1 ADSQUASH,DMCB,WORK,25                                            
         L     R4,DMCB+4                                                        
         SR    R2,R2                                                            
         IC    R2,TRNLN                                                         
         LA    R1,0(R2,R6)                                                      
         MVI   0(R1),C' '          END OF NARRATIVE                             
         SH    R4,=H'1'                                                         
         MVC   1(0,R1),WORK        STATION AND DATE TO NARRATIVE                
         EX    R4,*-6                                                           
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
         MVC   BYTE,SRTGIND        SAVE OFF INDICATOR BYTE                      
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
         TM    BYTE,X'20'          IS PST RUNNING IN BINARY?                    
         BO    *+14                                                             
         ZAP   DUB,SRTPSTAM        POST THE PST AMOUNT                          
         B     *+12                                                             
         ICM   R1,15,SRTPSTAM                                                   
         CVD   R1,DUB                                                           
         ZAP   TRNAMNT,DUB                                                      
         LLC   R1,1(R6)            ADD MEMO                                     
         AR    R6,R1               POINT TO NEW LOCATION TO BUILD               
*                                                                               
         USING SCIELD,R6                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,C'N'       NET TYPE                                      
         ZAP   SCIAMNT,NET        NET AMOUNT                                    
*        AP    SCIAMNT,GST        + GST    (REMOVED FOR DSFTK-158)              
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
* BUILD MEDIA CONTROL POSTING TO SLUSH                                *         
***********************************************************************         
                                                                                
PSLUSH   NTR1  ,                                                                
         L     R5,ATWORK                                                        
         USING PSHEADD,R5                                                       
         L     R6,ATWORKT                                                       
         USING TRNELD,R6                                                        
         SR    R2,R2                                                            
         MVI   PSHDEL,PSHDELQ            POSTING HEADER                         
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,SPACES                                                   
         MVC   PSHDACC(1),SRTCPY                                                
         MVC   PSHDACC+1(14),SAVSLSHC       SLUSH ACCOUNT                       
*                                                                               
         MVC   PSHDANAL(53),SPACES                                              
         MVC   PSHDSBAC+1(2),=C'SS'                                             
         CLI   COUNTRY,C'C'                                                     
         BNE   *+8                                                              
         MVI   PSHDSBAC+2,C'T'     CANADIAN                                     
         CLC   SRTAGY,=C'YN'       CLEAR ACROSS FILES?                          
         BNE   PSL002                                                           
         CLC   SRTOAGY,=C'H7'      FROM YN TO H7?                               
         BNE   PSL002                                                           
         MVI   PSHDACC+2,C'T'      FAKE IT TO CANADIAN LEDGER                   
*                                                                               
PSL002   MVC   PSHDSBAC+3(12),SPACES                                            
         MVC   PSHDSBAC+3(1),SRTMED                                             
*                                                                               
         MVC   PSHDSBAC+4(5),SAVSTA      ACCOUNT IS STATION                     
         MVC   PSHDSBAC+12(3),SRTCLI                                            
         MVC   PSHDSBNM(20),SAVSTAN                                             
         CLI   SRTMED,C'N'                                                      
         BNE   PSL010                    IS MEDIA NETWORK                       
         CLI   PROFIL6,C'Y'              USE LEDGER "U" FOR NETWORK             
         BNE   PSL010                                                           
         MVI   PSHDSBAC+2,C'U'           USE LEDGER U FOR NETWORK               
*                                                                               
PSL010   MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
         GOTO1 DATCON,DMCB,(0,SRTEND),(1,TRNDATE)                               
*        MVC   TRNDATE,TODAY                                                    
         MVC   TRNREF(3),SRTPROD         REFERENCE NUMBER                       
         MVC   TRNREF+3(3),SRTINV                                               
         XC    TRNSUB(2),TRNSUB                                                 
         MVI   TRNSTAT,X'80'                                                    
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNBTCH(2),MOS                                                   
         ZAP   TRNAMNT,SLUSH                                                    
         MVI   TRNTYPE,X'21'     TRANSACTION TYPE - STATION                     
         TM    ITEMS,ITMREP      TEST REP                                       
         BNO   *+8                                                              
         MVI   TRNTYPE,X'22'     TRANSACTION TYPE - REP                         
         TM    ITEMS,ITMREP+ITMUNW                                              
         BNO   *+8                                                              
         MVI   TRNTYPE,X'23'     TRANSACTION TYPE - UNWIRED REP                 
         MVC   TRNOFFC,OFFICE            OFFICE                                 
         TM    CMPSTA4,CPYSOFF2                                                 
         BO    PSL012                                                           
         CLI   SRTOFF,C' '                                                      
         BNH   *+10                                                             
         MVC   TRNOFFC,SRTOFF                                                   
*                                                                               
PSL012   OC    TRNOFFC,SPACES                                                   
         MVI   TRNNARR,C' '                                                     
         SR    R2,R2             L'44 ELEMENT IF NO NARRATIVE                   
         IC    R2,TRNLN                                                         
         SR    R1,R1                                                            
         CLI   SRTCOMM,0         GET TRANSACTION NARRATIVE                      
         BE    PSL020                                                           
         CLI   SRTCOMM,5                                                        
         BH    PSL020                                                           
         IC    R1,SRTCOMM                                                       
         MHI   R1,40                                                            
         SHI   R1,1                                                             
         MVC   TRNNARR(0),SRTNARR                                               
         EX    R1,*-6                                                           
         AR    R2,R1                                                            
         STC   R2,TRNLN          LENGTH OF ELEMENT WITH NARRATIVE               
*                                                                               
PSL020   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                     BUMP TO NEXT ELEMENT                   
*                                                                               
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ              X'DB'                                  
         MVI   FFTTYPE,FFTTINVN          INVOICE NUMBER (44)                    
         MVI   FFTSEQ,0                                                         
         LA    RF,SRTINV                                                        
         AHI   RF,L'SRTINV-1                                                    
         LHI   R1,L'SRTINV                                                      
PSL030   CLI   0(RF),C' '                                                       
         BH    PSL040                                                           
         SHI   RF,1                                                             
         BCT   R1,PSL030                                                        
*                                                                               
PSL040   STC   R1,FFTDLEN                ACTUAL LENGTH OF TEXT                  
         BCTR  R1,0                                                             
         MVC   FFTDATA(0),SRTINV                                                
         EX    R1,*-6                                                           
         AHI   R1,1                      RESTORE LENGTH                         
         LA    RE,FFTLN1Q                ELEMENT OVERHEAD                       
         AR    RE,R1                                                            
         AHI   RE,L'FFTDLEN                                                     
         STC   RE,FFTLN                  LENGTH OF ELEMENT                      
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
*                                                                               
         USING FFTELD,R6                                                        
         XC    FFTEL(12),FFTEL                                                  
         MVI   FFTEL,FFTELQ              X'DB'                                  
         MVI   FFTTYPE,FFTTESTN          ESTIMATE NUMBER (122)                  
         MVI   FFTSEQ,0                                                         
         MVC   FFTCESTN,SPACES     INIT AS SPACES                               
         MVC   FFTOESTN,SPACES                                                  
         LA    R0,3                VALIDATE THE ESTIMATE NUMBER                 
         LA    R1,SRTESTN          MUST BE 3 NUMERIC                            
         CLI   0(R1),C'0'                                                       
         BL    PSL060                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
*                                                                               
         MVC   FFTOESTN(L'SRTESTN),SRTESTN   MOVE IN EST # IN CHAR              
         MVI   FFTDLEN,12                                                       
         LA    R1,FFTESLNQ               SET LENGTH                             
         STC   R1,FFTLN                  LENGTH OF ELEMENT                      
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
*                                                                               
         USING GDAELD,R6                                                        
PSL060   XC    GDAEL(GDALNQ),GDAEL                                              
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDAMMOS     MEDIA MOS                                    
         LA    R4,SRTSTRT                                                       
         CLI   SRTEND+2,X'40'                                                   
         BE    *+8                                                              
         LA    R4,SRTEND                                                        
         CLI   SRTMED,C'N'         NETWORK ?                                    
         BE    PSL070              YES                                          
         GOTO1 =V(GETBROAD),DMCB,(1,(R4)),WORK,GETDAY,ADDAY                     
         LA    R4,WORK+6                                                        
*                                                                               
PSL070   GOTO1 DATCON,DMCB,(0,(R4)),(1,GDAYYMM)                                 
         MVI   GDAYYMM+L'GDAYYMM,0             CLEAR DAY FIELD                  
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                     BUMP TO NEXT ELEMENT                   
         MVI   0(R6),0                   END OF RECORD                          
*                                                                               
PSL090   CP    SLUSH,=P'0'                                                      
         BE    POSTX                     NO ZERO POSTINGS                       
         BAS   RE,PUTAPE                                                        
         B     POSTX                                                            
         DROP  R5,R6                                                            
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
         MVC   PSHDACC+1(3),=C'SZS'     HARD-MEDIA CONTROL ACCOUNTS             
         MVC   PSHDACC+4(1),SAVMED                                              
         CLI   SAVMED,C'N'                                                      
         BNE   PMEDC3                                                           
         CLI   PROFIL7,C'N'             ALTERNATE NETWORK CONTROL               
         BE    PMEDC3                                                           
         CLI   PROFIL7,0                                                        
         BE    PMEDC3                                                           
         MVC   PSHDACC+1(4),=C'SZUN' FOR NETWORK USE SZ?N                       
         MVC   PSHDACC+3(1),PROFIL7                                             
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
         ZAP   TRNAMNT,BUFTOT                                                   
         SP    TRNAMNT,BUFSLUSH   MINUS SLUSH (IF ANY)                          
         SP    TRNAMNT,BUFGST     MINUS GST   (IF ANY)                          
         SP    TRNAMNT,BUFPST     MINUS PST   (IF ANY)                          
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
* MAKE CTA TRADE BUY POSTINGS                                         *         
***********************************************************************         
                                                                                
CTAPOST  NTR1  ,                                                                
         USING CTABLKD,R2                                                       
         LA    R2,SRTCTABK                                                      
         SR    R4,R4                                                            
         IC    R4,SRTCTA#          # CTA ENTRIES                                
*                                                                               
CTA100   MVC   CTS2CODE,SPACES                                                  
         MVC   CTS2CPY,SRTCPY      CPY                                          
         MVC   CTS2UL,=C'S2'       U/L                                          
         MVI   CTS2ASYS,C'S'       SPOT                                         
         MVC   CTS2AMED,SRTMED     MEDIA                                        
         UNPK  WORK(5),CTACTRCT                                                 
         OI    WORK+4,X'F0'                                                     
         MVC   CTS2ACON,WORK       CONTRACT NUMBER                              
         LA    R1,CTS2CODE         GET NAME FOR S2 ACCOUNT                      
         LA    R3,CTS2NAME                                                      
         BAS   RE,RETRNAME                                                      
         BNE   CTA150                                                           
*                                                                               
         BAS   RE,GETAMTS          GET CONTRACT AMOUNTS                         
         L     R5,AIO1                                                          
         AH    R5,FRSTL                                                         
*                                                                               
CTA110   CLI   0(R5),CNTELQ       X'92' CTA CONTRACT ELEMENT                    
         BE    CTA125                                                           
         CLI   0(R5),0                                                          
         BE    CTA150                                                           
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     CTA110                                                           
*                                                                               
         USING CNTELD,R5                                                        
CTA125   MVC   CTS1CODE,SPACES                                                  
         MVC   CTS1CPY,SRTCPY      CPY                                          
         MVC   CTS1UL,=C'S1'       U/L                                          
         MVC   CTS1ASYS,=C'S'      SPOT                                         
         MVC   CTS1AMED,SRTMED     MEDIA                                        
         MVC   CTS1ACON(L'CNTCONTR),CNTCONTR                                    
         OC    CTS1ACT,SPACES                                                   
         DROP  R5                                                               
*                                                                               
         LA    R1,CTS1CODE                                                      
         LA    R3,CTS1NAME                                                      
         BAS   RE,RETRNAME                                                      
         BE    CTA200                                                           
*                                                                               
CTA150   MVC   CTS1CODE,SPACES                                                  
         MVC   CTS1CPY,SRTCPY      CPY                                          
         MVC   CTS1UL,=C'S1'       U/L                                          
         MVC   CTS1CODE+3(10),=CL10'MISSING'                                    
         MVC   CTS1NAME(10),=CL10'MISSING'                                      
*                                                                               
CTA200   DS    0H                                                               
         USING CXRRECD,R5                                                       
         LA    R5,DKEY                                                          
         XC    DKEY,DKEY                                                        
         MVI   CXRKTYP,CXRKTYPQ    X'0D'                                        
         MVI   CXRKSUB,CXRKSUBQ    X'7D'                                        
         MVC   CXRKAGMD,AGYCODE    AGENCY                                       
         OC    CXRKAGMD,MEDCODE    MEDIA                                        
         MVC   CXRKCTA,CTS1ACON    CONTRACTOR                                   
*                                                                               
         GOTO1 ADMGR,SPTHI                                                      
         CLC   DIR(13),DKEY                                                     
         BNE   CTA500                                                           
         GOTO1 ADMGR,SPTGET                                                     
         L     R5,AIO1                                                          
         MVC   CTS1NAME,CXRCTCON   CONTRACTOR NAME                              
         CLC   CXRCTNAM,SPACES                                                  
         BNH   *+10                                                             
         MVC   CTS2NAME,CXRCTNAM   CONTRACT NAME                                
*                                                                               
         LA    RF,CXREL            A(FIRST ELEMENT)                             
         SR    R1,R1                                                            
CTA400   IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLI   0(RF),0                                                          
         BE    CTA500                                                           
         CLI   0(RF),CXADRELQ      X'22' ADDRESS ELEMENT                        
         BNE   CTA400                                                           
*                                                                               
CTA500   SR    R0,R0               CLEAR IO AREA                                
         SR    R1,R1                                                            
         L     RE,ATWORKL                                                       
         LH    RF,=Y(2000)                                                      
         MVCL  RE,R0                                                            
*                                                                               
*              DR S2 / CA S1                                                    
*                                                                               
         USING PSHEADD,R5                                                       
         L     R5,ATWORK                                                        
         MVI   PSHDEL,PSHDELQ      X'50' - POSTING HEADER ELEMENT               
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,CTS2CODE                                                 
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,CTS1CODE                                                
         MVC   PSHDSBNM,CTS1NAME                                                
*                                                                               
         USING TRNELD,R6                                                        
         LA    R6,PSHEADL(R5)                                                   
         MVI   TRNEL,TRNELQ        X'44' - TRANSACTION ELEMENT                  
         GOTO1 DATCON,DMCB,(0,SRTEND),(1,TRNDATE)                               
         MVC   TRNREF(3),SRTCLI                                                 
         MVC   TRNREF+3(3),SRTPROD                                              
         MVI   TRNSUB,0                                                         
         MVI   TRNTYPE,X'21'                                                    
         MVI   TRNSTAT,TRNSDR      DEBIT                                        
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNMOS,MOS                                                       
         ZAP   TRNAMNT,CTAGROSS                                                 
*                                                                               
         MVC   TRNOFFC,OFFICE      CPYSOFF2 ON CPY REC = 2 BYTE OFFICE          
         TM    CMPSTA4,CPYSOFF2                                                 
         BO    CTA600                                                           
         MVC   TRNOFFC,SPACES                                                   
         MVC   TRNOFFC(1),OFFICE   OFFICE ON CPY REC = 1 BYTE OFFICE            
         OC    SRTOFF,SPACES                                                    
         CLI   SRTOFF,C' '                                                      
         BNH   *+10                                                             
         MVC   TRNOFFC(1),SRTOFF                                                
*                                                                               
CTA600   MVI   TRNLN,TRNLN1Q                                                    
         CLI   SRTCOMM,0                                                        
         BE    CTA700                                                           
         CLI   SRTCOMM,5                                                        
         BH    CTA700                                                           
         SR    R1,R1                                                            
         IC    R1,SRTCOMM                                                       
         MHI   R1,40                                                            
         SHI   R1,1                                                             
         MVC   TRNNARR(0),SRTNARR                                               
         EX    R1,*-6                                                           
         LA    R1,TRNLN1Q+1(R1)                                                 
         STC   R1,TRNLN                                                         
CTA700   SR    R1,R1                                                            
         IC    R1,1(,R6)                                                        
         AR    R6,R1                                                            
*                                                                               
         USING XPYELD,R6                                                        
         XC    XPYEL(XPYLN3Q),XPYEL                                             
         MVI   XPYEL,XPYELQ        X'46' EXTRA PAYMENT ELEMENT                  
         MVI   XPYLN,XPYLNQ                                                     
         ZAP   XPYCD,=P'0'                                                      
         MVC   XPYCLI,CLINAME      CLIENT NAME                                  
         MVC   XPYPRO,PRDNAME      PRODUCT NAME                                 
         MVC   XPYAGY,SRTAGY       MEDIA AGENCY                                 
         MVC   XPYSEQ,SRTSEQN      SEQUENCE NUMBER                              
         MVC   XPYINV,SPACES                                                    
         MVC   XPYINV(11),SRTINV                                                
         XC    XPYPER,XPYPER       PAY PERIOD                                   
         MVC   XPYPER(6),SRTSTRT                                                
         CLI   SRTEND+2,X'40'                                                   
         BE    *+10                                                             
         MVC   XPYPER+6(6),SRTEND                                               
         MVC   XPYTYPE,SRTTYPE                                                  
         XC    XPYEST,XPYEST       ESTIMATE NUMBER                              
         LA    R0,3                VALIDATE THE ESTIMATE NUMBER                 
         LA    R1,SRTESTN          MUST BE 3 NUMERIC                            
         CLI   0(R1),C'0'                                                       
         BL    CTA720                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
*                                                                               
         PACK  DUB,SRTESTN                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,XPYEST                                                      
*                                                                               
CTA720   CLC   SRTBMKT,SPACES      ANY BINARY MARKET NUMBER FROM SPOT           
         BE    CTA800                                                           
         OC    SRTBMKT,SRTBMKT     ANY BINARY MARKET NUMBER FROM NET            
         BZ    CTA800                                                           
         MVI   XPYLN,XPYLN2Q                                                    
         MVC   XPYBMKT,SRTBMKT     BINARY MARKET NUMBER                         
*                                                                               
CTA800   BAS   RE,PUTAPE                                                        
*                                                                               
*              CR S1 / CA S2                                                    
*                                                                               
         USING PSHEADD,R5          X'50' POSTING ELEMENT                        
         L     R5,ATWORK                                                        
         MVC   PSHDACC,CTS1CODE                                                 
         MVC   PSHDSBAC,CTS2CODE                                                
         MVC   PSHDSBNM,CTS2NAME                                                
*                                                                               
         USING TRNELD,R6           X'44' TRANSACTION ELEMENT                    
         LA    R6,PSHEADL(R5)                                                   
         MVI   TRNSTAT,0                                                        
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
*                                                                               
CTA900   BAS   RE,PUTAPE                                                        
         LA    R2,L'CTAINFO(R2)                                                 
         BCT   R4,CTA100                                                        
         BAS   RE,CTASPCL          SPECIAL POSTINGS FOR WESTERN                 
         B     POSTX                                                            
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GET AMOUNTS FOR SPECIAL POSTINGS                                    *         
***********************************************************************         
                                                                                
         USING CTGELD,R5                                                        
GETAMTS  NTR1                                                                   
         CLC   CMPALPHA,=C'TY'     TBSNY - DOESN'T NEED THIS                    
         BE    POSTX                                                            
GAMT100  DS    0H                                                               
         ZAP   CTSFAMT,=P'0'                                                    
         ZAP   CTSIAMT,=P'0'                                                    
         ZAP   CTSZAMT,=P'0'                                                    
*                                                                               
         SR    R1,R1                                                            
         L     R5,AIO1                                                          
         AH    R5,FRSTL                                                         
GAMT110  CLI   0(R5),0             EOR?                                         
         BE    POSTX                                                            
         CLI   0(R5),CTGELQ        CATEGORY ELEMENT                             
         BE    GAMT120                                                          
         IC    R1,CTGLN                                                         
         AR    R5,R1                                                            
         B     GAMT110                                                          
*                                                                               
GAMT120  ZAP   DUB,CTAGROSS        GET 85% FOR SZ (TRADE NET)                   
         MP    DUB,=P'85'                                                       
         SRP   DUB,62,5                                                         
         ZAP   CTSZAMT,DUB                                                      
*                                                                               
         ZAP   WORK(10),CTGNPAY         GET EXPENSE DOLLARS FOR SB              
         MP    WORK(10),CTGCPCT                                                 
         DP    WORK(10),CTGGCI          GET THE GCI%                            
         ZAP   DUB,WORK(4)                                                      
         ZAP   WORK(10),DUB                                                     
         MP    WORK(10),CTAGROSS                                                
         SRP   WORK(10),60,5                                                    
         ZAP   CTSFAMT,WORK(10)                                                 
*                                                                               
         ZAP   CTSIAMT,CTSZAMT     SI = TRADE NET - EXPENSE                     
         SP    CTSIAMT,CTSFAMT                                                  
         B     POSTX                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* SPECIAL CTA POSTINGS FOR WESTERN                                    *         
***********************************************************************         
                                                                                
CTASPCL  NTR1                                                                   
         CLC   CMPALPHA,=C'TY'     TBSNY DOES IT THE OLD WAY                    
         BE    POSTX                                                            
*                                                                               
         USING MIOELD,R4                                                        
CTAS100  DS    0H                                                               
         L     R4,AMIBUFF                                                       
CTAS110  CLI   0(R4),X'FF'         EOB                                          
         BE    CTAS130             NONE, SHOW MISSING                           
         CLI   MIOCODE,C'S'        MATCH SYSTEM/MEDIA                           
         BNE   CTAS115                                                          
         CLC   MIOCODE+1(1),SRTMED                                              
         BE    CTAS130                                                          
CTAS115  LA    R4,MIOLNQ(R4)                                                    
         B     CTAS110                                                          
*                                                                               
CTAS130  DS    0H                                                               
         MVC   CTSFACCT,SPACES                                                  
         MVC   CTSFACCT(1),SRTCPY                                               
         MVC   CTSFACCT+1(2),=C'SF'       SF POSTING ACCOUNT                    
         MVC   CTSFACCT+3(1),SRTMED       MEDIA / STATION                       
         MVC   CTSFACCT+4(L'SRTSTA),SRTSTA                                      
*                                                                               
         MVC   CTSIACCT(1),SRTCPY                                               
         MVC   CTSIACCT+1(14),MIOCOMM     SI POSTING ACCT FROM MI REC           
         CLI   0(R4),X'FF'                                                      
         BNE   *+10                                                             
         MVC   CTSIACCT+1(14),=CL14'SIMISSING'                                  
*                                                                               
CTAS200  LA    R1,CTSFACCT         GET NAME FOR SF                              
         LA    R3,CTSFNAME                                                      
         BAS   RE,RETRNAME                                                      
         LA    R1,CTSIACCT         GET NAME FOR SI                              
         LA    R3,CTSINAME                                                      
         BAS   RE,RETRNAME                                                      
*                                                                               
         MVC   CTSZACCT,SPACES                                                  
         MVC   CTSZACCT(1),SRTCPY                                               
         MVC   CTSZACCT+1(3),=CL3'SZS'                                          
         MVC   CTSZACCT+4(1),SRTMED                                             
         LA    R1,CTSZACCT         GET NAME FOR SZ                              
         LA    R3,CTSZNAME                                                      
         BAS   RE,RETRNAME                                                      
*                                                                               
         MVC   CTSJCLI,SPACES      GET NAME FOR CLIENT                          
         MVC   CTSJCLI(1),SRTCPY                                                
         MVC   CTSJCLI+1(2),=C'SJ'                                              
         MVC   CTSJCLI+3(3),SRTCLI                                              
         LA    R1,CTSJCLI                                                       
         LA    R3,CTSJCLIN                                                      
         BAS   RE,RETRNAME                                                      
*                                                                               
         MVC   CTSJPRD,CTSJCLI     GET NAME FOR PRODUCT                         
         MVC   CTSJPRD+6(3),SRTPROD                                             
         LA    R1,CTSJPRD                                                       
         LA    R3,CTSJPRDN                                                      
         BAS   RE,RETRNAME                                                      
*                                                                               
*              DR SZ / CA SJ                                                    
*                                                                               
         USING PSHEADD,R5                                                       
         L     R5,ATWORK                                                        
         MVI   PSHDEL,PSHDELQ      X'50' - POSTING HEADER ELEMENT               
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,CTSZACCT                                                 
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,CTSJCLI                                                 
         MVC   PSHDSBNM,CTSJCLIN                                                
*                                                                               
         USING TRNELD,R6                                                        
         LA    R6,PSHEADL(R5)                                                   
         MVI   TRNEL,TRNELQ        X'44' - TRANSACTION ELEMENT                  
         MVI   TRNLN,TRNLN1Q                                                    
         GOTO1 DATCON,DMCB,(0,SRTEND),(1,TRNDATE)                               
         MVC   TRNREF,CTS2ACON     CONTRACT NUMBER AS REFERENCE                 
         XC    TRNSUB(2),TRNSUB                                                 
         MVI   TRNSTAT,X'80'       DEBIT                                        
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNMOS,MOS                                                       
         MVI   TRNTYPE,X'21'       STATION                                      
         ZAP   TRNAMNT,CTSZAMT                                                  
         MVC   TRNOFFC,BUFOFF                                                   
         BAS   RE,PUTAPE                                                        
*                                                                               
*              CR SI / CA SJ                                                    
*                                                                               
         USING PSHEADD,R5                                                       
         L     R5,ATWORK                                                        
         MVI   PSHDEL,PSHDELQ      X'50' - POSTING HEADER ELEMENT               
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,CTSIACCT                                                 
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,CTSJPRD                                                 
         MVC   PSHDSBNM,CTSJPRDN                                                
*                                                                               
         USING TRNELD,R6                                                        
         LA    R6,PSHEADL(R5)                                                   
         MVI   TRNSTAT,0           CREDIT                                       
         ZAP   TRNAMNT,CTSIAMT                                                  
         BAS   RE,PUTAPE                                                        
*                                                                               
*              CR SF / CA SJ                                                    
*                                                                               
         USING PSHEADD,R5                                                       
         L     R5,ATWORK                                                        
         MVI   PSHDEL,PSHDELQ      X'50' - POSTING HEADER ELEMENT               
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,CTSFACCT                                                 
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,CTSJPRD                                                 
         MVC   PSHDSBNM,CTSJPRDN                                                
*                                                                               
         USING TRNELD,R6                                                        
         LA    R6,PSHEADL(R5)                                                   
         ZAP   TRNAMNT,CTSFAMT                                                  
         LA    R6,TRNLN1Q(R6)                                                   
*&&DO                                                                           
         USING XPYELD,R6                                                        
         XC    XPYEL(XPYLN3Q),XPYEL                                             
         MVI   XPYEL,XPYELQ        X'46' EXTRA PAYMENT ELEMENT                  
         MVI   XPYLN,XPYLNQ                                                     
         ZAP   XPYCD,=P'0'                                                      
         MVC   XPYCLI,CLINAME      CLIENT NAME                                  
         MVC   XPYPRO,PRDNAME      PRODUCT NAME                                 
         MVC   XPYAGY,SRTAGY       MEDIA AGENCY                                 
         MVC   XPYSEQ,SRTSEQN      SEQUENCE NUMBER                              
         MVC   XPYINV,SPACES                                                    
         MVC   XPYINV(11),SRTINV                                                
         XC    XPYPER,XPYPER       PAY PERIOD                                   
         MVC   XPYPER(6),SRTSTRT                                                
         CLI   SRTEND+2,X'40'                                                   
         BE    *+10                                                             
         MVC   XPYPER+6(6),SRTEND                                               
         MVC   XPYTYPE,SRTTYPE                                                  
         XC    XPYEST,XPYEST       ESTIMATE NUMBER                              
         LA    R0,3                VALIDATE THE ESTIMATE NUMBER                 
         LA    R1,SRTESTN          MUST BE 3 NUMERIC                            
         CLI   0(R1),C'0'                                                       
         BL    CTAS220                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         PACK  DUB,SRTESTN                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,XPYEST                                                      
CTAS220  CLC   SRTBMKT,SPACES      ANY BINARY MARKET NUMBER                     
         BE    CTAS300                                                          
         OC    SRTBMKT,SRTBMKT     ANY BINARY MARKET NUMBER FROM NET            
         BZ    CTAS300                                                          
         MVI   XPYLN,XPYLN2Q                                                    
         MVC   XPYBMKT,SRTBMKT     BINARY MARKET NUMBER                         
*&&                                                                             
         USING CNTELD,R6                                                        
CTAS300  DS    0H                                                               
*        LA    R6,XPYLNQ(R6)                                                    
         XC    CNTEL(CNTLNQ),CNTEL                                              
         MVI   CNTEL,CNTELQ        X'92' CTA CONTRACT ELEM                      
         MVI   CNTLN,CNTLNQ                                                     
         MVC   CNTCONTR,CTS1ACON   SAVE CONTRACTOR FOR REPORTING                
         ZAP   CNTGCI,=P'0'        NOT NEEDED FOR REPORTING                     
         ZAP   CNTNCI,=P'0'                                                     
         ZAP   CNTNCI,=P'0'                                                     
         ZAP   CNTOPNPO,=P'0'                                                   
         ZAP   CNTINVPO,=P'0'                                                   
         ZAP   CNTCOST,=P'0'                                                    
*                                                                               
         BAS   RE,PSTAT            POST STATION / REP                           
CTAS400  BAS   RE,PUTAPE                                                        
*                                                                               
         B     POSTX                                                            
         EJECT                                                                  
***********************************************************************         
* RETRIEVE NAME FROM ACCOUNT                                          *         
*   R1 --> ACCOUNT CODE                                               *         
*   R3 --> ACCOUNT NAME                                               *         
***********************************************************************         
                                                                                
RETRNAME NTR1                                                                   
         MVC   0(L'CTSFNAME,R3),SPACES                                          
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(15),0(R1)      READ ACCOUNT                                 
         GOTO1 ADMGR,ACCRD                                                      
         BE    RETRN50                                                          
         MVC   0(10,R3),=CL10'MISSING'                                          
         LTR   RB,RB               SET CC NE                                    
         B     POSTX                                                            
*                                                                               
RETRN50  GOTO1 ADMGR,ACCGET                                                     
         L     R5,AIO1                                                          
         USING ACTRECD,R5                                                       
         BAS   RE,GETNME                                                        
         MVC   0(L'CTSFNAME,R3),WORK                                            
         CR    RB,RB                                                            
         B     POSTX                                                            
         EJECT                                                                  
***********************************************************************         
* POST THE TOTAL RECORD                                               *         
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
         MVC   WORK(0),NAMEREC                                                  
         EX    R1,*-6                                                           
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
         LA    RE,SPOTSEN          SET SPOT SE NUMBER                           
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
         B     DSPTOPN             OPEN SPOT FILES                              
         B     DSPTRD              READ SPOTDIR                                 
         B     DSPTHI              READ HIGH SPOTDIR                            
         B     DSPTSEQ             READ SEQ SPOTDIR                             
         B     DSPTGET             GETREC SPOT FILE                             
         B     DSTARD              READ STATION FILE                            
         B     DSTAHI              READ HIGH STATION FILE                       
         B     DSPTREQ             READ SPOT REQUEST FILE                       
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
* DATA MANAGER ROUTINES - SPOT FILES                                  *         
***********************************************************************         
                                                                                
DSPTOPN  GOTO1 DATAMGR,DMCB,OPEN,SPOT,SPFILEL        OPEN SPOT FILES            
         B     DMXIT                                                            
*                                                                               
DSPTRD   GOTO1 DATAMGR,DMCB,DMREAD,SPTDIR,DKEY,DIR   READ SPOT DIR              
         MVC   DA,DIR+14                                                        
         B     DMERR                                                            
*                                                                               
DSPTHI   GOTO1 DATAMGR,DMCB,DMRDHI,SPTDIR,DKEY,DIR   READ HI SPOT DIR           
         MVC   DA,DIR+14                                                        
         B     DMERR                                                            
*                                                                               
DSPTSEQ  GOTO1 DATAMGR,DMCB,DMRSEQ,SPTDIR,DKEY,DIR   READ SEQ SPOT DIR          
         MVC   DA,DIR+14                                                        
         B     DMERR                                                            
*                                                                               
DSPTGET  GOTO1 DATAMGR,DMCB,GETREC,SPTFIL,DA,AIO1,DMWORK SPOT FILE              
         B     DMERR                                                            
*                                                                               
DSTARD   GOTO1 DATAMGR,DMCB,DMREAD,STATION,DKEY,AIO1    READ STATION            
         B     DMERR                                                            
*                                                                               
DSTAHI   GOTO1 DATAMGR,DMCB,DMRDHI,STATION,DKEY,AIO1    READ HI STATION         
         B     DMERR                                                            
*                                                                               
DSPTREQ  GOTO1 ,DMCB,(X'10',DMRSEQ),REQUEST,DISKAD,AREQIO,ATRKBLK               
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
         B     DSPTREQ                                                          
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
         ORG   AC5302+(4096*5)                                                  
AC53C    CSECT                                                                  
*                                                                               
CENTER   DC    V(CENTER)                                                        
CLPACK   DC    V(CLPACK)                                                        
DATVAL   DC    V(DATVAL)                                                        
HEXIN    DC    V(HEXIN)                                                         
GETLOGO  DC    V(GETLOGO)                                                       
UNDERLIN DC    V(UNDERLIN)                                                      
VATICAN  DC    V(VATICAN)                                                       
PQPROF   DC    V(PQPROF)                                                        
*                                                                               
APOST    DC    A(POST)             POSTING ROUTINE                              
*                                                                               
AOFNTAB  DC    A(OFNTAB)           OFFICE TABLE OF CODES AND NAMES              
AOFFMTAB DC    A(OFFMTAB)          OFFICE TABLE FOR MEDIAS                      
AOFFATAB DC    A(OFFATAB)          OFFICE TABLE FOR AGENCIES                    
AACCUMS  DC    A(ACCUMS)           LEVELS OF ACCUMULATORS                       
ATWORKL  DC    A(TWORKL)           A(LENGTH OF POSTING)                         
ATWORK   DC    A(TWORK)            A(POSTING FILE HEADER)                       
ATWORKT  DC    A(TWORKT)           A(POSTING FILE TRANSACTION)                  
AREQIO   DC    A(REQIO)            A(REQUEST INPUT AREA)                        
AIO1     DC    A(IO1)              A(ACC/SPOT FILE INPUT AREA)                  
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
SPTOPN   EQU   7                   SPOT FILE - OPEN                             
SPTRD    EQU   8                   SPOT DIR  - READ                             
SPTHI    EQU   9                             - HIGH                             
SPTSEQ   EQU   10                            - SEQUENTIAL                       
SPTGET   EQU   11                  SPOT FILE - GETREC                           
STARD    EQU   12                  STATION FILE - READ                          
STAHI    EQU   13                               - HIGH                          
SPTREQ   EQU   14                               - HIGH                          
WRKOPN   EQU   15                  WORKER FILE  - OPEN                          
WRKADD   EQU   16                               - ADD                           
WRKCLO   EQU   17                               - CLOSE                         
ODDOPN   EQU   18                  ODD FILE     - OPEN                          
ODDADD   EQU   19                               - ADD                           
ODDCLO   EQU   20                               - CLOSE                         
ONLOPN   EQU   21                  ONLINE       - OPEN                          
ONLADD   EQU   22                               - ADD                           
ONLCLO   EQU   23                               - CLOSE                         
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
LEVELS   DC    0X'00'                                                           
LEVA     DC    X'00'               LEVEL A LENGTH                               
LEVB     DC    X'00'               LEVEL B LENGTH (A+B)                         
LEVC     DC    X'00'               LEVEL C LENGTH (A+B+C)                       
LEVD     DC    X'00'               LEVEL D LENGTH (A+B+C+D)                     
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
ROWSTA   EQU   1                   STATION                                      
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
CASHRO   DC    PL8'0'              CASH RECEIPT FOR ODDS FILE                   
CASHD    DC    PL8'0'              CASH DISCOUNT                                
TRADE    DC    PL8'0'              TRADE AMOUNT                                 
SLUSH    DC    PL8'0'              SLUSH AMOUNT                                 
INVAMT   DC    PL8'0'              INVOICE AMOUNT                               
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
SPOT     DC    C'SPOT    '                                                      
SPFILEL  DC    C'NSPTDIR NSPTFIL NSTAFIL '                                      
REQFIL   DC    C'NREQ    X '                                                    
SPTDIR   DC    C'SPTDIR  '                                                      
SPTFIL   DC    C'SPTFIL  '                                                      
STATION  DC    C'STATION '                                                      
REQUEST DC     C'REQUEST '                                                      
*                                                                               
SPTREQQ  EQU   37                  SPOT REQUEST FILE                            
SPTPAYQ  EQU   19                  SPOT PAY PROGRAM                             
*                                                                               
SYSNUMB  DC    CL6' '              SYSTEM AND NUMBER (SPOT X)                   
SPTNUM   DS    CL6                 SPOT/NET LOGICAL NUMBER CHARACTER            
SEN      DS    XL1                 BASE SYSTEM NUMBER SPOT IS 2, NET 3          
SENSPOT  EQU   2                                                                
SENNET   EQU   3                                                                
*                                                                               
FLAG     DS    XL1                 FLAG FOR AMOUNT USAGE                        
FLGNET   EQU   X'80'               RUN USES NET AMOUNT                          
*                                                                               
SPOTSEN  DC    X'00'               SPOT SE NUMBER                               
ACCTSEN  DC    X'00'               ACC SE NUMBER                                
SYSTSEN  DC    X'00'               SYSTEM SE NUMBER                             
*                                                                               
ACCTAB   DS    XL20                TABLE OF OPEN ACC FILES                      
*                                                                               
BREAK    DC    X'00'                                                            
CLITQ    EQU   X'80'               CLIENT                                       
STATQ    EQU   X'40'               STATION                                      
REPTQ    EQU   X'20'               REP                                          
MEDTQ    EQU   X'10'               MEDIA                                        
AGYTQ    EQU   X'08'               SOURCE AGENCY                                
CPYTQ    EQU   X'04'               POSTING COMPANY                              
*                                                                               
CLIQ     EQU   CLITQ                                                            
STAQ     EQU   CLITQ+STATQ                                                      
REPQ     EQU   CLITQ+STATQ+REPTQ                                                
MEDQ     EQU   CLITQ+STATQ+REPTQ+MEDTQ                                          
AGYQ     EQU   CLITQ+STATQ+REPTQ+MEDTQ+AGYTQ                                    
CPYQ     EQU   CLITQ+STATQ+REPTQ+MEDTQ+AGYTQ+CPYTQ                              
*                                                                               
CTLTBL   DS    0XL2                                                             
         DC    AL1(SRTAGY-SRTK-1),AL1(CPYQ)     COMPANY POSTING                 
         DC    AL1(SRTMED-SRTK-1),AL1(AGYQ)     AGENCY TOTAL                    
         DC    AL1(SRTREP-SRTK-1),AL1(MEDQ)     MEDIA TOTAL                     
         DC    AL1(SRTSTA-SRTK-1),AL1(REPQ)     REP TOTAL                       
         DC    AL1(SRTCLI-SRTK-1),AL1(STAQ)     STATION TOTAL                   
         DC    AL1(SRTLAST-SRTK-1),AL1(CLIQ)    CLIENT TOTAL                    
*                                                                               
FLGTOT   DC    X'00'               FLAG FOR TOTALS RUN                          
FLGAGY   EQU   X'80'               AGENCY TOTALS                                
FLGRUN   EQU   X'20'               RUN TOTALS                                   
FLGSUM   EQU   X'10'               SUMMARY REPORT                               
FLGIDS   EQU   X'08'               NOW PROCESSING ID REPORTS                    
*                                                                               
*  SORT SEQUENCE IS AGENCY/MEDIA/REP/STATION/CLI/PROD                           
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,NNN,A),FORMAT=BI,WORK=1 '                    
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=NNN '                                  
*                                                                               
POSTC    DS    XL1                 POSTING CONTROL                              
PSTDTL   EQU   1                   POST CREDIT TO PAYABLE ACCOUNT               
PSTMED   EQU   2                   POST MEDIA TOTAL (DEBIT TO SZ)               
PSTSLS   EQU   3                   POST SLUSH TOTAL                             
PSTTOT   EQU   4                   POST THE TOTAL RECORD                        
*                                                                               
ORIGNUM  DS    XL2                 ORIGIN NUMBER FROM SPOT PAY                  
*                                                                               
POSTKEY  DC    XL16'00'            INDEX KEY FOR POSTING FILE                   
ODDKEY   DC    XL16'00'            INDEX KEY FOR ODD FILE                       
ONLNKEY  DC    XL16'00'            INDEX KEY FOR ONLINE TOTAL                   
*                                                                               
RPTIT    DC    C'DAILY CLEARANCE REPORT'                                        
*                                                                               
MEDCODE  DS    XL1                 MEDIA CODE                                   
MEDNAME  DS    CL15                MEDIA NAME                                   
*                                                                               
MEDTAB   DS    0CL17                                                            
         DC    C'T',X'01',CL15'TELEVISION     '                                 
         DC    C'R',X'02',CL15'RADIO          '                                 
         DC    C'N',X'03',CL15'NETWORK        '                                 
         DC    C' ',X'04',CL15'RADIO NETWORK  '                                 
*                                                                               
SAVAUTL  DS    XL1                                                              
SAVPROFL DS    CL(L'PROFILE)                                                    
*                                                                               
*                                                                               
ACTFLAG  DS    XL1                 ACCOUNT FLAG                                 
ACT20    EQU   X'80'               NAME LOCKED - DO NOT ADD NAMELD              
ACT22    EQU   X'40'               ADDRESS LOCKED - DO NOT ADD ADRELD           
*                                                                               
TODAY    DS    XL3                 TODAY PWOS                                   
TODAY6   DS    CL6                 TODAY CHARACTER YYMMDD                       
XDATE    DS    CL6                 EXTRA CLEARANCE DATE                         
MOS      DS    XL2                 MONTH OF SERVICE                             
*                                                                               
CPYCODE  DS    XL1                 ACC COMPANY CODE                             
CMPSTA4  DC    X'00'               COMPANY STATUS 4                             
CMPSTA8  DC    X'00'               COMPANY STATUS 8                             
CMPALPHA DS    CL2                 COMPANY ALPHA CODE                           
COMREP   DS    CL3                 CODE FOR COMMISSION ONLY REP                 
AGID     DS    XL2                 ACC PRINCIPAL ID                             
*DST1    DC    H'5'                ID FOR DATA CONTROL PAGE                     
DDST1    DC    H'498'              ID FOR DATA CONTROL PAGE (DDSZ1)             
CTRY     DS    XL1                 COUNTRY CODE                                 
PAYACCT  DS    CL15                PAYABLE ACCOUNT                              
PAYNAME  DS    CL36                ACCOUNT NAME                                 
TAXUL    DS    CL2                 UNIT/LEDGER FOR GST                          
*                                                                               
AGYCODE  DC    XL1'00'             SPOT AGENCY CODE                             
AGYNM    DC    CL33' '             AGENCY NAME                                  
CLINAME  DC    CL20' '             CLIENT NAME                                  
OFFICE   DC    CL2' '              CLIENT OFFICE                                
SAVPRD   DC    CL3' '              PRODUCT CODE                                 
PRDNAME  DC    CL20' '             PRODUCT NAME                                 
*                                                                               
STANM    DC    CL20' '             STATION NAME                                 
STAAD    DC    CL105' '            STATION ADDRESS                              
         ORG   STAAD+1                                                          
STASTR   DC    CL26' '             STREET ADDRESS                               
STACTST  DS    0CL26               CITY STATE                                   
STACTY   DC    CL22' '             CITY                                         
         DC    CL1' '                                                           
STASTA   DC    CL3' '              STATE                                        
STAZIP   DC    CL26' '             ZIP                                          
         DC    CL26' '                                                          
*                                                                               
REPNAME  DC    CL22' '             REP NAME                                     
REPUBAD  DC    CL105' '            REP ADDRESS                                  
*                                                                               
CRACCT   DC    CL12' '                                                          
LASTOFF  DC    CL2' '                                                           
*                                                                               
ITEMS    DC    X'00'               ITEM STATUS                                  
ITMREP   EQU   X'80'               REP                                          
ITMUNW   EQU   X'40'               UNWIRED REP                                  
ITMCOMO  EQU   X'20'               COMMISSION ONLY REP                          
*                                                                               
BUFLINE  DS    0CL130              *** BUFFALO RECORD ***                       
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
BUFCASHO DC    PL8'0'                                                           
BUFCASHD DC    PL8'0'                                                           
BUFTRADE DC    PL8'0'                                                           
BUFSLUSH DC    PL8'0'                                                           
BUFINVAM DC    PL8'0'                                                           
BUFCONET DC    PL8'0'                                                           
BUFCOGST DC    PL8'0'                                                           
BUFCOPST DC    PL8'0'                                                           
BUFTOT#  EQU   (*-BUFTOTAL)/L'BUFTOTAL                                          
BUFLINQ  EQU   *-BUFLINE                                                        
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
NOSPOTF  DC    PL2'0'                                                           
*                                                                               
MIELM    DC    CL(MIOLNQ)' '      CURRENT MI ELEMENT                            
REMOPT   DC    CL1' '                                                           
*                                                                               
PROFILE  DS    CL16                                                             
         ORG   PROFILE                                                          
PROFIL1  DS    CL1                 PRINT DETAILS FOR SPOT         Y,N           
PROFIL2  DS    CL1                 PRINT DETAILS FOR PRINT        Y,N           
PROFIL3  DS    CL1                 POST SPOT CASH REC BY MEDIA    Y,N           
PROFIL4  DS    CL1                 POST PRINT CASH REC BY MEDIA   Y,N           
PROFIL5  DS    CL1                 USE PRODUCT NAME FOR SPOT      Y,N           
PROFIL6  DS    CL1                 USE LEDGER U FOR NETWORK       Y,N           
PROFIL7  DS    CL1                 ALTERNATE NETWORK CONTROL-SZ   U-Z,N         
PROFIL8  DS    CL1                 ADD'L PRINT REPORT BY ID       N,Y           
PROFIL9  DS    CL1                 ADD'L SPOT REPORT BY ID        N,Y           
PROFIL10 DS    CL1                 ADD'L NETWORK REPORT BY ID     N,Y           
PROFIL11 DS    CL1                 ALTERNATE CODE FOR LOST C.D.   A-Z           
PROFIL12 DS    CL1                 ANALYSIS BY OFFICE             Y,N           
PROFIL13 DS    CL1                 N/D                                          
PROFIL14 DS    CL1                 N/D                                          
PROFIL15 DS    CL1                 N/D                                          
PROFIL16 DS    CL1                 N/D                                          
*                                                                               
CTS1CODE DS    0XL15               CONTRACTOR ACCOUNT CODE                      
CTS1CPY  DC    X'00'                                                            
CTS1UL   DC    XL2'00'                                                          
CTS1ACT  DS    0CL12                                                            
CTS1ASYS DC    CL1' '              SYSTEM                                       
CTS1AMED DC    CL1' '              MEDIA                                        
CTS1ACON DC    CL6' '              CONTRACTOR                                   
         DC    XL4'00'             N/D                                          
CTS1NAME DC    CL36' '             CONTRACTOR ACCOUNT NAME                      
CTS2CODE DS    0XL15               CONTRACT ACCOUNT CODE                        
CTS2CPY  DC    X'00'                                                            
CTS2UL   DC    XL2'00'                                                          
CTS2ACT  DS    0CL12                                                            
CTS2ASYS DC    CL1' '              SYSTEM                                       
CTS2AMED DC    CL1' '              MEDIA                                        
CTS2ACON DC    CL5' '              CONTRACT NUMBER                              
         DC    XL5'00'             N/D                                          
CTS2NAME DC    CL36' '             CONTRACT ACCOUNT NAME                        
*                                                                               
CTSZACCT DS    CL15                SZ POSTING ACCOUNT                           
CTSZNAME DS    CL36                SZ POSTING ACCOUNT NAME                      
CTSZAMT  DS    PL8                 AMOUNT TO POST FOR SZ                        
CTSFACCT DS    CL15                SF POSTING ACCOUNT                           
CTSFNAME DS    CL36                SF POSTING ACCOUNT NAME                      
CTSFAMT  DS    PL8                 AMOUNT TO POST FOR SF                        
CTSIACCT DS    CL15                SI POSTING ACCOUNT                           
CTSINAME DS    CL36                SI POSTING ACCOUNT NAME                      
CTSIAMT  DS    PL8                 AMOUNT TO POST FOR SI                        
CTSJCLI  DS    CL15                SJ CLIENT ACCOUNT                            
CTSJCLIN DS    CL36                CLIENT NAME                                  
CTSJPRD  DS    CL15                SJ PRODUCT ACCOUNT                           
CTSJPRDN DS    CL36                PRODUCT NAME                                 
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
SAVREPT  DS    CL(L'REQREPT)       TYPE                                         
SAVSTA   DS    CL(L'SRTSTA)        STATION                                      
SAVCLI   DS    CL(L'SRTCLI)        CLIENT                                       
SAVSTAN  DS    CL(L'STANM)         STATION NAME                                 
SAVSLSHC DS    CL(L'ACTKULA)       SLUSH ACCOUNT CODE (U/L/A)                   
*                                                                               
SRTWRK   DS    0CL(SRTLEN)                                                      
SRTK     DS    0C                  SORT KEY                                     
SRTOID   DS    XL2                 ORIGIN ID                                    
SRTASE   DS    XL1                 ACC SE NUMBER                                
SRTCPY   DS    XL1                 ACC COMPANY CODE                             
SRTAGY   DS    CL(L'REQAGY)        AGENCY ALPHA                                 
SRTMED   DS    CL(L'REQMED)        MEDIA                                        
SRTREP   DS    CL(L'REQREP)        REP                                          
SRTREPT  DS    CL(L'REQREPT)       TYPE                                         
SRTSTA   DS    CL(L'REQSTA)        STATION                                      
SRTCLI   DS    CL(L'REQCLI)        CLIENT                                       
SRTLAST  EQU   *                                                                
SRTBKLNQ EQU   *-SRTK              CONTROL BREAK                                
SRTPROD  DS    CL(L'REQPROD)       PRODUCT                                      
SRTSEQ   DS    PL3                 SEQUENCE                                     
SRTKLEN  EQU   *-SRTK              LENGTH OF SORT KEY                           
SRTORIG  DS    XL2                 ORIGIN NUMBER                                
SRTESTN  DS    CL(L'REQESTN)       ESTIMATE NUMBER                              
SRTSTAT  DS    XL1                 STATUS                                       
SRTONLNE EQU   X'80'               CLEARED ONLINE                               
SRTAUTO  EQU   X'40'               AUTO CLEARANCE                               
SRTDAYP  DS    XL3                 TODAY PWOS                                   
SRTOAGY  DS    CL(L'REQOAGY)       OVERRIDE AGENCY                              
SRTCOMM  DS    XL1                 # OF COMMENTS IN REQNARR (0-5)               
SRTSEQN  DS    XL1                 CLEARANCE SEQUENCE NUMBER                    
SRTSTRT  DS    CL(L'REQSTRT)                                                    
SRTEND   DS    CL(L'REQEND)                                                     
SRTPROD2 DS    CL(L'REQPROD2)                                                   
SRTPROD3 DS    CL(L'REQPROD3)                                                   
SRTTYPE  DS    CL1                                                              
SRTTPAY  EQU   C'1'                PAYMENT                                      
SRTTCRM  EQU   C'2'                CREDIT MEMO                                  
SRTTCHK  EQU   C'3'                CHECK - A CASH RECEIPT                       
SRTPNET  DS    PL6                 PACKED NET - FITS 999,999,999.99             
SRTGSTYP DS    CL1                 GST TAX CODE TYPE                            
SRTPGST  DS    XL4                 GST TAX AMOUNT                               
SRTOFF   DS    CL2                 OFFICE                                       
SRTINV   DS    CL12                INVOICE NUMBER                               
SRTSMED  DS    CL1                 SUB-MEDIA (NET)                              
SRTBMKT  DS    XL2                 BINARY MARKET NUMBER                         
*                                                                               
SRTCARD2 DS    0CL80                                                            
SRTPST   DS    0CL42               PST BLOCK                                    
SRTPSTPR DS    CL2                 PST PROVINCE CODE                            
SRTPSTYP DS    CL1                 PST TYPE                                     
SRTPSTAM DS    XL4                 PST AMOUNT                                   
SRTPSTLQ EQU   *-SRTPST            PST ENTRY LENGTH                             
         ORG   SRTPST+L'SRTPST                                                  
         DS    XL38                N/D                                          
         ORG   SRTPST                                                           
SRTCTABK DS    5XL10               CTA BLOCK 5 ITEMS XL10 (CTABLKD)             
SRTPGRS  DS    PL6                                                              
SRTPID   DS    XL2                                                              
SRTPNET2 DS    PL6                 MOVED INTO SRTPNET                           
SRTGIND  DS    XL1                 X'80' GROSS IS A CALCULATION                 
SRTSLUSH DS    PL3                 ROUNDING DIFFERENCE                          
SRTINVAM DS    PL5                 INVOICE AMOUNT                               
SRTCSHD  DS    XL(L'REQCD)         CASH DISCOUNT                                
         DS    XL2                 N/D                                          
SRTCTA#  DS    XL1                 NUMBER OF CTA ENTRIES                        
SRTCTA   DS    CL1                 CTA INDICATOR                                
SRTCTAQ  EQU   C'T'                                                             
         ORG   SRTCARD2+L'SRTCARD2                                              
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
         DC    CL8'**IO1** '       ACC/SPOT FILE INPUT AREA                     
IO1      DC    4000X'00'                                                        
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
MIMAX    EQU   2000                                                             
         DS    0D                                                               
         DC    CL8'*PROFBUF'       BUFFER FOR CONTROL PROFILES                  
PROFBUF  DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*TRKBLK*'       BUFFER FOR FULL TRACK READ                   
TRKBLK   DC    60000X'00'                                                       
*                                                                               
         BUFF  LINES=50,ROWS=1,COLUMNS=13,FLAVOR=PACKED,KEYLIST=(26,A)          
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,MACRF=GM,EODAD=PUTSX,            X        
               RECFM=VB,LRECL=8200                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST RECORD DSECT                                                *         
***********************************************************************         
                                                                                
REQD     DSECT                                                                  
REQPROG  DS    CL2                C'30' FOR CHECK ELEMENTS                      
REQAGY   DS    CL2                                                              
REQMED   DS    CL1                                                              
REQCLI   DS    CL3                                                              
REQESTN  DS    CL3                                                              
REQPROD  DS    CL3                                                              
REQSTAT  DS    CL1                 STATUS                                       
REQONLNE EQU   X'80'               CLEARED ONLINE                               
REQAUTO  EQU   X'40'               AUTO CLEARANCE                               
         DS    XL1                                                              
REQBMKT  DS    XL2                 BINARY MARKET NUMBER FOR MGRPID              
REQSTA   DS    CL5                                                              
REQTODAY DS    CL6                                                              
REQREP   DS    CL3                                                              
REQREPT  DS    CL1                                                              
REQOAGY  DS    CL2                 OVERRIDE AGENCY                              
REQCOMM  DS    CL1                 # OF COMMENTS IN REQNARR (0-5)               
REQSEQN  DS    XL1                 CLEARANCE SEQUENCE NUMBER                    
REQSTRT  DS    CL6                                                              
REQEND   DS    CL6                                                              
REQPROD2 DS    CL3                                                              
REQPROD3 DS    CL3                                                              
REQTYPE  DS    CL1                                                              
REQTPAY  EQU   C'1'                PAYMENT                                      
REQTCRM  EQU   C'2'                CREDIT MEMO                                  
REQTCHK  EQU   C'3'                CHECK - A CASH RECEIPT                       
REQNET   DS    CL10                                                             
         ORG   REQNET                                                           
REQPNET  DS    PL5                 PACKED NET - SEE REQPNET2                    
REQGSTYP DS    CL1                 GST TAX CODE TYPE                            
REQPGST  DS    XL4                 GST TAX AMOUNT (BINARY 6/22/99)              
REQOFF   DS    CL2                 OFFICE                                       
REQINV   DS    CL12                INVOICE NUMBER                               
*                                                                               
REQCARD2 DS    0CL80                                                            
REQPST   DS    0CL42               PST BLOCK                                    
REQPSTPR DS    CL2                 PST PROVINCE CODE                            
REQPSTYP DS    CL1                 PST TYPE                                     
REQPSTAM DS    XL4                 PST AMOUNT                                   
REQPSTLQ EQU   *-REQPST            PST ENTRY LENGTH                             
         ORG   REQPST+L'REQPST                                                  
         DS    XL38                N/D                                          
         ORG   REQPST                                                           
REQCTABK DS    5XL10               CTA BLOCK 5 ITEMS XL10 (CTABLKD)             
REQPGRS  DS    PL6                 GROSS AMOUNT                                 
REQPID   DS    XL2                 PID NUMBER                                   
REQPNET2 DS    PL6                 NET AMOUNT                                   
REQGIND  DS    XL1                 X'80' GROSS IS A CALCULATION                 
*                                  X'20' PST IS IN BINARY                       
REQSMED  DS    CL1                 SUB-MEDIA (NET)                              
         ORG   REQSMED                                                          
REQSLUSH DS    PL3                 ROUNDING DIFFERENCE                          
REQINVAM DS    PL5                 INVOICE AMOUNT                               
REQCD    DS    PL3                 CASH DISCOUNT                                
         DS    CL2                 SPARE                                        
REQCTA#  DS    XL1                 NUMBER OF CTA ENTRIES                        
REQCTA   DS    CL1                 CTA INDICATOR                                
REQCTAQ  EQU   C'T'                                                             
REQNARR  DS    CL200               COMMENTS (5 COMMENTS CL40 EACH)              
REQLEN   EQU   *-REQD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
         ORG   PLINED+1                                                         
PLINE    DS    0CL132              ENTIRE PRINT LINE                            
PANAL    DS    0CL18               ANALYSIS LINE                                
PREP     DS    CL3                 REP                                          
         DS    CL2                                                              
PSTA     DS    CL5                 STATION                                      
         ORG   PLINED+14                                                        
PCLIENT  DS    CL3                                                              
         DS    CL3                                                              
PCLINAME DS    CL20                                                             
         ORG   PLINE+14                                                         
POFFCDE  DS    CL2                                                              
         DS    CL3                                                              
POFFNME  DS    CL36                                                             
         ORG   PLINED+21                                                        
PINVOICE DS    CL11                                                             
         ORG   PLINED+33                                                        
PTOTLINE DS    0CL25               TOTALS                                       
PSTART   DS    CL6                 START DATE                                   
         DS    CL1                                                              
PEND     DS    CL6                 END DATE                                     
         ORG   PLINED+47                                                        
PPRODNM  DS    0CL20               PRODUCT NAME                                 
PPROD1   DS    CL3                                                              
PPROD1#  DS    CL1                                                              
PPROD2   DS    CL3                                                              
PPROD2#  DS    CL1                                                              
PPROD3   DS    CL3                                                              
         ORG   PPRODNM                                                          
PTOTLIN2 DS    CL14                TOTAL LINE                                   
         ORG   PLINED+68                                                        
PTOTCR   DS    0CL12               CASH RECIEPT TOTAL                           
         DS    CL5                                                              
PONLINE  DS    CL1                 ONLINE INDICATOR                             
PTYPE    DS    CL2                 TYPE (REGULAR, ETC...)                       
PIND     DS    CL1                 SLUSH INDICATOR (*)                          
         ORG   PLINED+82                                                        
PTOTGST  DS    CL10                GST TOTAL                                    
         ORG   PLINED+94                                                        
PTOTPST  DS    CL10                PST TOTAL                                    
         ORG   PLINED+105                                                       
PTOTCD   DS    CL12                CASH DISCOUNT TOTAL                          
         ORG   PLINED+120                                                       
PTOTNET  DS    CL12                NET TOTAL                                    
         ORG                                                                    
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
         EJECT                                                                  
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
                                                                                
                                                                                
***********************************************************************         
* DSECT FOR CTA BLOCK                                                 *         
***********************************************************************         
                                                                                
CTABLKD  DSECT                                                                  
CTAINFO  DS    0XL10                                                            
CTACTRCT DS    PL4                 CONTRACT NUMBER                              
CTAGROSS DS    PL6                 GROSS AMOUNT                                 
CTABMAX  EQU   5                   MAX NUMBER ENTRIES IN CTA BLOCK              
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
ODDAGY   DS    CL2                 SPOT OR PRINT AGENCY CODE                    
ODDAMT   DS    PL6                 AMOUNT FROM CLEARANCE AC53                   
ODDNAM   DS    CL23                AGENCY NAME                                  
ODDSTAT  DS    XL1                 STATUS BYTE                                  
ODDSSL   EQU   X'80'               LINE HAS A SLUSH AND NOT TRADE               
ODDCR    DS    PL5                 CASH RECEIPT                                 
ODDSE    DS    CL1                 ACCOUNT SE NUMBER                            
ODDCOMO  DS    PL6                 COMMISSION ONLY                              
ODDTRADE DS    PL6                 TRADE BUYS                                   
         ORG   ODDTRADE                                                         
ODDSLUSH DS    PL6                 SLUSH AMOUNT                                 
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
* DMDDNAMED                                                                     
DDNAMED  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DMDDNAMED                                                      
         PRINT ON                                                               
* SPGENADD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENADD                                                       
         PRINT ON                                                               
* SPGENAGY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
* SPGENCLT                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
* SPGENPRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENPRD                                                       
         PRINT ON                                                               
* SPGENREP (WITH X_ PREFIX)                                                     
         PRINT OFF                                                              
*PREFIX=X_                                                                      
       ++INCLUDE SPGENREP                                                       
*PREFIX=                                                                        
         PRINT ON                                                               
* SPGENCTR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENCTR                                                       
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
**PAN#1  DC    CL21'096ACREP5302 11/03/16'                                      
         END                                                                    
