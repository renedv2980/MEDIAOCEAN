*          DATA SET ACREPIR02  AT LEVEL 013 AS OF 04/10/15                      
*PHASE ACIR02A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'ACIR - BILLING AND ESTIMATE INTERFACE'                          
***********************************************************************         
*                                                                     *         
*        NOTE: WHEN REQUESTED BY YNRA, MUST ALSO HAVE 'ALL EXCEPT D'  *         
*              AS THE OFFICE                                          *         
*              WHEN REQUESTED BY YNBR, MUST HAVE OFFICE D IN REQ      *         
*        AS PER RJR - THEY WANT ABOVE DATA ON TWO DIFFERNT TAPES      *         
*                                                                     *         
***********************************************************************         
ACIR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**IR02**,R9                                                    
         USING ACWORKD,RA                                                       
         L     RA,0(R1)                                                         
         USING ACIR02D,RC                                                       
         LA    RC,SPACEND                                                       
         CLI   MODE,RUNFRST                                                     
         BE    RUNINIT                                                          
         CLI   MODE,REQFRST                                                     
         BE    REQINIT                                                          
         CLI   MODE,LEVBFRST                                                    
         BE    PRODRT                                                           
         CLI   MODE,PROCACC                                                     
         BE    ACCRT                                                            
         CLI   MODE,PROCTRNS                                                    
         BE    TRANSRT                                                          
         CLI   MODE,REQLAST                                                     
         BE    GETSORT                                                          
EXIT     XIT1                                                                   
         EJECT                                                                  
RUNINIT  L     RF,=A(ADRC)         SAVE RC FOR BINADD                           
         ST    RC,0(RF)                                                         
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   EXIT                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
         USING SORTD,R2                                                         
REQINIT  LA    R2,SORTREC                                                       
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         MVC   STALL,SPACES        CLEAR RECORD                                 
         CLC   QSTART(L'QSTART+L'QEND),SPACES                                   
         BNH   NODATE                                                           
         CLC   QSTART,SPACES                                                    
         BNE   *+10                                                             
         MVC   QSTART,QEND                                                      
         CLC   QEND,SPACES                                                      
         BNE   *+10                                                             
         MVC   QEND,QSTART                                                      
         GOTO1 DATCON,DMCB,(0,QEND),(1,QEND3)                                   
         GOTO1 DATCON,DMCB,(0,QSTART),(1,QSTR3)                                 
TAPE     XC    TABLE(4),TABLE      CLEAR NUMBER OF ENTRIES                      
         CLI   QOPT1,C'Y'          ARE WE CREATING A TAPE ?                     
         BNE   CLIENT              NO, DON'T DO ALLOCATE                        
         AP    TPVOLS,=P'1'                                                     
         CVB   R7,TPVOLS                                                        
         MVC   TPDSN,=CL20'ACCTAPE.AC0IRXX1'                                    
         MVC   TPDSN+13(L'ALPHAID),ALPHAID DYNAMIC ALLOCATION                   
         GOTO1 DYNALLOC,DMCB,(0,=CL8'OUTP'),((R7),TPDSN)                        
         OPEN  (OUTP,(OUTPUT))                                                  
CLIENT   MVC   MYKEY,SPACES        PUT CLIENT NAME INTO HEADER                  
         MVC   MYKEY(1),QCOMPANY                                                
         MVC   MYKEY+1(2),=C'SJ'                                                
         MVC   MYKEY+3(3),QACCOUNT                                              
         MVC   HEADHOLD,SPACES                                                  
         LA    R7,BUFFER                                                        
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,(R7)                       
         CLI   DMCB+8,0                                                         
         BNE   AGENCY                                                           
         CLC   MYKEY,0(R7)                                                      
         BNE   AGENCY                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   AGENCY                                                           
         USING ACNAMED,R7                                                       
         SR    RE,RE                                                            
         IC    RE,ACNMLEN                                                       
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   HEADHOLD(0),ACNMNAME                                             
AGENCY   ZAP   TPCT,=P'0'                                                       
         ZAP   SORTCT,=P'0'                                                     
         MVC   STAGY,=C'99'                                                     
         LA    R7,AGYTBL                                                        
AGYLOOP  CLC   0(1,R7),QCOMPANY    MOVE CORRECT AGENCY CODE TO                  
         BNE   AGYNEXT              SORT RECORD WHEN LOCATED                    
         MVC   STAGY,1(R7)                                                      
         CLC   ORIGINUM,=H'1614'    YNR OVERRIDES STAGY AT ORIGIN LEVEL         
         BNE   EXIT                                                             
         MVC   STAGY,=C'09'                                                     
         OI    RUNSTAT,YNBR                                                     
         B     EXIT                                                             
*                                                                               
AGYNEXT  LA    R7,L'AGYTBL(R7)                                                  
         CLI   0(R7),X'00'         EXIT IF END IS LOCATED FIRST                 
         BNE   AGYLOOP                                                          
         B     EXIT                                                             
*                                                                               
NODATE   GOTO1 DATCON,DMCB,(5,QSTR3),(1,QSTR3)                                  
         MVC   QEND3,QSTR3                                                      
         B     TAPE                                                             
         DROP  R7                                                               
         EJECT                                                                  
PRODRT   NI    RUNSTAT,X'FF'-GOODTEST                                           
         LA    R2,SORTREC                                                       
         MVC   STCMP,=C'04'        MOVE IN COMPANY CODE                         
         USING ACPROFD,R7                                                       
         L     R7,ADLVBSUP         PRODUCT PROFILE ELEMENT                      
         CLI   ACPRUNIT,C'D'                                                    
         BNE   PROD01                                                           
         OI    RUNSTAT,OFFD                                                     
PROD01   TM    RUNSTAT,YNBR+OFFD   IF YNBR, MUST BE OFFICE D OR IF              
         BM    EXIT                NOT YNBR, NOT OFFICE D                       
         OI    RUNSTAT,GOODTEST                                                 
         TM    RUNSTAT,YNBR+OFFD   TEST AGAIN                                   
         BO    EXIT                YNBR, OFFICE D; I'M DONE                     
*                                                                               
         L     R6,ADHEIRB          GET PRODUCT                                  
         LA    R7,PRODTBL          LIST OF PRODUCTS W/ STCMP OVERRIDE           
         LA    R1,PRODNUM          NUMBER OF PRODUCTS IN TABLE                  
PROD02   CLC   6(3,R6),0(R7)                                                    
         BNE   PROD03                                                           
         MVC   STCMP,=C'79'                                                     
         B     EXIT                                                             
*                                                                               
PROD03   LA    R7,L'PRODTBL(R7)                                                 
         BCT   R1,PROD02                                                        
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
ACCRT    MVI   FCRDTRNS,C'N'                                                    
*                                                                               
         USING SORTD,R2                                                         
         LA    R2,SORTREC                                                       
         CLC   STAGY,=C'99'        EXIT IF : NOT VALID AGENCY                   
         BE    EXIT                                                             
         TM    RUNSTAT,GOODTEST                                                 
         BNO   EXIT                                                             
         MVI   FCRDTRNS,C'Y'                                                    
         XC    STERR,STERR                                                      
*                                                                               
         USING BIND,R5                                                          
         L     R5,ATABLE           REINIT TABLE IF INV#/WC/AMOUNTS              
         XC    BININ,BININ                                                      
         XC    JBDATA(JBDATALN),JBDATA                                          
*                                                                               
         BAS   RE,ACCOUNT          GET ESTIMATE NUMBER                          
         DROP  R5                                                               
*                                                                               
         BAS   RE,LOOKUP                                                        
         USING JBCOLD,R3                                                        
         USING JBLOCKD,R5                                                       
         LH    R1,JBNROWS                                                       
         CH    R1,=H'2'            IF NO MORE THAN 2 ROWS, THERE ARE            
         BNH   EXIT                  NO ESTIMATES                               
         DROP  R3,R5                                                            
*                                                                               
*                                                                               
         USING ACNAMED,R7                                                       
         L     R7,ADACCNAM         GET ESTIMATE (JOB) NAME                      
         MVC   ESTNAME,SPACES                                                   
         SR    RE,RE                                                            
         IC    RE,ACNMLEN                                                       
         SH    RE,=H'3'                                                         
         EX    RE,*+8               AND SAVE IT FOR LATER                       
         B     *+10                                                             
         MVC   ESTNAME(0),ACNMNAME                                              
*                                                                               
NUMBER   MVI   ELCODE,X'25'        GET NUMBER ELEMENT                           
         BAS   RE,GETELIO                                                       
         BNE   PROMO                                                            
         USING ACNOD,R7                                                         
         MVC   JBREASRV,ACNO       GET REASON FOR REVISION (NUM2=)              
*                                                                               
PROMO    MVI   ELCODE,X'27'        GET ACCOUNT BILLING ELEMENT                  
         BAS   RE,GETELIO                                                       
         BNE   PROMO50                                                          
         USING ACABILLD,R7                                                      
         MVC   JBPROMO,ACABBUNO                                                 
         CLC   JBPROMO,SPACES                                                   
         BNE   STATUS                                                           
*                                                                               
PROMO50  L     R7,ADACC            GET MEDIA                                    
         CLI   9(R7),C'P'          IF MEDIA 'P', MUST HAVE                      
         BNE   STATUS              PROMO NUMBER                                 
         OI    STERR,NOPROMO                                                    
*                                                                               
STATUS   MVI   ELCODE,X'30'        GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BNE   DUMP                                                             
*                                                                               
         USING ACSTATD,R7                                                       
         MVI   STTPTYP,C' '        INIT                                         
         TM    ACSTSTAT,X'40'      IF X'40' ON, JOB IS CLOSED                   
         BNO   *+8                                                              
         MVI   STTPTYP,C'4'                                                     
*                                                                               
         MVI   SUBCOMP,C' '       INIT SUBCOMP                                  
         CLI   ACSTSUB,C' '       IS THERE ONE ON THIS JOB                      
         BNH   *+10               NO, LEAVE THE SPACE                           
         MVC   SUBCOMP,ACSTSUB                                                  
*                                                                               
         MVI   ELCODE,X'26'        GET JOB ELEMENT                              
         BAS   RE,GETELIO                                                       
         BNE   DUMP                                                             
         USING ACJOBD,R7                                                        
*                                                                               
GETQTR   LA    R6,PUBQTAB                                                       
         LA    R5,PUBQNUM                                                       
         MVC   JBPPQ,SPACES                                                     
GETQ1    CLC   0(1,R6),SUBCOMP                                                  
         BNE   GETQ2                                                            
         MVC   JBPPQ(4),1(R6)    FOUND SUBCOMP                                  
         B     USERFLDS            EXIT LOOP                                    
GETQ2    LA    R6,L'PUBQTAB(R6)                                                 
         BCT   R5,GETQ1                                                         
         OI    STERR,NOPUBQ       PRINT ERROR MESSAGE                           
*                                                                               
USERFLDS BAS   RE,LOADUSER         GET ANY USER FIELDS DEFINED                  
         MVC   STENO,JBENO                                                      
*                                                                               
         CLC   JBPPQ+1(L'JBPPQ-1),SPACES DID THEY PUT A CODE                    
         BNE   CLOSED                    NO, THEY PUT IN THE QUARTER            
*                                                                               
         LA    R6,PUBQTAB                                                       
         LA    R5,PUBQNUM                                                       
REGETQ1  CLC   0(1,R6),JBPPQ                                                    
         BNE   REGETQ2                                                          
         MVC   JBPPQ(4),1(R6)    FOUND CODE IN TABLE                            
         B     CLOSED              EXIT LOOP                                    
REGETQ2  LA    R6,L'PUBQTAB(R6)                                                 
         BCT   R5,REGETQ1                                                       
*                                                                               
CLOSED   CLI   STTPTYP,C'4'                                                     
         BNE   ACCEST                                                           
         CLC   ACJBCLOS,QSTR3      VERIFY WITHIN DATE RANGE                     
         BL    ACCEST                                                           
         CLC   ACJBCLOS,QEND3                                                   
         BH    ACCEST                                                           
         MVI   STRECTYP,C'0'       INDICATE NO DETAIL                           
         BAS   RE,SENDIT            AND SORT                                    
*                                                                               
ACCEST   OC    ACJBREV,ACJBREV                                                  
         BZ    *+10                                                             
         MVC   JBREVDAT,ACJBREVD   ACCOUNT ESTIMATE DATA                        
*        MVC   JBNEWDAT,ACJBOPND                                                
         TM    ACJBSTAT,ACJBNEWQ                                                
         BNO   *+8                                                              
         BAS   RE,NEWESTS          GET NEW ESTIMATE DATES                       
*                                                                               
REVISED  OC    JBREVDAT,JBREVDAT   HAS ESTIMATE BEEN REVISED ?                  
         BZ    NEW                 NO, SEE IF NEW                               
         CLC   JBREVDAT,QSTR3      YES, VERIFY WITHIN DATE RANGE                
         BL    NEW                                                              
         CLC   JBREVDAT,QEND3                                                   
         BH    NEW                                                              
*                                                                               
         MVI   STTPTYP,C'2'        INDICATE REVISED ESTIMATE                    
         BAS   RE,ESTWC              GET WC AMOUNTS                             
         BAS   RE,ESTDATA            THEN ESTIMATE DATA                         
         BAS   RE,ESTCOMM           THEN ESTIMATE COMMENTS                      
*                                                                               
NEW      CLI   ACJBLEN,X'8'        IS THIS EL TOO SMALL TO HAVE A DATE          
         BNH   EXIT                YES                                          
         OC    JBNEWDAT,JBNEWDAT   IS THERE AN ESTIMATE CREATE DATE             
         BZ    EXIT                NO,                                          
         CLC   JBNEWDAT,QSTR3      SEE IF OPENED IN DATE RANGE                  
         BL    EXIT                                                             
         CLC   JBNEWDAT,QEND3                                                   
         BH    EXIT                                                             
         MVI   STTPTYP,C'1'        INDICATE NEW ESTIMATE                        
         BAS   RE,ESTWC             GET WC AMOUNTS                              
         BAS   RE,ESTDATA           THEN ESTIMATE DATA                          
         BAS   RE,ESTCOMM           THEN ESTIMATE COMMENTS                      
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
         USING ACKEYD,R6                                                        
         USING TRANSD,R7                                                        
TRANSRT  L     R6,ADTRANS                                                       
         LR    R7,R6                                                            
         SH    R6,DATADISP                                                      
         CLI   TRNSEL,X'44'        EXIT IF : NOT A X'44' ELEMENT                
         BNE   EXIT                                                             
*                                                                               
         LA    R2,SORTREC                                                       
         CLC   STAGY,=C'99'        JUST IN CASE!!!                              
         BE    EXIT                                                             
         TM    RUNSTAT,GOODTEST    NOT YNBR, OFFD, ETC                          
         BNO   EXIT                                                             
*                                                                               
         CLC   TRNSANAL,=C'**'     OR NOT VALID WORKCODE                        
         BE    EXIT                                                             
*                                                                               
         ZAP   DUB,TRNSAMNT                                                     
         CLC   TRNSANAL,=C'99'     IF BILLING, USE OTHER ROUTINE                
         BE    TRNS99                                                           
*                                                                               
         USING TABLD,R5                                                         
         LA    R5,TABLREC          BUILD TABLE OF WORKCODE,                     
         MVC   TABLCODE,TRNSANAL    INVOICE AND AMOUNT                          
         USING ACMD,R7                                                          
         L     R7,AMONACC                                                       
         L     R7,ACMAPRO2         FIRST 77 ELEM                                
         CLI   0(R7),PTAELQ                                                     
         B     NEXT77A                                                          
NEXT77   MVI   ELCODE,PTAELQ                                                    
         BAS   RE,NEXTEL                                                        
NEXT77A  BNE   EXIT                                                             
         USING PTAELD,R7                                                        
         CLI   PTATYPE,PTATRAL     NOT BILLED IF INVOICE IS BLANK               
         BNE   NEXT77                                                           
         MVC   TABLINV,PTARBLNO    MOVE INVOICE AND AMOUNT TO TABLE             
         ZAP   TABLAMT,PTANET                                                   
         GOTO1 BINADD,DMCB,(R5),ABINTAB,ATABLE                                  
         B     NEXT77                                                           
*                                                                               
TRNS99   CP    DUB,=P'0'                                                        
         BE    EXIT                                                             
         USING TRSTATD,R7                                                       
         MVI   ELCODE,X'60'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   DUMP                                                             
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,DATE)                                
         CLC   DATE,QSTR3          VERIFY WITHIN DATE RANGE                     
         BL    EXIT                                                             
         CLC   DATE,QEND3                                                       
         BH    EXIT                                                             
         USING BIND,R5                                                          
         L     R5,ATABLE                                                        
         OC    BININ,BININ                                                      
         BZ    EXIT                IF NO TABLE ENTRIES, JUST EXIT               
         BAS   RE,BILLWC                                                        
         BAS   RE,BILLDATA                                                      
         B     EXIT                                                             
         DROP  R5,R6,R7                                                         
         EJECT                                                                  
GETSORT  CP    SORTCT,=P'0'        DID WE SORT ANYTHING ?                       
         BE    DONE                NO, JUST PRINT TOTAL                         
GETNEXT  GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R6,15,DMCB+4        DONE WHEN RECORD ADDRESS ZERO                
         BZ    DONE                                                             
         LA    R4,SORTREC          MOVE RECORD BACK TO SORTREC                  
         LH    R5,=H'283'           WHERE WE CAN SEE IT                         
         LR    R7,R5                                                            
         MVCL  R4,R6                                                            
         LA    R2,SORTREC          ADDRESS SORT RECORD                          
         LA    R4,P                ADDRESS PRINT LINE                           
         CLC   SAVETYP,STTPTYP     IF NOT SAME TYPE, FORCE HEADING              
         BE    PRTYP4                                                           
         MVC   SAVETYP,STTPTYP                                                  
         MVI   FORCEHED,C'Y'                                                    
PRTYP4   MVI   RCSUBPRG,X'04'      IF CLOSED, SET HEADING AND EXIT              
         CLI   STTPTYP,C'4'                                                     
         BE    PRTLINEA                                                         
PRTYP3   CLI   STTPTYP,C'3'        IS THIS BILLING ?                            
         BNE   PRTYP2               NO, SEE IF REVISION                         
         MVI   RCSUBPRG,X'07'       YES, SETUP FOR DETAIL                       
         CLI   STRECTYP,C'2'                                                    
         BE    PRTWC                                                            
         MVI   RCSUBPRG,X'03'      NOT DETAIL, SETUP FOR DATA                   
         USING PRINT3,R4                                                        
         MVC   PINVC,STINVC                                                     
         MVC   PDATE,STDATE                                                     
         B     PRTLINE                                                          
*                                                                               
PRTYP2   CLI   STTPTYP,C'2'        IS THIS A REVISION ?                         
         BNE   PRTYP1               NO, MUST BE NEW                             
         CLI   STRECTYP,C'2'       YES, IS IT DETAIL LEVEL ?                    
         BNE   PRTYP2A              NO,  COMMENT OR ESTIMATE LEVEL              
         MVI   RCSUBPRG,X'06'                                                   
         USING PRINT5,R4                                                        
PRTWC    LA    R0,14                                                            
         MVC   PWC,STWC                                                         
         EDIT  (C13,STWCAMT),(13,PWCAMT),2,MINUS=YES                            
         BAS   RE,PRTERR                                                        
         B     PRTWC2                                                           
*                                                                               
PRTWC1   CLC   STWC,SPACES         SKIP A LINE WHEN DONE                        
         BNH   PRTWC3                                                           
         MVC   PWC,STWC                                                         
         EDIT  (C13,STWCAMT),(13,PWCAMT),2,MINUS=YES                            
         BAS   RE,PRINTIT                                                       
PRTWC2   LA    R2,STWCLNG(R2)                                                   
         BCT   R0,PRTWC1                                                        
*                                                                               
PRTWC3   MVC   P,SPACES                                                         
         BAS   RE,PRINTIT                                                       
         LA    R2,SORTREC          READDRESS RECORD                             
         B     PDUMP                                                            
*                                                                               
         USING PRINT2,R4                                                        
PRTYP2A  CLI   STRECTYP,C'3'       COMMENT DATA                                 
         BNE   PRTYP2B             NO,                                          
         BAS   RE,PRTCOMM          YES, USE COMMENT PRINTER                     
         B     PDUMP               THEN WRITE RECORD TO TAPE                    
*                                                                               
PRTYP2B  MVI   RCSUBPRG,X'02'      ESTIMATE LEVEL - REVISION                    
         MVC   PREASRV,STREASRV                                                 
         B     PRTLINE                                                          
*                                                                               
         USING PRINT1,R4                                                        
PRTYP1   CLI   STTPTYP,C'1'        IF NOT NEW, SKIP IT                          
         BNE   GETNEXT                                                          
         MVI   RCSUBPRG,X'05'                                                   
         CLI   STRECTYP,C'2'       IS THIS DETAIL LEVEL ?                       
         BE    PRTWC                YES, GO PROCESS AS REVISON DETAIL           
         CLI   STRECTYP,C'3'       COMMENT DATA                                 
         BNE   PRTYP1A             NO,                                          
         BAS   RE,PRTCOMM          YES, USE COMMENT PRINTER                     
         B     PDUMP               THEN WRITE RECORD TO TAPE                    
*                                                                               
PRTYP1A  MVI   RCSUBPRG,X'01'                                                   
         MVC   PPROMO,STPROMO                                                   
*                                                                               
*        NO LONGER SO, AS PER DFLODDNY, CFORYNNY 9/91                           
*                                                                               
*        CLC   STPROMO,SPACES      IF BLANK, MOVE ZEROES TO TAPE                
*        BNH   *+10                                                             
*        MVC   STPROMO,ZEROES                                                   
*                                                                               
         MVC   PPPQ,STPPQ                                                       
PRTLINE  EDIT  (C13,STBILAMT),(13,P+29),2,MINUS=YES                             
         MVC   P+45(30),STESTDSC   PRINT ESTIMATE DESCRIPTION                   
PRTLINEA BAS   RE,PRTERR                                                        
PDUMP    CLI   QOPT7,C'Y'          DO PDUMP IF WANTED                           
         BNE   CHKOPT                                                           
         LA    R6,283                                                           
         GOTO1 PRNTBL,DMCB,=C'PUT',(R2),C'DUMP',(R6),=C'1D'                     
CHKOPT   CLI   QOPT1,C'Y'          GET NEXT RECORD IF NOT CREATING              
         BNE   GETNEXT              A TAPE                                      
         LA    R6,SORTREC                                                       
         LA    R4,TPREC            MOVE PART OF RECORD TO TAPE                  
         LH    R5,=H'270'           TO WHERE WE CAN SEE IT                      
         LR    R7,R5                                                            
         MVCL  R4,R6                                                            
         PUT   OUTP,TPREC          PUT OUT TAPE RECORD                          
         AP    TPCT,=P'1'                                                       
         B     GETNEXT                                                          
         DROP  R4                                                               
         SPACE 2                                                                
DONE     GOTO1 SORTER,DMCB,=C'END'                                              
         MVI   FORCEHED,C'Y'                                                    
         LA    R6,P                                                             
         MVI   RCSUBPRG,X'00'                                                   
         MVC   1(14,R6),=C'REQUEST TOTALS'                                      
         LA    R6,PSECOND                                                       
         MVC   1(22,R6),=C'TAPE RECORDS WRITTEN: '                              
         EDIT  (P4,TPCT),(9,23(R6)),COMMAS=YES,ALIGN=LEFT                       
         LA    R6,PTHIRD                                                        
         MVC   1(16,R6),=C'RECORDS SORTED: '                                    
         EDIT  (P4,SORTCT),(9,17(R6)),COMMAS=YES,ALIGN=LEFT                     
         BAS   RE,PRINTIT                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         CLOSE (OUTP)                                                           
         B     EXIT                                                             
         EJECT                                                                  
PRTCOMM  NTR1                                                                   
         MVI   RCSUBPRG,8          COMMENT HEADING                              
         LA    R1,3                MAX COMMENTS ON RECORD                       
         LA    R2,SORTREC          MAKE SURE R2 IS RIGHT                        
         MVC   STINVC(8),SPACES    RESTORE TEMPORARY SEQ NUMBER                 
PRTCOM1  EQU   *                                                                
         MVC   P+2(L'STCOCOMM),STCOCOMM                                         
         BAS   RE,PRINTIT                                                       
PRTCOM2  LA    R2,STCOLNG(R2)                                                   
         BCT   R1,PRTCOM1                                                       
         B     EXIT                                                             
         EJECT                                                                  
ESTWC    NTR1                                                                   
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         L     R3,ACMACOL                                                       
         USING JBCOLD,R3                                                        
         LH    R1,JBNROWS                                                       
         MVI   STRECTYP,C'2'       INDICATE WORK CODE LEVEL DATA                
         ZAP   ESTTOT,JBCOLVAL     GET TOTAL FIRST                              
EWC010   LA    R2,SORTREC                                                       
         MVI   STDTL,C' '                                                       
         MVC   STDTL+1(L'STDTL-1),STDTL                                         
         LA    R0,14               MAXIMUM PER RECORD                           
EWC020   CLI   JBCOLTYP,JBCOLTWC                                                
         BNE   EWC040                                                           
         MVC   STWC,SPACES                                                      
         MVC   STWC(2),JBCOLWC                                                  
         UNPK  STWCAMT,JBCOLVAL    GET W/C AMOUNT                               
         LA    R2,STWCLNG(R2)                                                   
*                                                                               
EWC040   AH    R3,JBLCOL                                                        
         BCT   R1,*+8                                                           
         B     EWC060                                                           
         BCT   R0,EWC020                                                        
         BAS   RE,SENDIT           SEND RECORD TO SORT                          
         B     EWC010                                                           
*                                                                               
EWC060   CH    R0,=H'14'                                                        
         BE    *+8                                                              
         BAS   RE,SENDIT                                                        
         LA    R2,SORTREC                                                       
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
ESTCOMM  NTR1                      WRITE AGENCY ESTIMATE COMMENTS               
         L     R7,ADACC                                                         
         AH    R7,DATADISP                                                      
         ST    R7,MYFULL                                                        
*        BAS   RE,SAVESEQ          SAVE SEQ FOR MONACC                          
         ZAP   COMSEQ,=P'0'        SEQ NUMBER ON TAPE                           
         MVI   STRECTYP,C'3'       INDICATE COMMENT DATA                        
         LA    R2,SORTREC                                                       
         MVI   STDTL,C' '          CLEAR THE 244                                
         MVC   STDTL+1(L'STDTL-1),STDTL                                         
*                                                                               
         MVI   COMCNT,0            COMMENTS WRITTEN TO SORT RECORD              
         USING ACOMMD,R7                                                        
         MVI   ELCODE,X'3E'                                                     
ECO020   L     R7,MYFULL           RESTORE R7 (IF FROM DATAMGR)                 
         BAS   RE,NEXTEL           LOOK FOR COMMENT RECORDS                     
         BNE   ECO040              NO MORE COMMENTS IN ACCOUNT                  
         ST    R7,MYFULL                                                        
*                                                                               
         TM    ACOMTYPE,X'40'      MUST BE PRINT ON ESTIMATE COMMENT            
         BNO   ECO020                                                           
         XC    MYKEY,MYKEY         CLEAR KEY TO READ COMMENT RECORD             
         MVI   MYKEY,X'0C'         RECORD TYPE                                  
         MVC   MYKEY+1(1),QCOMPANY AGENCY HEX CODE                              
         MVC   MYKEY+2(6),ACOMMENT COMMENT NUMBER                               
         LA    R7,BUFFER                                                        
         MVC   KEYSAVE,MYKEY       PREPARE FOR READHIGH                         
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,(R7)                       
         CLC   KEYSAVE,0(R7)       DID I GET THE RECORD                         
         BNE   ECO020              NO, LOOK FOR ANOTHER COMMENT ELEMENT         
*                                                                               
         MVI   ELCODE,X'3E'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ECO030   BAS   RE,NEXTEL           LOOK FOR THE 3E'S IN THIS RECORD             
         BNE   ECO020              LOOK FOR ANOTHER COMMENT RECORD              
         ZIC   R1,ACOMLEN          LENGTH OF THIS COMMENT ELEMENT               
         SH    R1,=H'5'            MINUS 4 IS LEN OF COMMENT -1 FOR MVC         
         LH    R3,=H'69'           MAX IS 70 (-1 FOR THE EX MVC)                
         CR    R1,R3                                                            
         BH    ECO035              LENGTH GT 70, NEED 2 MOVES                   
         CLI   COMCNT,3            ANY ROOM LEFT ON THIS SORT RECORD            
         BL    ECO033              YES                                          
         BAS   RE,ECO099           NO, WRITE CLEAR AND RESET                    
*                                                                               
ECO033   EQU   *                   THERE IS ENOUGH ROOM LEFT SO...              
         EX    R1,*+8              WRITE OUT THIS COMMENT                       
         B     *+10                                                             
         MVC   STCOCOMM(0),ACOMMENT                                             
         AP    COMSEQ,=P'1'        BUMP SEQUENCE COUNTER                        
         UNPK  STCOSEQ,COMSEQ      WRITE SEQ NUM TO TAPE                        
         OI    STCOSEQ+2,X'F0'                                                  
*                                                                               
         ZIC   R3,COMCNT                                                        
         LA    R3,1(R3)            BUMP COUNTER                                 
         STC   R3,COMCNT                                                        
         LA    R2,STCOLNG(R2)      NEXT COMMENT AREA                            
         B     ECO030              GET NEXT COMMENT                             
*                                                                               
ECO035   EQU   *                   COMMENT LONGER THAN 60                       
         CLI   COMCNT,3            ENOUGH ROOM FOR FIRST                        
         BL    ECO036              YES                                          
         BAS   RE,ECO099           NO, WRITE CLEAR AND RESET                    
ECO036   MVC   STCOCOMM(70),ACOMMENT  WRITE OUT FIRST COMMENT                   
         AP    COMSEQ,=P'1'        BUMP SEQUENCE COUNTER                        
         UNPK  STCOSEQ,COMSEQ      WRITE SEQ NUM TO TAPE                        
         OI    STCOSEQ+2,X'F0'                                                  
         ZIC   R3,COMCNT                                                        
         LA    R3,1(R3)            BUMP COUNTER                                 
         STC   R3,COMCNT                                                        
         LA    R2,STCOLNG(R2)      NEXT COMMENT AREA                            
         CLI   COMCNT,3            ENOUGH ROOM FOR SECOND PART                  
         BL    ECO036A             YES                                          
         BAS   RE,ECO099           NO, WRITE CLEAR AND RESET                    
ECO036A  SH    R1,=H'70'           GET LENGTH OF REMAINDER                      
         CH    R1,=H'70'                                                        
         BL    *+6                 SECOND HALF GT 60 BYTES                      
         DC    H'0'                TO BIG                                       
         EX    R1,*+8              WRITE OUT THIS COMMENT                       
         B     *+10                                                             
         MVC   STCOCOMM(0),ACOMMENT+70                                          
         AP    COMSEQ,=P'1'        BUMP SEQUENCE COUNTER                        
         UNPK  STCOSEQ,COMSEQ      WRITE SEQ NUM TO TAPE                        
         OI    STCOSEQ+2,X'F0'                                                  
         ZIC   R3,COMCNT                                                        
         LA    R3,1(R3)            BUMP COUNTER                                 
         STC   R3,COMCNT                                                        
         LA    R2,STCOLNG(R2)      NEXT COMMENT AREA                            
         B     ECO030              GET NEXT COMMENT                             
*                                                                               
ECO040   EQU   *                                                                
         CLI   COMCNT,0            ANYTHING LEFT TO PUT TO SORT                 
         BE    ECO041                                                           
         BAS   RE,ECO099                                                        
ECO041   EQU   *                                                                
*CO041   BAS   RE,RESTSEQ          RESTORE READ SEQUENCE FOR MONACC             
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
ECO099   EQU   *                   WRITE, CLEAR AND RESET                       
         ST    RE,FULL             STORE RETURN ADDRESS                         
         LA    R2,SORTREC          RESET R2                                     
         ZAP   STINVC,COMSEQ       RECORD SEQ FOR SORTER                        
         BAS   RE,SENDIT           WRITE OUT WHAT YOU HAVE                      
         MVC   STINVC,SPACES                                                    
         MVI   STDTL,C' '          CLEAR THE 244                                
         MVC   STDTL+1(L'STDTL-1),STDTL                                         
         MVI   COMCNT,0            RESET COUNT                                  
         L     RE,FULL           RESTORE RETURN ADDRESS                         
         BR    RE                  BYE                                          
         DROP  R7                                                               
         EJECT                                                                  
ESTDATA  NTR1                                                                   
         MVI   STDTL,C' '                                                       
         MVC   STDTL+1(L'STDTL-1),STDTL                                         
         MVI   STRECTYP,C'1'       INDICATE ESTIMATE LEVEL DATA                 
         UNPK  STBILAMT,ESTTOT     MOVE IN ESTIMATE TOTAL                       
         CLI   STTPTYP,C'2'        IS THIS A REVISED ESTIMATE ?                 
         BNE   ESTTYP1             NO, SKIP REVISON NUMBER                      
ESTTYP2  MVC   STREASRV,JBREASRV   MOVE IN REASON FOR REVISION                  
         OC    STREASRV,STREASRV                                                
         BNZ   *+10                                                             
         MVC   STREASRV,SPACES                                                  
         B     ESTXIT                                                           
*                                                                               
ESTTYP1  MVC   STPROMO,JBPROMO     MOVE IN  PROMOTION NUMBER                    
*                                                                               
         OC    STPROMO,STPROMO                                                  
         BNZ   *+10                                                             
         MVC   STPROMO,SPACES                                                   
*                                                                               
         MVC   STPPQ,JBPPQ          AND PUBLICATION QUARTER                     
         OC    STPPQ,STPPQ                                                      
         BNZ   *+10                                                             
         MVC   STPPQ,SPACES                                                     
*                                                                               
         MVC   STESTDSC,ESTNAME    MOVE IN ESTIMATE NAME                        
ESTXIT   BAS   RE,SENDIT           SEND IT TO SORT AND EXIT                     
         B     EXIT                                                             
         EJECT                                                                  
NEWESTS  NTR1                                                                   
*                                                                               
*        BAS   RE,SAVESEQ          SAVE MONACCS READ SEQUENCE                   
*                                                                               
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         USING JBLOCKD,R5                                                       
         L     R5,ACMAJOBB                                                      
*                                                                               
*                                                                               
         XC    MYKEY,MYKEY         PREP READING NEW EST RECORDS                 
         USING ACKEYD,R2                                                        
         LA    R2,MYKEY                                                         
         L     R4,ADACC                                                         
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL(1),0(R4)                                                 
         MVC   ACEVCUL+1(2),1(R4)                                               
         MVC   ACEVCLI,SPACES                                                   
         MVC   ACEVCLI(3),3(R4)                                                 
         MVC   ACEVPROD,SPACES                                                  
         MVC   ACEVPROD(3),6(R4)                                                
         MVC   ACEVJOB(6),9(R4)                                                 
         MVI   ACEVTYPE,ACEVREV                                                 
         MVI   ACEVERS,1           GET ORIGINAL ESTIAMTE                        
*                                                                               
         LA    R2,MYKEY                                                         
         BAS   RE,HIGH          POINT R7 TO THE RECORD                          
*                                                                               
         CLC   MYKEY(ACEVERS-ACEVKEY),0(R7) DID I GET R1                        
         BNE   EST50               NO                                           
*                                                                               
         MVI   ELCODE,ACEPELQ      GET EST PREPARER EL                          
         BAS   RE,GETEL                                                         
         BNE   EST50               NO PREP DATE, DON'T USE EST                  
*                                                                               
         USING ACEPD,R7                                                         
         MVC   JBNEWDAT,ACEPDATE   SAVE PREP DATE                               
*                                                                               
         USING ACKEYD,R2                                                        
EST50    CLI   JBHIREV,1           IS THERE A HIGHER REV                        
         BE    ESTX                NO                                           
         LA    R2,MYKEY                                                         
         MVC   ACEVERS,JBHIREV     GET HIGHEST REVISION                         
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   MYKEY(ACEVERS-ACEVKEY),0(R7) DID I GET HIGHEST R                 
         BNE   EST50               NO                                           
*                                                                               
         MVI   ELCODE,ACEPELQ      GET EST PREPARER EL                          
         BAS   RE,GETEL                                                         
         BNE   ESTX                NO PREP DATE, DON'T USE EST                  
*                                                                               
         USING ACEPD,R7                                                         
         MVC   JBREVDAT,ACEPDATE   SAVE PREP DATE                               
*                                                                               
ESTX     EQU   *                                                                
*STX     BAS   RE,RESTSEQ          RESTORE READ SEQUENCE                        
         B     EXIT                                                             
*                                                                               
READ     MVC   COMMAND,=CL8'DMREAD'                                             
         LA    R7,BUFFER                                                        
         B     ESTDMGR                                                          
HIGH     MVC   COMMAND,=CL8'DMRDHI'                                             
         LA    R7,BUFFER                                                        
         B     ESTDMGR                                                          
SEQ      MVC   COMMAND,=CL8'DMRSEQ'                                             
         LA    R7,BUFFER                                                        
         B     ESTDMGR                                                          
         SPACE                                                                  
ESTDMGR  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R7)                      
         B     EXIT                                                             
         DROP  R2,R3,R5,R7                                                      
         EJECT                                                                  
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SAVE THE READ SEQUENCE                                                 
*----------------------------------------------------------------------         
***********************************************************************         
*AVESEQ  NTR1                                                         *         
*        L     RE,ADACCFIL         A(ACC DIR)                         *         
*        L     RE,ISPDKEY-ISDTF(RE)                                   *         
*        MVC   DCBKEY,0(RE)                                           *         
*        B     EXIT                                                   *         
*                                                                     *         
*                                                                     *         
*----------------------------------------------------------------------         
*        RESTORE THE READ SEQUENCE                                    *         
*----------------------------------------------------------------------         
*ESTSEQ  NTR1                                                         *         
*        MVC   MYKEY,DCBKEY                                           *         
*        BAS   RE,READ                                                *         
*        B     EXIT                                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         USING BIND,R5                                                          
         USING TRANSD,R7                                                        
         USING SORTD,R2                                                         
BILLWC   NTR1                                                                   
         LA    R2,SORTREC                                                       
         L     R7,ADTRANS                                                       
         MVI   STTPTYP,C'3'        INDICATE BILLING                             
         MVI   STRECTYP,C'2'        WORK CODE LEVEL                             
         MVC   STINVC(6),TRNSREF   PLUG IN INVOICE NUMBER                       
         ZAP   BILLTOT,=P'0'       CLEAR TOTAL BUCKET                           
         MVI   STDTL,C' '                                                       
         MVC   STDTL+1(L'STDTL-1),STDTL                                         
         L     R3,BININ                                                         
         L     R5,ABINTAB                                                       
         LA    R0,14                                                            
         LA    R2,SORTREC                                                       
         B     BWC030                                                           
         USING TABLD,R5                                                         
BWC010   LA    R0,14               MAXIMUM PER RECORD                           
         LA    R2,SORTREC                                                       
         MVI   STDTL,C' '                                                       
         MVC   STDTL+1(L'STDTL-1),STDTL                                         
BWC020   LA    R5,TABLLEN(R5)                                                   
BWC030   CLC   TABLINV,TRNSREF                                                  
         BE    BWC040                                                           
         BCT   R3,BWC020                                                        
         B     BWC060                                                           
*                                                                               
BWC040   MVC   STWC(2),TABLCODE                                                 
         UNPK  STWCAMT,TABLAMT     GET W/C AMOUNT                               
         AP    BILLTOT,TABLAMT                                                  
         LA    R2,STWCLNG(R2)                                                   
         BCT   R0,BWC020                                                        
         BAS   RE,SENDIT           SEND RECORD TO SORT                          
         B     BWC010                                                           
*                                                                               
BWC060   CH    R0,=H'14'                                                        
         BE    *+8                                                              
         BAS   RE,SENDIT                                                        
         LA    R2,SORTREC                                                       
         B     EXIT                                                             
         DROP  R5,R7                                                            
         EJECT                                                                  
         USING BIND,R5                                                          
         USING TRANSD,R7                                                        
BILLDATA NTR1                                                                   
         L     R7,ADTRANS                                                       
         MVI   STDTL,C' '                                                       
         MVC   STDTL+1(L'STDTL-1),STDTL                                         
         MVI   STTPTYP,C'3'        INDICATE BILLING                             
         MVI   STRECTYP,C'1'        ESTIMATE LEVEL                              
         UNPK  STBILAMT,BILLTOT    MOVE IN BILLING TOTAL                        
         GOTO1 DATCON,DMCB,(5,DATE),(X'20',STDATE)                              
         MVC   STINVC(6),TRNSREF                                                
         BAS   RE,SENDIT                                                        
         MVC   STINVC,SPACES       CLEAR FOR NEXT TIME                          
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
ACCOUNT  NTR1                                                                   
         USING ACKEYD,R7                                                        
         L     R7,ADACC                                                         
         MVC   STACCT,ACKEYACC+3   GET THE JOB NUMBER                           
         MVI   STERR,NOENO         PREPARE FOR NO ENO= ERROR                    
         MVI   ELCODE,X'27'        GET ACCOUNT BILLING ELEMENT                  
         BAS   RE,GETELIO                                                       
         BNE   EXIT                ESTIMATE NUMBER REQUIRED                     
         USING ACABILLD,R7                                                      
         CLC   ACABESNO,SPACES     EXIT IF FIELD BLANK OR ZERO                  
         BNH   EXIT                REQUIRED FIELD                               
         MVI   STERR,X'00'         CLEAR ERROR CODE                             
         MVC   JBENO,ACABESNO      GET ESTIMATE NUMBER (ENO=)                   
         BAS   RE,VALENO           VALIDATE ENO                                 
*                                                                               
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
PRTERR   NTR1                                                                   
         MVC   P+1(12),STACCT                                                   
         MVC   P+15(12),STENO                                                   
*                                                                               
         TM    STERR,NOPROMO+NOPUBQ+NOENO+INVALENO ANYTHING WRONG               
         BZ    PERR40                                                           
*                                                                               
         TM    STERR,NOPROMO                                                    
         BNO   PERR20                                                           
         MVC   P+99(24),MESSAGE2                                                
         BAS   RE,PRINTIT                                                       
*                                                                               
PERR20   TM    STERR,INVALENO                                                   
         BNO   PERR30                                                           
         MVC   P+99(23),MESSAGE3                                                
         BAS   RE,PRINTIT                                                       
*                                                                               
PERR30   TM    STERR,NOENO                                                      
         BNO   PERR35                                                           
         MVC   P+99(18),MESSAGE1                                                
         BAS   RE,PRINTIT                                                       
PERR35   TM    STERR,NOPUBQ         PUB QUARTER MISSING                         
         BNO   EXIT                                                             
         MVC   P+99(27),MESSAGE4                                                
PERR40   BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
         EJECT                                                                  
PRINTIT  ST    RE,SVRE                                                          
         MVC   HEAD1+41(30),HEADHOLD                                            
         MVI   SPACING,1                                                        
         GOTO1 ACREPORT            PRINT SUBROUTINE                             
         L     RE,SVRE                                                          
         BR    RE                  RETURN TO USER                               
         EJECT                                                                  
SENDIT   NTR1                                                                   
         CLI   QOPT2,C' '          ANY CLOSED JOB FILTERING                     
         BE    SEND50                                                           
         USING SORTD,R2                                                         
         LA    R2,SORTREC                                                       
         CLI   STTPTYP,C'4'        IS THIS A CLOSED SORT REC                    
         BNE   SEND20              NO                                           
         CLI   QOPT2,C'N'          NO CLOSED?                                   
         BE    SENDITX             YES                                          
         B     SEND50                                                           
*                                                                               
SEND20   CLI   QOPT2,C'O'          CLOSED ONLY                                  
         BE    SENDITX             YES REJECT ALL OTHER RECORD TYPES            
*                                                                               
SEND50   GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         AP    SORTCT,=P'1'                                                     
SENDITX  B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        LOAD JBDATA WITH DATA FROM THE USER FIELDS ON THIS JOB                 
*----------------------------------------------------------------------         
LOADUSER NTR1                                                                   
         USING SORTD,R2                                                         
         LA    R2,SORTREC                                                       
         MVC   JBERR,STERR         SAVE CURRENT ERRORS                          
         USING ACUFD,R7                                                         
         L     R7,ADACC                                                         
         MVI   ELCODE,ACUFELQ                                                   
         BAS   RE,GETEL                                                         
LU10     BNE   LUX                                                              
         USING LUD,R3                                                           
         LA    R3,USERTAB                                                       
         LA    R0,LUNUM                                                         
*                                                                               
LU20     CLC   ACUFCODE,LUCODE     IS THIS CODE IN THE TABLE                    
         BE    LU50                YES, SAVE IT IN JBDATA                       
         LA    R3,LUDLN(R3)                                                     
         BCT   R0,LU20                                                          
*                                                                               
         B     LU60                THIS USER FIELD NOT NEEDED ON TAPE           
*                                                                               
LU50     ZIC   R2,ACUFLEN                                                       
         SH    R2,=H'33'                                                        
         BM    LU60                NO DATA ON THIS USER FIELD                   
*                                                                               
         MVI   BYTE,X'FF'          TURN OFF ERROR MSG FOR THIS FIELD            
         XC    BYTE,LUERRMSG       GET COMPLIMENT                               
         NC    JBERR,BYTE          "AND" WITH ORIGINAL                          
*                                                                               
         LA    R1,JBDATA           POINT R1 TO JB FIELD FOR THIS UF             
         SR    R0,R0                                                            
         ICM   R0,B'0011',LUJBFLD  OFFSET INTO JBDATA OF THIS FIELD             
         AR    R1,R0                                                            
*                                                                               
         ZIC   R6,LUJBLEN          LENGTH OF THIS TAPE FIELD                    
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES                                                   
*                                                                               
         CR    R2,R6               Q, ACTUAL LENGTH GREATER THAN MAX.           
         BNH   *+6                                                              
         LR    R2,R6               Y, ALLOW ONLY THE MAX. LENGTH                
         EX    R2,LU80                                                          
LU60     BAS   RE,NEXTEL                                                        
         B     LU10                                                             
*                                                                               
LU80     MVC   0(0,R1),ACUFDATA                                                 
*                                                                               
         USING SORTD,R2                                                         
LUX      LA    R2,SORTREC                                                       
         MVC   STERR,JBERR         SAVE CURRENT ERRORS                          
         BAS   RE,VALENO                                                        
*                                                                               
         L     R7,ADACC            GET MEDIA                                    
         CLI   9(R7),C'P'          IF NOT MEDIA 'P', DON'T NEED                 
         BE    LUXX                PROMO NUMBER                                 
         NI    STERR,X'FF'-NOPROMO                                              
LUXX     B     EXIT                                                             
         DROP  R3,R7                                                            
         EJECT                                                                  
VALENO   NTR1                                                                   
         USING SORTD,R2                                                         
         LA    R2,SORTREC                                                       
         NI    STERR,X'FF'-INVALENO                                             
         TM    STERR,NOENO         ANY ENO AT ALL                               
         BO    EXIT                NO                                           
*                                                                               
         CLI   JBENO+4,C'0'        VALIDATE ENO                                 
         BE    EXIT                                                             
         CLI   JBENO+4,C'A'                                                     
         BNL   *+12                                                             
         OI    STERR,INVALENO      INDICATE TYPE OF ERROR                       
         B     EXIT                                                             
         CLI   JBENO+4,C'D'                                                     
         BH    *-12                                                             
         B     EXIT                                                             
         EJECT                                                                  
LOOKUP   NTR1                                                                   
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,LASTIO                                                    
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACMACOL                                                       
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CE'                                                            
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
OUTP     DCB   DDNAME=OUTP,DSORG=PS,RECFM=FB,LRECL=270,MACRF=PM,       X        
               BUFNO=2,BLKSIZE=32400                                            
         EJECT                                                                  
GETELIO  L     R7,ADACC                                                         
         GETEL R7,DATADISP,ELCODE                                               
         SPACE 3                                                                
DUMP     DC    H'0'                                                             
*                                                                               
*ZEROES   DC    12C'0'                                                          
*                                                                               
         DS    0D                    DOUBLE ALLIGN TPVOLS FOR CVB INST.         
TPVOLS   DC    PL8'0'              GENERATE TAPE +1                             
SQUASHER DC    V(SQUASHER)                                                      
PRNTBL   DC    V(PRNTBL)                                                        
SORTER   DC    V(SORTER)                                                        
ATABLE   DC    A(TABLE)                                                         
BINADD   DC    A(BINADDC)                                                       
ABINTAB  DC    A(BINTABLE)                                                      
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,16,A,25,2,A,17,8,A),FORMAT=BI'               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(283)'                                 
         SPACE 3                                                                
MESSAGE4 DC    CL27'PUBLICATION QUARTER MISSING'                                
MESSAGE3 DC    CL23'INVALID ESTIMATE NUMBER'                                    
MESSAGE2 DC    CL24'PROMOTION NUMBER MISSING'                                   
MESSAGE1 DC    CL18'NO ESTIMATE NUMBER'                                         
         SPACE 3                                                                
AGYTBL   DS    0CL3                                                             
         DC    X'5F',C'10'         MEZZINA/BROWN                                
         DC    X'F9',C'09'         SJR - USE FOR TESTING                        
         DC    X'00'                                                            
*                                                                               
PRODTBL  DS    0CL3                PRODUCTS WHICH OVERRIDE STCMP                
         DC    C'PB '                                                           
         DC    C'PF '                                                           
         DC    C'PM '                                                           
PRODNUM  EQU   ((*-PRODTBL)/L'PRODTBL)                                          
         SPACE 3                                                                
TABLE    DS    0D                                                               
         DC    F'0'                NUMBER OF TABLE ENTRIES                      
         DC    AL4(TABLLEN)        RECORD LENGTH                                
         DC    AL4(TABLKLN)        DISP OF KEY/ KEY LENGTH                      
         DC    F'6000'             MAXIMUM NUMBER OF ENTRIES                    
         DC    AL1(1)              NUMBER OF BUCKETS                            
         DC    AL1(TABLBK-TABLD)   DISPLACEMENT TO BUCKETS                      
         DC    AL1(0)              SPARE                                        
         SPACE 3                                                                
PUBQTAB  DS    0CL5               SUBCOMPAY/PUBLICATION QUARTER                 
         DC    C'1',C'8901'                                                     
         DC    C'2',C'8902'                                                     
         DC    C'3',C'8903'                                                     
         DC    C'4',C'8904'                                                     
         DC    C'5',C'9001'                                                     
         DC    C'6',C'9002'                                                     
         DC    C'7',C'9003'                                                     
         DC    C'8',C'9004'                                                     
         DC    C'9',C'9101'                                                     
         DC    C'A',C'9102'                                                     
         DC    C'B',C'9103'                                                     
         DC    C'C',C'9104'                                                     
         DC    C'D',C'9201'                                                     
         DC    C'E',C'9202'                                                     
         DC    C'F',C'9203'                                                     
         DC    C'G',C'9204'                                                     
PUBQNUM  EQU   (*-PUBQTAB)/L'PUBQTAB                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        USER FIELD TABLE                                                       
*        TABLE DEFINES WHERE IN TPREC TO PUT A USER FIELD                       
*        COVERED BY LUD                                                         
*----------------------------------------------------------------------         
USERTAB  DS    0C                                                               
         DC    C'R1',AL2(JBPPQ-JBDATA),AL1(L'JBPPQ),AL1(NOPUBQ)                 
         DC    C'R2',AL2(JBENO-JBDATA),AL1(L'JBENO),AL1(NOENO)                  
         DC    C'R3',AL2(JBPROMO-JBDATA),AL1(L'JBPROMO),AL1(NOPROMO)            
         DC    C'R4',AL2(JBREASRV-JBDATA),AL1(L'JBREASRV),AL1(0)                
LUNUM    EQU   (*-USERTAB)/LUDLN                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BINTABLE DS    (6000*TABLLEN)C     TABLE                                        
         EJECT                                                                  
*              ROUTINE TO ADD TO A BINSRCH TABLE                                
*              PARAM1              A(RECORD TO BE ADDED)                        
*              PARAM2              A(TABLE)                                     
*              PARAM3              A(BINSRCH PARAMS)                            
         USING BIND,R5                                                          
BINADDC  DS    0D                                                               
         NMOD1 0,*BINADD*                                                       
         L     RC,ADRC             RESTORE REGISTER 12                          
         L     R2,0(R1)            A(RECORD)                                    
         L     R3,4(R1)            A(TABLE)                                     
         L     R5,8(R1)            BINSRCH PARAMETERS                           
         MVC   DMCB+8(16),BININ                                                 
         GOTO1 BINSRCH,DMCB,(1,(R2)),(R3)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R2,R6               NEW RECORD                                   
         ZIC   R0,BINNUMB          NUMBER OF BUCKETS                            
         AP    0(8,R4),0(8,R2)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R2,8(R2)                                                         
         BCT   R0,*-14                                                          
BINXIT   XIT1                                                                   
         SPACE 3                                                                
ADRC     DS    F                   SAVE ADDRESS OF REGISTER 12                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
ACIR02D  DSECT                                                                  
TPDSN    DS    CL20                                                             
TPREC    DS    CL270                                                            
SORTREC  DS    CL283                                                            
QSTR3    DS    XL3                                                              
QEND3    DS    XL3                                                              
DATE     DS    PL3                                                              
TPCT     DS    PL4                                                              
SORTCT   DS    PL4                                                              
ELCODE   DS    CL1                                                              
COMMAND  DS    CL8                                                              
SVRE     DS    F                   SAVE AREA FOR RETURN REGISTER                
SUBCOMP  DS    CL1                                                              
SAVETYP  DS    CL2                                                              
ESTNAME  DS    CL36                ESTIMATE NAME                                
ESTTOT   DS    PL7                 TOTAL ESTIMATE                               
BILLTOT  DS    PL7                 TOTAL BILLING                                
HEADHOLD DS    CL30                SAVE CLIENT NAME FOR HEADING                 
MYKEY    DS    CL42                KEY FOR DATAMGR READ                         
*CBKEY   DS    CL42                                                             
MYFULL   DS    F                   FULLWORD TO STORE REGISTER                   
COMCNT   DS    CL1                 COUNTER OF COMS ON A SORT REC -MAX 4         
COMSEQ   DS    PL2                 PACKED COUNTER-NUM OF COMM RECS/JOB          
RUNSTAT  DS    CL1                 STATUS BYTE                                  
YNBR     EQU   1                   ON IF ORIGIN IS YNBR                         
OFFD     EQU   2                   ON IF OFFICE AT PRODUCT LEVEL IS D           
GOODTEST EQU   80                  ON IF YNBR AND OFFD ARE CORRECT              
*                                                                               
JBDATA   DS    0C                  JOB LEVEL DATA FOR TAPE                      
JBNEWDAT DS    CL3                 ACJBOPND OR PREP DATE ON R1                  
JBREVDAT DS    CL3                 ACJBREVD OR PREP DATE OF HIGHEST R           
JBENO    DS    CL12                                                             
JBPROMO  DS    CL12                                                             
JBREASRV DS    CL2                                                              
JBPPQ    DS    CL4                                                              
JBERR    DS    CL1                                                              
JBDATALN EQU   *-JBDATA                                                         
*                                                                               
BUFFER   DS    CL1000              BUFFER FOR CLIENT RECORD                     
TABLREC  DS    (TABLLEN)C                                                       
         EJECT                                                                  
TABLD    DSECT                                                                  
TABLINV  DS    CL6                 INVOICE NUMBER                               
TABLCODE DS    CL2                 ANALYSIS CODE                                
TABLKLN  EQU   *-TABLD                                                          
TABLBK   EQU   *                                                                
TABLAMT  DS    CL8                 ACCUMULATOR                                  
TABLLEN  EQU   *-TABLD                                                          
         EJECT                                                                  
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER OF ENTRIES IN TABLE                   
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT OF KEY                          
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER OF ENTRIES                    
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISPLACEMENT OF 1ST BUCKET                   
         DS    CL1                 SPARE                                        
         EJECT                                                                  
SORTD    DSECT                                                                  
STALL    DS    0CL26                                                            
STCMP    DS    CL2                                                              
STAGY    DS    CL2                                                              
STENO    DS    CL12                                                             
STINVC   DS    CL8                                                              
STTPTYP  DS    CL1                                                              
STRECTYP DS    CL1                                                              
*                                                                               
STDTL    DS    0CL244                                                           
STBILAMT DS    CL13                                                             
STESTDSC DS    CL30                                                             
STPROMO  DS    CL12                                                             
STREASRV DS    CL2                                                              
STPPQ    DS    CL4                                                              
STDATE   DS    CL6                 BILLING DATE                                 
         DS    CL177                                                            
STACCT   DS    CL12                                                             
STERR    DS    CL1                                                              
NOPROMO  EQU   X'20'                                                            
NOPUBQ   EQU   X'10'                                                            
NOENO    EQU   X'80'                                                            
INVALENO EQU   X'40'                                                            
*                                                                               
         ORG   STDTL                                                            
STWC     DS    CL4                                                              
STWCAMT  DS    CL13                                                             
STWCLNG  EQU   *-STWC                                                           
         DS    CL221                                                            
         DS    CL6                                                              
         ORG   STDTL               ESTIMATE COMMENT DATA                        
STCO     DS    0CL244                                                           
STCOSEQ  DS    CL3                 SEQUENCE OF COMMENTS                         
STCOCOMM DS    CL70                FREE FORM AGENCY COMMENT                     
STCOLNG  EQU   *-STCO              73                                           
         DS    CL150               2 MORE AGENCY COMMENTS                       
         DS    CL1                 FILLER                                       
         EJECT                                                                  
PRINT1   DSECT                                                                  
         DS    CL77                                                             
PPROMO   DS    CL12                                                             
         DS    CL2                                                              
PPPQ     DS    CL4                                                              
         EJECT                                                                  
PRINT2   DSECT                                                                  
         DS    CL77                                                             
PREASRV  DS    CL2                                                              
         EJECT                                                                  
PRINT3   DSECT                                                                  
         DS    CL77                                                             
PDATE    DS    CL6                                                              
         DS    CL6                                                              
PINVC    DS    CL6                                                              
         EJECT                                                                  
PRINT5   DSECT                                                                  
         DS    CL29                                                             
PWC      DS    CL4                                                              
         DS    CL2                                                              
PWCAMT   DS    CL13                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        DSECT TO COVER USER FIELD TABLE                                        
*----------------------------------------------------------------------         
LUD      DSECT                                                                  
LUCODE   DS    CL2                                                              
LUJBFLD  DS    AL2                                                              
LUJBLEN  DS    AL1                                                              
LUERRMSG DS    AL1                                                              
LUDLN    EQU   *-LUD                                                            
         EJECT                                                                  
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACJOBBERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
* DMDTFIS                                                                       
*********PRINT OFF                                                              
*********INCLUDE DMDTFIS                                                        
*********PRINT ON                                                               
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPIR02 04/10/15'                                      
         END                                                                    
